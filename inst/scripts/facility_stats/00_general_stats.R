## #####################################################################################
##
## PURPOSE: Populate the "Facility stats" tab of the RESPOND facility profiling template.
##
##   Most columns are pulled from existing facility tables (type, management, catchment
##   population, HIV prevalence) and the DHAMIS ART cohort, plus two proximity metrics we
##   compute ourselves: distance to the nearest facility and the number of facilities
##   within 10 km. The filled workbook is written to the versioned Facility Statistics
##   output, preserving all existing formatting and the other two tabs.
##
##   All input/output paths and parameters live in config.yaml.
##
## #####################################################################################

REPO_DIR <- '~/repos/mwi.hiv.factors'

## Setup ------------------------------------------------------------------------------->

devtools::load_all(REPO_DIR)
library(data.table)

config <- versioning::Config$new(file.path(REPO_DIR, 'config.yaml'))

SHEET <- 'Facility stats'
DATA_ROWS <- 5:35            # facility rows in the template (header is on row 4)
STATS_FIRST_COL <- 4L        # the assembled stats block starts in column D
COUNT_NUMFMT <- '#,##0'      # count columns: thousands separators, no decimals

## Load inputs ------------------------------------------------------------------------->

# Facility name crosswalk: template_name (Excel col A) -> output_name + facility_id
crosswalk <- fread(path.expand(config$get_file_path('repo', 'facility_stats_crosswalk')))
data.table::setnames(crosswalk, 'facility_name', 'xwalk_facility_name')

# Cleaned facility metadata (facility_id == DHAMIS hfacility_id). Quoted fields contain
# commas, so a proper CSV reader is required.
facilities <- fread(
  path.expand(config$get_file_path('catchments', 'facility_metadata'))
)

# Facility-catchment population and modelled HIV prevalence
catchment_summary <- fread(
  path.expand(config$get_file_path('splitting', 'aggregated_results'))
)

# DHAMIS quarterly ART data (ARTOutcAlive = patients currently alive on ART)
art_cohort <- fread(
  path.expand(config$get_file_path('dhamis_obs', 'dhamis_cumulative_art'))
)

# M&E facility indicators: indicators in rows (label in column A), facilities across
# columns (names in `me_data_facility_name_row`).
me_raw <- openxlsx2::wb_to_df(
  openxlsx2::wb_load(path.expand(config$get_file_path('me_data', 'facility_indicators'))),
  sheet = config$get('me_data_sheet'), col_names = FALSE
)

## Assemble the facility statistics ---------------------------------------------------->

# Validate the crosswalk against facility metadata (each id must resolve to one facility)
missing_ids <- setdiff(crosswalk$facility_id, facilities$facility_id)
if(length(missing_ids) > 0L){
  stop('Crosswalk facility_id(s) not found in facility metadata: ',
       paste(missing_ids, collapse = ', '))
}

target <- merge(
  x = crosswalk,
  y = facilities[, .(
    facility_id, facility_name, facility_type, health_authority, restype, catchment_id
  )],
  by = 'facility_id',
  all.x = TRUE
)
# Setting (D): urban/rural classification from `restype`, normalised to match the
# template's stated categories (Urban / Rural / Semi-Urban)
target[, setting := c('Semi-urban' = 'Semi-Urban')[restype] ]
target[is.na(setting), setting := restype ]
name_mismatch <- target[xwalk_facility_name != facility_name, ]
if(nrow(name_mismatch) > 0L){
  warning(
    'Crosswalk facility_name differs from metadata for: ',
    paste(name_mismatch$template_name, collapse = ', ')
  )
}

# Catchment population (F) and HIV prevalence (G), keyed by catchment_id. The summary
# table's `id` is character (district ids look like "MWI_3_01"), so coerce the
# facility-catchment ids back to integer to match the facility metadata.
fac_catchments <- catchment_summary[
  aggregation_level == 'FACILITY_CATCHMENTS',
  .(catchment_id = as.integer(id), pop_total, prev15to49_mean)
]
target[
  fac_catchments,
  `:=` (pop_total = i.pop_total, prev15to49_mean = i.prev15to49_mean),
  on = 'catchment_id'
]

# Recent ART cohort size (H): sum ARTOutcAlive across departments per facility for the
# configured reporting period.
cohort_year <- config$get('art_cohort_year')
cohort_quarter <- config$get('art_cohort_quarter')
art_recent <- art_cohort[
  (year == cohort_year) & (quarter == cohort_quarter),
  .(art_cohort_size = sum(ARTOutcAlive, na.rm = TRUE)),
  by = .(facility_id = hfacility_id)
]
target[art_recent, art_cohort_size := i.art_cohort_size, on = 'facility_id']

# Proximity metrics (I, J): computed against all facilities nationwide
proximity <- facility_proximity_metrics(
  facilities = facilities,
  id_field = 'facility_id',
  lon_field = 'longitude',
  lat_field = 'latitude',
  within_km = 10
)
target[
  proximity,
  `:=` (nearest_facility_km = i.nearest_facility_km, n_within_10km = i.n_within_10km),
  on = 'facility_id'
]

## Assemble the M&E indicators --------------------------------------------------------->

me_rows <- config$get('me_data_indicator_rows')
me_name_row <- config$get('me_data_facility_name_row')
me_excel_rows <- as.integer(rownames(me_raw))

# Indicator labels (column A) become the new column names; "%" labels are rate indicators
me_labels <- vapply(
  me_rows, function(r) trimws(as.character(me_raw[me_excel_rows == r, 1L])), character(1)
)
me_is_percent <- grepl('%', me_labels)

# Facility names sit in column B onwards of the facility-name row
me_facility_names <- as.character(unlist(me_raw[me_excel_rows == me_name_row, -1L]))

# Long table of (facility, indicator, value), then resolve to our facilities via me_name
me_long <- data.table::rbindlist(lapply(me_rows, function(r){
  data.table(
    me_name = me_facility_names,
    indicator = trimws(as.character(me_raw[me_excel_rows == r, 1L])),
    value = as.numeric(unlist(me_raw[me_excel_rows == r, -1L]))
  )
}))
me_long[crosswalk, template_name := i.template_name, on = 'me_name']
unmatched_me <- sort(unique(me_long[is.na(template_name), me_name]))
if(length(unmatched_me) > 0L){
  stop('M&E facilities not found in crosswalk me_name column: ',
       paste(unmatched_me, collapse = ', '))
}

# Wide table: one row per facility, indicator columns in their original (row) order
me_long[, indicator := factor(indicator, levels = me_labels)]
me_wide <- data.table::dcast(me_long, template_name ~ indicator, value.var = 'value')
target <- merge(target, me_wide, by = 'template_name', all.x = TRUE)

# Warn about any missing values before writing
report_cols <- c(
  'setting', 'facility_type', 'health_authority', 'pop_total', 'prev15to49_mean',
  'art_cohort_size', 'nearest_facility_km', 'n_within_10km', me_labels
)
for(col in report_cols){
  na_rows <- target[is.na(get(col)), template_name]
  if(length(na_rows) > 0L){
    warning('Missing ', col, ' for: ', paste(na_rows, collapse = ', '))
  }
}

## Write into the template, preserving formatting -------------------------------------->

template_fp <- path.expand(config$get_file_path('facility_surveys', 'facility_stats_template'))
wb <- openxlsx2::wb_load(template_fp)

# Map each template row to a crosswalk entry using the existing facility names in col A
template_names <- openxlsx2::wb_to_df(
  wb, sheet = SHEET, dims = paste0('A', min(DATA_ROWS), ':A', max(DATA_ROWS)),
  col_names = FALSE
)[[1]]
row_map <- data.table(excel_row = DATA_ROWS, template_name = template_names)
row_map <- merge(row_map, target, by = 'template_name', all.x = TRUE, sort = FALSE)
data.table::setorder(row_map, excel_row)
unmatched <- row_map[is.na(facility_id), template_name]
if(length(unmatched) > 0L){
  stop('Template facilities not found in crosswalk: ', paste(unmatched, collapse = ', '))
}

# Columns D:K in template order: setting, type, management, population, prevalence,
# ART cohort, nearest-facility distance, facilities within 10 km
stats_block <- row_map[, .(
  setting, facility_type, health_authority, pop_total, prev15to49_mean,
  art_cohort_size, nearest_facility_km, n_within_10km
)]
# Use the in-place method form: the functional wb_add_data() returns a clone instead
# of modifying `wb`, so writes would otherwise be lost.
wb$add_data(
  sheet = SHEET, x = stats_block,
  dims = paste0(openxlsx2::int2col(STATS_FIRST_COL), min(DATA_ROWS)), col_names = FALSE
)

# Number formats for the assembled stats columns: counts get thousands separators (no
# decimals), the prevalence a one-decimal percentage, the distance one decimal place.
stats_formats <- c(
  pop_total = COUNT_NUMFMT,
  art_cohort_size = COUNT_NUMFMT,
  n_within_10km = COUNT_NUMFMT,
  prev15to49_mean = '0.0%',
  nearest_facility_km = '0.0'
)
for(nm in names(stats_formats)){
  col <- openxlsx2::int2col(STATS_FIRST_COL + match(nm, names(stats_block)) - 1L)
  wb$add_numfmt(
    sheet = SHEET, dims = paste0(col, min(DATA_ROWS), ':', col, max(DATA_ROWS)),
    numfmt = stats_formats[[nm]]
  )
}

# Correct facility display names where the crosswalk overrides the template label
name_fixes <- row_map[output_name != template_name, ]
for(i in seq_len(nrow(name_fixes))){
  wb$add_data(
    sheet = SHEET, x = name_fixes$output_name[i],
    dims = paste0('A', name_fixes$excel_row[i]), col_names = FALSE
  )
}

## Write the "M&E Data" column group --------------------------------------------------->

header_row <- min(DATA_ROWS) - 1L       # indicator headers share the row-4 header band
# Place the M&E block immediately after the template's existing columns, derived from the
# header row so it adapts if columns are added/removed in the template.
existing_headers <- openxlsx2::wb_to_df(
  wb, sheet = SHEET, dims = paste0('A', header_row, ':Z', header_row), col_names = FALSE
)
ME_FIRST_COL <- max(which(!is.na(unlist(existing_headers)))) + 1L
orig_last <- openxlsx2::int2col(ME_FIRST_COL - 1L)   # last existing column (for styling)
me_col_letters <- openxlsx2::int2col(ME_FIRST_COL + seq_along(me_labels) - 1L)
me_first <- me_col_letters[1L]
me_last <- me_col_letters[length(me_col_letters)]

# Indicator headers (row 4) and per-facility values (rows 5:35), in template-row order
wb$add_data(
  sheet = SHEET, x = data.frame(t(me_labels)),
  dims = paste0(me_first, header_row), col_names = FALSE
)
wb$add_data(
  sheet = SHEET, x = row_map[, ..me_labels],
  dims = paste0(me_first, min(DATA_ROWS)), col_names = FALSE
)

# "M&E Data" group banner across the indicator columns, mirroring the title row
wb$merge_cells(sheet = SHEET, dims = paste0(me_first, '1:', me_last, '1'))
wb$add_data(sheet = SHEET, x = 'M&E Data', dims = paste0(me_first, '1'), col_names = FALSE)

# Match existing formatting: banner -> title style, headers -> header style, data ->
# data-cell style; then apply a percentage number format to the rate indicators.
wb$set_cell_style(
  sheet = SHEET, dims = paste0(me_first, '1:', me_last, '1'),
  style = wb$get_cell_style(sheet = SHEET, dims = 'A1')
)
wb$set_cell_style(
  sheet = SHEET, dims = paste0(me_first, header_row, ':', me_last, header_row),
  style = wb$get_cell_style(sheet = SHEET, dims = paste0(orig_last, header_row))
)
wb$set_cell_style(
  sheet = SHEET, dims = paste0(me_first, min(DATA_ROWS), ':', me_last, max(DATA_ROWS)),
  style = wb$get_cell_style(sheet = SHEET, dims = paste0(orig_last, min(DATA_ROWS)))
)
for(k in which(me_is_percent)){
  pct_col <- me_col_letters[k]
  wb$add_numfmt(
    sheet = SHEET,
    dims = paste0(pct_col, min(DATA_ROWS), ':', pct_col, max(DATA_ROWS)),
    numfmt = '0.0%'
  )
}
for(k in which(!me_is_percent)){
  count_col <- me_col_letters[k]
  wb$add_numfmt(
    sheet = SHEET,
    dims = paste0(count_col, min(DATA_ROWS), ':', count_col, max(DATA_ROWS)),
    numfmt = COUNT_NUMFMT
  )
}
wb$set_col_widths(sheet = SHEET, cols = ME_FIRST_COL + seq_along(me_labels) - 1L, widths = 20)

## Populate the "Facility Assessment" tab --------------------------------------------->

# Indicator codes are parsed from the bracketed [code] in each row-5 header and pulled
# from the same-named variable in the Main facility-assessment file.
fa_sheet <- config$get('facility_assessment_sheet')
fa_col_a <- openxlsx2::wb_to_df(wb, sheet = fa_sheet, dims = 'A1:A40', col_names = FALSE)[[1]]
fa_header_row <- which(fa_col_a == 'Facility')
fa_data_rows <- which(!is.na(fa_col_a) & seq_along(fa_col_a) > fa_header_row)

fa_headers <- as.character(unlist(openxlsx2::wb_to_df(
  wb, sheet = fa_sheet, dims = paste0('A', fa_header_row, ':Z', fa_header_row),
  col_names = FALSE
)))
has_code <- grepl('\\[[a-z0-9]+\\]', fa_headers)
fa_codes <- gsub('\\[|\\]', '', regmatches(fa_headers, regexpr('\\[[a-z0-9]+\\]', fa_headers)))
fa_first_col <- min(which(has_code))   # indicator columns are contiguous from here

# Read only the needed columns from the Main facility-assessment file (value labels
# decode 17 of the 20 indicators to text; the other three are recoded below)
fa_main <- data.table::as.data.table(readstata13::read.dta13(
  path.expand(config$get_file_path('facility_surveys', 'facility_assessment_main')),
  convert.factors = TRUE, select.cols = c('facility_name', fa_codes)
))

# Decode the three indicators that lack usable value labels in the .dta. e02 and e04 use
# the "Observed / Reported, not seen / No / DK" scheme from the FA Data Dictionary tab,
# with the code order taken from the file's `doc_obs` label set (0=No, 1=observed,
# 2=reported-not-seen) and 98=Don't know. e07c4 has no codes in either the data or the
# dictionary, so 1=Yes remains an inference - confirm with the data team.
recode_obs <- c('0' = 'No', '1' = 'Observed', '2' = 'Reported, not seen', '98' = "Don't know")
if('e02' %in% names(fa_main))   fa_main[, e02 := unname(recode_obs[as.character(e02)]) ]
if('e04' %in% names(fa_main))   fa_main[, e04 := unname(recode_obs[as.character(e04)]) ]
if('e07c4' %in% names(fa_main)) fa_main[, e07c4 := data.table::fifelse(e07c4 == 1L, 'Yes', NA_character_) ]
message(
  'Facility Assessment: decoded e02/e04 per the FA Data Dictionary ',
  '(0=No, 1=Observed, 2=Reported, not seen, 98=Don\'t know); e07c4 1=Yes still inferred ',
  '(no codes in data or dictionary) - confirm with data team.'
)

# Decoded indicators as text, one row per our facility (joined via assessment_name)
fa_main[, (fa_codes) := lapply(.SD, as.character), .SDcols = fa_codes]
fa_decoded <- merge(
  crosswalk[, .(template_name, assessment_name)],
  fa_main, by.x = 'assessment_name', by.y = 'facility_name', all.x = TRUE
)
no_assessment <- crosswalk[is.na(assessment_name) | assessment_name == '', template_name]
if(length(no_assessment) > 0L){
  message('Facility Assessment: no submission for ', paste(no_assessment, collapse = ', '),
          ' (left blank).')
}

# Map the tab's facility rows to our facilities (col A uses template/output labels)
fa_name_lookup <- unique(data.table::rbindlist(list(
  crosswalk[, .(name = template_name, template_name)],
  crosswalk[, .(name = output_name, template_name)]
)))
fa_rows <- data.table(excel_row = fa_data_rows, name = fa_col_a[fa_data_rows])
fa_rows[fa_name_lookup, template_name := i.template_name, on = 'name']
fa_unmatched <- fa_rows[is.na(template_name), name]
if(length(fa_unmatched) > 0L){
  stop('Facility Assessment rows not matched to crosswalk: ',
       paste(fa_unmatched, collapse = ', '))
}
fa_rows <- merge(
  fa_rows, fa_decoded[, c('template_name', fa_codes), with = FALSE],
  by = 'template_name', all.x = TRUE, sort = FALSE
)
data.table::setorder(fa_rows, excel_row)

# Write the indicator block (categorical values; no numeric formatting needed)
wb$add_data(
  sheet = fa_sheet, x = fa_rows[, ..fa_codes],
  dims = paste0(openxlsx2::int2col(fa_first_col), min(fa_data_rows)), col_names = FALSE
)

# Apply the same facility renames as the Facility stats tab (output_name) to column A
fa_rows[crosswalk, output_name := i.output_name, on = 'template_name']
fa_name_fixes <- fa_rows[output_name != name, ]
for(i in seq_len(nrow(fa_name_fixes))){
  wb$add_data(
    sheet = fa_sheet, x = fa_name_fixes$output_name[i],
    dims = paste0('A', fa_name_fixes$excel_row[i]), col_names = FALSE
  )
}

## Save -------------------------------------------------------------------------------->

out_fp <- path.expand(config$get_file_path('facility_stats_output', 'facility_stats_filled'))
dir.create(dirname(out_fp), recursive = TRUE, showWarnings = FALSE)
openxlsx2::wb_save(wb, file = out_fp, overwrite = TRUE)
message('Wrote filled facility stats table to: ', out_fp)
