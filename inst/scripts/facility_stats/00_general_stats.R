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
    facility_id, facility_name, facility_type, health_authority, catchment_id
  )],
  by = 'facility_id',
  all.x = TRUE
)
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

# Warn about any missing values before writing
report_cols <- c(
  'facility_type', 'health_authority', 'pop_total', 'prev15to49_mean',
  'art_cohort_size', 'nearest_facility_km', 'n_within_10km'
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

# Columns D:J in template order: type, management, population, prevalence, ART cohort,
# nearest-facility distance, facilities within 10 km
stats_block <- row_map[, .(
  facility_type, health_authority, pop_total, prev15to49_mean,
  art_cohort_size, nearest_facility_km, n_within_10km
)]
# Use the in-place method form: the functional wb_add_data() returns a clone instead
# of modifying `wb`, so writes would otherwise be lost.
wb$add_data(
  sheet = SHEET, x = stats_block,
  dims = paste0('D', min(DATA_ROWS)), col_names = FALSE
)

# Correct facility display names where the crosswalk overrides the template label
name_fixes <- row_map[output_name != template_name, ]
for(i in seq_len(nrow(name_fixes))){
  wb$add_data(
    sheet = SHEET, x = name_fixes$output_name[i],
    dims = paste0('A', name_fixes$excel_row[i]), col_names = FALSE
  )
}

## Save -------------------------------------------------------------------------------->

out_fp <- path.expand(config$get_file_path('facility_stats_output', 'facility_stats_filled'))
dir.create(dirname(out_fp), recursive = TRUE, showWarnings = FALSE)
openxlsx2::wb_save(wb, file = out_fp, overwrite = TRUE)
message('Wrote filled facility stats table to: ', out_fp)
