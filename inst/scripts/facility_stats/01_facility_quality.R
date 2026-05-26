## #####################################################################################
##
## PURPOSE: Fill the first two tabs of the RESPOND facility quality template by
##   aggregating the client exit survey (client-level) to the profiled facilities:
##     - "Client exit survey - main": sample size, median wait time, % "no problem"
##       service-quality items, satisfaction, and community-reputation complaints.
##     - "Service receipt": HTS and ART client counts, counselling/VL receipt rates,
##       and median counselling time / years on ART.
##
##   Aggregation rules follow the template's "Data dictionary" tab. Number formatting
##   matches 00_general_stats.R (counts #,##0; percentages 0.0%; medians one decimal).
##   The filled workbook is saved to the versioned Facility Statistics output.
##
##   All input/output paths and parameters live in config.yaml.
##
## #####################################################################################

REPO_DIR <- '~/repos/mwi.hiv.factors'

## Setup ------------------------------------------------------------------------------->

devtools::load_all(REPO_DIR)
library(data.table)

config <- versioning::Config$new(file.path(REPO_DIR, 'config.yaml'))

MAIN_SHEET <- config$get('facility_quality_main_sheet')
SERVICE_SHEET <- config$get('facility_quality_service_sheet')
COUNT_NUMFMT <- '#,##0'      # counts: thousands separators, no decimals
PCT_NUMFMT <- '0.0%'         # percentages (stored as proportions): one-decimal percent
MEDIAN_NUMFMT <- '0.0'       # medians: one decimal place

## Load inputs ------------------------------------------------------------------------->

crosswalk <- fread(path.expand(config$get_file_path('repo', 'facility_stats_crosswalk')))

# Client exit survey (one row per interviewed client; raw codes for the == comparisons)
clients <- data.table::as.data.table(readstata13::read.dta13(
  path.expand(config$get_file_path('facility_surveys', 'client_exit_survey')),
  convert.factors = FALSE
))
# The survey's free-text facility names match our crosswalk assessment_name
clients[crosswalk, template_name := i.template_name, on = c(facility = 'assessment_name')]
clients <- clients[!is.na(template_name)]

## Aggregation helpers ----------------------------------------------------------------->

# % "no problem": response = 'No problem' (3) over valid responses Major/Minor/No problem
pct_no_problem <- function(x){
  denom <- sum(x %in% c(1L, 2L, 3L))
  if(denom == 0L) NA_real_ else sum(x == 3L, na.rm = TRUE) / denom
}
# Proportion equal to `value` over a supplied denominator (e.g. all clients, HTS, ART)
pct_eq <- function(x, value, denom){
  if(is.na(denom) || denom == 0L) NA_real_ else sum(x == value, na.rm = TRUE) / denom
}
median_na <- function(x) if(all(is.na(x))) NA_real_ else as.double(stats::median(x, na.rm = TRUE))

# Most common reason for dissatisfaction (d07) among not-satisfied (d05 == 3) clients
D07_LABELS <- c(
  '1' = 'Inconvenient operating hours', '2' = 'Service availability not reliable',
  '3' = 'Rude personnel', '4' = 'No medicine', '5' = 'Not private',
  '6' = 'Expensive', '99' = 'Other'
)
top_reason <- function(d07, d05){
  codes <- d07[d05 == 3L & !is.na(d07)]
  if(length(codes) == 0L) return(NA_character_)
  modal <- names(sort(table(codes), decreasing = TRUE))[1]
  unname(D07_LABELS[modal])
}

## Tab 1 - "Client exit survey - main" ------------------------------------------------->
## Columns C:V; satisfaction/complaint denominators are all completed interviews.
main_agg <- clients[, .(
  n_interviews = .N,
  wait_time    = median_na(b01b),
  d01g = pct_no_problem(d01g),
  d01h = pct_no_problem(d01h),
  d01b = pct_no_problem(d01b),
  d01c = pct_no_problem(d01c),
  d01j = pct_no_problem(d01j),
  d01d = pct_no_problem(d01d),
  d01e = pct_no_problem(d01e),
  d01k = pct_no_problem(d01k),
  very_satisfied = pct_eq(d05, 1L, .N),
  not_satisfied  = pct_eq(d05, 3L, .N),
  recommend      = pct_eq(d06, 1L, .N),
  top_reason     = top_reason(d07, d05),
  e01 = pct_eq(e01, 1L, .N),
  e02 = pct_eq(e02, 1L, .N),
  e03 = pct_eq(e03, 1L, .N),
  e04 = pct_eq(e04, 1L, .N),
  e05 = pct_eq(e05, 1L, .N),
  e06 = pct_eq(e06, 1L, .N)
), by = template_name]
main_cols <- setdiff(names(main_agg), 'template_name')

## Tab 2 - "Service receipt" ----------------------------------------------------------->
## Columns C:N; HTS items use the HTS-client denominator, ART items the ART denominator.
service_agg <- clients[, {
  n_hts <- sum(a07b == 1L, na.rm = TRUE)
  n_art <- sum(a07b == 2L, na.rm = TRUE)
  is_hts <- a07b == 1L
  is_art <- a07b == 2L
  .(
    n_hts        = n_hts,
    b02a = pct_eq(b02a[is_hts], 1L, n_hts),
    b02c = pct_eq(b02c[is_hts], 1L, n_hts),
    b02d = pct_eq(b02d[is_hts], 1L, n_hts),
    b02e = pct_eq(b02e[is_hts], 1L, n_hts),
    hts_counsel_time = median_na(b03[is_hts]),
    n_art        = n_art,
    c02a = pct_eq(c02a[is_art], 1L, n_art),
    c02d = pct_eq(c02d[is_art], 1L, n_art),
    c02e = pct_eq(c02e[is_art], 1L, n_art),
    art_counsel_time = median_na(c03[is_art]),
    years_on_art     = median_na(f04[is_art])
  )
}, by = template_name]
service_cols <- setdiff(names(service_agg), 'template_name')
# NB: b02d ("% linked to ART") uses the full HTS denominator; the dictionary flags that a
# HIV-negative-client denominator may be preferred. Confirm with the data team.

## Write a quality tab, preserving formatting ----------------------------------------->
## Maps the tab's facility rows to our facilities (col A uses template/output labels),
## writes the indicator block from C onward, applies the output_name renames, and sets
## number formats inferred from each header (% -> percent, Median -> one decimal,
## n / [n] -> count).
write_quality_tab <- function(wb, sheet, agg, value_cols){
  col_a <- openxlsx2::wb_to_df(wb, sheet = sheet, dims = 'A1:A40', col_names = FALSE)[[1]]
  header_row <- which(col_a == 'Facility')
  data_rows <- which(!is.na(col_a) & seq_along(col_a) > header_row)
  first_data <- min(data_rows)
  last_data <- max(data_rows)
  first_col <- 3L   # indicators begin in column C (after Facility, Profile)
  last_col <- first_col + length(value_cols) - 1L

  name_lookup <- unique(data.table::rbindlist(list(
    crosswalk[, .(name = template_name, template_name)],
    crosswalk[, .(name = output_name, template_name)]
  )))
  rows <- data.table(excel_row = data_rows, name = col_a[data_rows])
  rows[name_lookup, template_name := i.template_name, on = 'name']
  unmatched <- rows[is.na(template_name), name]
  if(length(unmatched) > 0L){
    stop(sheet, ': facilities not matched to crosswalk: ', paste(unmatched, collapse = ', '))
  }
  rows <- merge(rows, agg, by = 'template_name', all.x = TRUE, sort = FALSE)
  data.table::setorder(rows, excel_row)

  # Indicator block
  wb$add_data(
    sheet = sheet, x = rows[, ..value_cols],
    dims = paste0(openxlsx2::int2col(first_col), first_data), col_names = FALSE
  )

  # Apply the same facility renames as the other tabs (output_name) to column A
  rows[crosswalk, output_name := i.output_name, on = 'template_name']
  name_fixes <- rows[output_name != name, ]
  for(i in seq_len(nrow(name_fixes))){
    wb$add_data(
      sheet = sheet, x = name_fixes$output_name[i],
      dims = paste0('A', name_fixes$excel_row[i]), col_names = FALSE
    )
  }

  # Number formats inferred from each indicator header
  headers <- as.character(unlist(openxlsx2::wb_to_df(
    wb, sheet = sheet,
    dims = paste0('A', header_row, ':', openxlsx2::int2col(last_col), header_row),
    col_names = FALSE
  )))
  for(k in seq_along(value_cols)){
    h <- headers[first_col + k - 1L]
    fmt <- if(grepl('%', h)) PCT_NUMFMT
      else if(grepl('Median', h)) MEDIAN_NUMFMT
      else if(grepl('^n |\\[n\\]|interviews completed', h)) COUNT_NUMFMT
      else NA_character_   # text columns (e.g. top reason) get no numeric format
    if(!is.na(fmt)){
      col <- openxlsx2::int2col(first_col + k - 1L)
      wb$add_numfmt(sheet = sheet, dims = paste0(col, first_data, ':', col, last_data),
                    numfmt = fmt)
    }
  }
  invisible(wb)
}

## Fill the template and save ---------------------------------------------------------->

template_fp <- path.expand(config$get_file_path('facility_surveys', 'facility_quality_template'))
wb <- openxlsx2::wb_load(template_fp)

write_quality_tab(wb, MAIN_SHEET, main_agg, main_cols)
write_quality_tab(wb, SERVICE_SHEET, service_agg, service_cols)

no_clients <- setdiff(crosswalk$template_name, clients$template_name)
if(length(no_clients) > 0L){
  message('Facility quality: no client-exit data for ', paste(no_clients, collapse = ', '),
          ' (left blank).')
}

out_fp <- path.expand(config$get_file_path('facility_stats_output', 'facility_quality_filled'))
dir.create(dirname(out_fp), recursive = TRUE, showWarnings = FALSE)
openxlsx2::wb_save(wb, file = out_fp, overwrite = TRUE)
message('Wrote filled facility quality tables to: ', out_fp)
