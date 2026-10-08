## #######################################################################################
##
## DHS 05. SUMMARY TABLE OF WEALTH AND WORK INDICATORS BY COMMUNITY PROFILE
##
## AUTHOR: Nat Henry, nat@henryspatialanalysis.com
## CREATED: 2026-10-08
## PURPOSE: One-page Word table of sample sizes, wealth indicators, and work indicators
##   for profiles 1-5, from the outputs of 02_estimate_profile_indicators.R.
##
##   Every cell reads "Naive (Lower – Upper)":
##   - Naive: the value when each cluster is assigned to the catchment containing its
##     displaced GPS point (the sensitivity run in script 02)
##   - Lower – Upper: for counts, the 2.5th and 97.5th percentiles across location
##     imputations; for estimates, the pooled 95% CI from the displacement-aware
##     multiple imputation
##
##   Usage: Rscript 05_profile_summary_table.R <survey> (default 'mwi_2024').
##
## #######################################################################################

REPO_DIR <- '~/repos/mwi.hiv.factors'


## SETUP -------------------------------------------------------------------------------->

load_pkgs <- c('data.table', 'versioning')
lapply(load_pkgs, library, character.only = TRUE) |> invisible()
devtools::load_all(REPO_DIR, quiet = TRUE)

config <- versioning::Config$new(file.path(REPO_DIR, 'config.yaml'))
dhs_settings <- config$get('dhs')
survey_id <- commandArgs(trailingOnly = TRUE)[1]
if(is.na(survey_id)) survey_id <- 'mwi_2024'
survey <- dhs_settings$surveys[[survey_id]]
if(is.null(survey)) stop("Unknown survey: ", survey_id)
out_path <- function(key) dhs_survey_path(config, 'dhs_analysis', key, survey_id)

# Profile 6 (Rural/remote) is omitted: too few clusters for usable estimates
profiles_shown <- 1:5

estimates <- data.table::fread(out_path('estimates'))
naive <- data.table::fread(out_path('sensitivity'))
draws <- data.table::fread(out_path('estimates_by_draw'))
sample_sizes <- data.table::fread(out_path('sample_sizes'))
for(dt in list(estimates, naive, sample_sizes)){
  dt[, profile_num := as.integer(sub('\\..*$', '', profile))]
}
draws[, profile_num := as.integer(sub('\\..*$', '', domain))]


## CELL FORMATTING ---------------------------------------------------------------------->

en_dash <- '–'
fmt_cell <- function(point, lower, upper, digits){
  f <- function(x) formatC(x, format = 'f', digits = digits)
  sprintf('%s (%s %s %s)', f(point), f(lower), en_dash, f(upper))
}

# Counts: naive count with the 95% range across location imputations
count_row <- function(naive_counts, draw_counts){
  sapply(profiles_shown, function(pp){
    dc <- draw_counts[profile_num == pp, value]
    fmt_cell(
      naive_counts[profile_num == pp, value],
      round(stats::quantile(dc, 0.025)), round(stats::quantile(dc, 0.975)),
      digits = 0
    )
  })
}

# Estimates: naive estimate with the displacement-aware 95% CI
estimate_row <- function(this_indicator, this_group){
  sapply(profiles_shown, function(pp){
    mi <- estimates[indicator == this_indicator & group == this_group & profile_num == pp]
    nv <- naive[indicator == this_indicator & group == this_group & profile_num == pp]
    is_pct <- mi$type == 'proportion'
    scale <- if(is_pct) 100 else 1
    fmt_cell(
      nv$est * scale, mi$lower * scale, mi$upper * scale, digits = if(is_pct) 0 else 2
    )
  })
}


## TABLE CONTENT ------------------------------------------------------------------------>

household_draws <- draws[indicator == 'wealth_score' & group == 'Households']
adult_draws <- draws[indicator == 'worked_12m' & group == 'All adults']

rows <- list(
  list(type = 'row', label = 'N clusters', values = count_row(
    sample_sizes[, .(profile_num, value = naive_clusters)],
    household_draws[, .(profile_num, value = n_clusters)]
  )),
  list(type = 'section', label = 'DHS Household Wealth Index'),
  list(type = 'row', label = 'N households', values = count_row(
    naive[indicator == 'wealth_score' & group == 'Households', .(profile_num, value = n)],
    household_draws[, .(profile_num, value = n)]
  )),
  list(type = 'row', label = 'Mean', values = estimate_row('wealth_score', 'Households')),
  list(
    type = 'row', label = 'Gini (<em>national</em> zeroing)',
    values = estimate_row('wealth_gini', 'Households')
  ),
  list(
    type = 'row', label = 'Gini (<em>groupwise</em> zeroing)',
    values = estimate_row('wealth_gini_groupwise', 'Households')
  ),
  list(type = 'row', label = 'SD', values = estimate_row('wealth_sd', 'Households')),
  list(
    type = 'row', label = 'HH in poorest 40% nationwide',
    values = estimate_row('bottom_40', 'Households')
  ),
  list(
    type = 'row', label = 'HH in richest 20% nationwide',
    values = estimate_row('quintile_5', 'Households')
  ),
  list(type = 'section', label = 'Informal, seasonal, and inconsistent work'),
  list(type = 'row', label = 'N adults 15-49', values = count_row(
    naive[indicator == 'worked_12m' & group == 'All adults', .(profile_num, value = n)],
    adult_draws[, .(profile_num, value = n)]
  )),
  list(
    type = 'row', label = '% worked in last 12 months',
    values = estimate_row('worked_12m', 'All adults')
  ),
  list(
    type = 'row', label = '% non-wage work (working women)',
    values = estimate_row('nonwage_work', 'Women')
  ),
  list(
    type = 'row', label = '% <u>no</u> cash earnings (working adults)',
    values = estimate_row('no_cash_earnings', 'All adults')
  ),
  list(
    type = 'row', label = '% seasonal or occasional work (working adults)',
    values = estimate_row('seasonal_or_occasional', 'All adults')
  ),
  list(
    type = 'row', label = 'of which: seasonal',
    values = estimate_row('seasonal', 'All adults')
  ),
  list(
    type = 'row', label = 'of which: occasional',
    values = estimate_row('occasional', 'All adults')
  )
)


## WRITE CSV ---------------------------------------------------------------------------->

table_dt <- lapply(rows, function(r){
  vals <- if(r$type == 'row') r$values else rep('', length(profiles_shown))
  data.table::as.data.table(as.list(c(
    Indicator = gsub('<[^>]+>', '', r$label),
    stats::setNames(vals, as.character(profiles_shown))
  )))
}) |> data.table::rbindlist()
data.table::fwrite(table_dt, out_path('summary_table_csv'))


## WRITE WORD TABLE --------------------------------------------------------------------->

# Pandoc's default reference document, with gridlines added to the table style
make_reference_docx <- function(path){
  work_dir <- tempfile('refdocx_')
  dir.create(work_dir)
  default_docx <- file.path(work_dir, 'default.docx')
  system2(
    rmarkdown::pandoc_exec(),
    c('-o', default_docx, '--print-default-data-file', 'reference.docx')
  )
  unzip_dir <- file.path(work_dir, 'unzipped')
  utils::unzip(default_docx, exdir = unzip_dir)
  styles_path <- file.path(unzip_dir, 'word', 'styles.xml')
  styles <- readLines(styles_path, warn = FALSE, encoding = 'UTF-8') |>
    paste(collapse = '\n')
  border <- function(side){
    sprintf('<w:%s w:val="single" w:sz="4" w:space="0" w:color="808080"/>', side)
  }
  borders <- paste0(
    '<w:tblBorders>',
    paste(vapply(
      c('top', 'left', 'bottom', 'right', 'insideH', 'insideV'), border, character(1)
    ), collapse = ''),
    '</w:tblBorders>'
  )
  styles <- sub(
    '(?s)(<w:style w:type="table" w:default="1" w:styleId="Table">.*?<w:tblPr>)',
    paste0('\\1', borders), styles, perl = TRUE
  )
  writeLines(styles, styles_path, useBytes = TRUE)
  old_wd <- setwd(unzip_dir)
  on.exit(setwd(old_wd), add = TRUE)
  utils::zip(
    zipfile = normalizePath(path, mustWork = FALSE),
    files = list.files('.', recursive = TRUE, all.files = TRUE),
    flags = '-q -X'
  )
  invisible(path)
}

n_cols <- length(profiles_shown) + 1
header <- paste0(
  '<tr><th align="left">Indicator</th>',
  paste0('<th align="center">', profiles_shown, '</th>', collapse = ''),
  '</tr>'
)
body <- vapply(rows, function(r){
  if(r$type == 'section'){
    sprintf(
      '<tr><td colspan="%d" align="center"><strong>%s</strong></td></tr>',
      n_cols, r$label
    )
  } else {
    paste0(
      '<tr><td align="left">', r$label, '</td>',
      paste0('<td align="center">', r$values, '</td>', collapse = ''),
      '</tr>'
    )
  }
}, character(1))

profile_key <- sample_sizes[profile_num %in% profiles_shown][order(profile_num)]
caution <- estimates[
  indicator == 'wealth_score' & profile_num %in% profiles_shown &
    grepl('^Caution', reliability),
  sort(profile_num)
]
notes <- c(
  paste0(
    'Profiles: ', paste(profile_key$profile, collapse = '; '), '. Profile 6 ',
    '(Rural/remote) is omitted because too few DHS clusters fall in it.'
  ),
  paste0(
    'Each cell shows the naive value, with clusters assigned to the catchment ',
    'containing their displaced GPS point, followed by a 95% interval that accounts ',
    'for GPS displacement. For counts, the interval is the 2.5th to 97.5th ',
    'percentile across ', dhs_settings$n_imputations,
    ' imputations of cluster location. For estimates, it is ',
    'the pooled 95% confidence interval from those imputations, covering sampling and ',
    'location uncertainty. Naive values can fall outside the interval.'
  ),
  if(length(caution) > 0) paste0(
    'Fewer than 10 expected DHS clusters (interpret with caution): profile ',
    paste(caution, collapse = ', '), '.'
  ),
  paste0(
    'Wealth index: DHS wealth index factor score; national mean = 0. Gini with national ',
    'zeroing shifts scores so the poorest household nationally scores 0; groupwise ',
    'zeroing shifts by the poorest household in each profile (DHS report method). ',
    'Percentages of households and of adults are person- and survey-weighted.'
  ),
  'Work indicators: adults aged 15-49; non-wage work is reported for women only.'
)

html <- paste0(
  '<html><body>',
  '<p><strong>Household wealth and work by community profile, ', survey$label,
  '</strong></p>',
  '<table><thead>', header, '</thead><tbody>', paste(body, collapse = ''),
  '</tbody></table>',
  paste0('<p><small>', notes, '</small></p>', collapse = ''),
  '</body></html>'
)
html_path <- tempfile(fileext = '.html')
writeLines(html, html_path, useBytes = TRUE)
reference_docx <- tempfile(fileext = '.docx')
make_reference_docx(reference_docx)
rmarkdown::pandoc_convert(
  input = html_path,
  from = 'html',
  to = 'docx',
  output = normalizePath(out_path('summary_table_docx'), mustWork = FALSE),
  options = c('--reference-doc', reference_docx)
)
message('Wrote ', out_path('summary_table_docx'))
