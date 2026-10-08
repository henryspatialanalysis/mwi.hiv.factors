## #######################################################################################
##
## DHS 04. RENDER THE INTERNAL WRITEUP
##
## AUTHOR: Nat Henry, nat@henryspatialanalysis.com
## CREATED: 2026-10-08
## PURPOSE: Fill the writeup template (writeup_template.md, next to this script) with
##   tables built from the estimates, and render it to Markdown and Word in the
##   dhs_analysis output folder.
##
##   Template placeholders take the form {{name}}; each is replaced with a Markdown
##   table built below.
##
## #######################################################################################

REPO_DIR <- '~/repos/mwi.hiv.factors'


## SETUP -------------------------------------------------------------------------------->

load_pkgs <- c('data.table', 'versioning')
lapply(load_pkgs, library, character.only = TRUE) |> invisible()
devtools::load_all(REPO_DIR, quiet = TRUE)

config <- versioning::Config$new(file.path(REPO_DIR, 'config.yaml'))
paper <- config$get('paper')
surveys <- config$get('dhs', 'surveys')
out_dir <- config$get_dir_path('dhs_analysis')
template_path <- file.path(REPO_DIR, 'inst/scripts/dhs/writeup_template.md')
decision_log_path <- file.path(dirname(dirname(out_dir)), 'decision_log.md')
analysis_path <- function(key, survey_id){
  dhs_survey_path(config, 'dhs_analysis', key, survey_id)
}

profile_order <- names(paper$profiles)
profile_levels <- paste0(seq_along(profile_order), '. ', unlist(paper$profiles))


## TABLE HELPERS ------------------------------------------------------------------------>

md_table <- function(dt, align = NULL){
  dt <- as.data.frame(dt)
  if(is.null(align)) align <- c('l', rep('r', ncol(dt) - 1))
  sep <- vapply(align, function(a) if(a == 'r') '---:' else ':---', character(1))
  rows <- apply(dt, 1, function(r) paste0('| ', paste(r, collapse = ' | '), ' |'))
  paste(
    c(
      paste0('| ', paste(names(dt), collapse = ' | '), ' |'),
      paste0('|', paste(sep, collapse = '|'), '|'),
      rows
    ),
    collapse = '\n'
  )
}
format_estimate <- function(est, lower, upper, type, reliability){
  txt <- mapply(function(e, l, u, is_pct){
    d <- if(is_pct) 0 else 2
    s <- if(is_pct) 100 else 1
    sprintf(
      '%s (%s to %s)',
      formatC(e * s, format = 'f', digits = d),
      formatC(l * s, format = 'f', digits = d),
      formatC(u * s, format = 'f', digits = d)
    )
  }, est, lower, upper, type == 'proportion')
  marker <- data.table::fcase(
    grepl('^Unreliable|^Suppress', reliability), ' †',
    grepl('^Caution', reliability), ' *',
    default = ''
  )
  paste0(txt, marker)
}

# Profile-by-column table; rows whose indicator is not estimated for a survey are dropped
wide_table <- function(estimates, rows, value_col = 'cell'){
  rows <- rows[paste(indicator, group) %in% estimates[, paste(indicator, group)]]
  out <- lapply(seq_len(nrow(rows)), function(ii){
    r <- rows[ii, ]
    vals <- estimates[
      indicator == r$indicator & group == r$group
    ][order(profile_num), get(value_col)]
    data.table::as.data.table(
      as.list(c(Indicator = r$label, stats::setNames(vals, as.character(1:6))))
    )
  }) |> data.table::rbindlist()
  return(out)
}

headline_rows <- data.table::data.table(
  indicator = c(
    'wealth_score', 'bottom_40', 'quintile_5', 'wealth_gini', 'wealth_gini_groupwise',
    'wealth_sd', 'worked_12m', 'nonwage_work', 'nonwage_work', 'no_cash_earnings',
    'agriculture', 'seasonal_or_occasional', 'seasonal', 'occasional'
  ),
  group = c(
    rep('Households', 6), 'All adults', 'Women', 'Men', rep('All adults', 5)
  ),
  label = c(
    'Mean wealth score', '% in national poorest 40%', '% in national richest 20%',
    'Gini coefficient (wealth score, national zeroing)',
    'Gini coefficient (wealth score, groupwise zeroing)', 'SD of wealth score',
    '% worked in last 12 months', '% non-wage work (working women)',
    '% non-wage work (working men)',
    '% no cash earnings (working adults)', '% agricultural occupation (working adults)',
    '% seasonal or occasional work (working adults)', '  of which: seasonal',
    '  of which: occasional'
  )
)
sex_rows <- data.table::CJ(
  indicator = c(
    'worked_12m', 'nonwage_work', 'no_cash_earnings', 'agriculture',
    'seasonal_or_occasional'
  ),
  group = c('Women', 'Men'),
  sorted = FALSE
)
sex_rows[, label := paste0(
  c(
    worked_12m = '% worked in last 12 months', nonwage_work = '% non-wage work',
    no_cash_earnings = '% no cash earnings', agriculture = '% agricultural occupation',
    seasonal_or_occasional = '% seasonal or occasional work'
  )[indicator],
  ', ', tolower(group)
)]
full_df_rows <- headline_rows[indicator %in% c(
  'wealth_score', 'bottom_40', 'wealth_gini', 'wealth_gini_groupwise', 'nonwage_work',
  'no_cash_earnings', 'agriculture', 'seasonal_or_occasional'
)]


## TABLES FOR EACH SURVEY --------------------------------------------------------------->

load_estimates <- function(survey_id){
  estimates <- data.table::fread(analysis_path('estimates', survey_id))
  estimates[, cell := format_estimate(est, lower, upper, type, reliability)]
  estimates[, profile_num := as.integer(sub('\\..*$', '', profile))]
  estimates[, full_df_cell := mapply(function(l, u, is_pct){
    s <- if(is_pct) 100 else 1
    d <- if(is_pct) 0 else 2
    sprintf(
      '%s to %s', formatC(l * s, format = 'f', digits = d),
      formatC(u * s, format = 'f', digits = d)
    )
  }, lower_full_df, upper_full_df, type == 'proportion')]
  return(estimates[])
}
all_estimates <- lapply(names(surveys), load_estimates) |> stats::setNames(names(surveys))

survey_tables <- function(survey_id){
  estimates <- all_estimates[[survey_id]]
  sample_sizes <- data.table::fread(analysis_path('sample_sizes', survey_id))
  ss <- sample_sizes[match(profile_levels, profile)]
  profile_key <- md_table(
    data.table::data.table(
      `#` = seq_along(profile_levels),
      Profile = sub('^[0-9]+\\. ', '', profile_levels),
      `Expected clusters` = sprintf('%.1f', ss$expected_clusters),
      `Clusters (naive)` = ss$naive_clusters,
      Households = sprintf('%.0f', ss$mean_households),
      `Women 15-49` = sprintf('%.0f', ss$mean_women),
      `Men 15-49` = sprintf('%.0f', ss$mean_men),
      Reliability = estimates[indicator == 'wealth_score'][
        match(profile_levels, profile), reliability
      ]
    ),
    align = c('l', 'l', rep('r', 5), 'l')
  )
  list(
    profile_key = profile_key,
    headline_table = md_table(wide_table(estimates, headline_rows)),
    sex_table = md_table(wide_table(estimates, sex_rows)),
    full_df_table = md_table(wide_table(estimates, full_df_rows, 'full_df_cell'))
  )
}

# Indicators whose levels can be compared across surveys: work questions are asked the
#  same way, and the poorest-40% share is a position within each year's national
#  distribution
comparison_rows <- data.table::data.table(
  indicator = c(
    'bottom_40', 'worked_12m', 'nonwage_work', 'no_cash_earnings', 'agriculture',
    'seasonal_or_occasional'
  ),
  group = c(
    'Households', 'All adults', 'Women', 'All adults', 'All adults', 'All adults'
  ),
  label = c(
    '% in national poorest 40%', '% worked in last 12 months',
    '% non-wage work (working women)', '% no cash earnings (working adults)',
    '% agricultural occupation (working adults)',
    '% seasonal or occasional work (working adults)'
  )
)
survey_order <- names(surveys)[order(vapply(surveys, `[[`, '', 'short_label'))]
comparison_table <- lapply(seq_len(nrow(comparison_rows)), function(ii){
  lapply(survey_order, function(sid){
    wide_table(all_estimates[[sid]], comparison_rows[ii])[
      , Indicator := paste0(Indicator, ', ', surveys[[sid]]$short_label)
    ]
  }) |> data.table::rbindlist()
}) |>
  data.table::rbindlist() |>
  md_table()


## FILL TEMPLATE AND RENDER ------------------------------------------------------------->

template <- readLines(template_path, warn = FALSE) |> paste(collapse = '\n')
decisions <- if(file.exists(decision_log_path)){
  readLines(decision_log_path, warn = FALSE)[-1] |> paste(collapse = '\n')
} else ''
replacements <- list(
  comparison_table = comparison_table,
  decision_log = decisions,
  n_imputations = as.character(config$get('dhs', 'n_imputations'))
)
for(sid in names(surveys)){
  tables <- survey_tables(sid)
  names(tables) <- paste0(names(tables), '_', sid)
  replacements <- c(replacements, tables)
}
filled <- template
for(nm in names(replacements)){
  filled <- gsub(paste0('{{', nm, '}}'), replacements[[nm]], filled, fixed = TRUE)
}
unfilled <- regmatches(filled, gregexpr('\\{\\{[a-z0-9_]+\\}\\}', filled))[[1]]
if(length(unfilled) > 0){
  stop("Unfilled template fields: ", paste(unfilled, collapse = ', '))
}
md_path <- config$get_file_path('dhs_analysis', 'writeup_md')
writeLines(filled, md_path)
rmarkdown::pandoc_convert(
  input = md_path,
  to = 'docx',
  output = config$get_file_path('dhs_analysis', 'writeup_docx'),
  options = c('--resource-path', out_dir, '--standalone'),
  wd = out_dir
)
message('Rendered writeup to ', out_dir)
