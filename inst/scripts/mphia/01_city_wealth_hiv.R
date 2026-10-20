## #######################################################################################
##
## MPHIA 01. HIV PREVALENCE BY WITHIN-CITY WEALTH QUINTILE IN BLANTYRE AND LILONGWE
##
## AUTHOR: Nat Henry, nat@henryspatialanalysis.com
## CREATED: 2026-10-19
## PURPOSE: Replicate the MPHIA 2015-16 column of Burke et al. (AIDS 2022), which found
##   higher HIV prevalence in poorer parts of Blantyre, and repeat the analysis for
##   Blantyre and Lilongwe in MPHIA 2015-16 and 2020-21.
##
##   Design, following Burke et al.:
##   - Study area: the city plus its surrounding rural district, with clusters assigned
##     by geomasked centroid to Naomi polygons
##   - Adults aged 15-64 with a final HIV test result, weighted by the blood test weight
##   - Wealth quintiles of the household wealth score, cut within each study area and
##     survey across households with a tested adult, weighted by the household weight
##   - Standard errors treat the study area as its own survey: clusters as PSUs, no
##     strata, Taylor linearisation
##
##   A sensitivity analysis repeats the estimates using only clusters inside the city
##   boundary, with wealth quintiles cut among city households.
##
##   Also links MPHIA clusters in the Lilongwe study area to the community profiles of
##   the facility catchments that contain their geomasked centroids.
##
##   Outputs (mphia_analysis directory):
##   - mphia_city_wealth_hiv_estimates.csv: prevalence by subgroup, scope, city, survey
##   - mphia_city_wealth_hiv_trends.csv: odds ratios per wealth quintile
##   - mphia_lilongwe_profile_estimates.csv: prevalence and wealth by community profile
##   - mphia_city_wealth_hiv.xlsx: formatted tables
##   - mphia_city_wealth_hiv_district_vs_city.xlsx: district and city-only results
##
## #######################################################################################

REPO_DIR <- '~/repos/mwi.hiv.factors'


## SETUP -------------------------------------------------------------------------------->

load_pkgs <- c('data.table', 'sf', 'survey', 'versioning')
lapply(load_pkgs, library, character.only = TRUE) |> invisible()
devtools::load_all(REPO_DIR, quiet = TRUE)

config <- versioning::Config$new(file.path(REPO_DIR, 'config.yaml'))
settings <- config$get('mphia')
paper <- config$get('paper')
sf::sf_use_s2(FALSE)

out_path <- function(key){
  path <- config$get_file_path('mphia_analysis', key)
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  return(path)
}

age_breaks <- unlist(settings$age_breaks)
age_labels <- paste0(head(age_breaks, -1), '-', tail(age_breaks, -1) - 1)
quintile_labels <- c('Poorest', 'Poorer', 'Middle', 'Richer', 'Richest')
survey_ids <- names(settings$surveys)
cities <- names(settings$cities)


## LOAD SURVEYS AND ASSIGN STUDY AREAS -------------------------------------------------->

admin_bounds <- config$read('catchments', 'admin_bounds', quiet = TRUE)
areas_sf <- admin_bounds[admin_bounds$area_level == settings$area_level, 'area_name']
area_lookup <- lapply(cities, function(city){
  data.table::data.table(
    city = city,
    location = names(settings$cities[[city]]),
    area_name = unlist(settings$cities[[city]])
  )
}) |> data.table::rbindlist()

adults <- lapply(survey_ids, function(survey_id){
  survey <- settings$surveys[[survey_id]]
  raw_path <- function(key){
    file.path(config$get_dir_path('mphia_raw'), survey$raw_dir, survey[[key]])
  }
  dat <- read_mphia_survey(
    bio_path = raw_path('bio'),
    hh_path = raw_path('hh'),
    centroids_path = raw_path('centroids'),
    age_range = unlist(settings$age_range)
  )
  dat[, area_name := assign_mphia_clusters(dat, areas_sf, 'area_name')]
  message(
    survey$label, ': ', nrow(dat), ' tested adults; ',
    dat[is.na(area_name), uniqueN(centroidid)], ' clusters outside all areas'
  )
  dat <- merge(dat, area_lookup, by = 'area_name')
  dat[, `:=`(survey_id = survey_id, survey = survey$label)]
  return(dat)
}) |> data.table::rbindlist()

adults[, age_group := cut(
  age, breaks = age_breaks, labels = age_labels, right = FALSE
) |> as.character()]
# Quintiles are cut separately within each city's study area and survey
adults[, wealth_quintile := mphia_wealth_quintiles(.SD), by = .(city, survey_id)]
adults[, `:=`(
  location = factor(location, levels = c('City', 'Rural')),
  sex = factor(sex, levels = c('Female', 'Male')),
  age_group = factor(age_group, levels = age_labels)
)]


## ESTIMATES BY SCOPE, CITY, AND SURVEY ------------------------------------------------->

# Sensitivity analysis: the main scope is the full study area (city and rural district);
#  the second keeps only clusters inside the city boundary, with wealth quintiles cut
#  again among city households
city_adults <- adults[location == 'City']
city_adults[, wealth_quintile := mphia_wealth_quintiles(.SD), by = .(city, survey_id)]
scopes <- list(
  'District' = list(dat = adults, group_vars = c('location', 'sex', 'age_group')),
  'City only' = list(dat = city_adults, group_vars = c('sex', 'age_group'))
)
trend_models <- list(
  'Crude' = NULL,
  'Adjusted for age and sex' = c('sex', 'age_group'),
  'Adjusted for age, sex, and location' = c('sex', 'age_group', 'location')
)
estimates <- list()
trends <- list()
for(scope in names(scopes)){
  group_vars <- c(scopes[[scope]]$group_vars, 'wealth_quintile')
  for(city_name in cities){
    for(sid in survey_ids){
      sub <- scopes[[scope]]$dat[city == city_name & survey_id == sid]
      ids <- list(scope = scope, city = city_name, survey_id = sid, survey = sub$survey[1])
      est <- mphia_prevalence_table(sub, group_vars)
      est[, names(ids) := ids]
      estimates[[length(estimates) + 1]] <- est
      for(model_name in names(trend_models)){
        adjust_vars <- trend_models[[model_name]]
        if(!all(adjust_vars %in% group_vars)) next
        tr <- mphia_wealth_trend(sub, adjust_vars = adjust_vars)
        tr[, names(ids) := ids][, model := model_name]
        trends[[length(trends) + 1]] <- tr
      }
    }
  }
}
estimates <- data.table::rbindlist(estimates)
trends <- data.table::rbindlist(trends)
data.table::setcolorder(estimates, c('scope', 'city', 'survey_id', 'survey'))
data.table::setcolorder(trends, c('scope', 'city', 'survey_id', 'survey', 'model'))
data.table::fwrite(estimates, out_path('estimates'))
data.table::fwrite(trends, out_path('trends'))


## COMMUNITY PROFILES IN THE LILONGWE STUDY AREA ---------------------------------------->

vetted <- config$read(
  'analysis', 'vetted_profile_groupings', custom_version = paper$stage_versions$C
)
data.table::setnames(vetted, 1, 'catchment_id')
profile_order <- names(paper$profiles)
profile_levels <- c(
  paste0(seq_along(profile_order), '. ', unlist(paper$profiles)), 'Not profiled'
)
profile_lookup <- vetted[
  !profile_color %in% paper$excluded_profiles,
  .(catchment_id, profile = profile_levels[match(profile_color, profile_order)])
]
catchments_sf <- config$read('catchments', 'facility_catchments', quiet = TRUE)

profile_adults <- adults[city == settings$profile_city]
profile_adults[
  ,
  catchment_id := assign_mphia_clusters(profile_adults, catchments_sf, 'catchment_id')
]
profile_adults[profile_lookup, profile := i.profile, on = 'catchment_id']
profile_adults[is.na(profile), profile := 'Not profiled']

profile_estimates <- lapply(survey_ids, function(sid){
  sub <- profile_adults[survey_id == sid]
  design <- survey::svydesign(ids = ~centroidid, weights = ~weight, data = sub)
  prev <- survey::svyby(~hiv, ~profile, design, survey::svymean)
  wealth <- survey::svyby(
    ~factor(wealth_quintile, levels = 1:5), ~profile, design, survey::svymean
  )
  out <- data.table::data.table(
    profile = as.character(prev$profile),
    prev = as.numeric(stats::coef(prev)),
    se = as.numeric(survey::SE(prev))
  )
  shares <- data.table::as.data.table(wealth)[, c(1, 2:6), with = FALSE]
  data.table::setnames(shares, c('profile', paste0('wealth_q', 1:5)))
  counts <- sub[
    ,
    .(
      n_clusters = uniqueN(centroidid),
      n = .N,
      wtd = sum(weight),
      city_share = sum(weight * (location == 'City')) / sum(weight)
    ),
    by = profile
  ][, wtd_pct := wtd / sum(wtd)][, wtd := NULL]
  out <- merge(counts, out, by = 'profile') |> merge(shares, by = 'profile')
  out[, `:=`(survey_id = sid, survey = sub$survey[1], city = settings$profile_city)]
  return(out)
}) |> data.table::rbindlist()
profile_estimates[, profile := factor(profile, levels = profile_levels)]
data.table::setorder(profile_estimates, survey_id, profile)
data.table::setcolorder(profile_estimates, c('city', 'survey_id', 'survey', 'profile'))
data.table::fwrite(profile_estimates, out_path('profile_estimates'))


## FORMATTED WORKBOOK ------------------------------------------------------------------->

# Body rows shared by the results and replication tables: label, estimate variable,
#  level. Sections whose variable is missing from a set of estimates are dropped.
row_spec_all <- data.table::rbindlist(list(
  list('Clusters', 'Clusters', NA),
  list('Total N', 'Total', 'Total'),
  list('Location', 'location', NA),
  list('City', 'location', 'City'),
  list('Rural district', 'location', 'Rural'),
  list('Sex', 'sex', NA),
  list('Female', 'sex', 'Female'),
  list('Male', 'sex', 'Male'),
  list('Age', 'age_group', NA),
  data.table::data.table(age_labels, 'age_group', age_labels),
  list('Wealth quintile', 'wealth_quintile', NA),
  data.table::data.table(quintile_labels, 'wealth_quintile', as.character(1:5))
), use.names = FALSE)
data.table::setnames(row_spec_all, c('label', 'variable', 'level'))
row_spec_all[, section := is.na(level) & !variable %in% 'Clusters']
get_row_spec <- function(est){
  return(row_spec_all[variable %in% c('Clusters', unique(est$variable))])
}

# Two character columns, "N (wtd %)" and "HIV prev. (SE)", for one set of estimates.
#  The cluster count sits in the first column, with an #N/A placeholder in the second.
format_pair <- function(est, row_spec){
  out <- matrix(NA_character_, nrow = nrow(row_spec), ncol = 2)
  for(ii in seq_len(nrow(row_spec))){
    vv <- row_spec$variable[ii]
    if(row_spec$section[ii]) next
    if(vv == 'Clusters'){
      out[ii, ] <- c(as.character(est[variable == 'Total', n_clusters]), '=NA()')
      next
    }
    hit <- est[variable == vv & level == row_spec$level[ii]]
    if(nrow(hit) == 0){
      out[ii, ] <- '–'
    } else {
      out[ii, ] <- c(format_n_pct(hit$n, hit$wtd_pct), format_prev_se(hit$prev, hit$se))
    }
  }
  return(out)
}
trend_rows <- function(tr_list){
  # One row per model; each odds ratio spans its pair of columns
  models <- intersect(names(trend_models), unique(tr_list[[1]]$model))
  rows <- lapply(models, function(model_name){
    cells <- lapply(tr_list, function(tr){
      c(format_trend(tr[model == model_name]), NA_character_)
    }) |> unlist()
    c(model_name, cells)
  })
  return(rbind(c('Odds ratio per quintile (95% CI)', rep(NA, 2 * length(tr_list))),
    do.call(rbind, rows)))
}

# Results sheet for one scope: Blantyre and Lilongwe side by side, both surveys
col_sets <- data.table::CJ(city = cities, survey_id = survey_ids, sorted = FALSE)
n_sets <- nrow(col_sets)
results_header <- rbind(
  c('', as.vector(rbind(
    ifelse(!duplicated(col_sets$city), col_sets$city, NA), NA
  ))),
  c('', as.vector(rbind(
    vapply(col_sets$survey_id, function(s) settings$surveys[[s]]$label, ''), NA
  ))),
  c('', rep(c('N (wtd %)', 'HIV prev. (SE)'), n_sets))
)
add_results_sheet <- function(wb, sheet, scope_name, title){
  est <- estimates[scope == scope_name]
  tr <- trends[scope == scope_name]
  row_spec <- get_row_spec(est)
  body <- cbind(
    row_spec$label,
    do.call(cbind, lapply(seq_len(n_sets), function(ii){
      format_pair(
        est[city == col_sets$city[ii] & survey_id == col_sets$survey_id[ii]], row_spec
      )
    }))
  )
  body <- rbind(body, trend_rows(lapply(seq_len(n_sets), function(ii){
    tr[city == col_sets$city[ii] & survey_id == col_sets$survey_id[ii]]
  })))
  add_table_sheet(
    wb, sheet = sheet,
    title = title,
    header = results_header,
    body = body,
    section_rows = c(which(row_spec$section), nrow(row_spec) + 1L),
    col_widths = c(36, rep(15, 2 * n_sets))
  )
  return(invisible(wb))
}
main_title <- 'HIV prevalence by within-city wealth quintile, adults 15-64, MPHIA'

# Main workbook: district results, replication, Lilongwe profiles, numeric estimates
wb <- openxlsx2::wb_workbook()
add_results_sheet(wb, 'City results', 'District', main_title)

# Replication of Burke et al. 2022
rep_settings <- settings$replication
published <- data.table::fread(
  config$get_file_path('repo', 'burke_2022_published'), colClasses = list(
    character = c('variable', 'level')
  )
)
published[, n_clusters := published[variable == 'Clusters', n]]
published <- published[variable != 'Clusters']
replicated <- estimates[
  scope == 'District' & city == rep_settings$city & survey_id == rep_settings$survey
]
rep_row_spec <- get_row_spec(replicated)
rep_body <- cbind(
  rep_row_spec$label,
  format_pair(replicated, rep_row_spec),
  format_pair(published, rep_row_spec)
)
add_table_sheet(
  wb, sheet = 'Replication',
  title = paste(
    rep_settings$city, settings$surveys[[rep_settings$survey]]$label,
    ': this analysis compared with Burke et al. (AIDS 2022)'
  ),
  header = rbind(
    c('', 'This analysis', NA, 'Burke et al. 2022', NA),
    c('', 'N (wtd %)', 'HIV prev. (SE)', 'N (wtd %)', 'HIV prev. (SE)')
  ),
  body = rep_body,
  section_rows = which(rep_row_spec$section),
  col_widths = c(36, rep(15, 4))
)

# Community profiles in the Lilongwe study area
prof_body <- lapply(survey_ids, function(sid){
  pe <- profile_estimates[survey_id == sid]
  rbind(
    c(settings$surveys[[sid]]$label, rep(NA, 9)),
    cbind(
      as.character(pe$profile),
      as.character(pe$n_clusters),
      format_n_pct(pe$n, pe$wtd_pct),
      sprintf('%.0f', 100 * pe$city_share),
      # A single cluster gives no design-based standard error
      data.table::fifelse(
        pe$n_clusters > 1,
        format_prev_se(pe$prev, pe$se),
        sprintf('%.1f (\u2013)', 100 * pe$prev)
      ),
      vapply(
        1:5, function(q) sprintf('%.0f', 100 * pe[[paste0('wealth_q', q)]]),
        character(nrow(pe))
      )
    )
  )
})
prof_section_rows <- cumsum(c(1, vapply(head(prof_body, -1), nrow, 1L)))
prof_body <- do.call(rbind, prof_body)
add_table_sheet(
  wb, sheet = 'Lilongwe profiles',
  title = paste(
    'HIV prevalence and wealth by community profile,', settings$profile_city,
    'study area, adults 15-64'
  ),
  header = rbind(
    c('', 'Clusters', 'N (wtd %)', '% in city', 'HIV prev. (SE)',
      'Wealth quintile (wtd %)', NA, NA, NA, NA),
    c('', NA, NA, NA, NA, quintile_labels)
  ),
  body = prof_body,
  section_rows = prof_section_rows,
  col_widths = c(44, 10, 13, 10, 15, rep(10, 5))
)

# Numeric estimates
long <- estimates[
  scope == 'District',
  .(city, survey, variable, level, n, n_clusters, wtd_pct, prev, se)
]
wb$add_worksheet('Estimates')
wb$add_data(sheet = 'Estimates', x = long)
wb$add_numfmt(sheet = 'Estimates', dims = paste0('G2:I', nrow(long) + 1), numfmt = '0.000')
wb$add_font(sheet = 'Estimates', dims = 'A1:I1', bold = TRUE)
wb$set_col_widths(sheet = 'Estimates', cols = 1:9, widths = c(10, 15, 16, 10, 8, 10, 9, 9, 9))

wb$save(out_path('workbook'))
message('Wrote ', out_path('workbook'))

# Sensitivity workbook: full study area against city boundaries only
wb_sens <- openxlsx2::wb_workbook()
add_results_sheet(
  wb_sens, 'District results', 'District', paste0(main_title, ': city and rural district')
)
add_results_sheet(
  wb_sens, 'City only results', 'City only', paste0(main_title, ': city boundary only')
)
wb_sens$save(out_path('sensitivity_workbook'))
message('Wrote ', out_path('sensitivity_workbook'))
