## #######################################################################################
##
## DHS 02. DIRECT ESTIMATES OF WEALTH AND WORK INDICATORS BY COMMUNITY PROFILE
##
## AUTHOR: Nat Henry, nat@henryspatialanalysis.com
## CREATED: 2026-10-08
## PURPOSE: Design-based domain estimates of household wealth, wealth inequality,
##   informal (non-wage) work, and seasonal work for each final community profile, from
##   one Malawi DHS survey.
##
##   Profiles are unplanned domains, and DHS cluster coordinates are displaced. Cluster
##   membership in profile catchments is multiply imputed from the posterior location
##   probabilities made in 01_prepare_dhs.R. Each imputation yields Taylor-linearised
##   domain estimates on the full national design; estimates and pairwise profile
##   contrasts are combined across imputations with Rubin's rules.
##
##   A sensitivity analysis assigns each cluster to the catchment containing its
##   displaced point, ignoring displacement.
##
##   Usage: Rscript 02_estimate_profile_indicators.R <survey> (default 'mwi_2024').
##
## #######################################################################################

REPO_DIR <- '~/repos/mwi.hiv.factors'


## SETUP -------------------------------------------------------------------------------->

load_pkgs <- c('data.table', 'survey', 'convey', 'parallel', 'versioning')
lapply(load_pkgs, library, character.only = TRUE) |> invisible()
devtools::load_all(REPO_DIR, quiet = TRUE)
options(survey.lonely.psu = 'adjust', survey.adjust.domain.lonely = TRUE)

config <- versioning::Config$new(file.path(REPO_DIR, 'config.yaml'))
dhs_settings <- config$get('dhs')
survey_id <- commandArgs(trailingOnly = TRUE)[1]
if(is.na(survey_id)) survey_id <- 'mwi_2024'
if(is.null(dhs_settings$surveys[[survey_id]])) stop("Unknown survey: ", survey_id)
in_path <- function(key) dhs_survey_path(config, 'dhs_prepared', key, survey_id)
out_path <- function(key) dhs_survey_path(config, 'dhs_analysis', key, survey_id)
set.seed(dhs_settings$seed)
# DHS_N_IMP overrides the configured number of imputations for quick test runs
n_imputations <- as.integer(Sys.getenv('DHS_N_IMP', dhs_settings$n_imputations))
n_cores <- max(1L, min(8L, parallel::detectCores() - 2L))

households <- data.table::fread(in_path('households'))
adults <- data.table::fread(in_path('adults'))
location_probs <- data.table::fread(in_path('location_probs'))
location_probs[profile == '', profile := NA_character_]
profile_catchments <- data.table::fread(in_path('profile_catchments'))
clusters <- sf::st_read(in_path('clusters'), quiet = TRUE) |>
  sf::st_drop_geometry() |>
  data.table::as.data.table()
profile_levels <- sort(unique(profile_catchments$profile))


## INDICATOR DEFINITIONS ---------------------------------------------------------------->

indicator_specs <- data.table::rbindlist(list(
  list('wealth_score', 'households', 'Households', 'mean', NA_character_),
  list('bottom_40', 'households', 'Households', 'proportion', NA_character_),
  list('wealth_gini', 'households', 'Households', 'gini', NA_character_),
  list('wealth_gini_groupwise', 'households', 'Households', 'gini', NA_character_),
  list('wealth_sd', 'households', 'Households', 'sd', NA_character_),
  list('quintile_1', 'households', 'Households', 'proportion', NA_character_),
  list('quintile_2', 'households', 'Households', 'proportion', NA_character_),
  list('quintile_3', 'households', 'Households', 'proportion', NA_character_),
  list('quintile_4', 'households', 'Households', 'proportion', NA_character_),
  list('quintile_5', 'households', 'Households', 'proportion', NA_character_)
))
data.table::setnames(
  indicator_specs, c('indicator', 'data', 'group', 'type', 'denominator')
)
adult_indicators <- c(
  'worked_12m', 'nonwage_work', 'no_cash_earnings', 'agriculture',
  'seasonal_or_occasional', 'seasonal', 'occasional'
)
adult_specs <- data.table::CJ(
  indicator = adult_indicators, group = c('All adults', 'Women', 'Men'), sorted = FALSE
)[, `:=` (
  data = 'adults',
  type = 'proportion',
  denominator = data.table::fifelse(indicator == 'worked_12m', NA_character_, 'is_worker')
)]
# The full non-wage definition needs the employer question; when the men's
#  questionnaire omits it, non-wage work is estimated for women only
if(all(is.na(adults[sex == 'Men', nonwage_work]))){
  adult_specs <- adult_specs[!(indicator == 'nonwage_work' & group != 'Women'), ]
}
indicator_specs <- data.table::rbindlist(
  list(indicator_specs, adult_specs), use.names = TRUE
)
indicator_specs[, var := data.table::fcase(
  indicator == 'wealth_gini', 'wealth_score_shifted',
  indicator == 'wealth_gini_groupwise', 'wealth_score_groupwise',
  indicator == 'wealth_sd', 'wealth_score',
  default = indicator
)]
indicator_specs[, transform := data.table::fcase(
  type == 'proportion', 'beta',
  type == 'gini', 'logit',
  type == 'sd', 'log',
  default = 'identity'
)]


## ESTIMATION FOR ONE SET OF CLUSTER ASSIGNMENTS ---------------------------------------->

#' @param cluster_profiles data.table with fields `cluster`, `profile` (NA = none)
estimate_one_assignment <- function(cluster_profiles){
  hh <- data.table::copy(households)
  hh$profile <- cluster_profiles$profile[match(hh$cluster, cluster_profiles$cluster)]
  # Groupwise zeroing (as in DHS reports): shift scores so that the poorest sampled
  #  household in each profile scores zero. Recomputed for each imputation, since
  #  profile membership changes between imputations
  hh[, wealth_score_groupwise := wealth_score - min(wealth_score), by = profile]
  ad <- data.table::copy(adults)
  ad$profile <- cluster_profiles$profile[match(ad$cluster, cluster_profiles$cluster)]

  designs <- list(
    Households = survey::svydesign(
      ids = ~psu, strata = ~strata, weights = ~person_weight, data = hh, nest = TRUE
    ) |>
      convey::convey_prep(),
    `All adults` = survey::svydesign(
      ids = ~psu, strata = ~strata, weights = ~weight_pooled, data = ad, nest = TRUE
    ),
    Women = survey::svydesign(
      ids = ~psu, strata = ~strata, weights = ~weight_sex, data = ad[sex == 'Women', ],
      nest = TRUE
    ),
    Men = survey::svydesign(
      ids = ~psu, strata = ~strata, weights = ~weight_sex, data = ad[sex == 'Men', ],
      nest = TRUE
    )
  )

  results <- lapply(seq_len(nrow(indicator_specs)), function(ii){
    spec <- indicator_specs[ii, ]
    est <- estimate_by_domain(
      design = designs[[spec$group]],
      var = spec$var,
      domain_var = 'profile',
      type = spec$type,
      denominator = if(is.na(spec$denominator)) NULL else spec$denominator,
      psu_var = 'psu',
      strata_var = 'strata'
    )
    contrasts <- pairwise_domain_contrasts(est)
    est$estimates[, `:=` (indicator = spec$indicator, group = spec$group)]
    if(!is.null(contrasts)){
      contrasts[, `:=` (indicator = spec$indicator, group = spec$group)]
    }
    return(list(estimates = est$estimates, contrasts = contrasts))
  })
  return(list(
    estimates = data.table::rbindlist(lapply(results, `[[`, 'estimates')),
    contrasts = data.table::rbindlist(lapply(results, `[[`, 'contrasts'))
  ))
}


## MULTIPLY IMPUTED ESTIMATES ----------------------------------------------------------->

# Collapse catchment probabilities to profiles; NA = outside all profiled catchments
profile_probs <- location_probs[, .(prob = sum(prob)), by = .(cluster, profile)]
assignments <- draw_cluster_assignments(
  probs = profile_probs[, .(cluster, zone = profile, prob)],
  n_draws = n_imputations
)
data.table::setnames(assignments, 'zone', 'profile')

message('Running ', n_imputations, ' imputations on ', n_cores, ' cores...')
start_time <- Sys.time()
draw_results <- parallel::mclapply(
  seq_len(n_imputations),
  function(dd){
    res <- estimate_one_assignment(assignments[draw == dd, .(cluster, profile)])
    res$estimates[, draw := dd]
    res$contrasts[, draw := dd]
    return(res)
  },
  mc.cores = n_cores,
  mc.preschedule = FALSE
)
# A worker killed by the OS returns NULL rather than a try-error
failed <- which(vapply(
  draw_results, function(x) is.null(x) || inherits(x, 'try-error'), logical(1)
))
if(length(failed) > 0){
  stop(
    "Imputations failed: ", paste(failed, collapse = ', '), '\n',
    draw_results[[failed[1]]]
  )
}
message('Imputations finished in ', format(round(Sys.time() - start_time, 1)))

draw_estimates <- data.table::rbindlist(lapply(draw_results, `[[`, 'estimates'))
draw_contrasts <- data.table::rbindlist(lapply(draw_results, `[[`, 'contrasts'))
draw_estimates[, df_complete := pmax(n_clusters - n_strata, 1)]

pool_by_transform <- function(draws, by_cols, n_imp){
  draws <- merge(
    draws, indicator_specs[, .(indicator, group, type, transform)],
    by = c('indicator', 'group')
  )
  lapply(split(draws, by = 'transform'), function(dt){
    pool_rubin(
      dt, by = by_cols, transform = dt$transform[1], n_imputations = n_imp
    )
  }) |> data.table::rbindlist()
}

estimates <- pool_by_transform(
  draw_estimates, by_cols = c('indicator', 'group', 'domain'), n_imp = n_imputations
)
# Secondary intervals using the full-survey design degrees of freedom (PSUs minus
#  strata), as in standard DHS subgroup tables; less conservative for small domains
full_design_df <- households[, data.table::uniqueN(psu) - data.table::uniqueN(strata)]
estimates_full_df <- pool_by_transform(
  data.table::copy(draw_estimates)[, df_complete := full_design_df],
  by_cols = c('indicator', 'group', 'domain'), n_imp = n_imputations
)[, .(indicator, group, domain, lower_full_df = lower, upper_full_df = upper)]
estimates <- merge(estimates, estimates_full_df, by = c('indicator', 'group', 'domain'))
draw_counts <- draw_estimates[
  ,
  .(
    mean_n = mean(n),
    mean_clusters = mean(n_clusters),
    min_clusters = min(n_clusters),
    max_clusters = max(n_clusters)
  ),
  by = .(indicator, group, domain)
]
estimates <- merge(estimates, draw_counts, by = c('indicator', 'group', 'domain'))

data.table::setnames(draw_contrasts, 'diff', 'est')
contrasts <- pool_rubin(
  draw_contrasts,
  by = c('indicator', 'group', 'domain_a', 'domain_b'),
  transform = 'identity',
  n_imputations = n_imputations
)
data.table::setnames(contrasts, 'est', 'diff')
# Holm adjustment within each indicator and group across the 15 profile pairs
contrasts[, p_holm := stats::p.adjust(p_value, method = 'holm'), by = .(indicator, group)]


## RELIABILITY FLAGS -------------------------------------------------------------------->

expected_clusters <- profile_probs[
  !is.na(profile),
  .(expected_clusters = sum(prob)),
  by = .(domain = profile)
]
estimates <- merge(estimates, expected_clusters, by = 'domain', all.x = TRUE)
estimates[, reliability := data.table::fcase(
  mean_n < dhs_settings$suppress_n, 'Suppress (n < 25)',
  expected_clusters < dhs_settings$unreliable_clusters, 'Unreliable (< 5 clusters)',
  expected_clusters < dhs_settings$min_clusters, 'Caution (< 10 clusters)',
  mean_n < dhs_settings$caution_n, 'Caution (n < 50)',
  default = 'Reliable'
)]
data.table::setnames(estimates, 'domain', 'profile')
data.table::setnames(contrasts, c('domain_a', 'domain_b'), c('profile_a', 'profile_b'))
estimates <- merge(
  estimates, indicator_specs[, .(indicator, group, type, denominator)],
  by = c('indicator', 'group')
)


## SENSITIVITY: NAIVE POINT-IN-POLYGON ASSIGNMENT --------------------------------------->

naive_assign <- clusters[, .(cluster, profile = naive_profile)]
naive_res <- estimate_one_assignment(naive_assign)
naive_draws <- naive_res$estimates[
  , `:=` (draw = 1L, df_complete = pmax(n_clusters - n_strata, 1))
]
naive_estimates <- pool_by_transform(
  naive_draws, by_cols = c('indicator', 'group', 'domain'), n_imp = 1L
)
naive_estimates <- merge(
  naive_estimates, naive_draws[, .(indicator, group, domain, n, n_clusters)],
  by = c('indicator', 'group', 'domain')
)
data.table::setnames(naive_estimates, 'domain', 'profile')


## SAMPLE SIZES ------------------------------------------------------------------------->

sample_sizes <- merge(
  expected_clusters,
  clusters[!is.na(naive_profile), .(naive_clusters = .N), by = .(domain = naive_profile)],
  by = 'domain', all = TRUE
)
hh_counts <- estimates[
  indicator == 'wealth_score', .(domain = profile, mean_households = mean_n)
]
adult_counts <- estimates[
  indicator == 'worked_12m' & group %in% c('Women', 'Men'),
  .(domain = profile, group, mean_n)
] |>
  data.table::dcast(domain ~ group, value.var = 'mean_n')
data.table::setnames(adult_counts, c('Men', 'Women'), c('mean_men', 'mean_women'))
sample_sizes <- Reduce(
  function(a, b) merge(a, b, by = 'domain', all = TRUE),
  list(sample_sizes, hh_counts, adult_counts)
)
data.table::setnames(sample_sizes, 'domain', 'profile')
print(sample_sizes)


## SAVE --------------------------------------------------------------------------------->

quintiles <- estimates[grepl('^quintile_', indicator), ]
data.table::fwrite(estimates, out_path('estimates'))
data.table::fwrite(contrasts, out_path('contrasts'))
data.table::fwrite(quintiles, out_path('quintiles'))
data.table::fwrite(sample_sizes, out_path('sample_sizes'))
data.table::fwrite(naive_estimates, out_path('sensitivity'))
data.table::fwrite(draw_estimates, out_path('estimates_by_draw'))
message('Done estimating DHS profile indicators for ', survey_id, '.')
