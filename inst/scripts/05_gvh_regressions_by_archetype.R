## #######################################################################################
##
## 05. RUN REGRESSIONS BY GVH ARCHETYPE
##
## AUTHOR: Nat Henry, nat@henryspatialanalysis.com
## CREATED: September 26, 2025
## PURPOSE: Run regressions by GVH archetype
##
## #######################################################################################

REPO_DIR <- '~/repos/mwi.hiv.factors'

## Setup -------------------------------------------------------------------------------->

load_pkgs <- c('data.table', 'ggplot2', 'glue', 'versioning')
lapply(load_pkgs, library, character.only = TRUE) |> invisible()

# Load config
config <- versioning::Config$new(file.path(REPO_DIR, 'config.yaml'))
facility_data <- config$read("analysis", "pca_results_combined")
facility_data[, facility_label := paste0(region_label, ', C', kmeans_best)]

# Load GVH data
gvh_data <- config$read("prepared_data", "covariates_by_gvh") |>
  merge(
    y = facility_data[, .(closest_facility_id, facility_label)],
    by = 'closest_facility_id'
  )
overall <- data.table::copy(gvh_data)[, facility_label := 'All archetypes']
gvh_data <- data.table::rbindlist(list(overall, gvh_data))

# Normalize covariates
cov_names <- intersect(names(config$get("covariates")), names(gvh_data))
gvh_norm <- (
  data.table::copy(gvh_data)
  [, (cov_names) := lapply(.SD, scale), .SDcols = cov_names]
)

## Get variation of each covariate by archetype ----------------------------------------->

archetype_quantiles <- (
  gvh_data
  [, c(cov_names, 'facility_label'), with = FALSE] |>
  data.table::melt.data.table(id.vars = 'facility_label', variable.name = 'covar') |>
  _[, .(
    avg = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    q10 = quantile(value, probs = 0.1, na.rm = TRUE),
    q90 = quantile(value, probs = 0.9, na.rm = TRUE)
  ), by = .(facility_label, covar)]
)

## Run regressions by archetype --------------------------------------------------------->

regress_covar <- function(dt, covar_col, outcome_col){
  regress_dt <- (
    data.table::copy(dt)
    [, .(cov = get(covar_col), outcome = get(outcome_col))] |>
    na.omit()
  )
  lm_mod <- stats::lm(outcome ~ cov, data = regress_dt)
  slope <- coef(lm_mod)[2]
  mod_r2 <- summary(lm_mod)$r.squared
  return(data.table::data.table(
    covar = covar_col, outcome = outcome_col, r2 = mod_r2, slope = slope
  ))
}

covariate_regressions <- lapply(cov_names, function(covar_col){
  lapply(c('viraemia15to49_mean', 'prev15to49_mean'), function(outcome_col){
    gvh_norm[, regress_covar(.SD, covar_col, outcome_col), by = facility_label]
  }) |> data.table::rbindlist()
}) |> data.table::rbindlist()


# Combine and save results -------------------------------------------------------------->

results_full <- merge(
  x = covariate_regressions,
  y = archetype_quantiles,
  by = c('covar', 'facility_label')
)
config$write(results_full, "analysis", "gvh_regressions_by_archetype")
