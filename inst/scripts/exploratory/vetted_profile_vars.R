## #######################################################################################
##
## VETTED PROFILE VARIABLES
##
## AUTHOR: Nat Henry, nat@henryspatialanalysis.com
## CREATED: 2026-05-04
## PURPOSE: Explore vetted profile variables for IAS late-breaker
##
## #######################################################################################

REPO_DIR <- '~/repos/mwi.hiv.factors'
WORK_DIR <- '/mnt/c/Users/nathe/Dropbox/RESPOND_Community Archetyping/Analysis/2026-03-16'
IN_FP <- file.path(WORK_DIR, 'vetted_profile_groupings.csv')
OUT_FP <- file.path(WORK_DIR, 'vetted_profile_summaries.csv')

## SETUP -------------------------------------------------------------------------------->

load_pkgs <- c('data.table', 'versioning', 'scales')
lapply(load_pkgs, library, character.only = TRUE) |> invisible()

config <- versioning::Config$new(file.path(REPO_DIR, 'config.yaml'))

## LOAD INPUTS -------------------------------------------------------------------------->

vetted_profile_groupings <- data.table::fread(IN_FP) |>
  _[, prof_label := paste0(profile_color, ': ', profile_label)]
cov_names <- intersect(
  names(vetted_profile_groupings),
  config$get('pca_covariates') |> names()
)
comm_vars <- c('mobility', 'econ_activity', 'pop_growth', 'weather')

summ_fun_gis <- function(vec){
  accuracy <- dplyr::case_when(
    max(vec, na.rm = TRUE) > 100 ~ 0,
    max(vec, na.rm = TRUE) > 10 ~ 0.1,
    TRUE ~ 0.01
  )
  mean <- vec |> mean(na.rm = TRUE) |> scales::comma(accuracy = accuracy)
  sd <- vec |> sd(na.rm = TRUE) |> scales::comma(accuracy = accuracy)
  return(paste0(mean, ' (', sd, ')'))
}
summ_fun_comm <- function(vec){
  scales::percent(mean(vec, na.rm = TRUE))
}

agg <- vetted_profile_groupings[
  , c(lapply(.SD, summ_fun_gis), list(N = .N)),
  .SDcols = setdiff(cov_names, comm_vars),
  by = prof_label
] |>
  _[, prof_label := paste0(prof_label, ' (N = ', N, ')')][, N := NULL]

comm_agg <- vetted_profile_groupings[
  , c(lapply(.SD, summ_fun_comm), list(N = .N)),
  .SDcols = comm_vars,
  by = prof_label
] |>
  _[, prof_label := paste0(prof_label, ' (N = ', N, ')')][, N := NULL]

full <- merge(x = agg, y = comm_agg, by = 'prof_label')

data.table::fwrite(agg, OUT_FP)