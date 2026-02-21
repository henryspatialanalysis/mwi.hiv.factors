## #######################################################################################
##
## 01. AGGREGATE COVARIATES
##
## AUTHOR: Nat Henry, nat@henryspatialanalysis.com
## CREATED: May 26, 2026
## PURPOSE: Aggregate covariates to health facility catchments
##
## #######################################################################################

REPO_DIR <- '~/repos/mwi.hiv.factors'

## SETUP -------------------------------------------------------------------------------->

# Load packages
load_pkgs <- c('data.table', 'versioning')
lapply(load_pkgs, library, character.only = TRUE) |> invisible()
devtools::load_all(REPO_DIR)

# Load config
config <- versioning::Config$new(file.path(REPO_DIR, 'config.yaml'))

# Load inputs
full_data <- config$read('prepared_data', 'covariates_by_facility')
discretized <- config$read('prepared_data', 'covariates_by_facility_discretized')
community_indicators <- config$read('community_workshops', 'indicators')


## Merge on community workshop indicators ----------------------------------------------->

# Ensure no double-merges
indicator_cols <- intersect(
  colnames(community_indicators),
  names(config$get('covariates'))
)
community_indicators <- community_indicators[
  , c('catchment_id', indicator_cols), with = FALSE
]
full_data_drop <- intersect(indicator_cols, colnames(full_data))
if(length(full_data_drop) > 0) full_data[, (full_data_drop) := NULL]
discretized_drop <- intersect(indicator_cols, colnames(discretized))
if(length(discretized_drop) > 0) discretized[, (discretized_drop) := NULL]

# Merge on community indicators
full_data <- merge(
  x = full_data,
  y = community_indicators,
  by = 'catchment_id',
  all.x = TRUE
)
discretized <- merge(
  x = discretized,
  y = community_indicators,
  by = 'catchment_id',
  all.x = TRUE
)

config$write(full_data, 'prepared_data', 'covariates_by_facility')
config$write(discretized, 'prepared_data', 'covariates_by_facility_discretized')
