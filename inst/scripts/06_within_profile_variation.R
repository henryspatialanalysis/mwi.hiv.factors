## #######################################################################################
##
## 06. WITHIN-PROFILE VARIATION
##
## AUTHOR: Nat Henry, nat@henryspatialanalysis.com
## CREATED: September 26, 2025
## PURPOSE: Show differences in covariate values within each community profile
##
## #######################################################################################

REPO_DIR <- '~/repos/mwi.hiv.factors'


## Setup -------------------------------------------------------------------------------->

load_pkgs <- c('data.table', 'ggplot2', 'glue', 'versioning')
lapply(load_pkgs, library, character.only = TRUE) |> invisible()

# Load config
config <- versioning::Config$new(file.path(REPO_DIR, 'config.yaml'))
out_dir <- config$get_dir_path("analysis")

# Load input data
vetted_profile_groupings <- config$read("analysis", "vetted_profile_groupings") |>
  stats::na.omit(cols = c('profile_color', 'profile_label'))

catchments_sf <- config$read('catchments', 'facility_catchments', quiet = TRUE)

# Add lat-long centers for each catchment ----------------------------------------------->

catchment_centroids <- catchments_sf |>
  sf::st_centroid() |>
  sf::st_coordinates() |>
  data.table::as.data.table() |>
  setnames(new = c('longitude', 'latitude'))
catchment_centroids[, catchment_id := catchments_sf$catchment_id ]
vetted_profile_groupings <- vetted_profile_groupings[
  catchment_centroids,
  on = 'catchment_id',
  nomatch = NULL
]


## Get 'distance' from each set of covariate values to the n-dimensional profile center ->

# Normalize each covariate within profiles
normed <- data.table::copy(vetted_profile_groupings)
cov_names <- c(names(config$get('pca_covariates')), 'longitude', 'latitude') |>
  intersect(names(vetted_profile_groupings))
cov_names_metro <- setdiff(cov_names, config$get('drop_from_metro'))
for(cov_name in cov_names){
  (normed
    [, (cov_name) := as.numeric(get(cov_name))]
    [, (cov_name) := scale(get(cov_name)), by = profile_color ]
    [is.na(get(cov_name)), (cov_name) := 0 ]
  )
}

# Helper functions
square <- function(x) x**2
cov_distances <- function(cov_matrix){
  cov_matrix |>
  square() |>
  rowSums() |>
  sqrt()
}

# Distance for non-metro
normed$cov_distance <- NA_real_
normed[in_municipality == 1L, ]$cov_distance <- (
  normed[in_municipality == 1L, cov_names_metro, with = FALSE] |>
  as.matrix() |>
  cov_distances()
)
normed[in_municipality == 0L, ]$cov_distance <- (
  normed[in_municipality == 0L, cov_names, with = FALSE] |>
  as.matrix() |>
  cov_distances()
)

vetted_profile_groupings[normed, cov_distance := i.cov_distance, on = 'catchment_id']
config$write(vetted_profile_groupings, 'analysis', 'within_profile_variation')


## Get pairwise distances between each facility in the profiles ------------------------->

all_pairwise <- merge(
  x = normed,
  y = normed,
  by = 'profile_color',
  allow.cartesian = TRUE,
  suffixes = c('_a', '_b')
)
for(cov_name in cov_names){
  all_pairwise[
    , (paste0(cov_name, '_diff')) := get(paste0(cov_name, '_a')) - get(paste0(cov_name, '_b'))
  ]
}
all_pairwise$cov_distance <- NA_real_
metro_rows <- all_pairwise[, grepl("Metro", profile_label_a)]
all_pairwise[metro_rows, ]$cov_distance <- (
  all_pairwise[metro_rows, paste0(cov_names_metro, '_diff'), with = FALSE] |>
  as.matrix() |>
  cov_distances()
)
all_pairwise[!metro_rows, ]$cov_distance <- (
  all_pairwise[!metro_rows, paste0(cov_names, '_diff'), with = FALSE] |>
  as.matrix() |>
  cov_distances()
)
for(this_profile_color in unique(all_pairwise$profile_color)){
  distance_matrix <- (
    all_pairwise[profile_color == this_profile_color, .(catchment_name_a, catchment_name_b, cov_distance)] |>
    dcast(formula = catchment_name_a ~ catchment_name_b, value.var = 'cov_distance')
  )
  data.table::fwrite(
    x = distance_matrix,
    file = glue::glue('{out_dir}/within_profile_variation_{this_profile_color}.csv')
  )
}

