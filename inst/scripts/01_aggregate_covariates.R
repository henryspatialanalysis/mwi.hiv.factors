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
load_pkgs <- c('terra', 'sf', 'mbg', 'data.table', 'versioning')
lapply(load_pkgs, library, character.only = TRUE) |> invisible()
devtools::load_all(REPO_DIR)

# Load config
config <- versioning::Config$new(file.path(REPO_DIR, 'config.yaml'))

# Create output directory
dir.create(config$get_dir_path('prepared_data'))

# Load inputs
pop <- config$read('raw_data', 'population_1km')
gvh_meta <- config$read('catchments', 'gvh_meta') |>
  _[, .(gvh_id, catchment_id)] |>
  na.omit() |>
  unique()
gvh_catchments <- config$read('catchments', 'gvh_catchments', quiet = TRUE) |>
  merge(y = gvh_meta, by = 'gvh_id', all.x = TRUE)
facility_catchments <- config$read('catchments', 'facility_catchments', quiet = TRUE)
aggregate_results <- config$read('splitting', 'aggregated_results')
admin_bounds <- config$read("catchments", "admin_bounds", quiet = TRUE)


## Assign metro vs. non-metro for each facility catchment and GVH ----------------------->

catchments_id_field <- config$get('catchments_id_field')
municipalities <- admin_bounds |>
  dplyr::filter(area_level == 5L) |>
  dplyr::filter(endsWith(area_name, ' City')) |>
  dplyr::mutate(municipality = area_name) |>
  dplyr::select(municipality)
catchment_to_municipality <- facility_catchments |>
  sf::st_point_on_surface() |>
  sf::st_join(y = municipalities) |>
  sf::st_drop_geometry() |>
  dplyr::select(all_of(catchments_id_field), municipality) |>
  suppressWarnings()
facility_catchments <- facility_catchments |>
  merge(y = catchment_to_municipality, by = catchments_id_field, all.x = TRUE) |>
  dplyr::mutate(in_municipality = as.integer(!is.na(municipality)))


## Create template raster and prepare aggregation table --------------------------------->

id_raster_fp <- config$get_file_path('prepared_data', 'id_raster')
if(file.exists(id_raster_fp)){
  id_raster <- versioning::autoread(id_raster_fp)
} else {
  id_raster <- mbg::build_id_raster(polygons = facility_catchments, template_raster = pop)
  versioning::autowrite(id_raster, file = id_raster_fp)
}
pop <- pop |> terra::crop(id_raster) |> terra::extend(id_raster)
pop[is.na(pop) & !is.na(id_raster)] <- 0

agg_types <- c('facility', 'gvh')
agg_tables <- vector('list', length = length(agg_types))
names(agg_tables) <- agg_types
for(agg_type in agg_types){
  agg_table_fp <- config$get_file_path(
    'prepared_data',
    paste0('aggregation_table_', agg_type)
  )
  if(file.exists(agg_table_fp)){
    agg_tables[[agg_type]] <- versioning::autoread(agg_table_fp)
  } else {
    agg_tables[[agg_type]] <- mbg::build_aggregation_table(
      polygons = if(agg_type == 'facility') facility_catchments else gvh_catchments,
      id_raster = id_raster,
      polygon_id_field = if(agg_type == 'facility') catchments_id_field else 'gvh_id',
      verbose = TRUE
    )
    versioning::autowrite(agg_tables[[agg_type]], file = agg_table_fp)
  }
}

## LOAD AND AGGREGATE EACH COVARIATE ---------------------------------------------------->

cov_names <- names(config$get('covariates'))
cov_tables <- vector('list', length = length(cov_names))
names(cov_tables) <- cov_names
all_cov_tables <- list()
for(agg_type in agg_types){
  all_cov_tables[[agg_type]] <- cov_tables
}

# Load each covariate
for(cov_name in cov_names){
  message("Running ", cov_name)
  if(cov_name %in% names(config$get('directories', 'raw_data', 'files'))){
    cov_raw <- config$read('raw_data', cov_name)
  } else if(cov_name %in% names(config$get('directories', 'raster_db', 'files'))){
    cov_raw <- config$read('raster_db', cov_name)
  } else {
    stop(paste('Covariate', cov_name, 'not found in raw_data or raster_db folders.'))
  }
  # Some covariates get missing values filled with zeroes before further preparation
  if(cov_name %in% c('cropland_carbon_loss', 'cropland_n2o_loss', 'people_near_forest')){
    terra::values(cov_raw)[is.na(terra::values(cov_raw))] <- 0.0
  }
  # Prepare covariate
  for(agg_type in agg_types){
    agg_cov_data <- mwi.hiv.factors::aggregate_covariate(
      cov = cov_raw,
      template = id_raster,
      resample_method = 'bilinear',
      aggregation_table = agg_tables[[agg_type]],
      aggregation_cols = if(agg_type == 'facility') catchments_id_field else 'gvh_id',
      method = ifelse(cov_name == 'population_1km', 'sum', 'weighted.mean'),
      weighting_raster = pop
    )
    if(cov_name %in% config$get('log_transformed')){
      agg_cov_data$data <- log1p(agg_cov_data$data)
    }
    data.table::setnames(agg_cov_data, old = 'data', new = cov_name)
    all_cov_tables[[agg_type]][[cov_name]] <- agg_cov_data
  }
}

for(agg_type in agg_types){
  # Combine all covariates into a single table
  catchments <- if(agg_type == 'facility') facility_catchments else gvh_catchments
  full_data <- catchments |>
    sf::st_drop_geometry() |>
    data.table::as.data.table()

  for(cov_name in cov_names){
    full_data <- merge(
      x = full_data,
      y = all_cov_tables[[agg_type]][[cov_name]],
      by = if(agg_type == 'facility') catchments_id_field else 'gvh_id',
      all.x = TRUE
    )
  }

  # Merge on viraemia, prevalence, VLS, and population by catchment
  agg_level <- if(agg_type == 'facility') 'FACILITY_CATCHMENTS' else 'GVH'
  hiv_by_catchment <- (
    aggregate_results
    [ aggregation_level == agg_level, ]
    [, id := as.integer(id)]
  )
  merge_id_field <- if(agg_type == 'facility') 'catchment_id' else 'gvh_id'
  if(any(hiv_by_catchment$id != full_data[[merge_id_field]])){
    stop("Mismatch between closest facility IDs in aggregate results and covariates by catchment")
  }
  keep_cols <- grep(pattern = '_mean$', x = colnames(hiv_by_catchment), value = TRUE)
  full_data <- cbind(full_data, hiv_by_catchment[, keep_cols, with = FALSE])

  # Save to file
  config$write(full_data, 'prepared_data', paste0('covariates_by_', agg_type))
}
