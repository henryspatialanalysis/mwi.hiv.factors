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
facility_catchments <- config$read('catchments', 'facility_catchments')
aggregate_results <- config$read('splitting', 'aggregated_results')


## Create template raster and prepare aggregation table --------------------------------->

catchments_id_field <- config$get('catchments_id_field')
facility_catchments[[catchments_id_field]] <- seq_len(nrow(facility_catchments))
id_raster_fp <- config$get_file_path('prepared_data', 'id_raster')
if(file.exists(id_raster_fp)){
  id_raster <- versioning::autoread(id_raster_fp)
} else {
  id_raster <- mbg::build_id_raster(polygons = facility_catchments, template_raster = pop)
  versioning::autowrite(id_raster, file = id_raster_fp)
}
pop <- pop |> terra::crop(id_raster) |> terra::extend(id_raster)
pop[is.na(pop) & !is.na(id_raster)] <- 0

agg_table_fp <- config$get_file_path('prepared_data', 'aggregation_table')
if(file.exists(agg_table_fp)){
  agg_table <- versioning::autoread(agg_table_fp)
} else {
  agg_table <- mbg::build_aggregation_table(
    polygons = facility_catchments,
    id_raster = id_raster,
    polygon_id_field = catchments_id_field,
    verbose = TRUE
  )
  versioning::autowrite(agg_table, file = agg_table_fp)
}


## LOAD AND AGGREGATE EACH COVARIATE ---------------------------------------------------->

cov_names <- names(config$get('covariates'))
cov_tables <- vector('list', length = length(cov_names))
names(cov_tables) <- cov_names

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
  # Prepare covariate
  cov_tables[[cov_name]] <- mwi.hiv.factors::aggregate_covariate(
    cov = cov_raw,
    template = id_raster,
    resample_method = 'bilinear',
    aggregation_table = agg_table,
    aggregation_cols = catchments_id_field,
    method = ifelse(cov_name == 'population_1km', 'sum', 'weighted.mean'),
    weighting_raster = pop
  )
  if(cov_name %in% config$get('log_transformed')){
    cov_tables[[cov_name]]$data <- log1p(cov_tables[[cov_name]]$data)
  }
  data.table::setnames(cov_tables[[cov_name]], old = 'data', new = cov_name)
}

# Combine all covariates into a single table
full_data <- facility_catchments |>
  sf::st_drop_geometry() |>
  data.table::as.data.table()

for(cov_name in cov_names){
  full_data <- merge(
    x = full_data,
    y = cov_tables[[cov_name]],
    by = catchments_id_field,
    all.x = TRUE
  )
}

# Merge on viraemia, prevalence, VLS, and population by catchment
hiv_by_catchment <- (
  aggregate_results
  [ aggregation_level == 'CLOSEST_FACILITIES', ]
  [, id := as.integer(id)]
)
if(any(hiv_by_catchment$id != full_data$closest_facility_id)){
  stop("Mismatch between closest facility IDs in aggregate results and covariates by catchment")
}
keep_cols <- grep(pattern = '_mean$', x = colnames(hiv_by_catchment), value = TRUE)
full_data <- cbind(full_data, hiv_by_catchment[, keep_cols, with = FALSE])

# Save to file
config$write(full_data, 'prepared_data', 'covariates_by_catchment')
