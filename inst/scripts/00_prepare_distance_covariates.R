## #######################################################################################
##
## PREPARE RELATIVE WEALTH INDEX RASTER
##
## AUTHOR: Nat Henry, nat@henryspatialanalysis.com
## CREATED: June 9, 2025
## PURPOSE: The Meta Relative Wealth Index (RWI) product comes as a CSV with a
##  non-standard projection. Convert it to a raster with the same resolution as most other
##  input covariates.
##
## #######################################################################################

REPO_DIR <- '~/repos/mwi.hiv.factors'


## Setup -------------------------------------------------------------------------------->

load_pkgs <- c('terra', 'sf', 'data.table', 'versioning')
lapply(load_pkgs, library, character.only = TRUE) |> invisible()
devtools::load_all(REPO_DIR)

config <- versioning::Config$new(file.path(REPO_DIR, 'config.yaml'))

# Load inputs
id_raster <- config$read('prepared_data', 'id_raster')
destinations <- list(
  admarc = config$read('raw_vectors', 'admarc'),
  border_posts = config$read('raw_vectors', 'border_posts'),
  towns = config$read('raw_vectors', 'towns'),
  borders = config$read("global_shp", "adm0") |>
    dplyr::filter(ADM0_NAME == 'Malawi')
)
town_populations <- config$read(
  'raw_vectors', 'town_populations', sheet = 'CityPopulation_de'
) |>
  data.table::as.data.table()
facility_metadata <- config$read('catchments', 'facility_metadata') |>
  data.table::as.data.table()


## Destinations data prep --------------------------------------------------------------->

# subset to towns with population > 20k
town_names_20k <- town_populations[C_2018 > 2e4, unique(Name)]
missing_towns <- setdiff(town_names_20k, destinations$towns$NAME)
if(length(missing_towns) > 0L) stop("Missing some towns with population > 20k")
destinations$towns <- destinations$towns[destinations$towns$NAME %in% town_names_20k, ]

# Prepare borders
destinations$borders <- destinations$borders |>
  sf::st_cast(to = 'MULTILINESTRING') |>
  sf::st_cast(to = 'LINESTRING') |>
  sf::st_simplify(dTolerance = .5/111)

## Calculate distance to features ------------------------------------------------------->

for(dest_name in names(destinations)){
  message("Preparing distance to ", dest_name)
  # Calculate distance to this destination type
  dist_raster <- distance_to_features(
    raster_template = id_raster,
    vector = destinations[[dest_name]]
  )
  # Save to file
  config$write(dist_raster, 'raw_data', paste0('distance_', dest_name))
}


## Cross-catchment facility distance ---------------------------------------------------->
##
## For each 1km pixel, the geodesic distance (m) to the nearest facility whose
## catchment does NOT overlap this pixel. A measure of cross-catchment choice.
## Note: id_raster values are unique per-pixel IDs; the pixel -> catchment mapping
## lives in aggregation_table_facility and a pixel can straddle catchments.

message("Preparing distance to nearest cross-catchment facility")

# Per-pixel raster cell info (non-NA cells only). id_raster value == pixel_id.
id_vals <- terra::values(id_raster)[, 1]
non_na_cells <- which(!is.na(id_vals))
pixel_xy <- terra::xyFromCell(id_raster, non_na_cells)
pixel_dt <- data.table::data.table(
  cell = non_na_cells,
  pixel_id = id_vals[non_na_cells],
  x = pixel_xy[, 1],
  y = pixel_xy[, 2]
)

# pixel_id -> set of overlapping catchment_ids (one pixel can span multiple catchments)
agg_table <- config$read('prepared_data', 'aggregation_table_facility') |>
  data.table::as.data.table()
pixel_catchments <- agg_table[
  , .(catchment_ids = list(unique(catchment_id))), by = pixel_id
]
pixel_dt <- merge(pixel_dt, pixel_catchments, by = 'pixel_id', all.x = TRUE)
if(any(vapply(pixel_dt$catchment_ids, is.null, logical(1)))){
  stop("Some non-NA id_raster pixels have no catchment in aggregation_table_facility")
}

# Drop facilities missing coordinates; confirm every overlapping catchment has a facility
facility_dt <- facility_metadata[!is.na(longitude) & !is.na(latitude), ]
all_pixel_catchments <- unique(unlist(pixel_dt$catchment_ids))
missing_catchments <- setdiff(all_pixel_catchments, facility_dt$catchment_id)
if(length(missing_catchments) > 0L){
  stop(
    "Missing facility coordinates for catchment IDs: ",
    paste(missing_catchments, collapse = ', ')
  )
}

# sf points in EPSG:4326 for geodesic distances (meters)
pixel_pts <- sf::st_as_sf(pixel_dt, coords = c('x', 'y'), crs = 'EPSG:4326')
facility_pts <- sf::st_as_sf(
  facility_dt, coords = c('longitude', 'latitude'), crs = 'EPSG:4326'
)

# Pixel-to-facility distance matrix; strip units to plain numeric matrix
dist_matrix <- sf::st_distance(pixel_pts, facility_pts) |>
  units::drop_units()

# Mask out every facility whose catchment overlaps the pixel, then take per-pixel min
fac_catchment_ids <- facility_pts$catchment_id
own_mask <- t(vapply(
  pixel_dt$catchment_ids,
  function(cids) fac_catchment_ids %in% cids,
  logical(length(fac_catchment_ids))
))
dist_matrix[own_mask] <- Inf
min_dist <- apply(dist_matrix, 1, min)

# Project min distances back onto an id_raster-shaped output raster (cell-indexed)
out_raster <- terra::rast(id_raster)
out_vals <- rep(NA_real_, terra::ncell(out_raster))
out_vals[pixel_dt$cell] <- min_dist
terra::values(out_raster) <- out_vals
config$write(out_raster, 'raw_data', 'distance_cross_catchment')
