## #######################################################################################
##
## PREPARE MALAWI POVERTY COVARIATE
##
## AUTHOR: Nat Henry, nat@henryspatialanalysis.com
## CREATED: September 15, 2025
## PURPOSE: The World Bank poverty estimates come as a CSV file, with poverty estimated
##   by village. This script standardizes to a 1x1km resolution.
##
## #######################################################################################

REPO_DIR <- '~/repos/mwi.hiv.factors'


## Setup -------------------------------------------------------------------------------->

load_pkgs <- c('terra', 'sf', 'data.table', 'versioning', 'readxl')
lapply(load_pkgs, library, character.only = TRUE) |> invisible()
devtools::load_all(REPO_DIR)

config <- versioning::Config$new(file.path(REPO_DIR, 'config.yaml'))

# Load inputs
pop_1x1 <- config$read('raw_data', 'population_1km')
pov_by_ea <- config$read('raw_vectors', 'poverty') |>
  data.table::setnames(old = 'eacode', new = 'ea_id')
ea_pts <- lapply(
  c('North', 'Central', 'South'),
  function(reg) config$read('census', 'villages', sheet = reg)
) |>
  lapply(data.table::as.data.table) |>
  data.table::rbindlist()


## Resample RWI data at ID raster locations --------------------------------------------->

# Connect all EAs to villages
pov_by_ea[, eacode := sprintf('%05d%03d', tacode, ea_id)]
ea_sf <- (ea_pts
  [pov_by_ea, poor := i.poor, on = 'eacode']
  [, .(poor, longitude, latitude)]
) |>
  na.omit() |>
  sf::st_as_sf(coords = c('longitude', 'latitude'), crs = 'EPSG:4326')

# Population raster to points
pop_pts <- pop_1x1 |>
  as.data.frame(xy = TRUE, na.rm = TRUE) |>
  setnames(new = c('x', 'y', 'pixel_id')) |>
  sf::st_as_sf(coords = c('x', 'y'), crs = 'EPSG:4326', remove = FALSE)

# Spatial join based on nearest village
pop_pts_joined <- pop_pts |>
  sf::st_join(y = ea_sf, join = sf::st_nearest_feature)

# Convert back to raster
pov_raster <- pop_1x1
terra::values(pov_raster)[!is.na(terra::values(pov_raster))] <- pop_pts_joined$poor
names(pov_raster) <- 'poverty'

## Save to file ------------------------------------------------------------------------->

if(file.exists(config$get_file_path('raw_data', 'poverty'))){
  file.remove(config$get_file_path('raw_data', 'poverty'))
}
config$write(pov_raster, 'raw_data', 'poverty')
