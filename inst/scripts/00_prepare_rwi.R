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
rwi_table <- config$read('raw_data', 'rwi_raw')


## Resample RWI data at ID raster locations --------------------------------------------->

# ID raster to points
id_points <- as.data.frame(id_raster, xy = TRUE) |>
  setnames(new = c('x', 'y', 'pixel_id')) |>
  sf::st_as_sf(coords = c('x', 'y'), crs = 'EPSG:4326', remove = FALSE)

# RWI to points
rwi_points <- rwi_table |>
  sf::st_as_sf(coords = c('longitude', 'latitude'), crs = 'EPSG:4326')

# Get closest RWI value to each ID raster point
id_points_joined <- id_points |>
  sf::st_join(y = rwi_points, join = sf::st_nearest_feature)

# Add back to ID raster pixels
rwi_raster <- id_raster
terra::values(rwi_raster)[!is.na(terra::values(rwi_raster))] <- id_points_joined$rwi
names(rwi_raster) <- 'rwi'

# Create a "relative poverty index" raster, -1 * RWI
rpi_raster <- rwi_raster * -1
names(rpi_raster) <- 'rpi'

## Save RWI raster to file -------------------------------------------------------------->

config$write(rwi_raster, 'raw_data', 'rwi')
config$write(rpi_raster, 'raw_data', 'rpi')
