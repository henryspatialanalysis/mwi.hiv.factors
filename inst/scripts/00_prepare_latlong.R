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


# Get longitude and latitude at each location, then save to file ------------------------>

xy_vals <- id_raster |>
  as.data.frame(xy = TRUE, na.rm = FALSE)

longitude <- id_raster
terra::values(longitude) <- xy_vals$x
latitude <- id_raster
terra::values(latitude) <- xy_vals$y

config$write(longitude, 'raw_data', 'longitude')
config$write(latitude, 'raw_data', 'latitude')
