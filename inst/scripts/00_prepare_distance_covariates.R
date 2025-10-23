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

load_pkgs <- c('terra', 'sf', 'versioning')
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
