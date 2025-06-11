#' Calculate distance from each raster cell to vector features
#'
#' @param raster_template ([terra::SpatRaster]) Raster template to calculate distance
#'   across. Each non-NA cell in the template will be filled with a distance value
#' @param vector ([terra::SpatVector] or [sf::sf]) Vector features to calculate distance
#'   to.
#' @param crs ([character], default 'EPSG:4326') CRS that will be used for measuring
#'   distance. The raster template will be returned in the same CRS it was passed in.
#'
#' @return ([terra::SpatRaster]) Raster filled with distance values.
#'
#' @importFrom terra crs project vect distance
#' @export
distance_to_features <- function(raster_template, vector, crs = 'EPSG:4326'){
  # Check input data types
  if(!inherits(raster_template, 'SpatRaster')) {
    stop('raster_template must be a terra::SpatRaster')
  }
  if(!inherits(vector, c('SpatVector', 'sf'))) {
    stop('vector must be a terra::SpatVector or sf::sf')
  }
  if(is.character(crs)){
    crs <- terra::crs(crs)
  }

  # Convert vector to SpatVector if needed
  if(inherits(vector, 'sf')) vector <- terra::vect(vector)
  # Get NA values from raster template
  na_values <- is.na(terra::values(raster_template))
  # Convert CRS if needed
  if(terra::crs(vector) != crs) vector <- terra::project(vector, crs)
  raster_original_crs <- terra::crs(raster_template)
  if(raster_original_crs != crs) raster_template <- terra::project(raster_template, crs)

  # Calculate distance
  dist_raster <- terra::distance(raster_template, vector)
  terra::values(dist_raster)[na_values] <- NA_real_
  # Reproject back to original CRS, if needed
  if(raster_original_crs != crs){
    dist_raster <- terra::project(dist_raster, raster_original_crs)
  }

  # Return result
  return(dist_raster)
}
