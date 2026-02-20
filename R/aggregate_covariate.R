#' Aggregate a covariate to polygon boundaries
#'
#' @details Quick population-weighted covariate aggregation using a template raster
#'
#' @param cov ([terra::SpatRaster]) Covariate to aggregate
#' @param template ([terra::SpatRaster]) Template raster to use for aggregation
#' @param agg_table ([data.table::data.table]) Aggregation table to use, created by
#'   [mbg::build_aggregation_table()]
#' @param resample_method (character(1), default 'bilinear') Resampling method to use
#'   for the covariate raster. See [terra::resample()] for options.
#' @param ... Additional arguments passed to [mbg::aggregate_raster_to_polygons()]
#'
#' @seealso [mbg::aggregate_raster_to_polygons()], [terra::resample()]
#' @importFrom terra crs compareGeom project resample crop extend
#' @importFrom mbg aggregate_raster_to_polygons
#' @export
aggregate_covariate <- function(
  cov, template, agg_table, resample_method = 'bilinear', ...
){
  if (terra::crs(cov) != terra::crs(template)) {
    cov <- terra::project(x = cov, y = template)
  }
  if (terra::ext(cov) != terra::ext(template)) {
    cov <- terra::resample(x = cov, y = template, method = resample_method)
  }
  # Crop and extend cov to match template dimensions exactly
  cov <- terra::crop(x = cov, y = template)
  cov <- terra::extend(x = cov, y = template)

  # Aggregate the covariate raster to the polygon boundaries
  aggregated <- mbg::aggregate_raster_to_polygons(
    data_raster = cov,
    ...
  )

  return(aggregated)
}
