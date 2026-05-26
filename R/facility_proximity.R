#' Compute facility proximity metrics
#'
#' @details For each facility, find the geodesic (great-circle) distance to its nearest
#'   neighbour and count how many other facilities fall within a given radius. Distances
#'   are computed with [sf::st_distance()] on lon/lat points, so they are returned in
#'   metres internally and converted to kilometres.
#'
#' @param facilities ([data.frame] or [data.table::data.table]) One row per facility,
#'   containing longitude, latitude, and a unique identifier.
#' @param id_field (character(1), default 'facility_id') Name of the unique identifier
#'   column.
#' @param lon_field (character(1), default 'longitude') Longitude column name.
#' @param lat_field (character(1), default 'latitude') Latitude column name.
#' @param within_km (numeric(1), default 10) Radius, in kilometres, for the neighbour
#'   count.
#' @param crs (character(1), default 'EPSG:4326') CRS of the input coordinates.
#'
#' @return ([data.table::data.table]) with one row per facility and columns:
#'   `<id_field>`, `nearest_facility_km` (distance to the closest other facility), and
#'   `n_within_<within_km>km` (count of other facilities within the radius, excluding
#'   the facility itself).
#'
#' @importFrom sf st_as_sf st_distance
#' @importFrom data.table as.data.table data.table setnames
#' @export
facility_proximity_metrics <- function(
  facilities,
  id_field = 'facility_id',
  lon_field = 'longitude',
  lat_field = 'latitude',
  within_km = 10,
  crs = 'EPSG:4326'
){
  dt <- data.table::as.data.table(facilities)
  required <- c(id_field, lon_field, lat_field)
  if(!all(required %in% names(dt))){
    stop('facilities must contain columns: ', paste(required, collapse = ', '))
  }
  # Drop facilities missing coordinates, which cannot enter the distance matrix
  has_coords <- !is.na(dt[[lon_field]]) & !is.na(dt[[lat_field]])
  if(any(!has_coords)){
    warning(sum(!has_coords), ' facilities missing coordinates were dropped.')
  }
  dt <- dt[has_coords, ]
  if(anyDuplicated(dt[[id_field]])){
    stop('Values in id_field ("', id_field, '") must be unique.')
  }

  # Full pairwise geodesic distance matrix (metres -> kilometres)
  pts <- sf::st_as_sf(dt, coords = c(lon_field, lat_field), crs = crs)
  dist_km <- matrix(as.numeric(sf::st_distance(pts, pts)), nrow = nrow(dt)) / 1000
  # Self-distances (the diagonal) are excluded from both metrics
  diag(dist_km) <- NA_real_

  out <- data.table::data.table(
    id = dt[[id_field]],
    nearest_facility_km = apply(dist_km, 1L, min, na.rm = TRUE),
    n_within = as.integer(rowSums(dist_km <= within_km, na.rm = TRUE))
  )
  data.table::setnames(
    out,
    c('id', 'n_within'),
    c(id_field, paste0('n_within_', within_km, 'km'))
  )
  return(out[])
}
