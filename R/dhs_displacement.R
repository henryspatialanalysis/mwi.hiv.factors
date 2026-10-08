#' Simulate DHS GPS displacement offsets
#'
#' @details DHS displaces each cluster by a uniformly distributed angle and a uniformly
#'   distributed distance up to a maximum: 2 km for urban clusters, and 5 km for rural
#'   clusters with 1% of rural clusters displaced up to 10 km. A uniform distance implies
#'   a 2D displacement density proportional to 1/r, concentrated near the original point.
#'
#' @param n (`integer(1)`) Number of offsets to simulate
#' @param urban (`logical(1)`) Simulate the urban kernel? If FALSE, simulate the rural
#'   mixture kernel.
#' @param params (`list`) Displacement parameters with items `urban_max_m`,
#'   `rural_max_m`, `rural_far_max_m`, and `rural_far_prob`
#'
#' @return Matrix with `n` rows and columns `dx` and `dy`, offsets in metres
#'
#' @export
dhs_displacement_offsets <- function(n, urban, params){
  if(urban){
    max_dist <- rep(params$urban_max_m, n)
  } else {
    is_far <- stats::runif(n) < params$rural_far_prob
    max_dist <- ifelse(is_far, params$rural_far_max_m, params$rural_max_m)
  }
  dist <- stats::runif(n) * max_dist
  angle <- stats::runif(n) * 2 * pi
  return(cbind(dx = sin(angle) * dist, dy = cos(angle) * dist))
}


#' Probability that a displaced point stays within its restricting polygon
#'
#' @details DHS redraws a displacement until the displaced point falls in the same
#'   admin-2 polygon as the true point. The likelihood of an observed point given a true
#'   location x is therefore the displacement kernel divided by A(x), the share of the
#'   kernel around x that falls within x's own polygon. This function approximates A(x)
#'   on a coarse grid by convolving each polygon's indicator surface with a kernel
#'   binned from simulated offsets.
#'
#' @param restrict_raster ([terra::SpatRaster]) Integer polygon codes on a coarse
#'   (around 1 km) longitude-latitude grid
#' @param codes (`integer(N)`) Polygon codes for which to compute A(x)
#' @param urban (`logical(1)`) Use the urban kernel? If FALSE, use the rural mixture.
#' @param params (`list`) Displacement parameters; see [dhs_displacement_offsets()]
#' @param n_sim (`integer(1)`, default 1e6) Number of offsets used to bin the kernel
#'
#' @return [terra::SpatRaster] on the grid of `restrict_raster` with one layer per code,
#'   named by the code. Each layer gives A(x) for a true location x assigned to that
#'   polygon; layers are not masked, so that 100 m samples near a polygon edge can be
#'   evaluated even where the coarse grid assigns their cell to a neighbour.
#'
#' @importFrom terra res focal ifel
#' @export
dhs_restriction_acceptance <- function(
  restrict_raster, codes, urban, params, n_sim = 1e6
){
  # Bin simulated offsets to grid cells at the raster's central latitude
  mid_lat <- mean(terra::ext(restrict_raster)[3:4])
  cell_m <- terra::res(restrict_raster) * c(111320 * cos(mid_lat * pi / 180), 110574)
  offsets <- dhs_displacement_offsets(n = n_sim, urban = urban, params = params)
  ix <- round(offsets[, 'dx'] / cell_m[1])
  iy <- round(-offsets[, 'dy'] / cell_m[2])
  half <- max(abs(c(ix, iy)))
  kernel <- matrix(0, nrow = 2 * half + 1, ncol = 2 * half + 1)
  counts <- table(paste(iy + half + 1, ix + half + 1))
  idx <- do.call(rbind, strsplit(names(counts), ' ')) |> apply(2, as.integer)
  kernel[idx] <- as.numeric(counts) / n_sim

  layers <- lapply(codes, function(code){
    indicator <- terra::ifel(restrict_raster == code, 1, 0)
    indicator[is.na(indicator)] <- 0
    terra::focal(indicator, w = kernel, fun = 'sum', na.rm = TRUE)
  })
  acceptance <- terra::rast(layers)
  names(acceptance) <- as.character(codes)
  return(acceptance)
}


#' Posterior probabilities of DHS cluster locations falling in each zone
#'
#' @details For each displaced cluster point y, the posterior density of the true
#'   location x is proportional to k(|y - x|) / A(x) * p(x) * 1[x in the cluster's
#'   restricting polygon], where k is the displacement kernel, A(x) the restriction
#'   acceptance probability (see [dhs_restriction_acceptance()]), and p(x) a population
#'   prior. Because k is symmetric, candidate true locations are sampled as y plus
#'   simulated offsets and weighted by p(x) / A(x) within the restricting polygon. The
#'   weighted share of candidates in each zone is that zone's posterior probability.
#'
#'   If no candidate for a cluster falls in a populated cell of its restricting polygon
#'   (usually a boundary mismatch), the restriction is dropped; if the population prior
#'   is still zero everywhere, candidates are weighted equally.
#'
#' @param clusters (`data.table`) One row per cluster with fields `cluster`, `lon`,
#'   `lat`, `urban` (logical), and `restrict_code` (integer)
#' @param pop_raster ([terra::SpatRaster]) Fine-resolution population prior
#' @param zone_raster ([terra::SpatRaster]) Integer zone IDs on the grid of
#'   `pop_raster`, NA outside all zones
#' @param restrict_raster ([terra::SpatRaster]) Integer restricting-polygon codes on the
#'   grid of `pop_raster`
#' @param acceptance (`list`) Two multi-layer [terra::SpatRaster]s named `urban` and
#'   `rural`, from [dhs_restriction_acceptance()]. Each sample's A(x) is read from the
#'   layer for its own cluster's restricting polygon.
#' @param params (`list`) Displacement parameters; see [dhs_displacement_offsets()]
#' @param n_samples (`integer(1)`) Candidate locations per cluster
#' @param min_acceptance (`numeric(1)`, default 0.05) Floor on A(x), to avoid extreme
#'   weights from coarse-grid artefacts at polygon corners
#'
#' @return `data.table` with fields `cluster`, `zone` (NA = outside all zones), `prob`,
#'   `ess` (Kish effective sample size of the cluster's candidate weights), and
#'   `fallback` ('none', 'unrestricted', or 'uniform')
#'
#' @importFrom data.table data.table rbindlist
#' @importFrom terra cellFromXY extract
#' @export
dhs_location_probabilities <- function(
  clusters, pop_raster, zone_raster, restrict_raster, acceptance, params, n_samples,
  min_acceptance = 0.05
){
  samples <- lapply(seq_len(nrow(clusters)), function(ii){
    cl <- clusters[ii, ]
    off <- dhs_displacement_offsets(n = n_samples, urban = cl$urban, params = params)
    data.table::data.table(
      cluster = cl$cluster,
      urban = cl$urban,
      restrict_code = cl$restrict_code,
      x = cl$lon + off[, 'dx'] / (111320 * cos(cl$lat * pi / 180)),
      y = cl$lat + off[, 'dy'] / 110574
    )
  }) |> data.table::rbindlist()

  xy <- as.matrix(samples[, .(x, y)])
  cells <- terra::cellFromXY(pop_raster, xy)
  valid <- which(!is.na(cells))
  extract_cells <- function(r){
    vals <- rep(NA_real_, length(cells))
    vals[valid] <- terra::extract(r, cells[valid])[[1]]
    return(vals)
  }
  samples$pop <- extract_cells(pop_raster)
  samples$zone <- extract_cells(zone_raster)
  samples$restrict <- extract_cells(restrict_raster)
  samples[is.na(pop), pop := 0]
  samples[, accept := NA_real_]
  for(code in unique(samples$restrict_code)){
    for(is_urban in c(TRUE, FALSE)){
      rows <- which(samples$restrict_code == code & samples$urban == is_urban)
      if(length(rows) == 0) next
      layer <- acceptance[[if(is_urban) 'urban' else 'rural']][[as.character(code)]]
      vals <- terra::extract(layer, xy[rows, , drop = FALSE])[[1]]
      data.table::set(samples, i = rows, j = 'accept', value = vals)
    }
  }
  samples[is.na(accept), accept := 1]
  samples[, accept := pmax(accept, min_acceptance)]

  samples[, in_restrict := data.table::fifelse(restrict == restrict_code, 1, 0, na = 0)]
  samples[, w := pop * in_restrict / accept]
  samples[, fallback := 'none']
  samples[, w_sum := sum(w), by = cluster]
  samples[w_sum == 0, `:=` (w = pop / accept, fallback = 'unrestricted')]
  samples[, w_sum := sum(w), by = cluster]
  samples[w_sum == 0, `:=` (w = 1, fallback = 'uniform')]

  out <- samples[
    ,
    .(prob = sum(w)),
    by = .(cluster, zone, fallback)
  ]
  out[, prob := prob / sum(prob), by = cluster]
  ess <- samples[, .(ess = sum(w)^2 / sum(w^2)), by = cluster]
  out <- merge(out, ess, by = 'cluster')
  data.table::setorder(out, cluster, -prob)
  return(out[])
}


#' Draw cluster-to-zone assignments from posterior location probabilities
#'
#' @param probs (`data.table`) Output of [dhs_location_probabilities()], with fields
#'   `cluster`, `zone`, `prob`. Clusters absent from `probs` are not returned.
#' @param n_draws (`integer(1)`) Number of imputations
#'
#' @return `data.table` with fields `draw`, `cluster`, `zone` (NA = outside all zones)
#'
#' @importFrom data.table data.table rbindlist
#' @export
draw_cluster_assignments <- function(probs, n_draws){
  by_cluster <- split(probs, by = 'cluster')
  draws <- lapply(by_cluster, function(cl){
    pick <- if(nrow(cl) == 1) rep(1L, n_draws) else {
      sample.int(nrow(cl), size = n_draws, replace = TRUE, prob = cl$prob)
    }
    data.table::data.table(
      draw = seq_len(n_draws), cluster = cl$cluster[1], zone = cl$zone[pick]
    )
  }) |> data.table::rbindlist()
  return(draws)
}
