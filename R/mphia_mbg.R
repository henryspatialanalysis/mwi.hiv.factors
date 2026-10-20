#' File path for a templated MPHIA output
#'
#' @details Fills `{name}` placeholders in a configured file name and creates the
#'   parent folder if needed.
#'
#' @param config ([versioning::Config]) Project configuration
#' @param dir_name (`character(1)`) Configured directory
#' @param file_name (`character(1)`) Configured file within that directory
#' @param ... Named placeholder values, e.g. `district = 'Lilongwe'`
#'
#' @return Full file path (`character(1)`)
#'
#' @export
mphia_output_path <- function(config, dir_name, file_name, ...){
  path <- config$get_file_path(dir_name, file_name)
  fills <- list(...)
  for(nm in names(fills)){
    path <- gsub(paste0('{', nm, '}'), fills[[nm]], path, fixed = TRUE)
  }
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  return(path)
}


#' Read one MPHIA survey's households with wealth scores and cluster locations
#'
#' @param hh_path (`character(1)`) Household file (.dta)
#' @param centroids_path (`character(1)`) Geomasked cluster centroids (.csv or .dta)
#'
#' @return `data.table` with one row per household with a wealth score and fields
#'   `householdid`, `centroidid`, `zone`, `urban`, `wealth_score`, `hh_weight`,
#'   `longitude`, `latitude`
#'
#' @importFrom data.table as.data.table
#' @export
read_mphia_households <- function(hh_path, centroids_path){
  hh <- haven::read_dta(
    hh_path,
    col_select = dplyr::all_of(c(
      'householdid', 'centroidid', 'zone', 'urban', 'wealthscorecont', 'hhwt0'
    ))
  ) |>
    haven::zap_labels() |>
    data.table::as.data.table()
  centroids <- if(grepl('\\.csv$', centroids_path, ignore.case = TRUE)){
    data.table::fread(centroids_path)
  } else {
    haven::read_dta(centroids_path) |> data.table::as.data.table()
  }
  out <- hh[!is.na(wealthscorecont) & hhwt0 > 0][
    ,
    .(householdid, centroidid, zone, urban, wealth_score = wealthscorecont,
      hh_weight = hhwt0)
  ] |>
    merge(
      centroids[, .(centroidid, longitude, latitude)], by = 'centroidid', all.x = TRUE
    )
  return(out)
}


#' Collapse unit-level MPHIA records to cluster counts for MBG
#'
#' @details Counts are unweighted, as is standard for binomial geostatistical models.
#'   Cluster coordinates are projected to the CRS of the prediction grid.
#'
#' @param dat (`data.table`) Unit records with fields `centroidid`, `longitude`,
#'   `latitude`, and the binary outcome
#' @param outcome (`character(1)`) Binary (0/1) outcome field
#' @param crs (`character(1)`) Projected CRS for the model, e.g. 'EPSG:32736'
#'
#' @return `data.table` with one row per cluster and the fields `mbg::MbgModelRunner`
#'   expects: `cluster_id`, `centroidid`, `x`, `y`, `indicator` (count of ones),
#'   `samplesize`, and `observed` (the cluster proportion)
#'
#' @importFrom sf st_as_sf st_transform st_coordinates
#' @export
mphia_mbg_input <- function(dat, outcome, crs){
  clusters <- dat[
    !is.na(get(outcome)),
    .(indicator = sum(get(outcome)), samplesize = .N),
    by = .(centroidid, longitude, latitude)
  ]
  xy <- sf::st_as_sf(clusters, coords = c('longitude', 'latitude'), crs = 4326) |>
    sf::st_transform(crs) |>
    sf::st_coordinates()
  clusters[, `:=`(
    cluster_id = seq_len(.N), x = xy[, 'X'], y = xy[, 'Y'],
    observed = indicator / samplesize
  )]
  data.table::setcolorder(
    clusters, c('cluster_id', 'centroidid', 'x', 'y', 'indicator', 'samplesize')
  )
  return(clusters[, .(cluster_id, centroidid, x, y, indicator, samplesize, observed)])
}


#' Build a projected prediction grid covering a study area
#'
#' @param polygon_sf (`sf`) Study area
#' @param resolution (`numeric(1)`) Cell size in the units of `crs`
#' @param crs (`character(1)`) Projected CRS
#'
#' @return ID raster (`SpatRaster`) from [mbg::build_id_raster()]
#'
#' @importFrom sf st_transform st_union st_sf
#' @export
build_study_area_id_raster <- function(polygon_sf, resolution, crs){
  area <- sf::st_sf(geometry = sf::st_union(sf::st_transform(polygon_sf, crs)))
  bbox <- sf::st_bbox(area)
  template <- terra::rast(
    xmin = floor(bbox$xmin / resolution) * resolution,
    xmax = ceiling(bbox$xmax / resolution) * resolution,
    ymin = floor(bbox$ymin / resolution) * resolution,
    ymax = ceiling(bbox$ymax / resolution) * resolution,
    resolution = resolution,
    crs = crs
  )
  return(mbg::build_id_raster(polygons = terra::vect(area), template_raster = template))
}


#' Fit an intercept-only SPDE model and predict on a grid
#'
#' @details Runs `mbg::MbgModelRunner` with no covariates: an intercept, an SPDE
#'   Gaussian process, and the default cluster nugget. Mesh settings and the range
#'   prior are in the units of the grid's CRS. `mbg` takes the range threshold as a
#'   proportion of the mesh's largest extent, so the distance given here is divided by
#'   the extent of the same mesh `mbg` builds (from the cluster points, with the grid
#'   cells as the domain). Other priors use the `mbg` defaults.
#'
#' @param input_data (`data.table`) Output of [mphia_mbg_input()]
#' @param id_raster (`SpatRaster`) Output of [build_study_area_id_raster()]
#' @param mesh_max_edge (`numeric(2)`) Inner and outer maximum triangle edge
#' @param mesh_cutoff (`numeric(1)`) Minimum distance between mesh vertices
#' @param prior_spde_range (`list`) PC prior for the range, with items `threshold` (a
#'   distance in CRS units) and `prob_below`
#' @param n_samples (`integer(1)`) Posterior draws
#' @param ui_width (`numeric(1)`) Width of the uncertainty interval, e.g. 0.8
#' @param use_nugget (`logical(1)`, default TRUE) Include the cluster nugget, in both
#'   fitting and prediction
#'
#' @return List with items:
#'   - `predictions`: `SpatRaster` with layers `mean`, `lower`, `upper`, `ci_width`
#'   - `hyperparameters`: `data.table` of posterior summaries of the hyperparameters
#'   - `mesh_nodes`: number of mesh vertices
#'   - `range_threshold_share`: the range threshold passed to `mbg`, as a proportion
#'     of the mesh extent
#'   - `validity`: one-row `data.table` of in-sample fit statistics from `mbg`: RMSE
#'     of cluster proportions, log posterior density, and WAIC
#'
#' @export
fit_mphia_mbg <- function(
  input_data, id_raster, mesh_max_edge, mesh_cutoff, prior_spde_range, n_samples,
  ui_width, use_nugget = TRUE
){
  # INLA builds a much coarser mesh from integer edge lengths, as YAML parses "[1, 10]"
  mesh_max_edge <- as.numeric(mesh_max_edge)
  mesh_cutoff <- as.numeric(mesh_cutoff)
  domain <- stats::na.omit(data.table::as.data.table(id_raster, xy = TRUE))
  mesh <- INLA::inla.mesh.2d(
    loc = input_data[, .(x, y)], loc.domain = domain[, .(x, y)],
    max.edge = mesh_max_edge, cutoff = mesh_cutoff
  )
  max_extent <- max(apply(mesh$loc[, 1:2], 2, function(x) diff(range(x))))
  range_share <- prior_spde_range$threshold / max_extent
  runner <- mbg::MbgModelRunner$new(
    input_data = input_data,
    id_raster = id_raster,
    use_covariates = FALSE,
    mesh_max_edge = mesh_max_edge,
    mesh_cutoff = mesh_cutoff,
    prior_spde_range = list(
      threshold = range_share, prob_below = prior_spde_range$prob_below
    ),
    use_nugget = use_nugget,
    nugget_in_predict = use_nugget,
    verbose = FALSE
  )
  runner$fit_mbg_model()
  preds <- runner$generate_predictions(n_samples = n_samples, ui_width = ui_width)
  out_rast <- c(
    preds$cell_pred_mean, preds$cell_pred_lower, preds$cell_pred_upper,
    preds$cell_pred_upper - preds$cell_pred_lower
  )
  names(out_rast) <- c('mean', 'lower', 'upper', 'ci_width')
  hyper <- runner$inla_fitted_model$summary.hyperpar
  hyperparameters <- data.table::data.table(
    parameter = rownames(hyper),
    mean = hyper$mean,
    lower = hyper[['0.025quant']],
    median = hyper[['0.5quant']],
    upper = hyper[['0.975quant']]
  )
  return(list(
    predictions = out_rast,
    hyperparameters = hyperparameters,
    mesh_nodes = nrow(runner$inla_inputs_list$mesh$loc),
    range_threshold_share = range_share,
    validity = runner$get_predictive_validity(in_sample = TRUE)
  ))
}


#' Average prediction surfaces within the city and the rest of the district
#'
#' @param predictions (`SpatRaster`) Layers from [fit_mphia_mbg()], e.g. `mean` and
#'   `ci_width`
#' @param city_sf (`sf`) City boundary; cells whose centres fall inside it are 'City'
#'   and other modelled cells 'Rural'
#'
#' @return `data.table` with fields `area` ('District', 'City', 'Rural'), `n_cells`,
#'   and the cell average of each layer
#'
#' @importFrom sf st_transform
#' @export
summarize_surface_by_area <- function(predictions, city_sf){
  city <- terra::rasterize(
    terra::vect(sf::st_transform(city_sf, terra::crs(predictions))),
    predictions[[1]], field = 1
  )
  cells <- data.table::as.data.table(c(predictions, city))
  data.table::setnames(cells, ncol(cells), 'in_city')
  cells <- cells[!is.na(get(names(predictions)[1]))]
  cells[, area := data.table::fifelse(is.na(in_city), 'Rural', 'City')]
  layers <- names(predictions)
  by_area <- cells[, c(list(n_cells = .N), lapply(.SD, mean)), by = area, .SDcols = layers]
  district <- cells[, c(list(area = 'District', n_cells = .N), lapply(.SD, mean)),
    .SDcols = layers]
  return(data.table::rbindlist(list(district, by_area), use.names = TRUE))
}
