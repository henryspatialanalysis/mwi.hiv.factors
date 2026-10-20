## #######################################################################################
##
## MPHIA 03. MAPS OF MBG ESTIMATES AND SURVEY CLUSTER OUTCOMES BY DISTRICT
##
## AUTHOR: Nat Henry, nat@henryspatialanalysis.com
## CREATED: 2026-10-20
## PURPOSE: Map the outputs of 02_mbg_fit.R for each model version, district, survey,
##   and outcome:
##   - mean: posterior mean surface
##   - ci_width: width of the 80% uncertainty interval
##   and, once per district, survey, and outcome, the model inputs:
##   - points: observed cluster proportions, sized by sample size
##   Maps are framed on the city (see `map: extent`) over the Close (wtm.frontend)
##   basemap, rebuilt from Stadia OpenMapTiles vector tiles and the Close style. Layers,
##   bottom to top: Close land cover and residential areas, the outcome (raster at 70%
##   opacity), Close water, roads, and buildings, survey points, the district outline,
##   the city outline, and Close labels, read from higher-zoom tiles. The Stadia API key
##   is read from the STADIA_API_KEY environment variable.
##   Colour limits are shared across model versions, districts, and surveys for each
##   outcome and map type; mean and point maps of an outcome share one scale.
##
##   Outputs (mphia_mbg directory):
##   - {variant}/maps/{district}_{survey}_{outcome}_{stat}.png
##   - input_maps/{district}_{survey}_{outcome}_points.png
##
## #######################################################################################

REPO_DIR <- '~/repos/mwi.hiv.factors'


## SETUP -------------------------------------------------------------------------------->

load_pkgs <- c('data.table', 'sf', 'terra', 'ggplot2', 'versioning')
lapply(load_pkgs, library, character.only = TRUE) |> invisible()
devtools::load_all(REPO_DIR, quiet = TRUE)

config <- versioning::Config$new(file.path(REPO_DIR, 'config.yaml'))
settings <- config$get('mphia')
mbg_settings <- settings$mbg
map_settings <- mbg_settings$map
sf::sf_use_s2(FALSE)

survey_ids <- unlist(mbg_settings$run_surveys)
districts <- names(settings$cities)
outcomes <- names(mbg_settings$outcomes)
path_for <- function(key, ...) mphia_output_path(config, 'mphia_mbg', key, ...)


## LOAD INPUTS AND PREDICTIONS ---------------------------------------------------------->

variants <- unlist(mbg_settings$run_variants)
inputs <- data.table::CJ(
  district = districts, survey = survey_ids, outcome = outcomes, sorted = FALSE
)
runs <- data.table::CJ(
  variant = variants, district = districts, survey = survey_ids, outcome = outcomes,
  sorted = FALSE
)
predictions <- lapply(seq_len(nrow(runs)), function(ii){
  terra::rast(path_for(
    'mbg_predictions', variant = runs$variant[ii],
    district = runs$district[ii], survey = runs$survey[ii], outcome = runs$outcome[ii]
  ))
})
points <- lapply(seq_len(nrow(inputs)), function(ii){
  data.table::fread(path_for(
    'mbg_input',
    district = inputs$district[ii], survey = inputs$survey[ii],
    outcome = inputs$outcome[ii]
  )) |>
    sf::st_as_sf(coords = c('x', 'y'), crs = mbg_settings$crs)
})

# Shared colour limits by outcome: mean and point maps share the range of the mean
#  surfaces (points beyond it are squished); CI width maps start at zero
limits <- lapply(outcomes, function(oo){
  idx <- which(runs$outcome == oo)
  mean_max <- max(vapply(idx, function(ii){
    terra::global(predictions[[ii]]$mean, 'max', na.rm = TRUE)[[1]]
  }, numeric(1)))
  mean_min <- min(vapply(idx, function(ii){
    terra::global(predictions[[ii]]$mean, 'min', na.rm = TRUE)[[1]]
  }, numeric(1)))
  ci_max <- max(vapply(idx, function(ii){
    terra::global(predictions[[ii]]$ci_width, 'max', na.rm = TRUE)[[1]]
  }, numeric(1)))
  list(
    mean = c(floor(mean_min * 20) / 20, ceiling(mean_max * 20) / 20),
    ci_width = c(0, ceiling(ci_max * 20) / 20)
  )
}) |> stats::setNames(outcomes)


## STUDY AREAS AND TILES ---------------------------------------------------------------->

admin_bounds <- config$read('catchments', 'admin_bounds', quiet = TRUE)
areas_sf <- admin_bounds[admin_bounds$area_level == settings$area_level, 'area_name']
basemap_settings <- map_settings$basemap
basemap_fonts <- register_basemap_fonts(basemap_settings$italic_font)
study_areas <- lapply(districts, function(district_name){
  area_names <- unlist(settings$cities[[district_name]])
  district_sf <- sf::st_sf(
    geometry = sf::st_union(areas_sf[areas_sf$area_name %in% area_names, ])
  )
  city_sf <- areas_sf[areas_sf$area_name == settings$cities[[district_name]]$City, ]
  frame_sf <- if(map_settings$extent == 'city') city_sf else district_sf
  bbox <- map_bbox(frame_sf, buffer = map_settings$extent_buffer)
  base_zoom <- map_zoom(bbox, map_settings$width_in) + basemap_settings$base_zoom_offset
  label_zoom <- min(
    base_zoom + basemap_settings$label_zoom_offset, basemap_settings$max_tile_zoom
  )
  message(
    district_name, ': basemap features at zoom ', base_zoom, ', labels at zoom ',
    label_zoom
  )
  list(
    district = district_sf,
    city = city_sf,
    bbox = bbox,
    basemap = close_basemap_layers(
      style_path = config$get_file_path('repo', 'close_basemap_style'),
      below = unlist(basemap_settings$below),
      above = unlist(basemap_settings$above),
      bbox = bbox,
      width_in = map_settings$width_in,
      base_zoom = base_zoom,
      label_zoom = label_zoom,
      label_style_zoom = basemap_settings$label_style_zoom,
      tile_url = basemap_settings$tile_url,
      api_key = Sys.getenv(basemap_settings$api_key_env),
      cache_dir = config$get_dir_path('vector_tile_cache'),
      label_scale = basemap_settings$label_scale,
      line_scale = basemap_settings$line_scale,
      fonts = basemap_fonts,
      background = basemap_settings$background
    )
  )
}) |> stats::setNames(districts)


## MAPS --------------------------------------------------------------------------------->

ci_colors <- rev(rcartocolor::carto_pal(7, mbg_settings$ci_palette))
stat_labels <- c(
  mean = 'Mean estimate',
  ci_width = paste0(100 * mbg_settings$ui_width, '% CI width'),
  points = 'Survey clusters, observed'
)
fill_scale_for <- function(outcome, stat){
  palette <- mbg_settings$outcomes[[outcome]]$palette
  if(stat == 'ci_width'){
    return(ggplot2::scale_fill_gradientn(
      colours = ci_colors, limits = limits[[outcome]]$ci_width,
      labels = function(x) paste0(round(100 * x), ' pts'), oob = scales::squish,
      na.value = 'transparent', name = 'CI width'
    ))
  }
  return(ggplot2::scale_fill_distiller(
    palette = palette, direction = 1, limits = limits[[outcome]]$mean,
    labels = scales::label_percent(accuracy = 1), oob = scales::squish,
    na.value = 'transparent', name = if(stat == 'points') 'Observed' else 'Mean'
  ))
}
draw_map <- function(district, survey, outcome, stat, title_suffix, out_file, ...){
  area <- study_areas[[district]]
  fig <- outcome_basemap_map(
    basemap = area$basemap,
    district_sf = area$district,
    city_sf = area$city,
    fill_scale = fill_scale_for(outcome, stat),
    bbox = area$bbox,
    raster_alpha = map_settings$raster_alpha,
    district_line = map_settings$district_line,
    city_line = map_settings$city_line,
    caption = basemap_settings$citation,
    title = paste0(
      district, ' district, ', settings$surveys[[survey]]$label, title_suffix, '\n',
      mbg_settings$outcomes[[outcome]]$label, ': ', stat_labels[[stat]]
    ),
    ...
  )
  ggplot2::ggsave(
    out_file, fig, width = map_settings$width_in, height = map_settings$height_in,
    dpi = map_settings$dpi, bg = 'white', device = ragg::agg_png
  )
  message('Wrote ', out_file)
}

# Model estimates, by version
for(ii in seq_len(nrow(runs))){
  run <- runs[ii]
  for(stat in c('mean', 'ci_width')){
    draw_map(
      district = run$district, survey = run$survey, outcome = run$outcome, stat = stat,
      title_suffix = paste0(' (', tolower(mbg_settings$variants[[run$variant]]$label), ')'),
      out_file = path_for(
        'mbg_map', variant = run$variant, district = run$district,
        survey = run$survey, outcome = run$outcome, stat = stat
      ),
      surface = predictions[[ii]][[stat]]
    )
  }
}

# Model inputs
for(ii in seq_len(nrow(inputs))){
  run <- inputs[ii]
  draw_map(
    district = run$district, survey = run$survey, outcome = run$outcome,
    stat = 'points', title_suffix = '',
    out_file = path_for(
      'mbg_points_map', district = run$district, survey = run$survey,
      outcome = run$outcome
    ),
    points = points[[ii]]
  )
}
