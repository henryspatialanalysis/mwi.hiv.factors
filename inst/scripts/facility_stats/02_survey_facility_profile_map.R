## #####################################################################################
##
## PURPOSE: Static map of the RESPOND survey facilities for the next analysis stage.
##
##   One figure assembled with gridExtra: a national locator (survey districts in red),
##   then per-region panels (Central; North and South stacked) each zoomed to the
##   districts that contain survey facilities, with a single shared legend along the
##   bottom. Each facility is a dot, filled by its manually-vetted profile (matching the
##   cell-fill colors in the "Facility stats" tab of the RESPOND profiling workbook) and
##   sized by the PLHIV living in its catchment. District and region boundaries sit
##   beneath the dots; survey districts are shaded light grey and labelled.
##
##   All input/output paths and parameters live in config.yaml.
##
## #####################################################################################

REPO_DIR <- '~/repos/mwi.hiv.factors'

## SETUP -------------------------------------------------------------------------------->

load_pkgs <- c(
  'data.table', 'ggplot2', 'sf', 'gridExtra', 'scales', 'glue', 'versioning'
)
lapply(load_pkgs, library, character.only = TRUE) |> invisible()
devtools::load_all(REPO_DIR)

config <- versioning::Config$new(file.path(REPO_DIR, 'config.yaml'))

# Profile fill colors, read from the workbook's "Profile" column cell fills. Teal/Pink/
# Green/Yellow are direct rgb fills; Orange/Purple are theme accents 6 and 4.
profile_colors <- c(
  Teal   = '#A8DADC',
  Pink   = '#F4C2C2',
  Green  = '#33cc33',
  Yellow = '#FFE699',
  Orange = '#F79646',
  Purple = '#8064A2'
)
DISTRICT_FILL <- '#D9D9D9'   # light grey for survey districts
FILL_ALPHA <- 0.75           # dot fill opacity (baked into palette; outline stays solid)

## LOAD INPUTS -------------------------------------------------------------------------->

# Survey facilities: name (col A), profile (col B), district (col C) from the workbook
template_fp <- path.expand(
  config$get_file_path('facility_surveys', 'facility_stats_template')
)
survey <- openxlsx2::wb_to_df(
  openxlsx2::wb_load(template_fp), sheet = 'Facility stats', dims = 'A4:C35'
) |>
  data.table::as.data.table()
data.table::setnames(survey, c('Facility', 'Profile', 'District'),
                     c('template_name', 'profile', 'district'))

# Name crosswalk -> facility_id
crosswalk <- data.table::fread(
  path.expand(config$get_file_path('repo', 'facility_stats_crosswalk'))
)[, .(template_name, facility_id)]

# Facility metadata -> coordinates + catchment_id
facilities <- config$read('catchments', 'facility_metadata') |>
  _[, .(facility_id, longitude, latitude, catchment_id)]

# PLHIV per facility catchment (15-49)
catchment_summary <- config$read('splitting', 'aggregated_results')
fac_plhiv <- catchment_summary[
  aggregation_level == 'FACILITY_CATCHMENTS',
  .(catchment_id = as.integer(id), plhiv15to49_mean)
]

# Admin boundaries: regions (level 1) and districts (level 3), both from one geojson
admin_bounds <- config$read('catchments', 'admin_bounds', quiet = TRUE)
districts_sf <- admin_bounds |> dplyr::filter(area_level == 3L)
regions_sf <- admin_bounds |> dplyr::filter(area_level == 1L)

## ASSEMBLE FACILITY POINTS ------------------------------------------------------------->

facility_dt <- survey |>
  merge(crosswalk, by = 'template_name', all.x = TRUE, sort = FALSE) |>
  merge(facilities, by = 'facility_id', all.x = TRUE, sort = FALSE) |>
  merge(fac_plhiv, by = 'catchment_id', all.x = TRUE, sort = FALSE)

# Warn on any unresolved join (mirrors 00_general_stats.R validation)
for(col in c('facility_id', 'longitude', 'latitude', 'catchment_id', 'plhiv15to49_mean')){
  bad <- facility_dt[is.na(get(col)), template_name]
  if(length(bad) > 0L) warning('Missing ', col, ' for: ', paste(bad, collapse = ', '))
}

facility_dt[, profile := factor(profile, levels = names(profile_colors))]

facility_sf <- sf::st_as_sf(
  facility_dt, coords = c('longitude', 'latitude'), crs = 4326, remove = FALSE
)

## DISTRICT -> REGION LOOKUP (level 3 -> level 2 -> level 1) ----------------------------->

ab <- data.table::as.data.table(sf::st_drop_geometry(admin_bounds))
l1 <- ab[area_level == 1L, .(area_id, region = area_name)]
l2 <- ab[area_level == 2L, .(area_id, parent_area_id)]
l2[l1, region := i.region, on = c(parent_area_id = 'area_id')]
district_region <- ab[area_level == 3L, .(district = area_name, parent_area_id)]
district_region[l2, region := i.region, on = c(parent_area_id = 'area_id')]
district_region <- district_region[, .(district, region)]

facility_dt[district_region, region := i.region, on = 'district']
facility_sf <- merge(
  facility_sf, unique(facility_dt[, .(template_name, region)]),
  by = 'template_name', all.x = TRUE
)

# Survey districts and their regions
survey_districts <- unique(facility_dt[, .(district, region)])
region_levels <- c('Northern', 'Central', 'Southern')
region_labels <- c(Northern = 'North', Central = 'Central', Southern = 'South')

## SHARED SCALES, THEME, AND HELPERS ---------------------------------------------------->

REGION_RED <- '#E41A1C'      # survey-district highlight on the national locator
REGION_LW <- 1.5             # region-boundary line width (solid black)

# Pad a bbox by a fraction of its span on every side
pad_bbox <- function(bb, frac = 0.08){
  dx <- (bb[['xmax']] - bb[['xmin']]) * frac
  dy <- (bb[['ymax']] - bb[['ymin']]) * frac
  c(xmin = bb[['xmin']] - dx, xmax = bb[['xmax']] + dx,
    ymin = bb[['ymin']] - dy, ymax = bb[['ymax']] + dy)
}

# Aspect ratio (east-west : north-south) of a bbox under coord_sf. Uses the bbox's own
# mid-latitude, since coord_sf scales each panel by its own latitude; a single shared
# correction would skew widths (e.g. render North wider than South).
aspect <- function(bb){
  cosf <- cos((bb[['ymin']] + bb[['ymax']]) / 2 * pi / 180)
  ((bb[['xmax']] - bb[['xmin']]) * cosf) / (bb[['ymax']] - bb[['ymin']])
}

# Padded bbox of a region's survey districts
region_bbox <- function(region_name){
  rd <- survey_districts[region == region_name, district]
  pad_bbox(sf::st_bbox(districts_sf[districts_sf$area_name %in% rd, ]), frac = 0.10)
}

# National-locator bbox: whole country, light padding, extra headroom for the title
malawi_bbox <- function(){
  bb <- pad_bbox(sf::st_bbox(districts_sf), frac = 0.02)
  bb[['ymax']] <- bb[['ymax']] + 0.10 * (bb[['ymax']] - bb[['ymin']])
  bb
}

# Grow a bbox (centred) to a target east-west:north-south aspect, expanding whichever
# dimension is deficient so nothing is cropped. Used to give North and South a common
# aspect (hence equal panel width).
expand_bbox_aspect <- function(bb, target){
  w <- bb[['xmax']] - bb[['xmin']]
  h <- bb[['ymax']] - bb[['ymin']]
  cosf <- cos((bb[['ymin']] + bb[['ymax']]) / 2 * pi / 180)
  if(aspect(bb) < target){
    pad <- (target * h / cosf - w) / 2
    bb[['xmin']] <- bb[['xmin']] - pad; bb[['xmax']] <- bb[['xmax']] + pad
  } else {
    pad <- ((w * cosf) / target - h) / 2
    bb[['ymin']] <- bb[['ymin']] - pad; bb[['ymax']] <- bb[['ymax']] + pad
  }
  bb
}

# Shared scales so dots are comparable across panels. Area limits are 500 and 10,000
# PLHIV; values outside are squished onto those bounds (the "<=500" / ">=10,000" keys).
fill_scale <- ggplot2::scale_fill_manual(
  values = scales::alpha(profile_colors, FILL_ALPHA), name = 'Profile', drop = FALSE
)
size_scale <- ggplot2::scale_size_area(
  max_size = 7.5, limits = c(500, 10000), oob = scales::squish,
  breaks = c(500, 1000, 2000, 5000, 10000),
  labels = c('≤500', '1,000', '2,000', '5,000', '≥10,000'),
  name = 'PLHIV in catchment'
)

# Common map theme: no axes, no grid, no panel background, no legend on the panels
map_theme <- ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.text = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    plot.margin = ggplot2::margin(2, 0, 2, 0),
    legend.position = 'none'
  )

# Place a panel title inside a padded bbox. `corner` combines top/bottom with left/right;
# omit left/right (e.g. 'top') to centre horizontally. `off` is the title's inset from the
# top/bottom edge as a fraction of bbox height (use a larger value on half-height panels
# so titles align vertically across panels of different physical height).
panel_title <- function(bb, label, size = 4.5, corner = 'topleft', off = 0.07){
  w <- bb[['xmax']] - bb[['xmin']]
  h <- bb[['ymax']] - bb[['ymin']]
  left <- grepl('left', corner)
  right <- grepl('right', corner)
  top <- grepl('top', corner)
  ggplot2::annotate(
    'text',
    x = if(left) bb[['xmin']] + 0.04 * w
        else if(right) bb[['xmax']] - 0.04 * w
        else (bb[['xmin']] + bb[['xmax']]) / 2,
    y = if(top) bb[['ymax']] - off * h else bb[['ymin']] + off * h,
    label = label, hjust = if(left) 0 else if(right) 1 else 0.5,
    vjust = if(top) 1 else 0, fontface = 'bold', size = size
  )
}

# Pull the single (bottom) legend grob out of a donor plot. ggplot 4.0 emits several
# empty guide-box cells, so select the one that actually contains keys.
extract_legend <- function(p){
  g <- ggplot2::ggplotGrob(p)
  boxes <- g$grobs[grepl('guide-box', g$layout$name)]
  nonempty <- Filter(function(b){
    inherits(b, 'gtable') && length(b$grobs) > 0 &&
      !all(vapply(b$grobs, function(x) inherits(x, 'zeroGrob'), logical(1)))
  }, boxes)
  if(length(nonempty) > 0) nonempty[[1]] else boxes[[1]]
}

# District-name positions: a hand-tuned table of lon/lat anchors. Seeded with district
# centroids, then nudged until no label overlaps a facility dot or a district boundary.
district_labels <- data.table::data.table(
  district = c('Karonga', 'Kasungu', 'Lilongwe', 'Dowa', 'Chikwawa', 'Mulanje', 'Nsanje'),
  x        = c( 33.883,    33.386,    33.618,     33.830,  34.709,     35.649,    35.143 ),
  y        = c(-10.087,   -12.887,   -14.160,    -13.620, -16.166,    -15.970,   -16.747 )
)

## BUILD PANELS ------------------------------------------------------------------------->

# (1) National locator: survey districts in red, solid region boundaries
build_malawi_panel <- function(bb){
  survey_all <- districts_sf[districts_sf$area_name %in% survey_districts$district, ]
  # Line widths are half those of the region panels on this small locator
  ggplot2::ggplot() +
    ggplot2::geom_sf(data = districts_sf, fill = 'white', color = '#444444', linewidth = 0.25) +
    ggplot2::geom_sf(data = survey_all, fill = REGION_RED, color = '#444444', linewidth = 0.25) +
    ggplot2::geom_sf(
      data = regions_sf, fill = NA, color = 'black', linewidth = REGION_LW * 0.5
    ) +
    ggplot2::coord_sf(
      xlim = c(bb[['xmin']], bb[['xmax']]), ylim = c(bb[['ymin']], bb[['ymax']]),
      expand = FALSE
    ) +
    panel_title(bb, 'Survey districts', size = 4.5, corner = 'top', off = 0.01) +
    map_theme
}

# (2-4) Region panel: grey survey districts, dots by profile/PLHIV, in-panel region title
build_region_panel <- function(region_name, bb, corner = 'topleft', title_off = 0.07){
  reg_districts <- survey_districts[region == region_name, district]
  reg_grey <- districts_sf[districts_sf$area_name %in% reg_districts, ]
  reg_points <- facility_sf[facility_sf$region == region_name, ]
  lab_dt <- district_labels[district %in% reg_districts]

  ggplot2::ggplot() +
    ggplot2::geom_sf(data = reg_grey, fill = DISTRICT_FILL, color = NA) +
    ggplot2::geom_sf(data = districts_sf, fill = NA, color = '#444444', linewidth = 0.5) +
    ggplot2::geom_sf(
      data = regions_sf, fill = NA, color = 'black', linewidth = REGION_LW
    ) +
    ggplot2::geom_sf(
      data = reg_points, ggplot2::aes(fill = profile, size = plhiv15to49_mean),
      shape = 21, color = 'black', stroke = 0.25
    ) +
    ggplot2::geom_text(
      data = lab_dt, ggplot2::aes(x = x, y = y, label = district),
      size = 3.0, fontface = 'bold', color = '#222222'
    ) +
    fill_scale + size_scale +
    ggplot2::coord_sf(
      xlim = c(bb[['xmin']], bb[['xmax']]), ylim = c(bb[['ymin']], bb[['ymax']]),
      expand = FALSE
    ) +
    panel_title(bb, region_labels[[region_name]], corner = corner, off = title_off) +
    map_theme +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(fill = NA, color = 'black', linewidth = 0.6)
    )
}

## ASSEMBLE WITH gridExtra::grid.arrange ------------------------------------------------->

# Panel extents. North and South are grown to a common aspect so they render at equal
# width and align in the right-hand column.
malawi_bb <- malawi_bbox()
central_bb <- region_bbox('Central')
north_bb <- region_bbox('Northern')
south_bb <- region_bbox('Southern')
ns_aspect <- max(aspect(north_bb), aspect(south_bb))
north_bb <- expand_bbox_aspect(north_bb, ns_aspect)
south_bb <- expand_bbox_aspect(south_bb, ns_aspect)

panels <- list(
  build_malawi_panel(malawi_bb),
  build_region_panel('Central', central_bb, corner = 'topleft', title_off = 0.01),
  build_region_panel('Northern', north_bb, corner = 'topright', title_off = 0.02),
  build_region_panel('Southern', south_bb, corner = 'bottomleft')
)

# Donor plot supplies one shared legend (all six profiles; titles above their keys)
legend_donor <- ggplot2::ggplot(
  facility_dt,
  ggplot2::aes(x = longitude, y = latitude, fill = profile, size = plhiv15to49_mean)
) +
  ggplot2::geom_point(shape = 21, color = 'black', stroke = 0.25) +
  fill_scale + size_scale +
  ggplot2::guides(
    fill = ggplot2::guide_legend(
      title.position = 'top', title.hjust = 0.5, nrow = 1,
      override.aes = list(size = 4), order = 1
    ),
    size = ggplot2::guide_legend(
      title.position = 'top', title.hjust = 0.5, nrow = 1, order = 2
    )
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = 'bottom', legend.box = 'horizontal',
    legend.title = ggplot2::element_text(size = 12, face = 'bold'),
    legend.text = ggplot2::element_text(size = 10)
  )
legend_grob <- extract_legend(legend_donor)

# Layout: col 1 = Malawi, col 2 = Central (both full height); col 3 = North over South;
# bottom row = shared legend.
layout_matrix <- rbind(
  c(1, 2, 3),
  c(1, 2, 4),
  c(5, 5, 5)
)
ROW_HEIGHTS <- c(1, 1, 0.22)   # two map rows + a short legend row

# Size each column to its map's true width at the available row height, so panels fill
# their cells and the image carries no horizontal whitespace between facets.
IMG_H <- 8
map_h <- IMG_H * sum(ROW_HEIGHTS[1:2]) / sum(ROW_HEIGHTS)   # height of the two map rows
col_widths <- c(
  map_h * aspect(malawi_bb),          # Malawi spans both map rows
  map_h * aspect(central_bb),         # Central spans both map rows
  (map_h / 2) * ns_aspect             # North / South each span one map row
)
IMG_W <- sum(col_widths)

## SAVE --------------------------------------------------------------------------------->

out_fp <- file.path(
  config$get_dir_path('facility_stats_output'), 'survey_facility_profile_map.png'
)
grDevices::png(out_fp, width = IMG_W, height = IMG_H, units = 'in', res = 300)
gridExtra::grid.arrange(
  grobs = c(lapply(panels, ggplot2::ggplotGrob), list(legend_grob)),
  layout_matrix = layout_matrix,
  widths = grid::unit(col_widths, 'null'),
  heights = grid::unit(ROW_HEIGHTS, 'null')
)
grDevices::dev.off()
message('Wrote survey facility profile map to: ', out_fp)
