## #######################################################################################
##
## Visualize RWI versus poverty
##
## AUTHOR: Nat Henry, nat@henryspatialanalysis.com
## CREATED: September 16, 2025
## PURPOSE: We have two measures of wealth and poverty:
##   1. Meta's Relative Wealth Index
##   2. A Malawi-specific poverty analysis from the World Bank
## Compare these two measures and their respective associations with HIV viraemia
##
## #######################################################################################

REPO_DIR <- '~/repos/mwi.hiv.factors'


## Setup -------------------------------------------------------------------------------->

load_pkgs <- c('terra', 'sf', 'data.table', 'versioning')
lapply(load_pkgs, library, character.only = TRUE) |> invisible()
devtools::load_all(REPO_DIR)

config <- versioning::Config$new(file.path(REPO_DIR, 'config.yaml'))

viz_dir <- config$get_dir_path('prepared_data') |> file.path('viz/poverty_vs_rwi')
dir.create(viz_dir, showWarnings = FALSE, recursive = TRUE)

## Load inputs -------------------------------------------------------------------------->

full_data_by_gvh <- config$read('prepared_data', 'covariates_by_gvh')

# Link districts to regions
admin_bounds <- config$read('catchments', 'admin_bounds', quiet = TRUE)
admin_hierarchy <- admin_bounds |>
  sf::st_drop_geometry() |>
  data.table::as.data.table()
districts_to_regions <- admin_hierarchy[
  area_level == 3L, .(district = area_name, adm2_id = parent_area_id)
] |>
  merge(
    y = admin_hierarchy[area_level == 2L, .(adm2_id = area_id, adm1_id = parent_area_id)],
    by = 'adm2_id'
  ) |>
  merge(
    y = admin_hierarchy[area_level == 1L, .(adm1_id = area_id, region = area_name)],
    by = 'adm1_id'
  ) |>
  _[, .(district, region)]

full_data_by_gvh[districts_to_regions, region := i.region, on = 'district']
full_data_by_gvh <- stats::na.omit(
  full_data_by_gvh,
  cols = c('longitude', 'latitude', 'rwi', 'poverty', 'log_population_1km')
)

# Check whether each metro is in a region
metro_bounds <- admin_bounds |>
  dplyr::filter(area_level == 5L) |>
  dplyr::filter(endsWith(area_name, ' City'))
gvh_pts <- full_data_by_gvh[, .(longitude, latitude)] |>
  sf::st_as_sf(coords = c('longitude', 'latitude'), crs = 'EPSG:4326') |>
  sf::st_join(y = metro_bounds, join = sf::st_within)
full_data_by_gvh$municipality <- gvh_pts$area_name
full_data_by_gvh[, metro_label := ifelse(is.na(municipality), 'Non-metro', 'Metro')]


## Scatter RWI versus poverty ----------------------------------------------------------->

scatter_fig <- ggplot2::ggplot(
  data = full_data_by_gvh,
  mapping = ggplot2::aes(x = rwi, y = poverty, color = region)
) +
  ggplot2::facet_grid('region ~ metro_label', scales = 'free_x') +
  ggplot2::geom_point(alpha = 0.3, mapping = ggplot2::aes(size = population_1km)) +
  ggplot2::geom_smooth(method = 'loess', se = TRUE, color = '#666666', linewidth = 0.25) +
  ggplot2::labs(
    title = 'Scatter: RWI vs. Poverty by GVH',
    x = 'Meta Relative Wealth Index',
    y = 'Poverty (World Bank 2018)'
  ) +
  ggplot2::scale_size_continuous(guide = 'none') +
  ggplot2::scale_color_discrete(guide = 'none') +
  ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, 1), oob = scales::squish) +
  ggplot2::theme_bw()

png(file.path(viz_dir, 'scatter_poverty_vs_rwi.png'), height = 8, width = 6, units = 'in', res = 300)
print(scatter_fig)
dev.off()


## Scatter each indicator vs viraemia --------------------------------------------------->

indicators <- list(
  poverty = c('Poverty', 'Poverty (World Bank 2018)'),
  rwi = c('RWI', 'Meta Relative Wealth Index')
)

for(indicator in names(indicators)){
  viraemia_fig <- ggplot2::ggplot(
    data = full_data_by_gvh,
    mapping = ggplot2::aes(x = get(indicator), y = viraemia15to49_mean, color = region)
  ) +
  ggplot2::facet_grid('region ~ metro_label', scales = 'free_x') +
  ggplot2::geom_point(alpha = 0.3, mapping = ggplot2::aes(size = population_1km)) +
  ggplot2::geom_smooth(method = 'loess', se = TRUE, color = '#666666', linewidth = 0.25) +
  ggplot2::labs(
    title = paste('Scatter: Viraemia vs.', indicators[[indicator]][1], 'by GVH'),
    x = indicators[[indicator]][2],
    y = "Community-level HIV viraemia"
  ) +
  ggplot2::scale_size_continuous(guide = 'none') +
  ggplot2::scale_color_discrete(guide = 'none') +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::theme_bw()
  if(indicator == 'poverty'){
    viraemia_fig <- viraemia_fig + ggplot2::scale_x_continuous(labels = scales::percent)
  }
  png(
    file.path(viz_dir, paste0('scatter_viraemia_vs_', indicator, '.png')),
    height = 8, width = 6, units = 'in', res = 300
  )
  print(viraemia_fig)
  dev.off()
}

## Scatter each indicator vs viraemia, after adjusting for population density ----------->

pop_mod <- stats::loess(viraemia15to49_mean ~ log_population_1km, data = full_data_by_gvh)
preds <- stats::predict(pop_mod, newdata = full_data_by_gvh)
full_data_by_gvh[, viraemia_residuals := viraemia15to49_mean - preds]

for(indicator in names(indicators)){
  viraemia_fig <- ggplot2::ggplot(
    data = full_data_by_gvh,
    mapping = ggplot2::aes(x = get(indicator), y = viraemia_residuals, color = region)
  ) +
  ggplot2::facet_grid('region ~ metro_label', scales = 'free_x') +
  ggplot2::geom_hline(yintercept = 0, color = '#222222', linewidth = 0.25, linetype = 2) +
  ggplot2::geom_point(alpha = 0.3, mapping = ggplot2::aes(size = population_1km)) +
  ggplot2::geom_smooth(method = 'loess', se = TRUE, color = '#666666', linewidth = 0.25) +
  ggplot2::labs(
    title = paste('Scatter: Viraemia vs.', indicators[[indicator]][1], 'by GVH (residuals)'),
    subtitle = 'Adjusted for population density',
    x = indicators[[indicator]][2],
    y = "Residuals: Community-level HIV viraemia, adjusted for population density"
  ) +
  ggplot2::scale_size_continuous(guide = 'none') +
  ggplot2::scale_color_discrete(guide = 'none') +
  ggplot2::scale_y_continuous(labels = function(x) {
    s <- ifelse(x > 0, "+", ifelse(x < 0, "-", ""))
    paste0(s, scales::percent(abs(x)))
  }) +
  ggplot2::theme_bw()
  if(indicator == 'poverty'){
    viraemia_fig <- viraemia_fig + ggplot2::scale_x_continuous(labels = scales::percent)
  }
  png(
    file.path(viz_dir, paste0('scatter_residuals_vs_', indicator, '.png')),
    height = 8, width = 6, units = 'in', res = 300
  )
  print(viraemia_fig)
  dev.off()
}
