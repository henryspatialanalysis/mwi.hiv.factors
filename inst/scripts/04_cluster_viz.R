## #######################################################################################
##
## 04. VISUALIZE CLUSTER CHARACTERISTICS
##
## Create descriptive plots of facility catchment archetypes
## AUTHOR: Nat Henry, nat@henryspatialanalysis.com
## DATE: 2025-08-18
##
## #######################################################################################

REPO_DIR <- '~/repos/mwi.hiv.factors'


## SETUP -------------------------------------------------------------------------------->

load_pkgs <- c('data.table', 'ggplot2', 'glue', 'versioning')
lapply(load_pkgs, library, character.only = TRUE) |> invisible()
devtools::load_all(REPO_DIR)
# Load config
config <- versioning::Config$new(file.path(REPO_DIR, 'config.yaml'))

# Load spatial data for map
catchments_sf <- config$read('catchments', 'facility_catchments')
catchments_sf$catchment_id <- seq_len(nrow(catchments_sf))
admin_bounds <- config$read("catchments", "admin_bounds", quiet = TRUE)
districts_sf <- admin_bounds |> dplyr::filter(area_level == 3L)
regions_sf <- admin_bounds |> dplyr::filter(area_level == 1L)

# Load facility data, combined across all regional analyses
facility_metadata <- config$read('catchments', 'facility_metadata')
cov_data <- config$read('prepared_data', 'covariates_by_facility_discretized')

# Set up viz directory
model_dir <- config$get_dir_path('analysis')
viz_dir <- file.path(model_dir, 'Visualization')
dir.create(viz_dir, showWarnings = FALSE)


## Create table linking each catchment to its cluster ----------------------------------->

cluster_counts <- config$get('cluster_counts')
group_names <- names(cluster_counts)

cluster_assignments <- lapply(group_names, function(group_name){
  file.path(model_dir, group_name, 'pca_kmeans_results.csv') |>
    data.table::fread() |>
    data.table::setnames(
      old = paste0('kmeans_', cluster_counts[group_name]),
      new = 'cluster_number'
    ) |>
    _[, .(
      catchment_id,
      pca_group = group_name,
      cluster = paste0(group_name, ' C', cluster_number)
    )]
}) |>
  data.table::rbindlist()

var_labels <- data.table::data.table(
  variable = config$get('covariates') |> names(),
  var_label = config$get('covariates') |>
    unlist() |> unname() |> gsub(pattern = ' ', replacement = '\n')
)

covariate_cluster_summaries <- lapply(group_names, function(group_name){
  file.path(model_dir, group_name, 'pca_kmeans_covariate_summaries.csv') |>
    data.table::fread() |>
    _[n_clusters == cluster_counts[[group_name]], ] |>
    _[
      var_labels,
      var_label := gsub(pattern = '\\n', replacement = ' ', x = i.var_label),
      on = 'variable'
    ] |>
    _[
      variable %in% c('mobility', 'econ_activity', 'pop_growth', 'weather'),
      `:=` (
        q1 = mean_cluster,
        q5 = 1 - mean_cluster
      )] |>
    _[, .(
      pca_group = group_name,
      cluster = paste(group_name, gsub('Cluster ', 'C', cluster)),
      variable = var_label,
      z_score = (mean_cluster - mean_baseline) / sd_baseline,
      pct_in_bottom_quintile = q1,
      pct_in_top_quintile = q5
    )]
}) |>
  data.table::rbindlist()

data.table::fwrite(
  covariate_cluster_summaries,
  file.path(viz_dir, 'covariate_summaries_by_profile.csv')
)

## Set colors for each cluster ---------------------------------------------------------->

possible_colors <- c(
  RColorBrewer::brewer.pal(n = 8, name = 'Set2'),
  RColorBrewer::brewer.pal(n = 8, name = 'Dark2'),
  RColorBrewer::brewer.pal(n = 8, name = 'Set1')
)

all_cluster_names <- lapply(
  group_names,
  function(group_name) paste0(group_name, ' C', seq_len(cluster_counts[[group_name]]))
) |>
  unlist()
cluster_colors <- possible_colors[seq_along(all_cluster_names)]
names(cluster_colors) <- all_cluster_names


## PREPARE DATA ------------------------------------------------------------------------->

cov_data_merged <- merge(
  x = cov_data,
  y = cluster_assignments,
  by = 'catchment_id'
)
catchments_sf_merged <- catchments_sf |>
  dplyr::select(catchment_id) |>
  merge(y = cov_data_merged, by = 'catchment_id')

cov_data_long <- data.table::copy(cov_data_merged) |>
  _[, c('cluster', 'catchment_id', var_labels$variable), with = FALSE] |>
  melt(
    id.vars = c('cluster', 'catchment_id'),
    variable.name = 'variable',
    value.name = 'value'
  ) |>
  _[var_labels, on = 'variable', var_label := i.var_label] |>
  suppressWarnings()

# Get overall and cluster level means, to be plotted as vertical lines
overall_means <- cov_data_long[
  , .(mean_val = mean(value, na.rm = TRUE)), by = .(variable, var_label)
]
cluster_means <- cov_data_long[
  , .(mean_val = mean(value, na.rm = TRUE)), by = .(cluster, variable, var_label)
]

# Add a combined group
cov_data_long_all <- data.table::rbindlist(list(
  data.table::copy(cov_data_long)[, cluster := 'All'],
  cov_data_long
))[, cluster_type := ifelse(cluster == 'All', 'All', 'Specific')]


## Create histogram --------------------------------------------------------------------->

hist_plot <- ggplot2::ggplot(data = cov_data_long_all, aes(x = value, fill = cluster)) +
  ggplot2::facet_grid('cluster ~ var_label', scales = 'free') +
  ggplot2::geom_histogram(bins = 15, alpha = 0.8) +
  ggplot2::geom_vline(
    data = overall_means, aes(xintercept = mean_val),
    linetype = 'dashed', color = 'black'
  ) +
  ggplot2::geom_vline(
    data = cluster_means, aes(xintercept = mean_val, color = cluster), linetype = 'dashed'
  ) +
  ggplot2::labs(x = 'Feature', y = 'Number of facility catchments in bin') +
  ggplot2::scale_fill_manual(
    values = cluster_colors,
    aesthetics = c('fill', 'color'),
    guide = 'none'
  ) +
  ggplot2::scale_y_continuous(labels = scales::comma_format(accuracy = 1)) +
  ggplot2::scale_x_continuous(labels = scales::comma_format(accuracy = 1)) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 60, hjust = 1)
  )

ggplot2::ggsave(
  filename = file.path(viz_dir, 'covariate_histograms_by_profile.pdf'),
  plot = hist_plot,
  width = uniqueN(cov_data_long_all$variable) + 1,
  height = uniqueN(cov_data_long_all$cluster) + 1
)

## Create maps of all clusters nationwide ----------------------------------------------->

# Subset to viz districts
plot_districts <- unique(catchments_sf_merged$district)
subset_districts <- config$get("subset_districts", fail_if_null = FALSE) |>
  setdiff(NA)
if(length(subset_districts) > 0){
  plot_districts <- intersect(plot_districts, subset_districts)
}
catchments_sf_merged <- catchments_sf_merged |>
  dplyr::filter(district %in% plot_districts)

districts_with_catchments <- districts_sf[districts_sf$area_name %in% plot_districts, ]
catchments_bbox <- sf::st_bbox(districts_with_catchments)

# Create a map of all clusters nationwide
national_map <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = districts_sf, fill = '#AAAAAA', color = '#444444', linewidth = 0.25
  ) +
  ggplot2::geom_sf(
    data = districts_with_catchments, fill = '#ffffff', color = NA, linewidth = 0.5
  ) +
  ggplot2::geom_sf(data = catchments_sf_merged, aes(fill = cluster), linewidth = 0.05) +
  ggplot2::geom_sf(
    data = districts_with_catchments, fill = NA, color = '#222222',
    linewidth = 0.5
  ) +
  ggplot2::scale_fill_manual(values = cluster_colors) +
  ggplot2::theme_bw() +
  ggplot2::labs(
    fill = 'Profile',
    title = 'Community profiles'
  ) +
  ggplot2::coord_sf(
    xlim = c(catchments_bbox['xmin'], catchments_bbox['xmax']),
    ylim = c(catchments_bbox['ymin'], catchments_bbox['ymax'])
  ) +
  ggplot2::theme(
    axis.text = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    panel.background = ggplot2::element_rect(color = NA, fill = '#eeeeee')
  )

ggplot2::ggsave(
  filename = file.path(viz_dir, 'profiles_map_national.png'),
  plot = national_map,
  width = 7,
  height = 7,
  units = 'in',
  dpi = 300
)

## Create zoomed in maps for each district ---------------------------------------------->

for(d_name in plot_districts){
  d_zoom <- districts_with_catchments[districts_with_catchments$area_name == d_name, ] |>
    sf::st_bbox()
  district_map <- suppressMessages(
    national_map +
    ggplot2::coord_sf(
      xlim = c(d_zoom['xmin'], d_zoom['xmax']),
      ylim = c(d_zoom['ymin'], d_zoom['ymax'])
    )
  )
  ggplot2::ggsave(
    filename = glue::glue("{viz_dir}/profiles_map_{d_name}.png"),
    plot = district_map,
    width = 5,
    height = 5,
    dpi = 300
  )
}


## Create district-level maps ----------------------------------------------------------->

district_plot_dir <- file.path(viz_dir, 'district_maps_by_profile')
dir.create(district_plot_dir, showWarnings = FALSE, recursive = TRUE)
catchments_sf_merged$cluster_name <- catchments_sf_merged$cluster

for(this_district in plot_districts){
  msg <- glue::glue("Creating district-level maps for {this_district}")
  message(msg)
  tictoc::tic(msg)
  # Get the subset of facilities to be plotted in this district
  facilities_to_plot <- facility_metadata |>
    _[(district == this_district) & (catchment_id %in% catchments_sf_merged$catchment_id), ] |>
    _[ , facility_label := paste0(
      mwi.hiv.factors::letters_ref()[.I], '. ',
      mwi.hiv.factors::short_label(facility_name)
    )]
  district_plot(
    district_name = this_district,
    districts_sf = districts_sf,
    catchments_sf = catchments_sf_merged,
    catchment_colors = cluster_colors,
    facility_data = facilities_to_plot,
    out_fp = glue::glue("{district_plot_dir}/{this_district}.png")
  )
  tictoc::toc()
}
