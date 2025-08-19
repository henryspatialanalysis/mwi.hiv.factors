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
PCA_ONLY <- TRUE


## SETUP -------------------------------------------------------------------------------->

load_pkgs <- c('data.table', 'ggplot2', 'glue', 'versioning')
lapply(load_pkgs, library, character.only = TRUE) |> invisible()
# Load config
config <- versioning::Config$new(file.path(REPO_DIR, 'config.yaml'))

# Load colors by cluster
cluster_colors_table <- config$read('analysis', 'cluster_colors')
plot_colors <- cluster_colors_table$color
names(plot_colors) <- cluster_colors_table$cluster_name

# Load spatial data for map
catchments_sf <- config$read('catchments', 'facility_catchments')
catchments_sf$catchment_id <- seq_len(nrow(catchments_sf))
admin_bounds <- config$read("catchments", "admin_bounds", quiet = TRUE)
districts_sf <- admin_bounds |> dplyr::filter(area_level == 3L)
regions_sf <- admin_bounds |> dplyr::filter(area_level == 1L)

## PREPARE DATA ------------------------------------------------------------------------->

# Load facility data, combined across all regional analyses
facility_data <- config$read("analysis", "pca_results_combined")

# Create grouping labels
facility_data[, cluster_name := paste0(region_label, ', C', kmeans_best)]

# Subset to group and features
cov_sublist <- if(PCA_ONLY) 'pca_covariates' else 'covariates'
cov_cols <- intersect(names(config$get(cov_sublist)), colnames(facility_data))
cov_names <- config$get(cov_sublist)[cov_cols] |>
  gsub(pattern = ' ', replacement = '\n')
setnames(facility_data, cov_cols, cov_names)
facility_data_melted <- facility_data[, c('cluster_name', cov_names), with = FALSE] |>
  melt(id.vars = 'cluster_name', variable.name = 'covariate', value.name = 'value')

# Get overall and cluster level means, to be plotted as vertical lines
overall_means <- facility_data_melted[
  , .(mean_val = mean(value, na.rm = TRUE)), by = covariate
]
cluster_means <- facility_data_melted[
  , .(mean_val = mean(value, na.rm = TRUE)), by = .(cluster_name, covariate)
]

# Add a combined group
facility_data_all <- data.table::rbindlist(list(
  data.table::copy(facility_data_melted)[, cluster_name := 'All'],
  facility_data_melted
))[, plot_color_group := ifelse(cluster_name == 'All', 'All', 'Specific')]


## Create histogram --------------------------------------------------------------------->

hist_plot <- ggplot2::ggplot(data = facility_data_all, aes(x = value, fill = cluster_name)) +
  ggplot2::facet_grid('cluster_name ~ covariate', scales = 'free') +
  ggplot2::geom_histogram(bins = 15, alpha = 0.8) +
  ggplot2::geom_vline(
    data = overall_means, aes(xintercept = mean_val),
    linetype = 'dashed', color = 'black'
  ) +
  ggplot2::geom_vline(
    data = cluster_means, aes(xintercept = mean_val, color = cluster_name), linetype = 'dashed'
  ) +
  ggplot2::labs(x = 'Feature', y = 'Number of facility catchments in bin') +
  ggplot2::scale_fill_manual(
    values = plot_colors,
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
  filename = config$get_file_path('analysis', 'pca_histograms'),
  plot = hist_plot,
  width = uniqueN(facility_data_all$covariate) * 1.5 + 1,
  height = uniqueN(facility_data_all$cluster_name) * 1.5 + 1
)

## Create a map of all clusters nationwide ---------------------------------------------->

catchments_meta <- merge(
  x = catchments_sf,
  y = facility_data[, .(catchment_id, cluster_name)],
  by = c('catchment_id')
)

# Create a map of all clusters nationwide
national_map <-ggplot2::ggplot() +
  ggplot2::geom_sf(data = catchments_meta, aes(fill = cluster_name), linewidth = 0.05) +
  ggplot2::geom_sf(data = districts_sf, fill = NA, color = '#444444', linewidth = 0.25) +
  ggplot2::geom_sf(data = regions_sf, fill = NA, color = 'black', linewidth = 0.5) +
  ggplot2::scale_fill_manual(values = plot_colors) +
  ggplot2::theme_bw() +
  ggplot2::labs(
    fill = 'Cluster',
    title = 'Community archetypes'
  ) +
  ggplot2::theme(
    axis.text = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank()
  )

ggplot2::ggsave(
  filename = config$get_file_path('analysis', 'pca_national_map'),
  plot = national_map,
  width = 6,
  height = 10
)
