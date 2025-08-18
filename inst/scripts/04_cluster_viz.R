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
PCA_ONLY <- FALSE

## SETUP -------------------------------------------------------------------------------->

load_pkgs <- c('data.table', 'ggplot2', 'glue', 'versioning')
lapply(load_pkgs, library, character.only = TRUE) |> invisible()
# Load config
config <- versioning::Config$new(file.path(REPO_DIR, 'config.yaml'))


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

plot_colors <- c('All' = 'red', 'Specific' = 'blue')

hist_plot <- ggplot2::ggplot(data = facility_data_all, aes(x = value, fill = plot_color_group)) +
  ggplot2::facet_grid('cluster_name ~ covariate', scales = 'free') +
  ggplot2::geom_histogram(bins = 15, alpha = 0.6) +
  ggplot2::geom_vline(
    data = overall_means, aes(xintercept = mean_val),
    linetype = 'dashed', color = plot_colors['All']
  ) +
  ggplot2::geom_vline(
    data = cluster_means, aes(xintercept = mean_val),
    linetype = 'dashed', color = plot_colors['Specific']
  ) +
  ggplot2::labs(x = 'Feature', y = 'Number of facility catchments in bin') +
  ggplot2::scale_fill_manual(values = plot_colors, guide = 'none') +
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