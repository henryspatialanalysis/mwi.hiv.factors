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

# Load colors by cluster
cluster_colors_table <- config$read('analysis', 'cluster_colors')
plot_colors <- cluster_colors_table$color
names(plot_colors) <- cluster_colors_table$cluster_name

# Load spatial data for map
catchments_sf <- config$read('catchments', 'facility_catchments')
catchments_sf$catchment_id <- seq_len(nrow(catchments_sf))
gvh_sf <- config$read('catchments', 'gvh_catchments')
admin_bounds <- config$read("catchments", "admin_bounds", quiet = TRUE)
districts_sf <- admin_bounds |> dplyr::filter(area_level == 3L)
regions_sf <- admin_bounds |> dplyr::filter(area_level == 1L)

# Load facility data, combined across all regional analyses
facility_data <- config$read("analysis", "pca_results_combined")

# Load GVH data
gvh_covs <- config$read('prepared_data', 'covariates_by_gvh')


## PREPARE DATA ------------------------------------------------------------------------->

# Create grouping labels
facility_data[, cluster_name := paste0(region_label, ', C', kmeans_best)]

# Merge cluster metadata onto GVH table
gvh_covs[facility_data, on = 'closest_facility_id', cluster_name := i.cluster_name ]
gvh_full <- merge(
  x = gvh_sf[, c('gvh_id')],
  y = gvh_covs[!is.na(cluster_name)],
  by = 'gvh_id'
)

# Subset to group and features
cov_sublist <- if(PCA_ONLY) 'pca_covariates' else 'covariates'
cov_cols <- intersect(names(config$get(cov_sublist)), colnames(facility_data))
cov_names <- config$get(cov_sublist)[cov_cols]
cov_names_long <- cov_names |>
  gsub(pattern = ' ', replacement = '\n')
setnames(facility_data, cov_cols, cov_names_long)
facility_data_melted <- facility_data[, c('cluster_name', cov_names_long), with = FALSE] |>
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


## Create trendlines of GVH characteristics *by cluster* -------------------------------->

coord_bbox <- function(bbox){
  gutter <- max(bbox$xmax - bbox$xmin, bbox$ymax - bbox$ymin) * 0.025
  coords <- ggplot2::coord_sf(
    xlim = c(bbox$xmin - gutter, bbox$xmax + gutter),
    ylim = c(bbox$ymin - gutter, bbox$ymax + gutter)
  )
  return(coords)
}

gg_scatter <- function(df, x_name, y_name, x_lab, y_lab, x_range, y_range, color_range){
  linear_mod <- lm(get(y_name) ~ get(x_name), data = df)
  mod_slope <- coef(linear_mod)[2]
  mod_intercept <- coef(linear_mod)[1]
  int_n_digits <- max(-1 * round(log10(abs(mod_intercept))) + 2, 1)
  slope_n_digits <- max(-1 * round(log10(abs(mod_slope))) + 2, 1)
  mod_r2 <- summary(linear_mod)$r.squared
  fig <- ggplot2::ggplot(data = df, aes(x = get(x_name), y = get(y_name))) +
    ggplot2::geom_smooth(
      method = 'lm', formula = y ~ x, se = TRUE, color = 'blue', linewidth = 0.5
    ) +
    ggplot2::geom_point(aes(fill = get(x_name)), alpha = 0.75, pch = 21) +
    ggplot2::scale_x_continuous(limits = x_range, labels = scales::comma) +
    ggplot2::scale_y_continuous(limits = y_range, labels = scales::percent) +
    ggplot2::scale_fill_gradientn(
      colors = viridisLite::viridis(n = 100),
      limits = color_range,
      na.value = '#888888',
      guide = 'none'
    ) +
    ggplot2::labs(
      x = x_lab,
      y = y_lab,
      title = glue::glue(
        "Y = {round(mod_intercept, int_n_digits)} + ",
        "{round(mod_slope, slope_n_digits)} * X"
      ),
      subtitle = glue::glue("R^2 = {round(mod_r2, 3)}")
    ) +
    ggplot2::theme_bw()
  return(fig)
}

all_clusters <- sort(unique(gvh_full$cluster_name))
prev_range <- c(0, max(gvh_full$prev15to49_mean, na.rm = TRUE))
vls_range <- range(gvh_full$vls15to49_mean, na.rm = TRUE)
viraemia_range <- range(gvh_full$viraemia15to49_mean, na.rm = TRUE)

gvh_viz_dir <- file.path(config$get_dir_path('analysis'), 'gvh_covariate_viz')
dir.create(gvh_viz_dir, showWarnings = FALSE, recursive = TRUE)

# Iterate through covariates
for(cov_i in seq_along(cov_cols)){
  cov_col <- cov_cols[cov_i]
  cov_name <- cov_names[cov_i]
  cov_name_long <- cov_names_long[cov_i]
  cov_range <- range(gvh_full[[cov_col]], na.rm = TRUE)

  # Iterate through clusters
  for(cluster_i in all_clusters){
    gvh_to_plot <- gvh_full |>
      dplyr::filter(cluster_name == cluster_i)
    color_range <- range(gvh_to_plot[[cov_col]], na.rm = TRUE)
    gvh_to_plot$this_cov <- gvh_to_plot[[cov_col]]
    facil_to_plot <- catchments_meta |> dplyr::filter(cluster_name == cluster_i)
    bbox <- sf::st_bbox(gvh_to_plot)
    # Figure 1a: inset map
    inset_map <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = districts_sf, fill = "#cccccc", color = '#222222', linewidth = 0.2) +
      ggplot2::geom_sf(data = sf::st_as_sfc(bbox), fill = NA, color = 'red', linewidth = 1) +
      ggplot2::theme_void() +
      ggplot2::theme(
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        panel.background = element_rect(color = NA, fill = "white", linewidth = 1)
      )
    # Figure 1b: choropleth
    map_fig <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = districts_sf, fill = '#cccccc', color = '#222222', linewidth = 0.2) +
      ggplot2::geom_sf(data = gvh_to_plot, aes(fill = this_cov), color = '#444444', linewidth = 0.05) +
      ggplot2::geom_sf(data = regions_sf, fill = NA, color = 'black', linewidth = 0.5) +
      coord_bbox(bbox) +
      ggplot2::scale_fill_gradientn(
        colors = viridisLite::viridis(n = 100),
        limits = color_range,
        labels = scales::comma,
        na.value = '#888888'
      ) +
      ggplot2::labs(fill = cov_name_long, y = '') +
      ggplot2::theme_bw() +
      ggplot2::theme(
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank()
      )
    if(nrow(gvh_to_plot) > 1){
      # Figure 2: scatterplot vs prevalence
      prev_scatter <- gg_scatter(
        df = gvh_to_plot,
        x_name = cov_col, y_name = "prev15to49_mean",
        x_lab = cov_name, y_lab = "Prevalence",
        x_range = cov_range, y_range = prev_range, color_range = color_range
      )
      # Figure 3: scatterplot vs VLS
      vls_scatter <- gg_scatter(
        df = gvh_to_plot,
        x_name = cov_col, y_name = "vls15to49_mean",
        x_lab = cov_name, y_lab = "Viral Load Suppression",
        x_range = cov_range, y_range = vls_range, color_range = color_range
      )
      # Figure 4: scatterplot vs viraemia
      vir_scatter <- gg_scatter(
        df = gvh_to_plot,
        x_name = cov_col, y_name = "viraemia15to49_mean",
        x_lab = cov_name, y_lab = "Viraemia",
        x_range = cov_range, y_range = viraemia_range, color_range = color_range
      )
    } else {
      vir_scatter <- vls_scatter <- prev_scatter <- ggplot2::ggplot() + ggplot2::theme_void()
    }
    # Assemble onto a single page with inset map
    # Create the main plot arrangement
    main_plot <- gridExtra::arrangeGrob(
      grobs = list(
        ggplot2::ggplotGrob(map_fig),
        ggplot2::ggplotGrob(prev_scatter),
        ggplot2::ggplotGrob(vls_scatter),
        ggplot2::ggplotGrob(vir_scatter)
      ),
      layout_matrix = matrix(c(1, 1, 1, 1, 1, 1, 2, 3, 4), ncol = 3)
    )

    # Create the final plot with inset map
    final_plot <- ggplot2::ggplot() +
      ggplot2::annotation_custom(
        grob = main_plot,
        xmin = 0.025, xmax = 1, ymin = 0, ymax = 1
      ) +
      ggplot2::annotation_custom(
        grob = ggplot2::ggplotGrob(inset_map),
        xmin = 0, xmax = 0.08, ymin = 0.75, ymax = 1
      ) +
      ggplot2::ggtitle(glue::glue("{cluster_i}: {cov_name} by GVH")) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, size = 18),
        plot.margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0)
      )

    # Display the plot
    png(
      glue::glue('{gvh_viz_dir}/{cov_name} - {cluster_i}.png'),
      height = 8, width = 12, units = 'in', res = 150
    )
    grid::grid.draw(final_plot)
    dev.off()
  }
}

# Save the PDF and close
dev.off()