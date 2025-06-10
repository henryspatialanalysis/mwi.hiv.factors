## #######################################################################################
##
## 03. RUN PRINCIPAL COMPONENT ANALYSIS
##
## AUTHOR: Nat Henry, nat@henryspatialanalysis.com
## CREATED: 2025-05-27
## PURPOSE: Run test PCA on facility catchments with high viraemia
##
## #######################################################################################

REPO_DIR <- '~/repos/mwi.hiv.factors'

## SETUP -------------------------------------------------------------------------------->

load_pkgs <- c(
  'corrr', 'data.table', 'factoextra', 'FactoMineR', 'ggplot2', 'ggcorrplot', 'GGally',
  'versioning'
)
lapply(load_pkgs, library, character.only = TRUE) |> invisible()

# Load config
config <- versioning::Config$new(file.path(REPO_DIR, 'config.yaml'))

# Create analysis directory
config$get_dir_path('analysis') |> dir.create()

# Load input data
cov_data <- config$read("prepared_data", "covariates_by_catchment")

# Keep catchments with highest viraemia
top_catchments_pct <- config$get("top_catchments_pct")
top_catchments <- (
  cov_data
  [order(-viraemia15to49_mean),]
  [seq_len(round(top_catchments_pct * .N)), ]
)

# Load catchment and district polygons (will be used for maps later)
catchments_sf <- config$read('catchments', 'facility_catchments')
districts_sf <- config$read("catchments", "admin_bounds") |>
  dplyr::filter(area_level == 3L)


## RUN PCA ------------------------------------------------------------------------------>

cov_names <- names(config$get("pca_covariates"))
pca_data <- top_catchments[, cov_names, with = FALSE] |>
  scale() |>
  data.table::as.data.table() |>
  data.table::setnames(
    old = cov_names,
    new = config$get('pca_covariates') |>
      unlist() |>
      gsub(pattern = ' ', replacement = '\n')
  )
pca_model <- stats::princomp(pca_data)


## PCA VISUALIZATIONS ------------------------------------------------------------------->

# Scree plot
config$get_file_path('analysis', 'pca_importance') |>
  png(height = 4, width = 6, units = 'in', res = 300)
factoextra::fviz_eig(
  X = pca_model,
  main = 'Variation explained by each principal component',
  xlab = 'Principal component',
  ylab = 'Variance explained (%)'
)
dev.off()

# Loadings plot
var_explained <- (pca_model$sdev^2 / sum(pca_model$sdev^2))
var_labels <- var_explained |>
  scales::percent(accuracy = 1.1)

config$get_file_path('analysis', 'pca_loadings') |>
  png(height = 8, width = 8, units = 'in', res = 300)
factoextra::fviz_pca_var(
  X = pca_model,
  axes = c(1, 2),
  col.var = 'cos2',
  gradient.cols = RColorBrewer::brewer.pal(n = 9, name = 'YlGnBu')[4:9],
  repel = TRUE,
  title = 'PCA loadings: first two components',
  xlab = glue::glue('Component 1 ("Urbanicity", explains {var_labels[1]} of variation)'),
  ylab = glue::glue('Component 2 ("Environment", explains {var_labels[2]} of variation)'),
  legend.title = 'Cosine\nsimilarity'
)
dev.off()

config$get_file_path('analysis', 'pca_loadings_pt2') |>
  png(height = 8, width = 8, units = 'in', res = 300)
factoextra::fviz_pca_var(
  X = pca_model,
  axes = c(3, 4),
  col.var = 'cos2',
  gradient.cols = RColorBrewer::brewer.pal(n = 9, name = 'YlGnBu')[4:9],
  repel = TRUE,
  title = 'PCA loadings: third and fourth components',
  xlab = glue::glue('Component 3 ("Climate", explains {var_labels[3]} of variation)'),
  ylab = glue::glue('Component 4 ("Travel times", explains {var_labels[4]} of variation)'),
  legend.title = 'Cosine\nsimilarity'
)
dev.off()


## Run k-means clustering on top catchments using the principle components as features -->

kmeans_data <- scale(pca_model$scores[, 1:4])
max_k <- 10

kmeans_models <- lapply(seq_len(max_k), function(k){
  kmeans(kmeans_data, centers = k)
})
sum_squares_table <- data.table::data.table(
  k = rep(seq_len(max_k), times = 2),
  sum_squares = c(
    sapply(kmeans_models, function(x) x$tot.withinss),
    sapply(kmeans_models, function(x) x$betweenss)
  ),
  type = rep(c('Within-cluster', 'Between-cluster'), each = max_k)
)

# Show sources of variation: within-cluster vs. between-cluster

config$get_file_path('analysis', 'kmeans_wss') |>
  png(height = 5, width = 7, units = 'in', res = 300)
ggplot2::ggplot(sum_squares_table, ggplot2::aes(x = k, y = sum_squares, color = type)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::geom_vline(xintercept = 5, linetype = 2) +
  ggplot2::geom_vline(xintercept = c(3, 8), linetype = 3) +
  ggplot2::scale_y_continuous(limits = c(0, max(wss_table$wss))) +
  ggplot2::scale_x_continuous(breaks = seq_len(max_k)) +
  ggplot2::labs(
    x = 'Number of clusters',
    y = 'Sum of squares',
    color = 'Variation',
    title = 'K-means clustering: variation explained by clusters'
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank())
dev.off()

# For particular values of k, create scatters and maps of results
for(k in c(3, 4, 5, 8)){
  model_results <- kmeans_models[[k]]
  # Create scatter plots of top four components
  plot_data <- pca_model$scores[, 1:4] |> as.data.table()
  plot_data$cluster <- paste("Cluster", model_results$cluster) |>
    factor(levels = paste("Cluster", seq_len(k)))
  scatter_fig <- GGally::ggpairs(
    data = plot_data,
    upper = list(continuous = 'blank'),
    columns = 1:4,
    mapping = ggplot2::aes(color = cluster),
    columnLabels = c(
      'PC1: Urbanicity', 'PC2: Environment', 'PC3: Climate', 'PC4: Travel times'
    )
  ) +
    ggplot2::ggtitle(glue::glue("K-means clustering details: {k} clusters")) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank()
    )
  config$get_file_path('analysis', 'kmeans_detail_template') |>
    glue::glue() |>
    png(height = 8, width = 8, units = 'in', res = 300)
  print(scatter_fig)
  dev.off()
  # Create map of high-viraemia clusters
  catchments_with_clusters <- copy(top_catchments)[, cluster := plot_data$cluster ]
  catchments_sf_sub <- merge(
    x = catchments_sf,
    y = catchments_with_clusters[, .(closest_facility_id, cluster)],
    by = 'closest_facility_id'
  )
  map_fig <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = catchments_sf_sub, mapping = ggplot2::aes(fill = cluster),
      color = '#444444', linewidth = 0.05
    ) +
    ggplot2::geom_sf(data = districts_sf, fill = NA, color = 'black', linewidth = 0.5) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank()
    ) +
    ggplot2::labs(fill = 'Cluster')
  config$get_file_path('analysis', 'kmeans_map_template') |>
    glue::glue() |>
    png(height = 8, width = 6, units = 'in', res = 300)
  print(map_fig)
  dev.off()
}