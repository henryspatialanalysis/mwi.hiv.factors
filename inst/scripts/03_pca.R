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
  'versioning', 'mbg'
)
lapply(load_pkgs, library, character.only = TRUE) |> invisible()

# Load config
# config <- versioning::Config$new(file.path(REPO_DIR, 'config.yaml'))
config <- versioning::Config$new(file.path(REPO_DIR, 'config_metro.yaml'))

# Create analysis directory
config$get_dir_path('analysis') |> dir.create()

# Load input data
facility_data <- config$read("prepared_data", "covariates_by_facility")
gvh_data <- config$read("prepared_data", "covariates_by_gvh")

# Keep catchments with highest viraemia
top_catchments_cutoff <- config$get("top_catchments_cutoff")
top_facilities <- facility_data[viraemia15to49_mean >= top_catchments_cutoff, ]
if(config$get("pca_level") == "facility"){
  top_catchments <- data.table::copy(top_facilities)
  catchments_sf <- config$read('catchments', 'facility_catchments')
} else {
  top_catchments <- gvh_data[closest_facility_id %in% top_facilities$closest_facility_id, ]
  catchments_sf <- config$read('catchments', 'gvh_catchments')
}
# Load catchment and district polygons (will be used for maps later)
admin_bounds <- config$read("catchments", "admin_bounds")
districts_sf <- admin_bounds |>
  dplyr::filter(area_level == 3L)
facility_metadata <- config$read('catchments', 'facility_metadata')


## DATA PREPARATION --------------------------------------------------------------------->

# Add region (north, central, south) onto district and catchment data
region_merge_inputs <- admin_bounds |>
  dplyr::filter(area_level %in% 1:3) |>
  sf::st_drop_geometry() |>
  data.table::as.data.table()
district_to_region <- (
  region_merge_inputs
  [area_level == 3L, .(district = area_name, area_id = parent_area_id)]
  [region_merge_inputs[area_level == 2L, ], area_id := i.parent_area_id, on = 'area_id']
  [region_merge_inputs[area_level == 1L, ], region := i.area_name, on = 'area_id']
  [, .(district, region)]
)
top_catchments[district_to_region, region := i.region, on = 'district']
districts_sf <- merge(
  x = districts_sf, y = district_to_region, by.x = 'area_name', by.y = 'district'
)
# Add southern/non-southern classification
districts_sf$southern <- ifelse(
  districts_sf$region == 'Southern',
  'Southern',
  'Non-southern'
)

# Add facility location to catchment data
(top_catchments
  [, facility_id := closest_facility_id]
  [facility_metadata, restype := i.restype, on = 'facility_id']
  [ is.na(in_municipality), in_municipality := 0L ]
  [, setting := ifelse(in_municipality == 1L, 'Municipal', 'Non-municipal') ]
  [, southern := ifelse(region == 'Southern', 'Southern', 'Non-southern') ]
)


## Optionally split data into sub-groups ------------------------------------------------>

split_cols <- config$get('split_pca_by', fail_if_null = FALSE)
if(length(split_cols) > 0L){
  split_meta <- top_catchments[, split_cols, with = FALSE] |> unique()
  split_meta[, group_label := do.call(paste, .SD), .SDcols = split_cols]
  top_catchments[split_meta, group_label := i.group_label, on = split_cols]
  # Create list of groups to iterate through
  pca_list <- list()
  for(group_idx in seq_len(nrow(split_meta))){
    split_meta_sub <- split_meta[group_idx, ] |> as.list()
    this_group_label <- split_meta_sub$group_label
    districts_sub <- districts_sf
    district_subset_cols <- intersect(names(split_meta_sub), names(districts_sub))
    for(d_s_col in district_subset_cols){
      districts_sub <- districts_sub[districts_sub[[d_s_col]] == split_meta_sub[[d_s_col]], ]
    }
    catchments_sub <- top_catchments[group_label == this_group_label, ]
    if(nrow(catchments_sub) > 0){
      pca_list[[this_group_label]] <- list(
        districts = districts_sub,
        catchments = catchments_sub
      )
    }
  }
} else {
  top_catchments[, group_label := "All" ]
  pca_list <- list(
    All = list(
      districts = districts_sf,
      catchments = top_catchments
    )
  )
}

# Save information about the number of catchments by PCA group
n_catchments_by_group <- top_catchments[, .(n_catchments = uniqueN(closest_facility_id)), by = group_label]
config$write(n_catchments_by_group, 'analysis', 'pca_groups')


## RUN PCA ------------------------------------------------------------------------------>

pca_groups <- names(pca_list)

for(pca_group in pca_groups){
  dir.create(file.path(config$get_dir_path('analysis'), pca_group))
  msg <- glue::glue("Running PCA for {pca_group}")
  message(msg)
  tictoc::tic(msg)

  cov_names <- names(config$get("pca_covariates"))
  catchments_sub <- pca_list[[pca_group]]$catchments
  pca_data <- catchments_sub[, cov_names, with = FALSE] |>
    scale() |>
    data.table::as.data.table()
  for(cov_name in cov_names){
    if(all(is.na(pca_data[[cov_name]]))){
      message("Dropping ", cov_name, " - no variation across catchments.")
      pca_data[[cov_name]] <- NULL
    }
    if(any(is.na(pca_data[[cov_name]]))){
      mean_val <- mean(pca_data[[cov_name]], na.rm = TRUE)
      pca_data[ is.na(get(cov_name)), (cov_name) := mean_val ]
    }
  }
  pca_data <- pca_data |>
    data.table::setnames(
      old = cov_names,
      new = config$get('pca_covariates') |>
        unlist() |>
        gsub(pattern = ' ', replacement = '\n'),
      skip_absent = TRUE
    )
  # If there are more features than catchments, remove the most collinear features based
  #  on VIF
  vif_cutoff <- 20
  while((ncol(pca_data) > nrow(pca_data)) & (vif_cutoff > 1)){
    pca_dummy <- data.table::copy(pca_data)
    colnames(pca_dummy) <- paste0('V', seq_len(ncol(pca_dummy)))
    vif_results <- mbg::vif_covariate_select(pca_dummy, vif_cutoff = vif_cutoff)
    keep_cols <- vif_results[keep == TRUE, covariate ] |>
      gsub(pattern = "^V", replacement = "") |>
      as.numeric()
    pca_data <- pca_data[, keep_cols, with = FALSE]
    vif_cutoff <- vif_cutoff / 2
  }
  if(ncol(pca_data) > nrow(pca_data)) stop('Too many features for PCA')
  pca_model <- stats::princomp(pca_data)

  # Save PCA loadings table
  pca_loadings <- pca_model$loadings |>
    as.numeric() |>
    matrix(nrow = ncol(pca_data))
  colnames(pca_loadings) <- paste0('component_', seq_len(ncol(pca_loadings)))
  pca_loadings <- cbind(
    data.frame(feature = colnames(pca_data) |> gsub(pattern = '\\n', replacement = ' ')),
    pca_loadings
  )
  config$get_file_path('analysis', 'pca_loadings_table') |>
    glue::glue() |>
    versioning::autowrite(x = pca_loadings)

  # PCA visualizations
  # Scree plot
  config$get_file_path('analysis', 'pca_importance') |>
    glue::glue() |>
    png(height = 4, width = 6, units = 'in', res = 300)
  factoextra::fviz_eig(
    X = pca_model,
    main = glue::glue('Variation explained by each principal component ({pca_group})'),
    xlab = 'Principal component',
    ylab = 'Variance explained (%)'
  ) |> print()
  dev.off()
  # Loadings plot: first and second components
  var_explained <- (pca_model$sdev^2 / sum(pca_model$sdev^2))
  var_labels <- var_explained |>
    scales::percent(accuracy = 1.1)
  n_components <- length(var_explained)
  if(n_components >= 2){
    config$get_file_path('analysis', 'pca_loadings') |>
      glue::glue() |>
      png(height = 8, width = 8, units = 'in', res = 300)
    factoextra::fviz_pca_var(
      X = pca_model,
      axes = c(1, 2),
      col.var = 'cos2',
      gradient.cols = RColorBrewer::brewer.pal(n = 9, name = 'YlGnBu')[4:9],
      repel = TRUE,
      title = glue::glue('PCA loadings: first two components ({pca_group})'),
      xlab = glue::glue('Component 1 (explains {var_labels[1]} of variation)'),
      ylab = glue::glue('Component 2 (explains {var_labels[2]} of variation)'),
      legend.title = 'Cosine\nsimilarity'
    ) |> print()
    dev.off()
  }
  # Loadings plot: third and fourth components
  if(n_components >= 4){
    config$get_file_path('analysis', 'pca_loadings_pt2') |>
      glue::glue() |>
      png(height = 8, width = 8, units = 'in', res = 300)
    factoextra::fviz_pca_var(
      X = pca_model,
      axes = c(3, 4),
      col.var = 'cos2',
      gradient.cols = RColorBrewer::brewer.pal(n = 9, name = 'YlGnBu')[4:9],
      repel = TRUE,
      title = glue::glue('PCA loadings: third and fourth components ({pca_group})'),
      xlab = glue::glue('Component 3 (explains {var_labels[3]} of variation)'),
      ylab = glue::glue('Component 4 (explains {var_labels[4]} of variation)'),
      legend.title = 'Cosine\nsimilarity'
    ) |> print()
    dev.off()
  }

  # Run k-means clustering on top catchments
  if(config$get('use_pca_for_clustering')){
    # Use the top principal components as features
    viz_components <- min(n_components, 4)
    kmeans_data <- scale(pca_model$scores[, seq_len(viz_components)])
  } else {
    # Use original features (these have already been rescaled to N(0, 1))
    viz_components <- ncol(pca_data)
    kmeans_data <- pca_data
  }
  max_k <- min(10, nrow(kmeans_data) - 1)
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
    glue::glue() |>
    png(height = 5, width = 7, units = 'in', res = 300)
  wss_table <- ggplot2::ggplot(sum_squares_table, ggplot2::aes(x = k, y = sum_squares, color = type)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_vline(xintercept = 5, linetype = 2) +
    ggplot2::geom_vline(xintercept = c(3, 8), linetype = 3) +
    ggplot2::scale_y_continuous(limits = c(0, max(sum_squares_table$sum_squares))) +
    ggplot2::scale_x_continuous(breaks = seq_len(max_k)) +
    ggplot2::labs(
      x = 'Number of clusters',
      y = 'Sum of squares',
      color = 'Variation',
      title = glue::glue('K-means clustering: variation explained by clusters ({pca_group})')
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank())
  print(wss_table)
  dev.off()

  # For particular values of k, create scatters and maps of results
  plot_k <- seq(2, max_k)
  cluster_summaries_list <- list()
  for(k in plot_k){
    model_results <- kmeans_models[[k]]
    # Create scatter plots of top four components
    plot_data <- pca_model$scores[, seq_len(viz_components)] |> as.data.table()
    plot_data$cluster <- paste("Cluster", model_results$cluster) |>
      factor(levels = paste("Cluster", seq_len(k)))
    scatter_fig <- GGally::ggpairs(
      data = plot_data,
      upper = list(continuous = 'blank'),
      columns = seq_len(viz_components),
      mapping = ggplot2::aes(color = cluster),
      columnLabels = paste0('PC', seq_len(viz_components))
    ) +
      ggplot2::ggtitle(glue::glue("K-means clustering details: {k} clusters ({pca_group})")) +
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
    catchments_with_clusters <- copy(catchments_sub)[, cluster := plot_data$cluster ]
    merge_id_field <- if(config$get("pca_level") == "facility") "closest_facility_id" else "gvh_id"
    catchments_sf_sub <- merge(
      x = catchments_sf,
      y = catchments_with_clusters[, c(merge_id_field, 'cluster'), with = FALSE],
      by = merge_id_field
    )
    map_fig <- ggplot2::ggplot() +
      ggplot2::geom_sf(
        data = catchments_sf_sub, mapping = ggplot2::aes(fill = cluster),
        color = '#444444', linewidth = 0.05
      ) +
      ggplot2::geom_sf(
        data = pca_list[[pca_group]]$districts,
        fill = NA, color = 'black', linewidth = 0.5
      ) +
      ggplot2::theme_minimal() +
      ggplot2::ggtitle(glue::glue("{pca_group}: {k} clusters")) +
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

    ## Summarize features in each cluster and compare to group overall
    cic <- data.table::copy(catchments_with_clusters[, c(cov_names, 'cluster'), with = FALSE])
    cluster_summaries_list[[paste0(k, '_cluster')]] <- list(
      cic[, lapply(.SD, mean, na.rm = T), by = cluster ][, summ := 'mean'],
      cic[, lapply(.SD, median, na.rm = T), by = cluster ][, summ := 'median'],
      cic[, lapply(.SD, sd, na.rm = T), by = cluster ][, summ := 'sd'],
      cic[, lapply(.SD, quantile, probs = 0.1, na.rm = T), by = cluster ][, summ := 'q10'],
      cic[, lapply(.SD, quantile, probs = 0.9, na.rm = T), by = cluster ][, summ := 'q90']
    ) |>
      data.table::rbindlist() |>
      data.table::melt(id.vars = c('cluster', 'summ')) |>
      data.table::dcast('variable + cluster ~ summ', value.var = 'value') |>
      _[, n_clusters := k ]
  }
  full_cluster_data <- data.table::rbindlist(cluster_summaries_list)
  for_summ <- catchments_sub[, cov_names, with = FALSE]
  group_baseline <- list(
      for_summ[, lapply(.SD, mean, na.rm = T)][, summ := 'mean'],
      for_summ[, lapply(.SD, median, na.rm = T)][, summ := 'median'],
      for_summ[, lapply(.SD, sd, na.rm = T)][, summ := 'sd'],
      for_summ[, lapply(.SD, quantile, probs = 0.1, na.rm = T)][, summ := 'q10'],
      for_summ[, lapply(.SD, quantile, probs = 0.9, na.rm = T)][, summ := 'q90']
  ) |>
    data.table::rbindlist() |>
    data.table::melt(id.vars = c('summ')) |>
    data.table::dcast(variable ~ summ, value.var = 'value')
  comparison_table_by_cluster <- merge(
    x = full_cluster_data,
    y = group_baseline,
    by = 'variable',
    suffixes = c('_cluster', '_baseline'),
    all = TRUE
  )

  ## Save results ------------------------------------------------------------------------->

  config$get_file_path('analysis', 'pca_kmeans_summaries') |>
    glue::glue() |>
    versioning::autowrite(x = comparison_table_by_cluster)

  pca_loadings <- pca_model$scores |> as.data.table()
  colnames(pca_loadings) <- paste0('pc', seq_len(ncol(pca_loadings)))

  full_data <- cbind(catchments_sub, pca_loadings)
  for(k in seq_len(max_k)){
    full_data[[paste0('kmeans_', k)]] <- kmeans_models[[k]]$cluster
  }

  config$get_file_path('analysis', 'pca_kmeans_results') |>
    glue::glue() |>
    versioning::autowrite(x = full_data)

  # End timer for this PCA group
  tictoc::toc()
}
