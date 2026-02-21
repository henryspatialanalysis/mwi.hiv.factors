## #######################################################################################
##
## 02. VISUALIZE COVARIATES
##
## AUTHOR: Nat Henry, nat@henryspatialanalysis.com
## CREATED: May 26, 2026
## PURPOSE: Visualize covariates using three plots:
##
## #######################################################################################

REPO_DIR <- '~/repos/mwi.hiv.factors'

## SETUP -------------------------------------------------------------------------------->

# Load packages
load_pkgs <- c(
  'data.table', 'ggplot2', 'grid', 'gridExtra', 'sf', 'versioning', 'viridis'
)
lapply(load_pkgs, library, character.only = TRUE) |> invisible()
devtools::load_all(REPO_DIR)

# Load config
config <- versioning::Config$new(file.path(REPO_DIR, 'config.yaml'))

# Load input data
cov_data <- config$read("prepared_data", "covariates_by_facility_discretized")
cov_cutoffs <- config$read("prepared_data", "covariates_by_facility_cutoffs")
catchments <- config$read("catchments", "facility_catchments")
admin_bounds <- config$read("catchments", "admin_bounds", quiet = TRUE)
districts <- admin_bounds |> dplyr::filter(area_level == 3L)
regions <- admin_bounds |> dplyr::filter(area_level == 1L)
cutoffs_list <- list(
  quintiles = list(low = 'q1', high = 'q5'),
  jenks = list(low = 'j1', high = 'jtop')
)

# Merge catchments with covariates
catchments_id_field <- config$get("catchments_id_field")
catchments[[catchments_id_field]] <- seq_len(nrow(catchments))
catchments_with_covs <- merge(
  x = catchments,
  y = cov_data,
  by = intersect(colnames(catchments), colnames(cov_data))
)

## Prepare covariate cutoffs ------------------------------------------------------------>

cutoffs_long <- cov_cutoffs |>
  data.table::melt(
    id.vars = 'cov_name', variable.name = 'cutoff_type', value.name = 'x'
  ) |>
  _[cutoff_type %in% c('q1', 'q4', 'j1', 'j3'), ] |>
  _[, label := ifelse(cutoff_type %in% c('q1', 'q4'), 'Quintiles', 'Jenks')] |>
  data.table::setnames(old = 'cov_name', new = 'this_cov')


## Add region (N+C, S) to spatial files ------------------------------------------------->

cov_names <- names(config$get("covariates"))
outcome_colors <- viridis::viridis(n = 100)

regions$region <- regions$area_name
districts$region <- districts |>
  sf::st_point_on_surface() |>
  sf::st_join(y = regions) |>
  _$region |>
  suppressWarnings()
d_to_r <- districts |>
  sf::st_drop_geometry() |>
  dplyr::select(district = area_name, region)
catchments_with_covs <- merge(catchments_with_covs, d_to_r, by = 'district')


# ## Plot each covariate nationwide ----------------------------------------------------->

# pdf(config$get_file_path("prepared_data", "covariates_viz"), height = 8, width = 8)
# for(cov_name in cov_names){
#   message("Preparing plots for ", cov_name)
#   cov_label <- config$get("covariates", cov_name)
#   mwi.hiv.factors::create_three_panel_plot(
#     catchments_with_covs = catchments_with_covs,
#     cov_data = cov_data,
#     district_bounds = districts,
#     cov_field = cov_name,
#     viraemia_field = 'viraemia15to49_mean',
#     pop_field = 'population_1km',
#     plot_title = paste(cov_label, 'by health facility catchment'),
#     cov_label = cov_label,
#     hist_num_breaks = 30,
#     outcome_colors = outcome_colors,
#     col_lims = quantile(cov_data$THIS_COV, probs = c(0.05, 0.95), na.rm = TRUE),
#     vlines = cutoffs_long[this_cov == cov_name, ]
#   )
# }
# dev.off()


## Plot each covariate nationwide, only for top catchments ------------------------------>

top_catchments_cutoff <- config$get("top_catchments_cutoff")
cov_data_top <- cov_data[viraemia15to49_mean >= top_catchments_cutoff, ]
catchments_with_covs_top <- catchments_with_covs[
  catchments_with_covs$viraemia15to49_mean >= top_catchments_cutoff,
]

pdf(config$get_file_path("prepared_data", "covariates_viz_top_catchments"), height = 8, width = 8)
for(cov_name in cov_names){
  message("Preparing plots for ", cov_name)
  cov_label <- config$get("covariates", cov_name)
  mwi.hiv.factors::create_three_panel_plot(
    catchments_with_covs = catchments_with_covs_top,
    cov_data = cov_data_top,
    district_bounds = districts,
    cov_field = cov_name,
    viraemia_field = 'viraemia15to49_mean',
    pop_field = 'population_1km',
    plot_title = paste(cov_label, 'by health facility catchment'),
    cov_label = cov_label,
    hist_num_breaks = 30,
    outcome_colors = outcome_colors,
    col_lims = quantile(cov_data[[cov_name]], probs = c(0.05, 0.95), na.rm = TRUE),
    vlines = cutoffs_long[this_cov == cov_name, ]
  )
}
dev.off()


## Plot each covariate by district ------------------------------------------------------>

plot_districts <- sort(unique(catchments_with_covs$district))
if(!is.null(config$get("subset_districts", fail_if_null = FALSE))){
  plot_districts <- intersect(plot_districts, config$get("subset_districts"))
}

pdf(config$get_file_path("prepared_data", "covariates_viz_by_district"), height = 8, width = 8)
for(this_district in plot_districts){
  message("Running top catchment plots for ", this_district)
  cov_data_sub <- cov_data[
    (district == this_district) & (viraemia15to49_mean >= top_catchments_cutoff),
  ]
  catchments_with_covs_sub <- catchments_with_covs[
    (catchments_with_covs$district == this_district) &
    (catchments_with_covs$viraemia15to49_mean >= top_catchments_cutoff),
  ]
  districts_sub <- districts[districts$area_name == this_district, ]
  if(nrow(cov_data_sub) > 0){
    for(cov_name in cov_names){
      message("  -> Preparing plots for ", cov_name)
      cov_label <- config$get("covariates", cov_name)
      mwi.hiv.factors::create_three_panel_plot(
        catchments_with_covs = catchments_with_covs_sub,
        cov_data = cov_data_sub,
        district_bounds = districts_sub,
        cov_field = cov_name,
        viraemia_field = 'viraemia15to49_mean',
        pop_field = 'population_1km',
        plot_title = paste(cov_label, 'by health facility catchment in', this_district),
        cov_label = cov_label,
        hist_num_breaks = 8,
        outcome_colors = outcome_colors,
        col_lims = quantile(cov_data$THIS_COV, probs = c(0.05, 0.95), na.rm = TRUE),
        vlines = cutoffs_long[this_cov == cov_name, ]
      )
    }
  }
}
dev.off()


## Plot DISCRETIZED by district --------------------------------------------------------->

for(cutoff_type in names(cutoffs_list)){
  pdf(
    config$get_file_path("prepared_data", "covariates_viz_by_district") |>
      gsub(pattern = '.pdf', replacement = paste0('_', cutoff_type, '.pdf'), fixed = TRUE),
    height = 8, width = 8
  )
  for(this_district in plot_districts){
    message("Running plots for ", this_district)
    cov_data_sub <- cov_data[
      (district == this_district) & (viraemia15to49_mean >= top_catchments_cutoff),
    ]
    catchments_with_covs_sub <- catchments_with_covs[
      (catchments_with_covs$district == this_district) &
      (catchments_with_covs$viraemia15to49_mean >= top_catchments_cutoff),
    ]
    districts_sub <- districts[districts$area_name == this_district, ]
    if(nrow(catchments_with_covs_sub) > 0){
      for(cov_name in cov_names){
        check_names <- paste0(cov_name, '_', cutoffs_list[[cutoff_type]])
        if(all(check_names %in% colnames(cov_data_sub))){
          message("  -> Preparing plots for ", cov_name)
          catchments_with_covs_sub$THIS_COV <- 'All others'
          low_col <- paste0(cov_name, '_', cutoffs_list[[cutoff_type]]$low)
          catchments_with_covs_sub$THIS_COV[catchments_with_covs_sub[[low_col]] == 1L] <- 'Low'
          high_col <- paste0(cov_name, '_', cutoffs_list[[cutoff_type]]$high)
          catchments_with_covs_sub$THIS_COV[catchments_with_covs_sub[[high_col]] == 1L] <- 'High'
          cov_label <- config$get("covariates", cov_name)
          map_fig <- ggplot2::ggplot() +
            ggplot2::geom_sf(
              data = catchments_with_covs_sub,
              mapping = ggplot2::aes(fill = THIS_COV),
              color = NA,
              linewidth = 0L
            ) +
            ggplot2::geom_sf(data = districts_sub, fill = NA, color = 'black', linewidth = 0.5) +
            ggplot2::scale_fill_manual(
              values = c('All others' = '#cccccc', 'Low' = '#cc0099', 'High' = '#ff9900'),
            ) +
            ggplot2::theme_minimal() +
            ggplot2::theme(
              panel.grid = ggplot2::element_blank(),
              axis.text = ggplot2::element_blank(),
              axis.ticks = ggplot2::element_blank(),
              axis.title = ggplot2::element_blank()
            ) +
            ggplot2::labs(
              fill = cov_label |> gsub(pattern = ' ', replacement = '\n'),
              title = glue::glue('{cov_label} by health facility catchment in {this_district}'),
              subtitle = glue::glue('Categorized by {cutoff_type}'),
            )
          print(map_fig)
        } else {
          message("  -> Skipping ", cov_name, " because it does not have the required columns")
        }
      }
    }
  }
  dev.off()
}
