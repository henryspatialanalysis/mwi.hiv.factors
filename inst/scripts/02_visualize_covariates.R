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
cov_data <- config$read("prepared_data", "covariates_by_facility")
catchments <- config$read("catchments", "facility_catchments")
admin_bounds <- config$read("catchments", "admin_bounds", quiet = TRUE)
districts <- admin_bounds |> dplyr::filter(area_level == 3L)
regions <- admin_bounds |> dplyr::filter(area_level == 1L)

# Merge catchments with covariates
catchments_id_field <- config$get("catchments_id_field")
catchments[[catchments_id_field]] <- seq_len(nrow(catchments))
catchments_with_covs <- merge(
  x = catchments,
  y = cov_data,
  by = intersect(colnames(catchments), colnames(cov_data))
)

## Add region (N+C, S) to spatial files ------------------------------------------------->

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


## Plot each covariate nationwide ------------------------------------------------------->

cov_names <- names(config$get("covariates"))
outcome_colors <- viridis::viridis(n = 100)

pdf(config$get_file_path("prepared_data", "covariates_viz"), height = 8, width = 8)
for(cov_name in cov_names){
  message("Preparing plots for ", cov_name)
  cov_label <- config$get("covariates", cov_name)
  mwi.hiv.factors::create_three_panel_plot(
    catchments_with_covs = catchments_with_covs,
    cov_data = cov_data,
    district_bounds = districts,
    cov_field = cov_name,
    viraemia_field = 'viraemia15to49_mean',
    pop_field = 'population_1km',
    plot_title = paste(cov_label, 'by health facility catchment'),
    cov_label = cov_label,
    hist_num_breaks = 30,
    outcome_colors = outcome_colors,
    col_lims = quantile(cov_data$THIS_COV, probs = c(0.05, 0.95), na.rm = TRUE)
  )
}
dev.off()


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
    col_lims = quantile(cov_data[[cov_name]], probs = c(0.05, 0.95), na.rm = TRUE)
  )
}
dev.off()

# Add maps for each image in C+N and S
regional_plots_dir <- config$get_dir_path('prepared_data') |> file.path('regional_maps')
dir.create(regional_plots_dir, showWarnings = FALSE, recursive = TRUE)
for(cov_name in cov_names){
  message("Plotting regional maps for ", cov_name)
  cov_label <- config$get("covariates", cov_name)
  catchments_with_covs_top$THIS_COV <- catchments_with_covs_top[[cov_name]]

  # Plot map
  png(
    glue::glue("{regional_plots_dir}/{cov_name}_map_nationwide.png"),
    height = 8, width = 5, units = 'in', res = 300
  )
  cov_map <- mwi.hiv.factors::create_covariate_map(
    catchments_with_covs = catchments_with_covs_top,
    district_bounds = districts,
    outcome_colors = outcome_colors,
    col_lims = quantile(cov_data[[cov_name]], probs = c(0.05, 0.95), na.rm = TRUE),
    log_scale = cov_name %in% config$get('log_viz'),
    cov_label = config$get("covariates", cov_name)
  )
  print(cov_map)
  dev.off()
  # Regional maps
  for(reg in c('Southern', 'North-Central')){
    if(reg == 'Southern'){
      d_sub <- districts[districts$region == 'Southern', ]
      c_sub <- catchments_with_covs_top[catchments_with_covs_top$region == 'Southern', ]
    } else {
      d_sub <- districts[districts$region != 'Southern', ]
      c_sub <- catchments_with_covs_top[catchments_with_covs_top$region != 'Southern', ]
    }
    png(
      glue::glue("{regional_plots_dir}/{cov_name}_map_{reg}.png"),
      height = 8, width = 5, units = 'in', res = 300
    )
    cov_map <- mwi.hiv.factors::create_covariate_map(
      catchments_with_covs = c_sub,
      district_bounds = d_sub,
      outcome_colors = outcome_colors,
      col_lims = quantile(cov_data[[cov_name]], probs = c(0.05, 0.95), na.rm = TRUE),
      log_scale = cov_name %in% config$get('log_viz'),
      cov_label = config$get("covariates", cov_name)
    )
    print(cov_map)
    dev.off()
  }
}


## Plot each covariate nationwide, excluding the largest metro areas -------------------->

cov_data_sub <- cov_data[in_municipality == 0L, ]
catchments_with_covs_sub <- catchments_with_covs[catchments_with_covs$in_municipality == 0L, ]

pdf(config$get_file_path("prepared_data", "covariates_viz_no_metros"), height = 8, width = 8)
for(cov_name in cov_names){
  message("Preparing plots for ", cov_name)
  cov_label <- config$get("covariates", cov_name)
  mwi.hiv.factors::create_three_panel_plot(
    catchments_with_covs = catchments_with_covs_sub,
    cov_data = cov_data_sub,
    district_bounds = districts,
    cov_field = cov_name,
    viraemia_field = 'viraemia15to49_mean',
    pop_field = 'population_1km',
    plot_title = paste(cov_label, 'by health facility catchment (largest metros excluded)'),
    cov_label = cov_label,
    hist_num_breaks = 30,
    outcome_colors = outcome_colors,
    col_lims = quantile(cov_data$THIS_COV, probs = c(0.05, 0.95), na.rm = TRUE)
  )
}
dev.off()


## Plot each covariate by district ------------------------------------------------------>

pdf(config$get_file_path("prepared_data", "covariates_viz_by_district"), height = 8, width = 8)
for(this_district in sort(unique(catchments_with_covs$district))){
  message("Running ALL plots for ", this_district)
  cov_data_sub <- cov_data[district == this_district, ]
  catchments_with_covs_sub <- catchments_with_covs[
    catchments_with_covs$district == this_district,
  ]
  districts_sub <- districts[districts$area_name == this_district, ]
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
      col_lims = quantile(cov_data$THIS_COV, probs = c(0.05, 0.95), na.rm = TRUE)
    )
  }
}
dev.off()
