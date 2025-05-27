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
cov_data <- config$read("prepared_data", "covariates_by_catchment")
catchments <- config$read("catchments", "facility_catchments")
districts <- config$read("catchments", "admin_bounds") |>
  dplyr::filter(area_level == 3L)

# Merge catchments with covariates
catchments_id_field <- config$get("catchments_id_field")
catchments[[catchments_id_field]] <- seq_len(nrow(catchments))
catchments_with_covs <- merge(
  x = catchments,
  y = cov_data,
  by = intersect(colnames(catchments), colnames(cov_data))
)

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

## Plot each covariate nationwide, excluding the largest metro areas -------------------->

metro_districts <- c('Lilongwe', 'Blantyre', 'Zomba', 'Mzimba')
cov_data_sub <- cov_data[!district %in% metro_districts, ]
catchments_with_covs_sub <- catchments_with_covs[
  !catchments_with_covs$district %in% metro_districts,
]

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
