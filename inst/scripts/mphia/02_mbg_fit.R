## #######################################################################################
##
## MPHIA 02. MODEL-BASED GEOSTATISTICS FOR HIV PREVALENCE AND WEALTH BY DISTRICT
##
## AUTHOR: Nat Henry, nat@henryspatialanalysis.com
## CREATED: 2026-10-20
## PURPOSE: Fit intercept-only SPDE models with the mbg package, separately for each
##   district (Lilongwe, Blantyre), survey (MPHIA 2015-16, 2020-21), and outcome:
##   - hiv: HIV prevalence among tested adults aged 15-64
##   - wealth_top40: share of households in the top two wealth quintiles, with
##     quintiles cut within each district and survey across households with a wealth
##     score, weighted by the household weight
##   Each model uses only the clusters whose geomasked centroids fall in the district
##   (the city plus the rural district) and predicts on a 500 m grid over the district.
##
##   Fits the surveys in `mphia: mbg: run_surveys` and the model versions in
##   `run_variants` (from `variants`: with and without the cluster nugget), or the
##   versions named on the command line:
##   Rscript 02_mbg_fit.R [variant ...]
##
##   Outputs (mphia_mbg directory):
##   - inputs/{district}_{survey}_{outcome}.csv: cluster counts used in each model
##   - {variant}/predictions/{district}_{survey}_{outcome}.tif: mean, lower, upper,
##     ci_width
##   - {variant}/fit_stats/{district}_{survey}_{outcome}_hyperparameters.csv:
##     hyperparameter posteriors and mesh sizes
##   - {variant}/fit_stats/{district}_{survey}_{outcome}_validity.csv: in-sample RMSE,
##     log posterior density, and WAIC
##   Only districts in `run_districts` are fit; the others' outputs are left in place.
##
## #######################################################################################

REPO_DIR <- '~/repos/mwi.hiv.factors'


## SETUP -------------------------------------------------------------------------------->

load_pkgs <- c('data.table', 'sf', 'terra', 'versioning')
lapply(load_pkgs, library, character.only = TRUE) |> invisible()
devtools::load_all(REPO_DIR, quiet = TRUE)

config <- versioning::Config$new(file.path(REPO_DIR, 'config.yaml'))
settings <- config$get('mphia')
mbg_settings <- settings$mbg
sf::sf_use_s2(FALSE)
set.seed(mbg_settings$seed)

survey_ids <- unlist(mbg_settings$run_surveys)
districts <- names(settings$cities)


## STUDY AREAS -------------------------------------------------------------------------->

admin_bounds <- config$read('catchments', 'admin_bounds', quiet = TRUE)
areas_sf <- admin_bounds[admin_bounds$area_level == settings$area_level, 'area_name']
area_lookup <- lapply(districts, function(district){
  data.table::data.table(
    district = district, area_name = unlist(settings$cities[[district]])
  )
}) |> data.table::rbindlist()


## MODEL INPUTS ------------------------------------------------------------------------->

inputs <- list()
for(sid in survey_ids){
  survey <- settings$surveys[[sid]]
  raw_path <- function(key){
    file.path(config$get_dir_path('mphia_raw'), survey$raw_dir, survey[[key]])
  }
  adults <- read_mphia_survey(
    bio_path = raw_path('bio'),
    hh_path = raw_path('hh'),
    centroids_path = raw_path('centroids'),
    age_range = unlist(settings$age_range)
  )
  households <- read_mphia_households(
    hh_path = raw_path('hh'), centroids_path = raw_path('centroids')
  )
  adults[, area_name := assign_mphia_clusters(adults, areas_sf, 'area_name')]
  households[, area_name := assign_mphia_clusters(households, areas_sf, 'area_name')]
  adults <- merge(adults, area_lookup, by = 'area_name')
  households <- merge(households, area_lookup, by = 'area_name')
  # Quintiles are cut within each district and survey
  households[, wealth_quintile := mphia_wealth_quintiles(.SD), by = district]
  households[, wealth_top40 := as.integer(wealth_quintile >= 4L)]

  for(district_name in districts){
    inputs[[length(inputs) + 1]] <- list(
      district = district_name, survey_id = sid, outcome = 'hiv',
      data = mphia_mbg_input(
        adults[district == district_name], 'hiv', mbg_settings$crs
      )
    )
    inputs[[length(inputs) + 1]] <- list(
      district = district_name, survey_id = sid, outcome = 'wealth_top40',
      data = mphia_mbg_input(
        households[district == district_name], 'wealth_top40', mbg_settings$crs
      )
    )
  }
}


## FIT AND PREDICT ---------------------------------------------------------------------->

id_rasters <- lapply(districts, function(district_name){
  build_study_area_id_raster(
    polygon_sf = areas_sf[areas_sf$area_name %in% area_lookup[
      district == district_name, area_name
    ], ],
    resolution = mbg_settings$resolution,
    crs = mbg_settings$crs
  )
}) |> stats::setNames(districts)

# Fit the configured model versions unless versions are named on the command line
variants <- commandArgs(trailingOnly = TRUE)
if(length(variants) == 0) variants <- unlist(mbg_settings$run_variants)
for(item in inputs){
  ids <- list(district = item$district, survey = item$survey_id, outcome = item$outcome)
  data.table::fwrite(
    item$data,
    do.call(mphia_output_path, c(list(config, 'mphia_mbg', 'mbg_input'), ids))
  )
}
fit_districts <- unlist(mbg_settings$run_districts)
if(is.null(fit_districts)) fit_districts <- districts
for(variant in variants){
  use_nugget <- mbg_settings$variants[[variant]]$use_nugget
  for(item in inputs){
    if(!item$district %in% fit_districts) next
    ids <- list(
      district = item$district, survey = item$survey_id, outcome = item$outcome
    )
    message(
      'Fitting ', variant, ': ', paste(ids, collapse = ' / '), ', ', nrow(item$data),
      ' clusters, ', sum(item$data$samplesize), ' observations'
    )
    # Inner and outer mesh edges, with any district override
    mesh_edge <- mbg_settings$mesh_max_edge_by_district[[item$district]]
    if(is.null(mesh_edge)) mesh_edge <- mbg_settings$mesh_max_edge
    mesh_edge <- unlist(mesh_edge)
    fit <- fit_mphia_mbg(
      input_data = item$data,
      id_raster = id_rasters[[item$district]],
      mesh_max_edge = mesh_edge,
      mesh_cutoff = mbg_settings$mesh_cutoff,
      prior_spde_range = mbg_settings$prior_spde_range,
      n_samples = mbg_settings$n_samples,
      ui_width = mbg_settings$ui_width,
      use_nugget = use_nugget
    )
    out_path <- function(key){
      do.call(
        mphia_output_path,
        c(list(config, 'mphia_mbg', key, variant = variant), ids)
      )
    }
    terra::writeRaster(fit$predictions, out_path('mbg_predictions'), overwrite = TRUE)
    # Fit statistics are written per model, so that a run covering some districts
    #  leaves the others' files in place
    hp <- fit$hyperparameters
    hp[, names(ids) := ids]
    hp[, `:=`(
      mesh_inner_edge = mesh_edge[1],
      mesh_nodes = fit$mesh_nodes,
      range_threshold_share = fit$range_threshold_share,
      n_clusters = nrow(item$data),
      n_obs = sum(item$data$samplesize)
    )]
    data.table::setcolorder(hp, c('district', 'survey', 'outcome'))
    data.table::fwrite(hp, out_path('mbg_hyperparameters'))
    vv <- data.table::copy(fit$validity)
    vv[, names(ids) := ids]
    data.table::setcolorder(vv, c('district', 'survey', 'outcome'))
    data.table::fwrite(vv, out_path('mbg_validity'))
  }
}
message('Done')
