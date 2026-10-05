## #######################################################################################
##
## 07. FIGURES AND TABLES FOR THE COMMUNITY PROFILES PAPER
##
## AUTHOR: Nat Henry, nat@henryspatialanalysis.com
## CREATED: 2026-09-29
## PURPOSE: Build the figures and summary tables for the community profiles manuscript.
##   - Figure 1: national maps of selected GIS indicators by facility catchment
##   - Figure 2: map of the final (expert-vetted) community profiles
##   - Figure 3: indicator distributions by final profile
##   - Table 2 inputs: indicator summaries by final profile
##   - Table 3: clustering fit at each analysis stage (A: GIS only; B: community
##     indicators added; C: expert-vetted profiles)
##   - Descriptive statistics by setting (metro vs. non-metro)
##
##   Stage versions, profile labels, colors, and indicator lists are set in the `paper`
##   block of config.yaml. Outputs are written to the `paper` directory.
##
## #######################################################################################

REPO_DIR <- '~/repos/mwi.hiv.factors'

## SETUP -------------------------------------------------------------------------------->

load_pkgs <- c('data.table', 'ggplot2', 'sf', 'patchwork', 'viridisLite', 'versioning')
lapply(load_pkgs, library, character.only = TRUE) |> invisible()
devtools::load_all(REPO_DIR)

config <- versioning::Config$new(file.path(REPO_DIR, 'config.yaml'))
paper <- config$get('paper')
out_dir <- config$get_dir_path('paper')
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

study_districts <- config$get('subset_districts')
cov_labels <- config$get('covariates') |> unlist()
community_vars <- config$get('community_indicators')

# Spatial data
catchments_sf <- config$read('catchments', 'facility_catchments', quiet = TRUE)
admin_bounds <- config$read('catchments', 'admin_bounds', quiet = TRUE)
districts_sf <- admin_bounds |> dplyr::filter(area_level == 3L)
study_districts_sf <- districts_sf[districts_sf$area_name %in% study_districts, ]

# Convert raw covariate units to reporting units: population-weighted density in people
#  per km2, distances in km, land use as a percentage of catchment area
to_reporting_units <- function(dt){
  dt <- data.table::copy(dt)
  if('log_population_1km' %in% names(dt)){
    dt[, log_population_1km := expm1(log_population_1km)]
  }
  dist_cols <- grep('^distance_', names(dt), value = TRUE)
  if(length(dist_cols) > 0){
    dt[, (dist_cols) := lapply(.SD, `/`, 1000), .SDcols = dist_cols]
  }
  lu_cols <- grep('^landuse_', names(dt), value = TRUE)
  if(length(lu_cols) > 0) dt[, (lu_cols) := lapply(.SD, `*`, 100), .SDcols = lu_cols]
  return(dt)
}
reporting_labels <- c(
  log_population_1km = 'Population density (people per km²)',
  rpi = 'Relative poverty index (higher = poorer)',
  landuse_agriculture = 'Agricultural land use (%)',
  landuse_settlements = 'Settlement and infrastructure land use (%)',
  landuse_logging_or_hard_commodities = 'Logging, drilling, or mining land use (%)',
  distance_towns = 'Distance to a town of 20,000+ (km)',
  distance_borders = 'Distance to international border (km)',
  distance_border_posts = 'Distance to border post (km)',
  distance_admarc = 'Distance to ADMARC market (km)',
  distance_cross_catchment = 'Distance to nearest facility in another catchment (km)',
  mobility = 'High population mobility (%)',
  econ_activity = 'Economic activity hub (%)',
  pop_growth = 'Rapid population growth (%)',
  weather = 'Extreme weather events (%)'
)


## FIGURE 1: NATIONAL INDICATOR MAPS ---------------------------------------------------->

national_covs <- config$read(
  'prepared_data', 'covariates_by_facility',
  custom_version = paper$national_covariates_version
) |>
  to_reporting_units()
national_sf <- catchments_sf |>
  dplyr::select(catchment_id) |>
  merge(y = national_covs, by = 'catchment_id')

fig1_panels <- lapply(paper$fig1_indicators, function(cov_name){
  national_sf$THIS_COV <- national_sf[[cov_name]]
  is_density <- cov_name == 'log_population_1km'
  col_lims <- stats::quantile(national_sf$THIS_COV, probs = c(0.02, 0.98), na.rm = TRUE)
  mwi.hiv.factors::create_covariate_map(
    catchments_with_covs = national_sf,
    district_bounds = districts_sf,
    outcome_colors = viridisLite::viridis(n = 100, direction = -1),
    col_lims = col_lims,
    log_scale = is_density,
    cov_label = reporting_labels[cov_name]
  ) +
    ggplot2::geom_sf(
      data = districts_sf, fill = NA, color = '#FFFFFF', linewidth = 0.15
    ) +
    ggplot2::geom_sf(
      data = study_districts_sf, fill = NA, color = '#171717', linewidth = 0.6
    ) +
    ggplot2::labs(
      fill = NULL, title = stringr::str_wrap(reporting_labels[cov_name], 28)
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 11, face = 'bold', hjust = 0.5),
      legend.text = ggplot2::element_text(size = 11),
      legend.key.width = ggplot2::unit(0.35, 'cm'),
      legend.key.height = ggplot2::unit(0.9, 'cm')
    )
})
# Population density spans two orders of magnitude: use fixed log breaks
density_idx <- which(paper$fig1_indicators == 'log_population_1km')
if(length(density_idx) == 1){
  fig1_panels[[density_idx]] <- suppressMessages(
    fig1_panels[[density_idx]] +
      ggplot2::scale_fill_gradientn(
        colors = viridisLite::viridis(n = 100, direction = -1),
        limits = c(100, 10000),
        breaks = c(100, 300, 1000, 3000, 10000),
        labels = scales::comma,
        oob = scales::squish,
        trans = 'log10'
      )
  )
}
fig1 <- patchwork::wrap_plots(fig1_panels, nrow = 2)
ggplot2::ggsave(
  filename = config$get_file_path('paper', 'fig1_indicator_maps'),
  plot = fig1, width = 9, height = 9, units = 'in', dpi = 400, bg = 'white'
)


## LOAD FINAL PROFILES ------------------------------------------------------------------>

profile_order <- names(paper$profiles)
profile_levels <- paste0(seq_along(profile_order), '. ', unlist(paper$profiles))
profile_colors <- unlist(paper$profile_colors)[profile_order] |>
  stats::setNames(profile_levels)

vetted <- config$read(
  'analysis', 'vetted_profile_groupings', custom_version = paper$stage_versions$C
)
data.table::setnames(vetted, 1, 'catchment_id')
vetted[, excluded := profile_color %in% paper$excluded_profiles]
vetted[
  excluded == FALSE,
  `:=` (
    profile_number = match(profile_color, profile_order),
    profile = factor(
      profile_levels[match(profile_color, profile_order)],
      levels = profile_levels
    )
  )
]
profiled <- vetted[excluded == FALSE, ]
message(
  'Final profiles: ', nrow(profiled), ' catchments in ', uniqueN(profiled$profile),
  ' profiles; ', vetted[excluded == TRUE, .N], ' excluded'
)


## FIGURE 2: MAP OF FINAL PROFILES ------------------------------------------------------>

profiled_sf <- catchments_sf |>
  dplyr::select(catchment_id) |>
  merge(y = profiled[, .(catchment_id, profile, profile_number)], by = 'catchment_id')
excluded_ids <- vetted[excluded == TRUE, catchment_id]
excluded_sf <- catchments_sf[catchments_sf$catchment_id %in% excluded_ids, ]

bbox_for <- function(district_names, pad = 0.05){
  bb <- sf::st_bbox(districts_sf[districts_sf$area_name %in% district_names, ])
  bb + c(-pad, -pad, pad, pad)
}
bbox_for_catchments <- function(district_names, pad){
  ids <- vetted[district %in% district_names, catchment_id]
  sf::st_bbox(profiled_sf[profiled_sf$catchment_id %in% ids, ]) + pad
}
lilongwe_city_bb <- bbox_for_catchments('Lilongwe', c(-0.03, -0.03, 0.03, 0.03))

kasungu_dowa_bb <- bbox_for_catchments(c('Kasungu', 'Dowa'), c(-0.25, -0.1, 0.25, 0.1))
karonga_bb <- bbox_for_catchments('Karonga', c(-0.15, -0.1, 0.15, 0.1))

national_panel <- mwi.hiv.factors::profile_map(
  catchments_sf = profiled_sf, districts_sf = districts_sf,
  profile_colors = profile_colors, focus_districts = study_districts,
  excluded_sf = excluded_sf, title = 'A. Study districts'
)
panel_specs <- list(
  list(title = 'B. Lilongwe City and surrounds', bbox = lilongwe_city_bb),
  list(title = 'C. Kasungu and Dowa', bbox = kasungu_dowa_bb),
  list(title = 'D. Northern Karonga', bbox = karonga_bb),
  list(
    title = 'E. Chikwawa, Nsanje, and Mulanje',
    bbox = bbox_for(c('Chikwawa', 'Nsanje', 'Mulanje'))
  )
)
zoom_panels <- lapply(panel_specs, function(spec){
  mwi.hiv.factors::profile_map(
    catchments_sf = profiled_sf, districts_sf = districts_sf,
    profile_colors = profile_colors, focus_districts = study_districts,
    bbox = spec$bbox, label_field = 'profile_number', excluded_sf = excluded_sf,
    title = spec$title, label_size = 4.2
  ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 12, face = 'bold', hjust = 0.02)
    )
})
# Label the excluded catchment in the Lilongwe panel
excluded_pt <- suppressWarnings(sf::st_point_on_surface(excluded_sf)) |>
  sf::st_coordinates()
zoom_panels[[1]] <- zoom_panels[[1]] +
  ggplot2::annotate(
    'label', x = excluded_pt[1, 'X'], y = excluded_pt[1, 'Y'], label = 'Excluded*',
    size = 2.4, label.size = 0, fill = '#FFFFFF'
  )
fig2 <- (
  national_panel |
  (zoom_panels[[1]] / zoom_panels[[2]]) |
  (zoom_panels[[3]] / zoom_panels[[4]])
) +
  patchwork::plot_layout(guides = 'collect', widths = c(1, 1.3, 1.3)) &
  ggplot2::theme(
    legend.position = 'bottom',
    legend.text = ggplot2::element_text(size = 11),
    legend.title = ggplot2::element_text(size = 12)
  ) &
  ggplot2::guides(fill = ggplot2::guide_legend(ncol = 2))
ggplot2::ggsave(
  filename = config$get_file_path('paper', 'fig2_profile_map'),
  plot = fig2, width = 9.5, height = 8, units = 'in', dpi = 400, bg = 'white'
)


## FIGURE 3: INDICATOR DISTRIBUTIONS BY PROFILE ----------------------------------------->

fig3_long <- to_reporting_units(profiled) |>
  _[, c('profile', paper$fig3_indicators), with = FALSE] |>
  data.table::melt(id.vars = 'profile', variable.name = 'indicator', value.name = 'value')
fig3_labels <- reporting_labels
fig3_labels['log_population_1km'] <- 'Population density, people per km²'
fig3_labels['rpi'] <- 'Relative poverty index (higher = poorer)'
fig3_labels['distance_borders'] <- 'Distance to the border (km)'
fig3_labels[community_vars] <- c(
  'Population mobility', 'Economic activity hub', 'Rapid population growth',
  'Extreme weather'
)
fig3_long[, indicator := factor(
  fig3_labels[as.character(indicator)],
  levels = fig3_labels[paper$fig3_indicators]
)]
# GIS-based (continuous) indicators as histograms on top, community-based (yes/no)
#  indicators as stacked bars below. patchwork aligns the panel areas so the six profile
#  columns line up across both parts.
fig3_community <- fig3_labels[intersect(paper$fig3_indicators, community_vars)]
fig3_split <- list(
  gis = fig3_long[!indicator %in% fig3_community, ],
  community = fig3_long[indicator %in% fig3_community, ]
) |> lapply(function(part_long) part_long[, indicator := droplevels(indicator)])
fig3_title_theme <- ggplot2::theme(
  plot.title = ggplot2::element_text(face = 'bold', size = 9, hjust = 0)
)
fig3_parts <- list(
  gis = mwi.hiv.factors::profile_indicator_histograms(
    data_long = fig3_split$gis,
    profile_colors = profile_colors,
    all_label = 'All profiled catchments',
    bins = 10,
    base_size = 8,
    show_all = FALSE,
    layout = 'indicators_as_rows',
    wrap_width = 16L,
    log_suffix = '(log10 scale)',
    log_indicators = fig3_labels['log_population_1km']
  ) +
    ggplot2::labs(title = 'A. GIS-based indicators'),
  community = mwi.hiv.factors::profile_indicator_bars(
    data_long = fig3_split$community,
    profile_colors = profile_colors,
    base_size = 8,
    wrap_width = 16L
  ) +
    ggplot2::labs(title = 'B. Community-based indicators')
) |> lapply(function(part) part + fig3_title_theme)
# Row heights in proportion to the number of indicators in each part
fig3 <- patchwork::wrap_plots(
  fig3_parts$gis, fig3_parts$community,
  ncol = 1,
  heights = c(
    length(paper$fig3_indicators) - length(fig3_community), length(fig3_community)
  )
)
ggplot2::ggsave(
  filename = config$get_file_path('paper', 'fig3_profile_indicators'),
  plot = fig3, width = 6.5, height = 8.2, units = 'in', dpi = 400, bg = 'white'
)


## TABLE 2 INPUTS: SUMMARIES BY PROFILE ------------------------------------------------->

gis_vars <- setdiff(names(config$get('pca_covariates')), community_vars) |>
  intersect(names(profiled))
profiled_ru <- to_reporting_units(profiled)
fmt_mean_sd <- function(x) sprintf('%s (%s)', signif(mean(x), 3), signif(stats::sd(x), 2))
table2 <- profiled_ru[
  , c(
    list(
      n_catchments = .N,
      districts = paste(sort(unique(district)), collapse = ', '),
      mean_viraemia_pct = round(100 * mean(viraemia15to49_mean), 2),
      plhiv_15to49 = round(sum(plhiv15to49_mean))
    ),
    lapply(.SD[, ..gis_vars], fmt_mean_sd),
    lapply(.SD[, ..community_vars], function(x) round(100 * mean(x)))
  ),
  by = profile
][order(profile)]
data.table::fwrite(table2, config$get_file_path('paper', 'table2_profile_summaries'))


## STAGE A RE-RUN: GIS INDICATORS ONLY ------------------------------------------------>
## The archived stage A outputs include an indicator (distance to the nearest facility in
## another catchment) that stages B and C do not. Re-run stage A here with the same GIS
## indicators as stage B, so that stages differ only in the community indicators.

n_pcs <- 4L
set.seed(2026L)
stage_b_data <- config$read('prepared_data', 'covariates_by_facility')
stage_b_data[is.na(in_municipality), in_municipality := 0L]
stage_a_data <- stage_b_data[
  (district %in% study_districts) & (
    (viraemia15to49_mean >= config$get('top_catchments_cutoff')) |
    (catchment_name %in% config$get('iit_facilities'))
  ),
][, setting := ifelse(in_municipality == 1L, 'Metro', 'Non-metro')]
stage_a_covs <- names(config$get('pca_covariates')) |>
  setdiff(community_vars) |>
  setdiff(paper$stage_a_exclude)
stage_a_results <- list()
for(setting_name in c('Non-metro', 'Metro')){
  setting_data <- stage_a_data[setting == setting_name, ]
  setting_covs <- stage_a_covs
  if(setting_name == 'Metro'){
    setting_covs <- setdiff(setting_covs, config$get('drop_from_metro'))
  }
  fit <- mwi.hiv.factors::pca_kmeans(
    covariate_data = setting_data, cov_names = setting_covs, n_pcs = n_pcs
  )
  pc_dt <- data.table::as.data.table(fit$scores[, seq_len(n_pcs), drop = FALSE]) |>
    data.table::setnames(paste0('pc', seq_len(n_pcs)))
  stage_a_results[[setting_name]] <- cbind(
    setting_data[, .(catchment_id, catchment_name, district)], pc_dt, fit$clusters
  )
  message(
    'Stage A re-run (', setting_name, '): ', length(setting_covs), ' indicators; ',
    'first ', n_pcs, ' components explain ',
    round(100 * sum(fit$variance_explained[seq_len(n_pcs)]), 1), '% of variance'
  )
}
stage_a_memberships <- lapply(names(stage_a_results), function(setting_name){
  k <- paper$stage_k[[setting_name]]
  stage_a_results[[setting_name]][
    , .(setting = setting_name, catchment_id, catchment_name, district,
        stage_a_cluster = get(paste0('kmeans_', k)))
  ]
}) |> data.table::rbindlist()
data.table::fwrite(stage_a_memberships, config$get_file_path('paper', 'stage_a_rerun'))


## TABLE 3: CLUSTERING FIT AT EACH STAGE ------------------------------------------------>

fit_rows <- list()
for(setting in c('Non-metro', 'Metro')){
  for(stage in c('A', 'A (B components)', 'B', 'C')){
    if(stage == 'A'){
      results <- stage_a_results[[setting]]
    } else if(stage == 'A (B components)'){
      # Stage A groups placed on the stage B components: one yardstick for A and B
      results <- file.path(
        config$get_dir_path('analysis', custom_version = paper$stage_versions$B),
        setting, 'pca_kmeans_results.csv'
      ) |>
        data.table::fread() |>
        merge(
          y = stage_a_results[[setting]][
            , c('catchment_id', paste0('kmeans_', paper$stage_k[[setting]])), with = FALSE
          ],
          by = 'catchment_id',
          suffixes = c('_b', '')
        )
    } else {
      # The results file name is templated by PCA group, so build the path directly
      results_fp <- file.path(
        config$get_dir_path('analysis', custom_version = paper$stage_versions[[stage]]),
        setting, 'pca_kmeans_results.csv'
      )
      results <- data.table::fread(results_fp)
    }
    pc_cols <- paste0('pc', seq_len(n_pcs))
    if(stage %in% c('A', 'A (B components)', 'B')){
      labels <- results[[paste0('kmeans_', paper$stage_k[[setting]])]]
      label_source <- sprintf('k-means, k = %d', paper$stage_k[[setting]])
    } else {
      labels <- vetted[results, on = 'catchment_id', profile_color]
      labels[labels %in% paper$excluded_profiles] <- NA
      label_source <- 'Expert-vetted profiles'
    }
    if(length(unique(stats::na.omit(labels))) < 2) next
    scores <- mwi.hiv.factors::score_clustering(as.matrix(results[, ..pc_cols]), labels)
    # Baseline: random assignment of the same units into the same number of groups
    baseline <- replicate(100, {
      mwi.hiv.factors::score_clustering(
        as.matrix(results[, ..pc_cols]),
        sample(seq_len(scores$k), size = nrow(results), replace = TRUE)
      )$pct_explained
    })
    fit_rows[[paste(setting, stage)]] <- data.table::data.table(
      setting = setting, stage = stage, grouping = label_source,
      n_catchments = scores$n_units, n_groups = scores$k,
      pct_variation_explained = round(100 * scores$pct_explained, 1),
      mean_silhouette = round(scores$mean_silhouette, 2),
      random_baseline_pct = round(100 * mean(baseline), 1)
    )
  }
}
table3 <- data.table::rbindlist(fit_rows)

## Stage A groups scored on the stage B components, so that A and B share one yardstick
stage_b_nonmetro <- file.path(
  config$get_dir_path('analysis', custom_version = paper$stage_versions$B),
  'Non-metro', 'pca_kmeans_results.csv'
) |> data.table::fread()
a_in_b <- merge(
  stage_b_nonmetro[, c('catchment_id', paste0('pc', seq_len(n_pcs))), with = FALSE],
  stage_a_memberships[setting == 'Non-metro', .(catchment_id, stage_a_cluster)],
  by = 'catchment_id'
)
a_in_b_score <- mwi.hiv.factors::score_clustering(
  as.matrix(a_in_b[, paste0('pc', seq_len(n_pcs)), with = FALSE]), a_in_b$stage_a_cluster
)
message(
  'Non-metro stage A groups scored on stage B components: ',
  round(100 * a_in_b_score$pct_explained, 1), '% explained; silhouette ',
  round(a_in_b_score$mean_silhouette, 2)
)

## Number of catchments that changed group between stages, after matching group labels
count_moves <- function(from, to){
  tab <- table(from, to)
  # Greedy one-to-one matching of groups by overlap, largest overlaps first
  matched <- 0
  while(length(tab) > 0 && nrow(tab) > 0 && ncol(tab) > 0){
    idx <- which(tab == max(tab), arr.ind = TRUE)[1, ]
    matched <- matched + tab[idx[1], idx[2]]
    tab <- tab[-idx[1], -idx[2], drop = FALSE]
  }
  length(from) - matched
}
stage_labels <- merge(
  stage_a_memberships[setting == 'Non-metro', .(catchment_id, A = stage_a_cluster)],
  stage_b_nonmetro[
    , .(catchment_id, B = get(paste0('kmeans_', paper$stage_k$`Non-metro`)))
  ],
  by = 'catchment_id'
)
stage_labels[vetted, C := i.profile_color, on = 'catchment_id']
stage_changes <- data.table::data.table(
  setting = 'Non-metro',
  comparison = c('A to B', 'B to C'),
  n_catchments = nrow(stage_labels),
  n_changed = c(
    count_moves(stage_labels$A, stage_labels$B),
    count_moves(stage_labels$B, stage_labels$C)
  ),
  stage_a_in_b_space_pct = round(100 * a_in_b_score$pct_explained, 1),
  stage_a_in_b_space_silhouette = round(a_in_b_score$mean_silhouette, 2)
)
print(stage_changes)
data.table::fwrite(stage_changes, config$get_file_path('paper', 'stage_changes'))
print(table3)
data.table::fwrite(table3, config$get_file_path('paper', 'table3_fit_by_stage'))


## DESCRIPTIVE STATISTICS BY SETTING ---------------------------------------------------->

study_covs <- national_covs[district %in% study_districts, ]
study_covs[is.na(in_municipality), in_municipality := 0L]
study_covs[, `:=` (
  setting = ifelse(in_municipality == 1L, 'Metro', 'Non-metro'),
  selected = catchment_id %in% vetted$catchment_id
)]
fmt_median_iqr <- function(x){
  q <- stats::quantile(x, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  sprintf('%s (%s to %s)', signif(q[2], 3), signif(q[1], 3), signif(q[3], 3))
}
desc_vars <- c('viraemia15to49_mean', gis_vars)
descriptive <- study_covs[
  , c(list(n_catchments = .N), lapply(.SD, fmt_median_iqr)),
  .SDcols = desc_vars,
  by = .(setting, selected)
][order(setting, -selected)]
data.table::fwrite(descriptive, config$get_file_path('paper', 'descriptive_stats'))
message('Wrote paper figures and tables to: ', out_dir)
