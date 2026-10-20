## #######################################################################################
##
## MPHIA 04. COMPARE MBG MODEL VERSIONS WITH AND WITHOUT THE CLUSTER NUGGET
##
## AUTHOR: Nat Henry, nat@henryspatialanalysis.com
## CREATED: 2026-10-20
## PURPOSE: For each district, survey, and outcome, compare the model versions fit by
##   02_mbg_fit.R on in-sample fit (WAIC, RMSE, log posterior density), hyperparameters,
##   the mean surface, and the width of the 80% uncertainty interval, averaged over the
##   district, the city, and the rural remainder.
##
##   Outputs (mphia_mbg directory):
##   - mbg_variant_comparison.csv: one row per version and model
##   - mbg_variant_ci_width.png: average CI width by version, model, and area
##
## #######################################################################################

REPO_DIR <- '~/repos/mwi.hiv.factors'


## SETUP -------------------------------------------------------------------------------->

load_pkgs <- c('data.table', 'sf', 'terra', 'ggplot2', 'versioning')
lapply(load_pkgs, library, character.only = TRUE) |> invisible()
devtools::load_all(REPO_DIR, quiet = TRUE)

config <- versioning::Config$new(file.path(REPO_DIR, 'config.yaml'))
settings <- config$get('mphia')
mbg_settings <- settings$mbg
sf::sf_use_s2(FALSE)
path_for <- function(key, ...) mphia_output_path(config, 'mphia_mbg', key, ...)

variants <- unlist(mbg_settings$run_variants)
if(length(variants) < 2) stop('Comparing model versions needs at least two in run_variants')
runs <- data.table::CJ(
  variant = variants, district = names(settings$cities),
  survey = unlist(mbg_settings$run_surveys), outcome = names(mbg_settings$outcomes),
  sorted = FALSE
)
admin_bounds <- config$read('catchments', 'admin_bounds', quiet = TRUE)
city_polygons <- lapply(names(settings$cities), function(district){
  admin_bounds[admin_bounds$area_name == settings$cities[[district]]$City, ]
}) |> stats::setNames(names(settings$cities))


## SURFACE SUMMARIES -------------------------------------------------------------------->

surfaces <- lapply(seq_len(nrow(runs)), function(ii){
  run <- runs[ii]
  pred <- terra::rast(path_for(
    'mbg_predictions', variant = run$variant, district = run$district,
    survey = run$survey, outcome = run$outcome
  ))
  out <- summarize_surface_by_area(pred[[c('mean', 'ci_width')]], city_polygons[[run$district]])
  out[, names(run) := as.list(run)]
  return(list(summary = out, mean = terra::values(pred$mean, mat = FALSE)))
})
area_summary <- data.table::rbindlist(lapply(surfaces, `[[`, 'summary'))

# Agreement of the mean surfaces between versions, cell by cell
models <- unique(runs[, .(district, survey, outcome)])
agreement <- lapply(seq_len(nrow(models)), function(jj){
  idx <- vapply(variants, function(vv){
    which(
      runs$variant == vv & runs$district == models$district[jj] &
        runs$survey == models$survey[jj] & runs$outcome == models$outcome[jj]
    )
  }, integer(1))
  a <- surfaces[[idx[1]]]$mean
  b <- surfaces[[idx[2]]]$mean
  keep <- !is.na(a) & !is.na(b)
  data.table::data.table(
    models[jj],
    mean_correlation = stats::cor(a[keep], b[keep]),
    mean_abs_difference = mean(abs(a[keep] - b[keep]))
  )
}) |> data.table::rbindlist()


## FIT STATISTICS AND HYPERPARAMETERS --------------------------------------------------->

read_variant <- function(key){
  lapply(seq_len(nrow(runs)), function(ii){
    run <- runs[ii]
    data.table::fread(path_for(
      key, variant = run$variant, district = run$district, survey = run$survey,
      outcome = run$outcome
    ))[, variant := run$variant]
  }) |> data.table::rbindlist(fill = TRUE)
}
validity <- read_variant('mbg_validity')
hyper <- read_variant('mbg_hyperparameters')
hyper_wide <- data.table::dcast(
  hyper[, .(variant, district, survey, outcome, parameter, median)],
  variant + district + survey + outcome ~ parameter, value.var = 'median'
)
data.table::setnames(
  hyper_wide,
  c('Range for space', 'Stdev for space', 'Precision for nugget'),
  c('range_km', 'spatial_sd', 'nugget_precision'),
  skip_absent = TRUE
)
hyper_wide[, nugget_sd := 1 / sqrt(nugget_precision)]

area_wide <- data.table::dcast(
  area_summary, variant + district + survey + outcome ~ area,
  value.var = c('mean', 'ci_width')
)
comparison <- merge(
  validity, hyper_wide, by = c('variant', 'district', 'survey', 'outcome')
) |>
  merge(area_wide, by = c('variant', 'district', 'survey', 'outcome')) |>
  merge(agreement, by = c('district', 'survey', 'outcome'))
data.table::setcolorder(comparison, c('district', 'survey', 'outcome', 'variant'))
data.table::setorder(comparison, outcome, district, survey, variant)
data.table::fwrite(comparison, path_for('mbg_comparison'))
print(comparison[, .(
  district, survey, outcome, variant,
  waic = round(waic_is, 1), rmse = round(rmse_is, 3), lpd = round(lpd_is, 1),
  range_km = round(range_km, 1), spatial_sd = round(spatial_sd, 2),
  nugget_sd = round(nugget_sd, 2),
  ci_district = round(100 * ci_width_District, 1),
  ci_city = round(100 * ci_width_City, 1), ci_rural = round(100 * ci_width_Rural, 1),
  mean_cor = round(mean_correlation, 3),
  mean_abs_diff = round(100 * mean_abs_difference, 1)
)])


## FIGURE: CI WIDTH BY VERSION ---------------------------------------------------------->

variant_labels <- vapply(mbg_settings$variants, `[[`, '', 'label')
plot_data <- area_summary[area != 'District']
plot_data[, `:=`(
  model = paste(district, settings$surveys[[survey[1]]]$label),
  version = factor(variant_labels[variant], levels = variant_labels),
  outcome_label = vapply(outcome, function(o) mbg_settings$outcomes[[o]]$label, ''),
  area = factor(area, levels = c('City', 'Rural'), labels = c('City', 'Rural district'))
), by = survey]
fig <- ggplot(plot_data, aes(x = 100 * ci_width, y = model)) +
  geom_line(aes(group = model), color = 'grey75', linewidth = 0.6) +
  geom_point(aes(color = version), size = 2.8) +
  scale_color_manual(values = c('#2a78d6', '#eb6834'), name = NULL) +
  facet_grid(area ~ outcome_label, scales = 'free_x') +
  labs(
    x = paste0('Average ', 100 * mbg_settings$ui_width, '% CI width (percentage points)'),
    y = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = 'top',
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    strip.text = element_text(face = 'bold'),
    plot.background = element_rect(fill = 'white', color = NA)
  )
ggsave(path_for('mbg_comparison_fig'), fig, width = 8, height = 4.5, dpi = 200)
message('Wrote ', path_for('mbg_comparison_fig'))
