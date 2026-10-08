## #######################################################################################
##
## DHS 03. FIGURES: WEALTH AND WORK INDICATORS BY COMMUNITY PROFILE
##
## AUTHOR: Nat Henry, nat@henryspatialanalysis.com
## CREATED: 2026-10-08
## PURPOSE: Static figures for the DHS profile indicators estimated in
##   02_estimate_profile_indicators.R:
##   - Point estimates and 95% CIs for the headline indicators
##   - Work indicators by sex
##   - Composition of each profile by national wealth quintile
##   - Distribution of the wealth score and Lorenz curves by profile
##   - Pairwise differences between profiles
##   - Map of DHS clusters and their probability of lying in a profiled catchment
##   - Sensitivity of estimates to ignoring GPS displacement
##
##   Distributional figures (density, Lorenz) weight each household by its person
##   weight times the probability that its cluster lies in the profile.
##
## #######################################################################################

REPO_DIR <- '~/repos/mwi.hiv.factors'


## SETUP -------------------------------------------------------------------------------->

load_pkgs <- c('data.table', 'ggplot2', 'sf', 'patchwork', 'versioning')
lapply(load_pkgs, library, character.only = TRUE) |> invisible()
devtools::load_all(REPO_DIR, quiet = TRUE)

config <- versioning::Config$new(file.path(REPO_DIR, 'config.yaml'))
paper <- config$get('paper')
dhs_settings <- config$get('dhs')
survey_id <- commandArgs(trailingOnly = TRUE)[1]
if(is.na(survey_id)) survey_id <- 'mwi_2024'
survey <- dhs_settings$surveys[[survey_id]]
if(is.null(survey)) stop("Unknown survey: ", survey_id)
in_path <- function(key) dhs_survey_path(config, 'dhs_prepared', key, survey_id)
out_path <- function(key) dhs_survey_path(config, 'dhs_analysis', key, survey_id)
# Figure titles name the survey
ttl <- function(x) paste0(x, ' (', survey$label, ')')
sf::sf_use_s2(FALSE)

estimates <- data.table::fread(out_path('estimates'))
contrasts <- data.table::fread(out_path('contrasts'))
naive <- data.table::fread(out_path('sensitivity'))
households <- data.table::fread(in_path('households'))
location_probs <- data.table::fread(in_path('location_probs'))
location_probs[profile == '', profile := NA_character_]
profile_catchments <- data.table::fread(in_path('profile_catchments'))
clusters_sf <- sf::st_read(in_path('clusters'), quiet = TRUE)

profile_order <- names(paper$profiles)
profile_levels <- paste0(seq_along(profile_order), '. ', unlist(paper$profiles))
profile_colors <- unlist(paper$profile_colors)[profile_order] |>
  stats::setNames(profile_levels)
set_profile_factor <- function(dt, cols = 'profile'){
  for(cc in cols) dt[, (cc) := factor(get(cc), levels = rev(profile_levels))]
  invisible(dt)
}
set_profile_factor(estimates)
set_profile_factor(naive)

indicator_labels <- c(
  wealth_score = 'Mean wealth score',
  bottom_40 = 'In national poorest 40%',
  wealth_gini = 'Gini coefficient of wealth score (national zeroing)',
  wealth_gini_groupwise = 'Gini coefficient of wealth score (groupwise zeroing)',
  wealth_sd = 'SD of wealth score',
  worked_12m = 'Worked in last 12 months',
  nonwage_work = 'Non-wage work (women)',
  no_cash_earnings = 'No cash earnings',
  agriculture = 'Agricultural occupation',
  seasonal_or_occasional = 'Seasonal or occasional work',
  seasonal = 'Seasonal work',
  occasional = 'Occasional work'
)
pct_indicators <- c(
  'bottom_40', 'worked_12m', 'nonwage_work', 'no_cash_earnings', 'agriculture',
  'seasonal_or_occasional', 'seasonal', 'occasional', paste0('quintile_', 1:5)
)
for(dt in list(estimates, naive)){
  dt[
    indicator %in% pct_indicators,
    `:=` (est = est * 100, lower = lower * 100, upper = upper * 100)
  ]
}
rel_levels <- c(
  'Reliable', 'Caution: fewer than 10 clusters',
  'Unreliable: fewer than 5 clusters or n < 25'
)
estimates[, rel_class := factor(
  data.table::fcase(
    grepl('^Unreliable|^Suppress', reliability), rel_levels[3],
    grepl('^Caution', reliability), rel_levels[2],
    default = rel_levels[1]
  ),
  levels = rel_levels
)]

base_theme <- ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(face = 'bold', hjust = 0),
    legend.position = 'bottom',
    plot.title.position = 'plot',
    plot.caption.position = 'plot',
    plot.background = ggplot2::element_rect(fill = '#FFFFFF', color = NA)
  )
reliability_shapes <- stats::setNames(c(16, 21, 4), rel_levels)

#' Truncate unreliable intervals to the range spanned by the other estimates in a panel
#'
#' Adds `lower_plot`, `upper_plot`, and flags `trunc_lower`, `trunc_upper`.
truncate_intervals <- function(dt, panel_col = 'panel', pad = 0.08){
  dt[, `:=` (
    lim_lo = min(c(lower[rel_class != rel_levels[3]], est), na.rm = TRUE),
    lim_hi = max(c(upper[rel_class != rel_levels[3]], est), na.rm = TRUE)
  ), by = panel_col]
  dt[, `:=` (
    lim_lo = lim_lo - pad * (lim_hi - lim_lo),
    lim_hi = lim_hi + pad * (lim_hi - lim_lo)
  )]
  dt[, `:=` (
    trunc_lower = lower < lim_lo, trunc_upper = upper > lim_hi,
    lower_plot = pmax(lower, lim_lo), upper_plot = pmin(upper, lim_hi)
  )]
  invisible(dt)
}

dot_interval_panel <- function(dt, x_label = NULL){
  truncate_intervals(dt)
  arrow_style <- grid::arrow(length = grid::unit(0.12, 'cm'), type = 'closed')
  ggplot2::ggplot(dt, ggplot2::aes(y = profile, x = est, color = profile)) +
    ggplot2::geom_linerange(
      ggplot2::aes(xmin = lower_plot, xmax = upper_plot), linewidth = 1.1
    ) +
    ggplot2::geom_segment(
      data = dt[trunc_upper == TRUE],
      ggplot2::aes(x = est, xend = upper_plot, yend = profile),
      linewidth = 1.1, arrow = arrow_style
    ) +
    ggplot2::geom_segment(
      data = dt[trunc_lower == TRUE],
      ggplot2::aes(x = est, xend = lower_plot, yend = profile),
      linewidth = 1.1, arrow = arrow_style
    ) +
    ggplot2::geom_point(
      ggplot2::aes(shape = rel_class), size = 2.8, fill = '#FFFFFF', stroke = 1.1
    ) +
    ggplot2::scale_color_manual(values = profile_colors, guide = 'none') +
    ggplot2::scale_shape_manual(values = reliability_shapes, name = NULL, drop = FALSE) +
    ggplot2::labs(x = x_label, y = NULL) +
    base_theme
}


## FIGURE: HEADLINE INDICATORS ---------------------------------------------------------->

headline <- data.table::rbindlist(list(
  estimates[
    group == 'Households' & indicator %in% c(
      'wealth_score', 'bottom_40', 'wealth_gini', 'wealth_gini_groupwise'
    )
  ],
  estimates[group == 'Women' & indicator == 'nonwage_work'],
  estimates[
    group == 'All adults' &
      indicator %in% c('no_cash_earnings', 'agriculture', 'seasonal_or_occasional')
  ]
))
headline_units <- c(
  wealth_score = 'Mean wealth score (national mean = 0)',
  bottom_40 = '% in national poorest 40%',
  wealth_gini = 'Gini coefficient of wealth score (national zeroing)',
  wealth_gini_groupwise = 'Gini coefficient of wealth score (groupwise zeroing)',
  nonwage_work = '% of working women in non-wage work',
  no_cash_earnings = '% of working adults with no cash earnings',
  agriculture = '% of working adults in agriculture',
  seasonal_or_occasional = '% of working adults in seasonal/occasional work'
)
headline[, panel := factor(headline_units[indicator], levels = headline_units)]
fig_indicators <- dot_interval_panel(headline) +
  ggplot2::facet_wrap(
    ~panel, ncol = 2, scales = 'free_x', labeller = ggplot2::label_wrap_gen(45)
  ) +
  ggplot2::labs(
    title = ttl('Household wealth and work by community profile'),
    subtitle = paste0(
      'Points: direct survey estimates. Bars: 95% CIs ',
      'including cluster-location uncertainty.\nArrows: interval continues beyond ',
      'the panel edge.'
    )
  )
ggplot2::ggsave(
  out_path('fig_indicators'), fig_indicators,
  width = 10, height = 11, dpi = 300, bg = 'white'
)


## FIGURE: WORK INDICATORS BY SEX ------------------------------------------------------->

work_inds <- c('worked_12m', 'no_cash_earnings', 'agriculture', 'seasonal_or_occasional')
by_sex <- estimates[indicator %in% work_inds & group %in% c('Women', 'Men')]
by_sex[, panel := factor(
  indicator_labels[indicator], levels = indicator_labels[work_inds]
)]
by_sex[, group := factor(group, levels = c('Women', 'Men'))]
truncate_intervals(by_sex)
dodge <- ggplot2::position_dodge(width = 0.6)
fig_sex <- ggplot2::ggplot(
  by_sex, ggplot2::aes(y = profile, x = est, color = profile, group = group)
) +
  ggplot2::geom_linerange(
    ggplot2::aes(xmin = lower_plot, xmax = upper_plot, linetype = group), linewidth = 0.9,
    position = dodge
  ) +
  ggplot2::geom_point(
    ggplot2::aes(shape = group), size = 2.6, fill = '#FFFFFF', stroke = 1.1,
    position = dodge
  ) +
  ggplot2::scale_shape_manual(values = c(Women = 16, Men = 24), name = NULL) +
  ggplot2::scale_linetype_manual(
    values = c(Women = 'solid', Men = 'solid'), guide = 'none'
  ) +
  ggplot2::scale_color_manual(values = profile_colors, guide = 'none') +
  ggplot2::facet_wrap(~panel, nrow = 1, scales = 'free_x') +
  ggplot2::labs(
    x = '% (work characteristics among those who worked in the last 12 months)', y = NULL,
    title = ttl('Work by sex and community profile'),
    subtitle = paste(
      'Adults aged 15-49. Circles: women; triangles: men. Bars: 95% CIs; those of',
      'profiles with\nfewer than 5 expected clusters are cut at the panel edge.'
    )
  ) +
  base_theme
ggplot2::ggsave(
  out_path('fig_work_by_sex'), fig_sex,
  width = 12, height = 5.5, dpi = 300, bg = 'white'
)


## FIGURE: WEALTH QUINTILE COMPOSITION -------------------------------------------------->

quint <- estimates[grepl('^quintile_', indicator)]
quint[, quintile := factor(
  c('Poorest', 'Poorer', 'Middle', 'Richer', 'Richest')[
    as.integer(sub('quintile_', '', indicator))
  ],
  levels = rev(c('Poorest', 'Poorer', 'Middle', 'Richer', 'Richest'))
)]
quint[, est_norm := est / sum(est) * 100, by = profile]
quintile_colors <- c('#B35806', '#F1A340', '#C9C9C9', '#998EC3', '#542788') |>
  stats::setNames(c('Poorest', 'Poorer', 'Middle', 'Richer', 'Richest'))
fig_quint <- ggplot2::ggplot(
  quint, ggplot2::aes(y = profile, x = est_norm, fill = quintile)
) +
  ggplot2::geom_col(width = 0.7, color = '#FFFFFF', linewidth = 0.3) +
  ggplot2::geom_text(
    ggplot2::aes(label = ifelse(est_norm >= 6, sprintf('%.0f', est_norm), '')),
    position = ggplot2::position_stack(vjust = 0.5), size = 3.2, color = '#171717'
  ) +
  ggplot2::geom_vline(
    xintercept = c(20, 40, 60, 80), color = '#FFFFFF', linetype = 'dotted'
  ) +
  ggplot2::scale_fill_manual(
    values = quintile_colors, breaks = names(quintile_colors), name = NULL
  ) +
  ggplot2::scale_x_continuous(expand = c(0, 0), breaks = seq(0, 100, 20)) +
  ggplot2::labs(
    x = '% of population by national wealth quintile', y = NULL,
    title = ttl('Where each profile sits in the national wealth distribution'),
    subtitle = 'Each national quintile holds 20% of Malawians. Shares normalised to 100%.'
  ) +
  base_theme +
  ggplot2::theme(
    axis.text.y = ggplot2::element_text(color = rev(profile_colors), face = 'bold')
  )
ggplot2::ggsave(
  out_path('fig_quintiles'), fig_quint,
  width = 10, height = 4.8, dpi = 300, bg = 'white'
)


## DISTRIBUTIONAL DATA: FRACTIONAL PROFILE WEIGHTS -------------------------------------->

profile_probs <- location_probs[
  !is.na(profile), .(prob = sum(prob)), by = .(cluster, profile)
]
hh_frac <- merge(
  households[, .(cluster, person_weight, wealth_score, wealth_score_shifted)],
  profile_probs, by = 'cluster', allow.cartesian = TRUE
)
hh_frac[, w := person_weight * prob]
hh_frac[, profile := factor(profile, levels = profile_levels)]


## FIGURE: WEALTH SCORE DISTRIBUTION ---------------------------------------------------->

density_dt <- hh_frac[
  ,
  {
    d <- stats::density(
      wealth_score, weights = w / sum(w), from = -1.5, to = 4.7, n = 400, bw = 0.12
    )
    .(x = d$x, y = d$y)
  },
  by = profile
]
quintile_cuts <- households[
  ,
  .(cut = max(wealth_score)),
  by = wealth_quintile
][order(wealth_quintile)][1:4, cut]
ghost <- data.table::copy(density_dt)[, ghost_profile := profile][, profile := NULL]
ghost <- ghost[
  ,
  .(profile = factor(profile_levels, levels = profile_levels)),
  by = .(x, y, ghost_profile)
]
ghost <- ghost[profile != ghost_profile]
fig_density <- ggplot2::ggplot(density_dt, ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_vline(
    xintercept = quintile_cuts, color = '#BBBBBB', linetype = 'dashed', linewidth = 0.3
  ) +
  ggplot2::geom_line(
    data = ghost, ggplot2::aes(group = ghost_profile), color = '#D0D0D0', linewidth = 0.35
  ) +
  ggplot2::geom_area(ggplot2::aes(fill = profile), alpha = 0.75) +
  ggplot2::geom_line(ggplot2::aes(color = profile), linewidth = 0.6) +
  ggplot2::scale_fill_manual(values = profile_colors, guide = 'none') +
  ggplot2::scale_color_manual(values = profile_colors, guide = 'none') +
  ggplot2::facet_wrap(~profile, ncol = 2) +
  ggplot2::labs(
    x = 'DHS wealth score', y = 'Population density',
    title = ttl('Distribution of household wealth by community profile'),
    subtitle = paste(
      'Grey lines: the other five profiles. Dashed lines: national quintile cut-points.'
    )
  ) +
  base_theme +
  ggplot2::theme(panel.grid.major.y = ggplot2::element_line(color = '#EEEEEE'))
ggplot2::ggsave(
  out_path('fig_wealth_density'), fig_density,
  width = 10, height = 8, dpi = 300, bg = 'white'
)


## FIGURE: LORENZ CURVES ---------------------------------------------------------------->

lorenz_dt <- hh_frac[order(profile, wealth_score_shifted)][
  ,
  .(
    pop_share = c(0, cumsum(w) / sum(w)),
    wealth_share = c(0, cumsum(w * wealth_score_shifted) / sum(w * wealth_score_shifted))
  ),
  by = profile
]
gini_labels <- estimates[indicator == 'wealth_gini' & group == 'Households'][
  , .(profile = factor(as.character(profile), levels = profile_levels),
      label = sprintf('Gini %.2f (%.2f-%.2f)', est, lower, upper))
]
fig_lorenz <- ggplot2::ggplot(lorenz_dt, ggplot2::aes(x = pop_share, y = wealth_share)) +
  ggplot2::geom_abline(slope = 1, intercept = 0, color = '#999999', linetype = 'dashed') +
  ggplot2::geom_line(ggplot2::aes(color = profile), linewidth = 1) +
  ggplot2::geom_text(
    data = gini_labels, ggplot2::aes(x = 0.03, y = 0.95, label = label), hjust = 0,
    size = 3.4, inherit.aes = FALSE
  ) +
  ggplot2::scale_color_manual(values = profile_colors, guide = 'none') +
  ggplot2::scale_x_continuous(labels = scales::percent) +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::coord_equal() +
  ggplot2::facet_wrap(~profile, ncol = 3, labeller = ggplot2::label_wrap_gen(32)) +
  ggplot2::labs(
    x = 'Cumulative share of population (poorest first)',
    y = 'Cumulative share of wealth score',
    title = ttl('Wealth inequality within each community profile'),
    subtitle = paste0(
      'Lorenz curves of the wealth score with national zeroing ',
      '(poorest household nationally = 0).'
    )
  ) +
  base_theme +
  ggplot2::theme(panel.grid.major.y = ggplot2::element_line(color = '#EEEEEE'))
ggplot2::ggsave(
  out_path('fig_lorenz'), fig_lorenz,
  width = 10, height = 7.5, dpi = 300, bg = 'white'
)


## FIGURE: PAIRWISE CONTRASTS ----------------------------------------------------------->

contrast_specs <- data.table::data.table(
  indicator = c(
    'wealth_score', 'bottom_40', 'wealth_gini', 'wealth_gini_groupwise', 'nonwage_work',
    'no_cash_earnings', 'agriculture', 'seasonal_or_occasional'
  ),
  group = c(
    'Households', 'Households', 'Households', 'Households', 'Women', 'All adults',
    'All adults', 'All adults'
  )
)
con <- merge(contrasts, contrast_specs, by = c('indicator', 'group'))
con[indicator %in% pct_indicators, diff := diff * 100]
# Show each pair in both directions so the matrix reads as row minus column
con_full <- data.table::rbindlist(list(
  con[, .(indicator, row = profile_a, col = profile_b, diff, p_holm)],
  con[, .(indicator, row = profile_b, col = profile_a, diff = -diff, p_holm)]
))
con_full[, `:=` (
  row = factor(row, levels = rev(profile_levels)),
  col = factor(col, levels = profile_levels),
  label = paste0(
    ifelse(
      abs(diff) >= 10 | indicator %in% pct_indicators,
      sprintf('%.0f', diff),
      sprintf('%.2f', diff)
    ),
    data.table::fcase(p_holm < 0.01, '**', p_holm < 0.05, '*', default = '')
  ),
  panel = factor(
    indicator_labels[indicator], levels = indicator_labels[contrast_specs$indicator]
  )
)]
con_full[indicator %in% c('wealth_gini', 'wealth_gini_groupwise'), label := paste0(
  sprintf('%.2f', diff),
  data.table::fcase(p_holm < 0.01, '**', p_holm < 0.05, '*', default = '')
)]
con_full[, scaled := diff / max(abs(diff)), by = indicator]
short_levels <- stats::setNames(as.character(seq_along(profile_levels)), profile_levels)
fig_contrasts <- ggplot2::ggplot(
  con_full, ggplot2::aes(x = col, y = row, fill = scaled)
) +
  ggplot2::geom_tile(color = '#FFFFFF') +
  ggplot2::geom_text(ggplot2::aes(label = label), size = 2.9) +
  ggplot2::scale_fill_distiller(palette = 'PuOr', limits = c(-1, 1), guide = 'none') +
  ggplot2::scale_x_discrete(labels = short_levels) +
  ggplot2::scale_y_discrete(labels = short_levels) +
  ggplot2::facet_wrap(~panel, ncol = 4) +
  ggplot2::labs(
    x = 'Column profile', y = 'Row profile',
    title = ttl('Differences between profiles (row minus column)'),
    subtitle = paste(
      'Percentage points for shares; score units for wealth and Gini.',
      '* p < 0.05, ** p < 0.01 after Holm adjustment across the 15 pairs.'
    ),
    caption = paste(
      paste0(seq_along(profile_levels), ' = ', sub('^[0-9]+\\. ', '', profile_levels)),
      collapse = '; '
    ) |>
      strwrap(width = 150) |> paste(collapse = '\n')
  ) +
  base_theme +
  ggplot2::theme(panel.grid = ggplot2::element_blank())
ggplot2::ggsave(
  out_path('fig_contrasts'), fig_contrasts,
  width = 12, height = 7, dpi = 300, bg = 'white'
)


## FIGURE: CLUSTER MAP ------------------------------------------------------------------>

catchments_sf <- config$read('catchments', 'facility_catchments', quiet = TRUE)
admin_bounds <- config$read('catchments', 'admin_bounds', quiet = TRUE)
districts_sf <- admin_bounds[admin_bounds$area_level == 3L, ]
study_districts <- config$get('subset_districts')
profiled_sf <- merge(
  catchments_sf[, 'catchment_id'], profile_catchments[, .(catchment_id, profile)],
  by = 'catchment_id'
)
profiled_sf$profile <- factor(profiled_sf$profile, levels = profile_levels)
p_in_profile <- location_probs[!is.na(profile), .(p_profile = sum(prob)), by = cluster]
clusters_sf <- merge(clusters_sf, p_in_profile, by = 'cluster', all.x = TRUE)
clusters_sf$p_profile[is.na(clusters_sf$p_profile)] <- 0
clusters_sf$p_band <- cut(
  clusters_sf$p_profile, breaks = c(-Inf, 0.001, 0.25, 0.75, Inf),
  labels = c('~0', '0-25%', '25-75%', '75-100%')
)
map_clusters <- clusters_sf[
  clusters_sf$p_profile > 0.001 | clusters_sf$dhs_district %in% study_districts,
]

bbox_for_catchments <- function(district_names, pad){
  ids <- profile_catchments[district %in% district_names, catchment_id]
  sf::st_bbox(profiled_sf[profiled_sf$catchment_id %in% ids, ]) + pad
}
panel_specs <- list(
  list(
    title = 'A. Lilongwe City and surrounds',
    bbox = bbox_for_catchments('Lilongwe', c(-0.05, -0.05, 0.05, 0.05))
  ),
  list(
    title = 'B. Kasungu and Dowa',
    bbox = bbox_for_catchments(c('Kasungu', 'Dowa'), c(-0.15, -0.1, 0.15, 0.1))
  ),
  list(
    title = 'C. Northern Karonga',
    bbox = bbox_for_catchments('Karonga', c(-0.12, -0.1, 0.12, 0.1))
  ),
  list(
    title = 'D. Chikwawa, Nsanje, and Mulanje',
    bbox = bbox_for_catchments(
      c('Chikwawa', 'Nsanje', 'Mulanje'), c(-0.1, -0.1, 0.1, 0.1)
    )
  )
)
map_panels <- lapply(panel_specs, function(spec){
  mwi.hiv.factors::profile_map(
    catchments_sf = profiled_sf, districts_sf = districts_sf,
    profile_colors = profile_colors, focus_districts = study_districts,
    bbox = spec$bbox, title = spec$title
  ) +
    ggplot2::geom_sf(
      data = map_clusters, ggplot2::aes(size = p_band, shape = urban),
      fill = '#171717', color = '#FFFFFF', stroke = 0.4, inherit.aes = FALSE
    ) +
    ggplot2::scale_size_manual(
      values = c(`~0` = 1, `0-25%` = 1.8, `25-75%` = 2.6, `75-100%` = 3.4),
      name = 'P(cluster in a profiled catchment)', drop = FALSE
    ) +
    ggplot2::scale_shape_manual(
      values = c(`TRUE` = 22, `FALSE` = 21),
      labels = c(`TRUE` = 'Urban', `FALSE` = 'Rural'),
      name = 'DHS cluster'
    ) +
    ggplot2::coord_sf(
      xlim = spec$bbox[c(1, 3)], ylim = spec$bbox[c(2, 4)], expand = FALSE
    ) +
    ggplot2::guides(
      size = ggplot2::guide_legend(
        override.aes = list(shape = 21, fill = '#171717', color = '#FFFFFF')
      ),
      shape = ggplot2::guide_legend(
        override.aes = list(size = 3, fill = '#171717', color = '#FFFFFF')
      )
    )
})
fig_map <- patchwork::wrap_plots(map_panels, ncol = 2) +
  patchwork::plot_layout(guides = 'collect') +
  patchwork::plot_annotation(
    title = ttl('DHS clusters near profiled catchments'),
    subtitle = paste0(
      'Displaced cluster points, sized by the posterior probability that the true ',
      'location lies in a profiled catchment.'
    )
  ) &
  ggplot2::theme(legend.position = 'right')
ggplot2::ggsave(
  out_path('fig_cluster_map'), fig_map,
  width = 12, height = 11, dpi = 300, bg = 'white'
)


## FIGURE: SENSITIVITY TO DISPLACEMENT -------------------------------------------------->

sens <- merge(
  headline[, .(indicator, group, profile, est, lower, upper, panel, rel_class)],
  naive[, .(
    indicator, group, profile, naive_est = est, naive_lower = lower, naive_upper = upper
  )],
  by = c('indicator', 'group', 'profile'), all.x = TRUE
)
sens_long <- data.table::rbindlist(list(
  sens[, .(
    panel, profile, rel_class, method = 'Displacement-aware (multiple imputation)', est,
    lower, upper
  )],
  sens[, .(
    panel, profile, rel_class, method = 'Naive (displaced point in polygon)',
    est = naive_est, lower = naive_lower, upper = naive_upper
  )]
))
truncate_intervals(sens_long)
sens_long[, method := factor(
  method,
  levels = c(
    'Displacement-aware (multiple imputation)', 'Naive (displaced point in polygon)'
  )
)]
fig_sens <- ggplot2::ggplot(
  sens_long,
  ggplot2::aes(y = profile, x = est, color = profile, shape = method, group = method)
) +
  ggplot2::geom_linerange(
    ggplot2::aes(xmin = lower_plot, xmax = upper_plot), linewidth = 0.8,
    position = ggplot2::position_dodge(width = 0.6)
  ) +
  ggplot2::geom_point(
    size = 2.4, fill = '#FFFFFF', stroke = 1,
    position = ggplot2::position_dodge(width = 0.6)
  ) +
  ggplot2::scale_shape_manual(values = c(16, 21), name = NULL) +
  ggplot2::scale_color_manual(values = profile_colors, guide = 'none') +
  ggplot2::facet_wrap(
    ~panel, ncol = 2, scales = 'free_x', labeller = ggplot2::label_wrap_gen(45)
  ) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = ttl('Sensitivity of profile estimates to GPS displacement'),
    subtitle = paste0(
      'Filled: clusters multiply imputed to catchments. Hollow: clusters assigned by ',
      'their displaced\ncoordinates. Intervals of profiles with fewer than 5 expected ',
      'clusters are cut at the panel edge.'
    )
  ) +
  base_theme
ggplot2::ggsave(
  out_path('fig_sensitivity'), fig_sens,
  width = 10, height = 11, dpi = 300, bg = 'white'
)
message('Done making DHS figures.')
