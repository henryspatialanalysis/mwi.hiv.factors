#' Map facility catchments by community profile
#'
#' @details Categorical map of catchment polygons filled by profile, drawn over a
#'   district basemap. Districts outside `focus_districts` are shaded grey. Each
#'   profiled catchment can carry a short text label (for example, a profile number) so
#'   that profile identity does not rely on color alone.
#'
#' @param catchments_sf ([sf::sf]) Catchment polygons with a factor field `profile`
#' @param districts_sf ([sf::sf]) District polygons with the field `area_name`
#' @param profile_colors (`character(N)`) Named fill colors, one per level of `profile`
#' @param focus_districts (`character(N)`, default NULL) Districts drawn in white; all
#'   other districts are shaded grey. If NULL, all districts are drawn in white.
#' @param bbox (`numeric(4)`, default NULL) Optional map extent as
#'   `c(xmin, ymin, xmax, ymax)`. If NULL, the extent of `districts_sf` is used.
#' @param label_field (`character(1)`, default NULL) Optional field in `catchments_sf`
#'   used to label each catchment at a point on its surface
#' @param excluded_sf ([sf::sf], default NULL) Optional catchments drawn with a white
#'   fill and dashed outline, for units that were profiled but set aside
#' @param title (`character(1)`, default NULL) Optional panel title
#' @param label_size (`numeric(1)`, default 2.5) Text size for catchment labels
#' @param show_legend (`logical(1)`, default TRUE) Whether to show the fill legend
#'
#' @return A [ggplot2::ggplot] object
#'
#' @import ggplot2
#' @importFrom sf st_bbox st_point_on_surface st_coordinates
#' @importFrom ggrepel geom_text_repel
#' @export
profile_map <- function(
  catchments_sf, districts_sf, profile_colors, focus_districts = NULL, bbox = NULL,
  label_field = NULL, excluded_sf = NULL, title = NULL, label_size = 2.5,
  show_legend = TRUE
){
  if(is.null(focus_districts)) focus_districts <- districts_sf$area_name
  focus_sf <- districts_sf[districts_sf$area_name %in% focus_districts, ]
  if(is.null(bbox)) bbox <- sf::st_bbox(districts_sf)

  map_fig <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = districts_sf, fill = '#E6E6E6', color = '#FFFFFF', linewidth = 0.3
    ) +
    ggplot2::geom_sf(data = focus_sf, fill = '#FFFFFF', color = NA) +
    ggplot2::geom_sf(
      data = catchments_sf,
      mapping = ggplot2::aes(fill = profile),
      color = '#FFFFFF',
      linewidth = 0.15
    )
  if(!is.null(excluded_sf)){
    map_fig <- map_fig +
      ggplot2::geom_sf(
        data = excluded_sf, fill = '#FFFFFF', color = '#222222', linewidth = 0.3,
        linetype = 'dashed'
      )
  }
  map_fig <- map_fig +
    ggplot2::geom_sf(data = focus_sf, fill = NA, color = '#222222', linewidth = 0.4) +
    ggplot2::scale_fill_manual(
      values = profile_colors,
      drop = FALSE,
      guide = if(show_legend) 'legend' else 'none'
    )
  if(!is.null(label_field)){
    label_points <- suppressWarnings(sf::st_point_on_surface(catchments_sf))
    label_dt <- data.table::data.table(
      sf::st_coordinates(label_points),
      label = catchments_sf[[label_field]]
    )
    # Label only catchments inside the map extent
    label_dt <- label_dt[X >= bbox[1] & X <= bbox[3] & Y >= bbox[2] & Y <= bbox[4], ]
    map_fig <- map_fig +
      ggrepel::geom_text_repel(
        data = label_dt,
        mapping = ggplot2::aes(x = X, y = Y, label = label),
        size = label_size,
        color = '#171717',
        fontface = 'bold',
        bg.color = '#FFFFFF',
        bg.r = 0.12,
        min.segment.length = 0.2,
        segment.size = 0.25,
        box.padding = 0.12,
        point.padding = 0,
        max.overlaps = Inf,
        seed = 1L
      )
  }
  map_fig <- map_fig +
    ggplot2::coord_sf(
      xlim = bbox[c(1, 3)],
      ylim = bbox[c(2, 4)],
      expand = FALSE
    ) +
    ggplot2::labs(title = title, fill = 'Profile') +
    ggplot2::theme_void() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = '#F7F7F7', color = '#444444'),
      plot.title = ggplot2::element_text(size = 10, face = 'bold', hjust = 0.02)
    )
  return(map_fig)
}


#' Facet labeller that wraps text, breaking long hyphenated words after a hyphen
#'
#' @details Works like [ggplot2::label_wrap_gen()], except that a hyphenated word longer
#'   than `width` may also break after one of its hyphens. Shorter hyphenated words are
#'   kept whole.
#'
#' @param width (`integer(1)`) Target line width in characters
#'
#' @return A ggplot2 labeller function
#'
#' @keywords internal
wrap_labeller <- function(width){
  wrap_one <- function(label){
    words <- strsplit(label, ' ', fixed = TRUE)[[1]]
    long_hyphenated <- nchar(words) > width & grepl('-', words, fixed = TRUE)
    words[long_hyphenated] <- gsub('-', '- ', words[long_hyphenated], fixed = TRUE)
    lines <- strwrap(paste(words, collapse = ' '), width = width)
    # Rejoin hyphenated words that did not need to break
    paste(gsub('- ', '-', lines, fixed = TRUE), collapse = '\n')
  }
  ggplot2::as_labeller(function(labels) vapply(labels, wrap_one, character(1)))
}


#' Faceted histograms of indicator values by community profile
#'
#' @details Each panel is a histogram of catchment values for one indicator in one
#'   profile. The dashed black line marks the mean across all catchments and the dashed
#'   colored line marks the profile mean. Two layouts are available:
#'   - `'profiles_as_rows'`: rows are profiles (plus an optional pooled row at the top)
#'     and columns are indicators; bars are vertical. Adapted from the cluster histograms
#'     in `04_cluster_viz.R`.
#'   - `'indicators_as_rows'`: rows are indicators and columns are profiles; bars are
#'     horizontal, so the indicator value runs up the vertical axis and each row shares
#'     one value scale across profiles.
#'
#' @param data_long ([data.table::data.table]) Long table with fields `profile` (factor),
#'   `indicator` (factor, in plotting order), and `value` (numeric)
#' @param profile_colors (`character(N)`) Named colors, one per level of `profile`
#' @param all_label (`character(1)`, default 'All') Label for the pooled group
#' @param all_color (`character(1)`, default '#6E6E6E') Color for the pooled group
#' @param bins (`integer(1)`, default 12) Number of histogram bins per panel
#' @param log_indicators (`character(N)`, default NULL) Indicator levels whose values
#'   should be shown on a log10 scale
#' @param base_size (`numeric(1)`, default 8) Base font size passed to the ggplot theme
#' @param log_suffix (`character(1)`, default '(log10)') Text appended to the labels of
#'   log-scaled indicators
#' @param show_all (`logical(1)`, default TRUE) Whether to add the pooled group as its
#'   own row (or column)
#' @param layout (`character(1)`, default 'profiles_as_rows') One of
#'   `'profiles_as_rows'` or `'indicators_as_rows'`
#' @param wrap_width (`integer(1)`, default 11) Character width for wrapping strip labels
#' @param binary_indicators (`character(N)`, default NULL) Indicator levels coded 0/1.
#'   These are drawn as two bars centred on axis labels given by `binary_labels`, and
#'   their mean lines show the share coded 1.
#' @param binary_labels (`character(2)`, default c('No', 'Yes')) Axis labels for 0 and 1
#'
#' @return A [ggplot2::ggplot] object
#'
#' @import ggplot2 data.table
#' @importFrom scales comma
#' @export
profile_indicator_histograms <- function(
  data_long, profile_colors, all_label = 'All', all_color = '#6E6E6E', bins = 12,
  log_indicators = NULL, base_size = 8, log_suffix = '(log10)', show_all = TRUE,
  layout = c('profiles_as_rows', 'indicators_as_rows'), wrap_width = 11L,
  binary_indicators = NULL, binary_labels = c('No', 'Yes')
){
  layout <- match.arg(layout)
  plot_data <- data.table::rbindlist(list(
    data.table::copy(data_long)[, profile := all_label],
    data.table::copy(data_long)[, profile := as.character(profile)]
  ))
  profile_levels <- c(all_label, levels(data_long$profile))
  plot_data[, profile := factor(profile, levels = profile_levels)]
  if(length(log_indicators) > 0){
    plot_data[indicator %in% log_indicators, value := log10(value)]
    levels(plot_data$indicator) <- ifelse(
      levels(plot_data$indicator) %in% log_indicators,
      paste(levels(plot_data$indicator), log_suffix),
      levels(plot_data$indicator)
    )
  }
  overall_means <- plot_data[
    profile == all_label, .(mean_val = mean(value, na.rm = TRUE)), by = indicator
  ]
  profile_means <- plot_data[
    profile != all_label,
    .(mean_val = mean(value, na.rm = TRUE)),
    by = .(profile, indicator)
  ]
  all_colors <- c(profile_colors, stats::setNames(all_color, all_label))
  # The pooled mean stays as the black reference line even when the pooled group is hidden
  if(!show_all){
    plot_data <- plot_data[profile != all_label, ]
    plot_data[, profile := droplevels(profile)]
  }

  # Yes/no indicators are drawn as bars centred on 0 and 1; everything else as histograms
  is_binary <- plot_data$indicator %in% binary_indicators
  binary_counts <- plot_data[is_binary, .(n = .N), by = .(profile, indicator, value)]
  continuous_data <- plot_data[!is_binary, ]
  # Keep both the 0 and 1 positions in every yes/no panel, even when one is empty
  binary_frame <- data.table::CJ(
    indicator = factor(binary_indicators, levels = levels(plot_data$indicator)),
    value = c(0, 1)
  )

  # Axis for indicator values: yes/no panels get labelled 0 and 1; panels spanning
  #  0 to 100 (percentages) get three ticks
  value_breaks <- function(lims){
    if(lims[1] > -0.6 && lims[2] < 1.6) return(c(0, 1))
    if(lims[1] <= 0 && lims[2] >= 100) return(c(0, 50, 100))
    scales::breaks_pretty(n = 3)(lims)
  }
  number_labels <- scales::label_number(accuracy = NULL, big.mark = ',')
  value_labels <- function(x){
    x_present <- x[!is.na(x)]
    if(length(binary_indicators) > 0 && identical(as.numeric(x_present), c(0, 1))){
      out <- x
      out[!is.na(x)] <- binary_labels
      return(out)
    }
    number_labels(x)
  }
  count_breaks <- function(lims) unique(floor(pretty(lims, n = 3)))
  labeller <- wrap_labeller(width = wrap_width)

  if(layout == 'profiles_as_rows'){
    fig <- ggplot2::ggplot(mapping = ggplot2::aes(x = value, fill = profile)) +
      ggplot2::facet_grid(profile ~ indicator, scales = 'free', labeller = labeller) +
      ggplot2::geom_histogram(
        data = continuous_data, bins = bins, color = '#FFFFFF', linewidth = 0.2
      ) +
      ggplot2::geom_col(
        data = binary_counts, ggplot2::aes(y = n), width = 0.4, color = '#FFFFFF',
        linewidth = 0.2
      ) +
      ggplot2::geom_blank(
        data = binary_frame, ggplot2::aes(x = value), inherit.aes = FALSE
      ) +
      ggplot2::geom_vline(
        data = overall_means, ggplot2::aes(xintercept = mean_val),
        linetype = 'dashed', color = '#171717', linewidth = 0.3
      ) +
      ggplot2::geom_vline(
        data = profile_means, ggplot2::aes(xintercept = mean_val, color = profile),
        linetype = 'dashed', linewidth = 0.4
      ) +
      ggplot2::scale_y_continuous(breaks = count_breaks) +
      ggplot2::scale_x_continuous(breaks = value_breaks, labels = value_labels) +
      ggplot2::labs(x = NULL, y = 'Number of catchments')
  } else {
    fig <- ggplot2::ggplot(mapping = ggplot2::aes(y = value, fill = profile)) +
      ggplot2::facet_grid(indicator ~ profile, scales = 'free', labeller = labeller) +
      ggplot2::geom_histogram(
        data = continuous_data, bins = bins, color = '#FFFFFF', linewidth = 0.2,
        orientation = 'y'
      ) +
      ggplot2::geom_col(
        data = binary_counts, ggplot2::aes(x = n), width = 0.4, color = '#FFFFFF',
        linewidth = 0.2, orientation = 'y'
      ) +
      ggplot2::geom_blank(
        data = binary_frame, ggplot2::aes(y = value), inherit.aes = FALSE
      ) +
      ggplot2::geom_hline(
        data = overall_means, ggplot2::aes(yintercept = mean_val),
        linetype = 'dashed', color = '#171717', linewidth = 0.3
      ) +
      ggplot2::geom_hline(
        data = profile_means, ggplot2::aes(yintercept = mean_val, color = profile),
        linetype = 'dashed', linewidth = 0.4
      ) +
      ggplot2::scale_x_continuous(breaks = count_breaks) +
      ggplot2::scale_y_continuous(breaks = value_breaks, labels = value_labels) +
      ggplot2::labs(x = 'Number of catchments', y = NULL)
  }
  fig <- fig +
    ggplot2::scale_fill_manual(
      values = all_colors, aesthetics = c('fill', 'color'), guide = 'none'
    ) +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = if(layout == 'profiles_as_rows'){
        ggplot2::element_blank()
      } else {
        ggplot2::element_line(color = '#EBEBEB')
      },
      panel.grid.major.y = if(layout == 'indicators_as_rows'){
        ggplot2::element_blank()
      } else {
        ggplot2::element_line(color = '#EBEBEB')
      },
      strip.background = ggplot2::element_rect(fill = '#F2F2F2', color = '#BBBBBB'),
      strip.text.y = ggplot2::element_text(angle = 0, hjust = 0, size = base_size * 0.85),
      strip.text.x = ggplot2::element_text(size = base_size * 0.85),
      axis.text.x = if(layout == 'profiles_as_rows'){
        ggplot2::element_text(angle = 45, hjust = 1)
      } else {
        ggplot2::element_text()
      }
    )
  return(fig)
}


#' Faceted stacked bars of yes/no indicators by community profile
#'
#' @details Rows are indicators and columns are profiles, matching the
#'   `'indicators_as_rows'` layout of [profile_indicator_histograms()]. Each panel holds
#'   one bar spanning 0 to 100%: the lower segment is the share of catchments coded 1
#'   (filled in the profile color) and the upper segment is the share coded 0 (white,
#'   outlined in the profile color). A dashed black line marks the share coded 1 across
#'   all catchments. When `show_divisions` is TRUE, each segment is subdivided into one
#'   slice per catchment, with white dividers in the lower segment and dividers in the
#'   profile color in the upper segment.
#'
#' @param data_long ([data.table::data.table]) Long table with fields `profile` (factor),
#'   `indicator` (factor, in plotting order), and `value` (numeric, coded 0/1)
#' @param profile_colors (`character(N)`) Named colors, one per level of `profile`
#' @param base_size (`numeric(1)`, default 8) Base font size passed to the ggplot theme
#' @param wrap_width (`integer(1)`, default 11) Character width for wrapping strip labels
#' @param bar_width (`numeric(1)`, default 0.5) Bar width as a share of the panel width
#' @param y_label (`character(1)`, default '% Yes') Y axis title
#' @param show_divisions (`logical(1)`, default TRUE) Whether to draw one slice per
#'   catchment within each bar segment
#' @param divider_linewidth (`numeric(1)`, default 0.15) Line width of the catchment
#'   dividers
#'
#' @return A [ggplot2::ggplot] object
#'
#' @import ggplot2 data.table
#' @export
profile_indicator_bars <- function(
  data_long, profile_colors, base_size = 8, wrap_width = 11L, bar_width = 0.5,
  y_label = '% Yes', show_divisions = TRUE, divider_linewidth = 0.15
){
  counts <- data_long[
    !is.na(value), .(n = .N, n_yes = sum(value == 1)), by = .(profile, indicator)
  ][, pct_yes := 100 * n_yes / n]
  overall <- data_long[, .(pct_yes = 100 * mean(value, na.rm = TRUE)), by = indicator]
  bars <- data.table::rbindlist(list(
    counts[, .(profile, indicator, segment = 'yes', ymin = 0, ymax = pct_yes)],
    counts[, .(profile, indicator, segment = 'no', ymin = pct_yes, ymax = 100)]
  ))
  bars[, `:=` (xmin = -bar_width / 2, xmax = bar_width / 2)]
  # One divider between each pair of adjacent catchments, except at the yes/no boundary
  dividers <- counts[n > 1, .(k = seq_len(n - 1)), by = .(profile, indicator, n, n_yes)][
    k != n_yes,
  ][, `:=` (
    y = 100 * k / n,
    segment = data.table::fifelse(k < n_yes, 'yes', 'no'),
    xmin = -bar_width / 2,
    xmax = bar_width / 2
  )]
  if(!show_divisions) dividers <- dividers[0, ]
  divider_aes <- ggplot2::aes(x = xmin, xend = xmax, y = y, yend = y)

  fig <- ggplot2::ggplot(
    mapping = ggplot2::aes(
      xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, color = profile
    )
  ) +
    ggplot2::facet_grid(
      indicator ~ profile, labeller = wrap_labeller(width = wrap_width)
    ) +
    # Fills, then catchment dividers, then outlines so that dividers do not cut the edges
    ggplot2::geom_rect(
      data = bars[segment == 'yes', ], ggplot2::aes(fill = profile), color = NA
    ) +
    ggplot2::geom_rect(data = bars[segment == 'no', ], fill = '#FFFFFF', color = NA) +
    ggplot2::geom_segment(
      data = dividers[segment == 'yes', ], mapping = divider_aes, inherit.aes = FALSE,
      color = '#FFFFFF', linewidth = divider_linewidth
    ) +
    ggplot2::geom_segment(
      data = dividers[segment == 'no', ],
      mapping = ggplot2::aes(x = xmin, xend = xmax, y = y, yend = y, color = profile),
      inherit.aes = FALSE, linewidth = divider_linewidth
    ) +
    ggplot2::geom_rect(data = bars, fill = NA, linewidth = 0.3) +
    ggplot2::geom_hline(
      data = overall, ggplot2::aes(yintercept = pct_yes),
      linetype = 'dashed', color = '#171717', linewidth = 0.3
    ) +
    ggplot2::scale_x_continuous(limits = c(-0.5, 0.5), breaks = NULL) +
    ggplot2::scale_y_continuous(limits = c(0, 100), breaks = c(0, 50, 100)) +
    ggplot2::scale_fill_manual(
      values = profile_colors, aesthetics = c('fill', 'color'), guide = 'none'
    ) +
    ggplot2::labs(x = NULL, y = y_label) +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = '#F2F2F2', color = '#BBBBBB'),
      strip.text.y = ggplot2::element_text(angle = 0, hjust = 0, size = base_size * 0.85),
      strip.text.x = ggplot2::element_text(size = base_size * 0.85)
    )
  return(fig)
}


#' Order radar spokes so that neighbouring spokes have similar profile patterns
#'
#' @details Finds the circular order of indicators that minimises the sum of
#'   (1 - correlation) between each pair of neighbouring spokes, including the pair that
#'   closes the circle. Correlations are taken across profiles, using the scaled profile
#'   means. The first indicator stays first. Every order is searched, so this is limited
#'   to 10 or fewer indicators.
#'
#' @param means ([data.table::data.table]) Table with fields `profile`, `indicator`,
#'   and `r` (scaled profile mean)
#' @param indicator_levels (`character(N)`) Indicators in their current order
#'
#' @return `character(N)` Indicators in the new order
#'
#' @keywords internal
similarity_axis_order <- function(means, indicator_levels){
  n_axes <- length(indicator_levels)
  if(n_axes > 10) stop('Similarity ordering searches every order; use 10 or fewer axes')
  wide <- data.table::dcast(
    means[, .(profile, indicator = as.character(indicator), r)],
    profile ~ indicator, value.var = 'r'
  )
  dissimilarity <- 1 - suppressWarnings(stats::cor(as.matrix(wide[, ..indicator_levels])))
  # Indicators with no variation across profiles are treated as uncorrelated
  dissimilarity[is.na(dissimilarity)] <- 1
  permutations <- function(v){
    if(length(v) <= 1) return(list(v))
    do.call(c, lapply(seq_along(v), function(i){
      lapply(permutations(v[-i]), function(p) c(v[i], p))
    }))
  }
  orders <- lapply(permutations(seq_len(n_axes)[-1]), function(p) c(1L, p))
  costs <- vapply(
    orders, function(o) sum(dissimilarity[cbind(o, c(o[-1], o[1]))]), numeric(1)
  )
  return(indicator_levels[orders[[which.min(costs)]]])
}


#' Radar chart of mean indicator values by community profile
#'
#' @details Each indicator is one spoke, starting at 12 o'clock and running clockwise
#'   in the order of the `indicator` factor levels. Indicators listed in
#'   `binary_indicators` are coded 0/1 and keep that scale, so their spokes read as the
#'   share coded 1. Each other indicator is rescaled across profile means, from the
#'   lowest profile mean (0) to the highest (1). Scaled value 0 sits on a small inner
#'   ring of radius `inner_radius` rather than at the centre. Each profile is drawn as a
#'   polygon joining its scaled means.
#'
#' @param data_long ([data.table::data.table]) Long table with fields `profile` (factor),
#'   `indicator` (factor, in plotting order), and `value` (numeric, already on the scale
#'   to be shown, e.g. logged)
#' @param profile_colors (`character(N)`) Named colors, one per level of `profile`
#' @param binary_indicators (`character(N)`, default NULL) Indicator levels coded 0/1,
#'   which are not rescaled
#' @param inner_radius (`numeric(1)`, default 0.1) Radius of scaled value 0, as a share
#'   of the outer radius
#' @param ring_breaks (`numeric(N)`, default c(0, 0.25, 0.5, 0.75, 1)) Scaled values at
#'   which to draw reference rings
#' @param range_labels (`character(2)`, default c('Min.', 'Max.')) Labels for scaled
#'   values 0 and 1, drawn along the first spoke
#' @param fill_alpha (`numeric(1)`, default 0.06) Fill opacity of the profile polygons
#' @param base_size (`numeric(1)`, default 8) Base font size
#' @param order_by_similarity (`logical(1)`, default FALSE) Whether to reorder the
#'   spokes so that neighbouring spokes have similar profile patterns; see
#'   [similarity_axis_order()]. The first indicator stays at 12 o'clock.
#' @param wrap_width (`integer(1)`, default 16) Character width for wrapping axis labels
#'
#' @return A [ggplot2::ggplot] object
#'
#' @import ggplot2 data.table
#' @export
profile_radar <- function(
  data_long, profile_colors, binary_indicators = NULL, inner_radius = 0.1,
  ring_breaks = c(0, 0.25, 0.5, 0.75, 1), range_labels = c('Min.', 'Max.'),
  order_by_similarity = FALSE, fill_alpha = 0.06, base_size = 8, wrap_width = 16L
){
  to_radius <- function(scaled_value) inner_radius + (1 - inner_radius) * scaled_value
  indicator_levels <- levels(data_long$indicator)

  # Average by profile; binary indicators keep their 0-1 scale and the others are
  #  rescaled to their range across profile means
  means <- data_long[
    !is.na(value), .(mean_val = mean(value)), by = .(profile, indicator)
  ]
  means[, `:=` (mean_min = min(mean_val), mean_range = max(mean_val) - min(mean_val)),
    by = indicator
  ]
  means[, r := data.table::fifelse(
    indicator %in% binary_indicators, mean_val,
    data.table::fifelse(mean_range > 0, (mean_val - mean_min) / mean_range, 0.5)
  )]
  if(order_by_similarity){
    indicator_levels <- similarity_axis_order(means, indicator_levels)
  }

  n_axes <- length(indicator_levels)
  axes <- data.table::data.table(
    indicator = factor(indicator_levels, levels = indicator_levels),
    axis_i = seq_len(n_axes)
  )[, angle := pi / 2 - 2 * pi * (axis_i - 1) / n_axes]
  means[, indicator := factor(as.character(indicator), levels = indicator_levels)]
  means <- merge(means, axes, by = 'indicator')[order(profile, axis_i)]
  means[, `:=` (x = to_radius(r) * cos(angle), y = to_radius(r) * sin(angle))]

  # Background: reference rings, spokes, and axis labels
  rings <- data.table::CJ(ring = ring_breaks, axis_i = seq_len(n_axes))
  rings <- merge(rings, axes, by = 'axis_i')[order(ring, axis_i)]
  rings[, `:=` (x = to_radius(ring) * cos(angle), y = to_radius(ring) * sin(angle))]
  label_r <- 1.08
  axes[, `:=` (
    x_start = inner_radius * cos(angle),
    y_start = inner_radius * sin(angle),
    x_end = cos(angle),
    y_end = sin(angle),
    x_label = label_r * cos(angle),
    y_label = label_r * sin(angle),
    hjust = data.table::fifelse(abs(cos(angle)) < 0.1, 0.5, (1 - sign(cos(angle))) / 2),
    vjust = data.table::fifelse(abs(sin(angle)) < 0.1, 0.5, (1 - sign(sin(angle))) / 2),
    label = wrap_labeller(width = wrap_width)(list(indicator = indicator_levels))[[1]]
  )]
  ring_labels <- data.table::data.table(ring = c(0, 1), label = range_labels)

  fig <- ggplot2::ggplot() +
    # Faint profile fills first, so that grid lines and profile outlines sit on top
    ggplot2::geom_polygon(
      data = means, ggplot2::aes(x = x, y = y, group = profile, fill = profile),
      alpha = fill_alpha, color = NA, show.legend = FALSE
    ) +
    ggplot2::geom_polygon(
      data = rings, ggplot2::aes(x = x, y = y, group = ring),
      fill = NA, color = '#D9D9D9', linewidth = 0.3
    ) +
    ggplot2::geom_segment(
      data = axes, ggplot2::aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
      color = '#D9D9D9', linewidth = 0.3
    ) +
    ggplot2::geom_text(
      data = ring_labels, ggplot2::aes(x = 0.015, y = to_radius(ring), label = label),
      hjust = 0, vjust = -0.3, size = base_size * 0.7 / ggplot2::.pt, color = '#8C8C8C'
    ) +
    ggplot2::geom_polygon(
      data = means,
      ggplot2::aes(x = x, y = y, group = profile, color = profile),
      fill = NA, linewidth = 0.5, key_glyph = 'path'
    ) +
    ggplot2::geom_point(
      data = means, ggplot2::aes(x = x, y = y, color = profile), size = 1.1,
      show.legend = FALSE
    ) +
    ggplot2::geom_text(
      data = axes,
      ggplot2::aes(x = x_label, y = y_label, label = label, hjust = hjust, vjust = vjust),
      size = base_size * 0.9 / ggplot2::.pt, lineheight = 0.9, color = '#171717'
    ) +
    ggplot2::scale_color_manual(
      values = profile_colors, name = NULL, guide = ggplot2::guide_legend(ncol = 2)
    ) +
    ggplot2::scale_fill_manual(values = profile_colors, guide = 'none') +
    # Leave room inside the panel for the spoke labels
    ggplot2::expand_limits(x = c(-1.6, 1.6), y = c(-1.38, 1.38)) +
    ggplot2::coord_equal(clip = 'off') +
    ggplot2::theme_void(base_size = base_size) +
    ggplot2::theme(
      legend.position = 'bottom',
      legend.key.height = grid::unit(base_size * 1.2, 'pt'),
      legend.key.spacing.y = grid::unit(base_size * 0.25, 'pt'),
      plot.margin = ggplot2::margin(5, 5, 5, 5),
      plot.background = ggplot2::element_rect(fill = '#FFFFFF', color = NA)
    )
  return(fig)
}


#' Score a clustering in principal component space
#'
#' @details Computes the total, within-cluster, and between-cluster sums of squares and
#'   the mean silhouette width for a set of cluster labels, using Euclidean distance in
#'   the space of the supplied principal component scores. Rows with missing scores or
#'   labels are dropped.
#'
#' @param pc_matrix (`matrix`) Principal component scores, one row per catchment
#' @param labels (`vector`) Cluster labels, one per row of `pc_matrix`
#'
#' @return Named list with `n_units`, `k`, `tss`, `wss`, `bss`, `pct_explained`
#'   (BSS / TSS), and `mean_silhouette`
#'
#' @importFrom cluster silhouette
#' @importFrom stats complete.cases dist
#' @export
score_clustering <- function(pc_matrix, labels){
  keep <- stats::complete.cases(pc_matrix) & !is.na(labels)
  pc_matrix <- pc_matrix[keep, , drop = FALSE]
  cluster_int <- as.integer(as.factor(labels[keep]))

  global_center <- colMeans(pc_matrix)
  tss <- sum(sweep(pc_matrix, 2, global_center)^2)
  wss <- split(as.data.frame(pc_matrix), cluster_int) |>
    vapply(function(grp){
      grp <- as.matrix(grp)
      sum(sweep(grp, 2, colMeans(grp))^2)
    }, numeric(1)) |>
    sum()
  bss <- tss - wss
  sil <- cluster::silhouette(cluster_int, stats::dist(pc_matrix))
  list(
    n_units = nrow(pc_matrix),
    k = length(unique(cluster_int)),
    tss = tss,
    wss = wss,
    bss = bss,
    pct_explained = bss / tss,
    mean_silhouette = mean(sil[, 'sil_width'])
  )
}
