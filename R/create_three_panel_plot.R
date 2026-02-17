

#' Create a map of covariate values by health facility catchment
#'
#' @param catchments_with_covs ([sf::sf] object) Polygons merged with covariate data, must contain column `THIS_COV` for plotting
#' @param district_bounds ([sf::sf] object) District boundaries to plot over catchment polygons
#' @param outcome_colors (`character(N)`) Vector of colors for filling the covariate map
#' @param col_lims (`numeric(2)`) Numeric vector of length two specifying color scale limits (min, max)
#' @param log_scale (`logical(1)`) Whether to use a log transformation for the color scale (`TRUE` or `FALSE`)
#' @param cov_label (`character(1)`) Label for the covariate to use in the legend
#'
#' @return A [ggplot2::ggplot] object showing the covariate map for catchments
#' @import ggplot2
#' @importFrom scales squish comma
#' @export
create_covariate_map <- function(
  catchments_with_covs,
  district_bounds,
  outcome_colors,
  col_lims,
  log_scale,
  cov_label
){
  map_fig <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = catchments_with_covs,
      mapping = ggplot2::aes(fill = THIS_COV),
      color = NA,
      linewidth = 0L
    ) +
    ggplot2::geom_sf(data = district_bounds, fill = NA, color = 'black', linewidth = 0.5) +
    ggplot2::scale_fill_gradientn(
      colors = outcome_colors,
      limits = col_lims,
      labels = scales::comma,
      oob = scales::squish,
      trans = if(log_scale) 'log1p' else 'identity'
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      fill = cov_label |> gsub(pattern = ' ', replacement = '\n')
    )
  return(map_fig)
}

#' Create three-panel plots for a covariate
#'
#' @param catchments_with_covs ([sf::sf] object) Polygons merged with covariate data
#' @param cov_data ([data.table::data.table]) Table containing at least fields with the
#'   covariate data, viraemia, and population
#' @param district_bounds ([sf::sf] object) District boundaries to plot over catchments
#' @param cov_field (`character(1)`) Name of the covariate field in data objects
#' @param viraemia_field (`character(1)`) Name of the viraemia field in `cov_data`
#' @param pop_field (`character(1)`) Name of the population field in `cov_data`
#' @param plot_title (`character(1)`) Title for the overall plot
#' @param cov_label (`character(1)`) Label for the covariate
#' @param hist_num_breaks (`integer(1)`, default 30) Number of breaks for the histogram
#' @param outcome_colors (`character(N)`, default `viridis::viridis(n = 100)`) Colors for
#'   the outcome variable
#' @param col_lims (`numeric(2)`, default `c(0, 1)`) Color limits for the outcome variable
#' @param log_scale (`logical(1)`, default `FALSE`) Whether to plot the outcome variable
#'   on a log scale
#' @param vlines (`data.frame`, default `NULL`) Data.frame with two fields: `label` and
#'   `x`. Each row is a vertical line to add to the plot.
#'
#' @return Plots the three panel plot to the active graphics device, returns NULL
#'
#' @import ggplot2 scales viridis data.table
#' @importFrom gridExtra grid.arrange
#' @importFrom graphics hist
#' @export
create_three_panel_plot <- function(
  catchments_with_covs, cov_data, district_bounds, cov_field, viraemia_field, pop_field,
  plot_title, cov_label, hist_num_breaks = 30,
  outcome_colors = viridis::viridis(n = 100), col_lims = c(0, 1), log_scale = FALSE,
  vlines = NULL
){
  # Prepare input data
  catchments_with_covs$THIS_COV <- catchments_with_covs[[cov_field]]
  cov_data <- data.table::copy(cov_data)
  cov_data$THIS_COV <- cov_data[[cov_field]]
  cov_data$VIRAEMIA <- cov_data[[viraemia_field]]
  cov_data$POPULATION <- cov_data[[pop_field]]

  p1 <- create_covariate_map(
    catchments_with_covs = catchments_with_covs,
    district_bounds = district_bounds,
    outcome_colors = outcome_colors,
    col_lims = col_lims,
    log_scale = log_scale,
    cov_label = cov_label
  )

  # Plot 2: Histogram
  hist_model <- graphics::hist(cov_data$THIS_COV, breaks = hist_num_breaks, plot = FALSE)
  cov_data$outcome_grp <- cut(
    x = cov_data$THIS_COV,
    breaks = hist_model$breaks,
    labels = hist_model$mids
  ) |> as.character() |> as.numeric()
  hist_data <- cov_data[, .(count = .N), by = outcome_grp ]
  p2 <- ggplot2::ggplot(data = hist_data) +
    ggplot2::geom_col(
      mapping = ggplot2::aes(x = outcome_grp, y = count, fill = outcome_grp),
      color = '#888888', linewidth = 0.15,
      width = diff(hist_model$breaks)[1]
    ) +
    ggplot2::scale_fill_gradientn(
      colors = outcome_colors, limits = col_lims, oob = scales::squish,
      guide = 'none'
    ) +
    ggplot2::labs(x = cov_label, y = 'Count', color = '') +
    ggplot2::theme_minimal()

  # Plot 3: Scatterplot with viraemia as the outcome
  p3 <- ggplot2::ggplot(data = cov_data) +
    ggplot2::geom_point(
      pch = 21,
      mapping = ggplot2::aes(
        x = THIS_COV, y = VIRAEMIA, size = POPULATION, fill = THIS_COV
      ), stroke = 0.25
    ) +
    ggplot2::scale_size_continuous(guide = 'none') +
    ggplot2::scale_fill_gradientn(
      colors = outcome_colors, limits = col_lims, oob = scales::squish,
      guide = 'none'
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(x = cov_label, y = 'HIV Viraemia', color = '') +
    ggplot2::theme_minimal()

  if(!is.null(vlines)){
    # Create custom color palette for vertical lines
    all_line_colors <- c(
      "#A65628", "#377EB8", "#F781BF", "#E41A1C", "#4DAF4A", "#FFFF33",
      "#984EA3", "#FF7F00", "#999999"
    )
    unique_labels <- sort(unique(vlines$label))
    line_colors <- all_line_colors[seq_along(unique_labels)]
    names(line_colors) <- unique_labels
    # Add to the histogram and scatter plots
    p2 <- p2 +
      ggplot2::geom_vline(
        data = vlines,
        mapping = ggplot2::aes(xintercept = x, color = label),
        linetype = 2
      ) +
      ggplot2::scale_color_manual(values = line_colors) +
      ggplot2::theme(legend.position = 'bottom')
    p3 <- p3 +
      ggplot2::geom_vline(
        data = vlines,
        mapping = ggplot2::aes(xintercept = x, color = label),
        linetype = 2
      ) +
      ggplot2::scale_color_manual(values = line_colors, guide = 'none')
  }

  # Assemble onto a single page
  gridExtra::grid.arrange(
    p1, p2, p3,
    ncol = 2, nrow = 2, layout_matrix = rbind(c(1, 2), c(1, 3)),
    top = plot_title
  )
  invisible(NULL)
}
