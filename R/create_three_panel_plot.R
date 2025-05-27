
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
  outcome_colors = viridis::viridis(n = 100), col_lims = c(0, 1)
){
  # Prepare input data
  catchments_with_covs$THIS_COV <- catchments_with_covs[[cov_field]]
  cov_data <- data.table::copy(cov_data)
  cov_data$THIS_COV <- cov_data[[cov_field]]
  cov_data$VIRAEMIA <- cov_data[[viraemia_field]]
  cov_data$POPULATION <- cov_data[[pop_field]]

  # Plot 1: Malawi map
  p1 <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = catchments_with_covs,
      mapping = ggplot2::aes(fill = THIS_COV),
      color = NA,
      linewidth = 0L
    ) +
    ggplot2::geom_sf(data = district_bounds, fill = NA, color = 'black', linewidth = 0.5) +
    ggplot2::scale_fill_gradientn(
      colors = outcome_colors, limits = col_lims, oob = scales::squish
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank()
    ) +
    ggplot2::labs(fill = cov_label |> gsub(pattern = ' ', replacement = '\n'))

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
    ggplot2::labs(x = cov_label, y = 'Count') +
    ggplot2::theme_minimal()

  # Plot 3: Scatterplot with viraemia as the outcome
  p3 <- ggplot2::ggplot(data = cov_data) +
    ggplot2::geom_point(
      mapping = ggplot2::aes(
        x = THIS_COV, y = VIRAEMIA, size = POPULATION, color = THIS_COV
      )
    ) +
    ggplot2::scale_size_continuous(guide = 'none') +
    ggplot2::scale_color_gradientn(
      colors = outcome_colors, limits = col_lims, oob = scales::squish,
      guide = 'none'
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(x = cov_label, y = 'HIV Viraemia') +
    ggplot2::theme_minimal()

  # Assemble onto a single page
  gridExtra::grid.arrange(
    p1, p2, p3,
    ncol = 2, nrow = 2, layout_matrix = rbind(c(1, 2), c(1, 3)),
    top = plot_title
  )
  invisible(NULL)
}
