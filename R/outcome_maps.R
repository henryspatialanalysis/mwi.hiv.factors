#' Web Mercator bounding box of a study area with a buffer
#'
#' @param area_sf (`sf`) Study area
#' @param buffer (`numeric(1)`) Share of the larger bounding box side added on each side
#'
#' @return `bbox` in EPSG:3857
#'
#' @export
map_bbox <- function(area_sf, buffer){
  bbox <- sf::st_bbox(sf::st_transform(area_sf, 3857))
  pad <- buffer * max(bbox$xmax - bbox$xmin, bbox$ymax - bbox$ymin)
  bbox[c('xmin', 'ymin')] <- bbox[c('xmin', 'ymin')] - pad
  bbox[c('xmax', 'ymax')] <- bbox[c('xmax', 'ymax')] + pad
  return(bbox)
}


#' Tile zoom at which a map extent matches its printed width
#'
#' @details The zoom at which one Web Mercator pixel equals one 96 dpi pixel of the
#'   printed map, rounded to the nearest level.
#'
#' @param bbox (`bbox`) Map extent in EPSG:3857
#' @param width_in (`numeric(1)`) Printed map width in inches
#'
#' @return `integer(1)`
#'
#' @export
map_zoom <- function(bbox, width_in){
  m_per_px <- (bbox[['xmax']] - bbox[['xmin']]) / (width_in * 96)
  return(as.integer(round(log2(156543.03 / m_per_px))))
}


#' Register the fonts used by the Close basemap
#'
#' @details Figtree Regular and SemiBold come from the installed Figtree font; the
#'   italic face is registered from a separate file under a new family name, so that
#'   nothing is added to the system font folder. Use the `ragg` device to draw them.
#'
#' @param italic_path (`character(1)`) Path to Figtree Italic (.ttf)
#'
#' @return List of fonts for [close_basemap_layers()]
#'
#' @export
register_basemap_fonts <- function(italic_path){
  italic_path <- path.expand(italic_path)
  if(!'Figtree Italic' %in% systemfonts::registry_fonts()$family){
    systemfonts::register_font(name = 'Figtree Italic', plain = italic_path)
  }
  return(list(
    regular = list(family = 'Figtree', fontface = 'plain'),
    italic = list(family = 'Figtree Italic', fontface = 'plain'),
    semibold = list(family = 'Figtree', fontface = 'bold')
  ))
}


#' Map of a raster surface or survey points over the Close basemap
#'
#' @details Layers, bottom to top: the basemap's lower layers (land cover and
#'   residential areas), the outcome raster, the basemap's upper layers (water, roads,
#'   railways, buildings), survey points, the district outline, the city outline, and
#'   the basemap labels. Line widths are given in pixels at 96 dpi.
#'
#' @param basemap (`list`) Output of [close_basemap_layers()]
#' @param district_sf,city_sf (`sf`) Outlines
#' @param fill_scale (`ScaleContinuous`) ggplot fill scale for the outcome
#' @param surface (`SpatRaster`, default NULL) One-layer outcome raster
#' @param points (`sf`, default NULL) Points with fields `observed` and `samplesize`
#' @param bbox (`bbox`) Map extent in EPSG:3857
#' @param raster_alpha (`numeric(1)`) Opacity of the outcome raster
#' @param district_line,city_line (`list`) Items `width_px`, `color`, `linetype`
#' @param title (`character(1)`) Map title
#' @param caption (`character(1)`, default NULL) Caption, e.g. the basemap attribution
#'
#' @return ggplot object
#'
#' @import ggplot2
#' @export
outcome_basemap_map <- function(
  basemap, district_sf, city_sf, fill_scale, surface = NULL, points = NULL, bbox,
  raster_alpha, district_line, city_line, title, caption = NULL
){
  # grid line widths are in 1/96 inch, so a pixel at 96 dpi is one lwd unit
  px_to_linewidth <- function(px) px / ggplot2::.pt
  fig <- ggplot2::ggplot() + basemap$below
  if(!is.null(surface)){
    # Only cells with values are drawn, so the grid outside the study area stays clear
    surface_3857 <- terra::project(surface, 'EPSG:3857', method = 'near')
    cells <- terra::as.data.frame(surface_3857, xy = TRUE, na.rm = TRUE)
    names(cells) <- c('x', 'y', 'value')
    fig <- fig + ggplot2::geom_raster(
      data = cells, ggplot2::aes(x = x, y = y, fill = value), alpha = raster_alpha
    )
  }
  fig <- fig + basemap$above
  if(!is.null(points)){
    pts <- sf::st_transform(points, 3857)
    fig <- fig + ggplot2::geom_sf(
      data = pts, ggplot2::aes(fill = observed, size = samplesize),
      shape = 21, color = 'grey20', stroke = 0.3
    ) +
      ggplot2::scale_size_area(max_size = 4, name = 'Sample size')
  }
  fig <- fig +
    ggplot2::geom_sf(
      data = sf::st_transform(district_sf, 3857), fill = NA,
      color = district_line$color, linetype = district_line$linetype,
      linewidth = px_to_linewidth(district_line$width_px)
    ) +
    ggplot2::geom_sf(
      data = sf::st_transform(city_sf, 3857), fill = NA,
      color = city_line$color, linetype = city_line$linetype,
      linewidth = px_to_linewidth(city_line$width_px)
    ) +
    basemap$labels +
    fill_scale +
    ggplot2::coord_sf(
      crs = 3857, xlim = bbox[c('xmin', 'xmax')], ylim = bbox[c('ymin', 'ymax')],
      expand = FALSE, datum = NA
    ) +
    ggplot2::labs(title = title, caption = caption, x = NULL, y = NULL) +
    ggplot2::theme_void(base_family = 'Figtree') +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = 'bold', size = 12),
      plot.caption = ggplot2::element_text(size = 7, color = 'grey40'),
      plot.background = ggplot2::element_rect(fill = 'white', color = NA),
      panel.background = ggplot2::element_rect(fill = basemap$background, color = NA),
      legend.key = ggplot2::element_rect(fill = 'white', color = NA),
      legend.position = 'right',
      plot.margin = ggplot2::margin(6, 6, 6, 6)
    )
  return(fig)
}
