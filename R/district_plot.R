#' Function to shorten a facility name
#'
#' @param facility_name (`character(1)`) Facility name to shorten
#'
#' @return (`character(1)`) Shortened facility name
#'
#' @export
short_label <- function(facility_name){
  facility_name |>
    gsub(pattern = "District Hospital", replacement = "Dist. Hosp.") |>
    gsub(pattern = " Area [0-9]+", replacement = "") |>
    gsub(pattern = " Health Centre", replacement = "") |>
    gsub(pattern = " Rural Hospital", replacement = "") |>
    gsub(pattern = " Mission Hospital", replacement = "") |>
    gsub(pattern = " Health Post", replacement = "") |>
    gsub(pattern = " Hospital", replacement = "") |>
    gsub(pattern = " Clinic", replacement = "") |>
    gsub(pattern = " Mission", replacement = "") |>
    gsub(pattern = " Community", replacement = "") |>
    gsub(pattern = " Anglican", replacement = "") |>
    gsub(pattern = " Rural Growth", replacement = "") |>
    gsub(pattern = " Services", replacement = "") |>
    gsub(pattern = " Lilongwe", replacement = "") |>
    gsub(pattern = "Lilongwe City Assembly", replacement = "") |>
    gsub(pattern = " Services", replacement = "") |>
    gsub(pattern = " Centre Of Excellence In Malawi", replacement = "") |>
    gsub(pattern = " (Child Legacy)", replacement = "", fixed = TRUE) |>
    gsub(pattern = "Partners In Hope", replacement = "PIH", ignore.case = TRUE)
}

#' Set up letters to map up to 78 facilities
#'
#' @return (`character(1)`) Letters to map up to 78 facilities
#'
#' @export
letters_ref <- function(){
  return(c(LETTERS, paste0(LETTERS, LETTERS), paste0(LETTERS, LETTERS, LETTERS)))
}

#' Function to plot a district map
#'
#' @param district_name (`character(1)`) District name
#' @param districts_sf (`sf::sf`) Districts shapefile containing at least the field
#'   "area_name"
#' @param catchments_sf (`sf::sf`) Catchments shapefile containing at least the field
#'   "cluster_name"
#' @param catchment_colors (`character(N)`) Catchment color scheme for ggplot,
#'   corresponding to the "cluster_name" field in `catchments_sf`
#' @param facility_data (`data.table::data.table`) Facility data including the following
#'   fields:
#'   - facility_label (`character(1)`) Facility name
#'   - latitude (`numeric(1)`) Latitude
#'   - longitude (`numeric(1)`) Longitude
#' @param out_fp (`character(1)`) Output file path
#'
#' @return Returns NULL; saves district map to `out_fp`
#'
#' @import ggplot2 data.table
#' @importFrom sf st_as_sf st_intersects st_crs st_bbox
#' @importFrom ggrepel geom_label_repel
#' @importFrom grDevices png dev.off
#' @export
district_plot <- function(
  district_name, districts_sf, catchments_sf, catchment_colors, facility_data, out_fp
){
  # Determine the figure dimensions
  this_district_sf <- districts_sf[districts_sf$area_name == district_name, ]
  other_districts_sf <- districts_sf[districts_sf$area_name != district_name, ]
  district_bbox <- sf::st_bbox(this_district_sf)
  if((diff(district_bbox[c(1, 3)]) + .4) > (diff(district_bbox[c(2, 4)]) + .1)){
    # Orient as a landscape
    image_width <- 31.1
    image_height <- 21.4
  } else {
    # Orient as a portrait
    image_width <- 21.4
    image_height <- 31.1
  }

  # Create the dataset for repelled labels
  repel_set <- data.table::CJ(
    longitude = seq(district_bbox[1], district_bbox[3], length.out = 200),
    latitude = seq(district_bbox[2], district_bbox[4], length.out = 200),
    facility_label = ''
  )
  # Keep only points that intersect the district polygon
  repel_keep_index <- repel_set |>
    sf::st_as_sf(
      coords = c('longitude', 'latitude'),
      crs = sf::st_crs(this_district_sf)
    ) |>
    sf::st_intersects(y = this_district_sf, sparse = FALSE) |>
    as.logical() |>
    which()
  repel_set <- repel_set[repel_keep_index, ]
  repel_set_full <- data.table::rbindlist(
    list(repel_set, facility_data[, .(longitude, latitude, facility_label)]),
    use.names = TRUE
  )

  # Build map object
  district_map_no_labels <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = catchments_sf,
      ggplot2::aes(fill = cluster_name),
      color = '#666666', linewidth = 0.15
    ) +
    ggplot2::geom_sf(data = other_districts_sf, fill = '#ffffff', color = NA, alpha = 0.5) +
    ggplot2::geom_sf(data = districts_sf, fill = NA, color = '#444444', linewidth = 0.2) +
    ggplot2::geom_sf(data = this_district_sf, fill = NA, color = '#222222', linewidth = 0.5) +
    ggplot2::geom_point(
      data = facility_data, ggplot2::aes(x = longitude, y = latitude), shape = 23, color = 'white',
      fill = '#3333ff', size = 4, stroke = 0.5
    ) +
    ggplot2::scale_fill_manual(values = catchment_colors) +
    ggplot2::labs(
      title = paste('Clustering results for', district_name),
      subtitle = 'Priority facility locations shown in blue',
      fill = 'Cluster'
    ) +
    ggplot2::lims(
      x = district_bbox[c(1, 3)] + c(-.2, .2),
      y = district_bbox[c(2, 4)] + c(-.05, .05)
    ) +
    ggplot2::guides(color = "none") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 24),
      plot.subtitle = ggplot2::element_text(size = 16),
      legend.title = ggplot2::element_text(size = 16),
      legend.text = ggplot2::element_text(size = 12)
    )
  box_padding <- if(district_name == "Lilongwe") 0.05 else 0.1
  district_map <- district_map_no_labels +
    ggrepel::geom_label_repel(
      data = repel_set_full,
      aes(x = longitude, y = latitude, label = facility_label),
      color = '#3333ff', fill = ggplot2::alpha('white', 0.75),
      box.padding = unit(box_padding, 'in'), force = 2.0, force_pull = 0.1,
      max.overlaps = Inf, max.time = 10, max.iter = 1e5
    )

  png(
    gsub(pattern = '.png', replacement = '_no_labels.png', x = out_fp, fixed = TRUE),
    height = image_height / 2, width = image_width / 2, units = 'in', res = 600
  )
  print(district_map_no_labels)
  dev.off()
  png(
    out_fp,
    height = image_height / 2, width = image_width / 2, units = 'in', res = 600
  )
  print(district_map)
  dev.off()
}
