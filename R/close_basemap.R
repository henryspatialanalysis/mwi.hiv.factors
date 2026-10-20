#' Convert a CSS colour to a hex colour and an opacity
#'
#' @details Handles the forms used in Mapbox GL styles: `#rgb`, `#rrggbb`, `rgb()`,
#'   `rgba()`, `hsl()`, `hsla()`, and R colour names.
#'
#' @param x (`character(1)`) CSS colour
#'
#' @return List with items `col` (`#rrggbb`) and `alpha` (0 to 1)
#'
#' @export
css_color <- function(x){
  x <- gsub('\\s', '', tolower(x))
  nums <- function(s) as.numeric(strsplit(gsub('[^0-9.,]', '', s), ',')[[1]])
  if(grepl('^#[0-9a-f]{3}$', x)){
    x <- paste0('#', paste(rep(strsplit(substring(x, 2), '')[[1]], each = 2), collapse = ''))
  }
  if(grepl('^#[0-9a-f]{6}$', x)) return(list(col = x, alpha = 1))
  if(grepl('^rgba?\\(', x)){
    v <- nums(x)
    return(list(
      col = grDevices::rgb(v[1], v[2], v[3], maxColorValue = 255),
      alpha = if(length(v) > 3) v[4] else 1
    ))
  }
  if(grepl('^hsla?\\(', x)){
    v <- nums(x)
    h <- (v[1] %% 360) / 360
    s <- v[2] / 100
    l <- v[3] / 100
    q <- if(l < 0.5) l * (1 + s) else l + s - l * s
    p <- 2 * l - q
    hue <- function(t){
      t <- t %% 1
      if(t < 1 / 6) return(p + (q - p) * 6 * t)
      if(t < 1 / 2) return(q)
      if(t < 2 / 3) return(p + (q - p) * (2 / 3 - t) * 6)
      return(p)
    }
    rgb <- c(hue(h + 1 / 3), hue(h), hue(h - 1 / 3))
    return(list(
      col = grDevices::rgb(rgb[1], rgb[2], rgb[3]),
      alpha = if(length(v) > 3) v[4] else 1
    ))
  }
  rgb <- grDevices::col2rgb(x)
  return(list(col = grDevices::rgb(rgb[1], rgb[2], rgb[3], maxColorValue = 255), alpha = 1))
}


#' Evaluate a Mapbox GL style property at a zoom level
#'
#' @details Constants are returned as-is. Zoom functions (`list(base, stops)`) are
#'   interpolated exponentially for numbers, as in Mapbox GL, and stepped for strings.
#'
#' @param value Style property value, as parsed by `jsonlite::fromJSON(simplifyVector =
#'   FALSE)`
#' @param zoom (`numeric(1)`) Zoom level
#'
#' @return The property value at `zoom`
#'
#' @export
style_value <- function(value, zoom){
  if(!is.list(value) || is.null(value$stops)) return(value)
  base <- if(is.null(value$base)) 1 else value$base
  z <- vapply(value$stops, function(s) as.numeric(s[[1]]), numeric(1))
  v <- lapply(value$stops, `[[`, 2)
  if(zoom <= z[1]) return(v[[1]])
  if(zoom >= z[length(z)]) return(v[[length(v)]])
  ii <- max(which(z <= zoom))
  if(!is.numeric(v[[ii]])) return(v[[ii]])
  t <- if(base == 1){
    (zoom - z[ii]) / (z[ii + 1] - z[ii])
  } else {
    (base^(zoom - z[ii]) - 1) / (base^(z[ii + 1] - z[ii]) - 1)
  }
  return(v[[ii]] + t * (v[[ii + 1]] - v[[ii]]))
}


#' Evaluate a legacy Mapbox GL filter against vector tile features
#'
#' @details Supports `all`, `any`, `==`, `!=`, `in`, `!in`, `has`, `!has`, `<`, `<=`,
#'   `>`, `>=`, and the `$type` key. Attribute names follow GDAL, which writes
#'   `name:latin` as `name.latin`.
#'
#' @param filter (`list`) Parsed filter, or NULL to keep every feature
#' @param features (`sf`) Features from one source layer
#'
#' @return Logical vector, one value per feature
#'
#' @export
style_filter <- function(filter, features){
  n <- nrow(features)
  if(is.null(filter)) return(rep(TRUE, n))
  geom_type <- c(
    POINT = 'Point', MULTIPOINT = 'Point', LINESTRING = 'LineString',
    MULTILINESTRING = 'LineString', POLYGON = 'Polygon', MULTIPOLYGON = 'Polygon'
  )[as.character(sf::st_geometry_type(features))]
  get_key <- function(key){
    if(key == '$type') return(unname(geom_type))
    col <- gsub(':', '.', key, fixed = TRUE)
    if(col %in% names(features)) return(sf::st_drop_geometry(features)[[col]])
    return(rep(NA, n))
  }
  op <- filter[[1]]
  args <- filter[-1]
  if(op == 'all'){
    return(Reduce(`&`, lapply(args, style_filter, features = features), rep(TRUE, n)))
  }
  if(op == 'any'){
    return(Reduce(`|`, lapply(args, style_filter, features = features), rep(FALSE, n)))
  }
  vals <- get_key(args[[1]])
  targets <- unlist(args[-1])
  out <- switch(
    op,
    '==' = !is.na(vals) & as.character(vals) == as.character(targets[1]),
    '!=' = is.na(vals) | as.character(vals) != as.character(targets[1]),
    'in' = !is.na(vals) & as.character(vals) %in% as.character(targets),
    '!in' = is.na(vals) | !as.character(vals) %in% as.character(targets),
    'has' = !is.na(vals),
    '!has' = is.na(vals),
    '<' = !is.na(vals) & as.numeric(vals) < as.numeric(targets[1]),
    '<=' = !is.na(vals) & as.numeric(vals) <= as.numeric(targets[1]),
    '>' = !is.na(vals) & as.numeric(vals) > as.numeric(targets[1]),
    '>=' = !is.na(vals) & as.numeric(vals) >= as.numeric(targets[1]),
    stop('Unsupported filter operator: ', op)
  )
  return(out)
}


#' Web Mercator tiles covering a bounding box
#'
#' @param bbox (`bbox`) Extent in EPSG:3857
#' @param zoom (`integer(1)`) Tile zoom
#'
#' @return `data.table` with fields `z`, `x`, `y`, `xmin`, `xmax`, `ymin`, `ymax`
#'
#' @export
tile_grid <- function(bbox, zoom){
  half <- 20037508.342789244
  size <- 2 * half / 2^zoom
  xs <- seq(floor((bbox[['xmin']] + half) / size), floor((bbox[['xmax']] + half) / size))
  ys <- seq(floor((half - bbox[['ymax']]) / size), floor((half - bbox[['ymin']]) / size))
  grid <- data.table::CJ(x = xs, y = ys)
  grid[, `:=`(
    z = zoom,
    xmin = -half + x * size, xmax = -half + (x + 1) * size,
    ymax = half - y * size, ymin = half - (y + 1) * size
  )]
  return(grid[])
}


#' Download and read vector tiles covering a bounding box
#'
#' @details Tiles are cached as `{cache_dir}/{z}/{x}/{y}.pbf`. Each tile's features
#'   are clipped to the tile's own bounds, removing the buffer that vector tiles share
#'   with their neighbours, so that features are not drawn twice. The API key is added
#'   to request URLs only and is never printed.
#'
#' @param bbox (`bbox`) Extent in EPSG:3857
#' @param zoom (`integer(1)`) Tile zoom
#' @param source_layers (`character(N)`) Vector tile layers to read
#' @param tile_url (`character(1)`) URL template with `{z}`, `{x}`, `{y}`
#' @param api_key (`character(1)`, default NULL) Key appended as `?api_key=`
#' @param cache_dir (`character(1)`) Tile cache folder
#'
#' @return Named list of `sf` objects in EPSG:3857, one per source layer
#'
#' @export
read_vector_tiles <- function(bbox, zoom, source_layers, tile_url, api_key = NULL, cache_dir){
  keep_cols <- c(
    'class', 'subclass', 'brunnel', 'admin_level', 'maritime', 'rank', 'capital',
    'service', 'ref', 'ref_length', 'network', 'name', 'name.latin'
  )
  grid <- tile_grid(bbox, zoom)
  out <- stats::setNames(vector('list', length(source_layers)), source_layers)
  for(ii in seq_len(nrow(grid))){
    tile <- grid[ii]
    path <- file.path(cache_dir, tile$z, tile$x, paste0(tile$y, '.pbf'))
    if(!file.exists(path)){
      dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
      url <- gsub('{z}', tile$z, gsub('{x}', tile$x, gsub('{y}', tile$y, tile_url,
        fixed = TRUE), fixed = TRUE), fixed = TRUE)
      if(!is.null(api_key) && nzchar(api_key)) url <- paste0(url, '?api_key=', api_key)
      ok <- tryCatch(
        utils::download.file(url, path, mode = 'wb', quiet = TRUE) == 0,
        error = function(e) FALSE, warning = function(w) FALSE
      )
      if(!ok){
        unlink(path)
        stop(sprintf('Could not download vector tile %d/%d/%d', tile$z, tile$x, tile$y))
      }
    }
    opts <- c(paste0('X=', tile$x), paste0('Y=', tile$y), paste0('Z=', tile$z))
    # GDAL prints the open options and feature-count notes for every tile
    utils::capture.output(available <- sf::st_layers(path, options = opts)$name)
    tile_box <- sf::st_as_sfc(sf::st_bbox(
      c(xmin = tile$xmin, ymin = tile$ymin, xmax = tile$xmax, ymax = tile$ymax),
      crs = sf::st_crs(3857)
    ))
    for(layer in intersect(source_layers, available)){
      utils::capture.output(
        feats <- suppressWarnings(sf::st_read(path, layer, options = opts, quiet = TRUE))
      )
      if(nrow(feats) == 0) next
      for(col in setdiff(keep_cols, names(feats))) feats[[col]] <- NA
      feats <- feats[, keep_cols]
      sf::st_crs(feats) <- 3857
      feats <- suppressWarnings(sf::st_intersection(feats, tile_box))
      feats <- feats[!sf::st_is_empty(feats), ]
      if(nrow(feats) > 0) out[[layer]] <- c(out[[layer]], list(feats))
    }
  }
  out <- lapply(out, function(pieces){
    if(length(pieces) == 0) return(NULL)
    return(do.call(rbind, pieces))
  })
  return(out)
}


#' Greedy label placement that drops colliding labels
#'
#' @param labels (`data.table`) Fields `x`, `y`, `half_w`, `half_h` (box half sizes in
#'   map units) and `priority` (higher is placed first)
#' @param bbox (`bbox`) Map extent; labels whose boxes leave it are dropped
#'
#' @return Logical vector of labels to keep, in the input order
#'
#' @export
declutter_labels <- function(labels, bbox){
  ord <- order(-labels$priority)
  keep <- rep(FALSE, nrow(labels))
  boxes <- matrix(numeric(0), ncol = 4)
  for(ii in ord){
    b <- c(
      labels$x[ii] - labels$half_w[ii], labels$x[ii] + labels$half_w[ii],
      labels$y[ii] - labels$half_h[ii], labels$y[ii] + labels$half_h[ii]
    )
    if(b[1] < bbox[['xmin']] || b[2] > bbox[['xmax']] ||
      b[3] < bbox[['ymin']] || b[4] > bbox[['ymax']]) next
    if(nrow(boxes) > 0){
      hit <- boxes[, 1] < b[2] & boxes[, 2] > b[1] & boxes[, 3] < b[4] & boxes[, 4] > b[3]
      if(any(hit)) next
    }
    boxes <- rbind(boxes, b)
    keep[ii] <- TRUE
  }
  return(keep)
}


#' Build basemap layers from a Mapbox GL style and vector tiles
#'
#' @details Reproduces the parts of a Mapbox GL style used by the Close (wtm.frontend)
#'   basemap as ggplot layers: fills, lines, and text labels with halos. Features and
#'   styling for fills and lines are taken at `base_zoom`; labels are read from tiles
#'   at `label_zoom`, which can be higher for more place and road names. Style pixel
#'   sizes are drawn at 96 dpi, scaled by `label_scale` for text. Overlapping labels are
#'   dropped, giving priority to later style layers, as Mapbox GL does.
#'
#' @param style_path (`character(1)`) Mapbox GL style JSON
#' @param below,above (`character(N)`) Style layer ids drawn below and above the data
#' @param bbox (`bbox`) Map extent in EPSG:3857
#' @param width_in (`numeric(1)`) Map width in inches, to convert pixels to map units
#' @param base_zoom,label_zoom (`integer(1)`) Tile zooms for features and for labels
#' @param label_style_zoom (`numeric(1)`, default `label_zoom`) Zoom at which label
#'   layer visibility and text sizes are read from the style. Setting it below
#'   `label_zoom` labels the fuller feature set of deeper tiles with the label layers
#'   of a wider view (the Close style drops village names from zoom 14).
#' @param tile_url,api_key,cache_dir Passed to [read_vector_tiles()]
#' @param label_scale (`numeric(1)`, default 1) Multiplier on text sizes
#' @param line_scale (`numeric(1)`, default 1) Multiplier on line widths; 0.5 draws
#'   features from one zoom level in as if rendered at twice the resolution
#' @param fonts (`list`) Font families for the style's text fonts, with items
#'   `regular`, `italic`, and `semibold`, each a list with `family` and `fontface`
#' @param background (`character(1)`) Map paper colour; Close sets it in CSS, not in
#'   the style
#'
#' @return List with items `background` (hex colour), `below`, `above`, and
#'   `labels` (lists of ggplot layers)
#'
#' @export
close_basemap_layers <- function(
  style_path, below, above, bbox, width_in, base_zoom, label_zoom,
  label_style_zoom = label_zoom, tile_url,
  api_key = NULL, cache_dir, label_scale = 1, line_scale = 1, fonts, background
){
  style <- jsonlite::fromJSON(style_path, simplifyVector = FALSE)
  layers <- stats::setNames(style$layers, vapply(style$layers, `[[`, '', 'id'))
  # Legacy `ref` layers take everything but their paint from the layer they name
  for(id in names(layers)){
    ref <- layers[[id]]$ref
    if(is.null(ref)) next
    inherited <- layers[[ref]][setdiff(names(layers[[ref]]), c('id', 'paint'))]
    layers[[id]] <- utils::modifyList(inherited, layers[[id]][c('id', 'paint')])
  }
  layers <- layers[intersect(names(layers), c(below, above))]
  visible <- function(lyr, zoom){
    (is.null(lyr$minzoom) || zoom >= lyr$minzoom) &&
      (is.null(lyr$maxzoom) || zoom < lyr$maxzoom)
  }
  is_symbol <- vapply(layers, function(l) l$type == 'symbol', logical(1))
  source_of <- function(ids) unique(vapply(layers[ids], `[[`, '', 'source-layer'))
  feature_ids <- names(layers)[!is_symbol & vapply(layers, visible, logical(1), base_zoom)]
  label_ids <- names(layers)[
    is_symbol & vapply(layers, visible, logical(1), label_style_zoom)
  ]
  base_tiles <- read_vector_tiles(
    bbox, base_zoom, source_of(feature_ids), tile_url, api_key, cache_dir
  )
  label_tiles <- read_vector_tiles(
    bbox, label_zoom, source_of(label_ids), tile_url, api_key, cache_dir
  )
  frame <- sf::st_as_sfc(bbox)
  m_per_px <- (bbox[['xmax']] - bbox[['xmin']]) / (width_in * 96)
  px_to_linewidth <- function(px) px / ggplot2::.pt
  px_to_size <- function(px) px * 72.27 / 96 / ggplot2::.pt

  # Fills and lines
  feature_layer <- function(id){
    lyr <- layers[[id]]
    feats <- base_tiles[[lyr[['source-layer']]]]
    if(is.null(feats)) return(NULL)
    feats <- feats[style_filter(lyr$filter, feats), ]
    if(nrow(feats) == 0) return(NULL)
    feats <- suppressWarnings(sf::st_crop(feats, bbox))
    paint <- lyr$paint
    if(lyr$type == 'fill'){
      col <- css_color(style_value(paint[['fill-color']], base_zoom))
      opacity <- style_value(paint[['fill-opacity']], base_zoom)
      alpha <- col$alpha * (if(is.null(opacity)) 1 else opacity)
      outline <- paint[['fill-outline-color']]
      return(ggplot2::geom_sf(
        data = feats, fill = col$col, alpha = alpha,
        colour = if(is.null(outline)) NA else css_color(outline)$col,
        linewidth = if(is.null(outline)) 0 else px_to_linewidth(0.5),
        inherit.aes = FALSE
      ))
    }
    col <- css_color(style_value(paint[['line-color']], base_zoom))
    opacity <- style_value(paint[['line-opacity']], base_zoom)
    width <- style_value(paint[['line-width']], base_zoom)
    return(ggplot2::geom_sf(
      data = feats, colour = col$col,
      alpha = col$alpha * (if(is.null(opacity)) 1 else min(opacity, 1)),
      linewidth = px_to_linewidth((if(is.null(width)) 1 else width) * line_scale),
      linetype = if(is.null(paint[['line-dasharray']])) 'solid' else 'dashed',
      lineend = 'round', inherit.aes = FALSE
    ))
  }
  layer_groups <- lapply(list(below = below, above = above), function(ids){
    ids <- intersect(ids, feature_ids)
    Filter(Negate(is.null), lapply(ids, feature_layer))
  })

  # Labels
  font_for <- function(font){
    if(grepl('Italic', font)) return(fonts$italic)
    if(grepl('Semibold|Bold', font)) return(fonts$semibold)
    return(fonts$regular)
  }
  candidates <- lapply(seq_along(label_ids), function(jj){
    lyr <- layers[[label_ids[jj]]]
    feats <- label_tiles[[lyr[['source-layer']]]]
    if(is.null(feats)) return(NULL)
    feats <- feats[style_filter(lyr$filter, feats), ]
    field <- lyr$layout[['text-field']]
    text <- if(is.character(field) && grepl('{ref}', field, fixed = TRUE)){
      feats$ref
    } else {
      data.table::fifelse(is.na(feats$name.latin), feats$name, feats$name.latin)
    }
    feats$text <- text
    feats <- feats[!is.na(feats$text) & nzchar(feats$text), ]
    if(nrow(feats) == 0) return(NULL)
    if(identical(lyr$layout[['text-transform']], 'uppercase')) feats$text <- toupper(feats$text)
    size_px <- style_value(lyr$layout[['text-size']], label_style_zoom) * label_scale
    placement <- style_value(lyr$layout[['symbol-placement']], label_style_zoom)
    if(identical(placement, 'line')){
      # One label per name, at the middle of its longest merged stretch, along the line
      feats <- feats[sf::st_geometry_type(feats) %in% c('LINESTRING', 'MULTILINESTRING'), ]
      if(nrow(feats) == 0) return(NULL)
      by_name <- split(seq_len(nrow(feats)), feats$text)
      merged <- sf::st_sf(
        text = names(by_name),
        geometry = do.call(c, lapply(by_name, function(idx){
          sf::st_line_merge(
            sf::st_cast(sf::st_union(sf::st_geometry(feats)[idx]), 'MULTILINESTRING')
          )
        })),
        crs = sf::st_crs(feats)
      )
      merged <- suppressWarnings(sf::st_cast(sf::st_cast(merged, 'MULTILINESTRING'),
        'LINESTRING'))
      merged$len <- as.numeric(sf::st_length(merged))
      merged <- merged[order(-merged$len), ]
      merged <- merged[!duplicated(merged$text), ]
      text_len <- nchar(merged$text) * size_px * 0.6 * m_per_px
      merged <- merged[merged$len > text_len, ]
      if(nrow(merged) == 0) return(NULL)
      pts <- lapply(c(0.45, 0.5, 0.55), function(f){
        sf::st_coordinates(sf::st_line_sample(merged, sample = f))[, 1:2, drop = FALSE]
      })
      angle <- atan2(pts[[3]][, 2] - pts[[1]][, 2], pts[[3]][, 1] - pts[[1]][, 1]) * 180 / pi
      angle <- ifelse(angle > 90, angle - 180, ifelse(angle < -90, angle + 180, angle))
      out <- data.table::data.table(
        x = pts[[2]][, 1], y = pts[[2]][, 2], text = merged$text, angle = angle,
        within = merged$len
      )
    } else {
      xy <- sf::st_coordinates(suppressWarnings(sf::st_point_on_surface(feats)))
      out <- data.table::data.table(
        x = xy[, 1], y = xy[, 2], text = feats$text, angle = 0,
        within = -as.numeric(feats$rank)
      )
    }
    paint <- lyr$paint
    halo <- paint[['text-halo-width']]
    out[, `:=`(
      layer = label_ids[jj],
      size_px = size_px,
      # Mapbox GL defaults: black text, no halo
      colour = css_color(if(is.null(paint[['text-color']])) '#000000' else
        style_value(paint[['text-color']], label_style_zoom))$col,
      halo_colour = css_color(if(is.null(paint[['text-halo-color']])) '#ffffff' else
        paint[['text-halo-color']])$col,
      halo_px = if(is.null(halo)) 0 else halo,
      font = lyr$layout[['text-font']][[1]],
      # Later style layers win collisions; within a layer, larger places and longer
      #  roads first
      priority = jj * 1e6 + data.table::frank(within, na.last = FALSE)
    )]
    return(out)
  }) |> data.table::rbindlist()
  label_layers <- list()
  if(nrow(candidates) > 0){
    candidates <- candidates[
      x >= bbox[['xmin']] & x <= bbox[['xmax']] & y >= bbox[['ymin']] & y <= bbox[['ymax']]
    ]
    rad <- candidates$angle * pi / 180
    w <- (nchar(candidates$text) * 0.3 * candidates$size_px + candidates$halo_px) * m_per_px
    h <- (0.6 * candidates$size_px + candidates$halo_px) * m_per_px
    candidates[, `:=`(
      half_w = abs(w * cos(rad)) + abs(h * sin(rad)),
      half_h = abs(w * sin(rad)) + abs(h * cos(rad))
    )]
    candidates <- candidates[declutter_labels(candidates, bbox)]
    # One text layer per style layer, drawn in style order
    for(id in intersect(label_ids, unique(candidates$layer))){
      sub <- candidates[layer == id]
      f <- font_for(sub$font[1])
      text_aes <- ggplot2::aes(x = x, y = y, label = text, angle = angle)
      label_layers[[length(label_layers) + 1]] <- if(sub$halo_px[1] > 0){
        shadowtext::geom_shadowtext(
          data = sub, mapping = text_aes,
          colour = sub$colour[1], bg.colour = sub$halo_colour[1],
          bg.r = sub$halo_px[1] / sub$size_px[1] * 0.75,
          size = px_to_size(sub$size_px[1]), family = f$family, fontface = f$fontface,
          inherit.aes = FALSE
        )
      } else {
        ggplot2::geom_text(
          data = sub, mapping = text_aes, colour = sub$colour[1],
          size = px_to_size(sub$size_px[1]), family = f$family, fontface = f$fontface,
          inherit.aes = FALSE
        )
      }
    }
  }
  return(list(
    background = css_color(background)$col,
    below = layer_groups$below,
    above = layer_groups$above,
    labels = label_layers
  ))
}
