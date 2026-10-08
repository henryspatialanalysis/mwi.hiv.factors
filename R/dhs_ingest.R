#' Parse a CSPro data dictionary
#'
#' @details Reads a CSPro `.dcf` dictionary, as distributed with DHS hierarchical flat
#'   files, into item positions and value labels. Only the fields needed to extract
#'   single-occurrence items are kept.
#'
#' @param dcf_path (`character(1)`) Path to the `.dcf` dictionary file
#'
#' @return A list with two [data.table::data.table]s:
#'   - `items`: one row per item, with fields `record_name`, `record_type`,
#'     `max_records`, `name`, `label`, `start`, `len`, `data_type`, `decimal`, and
#'     `occurrences`. Items listed under `[IdItems]` have `record_type` NA.
#'   - `value_labels`: one row per labelled value, with fields `name`, `value`, `label`
#'
#' @importFrom data.table data.table rbindlist
#' @export
read_cspro_dictionary <- function(dcf_path){
  lines <- readLines(dcf_path, warn = FALSE, encoding = 'UTF-8')
  # Older DHS dictionaries are Latin-1 encoded
  not_utf8 <- !validUTF8(lines)
  lines[not_utf8] <- iconv(lines[not_utf8], from = 'latin1', to = 'UTF-8')
  lines <- sub('^\ufeff', '', sub('\r$', '', lines))

  items <- list()
  value_labels <- list()
  section <- ''
  record <- list(name = NA_character_, type = NA_character_, max = NA_integer_)
  this_item <- NULL
  this_vs_item <- NULL

  flush_item <- function(){
    if(!is.null(this_item)) items[[length(items) + 1]] <<- this_item
    this_item <<- NULL
  }
  for(line in lines){
    if(grepl('^\\[.*\\]$', line)){
      flush_item()
      section <- line
      if(section == '[IdItems]'){
        record <- list(name = NA_character_, type = NA_character_, max = NA_integer_)
      } else if(section == '[Record]'){
        record <- list(name = NA_character_, type = NA_character_, max = 1L)
      } else if(section == '[Item]'){
        this_item <- list(
          record_name = record$name, record_type = record$type, max_records = record$max,
          name = NA_character_, label = NA_character_, start = NA_integer_,
          len = NA_integer_, data_type = 'Numeric', decimal = 0L, occurrences = 1L
        )
      }
      next
    }
    if(!grepl('=', line, fixed = TRUE)) next
    key <- sub('=.*$', '', line)
    val <- sub('^[^=]*=', '', line)
    if(section == '[Record]'){
      if(key == 'Name') record$name <- val
      if(key == 'RecordTypeValue') record$type <- gsub("'", '', val)
      if(key == 'MaxRecords') record$max <- as.integer(val)
    } else if(section == '[Item]' && !is.null(this_item)){
      if(key == 'Name') this_item$name <- val
      if(key == 'Label') this_item$label <- val
      if(key == 'Start') this_item$start <- as.integer(val)
      if(key == 'Len') this_item$len <- as.integer(val)
      if(key == 'DataType') this_item$data_type <- val
      if(key == 'Decimal') this_item$decimal <- as.integer(val)
      if(key == 'Occurrences') this_item$occurrences <- as.integer(val)
      # The value set follows its item; remember which item it belongs to
      if(key == 'Name') this_vs_item <- val
    } else if(section == '[ValueSet]'){
      if(key == 'Value' && grepl(';', val, fixed = TRUE)){
        value_labels[[length(value_labels) + 1]] <- list(
          name = this_vs_item,
          value = sub(';.*$', '', val),
          label = sub('^[^;]*;', '', val)
        )
      }
    }
  }
  flush_item()

  return(list(
    items = data.table::rbindlist(items),
    value_labels = data.table::rbindlist(value_labels)
  ))
}


#' Read selected items from a hierarchical CSPro flat file
#'
#' @details DHS distributes some recode files as hierarchical fixed-width CSPro data, in
#'   which each line is one record and the record type sits at a fixed position. This
#'   function extracts single-occurrence items from single-occurrence records, merging
#'   records on the level ID (for example `CASEID` for a woman).
#'
#' @param dat_path (`character(1)`) Path to the `.dat` file
#' @param dictionary (`list`) Output of [read_cspro_dictionary()]
#' @param item_names (`character(N)`) Items to extract; matching is case-insensitive
#' @param id_item (`character(1)`) ID item shared by all records of one case
#' @param type_start (`integer(1)`, default 16) Start position of the record type
#' @param type_len (`integer(1)`, default 3) Length of the record type
#'
#' @return [data.table::data.table] with one row per case, the ID column, and one
#'   column per item, named in lower case. Numeric items are returned as numeric with
#'   blanks set to NA.
#'
#' @importFrom data.table data.table merge.data.table
#' @export
read_cspro_items <- function(
  dat_path, dictionary, item_names, id_item, type_start = 16L, type_len = 3L
){
  dict <- dictionary$items
  id_def <- dict[toupper(name) == toupper(id_item) & is.na(record_type), ][1, ]
  if(is.na(id_def$start)) stop("ID item ", id_item, " not found among the ID items")
  # Restrict to the level that owns the ID item: items after it in the dictionary and
  #  before the next level's ID item
  id_rows <- which(is.na(dict$record_type))
  first_row <- which(toupper(dict$name) == toupper(id_item) & is.na(dict$record_type))[1]
  next_ids <- id_rows[id_rows > first_row]
  last_row <- if(length(next_ids) > 0) min(next_ids) - 1L else nrow(dict)
  level_dict <- dict[seq(first_row + 1L, last_row), ]

  sel <- level_dict[toupper(name) %in% toupper(item_names), ]
  missing_items <- setdiff(toupper(item_names), toupper(sel$name))
  if(length(missing_items) > 0){
    warning("Items not found: ", paste(missing_items, collapse = ', '))
  }
  bad <- sel[max_records > 1 | occurrences > 1, name]
  if(length(bad) > 0) stop("Multiple-occurrence items are not supported: ", bad)

  lines <- readLines(dat_path, warn = FALSE, encoding = 'UTF-8')
  lines[1] <- sub('^\ufeff', '', lines[1])
  rec_types <- substr(lines, type_start, type_start + type_len - 1L)

  out <- NULL
  for(this_type in unique(sel$record_type)){
    rec_lines <- lines[rec_types == this_type]
    rec_items <- sel[record_type == this_type, ]
    rec_dt <- data.table::data.table(
      id = trimws(substr(rec_lines, id_def$start, id_def$start + id_def$len - 1L))
    )
    for(ii in seq_len(nrow(rec_items))){
      it <- rec_items[ii, ]
      vals <- trimws(substr(rec_lines, it$start, it$start + it$len - 1L))
      if(it$data_type != 'Alpha'){
        vals <- suppressWarnings(as.numeric(vals))
        if(it$decimal > 0) vals <- vals / 10^it$decimal
      }
      data.table::set(rec_dt, j = tolower(it$name), value = vals)
    }
    out <- if(is.null(out)) rec_dt else merge(out, rec_dt, by = 'id', all = TRUE)
  }
  data.table::setnames(out, 'id', tolower(id_item))
  return(out)
}


#' Look up value labels for a CSPro item
#'
#' @param dictionary (`list`) Output of [read_cspro_dictionary()]
#' @param item_name (`character(1)`) Item name; matching is case-insensitive
#'
#' @return Named character vector of labels, with values as names
#'
#' @export
cspro_value_labels <- function(dictionary, item_name){
  vl <- dictionary$value_labels[toupper(name) == toupper(item_name), ]
  vl <- vl[!duplicated(value), ]
  return(stats::setNames(vl$label, vl$value))
}


#' Load DHS cluster GPS points
#'
#' @details Reads a DHS GE shapefile and drops clusters with missing coordinates, which
#'   DHS codes as (0, 0).
#'
#' @param shp_path (`character(1)`) Path to the GE `.shp` file
#'
#' @return [sf][sf::sf] points with fields `cluster`, `urban` (logical), `dhs_district`,
#'   `dhs_region`
#'
#' @importFrom sf st_read
#' @export
read_dhs_clusters <- function(shp_path){
  ge <- sf::st_read(shp_path, quiet = TRUE)
  ge <- ge[!(ge$LATNUM == 0 & ge$LONGNUM == 0), ]
  out <- sf::st_sf(
    cluster = as.integer(ge$DHSCLUST),
    urban = ge$URBAN_RURA == 'U',
    dhs_district = ge$ADM1NAME,
    dhs_region = ge$DHSREGNA,
    geometry = sf::st_geometry(ge)
  )
  return(out)
}
