#' Add a formatted table with grouped column headers to a workbook
#'
#' @details Writes a title, one or more header rows, and a character body. In the header
#'   and in body columns after the first, an NA cell following a filled cell in the same
#'   row is merged into it, so a header can span the columns beneath it and a single
#'   value (such as an odds ratio) can span a pair of estimate columns. In body columns
#'   after the first, whole-number cells are written as numbers and cells reading
#'   `=NA()` as that formula. Section rows are bold; other body rows are indented in the
#'   first column. Uses the in-place method form of openxlsx2, whose functional
#'   `wb_*()` helpers return modified clones.
#'
#' @param wb (`wbWorkbook`) Workbook, modified in place
#' @param sheet (`character(1)`) New worksheet name
#' @param title (`character(1)`) Title written in the first row
#' @param header (`matrix`) Character header rows, one column per table column
#' @param body (`matrix`) Character body, one column per table column
#' @param section_rows (`integer(N)`, default NULL) Body rows holding section labels
#' @param col_widths (`numeric(N)`) Column widths, one per table column
#'
#' @return The workbook, invisibly
#'
#' @export
add_table_sheet <- function(
  wb, sheet, title, header, body, section_rows = NULL, col_widths
){
  header <- as.matrix(header)
  body <- as.matrix(body)
  n_col <- ncol(body)
  first_header_row <- 3L
  first_body_row <- first_header_row + nrow(header)
  last_row <- first_body_row + nrow(body) - 1L
  cell <- function(row, col) paste0(openxlsx2::int2col(col), row)
  span <- function(row, c1, c2) paste0(cell(row, c1), ':', cell(row, c2))

  wb$add_worksheet(sheet, grid_lines = FALSE)
  wb$add_data(sheet = sheet, x = title, dims = 'A1', col_names = FALSE)
  wb$add_font(sheet = sheet, dims = 'A1', bold = TRUE, size = 13)

  # Merge each filled cell with the empty cells that follow it in the same row
  merge_runs <- function(values, excel_row, from_col){
    for(cc in seq(from_col, n_col)){
      if(is.na(values[cc])) next
      end <- cc
      while(end < n_col && is.na(values[end + 1L])) end <- end + 1L
      if(end > cc) wb$merge_cells(sheet = sheet, dims = span(excel_row, cc, end))
    }
  }
  for(ii in seq_len(nrow(header))){
    excel_row <- first_header_row + ii - 1L
    wb$add_data(
      sheet = sheet, x = t(header[ii, ]), dims = cell(excel_row, 1L),
      col_names = FALSE, na.strings = ''
    )
    merge_runs(header[ii, ], excel_row, 2L)
  }
  header_dims <- span(first_header_row, 1L, n_col) |>
    sub(pattern = '[0-9]+$', replacement = first_body_row - 1L)
  wb$add_font(sheet = sheet, dims = header_dims, bold = TRUE)
  wb$add_fill(sheet = sheet, dims = header_dims, color = openxlsx2::wb_color('F2F2F2'))
  wb$add_cell_style(
    sheet = sheet, dims = header_dims, horizontal = 'center', vertical = 'center',
    wrap_text = TRUE
  )
  wb$add_border(
    sheet = sheet, dims = span(first_header_row, 1L, n_col),
    top_border = 'thin', bottom_border = NULL, left_border = NULL, right_border = NULL
  )
  wb$add_border(
    sheet = sheet, dims = span(first_body_row - 1L, 1L, n_col),
    top_border = NULL, bottom_border = 'thin', left_border = NULL, right_border = NULL
  )

  wb$add_data(
    sheet = sheet, x = as.data.frame(body), dims = cell(first_body_row, 1L),
    col_names = FALSE, na.strings = ''
  )
  for(ii in seq_len(nrow(body))) merge_runs(body[ii, ], first_body_row + ii - 1L, 2L)
  # Overwrite counts with numbers and placeholders with formulas
  for(ii in seq_len(nrow(body))){
    for(cc in seq(2L, n_col)){
      value <- body[ii, cc]
      if(is.na(value)) next
      dims <- cell(first_body_row + ii - 1L, cc)
      if(value == '=NA()'){
        wb$add_formula(sheet = sheet, x = 'NA()', dims = dims)
      } else if(grepl('^[0-9]+$', value)){
        wb$add_data(sheet = sheet, x = as.numeric(value), dims = dims, col_names = FALSE)
      }
    }
  }
  # Cache the formulas' #N/A result, as Excel does, so viewers that do not recalculate
  #  still display it
  cc <- wb$worksheets[[length(wb$worksheets)]]$sheet_data$cc
  is_na_formula <- cc$f %in% 'NA()'
  cc$c_t[is_na_formula] <- 'e'
  cc$v[is_na_formula] <- '#N/A'
  wb$worksheets[[length(wb$worksheets)]]$sheet_data$cc <- cc
  wb$add_cell_style(
    sheet = sheet, dims = span(first_body_row, 2L, n_col) |>
      sub(pattern = '[0-9]+$', replacement = last_row),
    horizontal = 'center'
  )
  label_rows <- setdiff(seq_len(nrow(body)), section_rows)
  for(ii in label_rows){
    wb$add_cell_style(
      sheet = sheet, dims = cell(first_body_row + ii - 1L, 1L), indent = 1
    )
  }
  for(ii in section_rows){
    wb$add_font(sheet = sheet, dims = cell(first_body_row + ii - 1L, 1L), bold = TRUE)
  }
  wb$add_border(
    sheet = sheet, dims = span(last_row, 1L, n_col),
    top_border = NULL, bottom_border = 'thin', left_border = NULL, right_border = NULL
  )
  wb$set_col_widths(sheet = sheet, cols = seq_len(n_col), widths = col_widths)
  wb$freeze_pane(sheet = sheet, first_active_row = first_body_row)
  return(invisible(wb))
}
