#' File path for one DHS survey's prepared or analysis output
#'
#' @details Fills the `{survey}` placeholder in a configured file name and creates the
#'   survey's subfolder if needed, following the `{pca_group}` convention used
#'   elsewhere in the package.
#'
#' @param config ([versioning::Config]) Project configuration
#' @param dir_name (`character(1)`) Configured directory, e.g. 'dhs_prepared'
#' @param file_name (`character(1)`) Configured file within that directory
#' @param survey (`character(1)`) Survey ID, a name under `dhs: surveys:` in the config
#'
#' @return Full file path (`character(1)`)
#'
#' @export
dhs_survey_path <- function(config, dir_name, file_name, survey){
  path <- gsub(
    '{survey}', survey, config$get_file_path(dir_name, file_name), fixed = TRUE
  )
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  return(path)
}


#' Normalise district names for matching DHS clusters to restriction polygons
#'
#' @param x (`character(N)`) District names
#' @param fixes (`list`, default NULL) Named list mapping normalised names to
#'   replacement normalised names
#'
#' @return Lower-case, letters-only names (`character(N)`) with fixes applied
#'
#' @export
normalize_district <- function(x, fixes = NULL){
  x <- gsub('[^a-z]', '', tolower(x))
  if(!is.null(fixes)){
    hit <- x %in% names(fixes)
    x[hit] <- unlist(fixes)[x[hit]]
  }
  return(x)
}
