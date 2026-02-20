#' Discretize a covariate
#'
#' @param vec ([numeric](N)) Vector of values
#' @param num_jenks ([integer](1)) Number of Jenks natural breaks to use
#' @return List with six items:
#' \itemize{
#'   \item `quintiles`: Vector of quintile cutoffs
#'   \item `q1`: Bottom quintile
#'   \item `q5`: Top quintile
#'   \item `jenks`: Jenks natural breaks classification
#'   \item `j1`: Bottom Jenks class
#'   \item `jtop`: Top Jenks class
#' }
#' @importFrom stats na.omit quantile
#' @importFrom BAMMtools getJenksBreaks
#' @export
discretize_covariate <- function(vec, num_jenks = 5){
  vals <- stats::na.omit(vec)
  if(all(vals %in% c(0, 1))) return(NULL)
  quintiles <- stats::quantile(vals, probs = seq(0, 1, by = 0.2))
  jenks <- BAMMtools::getJenksBreaks(vals, k = num_jenks)
  out_list <- list(
    quintiles = quintiles,
    q1 = as.numeric(vec < quintiles[2]),
    q5 = as.numeric(vec >= quintiles[5]),
    jenks = jenks,
    j1 = as.numeric(vec < jenks[2]),
    jtop = as.numeric(vec >= jenks[length(jenks) - 1])
  )
  return(out_list)
}