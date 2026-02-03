#' Discretize a covariate
#'
#' @param vec ([numeric](N)) Vector of values
#'
#' @return Vector of discretized (0/1) values
#' @importFrom stats na.omit median
#' @export
discretize_covariate <- function(vec){
  vals <- stats::na.omit(vec)
  if(all(vals %in% c(0, 1))) return(vec)
  return(as.numeric(vec >= stats::median(vals)))
}