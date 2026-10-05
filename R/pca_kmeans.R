#' Run PCA followed by k-means clustering on catchment covariates
#'
#' @details Mirrors the core steps of `03_pca.R` without writing any files: covariates
#'   are standardized, remaining missing values are filled with the column mean,
#'   principal components are estimated with [stats::princomp()], and k-means is run on
#'   the standardized scores of the leading components for each requested value of k.
#'
#' @param covariate_data ([data.table::data.table]) One row per catchment, containing at
#'   least the columns in `cov_names`
#' @param cov_names (`character(N)`) Covariates to include
#' @param n_pcs (`integer(1)`, default 4) Number of leading components used for
#'   clustering
#' @param k_values (`integer(N)`, default 2:9) Numbers of clusters to fit
#' @param nstart (`integer(1)`, default 25) Number of random starts for each k-means fit
#'
#' @return Named list with:
#'   - `scores`: matrix of principal component scores (all components)
#'   - `variance_explained`: proportion of variance explained by each component
#'   - `loadings`: matrix of component loadings
#'   - `clusters`: data.table with one column `kmeans_{k}` per value of k
#'
#' @importFrom stats princomp kmeans
#' @importFrom data.table as.data.table data.table
#' @export
pca_kmeans <- function(
  covariate_data, cov_names, n_pcs = 4L, k_values = 2:9, nstart = 25L
){
  pca_data <- covariate_data[, cov_names, with = FALSE] |>
    scale() |>
    data.table::as.data.table()
  for(cov_name in cov_names){
    if(all(is.na(pca_data[[cov_name]]))){
      pca_data[[cov_name]] <- NULL
    } else if(any(is.na(pca_data[[cov_name]]))){
      pca_data[is.na(get(cov_name)), (cov_name) := 0]
    }
  }
  pca_model <- stats::princomp(pca_data)
  variance_explained <- pca_model$sdev^2 / sum(pca_model$sdev^2)
  use_pcs <- seq_len(min(n_pcs, ncol(pca_model$scores)))
  kmeans_data <- scale(pca_model$scores[, use_pcs, drop = FALSE])
  clusters <- lapply(k_values, function(k){
    stats::kmeans(kmeans_data, centers = k, nstart = nstart)$cluster
  }) |>
    stats::setNames(paste0('kmeans_', k_values)) |>
    data.table::as.data.table()
  list(
    scores = unclass(pca_model$scores),
    variance_explained = variance_explained,
    loadings = unclass(pca_model$loadings),
    clusters = clusters
  )
}
