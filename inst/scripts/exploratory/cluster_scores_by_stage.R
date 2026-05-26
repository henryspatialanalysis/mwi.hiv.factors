## #######################################################################################
##
## CLUSTER SCORES BY STAGE
##
## AUTHOR: Nat Henry, nat@henryspatialanalysis.com
## CREATED: 2026-05-25
## PURPOSE: Compare non-metro clustering quality across three analysis stages using the
##   mean silhouette width, within-cluster sum of squares (WSS), and between-cluster sum
##   of squares (BSS) in the space of the top 4 principal components.
##
##   Only the non-metro (i.e. non-Teal) facilities are scored; metro facilities are
##   dropped entirely. The three stages compared are:
##     1. 2026-02-26-no-cvs - k-means (k = 5), PCA fit without community workshop covariates
##     2. 2026-02-26        - k-means (k = 5), PCA fit with all covariates
##     3. 2026-03-16        - manually vetted profile colors (k = 5)
##
##   Stages 1 and 2 use their own PCA loadings; stage 3 shares the 2026-02-26 loadings,
##   so its PC scores are read directly from the 2026-03-16 results. The "Pink" group
##   (relabeled "Metro 2" during vetting) is retained in the non-metro set, so all three
##   stages score the same 32 facilities.
##
## #######################################################################################

REPO_DIR <- '~/repos/mwi.hiv.factors'
ANALYSIS_DIR <- '/mnt/c/Users/nathe/Dropbox/RESPOND_Community Archetyping/Analysis'
OUT_FP <- file.path(ANALYSIS_DIR, '2026-03-16', 'cluster_scores_by_stage.csv')

N_PCS <- 4L
PC_COLS <- paste0('pc', seq_len(N_PCS))

## SETUP -------------------------------------------------------------------------------->

load_pkgs <- c('data.table', 'cluster', 'versioning')
lapply(load_pkgs, library, character.only = TRUE) |> invisible()

config <- versioning::Config$new(file.path(REPO_DIR, 'config.yaml'))

## Score a single clustering in PC space ------------------------------------------------>
## Returns the mean silhouette width plus within- and between-cluster sum of squares,
## computed in the space of the top N_PCS principal components (Euclidean distance).

score_clustering <- function(pc_matrix, labels){
  keep <- stats::complete.cases(pc_matrix) & !is.na(labels)
  pc_matrix <- pc_matrix[keep, , drop = FALSE]
  cluster_int <- as.integer(as.factor(labels[keep]))

  # Total / within / between cluster sum of squares
  global_center <- colMeans(pc_matrix)
  tss <- sum(sweep(pc_matrix, 2, global_center)^2)
  wss <- split(as.data.frame(pc_matrix), cluster_int) |>
    vapply(function(grp){
      grp <- as.matrix(grp)
      sum(sweep(grp, 2, colMeans(grp))^2)
    }, numeric(1)) |>
    sum()
  bss <- tss - wss

  # Mean silhouette width
  sil <- cluster::silhouette(cluster_int, stats::dist(pc_matrix))
  list(
    n_facilities = nrow(pc_matrix),
    k_selected = data.table::uniqueN(cluster_int),
    mean_silhouette = mean(sil[, 'sil_width']),
    wss = wss,
    bss = bss
  )
}

## Load the non-metro PC scores and cluster labels for one stage ------------------------>

load_stage <- function(stage, cluster_source, k = NULL){
  dt <- file.path(ANALYSIS_DIR, stage, 'Non-metro', 'pca_kmeans_results.csv') |>
    data.table::fread()
  if(cluster_source == 'kmeans'){
    dt[, cluster_label := as.character(get(paste0('kmeans_', k)))]
  } else if(cluster_source == 'vetted_profile'){
    vetted <- file.path(ANALYSIS_DIR, stage, 'vetted_profile_groupings.csv') |>
      data.table::fread() |>
      _[, .(catchment_id, profile_color)]
    dt <- merge(dt, vetted, by = 'catchment_id', all.x = TRUE)
    # "Pink" was relabeled "Metro 2" during vetting but is retained in the non-metro set
    dt[, cluster_label := profile_color]
  } else {
    stop('Unknown cluster_source: ', cluster_source)
  }
  return(dt)
}

## RUN ----------------------------------------------------------------------------------->

stages <- list(
  list(stage = '2026-02-26-no-cvs', cluster_source = 'kmeans', k = 5L,
    description = 'k-means (PCA without community covariates)'),
  list(stage = '2026-02-26', cluster_source = 'kmeans', k = 5L,
    description = 'k-means (PCA with all covariates)'),
  list(stage = '2026-03-16', cluster_source = 'vetted_profile', k = NULL,
    description = 'manually vetted profile colors')
)

results <- lapply(stages, function(spec){
  dt <- load_stage(spec$stage, spec$cluster_source, spec$k)
  scores <- score_clustering(as.matrix(dt[, ..PC_COLS]), dt$cluster_label)
  data.table::data.table(
    stage = spec$stage,
    description = spec$description,
    cluster_source = spec$cluster_source,
    n_facilities = scores$n_facilities,
    k_selected = scores$k_selected,
    mean_silhouette = round(scores$mean_silhouette, 4),
    wss = round(scores$wss, 3),
    bss = round(scores$bss, 3)
  )
}) |> data.table::rbindlist()

print(results)

## SAVE ---------------------------------------------------------------------------------->

data.table::fwrite(results, OUT_FP)
message('Wrote summary to: ', OUT_FP)
