## #######################################################################################
##
## 03. RUN PRINCIPAL COMPONENT ANALYSIS
##
## AUTHOR: Nat Henry, nat@henryspatialanalysis.com
## CREATED: 2025-05-27
## PURPOSE: Run test PCA on facility catchments with high viraemia
##
## #######################################################################################

REPO_DIR <- '~/repos/mwi.hiv.factors'

## SETUP -------------------------------------------------------------------------------->

load_pkgs <- c(
  'corrr', 'data.table', 'factoextra', 'FactoMineR', 'ggplot2', 'ggcorrplot', 'versioning'
)
lapply(load_pkgs, library, character.only = TRUE) |> invisible()

# Load config
config <- versioning::Config$new(file.path(REPO_DIR, 'config.yaml'))

# Create analysis directory
config$get_dir_path('analysis') |> dir.create()

# Load input data
cov_data <- config$read("prepared_data", "covariates_by_catchment")

# Keep catchments with highest viraemia
top_catchments_pct <- config$get("top_catchments_pct")
top_catchments <- (
  cov_data
  [order(-viraemia15to49_mean),]
  [seq_len(round(top_catchments_pct * .N)), ]
)


## RUN PCA ------------------------------------------------------------------------------>

cov_names <- names(config$get("pca_covariates"))
pca_data <- top_catchments[, cov_names, with = FALSE] |>
  scale() |>
  data.table::as.data.table() |>
  data.table::setnames(
    old = cov_names,
    new = config$get('pca_covariates') |>
      unlist() |>
      gsub(pattern = ' ', replacement = '\n')
  )
pca_model <- stats::princomp(pca_data)


## PCA VISUALIZATIONS ------------------------------------------------------------------->

# Scree plot
config$get_file_path('analysis', 'pca_importance') |>
  png(height = 4, width = 6, units = 'in', res = 300)
factoextra::fviz_eig(
  X = pca_model,
  main = 'Variation explained by each principal component',
  xlab = 'Principal component',
  ylab = 'Variance explained (%)'
)
dev.off()

# Loadings plot
var_explained <- (pca_model$sdev^2 / sum(pca_model$sdev^2)) |>
  scales::percent(accuracy = 1.1)

config$get_file_path('analysis', 'pca_loadings') |>
  png(height = 8, width = 8, units = 'in', res = 300)
factoextra::fviz_pca_var(
  X = pca_model,
  col.var = 'cos2',
  gradient.cols = RColorBrewer::brewer.pal(n = 9, name = 'YlGnBu')[4:9],
  repel = TRUE,
  title = 'PCA loadings: first two components',
  xlab = glue::glue('Component 1 ("Urbanicity", explains {var_explained[1]} of variation)'),
  ylab = glue::glue('Component 2 ("Environment", explains {var_explained[2]} of variation)'),
  legend.title = 'Cosine\nsimilarity'
)
dev.off()
