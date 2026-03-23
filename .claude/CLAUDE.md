# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

R package for the RESPOND project, identifying contextual factors associated with HIV viraemia (viral load) across Malawi. The core research question is: what geographic and socioeconomic characteristics distinguish communities with high HIV viraemia from those with lower viraemia?

**Research approach:**
1. Identify health facility catchments with high viraemia (top 1% threshold, plus a set of IIT/intervention facilities)
2. Aggregate spatial covariates (land use, distance-based, economic, demographic) to those catchments
3. Run PCA separately for Metro and Non-metro settings to reduce dimensionality
4. Apply k-means clustering (2 clusters for Metro, 3 for Non-metro) to produce "community archetypes"
5. Characterize archetypes by their covariate profiles and assess within-profile variation

**Key outputs** (written to the versioned `analysis` directory in Dropbox):
- PCA scree plots, loading plots, and loadings tables per group
- K-means within/between cluster variance plots
- Cluster scatter plots (PC space) and maps per district
- `pca_kmeans_results.csv` — per-catchment PCA scores and cluster assignments
- `pca_kmeans_covariate_summaries.csv` — mean/median/SD of covariates per cluster
- `vetted_profile_groupings.csv` — manually reviewed final archetype assignments (input to scripts 05–06)
- `within_profile_variation.csv` and per-profile pairwise distance matrices
- Regression results linking archetypes to GVH-level outcomes

## Build & Package Commands

```bash
make install_deps   # Install R dependencies via remotes
make install        # Build and install the package
make check          # Build and run R CMD check (default target)
make clean          # Remove build artifacts
```

To load the package interactively during development: `devtools::load_all()` at the top of each script.

## Running Analysis Scripts

Scripts in `inst/scripts/` are numbered and run sequentially:

1. `00_prepare_*.R` — prepare individual covariates (distance rasters, RWI, poverty, lat/long)
2. `01_aggregate_covariates.R` — population-weighted aggregation of rasters to catchment polygons
3. `01b_add_community_workshop_indicators.R` — add community-level indicators from workshops
4. `02_visualize_covariates.R` — generate covariate distribution plots
5. `03_pca.R` — run PCA and k-means clustering on high-viraemia catchments
6. `04_cluster_viz.R` — visualization of cluster results
7. `05_gvh_regressions_by_archetype.R` — regression analysis by archetype
8. `06_within_profile_variation.R` — analyze variation within profiles

Each script sets `REPO_DIR <- '~/repos/mwi.hiv.factors'` at the top and loads config via:
```r
config <- versioning::Config$new(file.path(REPO_DIR, 'config.yaml'))
```

## Architecture

**Three-layer structure:**
- `R/` — reusable package functions (spatial aggregation, distance rasters, visualization)
- `inst/scripts/` — analysis workflow scripts that orchestrate functions
- `config.yaml` — all parameters, file paths, and data versions

**Configuration system (`config.yaml`):**
- All input/output paths are defined here, versioned via the `versioning` package
- Active covariates are uncommented; inactive covariates are commented out in YAML
- Two covariate lists: `covariates` (all available) and `pca_covariates` (subset used in PCA)
- Data versions are date/hash strings under `versions:` — changing these points scripts to different archived datasets
- Analysis is split by `setting` (Metro vs. Non-metro); metro catchments drop distance-based covariates (`drop_from_metro`)
- `subset_districts` restricts analysis to a named subset of districts

**Key data flow:**
- Raster covariates → aggregated to health facility catchments → PCA on high-viraemia catchments (top 1% by default) → k-means clustering → community archetypes
- The `pca_level` config key (`'facility'` or `'gvh'`) controls whether catchments are at facility or Group Village Headman level

**Spatial packages used:** `terra` (raster operations), `sf` (vector operations), `mbg` (VIF-based feature selection)
**Analysis packages:** `FactoMineR`/`factoextra` (PCA), base `stats::kmeans` (clustering)
**Data manipulation:** `data.table` throughout (prefer `data.table` syntax for new code)
