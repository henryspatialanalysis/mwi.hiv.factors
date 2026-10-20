#' Read one MPHIA survey's tested adults with household wealth and cluster locations
#'
#' @details Joins the adult biomarker file to the household file (zone, urban status,
#'   continuous wealth score, household weight) and the geomasked cluster centroids.
#'   Keeps adults in the age range with a final HIV test result and a positive blood
#'   test weight.
#'
#' @param bio_path (`character(1)`) Adult biomarker file (.dta)
#' @param hh_path (`character(1)`) Household file (.dta)
#' @param centroids_path (`character(1)`) Geomasked cluster centroids (.csv or .dta)
#' @param age_range (`integer(2)`) Minimum and maximum age, inclusive
#'
#' @return `data.table` with one row per tested adult and fields `personid`,
#'   `householdid`, `centroidid`, `zone`, `urban`, `sex`, `age`, `hiv`, `weight` (blood
#'   test weight), `wealth_score`, `hh_weight`, `longitude`, `latitude`
#'
#' @importFrom data.table as.data.table
#' @export
read_mphia_survey <- function(bio_path, hh_path, centroids_path, age_range){
  read_cols <- function(path, cols){
    haven::read_dta(path, col_select = dplyr::all_of(cols)) |>
      haven::zap_labels() |>
      data.table::as.data.table()
  }
  bio <- read_cols(
    bio_path,
    c('personid', 'householdid', 'centroidid', 'gender', 'age', 'bt_status',
      'hivstatusfinal', 'btwt0')
  )
  hh <- read_cols(
    hh_path, c('householdid', 'zone', 'urban', 'wealthscorecont', 'hhwt0')
  )
  centroids <- if(grepl('\\.csv$', centroids_path, ignore.case = TRUE)){
    data.table::fread(centroids_path)
  } else {
    read_cols(centroids_path, c('centroidid', 'longitude', 'latitude'))
  }
  dat <- bio[
    bt_status == 1 & hivstatusfinal %in% 1:2 & btwt0 > 0 &
      age >= age_range[1] & age <= age_range[2]
  ][
    ,
    .(
      personid, householdid, centroidid,
      sex = data.table::fifelse(gender == 2, 'Female', 'Male'),
      age, hiv = as.integer(hivstatusfinal == 1), weight = btwt0
    )
  ] |>
    merge(
      hh[, .(householdid, zone, urban, wealth_score = wealthscorecont, hh_weight = hhwt0)],
      by = 'householdid', all.x = TRUE
    ) |>
    merge(
      centroids[, .(centroidid, longitude, latitude)], by = 'centroidid', all.x = TRUE
    )
  return(dat)
}


#' Assign MPHIA clusters to the polygons containing their geomasked centroids
#'
#' @param dat (`data.table`) Output of [read_mphia_survey()]
#' @param polygons_sf (`sf`) Polygons to match against
#' @param id_field (`character(1)`) Field of `polygons_sf` to return
#'
#' @return Vector the length of `nrow(dat)` with the containing polygon's `id_field`,
#'   NA for centroids outside all polygons
#'
#' @importFrom sf st_as_sf st_within st_transform st_crs
#' @export
assign_mphia_clusters <- function(dat, polygons_sf, id_field){
  clusters <- unique(dat[, .(centroidid, longitude, latitude)])
  pts <- sf::st_as_sf(clusters, coords = c('longitude', 'latitude'), crs = 4326) |>
    sf::st_transform(sf::st_crs(polygons_sf))
  hit <- vapply(
    sf::st_within(pts, polygons_sf),
    function(x) if(length(x) > 0) x[1] else NA_integer_,
    integer(1)
  )
  matched <- polygons_sf[[id_field]][hit]
  return(matched[match(dat$centroidid, clusters$centroidid)])
}


#' Wealth quintiles within an MPHIA subsample
#'
#' @details Following Burke et al. (AIDS 2022), cut points are weighted quantiles of the
#'   continuous household wealth score across the subsample's households (those with at
#'   least one tested adult), weighted by the household weight. A household falls in
#'   quintile k when its score is at most the k-th cut point and above the (k-1)-th, the
#'   convention of Stata's `xtile`.
#'
#' @param dat (`data.table`) Subsample of [read_mphia_survey()] output
#' @param n_groups (`integer(1)`, default 5) Number of groups
#'
#' @return Integer vector of quintiles, 1 (poorest) to `n_groups` (richest)
#'
#' @export
mphia_wealth_quintiles <- function(dat, n_groups = 5L){
  hh <- unique(dat[!is.na(wealth_score), .(householdid, wealth_score, hh_weight)])
  data.table::setorder(hh, wealth_score)
  cum_share <- cumsum(hh$hh_weight) / sum(hh$hh_weight)
  probs <- seq_len(n_groups - 1L) / n_groups
  cuts <- vapply(probs, function(p) hh$wealth_score[which(cum_share >= p)[1]], numeric(1))
  quintile <- findInterval(dat$wealth_score, cuts, left.open = TRUE) + 1L
  quintile[is.na(dat$wealth_score)] <- NA_integer_
  return(quintile)
}


#' HIV prevalence by subgroup within an MPHIA subsample
#'
#' @details Follows the design of Burke et al. (AIDS 2022): the subsample is treated as
#'   its own survey, with clusters as primary sampling units and no stratification, and
#'   the blood test weight. Standard errors use Taylor linearisation.
#'
#' @param dat (`data.table`) Subsample of [read_mphia_survey()] output
#' @param group_vars (`character(N)`) Grouping fields; each is tabulated separately
#'
#' @return `data.table` with fields `variable`, `level`, `n` (unweighted), `wtd_pct`
#'   (weighted share of the subsample), `prev`, `se`, and `n_clusters`. The first row
#'   (`variable = 'Total'`) covers the whole subsample.
#'
#' @importFrom data.table data.table rbindlist
#' @export
mphia_prevalence_table <- function(dat, group_vars){
  design <- survey::svydesign(ids = ~centroidid, weights = ~weight, data = dat)
  total <- survey::svymean(~hiv, design)
  out <- list(data.table::data.table(
    variable = 'Total', level = 'Total', n = nrow(dat), wtd_pct = 1,
    prev = as.numeric(stats::coef(total)), se = as.numeric(survey::SE(total)),
    n_clusters = data.table::uniqueN(dat$centroidid)
  ))
  for(gv in group_vars){
    sub <- !is.na(dat[[gv]])
    res <- survey::svyby(
      ~hiv, stats::as.formula(paste0('~', gv)), design[sub, ], survey::svymean
    )
    counts <- dat[
      sub,
      .(n = .N, wtd = sum(weight), n_clusters = data.table::uniqueN(centroidid)),
      by = c(gv)
    ]
    data.table::setnames(counts, gv, 'level')
    counts[, `:=`(level = as.character(level), wtd_pct = wtd / sum(wtd))]
    est <- data.table::data.table(
      variable = gv,
      level = as.character(res[[gv]]),
      prev = as.numeric(stats::coef(res)),
      se = as.numeric(survey::SE(res))
    ) |>
      merge(counts[, .(level, n, wtd_pct, n_clusters)], by = 'level', sort = FALSE)
    out[[length(out) + 1]] <- est
  }
  out <- data.table::rbindlist(out, use.names = TRUE)
  data.table::setcolorder(
    out, c('variable', 'level', 'n', 'wtd_pct', 'prev', 'se', 'n_clusters')
  )
  return(out)
}


#' Trend in HIV prevalence across wealth quintiles
#'
#' @details Survey-weighted logistic regression of HIV status on the wealth quintile
#'   treated as a linear term, with the same design as [mphia_prevalence_table()].
#'   Intervals and p-values use the design degrees of freedom (clusters minus one).
#'
#' @param dat (`data.table`) Subsample of [read_mphia_survey()] output, with fields
#'   `wealth_quintile` and any adjustment variables
#' @param adjust_vars (`character(N)`, default NULL) Covariates to adjust for
#' @param level (`numeric(1)`, default 0.95) Confidence level
#'
#' @return One-row `data.table` with fields `or` (odds ratio per one-quintile increase),
#'   `lower`, `upper`, `p_value`, `df`
#'
#' @export
mphia_wealth_trend <- function(dat, adjust_vars = NULL, level = 0.95){
  design <- survey::svydesign(ids = ~centroidid, weights = ~weight, data = dat)
  fml <- stats::as.formula(
    paste('hiv ~', paste(c('wealth_quintile', adjust_vars), collapse = ' + '))
  )
  fit <- survey::svyglm(fml, design = design, family = stats::quasibinomial())
  df <- survey::degf(design)
  beta <- stats::coef(fit)[['wealth_quintile']]
  se <- sqrt(stats::vcov(fit)['wealth_quintile', 'wealth_quintile'])
  t_crit <- stats::qt(1 - (1 - level) / 2, df = df)
  return(data.table::data.table(
    or = exp(beta),
    lower = exp(beta - t_crit * se),
    upper = exp(beta + t_crit * se),
    p_value = 2 * stats::pt(-abs(beta / se), df = df),
    df = df
  ))
}


#' Format an unweighted count with its weighted share, e.g. "2664 (72)"
#'
#' @param n (`integer(N)`) Unweighted counts
#' @param wtd_pct (`numeric(N)`) Weighted shares, 0 to 1
#'
#' @return `character(N)`
#'
#' @export
format_n_pct <- function(n, wtd_pct){
  return(sprintf('%d (%.0f)', as.integer(n), 100 * wtd_pct))
}


#' Format a prevalence with its standard error, e.g. "17.7 (1.0)"
#'
#' @param est (`numeric(N)`) Prevalence, 0 to 1
#' @param se (`numeric(N)`) Standard error, 0 to 1
#'
#' @return `character(N)`
#'
#' @export
format_prev_se <- function(est, se){
  return(sprintf('%.1f (%.1f)', 100 * est, 100 * se))
}


#' Format an odds ratio with its interval and p-value
#'
#' @param trend (`data.table`) Output of [mphia_wealth_trend()]
#'
#' @return `character(1)`, e.g. "0.86 (0.78-0.95); p = 0.003"
#'
#' @export
format_trend <- function(trend){
  p <- if(trend$p_value < 0.001) 'p < 0.001' else sprintf('p = %.3f', trend$p_value)
  return(sprintf(
    '%.2f (%.2f–%.2f); %s', trend$or, trend$lower, trend$upper, p
  ))
}
