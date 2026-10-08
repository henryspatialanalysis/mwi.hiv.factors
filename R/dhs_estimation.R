#' Direct survey estimates of one indicator by domain
#'
#' @details Domain (subpopulation) estimation on the full survey design, so that
#'   standard errors reflect the random domain sample size. Proportions and means use
#'   [survey::svymean()] (a Hajek ratio estimator with Taylor-linearised variance); the
#'   Gini coefficient uses [convey::svygini()]; the standard deviation is the square root
#'   of [survey::svyvar()], with a delta-method standard error.
#'
#' @param design (`survey.design2`) Full survey design. For `type = 'gini'` it must have
#'   been prepared with [convey::convey_prep()].
#' @param var (`character(1)`) Outcome variable
#' @param domain_var (`character(1)`) Domain variable; rows with NA are outside all
#'   domains
#' @param type (`character(1)`) One of 'mean', 'proportion', 'gini', 'sd'
#' @param denominator (`character(1)`, default NULL) Logical variable restricting the
#'   indicator's denominator (for example, adults who worked in the last 12 months)
#' @param psu_var (`character(1)`) PSU variable, to count clusters per domain
#' @param strata_var (`character(1)`) Strata variable, to count strata per domain
#'
#' @return List with items:
#'   - `estimates`: `data.table` with fields `domain`, `est`, `se`, `n` (unweighted
#'     denominator), `n_clusters`, `n_strata`
#'   - `vcov`: domain covariance matrix (diagonal for 'gini' and 'sd')
#'
#' @importFrom data.table data.table as.data.table
#' @export
estimate_by_domain <- function(
  design, var, domain_var, type, denominator = NULL, psu_var, strata_var
){
  dat <- design$variables
  keep <- !is.na(dat[[var]]) & !is.na(dat[[domain_var]])
  if(!is.null(denominator)) keep <- keep & (dat[[denominator]] %in% TRUE)
  sub <- design[keep, ]
  sub_dat <- dat[keep, ]

  fml <- stats::as.formula(paste0('~', var))
  by_fml <- stats::as.formula(paste0('~', domain_var))
  if(type %in% c('mean', 'proportion')){
    res <- survey::svyby(fml, by_fml, sub, survey::svymean, covmat = TRUE, na.rm = TRUE)
    vc <- stats::vcov(res)
    est <- stats::coef(res)
  } else if(type == 'gini'){
    res <- survey::svyby(fml, by_fml, sub, convey::svygini, na.rm = TRUE)
    est <- stats::coef(res)
    vc <- diag(survey::SE(res)^2, nrow = length(est))
  } else if(type == 'sd'){
    res <- survey::svyby(fml, by_fml, sub, survey::svyvar, na.rm = TRUE)
    v <- stats::coef(res)
    est <- sqrt(v)
    vc <- diag((survey::SE(res) / (2 * est))^2, nrow = length(est))
  } else {
    stop("Unknown indicator type: ", type)
  }
  domains <- as.character(res[[domain_var]])
  names(est) <- domains
  dimnames(vc) <- list(domains, domains)

  counts <- data.table::data.table(
    domain = as.character(sub_dat[[domain_var]]),
    psu = sub_dat[[psu_var]],
    strata = sub_dat[[strata_var]]
  )[
    ,
    .(
      n = .N,
      n_clusters = data.table::uniqueN(psu),
      n_strata = data.table::uniqueN(strata)
    ),
    by = domain
  ]
  estimates <- data.table::data.table(
    domain = domains, est = as.numeric(est), se = sqrt(diag(vc))
  ) |> merge(counts, by = 'domain', all.x = TRUE)
  return(list(estimates = estimates, vcov = vc))
}


#' Pairwise differences between domain estimates
#'
#' @param estimate (`list`) Output of [estimate_by_domain()]
#'
#' @return `data.table` with fields `domain_a`, `domain_b`, `diff` (a minus b), `se`,
#'   `df_complete` (the smaller of the two domains' design degrees of freedom)
#'
#' @importFrom data.table data.table rbindlist
#' @export
pairwise_domain_contrasts <- function(estimate){
  est <- estimate$estimates
  domains <- est$domain
  if(length(domains) < 2) return(NULL)
  vc <- estimate$vcov[domains, domains, drop = FALSE]
  theta <- stats::setNames(est$est, domains)
  df_com <- stats::setNames(pmax(est$n_clusters - est$n_strata, 1), domains)
  pairs <- utils::combn(domains, 2, simplify = FALSE)
  out <- lapply(pairs, function(pr){
    a <- pr[1]; b <- pr[2]
    data.table::data.table(
      domain_a = a,
      domain_b = b,
      diff = theta[[a]] - theta[[b]],
      se = sqrt(max(vc[a, a] + vc[b, b] - 2 * vc[a, b], 0)),
      df_complete = min(df_com[[a]], df_com[[b]])
    )
  }) |> data.table::rbindlist()
  return(out)
}


#' Combine multiply-imputed estimates with Rubin's rules
#'
#' @details Point estimates and variances are pooled on the natural scale (Rubin 1987).
#'   Degrees of freedom follow Barnard and Rubin (1999), using each imputation's design
#'   degrees of freedom (clusters minus strata in the domain) as the complete-data
#'   degrees of freedom. Pooling on the natural scale avoids instability from
#'   imputations in which a rare proportion is exactly zero. Confidence intervals:
#'   - 'beta': Korn and Graubard (1998) interval for proportions, a Clopper-Pearson
#'     interval at the design effective sample size, deflated for the pooled degrees of
#'     freedom and capped at the unweighted sample size. Recommended for small domains
#'     and rare outcomes; the same method as `survey::svyciprop(method = 'beta')`.
#'   - 'logit' or 'log': delta-method interval on the transformed scale, back-transformed
#'   - 'identity': symmetric t interval
#'
#' @param draws (`data.table`) Per-imputation results with fields `est`, `se`,
#'   `df_complete`, the grouping fields named in `by`, and (for `transform = 'beta'`)
#'   `n`, the unweighted denominator
#' @param by (`character(N)`) Grouping fields identifying one pooled estimate
#' @param transform (`character(1)`) Confidence interval method: one of 'identity',
#'   'beta', 'logit', 'log'
#' @param n_imputations (`integer(1)`) Total imputations attempted. Imputations in
#'   which a domain had no observations are missing from `draws`; the count used is
#'   reported as `m_used`.
#' @param level (`numeric(1)`, default 0.95) Confidence level
#'
#' @return `data.table` with fields `by`, `est`, `se`, `lower`, `upper`, `df`,
#'   `share_var_location` (the share of total variance due to between-imputation
#'   variation, i.e. cluster-location uncertainty), `m_used`, and `p_value` (a test of
#'   zero; identity transform only, NA otherwise)
#'
#' @export
pool_rubin <- function(draws, by, transform, n_imputations, level = 0.95){
  eps <- 1e-6
  interval <- function(q, se, t_crit, n){
    if(transform == 'beta'){
      n_eff <- if(se > 0 && q > 0 && q < 1) q * (1 - q) / se^2 else n
      n_eff <- min(n_eff * (stats::qnorm(1 - (1 - level) / 2) / t_crit)^2, n)
      x <- q * n_eff
      lower <- if(x <= 0) 0 else stats::qbeta((1 - level) / 2, x, n_eff - x + 1)
      upper <- if(x >= n_eff) 1 else stats::qbeta(1 - (1 - level) / 2, x + 1, n_eff - x)
      return(c(lower, upper))
    }
    if(transform == 'logit' && q > eps && q < 1 - eps){
      half <- t_crit * se / (q * (1 - q))
      return(stats::plogis(stats::qlogis(q) + c(-half, half)))
    }
    if(transform == 'log' && q > eps){
      half <- t_crit * se / q
      return(exp(log(q) + c(-half, half)))
    }
    bounds <- q + c(-t_crit * se, t_crit * se)
    if(transform %in% c('logit', 'beta')) bounds <- pmin(pmax(bounds, 0), 1)
    if(transform == 'log') bounds <- pmax(bounds, 0)
    return(bounds)
  }
  is_identity <- transform == 'identity'
  pooled <- draws[
    ,
    {
      m <- .N
      q_bar <- mean(est)
      u_bar <- mean(se^2)
      b <- if(m > 1) stats::var(est) else 0
      t_var <- u_bar + (1 + 1 / m) * b
      lambda <- if(t_var > 0) (1 + 1 / m) * b / t_var else 0
      nu_com <- mean(df_complete)
      nu_obs <- (nu_com + 1) / (nu_com + 3) * nu_com * (1 - lambda)
      nu <- if(lambda > 0 && m > 1) {
        nu_old <- (m - 1) / lambda^2
        1 / (1 / nu_old + 1 / nu_obs)
      } else nu_obs
      nu <- max(nu, 1)
      t_crit <- stats::qt(1 - (1 - level) / 2, df = nu)
      n_mean <- if('n' %in% names(.SD)) mean(n) else NA_real_
      ci <- interval(q_bar, sqrt(t_var), t_crit, n_mean)
      .(
        est = q_bar,
        se = sqrt(t_var),
        lower = ci[1],
        upper = ci[2],
        df = nu,
        share_var_location = lambda,
        m_used = m,
        p_value = if(is_identity && t_var > 0){
          2 * stats::pt(-abs(q_bar / sqrt(t_var)), df = nu)
        } else NA_real_
      )
    },
    by = by,
    .SDcols = intersect('n', names(draws))
  ]
  pooled[, m_attempted := n_imputations]
  return(pooled[])
}
