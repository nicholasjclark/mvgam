# Numeric scoring kernels for posterior forecast evaluation.
#
# Eleven scores in two groups:
#   Univariate, one cell per time : CRPS, DRPS, SIS, Brier, log score,
#                                   DSS, quantile score, twCRPS
#   Multivariate, one per horizon : energy, variogram, twenergy
#
# `univariate_score()` applies a `(truth, fc_col, ...)` ->
# `c(score, in_interval)` cell over the horizon of one series.
# `multivariate_score()` applies a `(truth_at_h, fc_at_h, ...)` ->
# scalar cell to the series stacked at each horizon.

# ----- Shared utilities -------------------------------------------

#' The log transform of `log = TRUE`
#'
#' The offset keeps a zero count finite.
#'
#' @param x Numeric vector or matrix
#' @param eps Numeric offset
#' @return `log(x + eps)`
#' @noRd
log_offset <- function(x, eps = 0.001) log(x + eps)


#' Whether an observation lies in the central predictive interval
#'
#' @param fc_col Numeric vector of forecast draws
#' @param truth Numeric observed value
#' @param interval_width Numeric width of the central interval
#' @return `1L` inside the interval, `0L` outside
#' @noRd
in_central_pi <- function(fc_col, truth, interval_width = 0.9) {
  alpha <- (1 - interval_width) / 2
  iv <- stats::quantile(
    fc_col, probs = c(alpha, 1 - alpha), na.rm = TRUE
  )
  as.integer(truth >= iv[1L] && truth <= iv[2L])
}


#' A per-cell score applied over a forecast horizon
#'
#' A missing observation leaves its row missing.
#'
#' @param truth Numeric vector of observed values, one per horizon
#' @param fc Numeric matrix of forecast draws, one column per horizon
#' @param score_fn Function of `(truth_i, fc_col_i, ...)` returning
#'   `c(score, in_interval)`
#' @param ... Further arguments of `score_fn`
#' @return `[h, 2]` matrix with columns `score` and `in_interval`
#' @noRd
apply_univariate_score <- function(truth, fc, score_fn, ...) {
  checkmate::assert_numeric(truth)
  checkmate::assert_matrix(fc, ncols = length(truth))
  checkmate::assert_function(score_fn)
  out <- matrix(NA_real_, nrow = length(truth), ncol = 2L)
  keep <- which(!is.na(truth))
  for (i in keep) {
    out[i, ] <- score_fn(truth[i], fc[, i], ...)
  }
  colnames(out) <- c("score", "in_interval")
  out
}


#' Per-series forecast matrices regrouped by horizon
#'
#' @param fcs List of `[ndraws, h]` forecast matrices, one per series
#' @return List of `[n_series, ndraws]` matrices, one per horizon
#' @noRd
stack_fcs_per_horizon <- function(fcs) {
  h <- ncol(fcs[[1L]])
  lapply(seq_len(h), function(t) {
    do.call(rbind, lapply(fcs, function(fc) fc[, t]))
  })
}


#' A per-horizon multivariate score applied over the horizon
#'
#' @param truths Numeric `[n_series, h]` matrix of observed values
#' @param fcs List of `[ndraws, h]` forecast matrices, one per series
#' @param score_fn Function of `(truth_vec, fc_mat, ...)` returning a
#'   scalar, with `fc_mat` shaped `[n_series, ndraws]`
#' @param ... Further arguments of `score_fn`
#' @return Numeric vector of scores, one per horizon
#' @noRd
apply_multivariate_score <- function(truths, fcs, score_fn, ...) {
  checkmate::assert_matrix(truths)
  checkmate::assert_list(fcs, min.len = 1L)
  checkmate::assert_function(score_fn)
  fcs_per_h <- stack_fcs_per_horizon(fcs)
  vapply(seq_len(ncol(truths)), function(t) {
    score_fn(truths[, t], fcs_per_h[[t]], ...)
  }, numeric(1L))
}


#' Require the scoringRules package for a score that uses it
#'
#' @param score Character name of a score
#' @return `NULL`, invisibly, or an error
#' @noRd
require_scoring_rules <- function(score) {
  checkmate::assert_string(score)
  if (!score %in% c("drps", "sis", "brier")) {
    insight::check_if_installed(
      "scoringRules",
      reason = paste0("to compute the '", score, "' score")
    )
  }
  invisible(NULL)
}


#' One univariate score of one series over its horizon
#'
#' Each cell receives the arguments it uses.
#'
#' @param score Character name of a univariate score
#' @param truth Numeric vector of observed values, one per horizon
#' @param fc Numeric matrix of forecast draws, one column per horizon
#' @param interval_width Numeric width of the central interval
#' @param quantile_level Numeric quantile level of the `qs` score
#' @param lower,upper Numeric thresholds of the `twcrps` score
#' @param log Logical. `TRUE` scores on the log scale
#' @return `[h, 2]` matrix with columns `score` and `in_interval`
#' @noRd
univariate_score <- function(score, truth, fc, interval_width = 0.9,
                             quantile_level = 0.5, lower = -Inf,
                             upper = Inf, log = FALSE) {
  checkmate::assert_choice(score, univariate_forecast_scores)
  require_scoring_rules(score)
  cell <- switch(
    score,
    crps = crps_cell, drps = drps_cell, sis = sis_cell,
    brier = brier_cell, logs = logs_cell, dss = dss_cell, qs = qs_cell,
    twcrps = twcrps_cell
  )
  args <- switch(
    score,
    crps = , sis = list(interval_width = interval_width, log = log),
    drps = list(interval_width = interval_width),
    qs = list(alpha = quantile_level),
    twcrps = list(lower = lower, upper = upper),
    list()
  )
  do.call(apply_univariate_score, c(list(truth, fc, cell), args))
}


#' One multivariate score over the shared horizon
#'
#' @param score Character name of a multivariate score
#' @param truths Numeric `[n_series, h]` matrix of observed values
#' @param fcs List of `[ndraws, h]` forecast matrices, one per series
#' @param lower,upper Numeric thresholds of the `twenergy` score
#' @param log Logical. `TRUE` scores on the log scale
#' @param weights Numeric per-series weights of the `variogram` score,
#'   or `NULL`
#' @return Numeric vector of joint scores, one per horizon
#' @noRd
multivariate_score <- function(score, truths, fcs, lower = -Inf,
                               upper = Inf, log = FALSE,
                               weights = NULL) {
  checkmate::assert_choice(score, joint_forecast_scores)
  require_scoring_rules(score)
  switch(
    score,
    energy = apply_multivariate_score(truths, fcs, energy_cell,
                                      log = log),
    variogram = apply_multivariate_score(truths, fcs, variogram_cell,
                                         log = log, weights = weights),
    twenergy = apply_multivariate_score(truths, fcs, twenergy_cell,
                                        lower = lower, upper = upper)
  )
}


# ----- Per-cell univariate kernels --------------------------------

#' CRPS of the empirical distribution of the draws
#' @noRd
crps_cell <- function(truth, fc, interval_width = 0.9, log = FALSE) {
  if (log) {
    truth <- log_offset(truth)
    fc <- log_offset(fc)
  }
  s <- scoringRules::crps_sample(y = truth, dat = fc, method = "edf")
  c(s, in_central_pi(fc, truth, interval_width))
}


#' Discrete ranked probability score of a count
#'
#' The sum over `k >= 0` of `(1{k >= y} - F(k))^2`, with `F` the
#' empirical distribution of the draws. Every term above the largest of
#' the observation and the draws is zero.
#' @noRd
drps_cell <- function(truth, fc, interval_width = 0.9) {
  ys <- 0:max(c(truth, fc), na.rm = TRUE)
  Fy <- stats::ecdf(fc)
  s <- sum((as.integer(ys >= truth) - Fy(ys))^2)
  c(s, in_central_pi(fc, truth, interval_width))
}


#' Scaled interval score (Gneiting and Raftery 2007)
#'
#' The width of the central interval plus `2 / alpha` times the
#' distance of an observation outside it.
#' @noRd
sis_cell <- function(truth, fc, interval_width = 0.9, log = FALSE) {
  if (log) {
    truth <- log_offset(truth)
    fc <- log_offset(fc)
  }
  alpha <- 1 - interval_width
  creds <- stats::quantile(
    fc, probs = c(alpha / 2, 1 - alpha / 2), na.rm = TRUE
  )
  lo <- creds[[1L]]
  hi <- creds[[2L]]
  s <- (hi - lo) + (2 / alpha) * (max(truth - hi, 0) + max(lo - truth, 0))
  c(s, in_central_pi(fc, truth, interval_width))
}


#' Brier score of a binary outcome
#'
#' The squared error of the forecast probability, which the mean of the
#' draws estimates from 0/1 response draws and from expected values
#' alike. The mean of the per-draw squared errors would be linear in
#' the probability and would reward a forecast pushed to 0 or 1. The
#' coverage cell is `NA`. A central interval of a binary outcome has no
#' useful width.
#' @noRd
brier_cell <- function(truth, fc) {
  c((truth - mean(fc))^2, NA_real_)
}


#' Log score, `-log f(y)` for a kernel density estimate of the draws
#' @noRd
logs_cell <- function(truth, fc) {
  s <- scoringRules::logs_sample(
    y = truth, dat = fc, show_messages = FALSE
  )
  c(s, NA_real_)
}


#' Dawid-Sebastiani score, `((y - mu) / sigma)^2 + 2 log(sigma)`
#'
#' It depends on the predictive mean and standard deviation alone.
#' @noRd
dss_cell <- function(truth, fc) {
  s <- scoringRules::dss_sample(y = truth, dat = fc)
  c(s, NA_real_)
}


#' Quantile (pinball) score at level `alpha`
#'
#' It is strictly proper for the `alpha` quantile. The CRPS integrates
#' it over every level.
#' @noRd
qs_cell <- function(truth, fc, alpha = 0.5) {
  s <- scoringRules::qs_sample(
    y = truth, dat = fc, alpha = alpha, show_messages = FALSE
  )
  c(s, NA_real_)
}


#' Threshold-weighted CRPS
#'
#' The chaining function `v(x) = min(max(x, lower), upper)` scores the
#' forecast inside `[lower, upper]`. The default bounds give the CRPS.
#' @noRd
twcrps_cell <- function(truth, fc, lower = -Inf, upper = Inf) {
  s <- scoringRules::twcrps_sample(
    y = truth, dat = fc, a = lower, b = upper, show_messages = FALSE
  )
  c(s, NA_real_)
}


# ----- Per-horizon multivariate kernels ---------------------------

#' Forecast draws prepared for a multivariate score
#'
#' Drops every draw column holding a missing value and applies the log
#' offset when `log = TRUE`.
#'
#' @param truth Numeric vector of observed values, one per series
#' @param fc Numeric matrix of forecast draws, one row per series
#' @param log Logical. `TRUE` scores on the log scale
#' @return List holding the prepared `truth` and `fc`
#' @noRd
clean_fc_for_es <- function(truth, fc, log = FALSE) {
  checkmate::assert_atomic_vector(truth)
  checkmate::assert_matrix(fc)
  checkmate::assert_flag(log)
  fc <- fc[, colSums(is.na(fc)) == 0L, drop = FALSE]
  if (log) {
    truth <- log_offset(truth)
    fc <- log_offset(fc)
  }
  list(truth = truth, fc = fc)
}


#' Energy score
#' @noRd
energy_cell <- function(truth, fc, log = FALSE) {
  cleaned <- clean_fc_for_es(truth, fc, log = log)
  scoringRules::es_sample(y = cleaned$truth, dat = cleaned$fc)
}


#' Threshold-weighted energy score
#'
#' The chaining function of the threshold-weighted CRPS, applied to
#' each component.
#' @noRd
twenergy_cell <- function(truth, fc, lower = -Inf, upper = Inf) {
  cleaned <- clean_fc_for_es(truth, fc)
  scoringRules::twes_sample(
    y = cleaned$truth, dat = cleaned$fc, a = lower, b = upper
  )
}


#' Variogram score of order 0.5 (Scheuerer and Hamill 2015)
#'
#' Each pair of series takes the mean of their `weights`.
#' @noRd
variogram_cell <- function(truth, fc, log = FALSE, weights = NULL) {
  cleaned <- clean_fc_for_es(truth, fc, log = log)
  w_vs <- if (!is.null(weights)) {
    outer(weights, weights, function(x, y) (x + y) / 2)
  }
  scoringRules::vs_sample(y = cleaned$truth, dat = cleaned$fc,
                          w_vs = w_vs, p = 0.5)
}
