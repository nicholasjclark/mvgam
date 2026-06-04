# Numeric scoring kernels for posterior forecast evaluation.
#
# Six scorers, organised into two groups:
#   Univariate per-(time) cell : CRPS, DRPS, SIS, Brier
#   Multivariate per-horizon   : Energy, Variogram
#
# DRY: every per-cell scorer wraps a `(truth, fc_col, ...)` ->
# `c(score, in_interval)` kernel via apply_univariate_score(),
# which handles the NA guard, the output shape, and column
# naming. Multivariate scorers wrap a `(truth_at_h, fc_at_h,
# ...)` -> scalar kernel via apply_multivariate_score(), which
# stacks the per-series forecasts at each horizon and dispatches.
#
# CRPS uses scoringRules::crps_sample under the EDF method (the
# same target master's hand-rolled `crps_edf` was computing);
# energy uses scoringRules::es_sample. Variogram is kept in-tree
# because scoringRules::vs_sample uses a mean pairwise sqrt-
# difference, whereas the established mvgam choice is the median
# (more robust to skewed forecast distributions).


# ----- Shared utilities -------------------------------------------

# Log offset for non-negative response variables so the `log =
# TRUE` toggle on every scorer doesn't blow up at zero.
#'@noRd
log_offset <- function(x, eps = 0.001) log(x + eps)


# Empirical central PI coverage indicator: 1 if `truth` falls
# inside the [(1 - w)/2, 1 - (1 - w)/2] quantiles of `fc_col`,
# 0 otherwise. Used by CRPS / DRPS / SIS cell kernels.
#'@noRd
in_central_pi <- function(fc_col, truth, interval_width = 0.9) {
  alpha <- (1 - interval_width) / 2
  iv <- stats::quantile(
    fc_col, probs = c(alpha, 1 - alpha), na.rm = TRUE
  )
  as.integer(truth >= iv[1L] && truth <= iv[2L])
}


# Apply a per-cell scoring kernel across a forecast horizon.
# `score_fn(truth_i, fc_col_i, ...)` must return a length-2
# numeric `c(score, in_interval)`. NA truths are skipped (the
# corresponding output row stays NA). Returns a `[h, 2]` matrix
# with column names `score` / `in_interval`.
#'@noRd
apply_univariate_score <- function(truth, fc, score_fn, ...) {
  checkmate::assert_numeric(truth)
  checkmate::assert_matrix(fc, ncols = length(truth))
  out <- matrix(NA_real_, nrow = length(truth), ncol = 2L)
  keep <- which(!is.na(truth))
  for (i in keep) {
    out[i, ] <- score_fn(truth[i], fc[, i], ...)
  }
  colnames(out) <- c("score", "in_interval")
  out
}


# Stack a list of per-series `[ndraws, h]` forecast matrices
# into a list of `[n_series, ndraws]` matrices indexed by
# horizon. Shared by the energy and variogram public scorers.
#'@noRd
stack_fcs_per_horizon <- function(fcs) {
  h <- ncol(fcs[[1L]])
  lapply(seq_len(h), function(t) {
    do.call(rbind, lapply(fcs, function(fc) fc[, t]))
  })
}


# Apply a per-horizon multivariate kernel across the horizon.
# `score_fn(truth_vec, fc_mat, ...)` must return a scalar
# score; `truth_vec` is `[n_series]`, `fc_mat` is `[n_series,
# ndraws]`. Returns a length-h numeric vector.
#'@noRd
apply_multivariate_score <- function(truths, fcs, score_fn, ...) {
  checkmate::assert_matrix(truths)
  checkmate::assert_list(fcs, min.len = 1L)
  fcs_per_h <- stack_fcs_per_horizon(fcs)
  h <- ncol(truths)
  vapply(seq_len(h), function(t) {
    score_fn(truths[, t], fcs_per_h[[t]], ...)
  }, numeric(1L))
}


# ----- Per-cell univariate kernels --------------------------------

# CRPS via the empirical-CDF method (matches master's hand-rolled
# `crps_edf`; scoringRules computes the same target).
#'@noRd
crps_cell <- function(truth, fc, interval_width = 0.9,
                        log = FALSE) {
  if (log) {
    truth <- log_offset(truth)
    fc <- log_offset(fc)
  }
  s <- scoringRules::crps_sample(y = truth, dat = fc,
                                    method = "edf")
  c(s, in_central_pi(fc, truth, interval_width))
}


# DRPS for discrete (count) data. The empirical CDF of `fc` is
# evaluated over an integer support extended past either the
# log-transformed max (when `log = TRUE`) or the truth + a tail
# offset large enough that the squared-difference tail is
# negligible.
#'@noRd
drps_cell <- function(truth, fc, interval_width = 0.9,
                        log = FALSE) {
  if (log) {
    truth <- log_offset(truth)
    fc <- log_offset(fc)
    upper <- max(c(truth, fc), na.rm = TRUE) + 5
  } else {
    upper <- max(
      c(truth, stats::quantile(fc, 0.99, na.rm = TRUE)),
      na.rm = TRUE
    ) + 1000
  }
  ys <- 0:upper
  Fy <- stats::ecdf(fc)
  s <- sum((as.integer(ys - truth >= 0L) - Fy(ys))^2)
  c(s, in_central_pi(fc, truth, interval_width))
}


# Scaled Interval Score (Gneiting-Raftery): width of the central
# PI plus an `alpha`-scaled penalty for misses on either side.
#'@noRd
sis_cell <- function(truth, fc, interval_width = 0.9,
                       log = FALSE) {
  if (log) {
    truth <- log_offset(truth)
    fc <- log_offset(fc)
  }
  alpha <- 1 - interval_width
  lo_p <- alpha / 2
  hi_p <- 1 - lo_p
  creds <- stats::quantile(
    fc, probs = c(lo_p, hi_p), na.rm = TRUE
  )
  lo <- creds[1L]
  hi <- creds[2L]
  err_up <- truth - hi
  err_lo <- lo - truth
  s <- (hi - lo) +
    (2 / alpha) * (max(err_up, 0) + max(err_lo, 0))
  c(unname(s), in_central_pi(fc, truth, interval_width))
}


# Brier-style mean squared error of the forecast draws against
# the truth. Coverage cell is NA because the canonical Brier
# score targets binary outcomes, where a central PI is
# ill-defined.
#'@noRd
brier_cell <- function(truth, fc, ...) {
  c(mean((truth - fc)^2), NA_real_)
}


# Logarithmic score: -log f(y) on a kernel density estimate of
# the posterior draws. Strictly proper, density-local, and the
# direct bridge to ELPD / LOO / WAIC summaries. Coverage cell is
# NA -- the log score does not target a central PI.
#'@noRd
logs_cell <- function(truth, fc, ...) {
  s <- scoringRules::logs_sample(
    y = truth, dat = fc, show_messages = FALSE
  )
  c(s, NA_real_)
}


# Dawid-Sebastiani score: ((y - mu) / sigma)^2 + 2 * log(sigma).
# Cheap, moment-based, depends only on the first two predictive
# moments. Useful as a calibration diagnostic that separates
# sharpness (2 log sigma) from the standardised error term.
#'@noRd
dss_cell <- function(truth, fc, ...) {
  s <- scoringRules::dss_sample(y = truth, dat = fc)
  c(s, NA_real_)
}


# Quantile (pinball) score at level `alpha`. Strictly proper
# for the alpha-quantile functional. Targets one quantile;
# CRPS integrates this across all alpha, so the per-cell
# pinball lets users evaluate asymmetric / tail-focused
# horizons directly.
#'@noRd
qs_cell <- function(truth, fc, alpha = 0.5, ...) {
  s <- scoringRules::qs_sample(
    y = truth, dat = fc, alpha = alpha, show_messages = FALSE
  )
  c(s, NA_real_)
}


# Threshold-weighted CRPS via the chaining function
# v(x) = min(max(x, lower), upper). Defaults to plain CRPS;
# raise `lower` or lower `upper` to focus scoring on the
# corresponding tail (exceedance forecasts, management
# thresholds).
#'@noRd
twcrps_cell <- function(truth, fc, lower = -Inf, upper = Inf,
                          ...) {
  s <- scoringRules::twcrps_sample(
    y = truth, dat = fc, a = lower, b = upper,
    show_messages = FALSE
  )
  c(s, NA_real_)
}


# ----- Per-horizon multivariate kernels ---------------------------

# Internal: shared NA-column drop + optional log offset for the
# multivariate score cells (energy, twenergy).
#'@noRd
clean_fc_for_es <- function(truth, fc, log = FALSE) {
  has_na <- apply(fc, 2L, function(x) any(is.na(x)))
  fc <- fc[, !has_na, drop = FALSE]
  if (log) {
    truth <- log_offset(truth)
    fc <- log_offset(fc)
  }
  list(truth = truth, fc = fc)
}


# Multivariate Energy score via scoringRules.
#'@noRd
energy_cell <- function(truth, fc, log = FALSE) {
  insight::check_if_installed(
    "scoringRules",
    reason = "to calculate energy scores"
  )
  cleaned <- clean_fc_for_es(truth, fc, log = log)
  scoringRules::es_sample(y = cleaned$truth, dat = cleaned$fc)
}


# Threshold-weighted multivariate Energy score. Same chaining
# convention as twCRPS: focuses scoring on the joint region
# whose components fall inside `[lower, upper]`.
#'@noRd
twenergy_cell <- function(truth, fc, lower = -Inf, upper = Inf,
                            log = FALSE) {
  insight::check_if_installed(
    "scoringRules",
    reason = "to calculate threshold-weighted energy scores"
  )
  cleaned <- clean_fc_for_es(truth, fc, log = log)
  scoringRules::twes_sample(
    y = cleaned$truth, dat = cleaned$fc,
    a = lower, b = upper
  )
}


# Variogram score with the median pairwise sqrt-difference
# across forecast draws (more robust than the mean for skewed
# predictives, which is the established mvgam choice over
# scoringRules::vs_sample).
#
# Vectorised via outer() for the truth-side pairwise structure;
# the forecast-side median is filled into the upper triangle
# only (the lower triangle is implied by symmetry and never
# read again because the final sum runs over upper.tri()).
#'@noRd
variogram_cell <- function(truth, fc, log = FALSE,
                             weights = NULL) {
  if (log) {
    truth <- log_offset(truth)
    fc <- log_offset(fc)
  }
  n <- length(truth)
  W <- if (is.null(weights)) {
    matrix(1, n, n)
  } else {
    outer(weights, weights, function(x, y) (x + y) / 2)
  }
  Vd <- outer(truth, truth, function(x, y) abs(x - y)^0.5)
  Vfc <- matrix(0, n, n)
  if (n >= 2L) {
    for (i in seq_len(n - 1L)) {
      for (j in (i + 1L):n) {
        Vfc[i, j] <- stats::quantile(
          abs(fc[i, ] - fc[j, ])^0.5, 0.5, na.rm = TRUE
        )
      }
    }
  }
  pair_sq <- W * (Vd - Vfc)^2
  sum(pair_sq[upper.tri(pair_sq)])
}


# ----- Public per-horizon scorers ---------------------------------

# Univariate scorers: pass-through to apply_univariate_score().
# `truth` is a length-h numeric vector (the held-out responses
# for one series at the forecast horizon); `fc` is a `[ndraws,
# h]` matrix of posterior predictive draws for the same cells.
#'@noRd
crps_mcmc_object <- function(truth, fc, interval_width = 0.9,
                               log = FALSE) {
  apply_univariate_score(truth, fc, crps_cell,
                           interval_width = interval_width,
                           log = log)
}

#'@noRd
drps_mcmc_object <- function(truth, fc, interval_width = 0.9,
                               log = FALSE) {
  apply_univariate_score(truth, fc, drps_cell,
                           interval_width = interval_width,
                           log = log)
}

#'@noRd
sis_mcmc_object <- function(truth, fc, interval_width = 0.9,
                              log = FALSE) {
  apply_univariate_score(truth, fc, sis_cell,
                           interval_width = interval_width,
                           log = log)
}

#'@noRd
brier_mcmc_object <- function(truth, fc, ...) {
  apply_univariate_score(truth, fc, brier_cell)
}

#'@noRd
logs_mcmc_object <- function(truth, fc, ...) {
  apply_univariate_score(truth, fc, logs_cell)
}

#'@noRd
dss_mcmc_object <- function(truth, fc, ...) {
  apply_univariate_score(truth, fc, dss_cell)
}

#'@noRd
qs_mcmc_object <- function(truth, fc, alpha = 0.5, ...) {
  apply_univariate_score(truth, fc, qs_cell, alpha = alpha)
}

#'@noRd
twcrps_mcmc_object <- function(truth, fc, lower = -Inf,
                                 upper = Inf, ...) {
  apply_univariate_score(truth, fc, twcrps_cell,
                           lower = lower, upper = upper)
}


# Multivariate scorers: pass-through to apply_multivariate_score().
# `truths` is a `[n_series, h]` matrix; `fcs` is a length-
# n_series list of `[ndraws, h]` forecast matrices.
#'@noRd
energy_mcmc_object <- function(truths, fcs, log = FALSE,
                                 weights = NULL) {
  apply_multivariate_score(truths, fcs, energy_cell, log = log)
}

#'@noRd
variogram_mcmc_object <- function(truths, fcs, log = FALSE,
                                    weights = NULL) {
  apply_multivariate_score(truths, fcs, variogram_cell,
                             log = log, weights = weights)
}

#'@noRd
twenergy_mcmc_object <- function(truths, fcs, lower = -Inf,
                                    upper = Inf, log = FALSE) {
  apply_multivariate_score(truths, fcs, twenergy_cell,
                             lower = lower, upper = upper,
                             log = log)
}
