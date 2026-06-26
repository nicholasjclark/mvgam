# Public S3 surface for posterior forecast scoring. Wraps the
# per-cell / per-horizon kernels in `R/scoring_kernels.R` with
# input-shape handling (per-series for univariate scorers, joint
# stacking for multivariate ones), validation, and a tidy
# `data.frame`-per-series return shape with an `all_series`
# summary row at the end.


#' Generic for posterior forecast scoring
#'
#' Dispatches to a class-specific method to compute proper
#' scoring rules on a forecast object. See
#' [score.mvgam_forecast] for the mvgam method.
#'
#' @param object Object to score.
#' @param ... Method-specific arguments.
#' @return Method-dependent; see the relevant method.
#' @export
score <- function(object, ...) UseMethod("score")


#' Proper scoring rules for `mvgam_forecast` objects
#'
#' Compute one of eleven proper scoring rules on the held-out
#' forecasts of an `mvgam_forecast` object returned by
#' [forecast.mvgam] or [hindcast.mvgam].
#'
#' @section Available scorers:
#'
#' \describe{
#'   \item{`"crps"`}{Continuous Ranked Probability Score
#'     (Gneiting & Raftery, 2007); default proper score for
#'     continuous predictives.}
#'   \item{`"drps"`}{Discrete Ranked Probability Score; CRPS
#'     specialised to count outcomes.}
#'   \item{`"sis"`}{Scaled Interval Score; width of the central
#'     PI plus an `alpha`-scaled penalty for misses.}
#'   \item{`"brier"`}{Mean squared error of draws against
#'     truth; canonical for binary outcomes.}
#'   \item{`"logs"`}{Logarithmic score `-log f(y)` via a kernel
#'     density estimate; bridges to ELPD / LOO.}
#'   \item{`"dss"`}{Dawid-Sebastiani score: cheap moment-based
#'     proper score depending only on predictive mean / SD.}
#'   \item{`"qs"`}{Quantile (pinball) loss at level `alpha`.}
#'   \item{`"twcrps"`}{Threshold-weighted CRPS focusing scoring
#'     on the `[lower, upper]` region.}
#'   \item{`"energy"`}{Multivariate Energy score across series.}
#'   \item{`"variogram"`}{Variogram score with median pairwise
#'     sqrt-differences across forecast draws.}
#'   \item{`"twenergy"`}{Threshold-weighted Energy score.}
#' }
#'
#' @param object An `mvgam_forecast` object whose `type` is
#'   `"response"` or `"expected"` (so the held-out
#'   observations live on the same scale as the forecasts).
#' @param score Character; one of the names above.
#' @param interval_width Central PI width used by every scorer
#'   for the per-series `in_interval` indicator (and the SIS
#'   penalty width).
#' @param alpha Quantile level for `"qs"` (default median).
#' @param lower,upper Threshold bounds for `"twcrps"` /
#'   `"twenergy"`. Defaults `-Inf` / `Inf` recover the
#'   unweighted scores.
#' @param log Logical; apply `log(x + 0.001)` to truth and
#'   forecast draws before scoring.
#' @param weights Optional length-`n_series` numeric vector of
#'   per-series weights for `"variogram"`.
#' @param ... Currently unused.
#'
#' @return A named `list` with one entry per series plus a
#'   trailing `all_series` summary. Per-series entries are
#'   `data.frame`s with columns `score`, `in_interval`,
#'   `interval_width`, `eval_horizon`, and `score_type` (the
#'   last echoes the requested `score`; the `score` column is
#'   `NA` for the multivariate scorers' per-series rows -- they
#'   carry coverage diagnostics only, with the joint score
#'   sitting in `all_series`). The `all_series` entry holds the
#'   per-horizon aggregate: row-summed scores for univariate
#'   scorers (with `score_type` prefixed `"sum_"`) or the
#'   multivariate per-horizon score itself.
#'
#' @seealso [forecast.mvgam], [hindcast.mvgam],
#'   [mvgam_forecast-class][mvgam]. The CRPS / DRPS / ELPD /
#'   energy / variogram scoring rules and their per-horizon
#'   interpretation are illustrated in the online article
#'   \url{https://nicholasjclark.github.io/mvgam/articles/forecast_evaluation.html}.
#'
#' @references Gneiting, T. and Raftery, A. E. (2007). Strictly
#'   Proper Scoring Rules, Prediction, and Estimation.
#'   *Journal of the American Statistical Association*,
#'   102(477), 359-378.
#'
#' @examples
#' \donttest{
#' set.seed(11)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                      n_timepoints = 80L, trend_model = AR(),
#'                      proportional_train = 0.75)
#'
#' mod <- mvgam(
#'   y ~ s(x),
#'   trend_formula = ~ AR(p = 1),
#'   data    = simdat$data_train,
#'   newdata = simdat$data_test,
#'   family  = poisson(),
#'   chains  = 2, silent = 2
#' )
#'
#' # Score requires a forecast object with held-out test draws.
#' fc <- forecast(mod, newdata = mod$test_data)
#' sc <- score(fc, score = "crps")
#' head(sc[[1L]])
#' }
#'
#' @method score mvgam_forecast
#' @export
#' @export score
score.mvgam_forecast <- function(object,
                                   score = "crps",
                                   interval_width = 0.9,
                                   alpha = 0.5,
                                   lower = -Inf,
                                   upper = Inf,
                                   log = FALSE,
                                   weights = NULL,
                                   ...) {
  checkmate::assert_class(object, "mvgam_forecast")
  univariate <- c("crps", "drps", "sis", "brier",
                   "logs", "dss", "qs", "twcrps")
  multivariate <- c("energy", "variogram", "twenergy")
  score <- match.arg(score, c(univariate, multivariate))
  checkmate::assert_number(interval_width,
                             lower = 0.05, upper = 0.95)
  checkmate::assert_number(alpha, lower = 0, upper = 1)
  checkmate::assert_number(lower)
  checkmate::assert_number(upper)
  checkmate::assert_flag(log)

  validate_scoreable_forecast(object, score)

  series_names <- as.character(object$series_names)

  if (score %in% univariate) {
    return(score_univariate(
      object, series_names, score,
      interval_width = interval_width,
      alpha = alpha, lower = lower, upper = upper, log = log
    ))
  }

  score_multivariate(
    object, series_names, score,
    interval_width = interval_width,
    lower = lower, upper = upper, log = log, weights = weights
  )
}


# ----- Validation -------------------------------------------------

# Internal: shared validation for both score branches. Routes
# the family / type guards (the `brier` / `elpd` / `type` rules
# live here) and the empty-forecast guard.
#'@noRd
validate_scoreable_forecast <- function(object, score) {
  if (identical(object$type, "trend")) {
    stop(insight::format_error(c(
      "Cannot score a 'trend'-type forecast.",
      x = paste0(
        "test_observations are on the response scale; the ",
        "forecast is on the latent-state link scale."
      ),
      i = paste0(
        "Re-run 'forecast()' with type = 'response' or ",
        "'expected'."
      )
    )))
  }
  if (is.null(object$forecasts) ||
      is.null(object$test_observations)) {
    stop(insight::format_error(c(
      "'object' contains no held-out forecasts to score.",
      i = paste0(
        "Pass 'newdata' covering held-out times to 'forecast()'."
      )
    )))
  }
  if (identical(score, "brier") &&
      !identical(object$family, "bernoulli")) {
    stop(insight::format_error(c(
      "'brier' is only defined for 'bernoulli' families.",
      x = paste0("Got family = '", object$family, "'."),
      i = "Use 'crps' / 'drps' / 'logs' for other families."
    )))
  }
  invisible(NULL)
}


# ----- Univariate branch ------------------------------------------

# Internal: per-series univariate scoring. Builds the tidy
# `data.frame` per series plus an `all_series` row-summed
# aggregate.
#'@noRd
score_univariate <- function(object, series_names, score,
                               interval_width, alpha, lower,
                               upper, log) {
  series_score <- lapply(series_names, function(lv) {
    truth <- object$test_observations[[lv]]
    fc <- object$forecasts[[lv]]
    if (is.null(truth) || length(truth) == 0L ||
        is.null(fc) || ncol(fc) == 0L) {
      return(empty_univariate_df(score, interval_width))
    }
    kernel_out <- dispatch_univariate_score(
      score, truth, fc,
      interval_width = interval_width,
      alpha = alpha, lower = lower, upper = upper, log = log
    )
    data.frame(
      score = as.numeric(kernel_out[, "score"]),
      in_interval = as.numeric(kernel_out[, "in_interval"]),
      interval_width = interval_width,
      eval_horizon = seq_len(nrow(kernel_out)),
      score_type = score,
      stringsAsFactors = FALSE
    )
  })
  names(series_score) <- series_names

  series_score$all_series <- sum_univariate_horizon(
    series_score, score
  )
  series_score
}


# Internal: empty per-series stub when a series has no held-out
# data. Keeps the result shape uniform across series so callers
# can rbind / map without special-casing.
#'@noRd
empty_univariate_df <- function(score, interval_width) {
  data.frame(
    score = numeric(0L),
    in_interval = numeric(0L),
    interval_width = numeric(0L),
    eval_horizon = integer(0L),
    score_type = character(0L),
    stringsAsFactors = FALSE
  )
}


# Internal: aggregate per-series score columns into a single
# per-horizon row-summed `all_series` data.frame. Handles
# ragged per-series horizons by NA-padding to the longest.
#'@noRd
sum_univariate_horizon <- function(series_score, score) {
  cols <- lapply(series_score, function(df) df$score)
  cols <- cols[lengths(cols) > 0L]
  if (length(cols) == 0L) {
    return(data.frame(
      score = numeric(0L), eval_horizon = integer(0L),
      score_type = character(0L), stringsAsFactors = FALSE
    ))
  }
  h_max <- max(lengths(cols))
  padded <- vapply(cols, function(v) {
    c(v, rep(NA_real_, h_max - length(v)))
  }, numeric(h_max))
  # vapply collapses to a vector when h_max == 1; force matrix
  # shape so rowSums sees the column dimension.
  if (!is.matrix(padded)) {
    padded <- matrix(padded, nrow = h_max)
  }
  data.frame(
    score = rowSums(padded, na.rm = TRUE),
    eval_horizon = seq_len(h_max),
    score_type = paste0("sum_", score),
    stringsAsFactors = FALSE
  )
}


# ----- Multivariate branch ----------------------------------------

# Internal: multivariate scoring path. Per-series entries carry
# the coverage indicator only (the score column stays NA); the
# joint multivariate score lives in `all_series`.
#'@noRd
score_multivariate <- function(object, series_names, score,
                                 interval_width, lower, upper,
                                 log, weights) {
  truths_list <- lapply(series_names, function(lv) {
    object$test_observations[[lv]]
  })
  fcs <- lapply(series_names, function(lv) {
    object$forecasts[[lv]]
  })
  ok <- vapply(seq_along(series_names), function(i) {
    !(is.null(truths_list[[i]]) ||
        length(truths_list[[i]]) == 0L ||
        is.null(fcs[[i]]) || ncol(fcs[[i]]) == 0L)
  }, logical(1L))
  if (!any(ok)) {
    stop(insight::format_error(
      "No held-out series available for multivariate scoring."
    ))
  }
  if (length(unique(vapply(fcs[ok], ncol, integer(1L)))) > 1L) {
    stop(insight::format_error(c(
      "Multivariate scoring requires a shared forecast horizon.",
      i = "Per-series horizon lengths must match."
    )))
  }

  series_score <- lapply(series_names, function(lv) {
    truth <- object$test_observations[[lv]]
    fc <- object$forecasts[[lv]]
    if (is.null(truth) || length(truth) == 0L ||
        is.null(fc) || ncol(fc) == 0L) {
      return(empty_multivariate_df(score, interval_width))
    }
    cov_indicator <- vapply(
      seq_along(truth), function(j) {
        in_central_pi(fc[, j], truth[j],
                        interval_width = interval_width)
      }, integer(1L)
    )
    data.frame(
      score = rep(NA_real_, length(truth)),
      in_interval = as.numeric(cov_indicator),
      interval_width = interval_width,
      eval_horizon = seq_along(truth),
      score_type = score,
      stringsAsFactors = FALSE
    )
  })
  names(series_score) <- series_names

  truths <- do.call(rbind, truths_list[ok])
  joint <- dispatch_multivariate_score(
    score, truths, fcs[ok],
    lower = lower, upper = upper, log = log, weights = weights
  )
  series_score$all_series <- data.frame(
    score = joint,
    eval_horizon = seq_along(joint),
    score_type = score,
    stringsAsFactors = FALSE
  )
  series_score
}


# Internal: empty per-series stub for the multivariate path.
#'@noRd
empty_multivariate_df <- function(score, interval_width) {
  data.frame(
    score = numeric(0L),
    in_interval = numeric(0L),
    interval_width = numeric(0L),
    eval_horizon = integer(0L),
    score_type = character(0L),
    stringsAsFactors = FALSE
  )
}


# ----- Per-score routing ------------------------------------------

# Internal: route one of the univariate score names to its
# kernel with only the args it consumes -- no unused-arg leakage.
#'@noRd
dispatch_univariate_score <- function(score, truth, fc,
                                        interval_width, alpha,
                                        lower, upper, log) {
  switch(
    score,
    "crps" = crps_mcmc_object(
      truth, fc, interval_width = interval_width, log = log
    ),
    "drps" = drps_mcmc_object(
      truth, fc, interval_width = interval_width, log = log
    ),
    "sis" = sis_mcmc_object(
      truth, fc, interval_width = interval_width, log = log
    ),
    "brier" = brier_mcmc_object(truth, fc),
    "logs" = logs_mcmc_object(truth, fc),
    "dss" = dss_mcmc_object(truth, fc),
    "qs" = qs_mcmc_object(truth, fc, alpha = alpha),
    "twcrps" = twcrps_mcmc_object(
      truth, fc, lower = lower, upper = upper
    )
  )
}


# Internal: route one of the multivariate score names.
#'@noRd
dispatch_multivariate_score <- function(score, truths, fcs,
                                          lower, upper, log,
                                          weights) {
  switch(
    score,
    "energy" = energy_mcmc_object(truths, fcs, log = log),
    "variogram" = variogram_mcmc_object(
      truths, fcs, log = log, weights = weights
    ),
    "twenergy" = twenergy_mcmc_object(
      truths, fcs, lower = lower, upper = upper, log = log
    )
  )
}
