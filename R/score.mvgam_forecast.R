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


#' The scores that take `log = TRUE`
#'
#' The `brier` score grades a binary outcome. The threshold-weighted
#' scores set their thresholds on the untransformed scale. The `logs`,
#' `dss` and `qs` scores grade the draws as they are.
#'
#' @noRd
scores_honouring_log <- c("crps", "drps", "sis", "energy", "variogram")


#' The scores that grade every series jointly
#'
#' `score()` returns each as one `all_series` row and leaves the
#' per-series scores `NA`.
#'
#' @noRd
joint_forecast_scores <- c("energy", "variogram", "twenergy")


#' The axis label of each score
#'
#' @param score Character name of a score
#' @return Character scalar
#' @noRd
score_label <- function(score) {
  checkmate::assert_string(score)
  labels <- c(crps = "CRPS", drps = "DRPS", sis = "Scaled interval score",
              brier = "Brier score", logs = "Log score",
              dss = "Dawid-Sebastiani score", qs = "Quantile score",
              twcrps = "Threshold-weighted CRPS", energy = "Energy score",
              variogram = "Variogram score",
              twenergy = "Threshold-weighted energy score")
  if (score %in% names(labels)) labels[[score]] else score
}


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
#'     PI plus a penalty for misses scaled by its miscoverage
#'     rate, `1 - interval_width`.}
#'   \item{`"brier"`}{Brier score for a binary outcome: the squared
#'     difference between the observation and the mean of the draws,
#'     which estimates the forecast probability.}
#'   \item{`"logs"`}{Logarithmic score `-log f(y)` via a kernel
#'     density estimate; bridges to ELPD / LOO.}
#'   \item{`"dss"`}{Dawid-Sebastiani score: cheap moment-based
#'     proper score depending only on predictive mean / SD.}
#'   \item{`"qs"`}{Quantile (pinball) loss at `quantile_level`.}
#'   \item{`"twcrps"`}{Threshold-weighted CRPS focusing scoring
#'     on the `[lower, upper]` region.}
#'   \item{`"energy"`}{Multivariate Energy score across series.}
#'   \item{`"variogram"`}{Variogram score of order 0.5 across
#'     series (Scheuerer & Hamill, 2015).}
#'   \item{`"twenergy"`}{Threshold-weighted Energy score.}
#' }
#'
#' @param object An `mvgam_forecast` object whose `type` is
#'   `"response"` or `"expected"`. The held-out observations are then
#'   on the scale of the forecasts.
#' @param score Character; one of the names above.
#' @param interval_width Width of the central PI that sets the
#'   per-series `in_interval` indicator. It also sets the SIS penalty
#'   width. Computed for `"crps"`, `"drps"` and `"sis"`; the other
#'   scores report `in_interval` as `NA`.
#' @param quantile_level Quantile the `"qs"` pinball loss is
#'   taken at. Defaults to `0.5`, the median. Unrelated to the
#'   central interval the other scorers use, which
#'   `interval_width` sets.
#' @param lower,upper Threshold bounds for `"twcrps"` /
#'   `"twenergy"`. Defaults `-Inf` / `Inf` recover the
#'   unweighted scores.
#' @param log Logical; apply `log(x + 0.001)` to truth and
#'   forecast draws before scoring. Taken by `"crps"`, `"drps"`,
#'   `"sis"`, `"energy"` and `"variogram"`. Passing `TRUE` with any
#'   other score is an error. `"brier"` scores a binary outcome.
#'   `"twcrps"` and `"twenergy"` set their thresholds on the
#'   untransformed scale.
#' @param weights Optional length-`n_series` numeric vector of
#'   per-series weights for `"variogram"`.
#' @param ... Unused. Anything passed here is refused.
#'
#' @return A named `list` with one entry per series plus a
#'   trailing `all_series` summary. Per-series entries are
#'   `data.frame`s with columns `score`, `in_interval`,
#'   `interval_width`, `eval_horizon` and `score_type`. The
#'   `score_type` column echoes the requested `score`. The
#'   multivariate scorers' per-series rows hold coverage diagnostics
#'   only, with `score` set to `NA`. The `all_series` entry holds the
#'   per-horizon aggregate. For univariate scorers it sums the scores
#'   over the series at each forecast time, with `score_type`
#'   prefixed `"sum_"`. For multivariate scorers it holds the joint
#'   per-horizon score. Series forecast at their own times, as
#'   `CAR()` allows, are summed at the times they share. The
#'   `eval_horizon` column then counts the times any series is
#'   forecast at. A multivariate score needs every series at the same
#'   forecast times.
#'
#' @seealso [forecast.mvgam()] produces the forecast.
#'   [compare_scores()] scores several forecasts with one rule.
#'   [ensemble.mvgam_forecast()] combines forecasts. The
#'   `r mvgam_article("forecast_evaluation", "forecast evaluation article")`
#'   compares models by their scores at each horizon.
#'
#' @references Gneiting, T. and Raftery, A. E. (2007). Strictly
#'   Proper Scoring Rules, Prediction, and Estimation.
#'   *Journal of the American Statistical Association*,
#'   102(477), 359-378.
#'
#'   Scheuerer, M. and Hamill, T. M. (2015). Variogram-based proper
#'   scoring rules for probabilistic forecasts of multivariate
#'   quantities. *Monthly Weather Review*, 143(4), 1321-1334.
#'
#' @examples
#' \dontrun{
#' set.seed(11)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                      n_timepoints = 120L, trend_model = AR(),
#'                      proportional_train = 0.75)
#'
#' mod <- mvgam(
#'   y ~ s(x),
#'   trend_formula = ~ AR(p = 1),
#'   data = simdat$data_train,
#'   newdata = simdat$data_test,
#'   family = poisson(),
#'   chains = 2, silent = 2
#' )
#'
#' # Score requires a forecast object with held-out test draws.
#' fc <- forecast(mod)
#' sc <- score(fc, score = "crps")
#' head(sc[[1L]])
#' }
#'
#' @family forecasting
#' @method score mvgam_forecast
#' @export
#' @export score
score.mvgam_forecast <- function(object,
                                   score = "crps",
                                   interval_width = 0.9,
                                   quantile_level = 0.5,
                                   lower = -Inf,
                                   upper = Inf,
                                   log = FALSE,
                                   weights = NULL,
                                   ...) {
  checkmate::assert_class(object, "mvgam_forecast")
  rlang::check_dots_empty()
  # A long-frame wrapper is scored per response
  object <- forecast_parts(object)
  if (is_forecast_list(object)) {
    return(lapply(
      object, score.mvgam_forecast,
      score = score, interval_width = interval_width,
      quantile_level = quantile_level, lower = lower, upper = upper,
      log = log, weights = weights, ...
    ))
  }
  univariate <- c("crps", "drps", "sis", "brier",
                   "logs", "dss", "qs", "twcrps")
  score <- match.arg(score, c(univariate, joint_forecast_scores))
  checkmate::assert_number(interval_width,
                             lower = 0.05, upper = 0.95)
  checkmate::assert_number(quantile_level, lower = 0, upper = 1)
  checkmate::assert_number(lower)
  checkmate::assert_number(upper)
  checkmate::assert_flag(log)

  if (isTRUE(log) && !score %in% scores_honouring_log) {
    stop(insight::format_error(c(
      paste0("'log = TRUE' does not apply to score '", score, "'."),
      x = paste0(
        "Only ", paste0("'", scores_honouring_log, "'", collapse = ", "),
        " take 'log = TRUE'."
      ),
      i = paste0("To score on the log scale, transform the response ",
                 "before fitting or choose one of those scores.")
    )), call. = FALSE)
  }

  validate_scoreable_forecast(object, score)

  series_names <- as.character(object$series_names)

  if (score %in% univariate) {
    return(score_univariate(
      object, series_names, score,
      interval_width = interval_width,
      quantile_level = quantile_level, lower = lower,
      upper = upper, log = log
    ))
  }

  score_multivariate(
    object, series_names, score,
    interval_width = interval_width,
    lower = lower, upper = upper, log = log, weights = weights
  )
}


# ----- Validation -------------------------------------------------

#' Refuse a forecast that cannot be scored
#'
#' Both score branches call it. A `link` or `trend` forecast is on the
#' link scale and cannot be scored against observations. A forecast
#' needs held-out forecasts and observations. The `brier` score needs a
#' Bernoulli family for every series.
#'
#' @param object Object of class `mvgam_forecast`
#' @param score Character name of the score
#' @return `NULL`, invisibly, or an error
#' @noRd
validate_scoreable_forecast <- function(object, score) {
  checkmate::assert_list(object)
  checkmate::assert_string(score)
  if (object$type %in% c("link", "trend")) {
    stop(insight::format_error(c(
      "Scoring requires a 'response' or 'expected' forecast.",
      x = paste0("A '", object$type, "' forecast holds link-scale draws."),
      i = "Re-run 'forecast()' with type = 'response' or 'expected'."
    )), call. = FALSE)
  }
  if (is.null(object$forecasts) ||
      is.null(object$test_observations)) {
    stop(insight::format_error(c(
      "'object' contains no held-out forecasts to score.",
      i = "Pass 'newdata' covering held-out times to 'forecast()'."
    )), call. = FALSE)
  }
  # A wide fit's forecast holds one family per series. Every scored
  # series must be Bernoulli.
  families <- object$family
  if (identical(score, "brier") &&
      (!length(families) || !all(families == "bernoulli"))) {
    stop(insight::format_error(c(
      "'brier' is only defined for 'bernoulli' families.",
      x = paste0("Got family = ",
                 paste0("'", unique(families), "'", collapse = ", "),
                 "."),
      i = "Use 'crps', 'drps' or 'logs' for other families."
    )), call. = FALSE)
  }
  invisible(NULL)
}


# ----- Univariate branch ------------------------------------------

#' Univariate scores for each series and their sum over series
#'
#' @param object Object of class `mvgam_forecast`
#' @param series_names Character vector of the series to score
#' @param score Character name of a univariate score
#' @param interval_width Numeric width of the central interval
#' @param quantile_level Numeric quantile level of the `qs` score
#' @param lower Numeric lower threshold of the `twcrps` score
#' @param upper Numeric upper threshold of the `twcrps` score
#' @param log Logical. `TRUE` scores on the log scale
#' @return Named list of score data frames, one per series, followed by
#'   the `all_series` sum
#' @noRd
score_univariate <- function(object, series_names, score,
                               interval_width, quantile_level, lower,
                               upper, log) {
  checkmate::assert_list(object)
  checkmate::assert_character(series_names)
  checkmate::assert_string(score)
  checkmate::assert_number(interval_width)
  checkmate::assert_number(quantile_level)
  checkmate::assert_number(lower)
  checkmate::assert_number(upper)
  checkmate::assert_flag(log)
  series_score <- lapply(series_names, function(lv) {
    truth <- object$test_observations[[lv]]
    fc <- object$forecasts[[lv]]
    if (!has_heldout(truth, fc)) {
      return(empty_score_df())
    }
    kernel_out <- dispatch_univariate_score(
      score, truth, fc,
      interval_width = interval_width,
      quantile_level = quantile_level, lower = lower,
      upper = upper, log = log
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
    series_score, score, object$test_times[series_names]
  )
  series_score
}


#' Whether a series holds held-out observations and forecasts
#'
#' @param truth Numeric vector of held-out observations, or `NULL`
#' @param fc Numeric matrix of forecast draws, or `NULL`
#' @return Logical scalar
#' @noRd
has_heldout <- function(truth, fc) {
  length(truth) > 0L && !is.null(fc) && ncol(fc) > 0L
}


#' An empty score data frame for a series without held-out data
#'
#' Keeps the result shape uniform across series for callers that bind
#' or map over them.
#'
#' @return Zero-row data frame holding the score columns
#' @noRd
empty_score_df <- function() {
  data.frame(
    score = numeric(0L),
    in_interval = numeric(0L),
    interval_width = numeric(0L),
    eval_horizon = integer(0L),
    score_type = character(0L),
    stringsAsFactors = FALSE
  )
}


#' Per-series scores summed at each forecast time
#'
#' Horizon `k` is the `k`th time any series is forecast at. Each series
#' adds its score at the times it holds. A series without recorded
#' forecast times is summed by position.
#'
#' @param series_score Named list of per-series score data frames
#' @param score Character name of the score
#' @param test_times Named list of forecast times per series, or `NULL`
#' @return Data frame holding `score`, `eval_horizon` and `score_type`
#' @noRd
sum_univariate_horizon <- function(series_score, score, test_times) {
  checkmate::assert_list(series_score)
  checkmate::assert_string(score)
  checkmate::assert_list(test_times, null.ok = TRUE)
  scored <- names(series_score)[
    vapply(series_score, nrow, integer(1L)) > 0L
  ]
  if (length(scored) == 0L) {
    return(data.frame(
      score = numeric(0L), eval_horizon = integer(0L),
      score_type = character(0L), stringsAsFactors = FALSE
    ))
  }
  # An object that records no forecast times is summed by position
  times <- lapply(scored, function(lv) {
    n <- nrow(series_score[[lv]])
    if (is.null(test_times[[lv]])) {
      return(as.numeric(seq_len(n)))
    }
    as.numeric(test_times[[lv]])[seq_len(n)]
  })
  time <- unlist(times, use.names = FALSE)
  value <- unlist(lapply(scored, function(lv) series_score[[lv]]$score),
                  use.names = FALSE)
  total <- tapply(value, factor(time, levels = sort(unique(time))),
                  sum_measured)
  data.frame(
    score = as.numeric(total),
    eval_horizon = seq_along(total),
    score_type = paste0("sum_", score),
    stringsAsFactors = FALSE
  )
}


#' Whether the scored series share their forecast times
#'
#' @param test_times List of forecast times per series, or `NULL`
#' @return Logical scalar
#' @noRd
shares_forecast_times <- function(test_times) {
  checkmate::assert_list(test_times, null.ok = TRUE)
  if (length(test_times) == 0L) {
    return(TRUE)
  }
  first <- as.numeric(test_times[[1L]])
  all(vapply(test_times, function(t) {
    isTRUE(all.equal(as.numeric(t), first))
  }, logical(1L)))
}


# ----- Multivariate branch ----------------------------------------

#' A joint multivariate score with per-series interval coverage
#'
#' Each per-series data frame records interval coverage and leaves its
#' `score` column missing. The `all_series` element holds the joint
#' score. Every scored series must share one forecast horizon.
#'
#' @param object Object of class `mvgam_forecast`
#' @param series_names Character vector of the series to score
#' @param score Character name of a multivariate score
#' @param interval_width Numeric width of the central interval
#' @param lower Numeric lower threshold of the `twenergy` score
#' @param upper Numeric upper threshold of the `twenergy` score
#' @param log Logical. `TRUE` scores on the log scale
#' @param weights Numeric per-series weights of the `variogram` score, or
#'   `NULL`
#' @return Named list of per-series data frames, followed by the joint
#'   `all_series` score
#' @noRd
score_multivariate <- function(object, series_names, score,
                                 interval_width, lower, upper,
                                 log, weights) {
  checkmate::assert_list(object)
  checkmate::assert_character(series_names)
  checkmate::assert_string(score)
  checkmate::assert_number(interval_width)
  checkmate::assert_number(lower)
  checkmate::assert_number(upper)
  checkmate::assert_flag(log)
  checkmate::assert_numeric(weights, null.ok = TRUE)
  truths_list <- lapply(series_names, function(lv) {
    object$test_observations[[lv]]
  })
  fcs <- lapply(series_names, function(lv) {
    object$forecasts[[lv]]
  })
  ok <- mapply(has_heldout, truths_list, fcs)
  if (!any(ok)) {
    stop(insight::format_error(
      "No held-out series available for multivariate scoring."
    ), call. = FALSE)
  }
  # A joint score is of one vector per time, with every series in it
  if (length(unique(vapply(fcs[ok], ncol, integer(1L)))) > 1L ||
      !shares_forecast_times(object$test_times[series_names[ok]])) {
    stop(insight::format_error(c(
      "Multivariate scoring requires a shared forecast horizon.",
      i = "Give every series in 'newdata' the same forecast times.",
      i = paste0("'crps' and the other univariate scores take series ",
                 "at their own times.")
    )), call. = FALSE)
  }

  series_score <- lapply(seq_along(series_names), function(i) {
    truth <- truths_list[[i]]
    fc <- fcs[[i]]
    if (!ok[i]) {
      return(empty_score_df())
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


# ----- Per-score routing ------------------------------------------

#' One univariate score computed by its kernel
#'
#' Each kernel receives the arguments it uses.
#'
#' @param score Character name of a univariate score
#' @param truth Numeric vector of observed values over the horizon
#' @param fc Numeric matrix of forecast draws, one column per horizon
#' @param interval_width Numeric width of the central interval
#' @param quantile_level Numeric quantile level of the `qs` score
#' @param lower Numeric lower threshold of the `twcrps` score
#' @param upper Numeric upper threshold of the `twcrps` score
#' @param log Logical. `TRUE` scores on the log scale
#' @return Numeric matrix holding the `score` and `in_interval` columns
#' @noRd
dispatch_univariate_score <- function(score, truth, fc,
                                        interval_width, quantile_level,
                                        lower, upper, log) {
  checkmate::assert_string(score)
  checkmate::assert_atomic_vector(truth)
  checkmate::assert_matrix(fc)
  checkmate::assert_number(interval_width)
  checkmate::assert_number(quantile_level)
  checkmate::assert_number(lower)
  checkmate::assert_number(upper)
  checkmate::assert_flag(log)
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
    "qs" = qs_mcmc_object(truth, fc, quantile_level = quantile_level),
    "twcrps" = twcrps_mcmc_object(
      truth, fc, lower = lower, upper = upper
    )
  )
}


#' One multivariate score computed by its kernel
#'
#' @param score Character name of a multivariate score
#' @param truths Numeric matrix of observed values, one row per series
#' @param fcs List of forecast draw matrices, one per series
#' @param lower Numeric lower threshold of the `twenergy` score
#' @param upper Numeric upper threshold of the `twenergy` score
#' @param log Logical. `TRUE` scores on the log scale
#' @param weights Numeric per-series weights of the `variogram` score, or
#'   `NULL`
#' @return Numeric vector of joint scores, one per horizon
#' @noRd
dispatch_multivariate_score <- function(score, truths, fcs,
                                          lower, upper, log,
                                          weights) {
  checkmate::assert_string(score)
  checkmate::assert_matrix(truths)
  checkmate::assert_list(fcs)
  checkmate::assert_number(lower)
  checkmate::assert_number(upper)
  checkmate::assert_flag(log)
  checkmate::assert_numeric(weights, null.ok = TRUE)
  switch(
    score,
    "energy" = energy_mcmc_object(truths, fcs, log = log),
    "variogram" = variogram_mcmc_object(
      truths, fcs, log = log, weights = weights
    ),
    "twenergy" = twenergy_mcmc_object(
      truths, fcs, lower = lower, upper = upper
    )
  )
}
