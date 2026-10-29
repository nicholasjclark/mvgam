# score() for forecast objects. It validates the forecast and scores
# each series, or every series jointly, with the kernels of
# `R/scoring_kernels.R`. The result holds one data frame per series
# and an `all_series` summary.


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
#' The `brier` score grades a binary outcome. The `drps` score sums over
#' the integer support of a count. The threshold-weighted scores set
#' their thresholds on the untransformed scale. The `logs`, `dss` and
#' `qs` scores grade the draws as they are.
#'
#' @noRd
scores_honouring_log <- c("crps", "sis", "energy", "variogram")


#' The scores that grade each series on its own
#'
#' @noRd
univariate_forecast_scores <- c("crps", "drps", "sis", "brier", "logs",
                                "dss", "qs", "twcrps")


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
#' Scores the held-out forecasts of an `mvgam_forecast` object from
#' [forecast.mvgam()] or [hindcast.mvgam()] against the observations,
#' with one of eleven scoring rules. Lower scores are better.
#'
#' @section Available scores:
#'
#' \describe{
#'   \item{`"crps"`}{Continuous ranked probability score (Gneiting and
#'     Raftery 2007) of the empirical distribution of the draws.}
#'   \item{`"drps"`}{Discrete ranked probability score, the CRPS of a
#'     count summed over its integer support.}
#'   \item{`"sis"`}{Scaled interval score: the width of the central
#'     interval plus a penalty for an observation outside it, scaled by
#'     `2 / (1 - interval_width)`.}
#'   \item{`"brier"`}{Brier score of a binary outcome: the squared
#'     difference between the observation and the mean of the draws.}
#'   \item{`"logs"`}{Log score, `-log f(y)` for a kernel density
#'     estimate of the draws.}
#'   \item{`"dss"`}{Dawid-Sebastiani score, which depends on the
#'     predictive mean and standard deviation alone.}
#'   \item{`"qs"`}{Quantile (pinball) score at `quantile_level`.}
#'   \item{`"twcrps"`}{Threshold-weighted CRPS, which scores the
#'     forecast inside `[lower, upper]`.}
#'   \item{`"energy"`}{Energy score of the series jointly.}
#'   \item{`"variogram"`}{Variogram score of order 0.5 of the series
#'     jointly (Scheuerer and Hamill 2015).}
#'   \item{`"twenergy"`}{Threshold-weighted energy score.}
#' }
#'
#' @param object An `mvgam_forecast` object whose `type` is
#'   `"response"` or `"expected"`. The held-out observations are then
#'   on the scale of the forecasts.
#' @param score Character; one of the names under 'Available scores'.
#'   `"drps"` suits counts, `"crps"` continuous data and `"brier"`
#'   binary data. `"energy"` and `"variogram"` score several series
#'   jointly. Lower scores are better.
#' @param interval_width Width of the central interval that sets the
#'   per-series `in_interval` indicator and the `"sis"` penalty. The
#'   indicator is computed for `"crps"`, `"drps"`, `"sis"` and the joint
#'   scores. The other scores report it as `NA`.
#' @param quantile_level Level of the `"qs"` score. Defaults to `0.5`,
#'   the median.
#' @param lower,upper Thresholds of `"twcrps"` and `"twenergy"`. The
#'   defaults `-Inf` and `Inf` give the unweighted scores.
#' @param log Logical. `TRUE` applies `log(x + 0.001)` to observations
#'   and draws before scoring. Taken by `"crps"`, `"sis"`, `"energy"`
#'   and `"variogram"`. Passing `TRUE` with any other score is an error.
#' @param weights Optional numeric vector of non-negative weights, one
#'   per series, for `"variogram"`.
#' @param ... Unused. Anything passed here is refused.
#'
#' @return An object of class `mvgam_score`: a named list with one data
#'   frame per series plus a trailing `all_series` summary. Its `print()`
#'   method shows the mean score and interval coverage of each series.
#'   A multivariate model gives one such object per response. Each series' data frame has columns `score`,
#'   `in_interval`, `interval_width`, `eval_horizon` and `score_type`,
#'   the requested `score`. A joint score leaves the per-series `score`
#'   column `NA`.
#'
#'   For a univariate score, `all_series` sums the scores over the
#'   series at each forecast time, with `score_type` prefixed `"sum_"`.
#'   Series forecast at their own times, as `CAR()` allows, are summed at
#'   the times they share. Its `eval_horizon` then counts the times any
#'   series is forecast at. For a joint score, `all_series` holds the
#'   score at each horizon. A joint score needs every series at the same
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
#'   102(477), 359-378. \doi{10.1198/016214506000001437}
#'
#'   Scheuerer, M. and Hamill, T. M. (2015). Variogram-based proper
#'   scoring rules for probabilistic forecasts of multivariate
#'   quantities. *Monthly Weather Review*, 143(4), 1321-1334.
#'   \doi{10.1175/MWR-D-14-00269.1}
#'
#' @examples
#' \dontrun{
#' set.seed(11)
#' simdat <- sim_mvgam(family = poisson(), n_series = 2L,
#'                     n_timepoints = 120L, trend_model = AR())
#' mod <- mvgam(y ~ s(x, k = 5),
#'              trend_formula = ~ AR(p = 1),
#'              data = simdat$data_train,
#'              family = poisson(),
#'              chains = 2, silent = 2)
#'
#' # Forecast the held-out times, then score each series
#' fc <- forecast(mod, newdata = simdat$data_test)
#' sc <- score(fc, score = "drps")
#' head(sc$series_1)
#'
#' # The DRPS summed over series at each forecast horizon
#' library(ggplot2)
#' ggplot(sc$all_series, aes(eval_horizon, score)) +
#'   geom_line(colour = "#8F2727") +
#'   geom_point(colour = "#8F2727") +
#'   labs(x = "Horizon", y = "DRPS summed over series") +
#'   mvgam_theme()
#'
#' # A joint score of both series at each horizon
#' head(score(fc, score = "energy")$all_series)
#'
#' # Next: compare_scores() ranks several models by one rule
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
      log = log, weights = weights
    ))
  }
  score <- match.arg(score,
                     c(univariate_forecast_scores, joint_forecast_scores))
  checkmate::assert_number(interval_width,
                             lower = 0.05, upper = 0.95)
  checkmate::assert_number(quantile_level, lower = 0, upper = 1)
  checkmate::assert_number(lower)
  checkmate::assert_number(upper)
  checkmate::assert_flag(log)
  checkmate::assert_numeric(weights, lower = 0, any.missing = FALSE,
                            len = length(object$series_names),
                            null.ok = TRUE)
  if (!is.null(weights) && !identical(score, "variogram")) {
    stop(insight::format_error(
      "'weights' applies to the 'variogram' score only."
    ), call. = FALSE)
  }

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

  out <- if (score %in% univariate_forecast_scores) {
    score_univariate(
      object, series_names, score,
      interval_width = interval_width,
      quantile_level = quantile_level, lower = lower,
      upper = upper, log = log
    )
  } else {
    score_multivariate(
      object, series_names, score,
      interval_width = interval_width,
      lower = lower, upper = upper, log = log, weights = weights
    )
  }
  structure(out, class = "mvgam_score", score = score,
            interval_width = interval_width)
}


#' @rdname score.mvgam_forecast
#' @param x An `mvgam_score` object returned by `score()`.
#' @param digits Integer number of significant digits. Defaults to 2.
#' @method print mvgam_score
#' @export
print.mvgam_score <- function(x, digits = 2L, ...) {
  checkmate::assert_int(digits, lower = 1L)
  score <- attr(x, "score")
  tab <- data.frame(
    series = names(x),
    mean_score = vapply(x, function(d) mean_measured(d$score),
                        numeric(1L)),
    # Coverage is missing on the total row and under a joint score
    coverage = vapply(x, function(d) {
      mean_measured(d$in_interval %||% NA_real_)
    }, numeric(1L)),
    times = vapply(x, nrow, integer(1L))
  )
  notes <- if (score %in% joint_forecast_scores) {
    "'all_series' scores the series jointly at each time."
  } else {
    "'all_series' sums the series at each time."
  }
  if (any(!is.na(tab$coverage))) {
    notes <- c(notes, paste0("'coverage' is the share inside the ",
                             round(100 * attr(x, "interval_width")),
                             "% interval."))
  }
  cat_score_table(
    paste(score_label(score), "of", nrow(tab) - 1L,
          "forecast series, lower is better"),
    tab, digits, notes
  )
  invisible(x)
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
    kernel_out <- univariate_score(
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
  joint <- multivariate_score(
    score, truths, fcs[ok],
    lower = lower, upper = upper, log = log, weights = weights[ok]
  )
  series_score$all_series <- data.frame(
    score = joint,
    eval_horizon = seq_along(joint),
    score_type = score,
    stringsAsFactors = FALSE
  )
  series_score
}


#' Print a table of scores under a title
#'
#' Shared by the print methods of `score()` and `compare_scores()`.
#' Numeric columns print to `digits` significant digits. A missing
#' cell prints blank.
#'
#' @param title Character title line
#' @param tab Data frame to print
#' @param digits Integer number of significant digits
#' @param notes Character vector of lines printed after the table
#' @return `NULL`, invisibly
#' @noRd
cat_score_table <- function(title, tab, digits, notes) {
  checkmate::assert_string(title)
  checkmate::assert_data_frame(tab)
  checkmate::assert_int(digits, lower = 1L)
  checkmate::assert_character(notes)
  num <- vapply(tab, is.numeric, logical(1L))
  tab[num] <- lapply(tab[num], function(v) {
    ifelse(is.na(v), "", format(signif(v, digits)))
  })
  cat(title, "\n\n", sep = "")
  print(tab, row.names = FALSE, right = FALSE)
  cat("\n", paste0(notes, "\n"), sep = "")
  invisible(NULL)
}
