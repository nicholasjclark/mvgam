#' `mvgam_forecast` object description
#'
#' A \code{mvgam_forecast} object returned by function \code{\link{hindcast}}
#' or \code{\link[generics]{forecast}}. Run `methods(class = "mvgam_forecast")` to see
#' an overview of available methods.
#'
#' @details A `mvgam_forecast` object contains the following elements:
#'
#' \itemize{
#'   \item `family` \code{character} description of the observation
#'     distribution.
#'
#'   \item `family_pars` \code{list} containing draws of family-specific
#'     parameters (e.g. shape, scale, overdispersion). Only returned when
#'     `type = "link"`; otherwise `NULL`.
#'
#'   \item `type` The type of predictions included
#'     (`"link"`, `"response"`, `"expected"`, or `"trend"`).
#'
#'   \item `series_names` Factor of series names, taken from
#'     `levels(data$series)` in the original model fit.
#'
#'   \item `train_observations`, `train_times` Lists of length `n_series`
#'     holding the training responses and the time of each hindcast
#'     column. A closure-unit family's `"expected"` and `"response"`
#'     hindcasts have one column per visit. Their times repeat.
#'
#'   \item `test_observations`, `test_times` If `forecast()` produced the
#'     object, the corresponding test response vectors and time grids.
#'     `NULL` otherwise (e.g. from `hindcast()`).
#'
#'   \item `hindcasts` Named list of posterior hindcast draw matrices, one
#'     `(ndraws x n_train)` matrix per series.
#'
#'   \item `forecasts` Named list of posterior forecast draw matrices when
#'     produced by `forecast()`, otherwise `NULL`.
#'
#'   \item `site_units` `TRUE` when the fit came from [jsdgam()]. Its
#'     units are sites, which `plot()` refuses to draw along a time
#'     axis.
#' }
#'
#' @seealso [mvgam], [hindcast.mvgam], [forecast.mvgam]
#'
#' @author Nicholas J Clark
#'
#' @name mvgam_forecast-class
NULL

#' @title Posterior summary of hindcast and forecast objects
#'
#' @description Posterior medians and quantiles of the hindcast and
#'   forecast distributions of each series, with the observed values
#'   in `data` and `newdata` when the object holds `type = 'response'`
#'   predictions.
#'
#' @param object An `mvgam_forecast` object from [hindcast()] or
#'   [forecast()].
#'
#' @param probs The lower and upper quantiles to report in addition to
#'   the median.
#'
#' @param ... Unused. Anything passed here is refused.
#'
#' @return A long-format `tibble` with one row per series and time,
#'   holding the posterior median and the two quantiles of the
#'   predictions, the observed value for `type = 'response'` and the
#'   prediction type. A multivariate fit on a long frame adds a leading
#'   `resp` column naming the response.
#'
#' @method summary mvgam_forecast
#'
#' @seealso \code{\link{forecast.mvgam}}, \code{\link{plot.mvgam_forecast}}
#'
#' @author Nicholas J Clark
#'
#' @export
summary.mvgam_forecast <- function(object, probs = c(0.025, 0.975), ...) {
  rlang::check_dots_empty()
  parts <- forecast_parts(object)
  if (is_forecast_list(parts)) {
    out <- do.call(rbind, lapply(names(parts), function(r) {
      cbind(resp = r, summary(parts[[r]], probs = probs))
    }))
    class(out) <- c("tbl_df", "tbl", "data.frame")
    return(out)
  }
  object <- parts
  fc_preds <- do.call(rbind, lapply(object$series_names, function(s_name) {
    # The time vectors are listed in the order of the hindcasts.
    pos <- match(s_name, names(object$hindcasts))
    preds <- cbind(object$hindcasts[[pos]], object$forecasts[[s_name]])
    df <- data.frame(
      series = s_name,
      time = c(object$train_times[[pos]], object$test_times[[pos]]),
      median_interval_cols(preds, probs, "pred")
    )
    if (identical(object$type, "response")) {
      df$truth <- c(object$train_observations[[s_name]],
                    object$test_observations[[s_name]])
    }
    df$type <- object$type
    df
  }))
  fc_preds$series <- factor(fc_preds$series, levels = object$series_names)
  rownames(fc_preds) <- NULL
  class(fc_preds) <- c("tbl_df", "tbl", "data.frame")
  fc_preds
}


#' The forecasts a method acts on
#'
#' A multivariate fit's forecast is a wrapper holding one forecast per
#' response. Its own slots are empty. On a wide frame the responses are
#' the series. The wrapper then becomes one forecast over them. On a
#' long frame every response shares its series' latent state. A
#' trend-scale wrapper repeats that state once per response and becomes
#' its first element. Other scales differ by response. Each element
#' remains its response's forecast.
#'
#' @param x An `mvgam_forecast` object
#' @return An `mvgam_forecast` object, or the long wrapper as a named
#'   list of `mvgam_forecast` objects
#' @noRd
forecast_parts <- function(x) {
  checkmate::assert_class(x, "mvgam_forecast")
  if (isTRUE(attr(x, "response_keyed"))) {
    return(response_axis_forecast(x))
  }
  if (!isTRUE(attr(x, "mv_wrapper"))) {
    return(x)
  }
  if (identical(x[[1L]]$type, "trend")) {
    return(x[[1L]])
  }
  lapply(unclass(x), identity)
}


#' Whether `forecast_parts()` returned one forecast per response
#'
#' @param parts The value of `forecast_parts()`
#' @return A single logical
#' @noRd
is_forecast_list <- function(parts) !inherits(parts, "mvgam_forecast")


#' Compact summary of an `mvgam_forecast` object
#'
#' Without a dedicated method, `print()` falls back to the default
#' list printer and dumps every posterior matrix in the object,
#' which is dozens of pages of numbers for any non-trivial fit.
#' This method prints a single-screen header with the family, the
#' number of series, the size of the training and forecast windows,
#' and the number of posterior draws. Use [summary.mvgam_forecast()]
#' for the per-timepoint posterior summary tibble or
#' [plot.mvgam_forecast()] for the visual.
#'
#' @param x An `mvgam_forecast` object from [forecast.mvgam()],
#'   [hindcast.mvgam()] or [ensemble.mvgam_forecast()].
#' @param ... Ignored.
#'
#' @seealso [summary.mvgam_forecast()], [plot.mvgam_forecast()],
#'   [forecast.mvgam()], [hindcast.mvgam()],
#'   [ensemble.mvgam_forecast()], [score.mvgam_forecast()],
#'   [compare_scores()]
#'
#' @return The `mvgam_forecast` object `x`, returned invisibly.
#'
#' @method print mvgam_forecast
#' @export
print.mvgam_forecast <- function(x, ...) {
  parts <- forecast_parts(x)
  if (is_forecast_list(parts)) {
    for (r in names(parts)) {
      cat("Response '", r, "'\n", sep = "")
      print(parts[[r]])
    }
    return(invisible(x))
  }
  x_in <- x
  x <- parts
  n_series <- length(x$series_names)
  # The count is of occasions, one count per series. A closure-unit
  # hindcast has one column per visit and several visits on each
  # occasion. Series observed or forecast at their own times hold
  # different numbers. The header then prints their range.
  n_occasions <- function(draws, times) {
    if (length(draws) == 0L) return(0L)
    counts <- if (is.null(times)) {
      vapply(draws, ncol, integer(1L))
    } else {
      vapply(times, function(t) length(unique(t)), integer(1L))
    }
    range(counts)
  }
  count_label <- function(n) {
    if (length(unique(n)) == 1L) {
      return(paste0(n[1L], " timepoints"))
    }
    paste0(n[1L], " to ", n[2L], " timepoints per series")
  }
  n_train <- n_occasions(x$hindcasts, x$train_times)
  n_test <- n_occasions(x$forecasts, x$test_times)
  n_draws <- if (!is.null(x$hindcasts) && length(x$hindcasts) > 0L) {
    nrow(x$hindcasts[[1L]])
  } else if (!is.null(x$forecasts) && length(x$forecasts) > 0L) {
    nrow(x$forecasts[[1L]])
  } else {
    0L
  }
  ensemble_w <- attr(x, "weights", exact = TRUE)
  lines <- c(
    paste0("mvgam_forecast (type '", x$type %||% "response", "')"),
    paste0("  family:    ",
           paste(unique(x$family %||% "unknown"), collapse = ", ")),
    paste0("  series:    ", n_series),
    paste0("  hindcast:  ", count_label(n_train)),
    paste0("  forecast:  ",
           if (all(n_test == 0L)) "0 (hindcast only)" else
             count_label(n_test)),
    paste0("  draws:     ", n_draws)
  )
  if (!is.null(ensemble_w)) {
    lines <- c(
      lines,
      paste0("  ensemble:  ", length(ensemble_w),
             " components (weights ",
             paste(sprintf("%.2f", as.numeric(ensemble_w)),
                   collapse = ", "), ")")
    )
  }
  cat(lines, sep = "\n")
  invisible(x_in)
}
