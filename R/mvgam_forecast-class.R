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
#'     holding the training response vectors and their unique time grids.
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
#' @description This function takes an \code{mvgam_forecast} object and
#'   calculates a posterior summary of the hindcast and forecast distributions
#'   of each series, along with any true values that were included in `data`
#'   and `newdata` if `type = 'response'` was used in the call to
#'   \code{hindcast()} or \code{function()}
#'
#' @param object an object of class `mvgam_forecast` obtained using either the
#'   \code{hindcast()} or \code{function()} function. This object will contain
#'   draws from the posterior distribution of hindcasts and forecasts.
#'
#' @param probs The upper and lower percentiles to be computed by the
#'   `quantile` function, in addition to the median
#'
#' @param ... ignored
#'
#' @return A long-format `tibble` / `data.frame` reporting the posterior median,
#'   upper and lower percentiles of the predictions for each series at each of
#'   the timepoints that were originally supplied in `data` and, optionally,
#'   in `newdata`.
#'
#' @method summary mvgam_forecast
#'
#' @seealso \code{\link{forecast.mvgam}}, \code{\link{plot.mvgam_forecast}}
#'
#' @author Nicholas J Clark
#'
#' @export
summary.mvgam_forecast = function(object, probs = c(0.025, 0.975), ...) {
  if (length(probs) != 2L) {
    stop("argument 'probs' must be a vector of length 2", call. = FALSE)
  }
  validate_proportional(min(probs))
  validate_proportional(max(probs))

  n_series <- length(object$series_names)
  type <- object$type

  # Extract predictions and truths (if type = 'response')
  fc_preds <- do.call(
    rbind,
    lapply(1:n_series, function(x) {
      s_name <- object$series_names[x]
      preds <- cbind(
        object$hindcasts[[which(names(object$hindcasts) == s_name)]],
        object$forecasts[[which(names(object$forecasts) == s_name)]]
      )

      # Calculate quantiles of the forecast distribution
      cred <- sapply(
        1:NCOL(preds),
        function(n) quantile(preds[, n], probs = probs, na.rm = TRUE)
      )
      meds <- apply(preds, 2, median)

      # Put into a long "tidy" dataframe
      if (type == 'response') {
        df <- data.frame(
          series = s_name,
          time = c(
            object$train_times[[which(names(object$hindcasts) == s_name)]],
            object$test_times[[which(names(object$hindcasts) == s_name)]]
          ),
          pred_median = meds,
          pred_Qlower = cred[1, ],
          pred_Qupper = cred[2, ],
          truth = c(
            object$train_observations[[s_name]],
            object$test_observations[[s_name]]
          ),
          type = 'response'
        )
        colnames(df) <- c(
          'series',
          'time',
          'predQ50',
          paste0('predQ', 100 * min(probs)),
          paste0('predQ', 100 * max(probs)),
          'truth',
          'type'
        )
        rownames(df) <- NULL
      } else {
        df <- data.frame(
          series = s_name,
          time = c(
            object$train_times[[which(names(object$hindcasts) == s_name)]],
            object$test_times[[which(names(object$hindcasts) == s_name)]]
          ),
          predQ50 = meds,
          predQlower = cred[1, ],
          predQupper = cred[2, ],
          type = type
        )
        colnames(df) <- c(
          'series',
          'time',
          'predQ50',
          paste0('predQ', 100 * min(probs)),
          paste0('predQ', 100 * max(probs)),
          'type'
        )
        rownames(df) <- NULL
      }
      df
    })
  ) %>%
    dplyr::mutate(
      series = factor(series, levels = object$series_names)
    )
  class(fc_preds) <- c("tbl_df", "tbl", "data.frame")

  return(fc_preds)
}


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
  n_series <- length(x$series_names)
  n_train <- if (!is.null(x$hindcasts) && length(x$hindcasts) > 0L) {
    ncol(x$hindcasts[[1L]])
  } else {
    0L
  }
  n_test <- if (!is.null(x$forecasts) && length(x$forecasts) > 0L) {
    ncol(x$forecasts[[1L]])
  } else {
    0L
  }
  n_draws <- if (!is.null(x$hindcasts) && length(x$hindcasts) > 0L) {
    nrow(x$hindcasts[[1L]])
  } else if (!is.null(x$forecasts) && length(x$forecasts) > 0L) {
    nrow(x$forecasts[[1L]])
  } else {
    0L
  }
  ensemble_w <- attr(x, "weights", exact = TRUE)
  bullets <- c(
    paste0("mvgam_forecast (type '", x$type %||% "response", "')"),
    "*" = paste0("family:    ", x$family %||% "unknown"),
    "*" = paste0("series:    ", n_series),
    "*" = paste0("hindcast:  ", n_train, " timepoints"),
    "*" = paste0("forecast:  ", n_test,
                 if (n_test == 0L) " (none; hindcast only)" else " timepoints"),
    "*" = paste0("draws:     ", n_draws)
  )
  if (!is.null(ensemble_w)) {
    bullets <- c(
      bullets,
      "*" = paste0("ensemble:  ", length(ensemble_w),
                   " components (weights ",
                   paste(sprintf("%.2f", as.numeric(ensemble_w)),
                         collapse = ", "), ")")
    )
  }
  cli::cli_inform(bullets)
  invisible(x)
}
