#' `mvgam_forecast` object description
#'
#' A \code{mvgam_forecast} object returned by function \code{\link{hindcast}}
#' or \code{\link{forecast}}. Run `methods(class = "mvgam_forecast")` to see
#' an overview of available methods.
#'
#' @details A `mvgam_forecast` object contains the following elements:
#'
#' \itemize{
#'   \item `call` the original observation model formula
#'
#'   \item `trend_call` If a `trend_formula was supplied`, the original trend
#'     model formula is returned. Otherwise `NULL`
#'
#'   \item `family` \code{character} description of the observation distribution
#'
#'   \item `family_pars` \code{list} containing draws of family-specific
#'     parameters (i.e. shape, scale or overdispersion parameters). Only
#'     returned if `type = link`. Otherwise `NULL`
#'
#'   \item `trend_model` \code{character} description of the latent trend model
#'
#'   \item `drift` Logical specifying whether a drift term was used in the
#'     trend model
#'
#'   \item `use_lv` Logical flag indicating whether latent dynamic factors were
#'     used in the model
#'
#'   \item `fit_engine` `Character` describing the fit engine, either as `stan`
#'     or `jags`
#'
#'   \item `type` The type of predictions included (either `link`, `response`
#'     or `trend`)
#'
#'   \item `series_names` Names of the time series, taken from
#'     `levels(data$series)` in the original model fit
#'
#'   \item `train_observations` A `list` of training observation vectors of
#'     length `n_series`
#'
#'   \item `train_times` A `list` of the unique training times of length
#'     `n_series`
#'
#'   \item `test_observations` If the \code{\link{forecast}} function was used,
#'     a `list` of test observation vectors of length `n_series`. Otherwise
#'     `NULL`
#'
#'   \item `test_times` If the \code{\link{forecast}} function was used, a
#'     `list` of the unique testing (validation) times of length `n_series`.
#'     Otherwise `NULL`
#'
#'   \item `hindcasts` A `list` of posterior hindcast distributions of length
#'     `n_series`.
#'
#'   \item `forecasts` If the \code{\link{forecast}} function was used, a
#'     `list` of posterior forecast distributions of length `n_series`.
#'     Otherwise `NULL`
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
