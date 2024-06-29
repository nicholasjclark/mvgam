#' Posterior draws of `mvgam` residuals
#'
#' This method extracts posterior draws of Dunn-Smyth (randomized quantile)
#' residuals in the order in which the data were supplied to the model. It included
#' additional arguments for obtaining summaries of the computed residuals
#'
#' @inheritParams brms::residuals.brmsfit
#' @param object An object of class `mvgam`
#' @details This method gives residuals as Dunn-Smyth (randomized quantile) residuals. Any
#' observations that were missing (i.e. `NA`) in the original data will have missing values
#' in the residuals
#' @return An \code{array} of randomized quantile residual values.
#'   If \code{summary = FALSE} the output resembles those of
#'   \code{\link{posterior_epred.mvgam}} and \code{\link{predict.mvgam}}.
#'
#'   If \code{summary = TRUE} the output is an \code{n_observations} x \code{E}
#'   matrix. The number of summary statistics \code{E} is equal to \code{2 +
#'   length(probs)}: The \code{Estimate} column contains point estimates (either
#'   mean or median depending on argument \code{robust}), while the
#'   \code{Est.Error} column contains uncertainty estimates (either standard
#'   deviation or median absolute deviation depending on argument
#'   \code{robust}). The remaining columns starting with \code{Q} contain
#'   quantile estimates as specified via argument \code{probs}.
#' @examples
#' \donttest{
#' # Simulate some data and fit a model
#' simdat <- sim_mvgam(n_series = 1, trend_model = 'AR1')
#' mod <- mvgam(y ~ s(season, bs = 'cc'),
#'              trend_model = AR(),
#'              noncentred = TRUE,
#'              data = simdat$data_train,
#'              chains = 2)
#'
#'# Extract posterior residuals
#'resids <- residuals(mod)
#'str(resids)
#'}
#' @export
residuals.mvgam <- function(object,
                            summary = TRUE,
                            robust = FALSE,
                            probs = c(0.025, 0.975),
                            ...) {

  # What was the original time / series order?
  orig_order <- data.frame(series = object$obs_data$series,
                           time = object$obs_data$index..time..index)

  series_numeric <- as.numeric(orig_order$series)
  time_numeric <- match(orig_order$time, unique(orig_order$time))

  # Build a matrix to return residuals in this order
  resid_matrix <- matrix(NA, nrow = NROW(orig_order),
                         ncol = NROW(object$resids[[1]]))
  for(i in 1:NROW(resid_matrix)){
    resid_matrix[i,] <- object$resids[[series_numeric[i]]][,time_numeric[i]]
  }

  if(summary){
    Qupper <- apply(resid_matrix, 1, quantile, probs = max(probs), na.rm = TRUE)
    Qlower <- apply(resid_matrix, 1, quantile, probs = min(probs), na.rm = TRUE)

    if(robust){
      estimates <- apply(resid_matrix, 1, median, na.rm = TRUE)
      errors <- apply(abs(resid_matrix - estimates), 1, median, na.rm = TRUE)
    } else {
      estimates <- apply(resid_matrix, 1, mean, na.rm = TRUE)
      errors <- apply(resid_matrix, 1, sd, na.rm = TRUE)
    }

    out <- cbind(estimates, errors, Qlower, Qupper)
    colnames(out) <- c('Estimate', 'Est.Error', paste0('Q', 100*min(probs)),
                       paste0('Q', 100*max(probs)))
  } else {
    out <- resid_matrix
  }

  return(out)
}
