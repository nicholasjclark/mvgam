#' `mvgam_forecast` object description
#'
#' A \code{mvgam_forecast} object returned by function \code{\link{hindcast}} or \code{\link{forecast}}.
#' Run `methods(class = "mvgam_forecast")` to see an overview of available methods.
#' @details A `mvgam_forecast` object contains the following elements:
#'\itemize{
#'   \item `call` the original observation model formula
#'   \item `trend_call` If a `trend_formula was supplied`, the original trend model formula is
#'   returned. Otherwise `NULL`
#'   \item `family` \code{character} description of the observation distribution
#'   \item `family_pars` \code{list} containing draws of family-specific parameters (i.e.
#'   shape, scale or overdispersion parameters). Only returned if `type = link`. Otherwise `NULL`
#'   \item `trend_model` \code{character} description of the latent trend model
#'   \item `drift` Logical specifying whether a drift term was used in the trend model
#'   \item `use_lv` Logical flag indicating whether latent dynamic factors were used in the model
#'   \item `fit_engine` `Character` describing the fit engine, either as `stan` or `jags`
#'   \item `type` The type of predictions included (either `link`, `response` or `trend`)
#'   \item `series_names` Names of the time series, taken from `levels(data$series)` in the original
#'   model fit
#'   \item `train_observations` A `list` of training observation vectors of length `n_series`
#'   \item `train_times` A `list` of the unique training times of length `n_series`
#'   \item `test_observations` If the \code{\link{forecast}} function was used,
#'   a `list` of test observation vectors of length `n_series`. Otherwise `NULL`
#'   \item `test_times` If the \code{\link{forecast}} function was used, a
#'   `list` of the unique testing (validation) times of length `n_series`. Otherwise `NULL`
#'   \item `hindcasts` A `list` of posterior hindcast distributions of length `n_series`.
#'   \item `forecasts` If the \code{\link{forecast}} function was used,
#'   a `list` of posterior forecast distributions of length `n_series`. Otherwise `NULL`
#'   }
#' @seealso [mvgam], [hindcast.mvgam], [forecast.mvgam]
#' @author Nicholas J Clark
#' @name mvgam_forecast-class
NULL
