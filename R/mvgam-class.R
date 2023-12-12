#' Fitted `mvgam` object description
#'
#' A fitted \code{mvgam} object returned by function \code{\link{mvgam}}.
#' Run `methods(class = "mvgam")` to see an overview of available methods.
#' @details A `mvgam` object contains the following elements:
#'\itemize{
#'   \item `call` the original observation model formula
#'   \item `trend_call` If a `trend_formula was supplied`, the original trend model formula is
#'   returned. Otherwise `NULL`
#'   \item `family` \code{character} description of the observation distribution
#'   \item `trend_model` \code{character} description of the latent trend model
#'   \item `trend_map` \code{data.frame} describing the mapping of trend states to
#'   observations, if supplied in the original model. Otherwise `NULL`
#'   \item `drift` Logical specifying whether a drift term was used in the trend model
#'   \item `priors` If the model priors were updated from their defaults, the prior `dataframe`
#'   will be returned. Otherwise `NULL`
#'   \item `model_output` The `MCMC` object returned by the fitting engine. If the model was fitted
#' using `Stan`, this will be an object of class `stanfit` (see \code{\link[rstan]{stanfit-class}} for details).
#' If `JAGS` was used as the backend, this will be an object of class `runjags`
#' (see \code{\link[runjags]{runjags-class}} for details)
#'   \item `model_file` The `character` string model file used to describe the model in either
#' `Stan` or `JAGS` syntax
#'   \item `model_data` If `return_model_data` was set to `TRUE` when fitting the model, the `list` object
#' containing all data objects needed to condition the model is returned. Each item in the `list` is described
#' in detail at the top of the `model_file`. Otherwise `NULL`
#'   \item `inits` If `return_model_data` was set to `TRUE` when fitting the model, the initial value
#' functions used to initialise the MCMC chains will be returned. Otherwise `NULL`
#'   \item `monitor_pars` The parameters
#' that were monitored during MCMC sampling are returned as a `character vector`
#'   \item `sp_names` A `character vector` specifying the names for each smoothing parameter
#'   \item `mgcv_model` An object of class `gam` containing the `mgcv` version of the observation model.
#' This object is used for generating the linear predictor matrix when making predictions for new data. The
#' coefficients in this model object will contain the posterior median coefficients from the GAM linear predictor,
#' but these are only used if generating plots of smooth functions that `mvgam` currently cannot handle
#' (such as plots for three-dimensional smooths). This model therefore should not be used for inference.
#' See \code{\link[mgcv]{gamObject}} for details
#'   \item `trend_mgcv_model` If a `trend_formula was supplied`, an object of class `gam` containing
#'   the `mgcv` version of the trend model. Otherwise `NULL`
#'   \item `ytimes` The `matrix` object used in model fitting for indexing which series and timepoints
#'   were observed in each row of the supplied data. Used internally by some downstream plotting
#'   and prediction functions
#'   \item `resids` A named `list` object containing posterior draws of Dunn-Smyth
#'   randomized quantile residuals
#'   \item `use_lv` Logical flag indicating whether latent dynamic factors were used in the model
#'   \item `n_lv` If `use_lv == TRUE`, the number of latent dynamic factors used in the model
#'   \item `upper_bounds` If bounds were supplied in the original model fit, they will be returned.
#'   Otherwise `NULL`
#'   \item `obs_data` The original data object (either a `list` or `dataframe`) supplied in model
#'   fitting.
#'   \item `test_data` If test data were supplied (as argument `newdata` in the original model), it
#'   will be returned. Othwerise `NULL`
#'   \item `fit_engine` `Character` describing the fit engine, either as `stan` or `jags`
#'   \item `backend` `Character` describing the backend used for modelling, either as `rstan`, `cmdstanr` or `rjags`
#'   \item `algorithm` `Character` describing the algorithm used for finding the posterior,
#'   either as `sampling`, `laplace`, `pathfinder`, `meanfield` or `fullrank`
#'   \item `max_treedepth` If the model was fitted using `Stan`, the value supplied for the maximum
#'   treedepth tuning parameter is returned (see \code{\link[rstan]{stan}} for details).
#'   Otherwise `NULL`
#'   \item `adapt_delta` If the model was fitted using `Stan`, the value supplied for the adapt_delta
#'    tuning parameter is returned (see \code{\link[rstan]{stan}} for details).
#'   Otherwise `NULL`
#'   }
#' @seealso [mvgam]
#' @author Nicholas J Clark
#' @name mvgam-class
NULL
