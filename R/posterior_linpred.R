#' Posterior Linear Predictor for mvgam Models
#'
#' @description
#' Extract linear predictor values from fitted mvgam models. Combines
#' observation and trend components on the link scale with proper
#' uncertainty propagation.
#'
#' @name posterior_linpred
NULL


#' Get Combined Linear Predictor
#'
#' Extracts and combines observation and trend linear predictors from an
#' mvgam model. Observation and trend effects are added on the link scale.
#'
#' @param mvgam_fit mvgam object from mvgam()
#' @param newdata data.frame with prediction covariates
#' @param process_error Logical; if TRUE, includes draw-by-draw uncertainty
#'   from trend parameters. If FALSE, uses posterior mean of trend component,
#'   which speeds computation but understates total uncertainty.
#' @param ndraws Integer number of posterior draws to use (NULL = all)
#' @param re_formula Formula for random effects (NULL = all, NA = none)
#' @param allow_new_levels Logical; allow new factor levels in random effects
#' @param sample_new_levels Character; method for sampling new levels
#' @param resp Character; response name for multivariate models (NULL = all)
#'
#' @return Matrix [ndraws x nobs] of combined linear predictor values on
#'   link scale. For multivariate models with resp = NULL, returns named
#'   list of matrices.
#'
#' @noRd
get_combined_linpred <- function(mvgam_fit, newdata,
                                 process_error = TRUE,
                                 ndraws = NULL,
                                 re_formula = NULL,
                                 allow_new_levels = FALSE,
                                 sample_new_levels = "uncertainty",
                                 resp = NULL) {
  # Check if trend model exists and has required structure
  has_trend <- !is.null(mvgam_fit$trend_model) &&
               !is.null(mvgam_fit$trend_model$formula)

  # Extract observation linear predictor (validation handled internally)
  obs_linpred <- extract_component_linpred(
    mvgam_fit = mvgam_fit,
    newdata = newdata,
    component = "obs",
    resp = resp,
    ndraws = ndraws,
    re_formula = re_formula,
    allow_new_levels = allow_new_levels,
    sample_new_levels = sample_new_levels
  )

  # If no trend model, return observation linpred only
  if (!has_trend) {
    return(obs_linpred)
  }

  # Extract trend linear predictor
  trend_linpred <- extract_component_linpred(
    mvgam_fit = mvgam_fit,
    newdata = newdata,
    component = "trend",
    resp = resp,
    ndraws = ndraws,
    re_formula = re_formula,
    allow_new_levels = allow_new_levels,
    sample_new_levels = sample_new_levels
  )

  # Detect structure: list indicates multivariate, matrix indicates univariate
  is_multivariate <- is.list(obs_linpred) && !is.matrix(obs_linpred)

  if (is_multivariate) {
    # Multivariate: combine each response separately
    combined <- lapply(names(obs_linpred), function(resp_name) {
      obs_mat <- obs_linpred[[resp_name]]

      # Trend may be shared (matrix) or response-specific (list)
      # Shared trend means same latent process affects all responses
      if (is.list(trend_linpred) && !is.matrix(trend_linpred)) {
        trend_mat <- trend_linpred[[resp_name]]
      } else {
        # Shared trend across responses
        checkmate::assert_matrix(trend_linpred)
        trend_mat <- trend_linpred
      }

      # Validate dimensions match before combination
      if (nrow(trend_mat) != nrow(obs_mat) ||
          ncol(trend_mat) != ncol(obs_mat)) {
        stop(insight::format_error(
          "Dimension mismatch for response {.val {resp_name}}: ",
          "obs_linpred is [{nrow(obs_mat)} x {ncol(obs_mat)}] but ",
          "trend_linpred is [{nrow(trend_mat)} x {ncol(trend_mat)}]."
        ))
      }

      # Apply process_error: FALSE fixes trend at posterior mean
      if (!process_error) {
        trend_mean <- colMeans(trend_mat)
        trend_mat <- matrix(
          trend_mean,
          nrow = nrow(obs_mat),
          ncol = ncol(obs_mat),
          byrow = TRUE
        )
      }

      # Combine additively on link scale
      obs_mat + trend_mat
    })
    names(combined) <- names(obs_linpred)
    return(combined)
  }

  # Univariate case: both components are matrices
  checkmate::assert_matrix(obs_linpred)
  checkmate::assert_matrix(trend_linpred)

  # Validate dimensions match
  if (nrow(trend_linpred) != nrow(obs_linpred) ||
      ncol(trend_linpred) != ncol(obs_linpred)) {
    stop(insight::format_error(
      "Dimension mismatch: obs_linpred is ",
      "[{nrow(obs_linpred)} x {ncol(obs_linpred)}] but ",
      "trend_linpred is [{nrow(trend_linpred)} x {ncol(trend_linpred)}]."
    ))
  }

  # Apply process_error: FALSE fixes trend at posterior mean
  if (!process_error) {
    trend_mean <- colMeans(trend_linpred)
    trend_linpred <- matrix(
      trend_mean,
      nrow = nrow(obs_linpred),
      ncol = ncol(obs_linpred),
      byrow = TRUE
    )
  }

  # Combine additively on link scale
  obs_linpred + trend_linpred
}


#' Extract Posterior Linear Predictor from mvgam Models
#'
#' @description
#' Extract linear predictor values (eta, on link scale) from fitted mvgam
#' models. Combines observation model effects with State-Space trend
#' components. For Poisson models this is log-scale; for binomial models
#' this is logit-scale.
#'
#' @param object A fitted mvgam object from [mvgam()].
#' @param newdata Optional data frame with covariates for prediction. If
#'   NULL, uses original training data stored in the model object.
#' @param process_error Logical; if TRUE (default), includes full
#'   draw-by-draw uncertainty from trend parameters. If FALSE, fixes
#'   trend at posterior mean for faster computation.
#' @param ndraws Positive integer specifying number of posterior draws to
#'   use. NULL (default) uses all available draws.
#' @param re_formula Formula for random effects. NULL (default) includes
#'   all random effects, NA excludes all random effects.
#' @param allow_new_levels Logical; if TRUE, allows new factor levels in
#'   random effects grouping variables. Default FALSE.
#' @param sample_new_levels Character specifying how to handle new levels.
#'   Either "uncertainty" (default) or "gaussian".
#' @param resp Character specifying which response variable for
#'   multivariate models. NULL (default) returns predictions for all
#'   responses.
#' @param ... Additional arguments passed to internal methods.
#'
#' @return Matrix with dimensions [ndraws x nobs] containing linear
#'   predictor values. Each row is one posterior draw, each column is
#'   one observation from newdata. Values are on link scale.
#'
#'   For multivariate models with resp = NULL, returns a named list of
#'   matrices (one per response variable).
#'
#' @details
#' The linear predictor combines:
#' \itemize{
#'   \item Observation model effects: fixed effects, random effects,
#'     smooth terms, GP terms from the observation formula
#'   \item Trend effects: State-Space trend contributions from the
#'     trend formula (if present)
#' }
#'
#' For models without a trend component (trend_formula = NULL), behavior
#' matches [brms::posterior_linpred()].
#'
#' The \code{process_error} argument controls uncertainty propagation:
#' \itemize{
#'   \item TRUE: Full posterior uncertainty from both observation and
#'     trend parameters (draw-by-draw variation)
#'   \item FALSE: Trend fixed at posterior mean; only observation
#'     uncertainty propagated (faster but understates total uncertainty)
#' }
#'
#' @seealso [brms::posterior_linpred()] for the brms generic,
#'   [posterior_epred.mvgam()] for expected values on response scale,
#'   [posterior_predict.mvgam()] for posterior predictive samples.
#'
#' @examples
#' \dontrun{
#' # Fit a State-Space model
#' fit <- mvgam(
#'   count ~ temperature + s(day),
#'   trend_formula = ~ AR(p = 1),
#'   data = my_data,
#'   family = poisson()
#' )
#'
#' # Extract linear predictor (link scale)
#' linpred <- posterior_linpred(fit)
#'
#' # With new data
#' linpred_new <- posterior_linpred(fit, newdata = new_data)
#'
#' # Reduce uncertainty by fixing trend at posterior mean
#' linpred_fast <- posterior_linpred(fit, process_error = FALSE)
#'
#' # Subset draws for speed
#' linpred_sub <- posterior_linpred(fit, ndraws = 100)
#' }
#'
#' @importFrom brms posterior_linpred
#' @method posterior_linpred mvgam
#' @export
posterior_linpred.mvgam <- function(object, newdata = NULL,
                                    process_error = TRUE,
                                    ndraws = NULL,
                                    re_formula = NULL,
                                    allow_new_levels = FALSE,
                                    sample_new_levels = "uncertainty",
                                    resp = NULL,
                                    ...) {
  # Validate mvgam-specific parameters only (other validation delegated)
  checkmate::assert_class(object, "mvgam")
  checkmate::assert_logical(process_error, len = 1)

  # Handle newdata = NULL (use training data)
  if (is.null(newdata)) {
    if (is.null(object$data)) {
      stop(insight::format_error(
        "No training data found in model object. ",
        "Please provide {.field newdata} explicitly."
      ))
    }
    newdata <- object$data
  }

  # Delegate to get_combined_linpred (all other validation handled there)
  get_combined_linpred(
    mvgam_fit = object,
    newdata = newdata,
    process_error = process_error,
    ndraws = ndraws,
    re_formula = re_formula,
    allow_new_levels = allow_new_levels,
    sample_new_levels = sample_new_levels,
    resp = resp
  )
}
