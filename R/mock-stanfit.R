#' Create Mock stanfit for Parameter Subsetting
#'
#' Creates a lightweight S3 object that mimics a stanfit for the purpose of
#' providing parameter subsets to brms functions during prediction. This mock
#' only supports draws extraction via as_draws_matrix().
#'
#' @param draws_matrix A draws_matrix object containing a subset of parameters
#'   from the original stanfit. Must inherit from "draws_matrix" and "draws"
#'   classes and be a matrix object.
#'
#' @return S3 object of class c("mock_stanfit", "stanfit") containing the
#'   cached draws.
#'
#' @details
#' This function enables parameter subsetting for brms prediction functions
#' without creating a full stanfit object. The mock stanfit only implements
#' the as_draws_matrix() method, which is sufficient for brms prediction
#' workflows.
#'
#' The mock inherits from "stanfit" to satisfy brms type checks while
#' providing custom extraction behavior through the S3 method dispatch system.
#'
#' Usage pattern:
#' \code{
#' # Extract observation parameters from combined fit
#' full_draws <- posterior::as_draws_matrix(mvgam_fit$fit)
#' obs_params <- c("b_Intercept", "b_temperature", "sigma")
#' obs_draws <- full_draws[, obs_params]
#'
#' # Create mock stanfit
#' mock_fit <- create_mock_stanfit(obs_draws)
#'
#' # Use in brmsfit object for predictions
#' obs_brmsfit$fit <- mock_fit
#' prep <- brms:::prepare_predictions(obs_brmsfit, newdata)
#' }
#'
#' @noRd
create_mock_stanfit <- function(draws_matrix) {
  # Validate draws_matrix has required posterior package classes
  checkmate::assert_class(draws_matrix, "draws_matrix")

  # Verify required structure for brms compatibility
  if (!"draws" %in% class(draws_matrix)) {
    stop(insight::format_error(
      "{.field draws_matrix} must inherit from 'draws' class."
    ))
  }

  if (!is.matrix(draws_matrix)) {
    stop(insight::format_error(
      "{.field draws_matrix} must be a matrix."
    ))
  }

  # Construct lightweight mock with cached draws for brms compatibility
  structure(
    list(draws_cache = draws_matrix),
    class = c("mock_stanfit", "stanfit")
  )
}


#' Extract Draws from Mock stanfit
#'
#' S3 method for extracting draws from a mock stanfit object created by
#' create_mock_stanfit(). This method is called by brms prediction functions
#' to access parameter draws.
#'
#' @param x A mock_stanfit object created by create_mock_stanfit()
#' @param ... Additional arguments (ignored, for S3 compatibility)
#'
#' @return The cached draws_matrix from the mock stanfit
#'
#' @details
#' This method provides the core functionality of the mock stanfit pattern.
#' When brms calls as_draws_matrix() on our mock stanfit, it receives the
#' parameter subset we provided during creation.
#'
#' The method simply returns the cached draws without any transformation,
#' maintaining the original posterior samples and parameter structure.
#'
#' @method as_draws_matrix mock_stanfit
#' @export
as_draws_matrix.mock_stanfit <- function(x, ...) {
  x$draws_cache
}


#' Prepare Predictions for Mock stanfit
#'
#' S3 method for prepare_predictions that works with mock_stanfit objects
#' created by create_mock_stanfit(). This method generates design matrices
#' and prediction metadata using brmsfit formula and family information
#' without requiring a full Stan fit object.
#'
#' @param object A mock_stanfit object created by create_mock_stanfit()
#'   containing parameter draws subset
#' @param brmsfit A brmsfit object providing formula, family, and other
#'   metadata needed for design matrix construction
#' @param newdata Data frame with predictor values for predictions. If NULL,
#'   uses original fitting data from brmsfit
#' @param re_formula Formula for random effects. NULL includes all random
#'   effects, NA excludes all
#' @param allow_new_levels Logical indicating whether new factor levels in
#'   random effects are allowed
#' @param sample_new_levels Character specifying how to sample new levels:
#'   "uncertainty" or "gaussian"
#' @param ... Additional arguments passed to brms::make_standata()
#'
#' @return A brmsprep object (S3 list) containing design matrices, formula
#'   metadata, family information, and parameter draws needed for posterior
#'   predictions. Compatible with downstream mvgam prediction functions.
#'
#' @details
#' This method implements the prepare_predictions interface for mock_stanfit
#' objects, enabling mvgam to use brms prediction infrastructure with
#' parameter subsets extracted from combined Stan fits.
#'
#' The function leverages brms::make_standata() to generate design matrices
#' for fixed effects, random effects, smooths, GPs, and other special terms.
#' This ensures compatibility with all brms formula features without manual
#' reimplementation.
#'
#' The returned brmsprep object contains all metadata needed for computing
#' linear predictors, applying inverse link functions, and generating
#' posterior predictive samples.
#'
#' @method prepare_predictions mock_stanfit
#' @export
prepare_predictions.mock_stanfit <- function(object,
                                              brmsfit,
                                              newdata = NULL,
                                              re_formula = NULL,
                                              allow_new_levels = FALSE,
                                              sample_new_levels = "uncertainty",
                                              ...) {
  # Validate core object types
  checkmate::assert_class(object, "mock_stanfit")
  checkmate::assert_class(brmsfit, "brmsfit")

  # Validate newdata if provided
  if (!is.null(newdata)) {
    if (!is.data.frame(newdata)) {
      stop(insight::format_error(
        "{.field newdata} must be a data frame.",
        "Received object of class: {class(newdata)}"
      ))
    }
    if (nrow(newdata) < 1) {
      stop(insight::format_error(
        "{.field newdata} must contain at least one row.",
        "Received data frame with {nrow(newdata)} rows."
      ))
    }
  }

  # Validate re_formula parameter
  if (!is.null(re_formula) && !identical(re_formula, NA)) {
    if (!inherits(re_formula, "formula")) {
      stop(insight::format_error(
        "{.field re_formula} must be NULL, NA, or a valid formula object.",
        "Current value has class: {class(re_formula)}"
      ))
    }
  }

  # Validate prediction parameters
  checkmate::assert_logical(allow_new_levels, len = 1)
  checkmate::assert_choice(
    sample_new_levels,
    choices = c("uncertainty", "gaussian")
  )

  # Validate logical relationship between parameters
  if (sample_new_levels != "uncertainty" && !allow_new_levels) {
    stop(insight::format_error(
      "{.field sample_new_levels} can only be modified when ",
      "{.field allow_new_levels} is TRUE.",
      "Set {.field allow_new_levels = TRUE} or use default sampling."
    ))
  }

  # Use newdata if provided, otherwise use original data from brmsfit
  prediction_data <- newdata %||% brmsfit$data

  # Add dummy response variables for standata generation
  # Pattern follows brms internal validate_newdata() → add_dummy_responses()
  # Dummy values are required for formula evaluation and data structure
  # consistency, but are never used in prediction computations
  newdata_with_resp <- prediction_data

  # Extract response variable(s) - handle both univariate and multivariate
  if (brms::is.mvbrmsformula(brmsfit$formula)) {
    # Multivariate: get all response variables from formula components
    resp_vars <- names(brmsfit$formula$forms)
  } else {
    # Univariate: extract single response from formula LHS
    resp_vars <- as.character(brmsfit$formula$formula[[2]])
  }

  # Add dummy values for any missing response variables
  # Values based on family type (following brms pattern)
  for (rv in resp_vars) {
    if (!rv %in% names(newdata_with_resp)) {
      # Add dummy response with value 0 (sufficient for all families)
      # Actual values don't matter - never used in prediction computation
      newdata_with_resp[[rv]] <- rep(0, nrow(newdata_with_resp))
    }
  }

  # Generate Stan data structure using brmsfit object directly
  # This creates all design matrices via brms machinery
  # Use internal=TRUE for compatibility with brms prediction workflow
  sdata <- brms::standata(
    brmsfit,
    newdata = newdata_with_resp,
    re_formula = re_formula,
    allow_new_levels = allow_new_levels,
    internal = TRUE,
    ...
  )

  # Extract draws from mock stanfit using cached draws
  draws <- object$draws_cache

  # Build minimal brmsprep structure compatible with mvgam prediction workflow
  # Following brms prepare_predictions() return structure
  prep <- structure(
    list(
      sdata = sdata,
      draws = draws,
      formula = brmsfit$formula,
      family = brmsfit$family,
      newdata = prediction_data,
      nobs = nrow(prediction_data)
    ),
    class = c("brmsprep", "mvgam_prep")
  )

  return(prep)
}
