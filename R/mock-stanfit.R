#' Create Mock stanfit for Parameter Subsetting
#'
#' Creates a lightweight S3 object that mimics a stanfit for the purpose of
#' providing parameter subsets to brms functions during prediction. This mock
#' only supports draws extraction via as_draws_matrix().
#'
#' @param draws_matrix A draws_matrix object containing a subset of parameters
#'   from the original stanfit. Must have class c("draws_matrix", "draws",
#'   "matrix", "array").
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

  # Verify all required classes present
  if (!all(c("draws", "matrix", "array") %in% class(draws_matrix))) {
    stop(insight::format_error(
      "{.field draws_matrix} must have classes 'draws_matrix', 'draws', 'matrix', and 'array'."
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
