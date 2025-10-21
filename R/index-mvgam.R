#' Index \code{mvgam} objects
#'
#' @aliases variables
#'
#' Index variables and their `mgcv` coefficient names
#'
#' @param x A \code{mvgam} object or another \R object for which
#' the methods are defined.
#'
#' @param ... Arguments passed to individual methods (if applicable).
#'
#' @name index-mvgam
NULL

#' @rdname index-mvgam
#'
#' @importFrom posterior variables
#'
#' @param x \code{list} object returned from \code{mvgam}. See [mvgam()]
#'
#' @method variables mvgam
#'
#' @return a character vector of parameter names
#'
#' @author Nicholas J Clark
#'
#' @examples
#' \donttest{
#' # Simulate data and fit a model
#' simdat <- sim_mvgam(
#'   n_series = 1,
#'   trend_model = AR()
#' )
#'
#' mod <- mvgam(
#'   y ~ s(season, bs = 'cc', k = 6),
#'   trend_model = AR(),
#'   data = simdat$data_train,
#'   chains = 2,
#'   silent = 2
#' )
#'
#' # Extract model variables
#' variables(mod)
#' }
#'
#' @export
#' @export variables
variables.mvgam <- function(x, ...) {
  # Validate input
  checkmate::assert_class(x, "mvgam")

  # Extract parameter names via draws object for backend compatibility
  # Posterior package handles both rstan and cmdstanr stanfit objects
  all_vars <- variables(posterior::as_draws(x$fit), ...)

  # Apply parameter exclusions
  if (!is.null(x$exclude) && length(x$exclude) > 0) {
    all_vars <- setdiff(all_vars, x$exclude)
  }

  all_vars
}
