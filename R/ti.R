# This file contains functions dealing with the extended
# formula syntax to specify smooth terms via mgcv

#' Defining smooths in \pkg{mvgam} formulae
#'
#' Functions used in definition of smooth terms within model formulae.
#' The functions do not evaluate a (spline) smooth - they exist purely
#' to help set up mvgam models using spline based smooths.
#'
#' @param ... Arguments passed to \code{\link[mgcv:ti]{mgcv::ti}} or
#'  \code{\link[mgcv:ti]{mgcv::te}}
#'
#' @details The functions defined here are just simple wrappers of the respective
#'   functions of the \pkg{mgcv} package. When using them, please cite the
#'   appropriate references obtained via \code{citation("mgcv")}.
#'
#' @seealso \code{\link[mgcv:ti]{mgcv::ti}}, \code{\link[mgcv:ti]{mgcv::te}}
#'
#'
#' @examples
#' \donttest{
#' # Simulate some data
#' dat <- mgcv::gamSim(1, n = 200, scale = 2)
#'
#' # Fit univariate smooths for all predictors
#' fit1 <- mvgam(y ~ s(x0) + s(x1) + s(x2) + s(x3),
#'               data = dat, chains = 2, family = gaussian())
#' summary(fit1)
#' conditional_effects(fit1)
#'
#' # Fit a more complicated smooth model
#' fit2 <- mvgam(y ~ te(x0, x1) + s(x2, by = x3),
#'               data = dat, chains = 2, family = gaussian())
#' summary(fit2)
#' conditional_effects(fit2)
#' }
#'
#' @rdname ti
#' @export
ti <- function(...) {
  mgcv::ti(...)
}

#' @rdname ti
#' @export
te <- function(...) {
  mgcv::te(...)
}
