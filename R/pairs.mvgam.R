#' Create a matrix of output plots from a \code{mvgam} object
#'
#' A \code{\link[graphics:pairs]{pairs}} method for MCMC output.
#' With `variable` left at its default the panel holds the
#' parameters a reader interprets: the intercept and the parametric
#' coefficients, the family's parameters, the variance components,
#' the smoothing penalties, and the trend's dynamics and loadings.
#' The per-basis smooth coefficients (`s_*` / `zs_*`) and the
#' per-level group-level deviations (`r_*`) are left out, since a
#' spline or hierarchical fit carries hundreds of them; name them in
#' `variable` to draw them. \code{\link{mcmc_plot.mvgam}} takes the
#' same default.
#'
#' @param x An object of class \code{mvgam} or \code{jsdgam}.
#' @inheritParams mcmc_plot.mvgam
#' @param ... Further arguments passed to
#'   \code{\link[bayesplot:MCMC-scatterplots]{mcmc_pairs}}.
#'
#' @return A `bayesplot_grid` object; see
#'   \code{\link[bayesplot:MCMC-scatterplots]{mcmc_pairs}}.
#'
#' @examples
#' \dontrun{
#' set.seed(13)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                      n_timepoints = 120L, trend_model = AR())
#' mod <- mvgam(y ~ s(x), trend_formula = ~ AR(p = 1),
#'               data    = simdat$data_train,
#'               family  = poisson(),
#'               chains  = 2, silent = 2)
#'
#' # Default selection: intercept(s), smoothness penalties, trend
#' # dynamics, family extras.
#' pairs(mod)
#'
#' # Custom regex for a focused pairs panel.
#' pairs(mod, variable = "^(sigma|ar1)_trend", regex = TRUE)
#' }
#'
#' @export
pairs.mvgam <- function(
  x,
  variable = NULL,
  regex = FALSE,
  ...
) {
  if (is.null(variable)) {
    variable <- default_plot_variables(x)
    regex <- FALSE
  }
  draws <- as.array(x, variable = variable, regex = regex)
  with_color_scheme("red", bayesplot::mcmc_pairs(draws, ...))
}
