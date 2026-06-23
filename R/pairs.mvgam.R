#' Create a matrix of output plots from a \code{mvgam} object
#'
#' A \code{\link[graphics:pairs]{pairs}}
#' method that is customized for MCMC output.
#'
#' @param x An object of class \code{mvgam} or \code{jsdgam}
#' @inheritParams mcmc_plot.mvgam
#' @param ... Further arguments to be passed to
#'   \code{\link[bayesplot:MCMC-scatterplots]{mcmc_pairs}}.
#'
#' @return Plottable objects whose classes depend on the arguments supplied.
#' See \code{\link[bayesplot:MCMC-scatterplots]{mcmc_pairs}} for details.
#' @details For a detailed description see
#'   \code{\link[bayesplot:MCMC-scatterplots]{mcmc_pairs}}.
#'
#' @examples
#' \donttest{
#' set.seed(13)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                      n_timepoints = 60L, trend_model = AR())
#' mod <- mvgam(y ~ s(x), trend_formula = ~ AR(p = 1),
#'               data    = simdat$data_train,
#'               family  = poisson(),
#'               chains  = 2, silent = 2)
#'
#' # Pairs plot over the trend-dynamics parameters.
#' pairs(mod, variable = "trend_params", regex = TRUE)
#' }
#'
#' @export
pairs.mvgam <- function(
  x,
  variable = NULL,
  regex = FALSE,
  use_alias = TRUE,
  ...
) {
  # Default params to plot. By default, don't plot the betas
  # since spline models can have hundreds.
  if (is.null(variable)) {
    all_pars <- variables(x)
    variable <- c(
      all_pars$observation_pars[, 1],
      all_pars$observation_smoothpars[, 1],
      all_pars$observation_re_params[, 1],
      all_pars$trend_pars[, 1],
      all_pars$trend_smoothpars[, 1],
      all_pars$trend_re_params[, 1]
    )
    regex <- FALSE
  }
  draws <- as.array(
    x,
    variable = variable,
    regex = regex,
    use_alias = use_alias
  )
  with_color_scheme("red", bayesplot::mcmc_pairs(draws, ...))
}
