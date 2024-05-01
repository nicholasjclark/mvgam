#' Create a matrix of output plots from a \code{mvgam} object
#'
#' A \code{\link[graphics:pairs]{pairs}}
#' method that is customized for MCMC output.
#'
#' @param x An object of class \code{mvgam}
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
#' \dontrun{
#' simdat <- sim_mvgam(n_series = 1, trend_model = 'AR1')
#' mod <- mvgam(y ~ s(season, bs = 'cc'),
#'              trend_model = AR(),
#'              data = simdat$data_train,
#'              chains = 2)
#' pairs(mod)
#' pairs(mod, variable = c('ar1', 'sigma'), regex = TRUE)
#' }
#'
#' @export
pairs.mvgam <- function(x, variable = NULL, regex = FALSE,
                       use_alias = TRUE, ...) {

  # Set red colour scheme
  col_scheme <- attr(color_scheme_get(),
                     'scheme_name')
  color_scheme_set('red')

  # Set default params to plot
  # By default, don't plot the Betas as there can be hundreds
  # of them in spline models
  if(is.null(variable)) {
    all_pars <- variables(x)
    variable <- c(all_pars$observation_pars[,1],
                  all_pars$observation_smoothpars[,1],
                  all_pars$observation_re_params[,1],
                  all_pars$trend_pars[,1],
                  all_pars$trend_smoothpars[,1],
                  all_pars$trend_re_params[,1])
    regex <- FALSE
  }
  draws <- as.array(x, variable = variable,
                    regex = regex,
                    use_alias = use_alias)

  # Generate plot and reset colour scheme
  out_plot <- bayesplot::mcmc_pairs(draws, ...)
  color_scheme_set(col_scheme)

  # Return the plot
  return(out_plot)
}
