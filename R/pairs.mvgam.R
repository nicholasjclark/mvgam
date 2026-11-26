#' Pairs plot of mvgam parameters
#'
#' Scatters the posterior draws of each pair of parameters, with each
#' parameter's marginal posterior on the diagonal, as
#' [bayesplot::mcmc_pairs()] draws them. Divergent transitions are
#' marked. A ridge or a funnel between two parameters shows that the
#' data separate them poorly.
#'
#' Without `variable`, the plot shows the parameters
#' [mcmc_plot.mvgam()] shows: the population coefficients, the family
#' parameters, the variance components and the trend's dynamics and
#' loadings. The per-basis smooth coefficients (`s_*`) and the
#' group-level deviations (`r_*`) run to hundreds on a spline or
#' hierarchical fit. They are drawn when `variable` names them.
#'
#' @param x A fitted `mvgam` or `jsdgam` object
#' @inheritParams mcmc_plot.mvgam
#' @param ... Further arguments to [bayesplot::mcmc_pairs()], such as
#'   `off_diag_args`.
#'
#' @return A `bayesplot_grid` object
#'
#' @seealso [mcmc_plot.mvgam()] draws the other bayesplot MCMC plots.
#'   [mvgam_draws] lists the shortcuts `variable` accepts.
#'
#' @examples
#' \dontrun{
#' set.seed(13)
#' simdat <- sim_mvgam(family = gaussian(), n_series = 1L,
#'                      n_timepoints = 120L, trend_model = AR(),
#'                      prop_trend = 0.5)
#' mod <- mvgam(y ~ s(x, k = 5), trend_formula = ~ AR(p = 1),
#'               data    = simdat$data_train,
#'               family  = gaussian(),
#'               chains  = 2, silent = 2)
#'
#' # A Gaussian response adds white noise to the latent AR(1) process.
#' # The data split the total variance between the observation noise
#' # `sigma` and the process noise `sigma_trend` through the
#' # autocorrelation alone. A larger `sigma` leaves less variance for
#' # `sigma_trend`. The pair then forms a negative ridge. Divergences
#' # gather where `sigma` nears zero.
#' pairs(mod, variable = c("obs_params", "trend_params"))
#'
#' # Next, mcmc_plot(mod, type = "rank_overlay") checks that the chains
#' # mix
#' }
#'
#' @family posterior summaries
#' @export
pairs.mvgam <- function(x, variable = NULL, regex = FALSE, ...) {
  require_fitted_model(x, "pairs")
  mcmc_plot.mvgam(x, type = "pairs", variable = variable, regex = regex,
                  ...)
}
