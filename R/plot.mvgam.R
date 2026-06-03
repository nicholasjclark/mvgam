#' Default plot method for `mvgam` objects (placeholder)
#'
#' Scaffold for the eventual `plot.mvgam` S3 method. The full
#' implementation is pending the plot-overhaul batch that will
#' ship native ggplot helpers for latent trends and forecasts,
#' alongside the canonical wrappers around `pp_check.mvgam()`,
#' `conditional_effects.mvgam()`, and `mcmc_plot.mvgam()`.
#'
#' Until that lands, use the brms-style alternatives directly:
#'
#' \itemize{
#'   \item Posterior predictive checks:
#'     \code{pp_check(object, type = "dens_overlay")}.
#'   \item Marginal smooths / predictor effects:
#'     \code{conditional_effects(object)} or
#'     \code{marginaleffects::plot_predictions(object,
#'     condition = ...)}.
#'   \item Smooth posteriors directly:
#'     \code{conditional_smooths(object)} or
#'     \code{posterior_smooths(object, smooth = ...)}.
#'   \item MCMC traces / densities:
#'     \code{bayesplot::mcmc_trace(as_draws_df(object))} (the
#'     native \code{mcmc_plot.mvgam} wrapper is also pending
#'     the plot overhaul).
#'   \item Latent trend draws:
#'     \code{as_draws_df(object, variable = "^trend\\\\[",
#'     regex = TRUE)}.
#'   \item Input time-series EDA: \code{plot_mvgam_series(...)}.
#' }
#'
#' @param x An object of class \code{mvgam}.
#' @param ... Currently ignored.
#'
#' @return Stops with an informative error directing users to
#'   the alternatives above.
#'
#' @export
plot.mvgam <- function(x, ...) {
  stop(insight::format_error(c(
    paste0(
      "'plot.mvgam' is not yet ported on this branch."
    ),
    i = paste0(
      "Use 'pp_check(object)', 'conditional_effects(object)', ",
      "'conditional_smooths(object)', or extract latent trends ",
      "with 'as_draws_df(object, variable = \"^trend\\\\[\", ",
      "regex = TRUE)' in the meantime."
    ),
    i = paste0(
      "A ggplot-native 'plot.mvgam' is on the plot-overhaul ",
      "roadmap; see 'tasks/forecasting_roadmap.md'."
    )
  )))
}
