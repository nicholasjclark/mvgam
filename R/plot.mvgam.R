#' Default plots for \pkg{mvgam} models
#'
#' Draws one of the standard views of a fitted model, chosen by `type`.
#'
#' @param x A fitted `mvgam` object.
#' @param type The plot to draw:
#'   \describe{
#'     \item{`"residuals"` (default)}{Dunn-Smyth residuals of one
#'       series against the fitted values, their Q-Q plot, ACF and
#'       partial ACF, as [pp_check.mvgam()] draws them. An [occ()] or
#'       [nmix()] fit has one residual per closure unit and draws the
#'       Q-Q plot alone.}
#'     \item{`"smooths"`}{The posterior of each smooth, from
#'       [conditional_smooths.mvgam()], with several smooths combined
#'       into one figure.}
#'     \item{`"trend"`}{The latent trend over the training times, from
#'       [hindcast.mvgam()], or the state component `component` names.
#'       A [jsdgam()] fit has sites in place of times and refuses it.}
#'     \item{`"factors"`}{The path of each latent factor.
#'       [active_factors()] reports the variance each factor carries.}
#'     \item{`"series"`}{The observed data the fit carries.
#'       [mvgam_data()] plots data before a fit.}
#'     \item{`"latent_state"`}{The latent state of an [occ()] or
#'       [nmix()] fit drawn from the covariates alone: occupancy `psi` or
#'       abundance `N`, by series and time. A multi-season fit pools the
#'       sites of each time.}
#'   }
#' @param series A series index or name. `"trend"` and `"series"` also
#'   take `"all"`, their default. `"residuals"` draws one series, the
#'   first by default. `"smooths"`, `"factors"` and `"latent_state"`
#'   ignore it.
#' @inheritParams forecast.mvgam
#' @param ndraws The number of posterior draws, or `NULL` for every
#'   draw. `"residuals"` takes 100 by default. `"factors"` and
#'   `"series"` ignore it.
#' @param ... Further arguments to the function that draws the plot,
#'   such as `smooths` for [conditional_smooths.mvgam()].
#'
#' @return A `ggplot` object, or a `patchwork` of several.
#'
#' @seealso [forecast.mvgam()] and its `plot()` method draw forecasts.
#'   [conditional_effects.mvgam()] draws covariate effects.
#'   [mcmc_plot.mvgam()] draws posterior diagnostics.
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
#' # Residual diagnostics, the default
#' plot(mod)
#'
#' # The smooth of x and the latent AR(1) trend
#' plot(mod, type = "smooths")
#' plot(mod, type = "trend")
#'
#' # The slope of a local linear trend
#' mod_llt <- update(mod, trend_formula = ~ LLT())
#' plot(mod_llt, type = "trend", component = "slope")
#'
#' # Next, plot(forecast(mod, newdata = simdat$data_test)) carries the
#' # trend forward
#' }
#'
#' @author Nicholas J Clark
#' @family model checking
#' @method plot mvgam
#' @export
plot.mvgam <- function(
  x,
  type = c("residuals", "smooths", "trend", "factors", "series",
            "latent_state"),
  series = NULL,
  resp = NULL,
  ndraws = NULL,
  component = NULL,
  ...
) {
  checkmate::assert_class(x, "mvgam")
  require_fitted_model(x, "plot")
  type <- match.arg(type)
  checkmate::assert_string(resp, null.ok = TRUE)
  component <- resolve_trend_component(x, component, type)

  switch(
    type,
    residuals = mvgam_resid_panel(
      x, series = series, ndraws = ndraws %||% 100L, resp = resp, ...
    ),
    smooths = {
      panels <- plot(
        conditional_smooths(x, ndraws = ndraws, resp = resp, ...),
        plot = FALSE
      )
      if (length(panels) == 1L) panels[[1L]] else
        patchwork::wrap_plots(panels)
    },
    trend = plot(
      hindcast(x, type = "trend", component = component, ndraws = ndraws,
               resp = resp),
      series = series,
      ...
    ),
    factors = plot_factors(x, ...),
    series = plot_mvgam_series(object = x, series = series, resp = resp,
                               ...),
    latent_state = plot_latent_state(x, ndraws = ndraws, ...)
  )
}
