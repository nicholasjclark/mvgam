#' Default plots for \pkg{mvgam} models
#'
#' Plot method for fitted `mvgam` objects. A thin S3 dispatcher
#' that routes by `type` to the canonical plotting surface for
#' each kind of summary. No bespoke plotting code lives here —
#' every branch forwards to a single dedicated function.
#'
#' @param x A fitted `mvgam` object.
#' @param type Character. The kind of plot to produce. One of:
#'   \describe{
#'     \item{`"residuals"` (default)}{Four-panel Dunn-Smyth
#'       residual diagnostic: residuals vs fitted, Q-Q, ACF,
#'       pACF. See [pp_check.mvgam()] for the underlying types
#'       (`resid_vs_fitted`, `resid_qq`, `resid_acf`,
#'       `resid_pacf`).}
#'     \item{`"smooths"`}{Conditional smooth posteriors via
#'       [conditional_smooths.mvgam()], combined into a single
#'       `patchwork` when more than one smooth is present.}
#'     \item{`"trend"`}{Latent trend over the training grid via
#'       [plot.mvgam_forecast()] on
#'       `hindcast(x, type = "trend")`.}
#'     \item{`"factors"`}{Latent dynamic factors. Errors clearly
#'       when the fit does not include latent factors. The
#'       returned ggplot carries a `"contribution"` attribute
#'       (data.frame with `Factor`, `PathVarShare`, `Lower`,
#'       `Upper`) giving the loading-weighted variance partition
#'       across factors. Access via
#'       `attr(plot(fit, type = "factors"), "contribution")` if
#'       you want the numeric summary without the figure.
#'       Per-draw factor paths themselves live on `object$fit` as
#'       `lv_trend[t, k]`; pull them with
#'       [posterior::as_draws_array()] when you need the raw
#'       draws (e.g. for custom summaries).}
#'     \item{`"series"`}{Input-data exploratory plot (works
#'       without a fit, wraps the observation data carried on
#'       the object).}
#'     \item{`"latent_state"`}{Per-(series, time) ribbon of the
#'       marginal latent state for closure-unit families: latent
#'       abundance `N` for `nmix()` (and its variants) or
#'       occupancy `psi` for `occ()`. Pools across sites for
#'       multi-season fits so each ribbon summarises the marginal
#'       posterior at each (species, time) cell. Errors clearly
#'       for non-closure-unit fits.}
#'   }
#'
#'   For posterior forecasts, parametric effects, random
#'   effects, and arbitrary conditional surfaces use the
#'   dedicated surfaces directly:
#'   \itemize{
#'     \item Forecasts: `forecast(x, newdata = ...) |> plot()`.
#'     \item Parametric / random effects: [conditional_effects()]
#'       (plus its `plot` method).
#'     \item Arbitrary conditional plots:
#'       [marginaleffects::plot_predictions()].
#'   }
#' @param series Optional series subset. Passed through to the
#'   series-aware branches (`"residuals"`, `"trend"`,
#'   `"series"`). Accepts `NULL` (all series, default), `"all"`,
#'   an integer index, or a character series name. Ignored by
#'   branches that summarise across series (`"smooths"`,
#'   `"factors"`).
#' @param ndraws Optional cap on posterior draws used by branches
#'   that subsample. Passed through to the relevant surface.
#' @param ... Additional arguments forwarded to the dispatched
#'   function (e.g. `smooths = ...` for `smooths`).
#'
#' @return A `ggplot` object (or `patchwork`, which inherits
#'   from `ggplot`).
#'
#' @seealso [pp_check.mvgam()], [conditional_effects.mvgam()],
#'   [conditional_smooths.mvgam()], [forecast.mvgam()],
#'   [hindcast.mvgam()], [plot.mvgam_forecast()],
#'   [marginaleffects::plot_predictions()]
#'
#' @author Nicholas J Clark
#' @method plot mvgam
#' @export
plot.mvgam <- function(
  x,
  type = c("residuals", "smooths", "trend", "factors", "series",
            "latent_state"),
  series = NULL,
  resp = NULL,
  ndraws = NULL,
  ...
) {
  checkmate::assert_class(x, "mvgam")
  type <- match.arg(type)
  # `resp` is the per-response selector for multivariate fits
  # (mvbind / mvbrmsformula). When NULL on a mv fit the per-type
  # branches below fan out across responses and return a named
  # list of plots; when set to a single response name the plot
  # is scoped to that response. Univariate fits ignore the arg.
  checkmate::assert_string(resp, null.ok = TRUE)

  switch(
    type,
    residuals = mvgam_resid_panel(
      x, ndraws = ndraws %||% 100L, resp = resp, ...
    ),
    smooths = wrap_effects_list(
      conditional_smooths(x, ndraws = ndraws, resp = resp, ...)
    ),
    trend = plot(
      hindcast(x, type = "trend", ndraws = ndraws),
      series = series,
      ...
    ),
    factors = plot_factors(x, ...),
    series = plot_mvgam_series(object = x, series = series, ...),
    latent_state = plot_latent_state(x, ndraws = ndraws, ...)
  )
}


# Internal: collapse the named list of ggplots returned by
# `conditional_effects.mvgam` / `conditional_smooths.mvgam` into
# a single ggplot via patchwork, so every `plot.mvgam` branch
# returns a single renderable plot object.
#'@noRd
wrap_effects_list <- function(eff_list) {
  if (length(eff_list) == 0L) {
    stop(insight::format_error(c(
      "No effects to plot.",
      i = "The fit has no parametric or random effects of that kind."
    )))
  }
  if (length(eff_list) == 1L) {
    return(eff_list[[1L]])
  }
  patchwork::wrap_plots(eff_list)
}
