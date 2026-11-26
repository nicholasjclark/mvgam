# hindcast.mvgam: posterior hindcasts at the training times, built by
# `build_hindcast_arms()` from the fitted latent states.


#' @rdname hindcast.mvgam
#' @export
hindcast <- function(object, ...) {
  UseMethod("hindcast", object)
}


#' Posterior hindcasts for an mvgam state-space model
#'
#' Posterior draws at the training time points. Each draw takes the
#' latent state the model inferred at each time. The observation
#' family maps it to the response scale. A hindcast shows how closely
#' the fitted states track the data. [forecast.mvgam()] predicts
#' unseen times.
#'
#' @inheritParams forecast.mvgam
#' @param object A fitted [mvgam][mvgam::mvgam] object, or for
#'   `summary()` an `mvgam_latent_state` object.
#' @param ... Unused. Anything passed here is refused.
#' @param x An `mvgam_latent_state` object.
#' @param probs For `summary()`, the four interval bounds, lowest
#'   first. Defaults to `c(0.025, 0.25, 0.75, 0.975)`, the 50% and 95%
#'   intervals.
#' @param digits Integer number of digits in the printed range.
#'   Defaults to 2.
#' @param series Names or indices of the series to plot, or `NULL` for
#'   every series.
#' @param type The scale of the draws, as in [forecast.mvgam()], or
#'   `"latent_state"` for a closure-unit family. `"latent_state"`
#'   returns the occupancy probability `psi` of an [occ()] model or the
#'   abundance `N` of an [nmix()] model at each closure unit, as an
#'   `mvgam_latent_state` object. Its `print()` gives the range of the
#'   unit medians. `summary()` and `as.data.frame()` return one row per
#'   unit, with its key columns (`series`, `time` and the `site` of a
#'   multi-season fit), the median and the 50% and 95% interval bounds.
#'   `plot()` draws them per series, as a line with ribbons over time or
#'   as points with interval bars over units without an order. A
#'   multi-season fit holds several sites in each season. Its plot draws
#'   the mean over those sites in each draw: the expected share of sites
#'   occupied, or the mean abundance per site.
#' @param process_error Logical. `FALSE` (the default) takes the
#'   fitted latent state of each draw. `type = "response"` then adds
#'   the observation noise alone. `TRUE` replaces the fitted states
#'   with the covariate part plus newly drawn innovations, as
#'   [posterior_epred.mvgam()] gives with the same argument. The
#'   hindcast then shows what the model predicts without the observed
#'   history. A `component` is a fitted state and needs `FALSE`.
#'
#' @return An [mvgam_forecast-class][mvgam] object. Its hindcasts are a
#'   named list of `[ndraws, n_times]` matrices, one per series. Its
#'   forecast slots are `NULL`.
#'
#' @seealso [forecast.mvgam()] extrapolates the fitted state.
#'   [posterior_predict.mvgam()] and [posterior_epred.mvgam()] predict
#'   from the covariate part of the model. The
#'   `r mvgam_article("mvbf", "multivariate article")` compares
#'   `type = "trend"` hindcasts of one latent state from a joint model
#'   and from each data source alone. The
#'   `r mvgam_article("nmix", "N-mixture article")` summarises
#'   `type = "latent_state"` hindcasts of abundance.
#' @examples
#' \dontrun{
#' set.seed(11)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                      n_timepoints = 120L, trend_model = AR())
#'
#' mod <- mvgam(
#'   y ~ s(x, k = 5),
#'   trend_formula = ~ AR(p = 1),
#'   data    = simdat$data_train,
#'   family  = poisson(),
#'   chains  = 2, silent = 2
#' )
#'
#' # The fitted states track the training counts
#' hc <- hindcast(mod)
#' plot(hc)
#'
#' # The AR(1) trend on the log scale
#' plot(hindcast(mod, type = "trend"))
#'
#' # Fresh innovations in place of the fitted states give the wider
#' # spread a model without the observed history would predict
#' plot(hindcast(mod, process_error = TRUE))
#'
#' # A local linear trend's slope, the level's growth per step
#' mod_llt <- update(mod, trend_formula = ~ LLT())
#' plot(hindcast(mod_llt, type = "trend", component = "slope"))
#'
#' # Next, forecast the held-out times with forecast()
#' }
#'
#' @family predictions
#' @family forecasting
#' @method hindcast mvgam
#' @export
hindcast.mvgam <- function(object,
                           ...,
                           type = c("response", "link",
                                    "expected", "trend",
                                    "latent_state"),
                           component = NULL,
                           ndraws = NULL,
                           draw_ids = NULL,
                           obs_uncertainty = TRUE,
                           process_error = FALSE,
                           resp = NULL) {
  checkmate::assert_class(object, "mvgam")
  require_fitted_model(object, "hindcast")
  require_series_axis(object, "hindcast")
  # The arguments follow `...` and are matched by name only. A
  # misspelt name would land in `...` and leave its default in place.
  rlang::check_dots_empty()
  type <- match.arg(type)
  validate_draw_selectors(ndraws, draw_ids)
  checkmate::assert_flag(obs_uncertainty)
  checkmate::assert_flag(process_error)
  checkmate::assert_string(resp, null.ok = TRUE)
  component <- resolve_trend_component(object, component, type)
  if (!is.null(component) && process_error) {
    stop(insight::format_error(c(
      "'component' requires 'process_error = FALSE'.",
      x = "A state component is the fitted state of each draw.",
      i = "Drop 'process_error' to hindcast the component."
    )), call. = FALSE)
  }

  # The latent state of a closure-unit family is indexed by unit. A
  # single-season design has no time axis. The result is then an
  # `mvgam_latent_state` of the per-unit draws of
  # `posterior_latent_N()`.
  if (type == "latent_state") {
    return(hindcast_latent_state(object, ndraws = ndraws,
                                 draw_ids = draw_ids))
  }

  # Multivariate fan-out: one `mvgam_forecast` per response, in a
  # wrapper of the same class. `plot()`, `print()` and `score()`
  # dispatch on it as on a single forecast.
  fan <- mv_resp_fan_out(object, resp, class = "mvgam_forecast")
  if (!is.null(fan)) {
    return(fan)
  }

  series_levels <- fitted_series_levels(object)
  reported <- reported_series(object, resp, series_levels)

  draw_idx <- resolve_draw_indices(posterior::ndraws(object), ndraws,
                                   draw_ids)

  training <- build_training_arms(object, series_levels, resp = resp)
  hindcasts <- build_hindcast_arms(
    object, training, type, draw_idx, obs_uncertainty,
    process_error = process_error,
    resp = resp,
    series_levels = reported,
    component = component
  )

  family_pars <- if (type == "link") {
    extract_family_pars_for_draws(object, draw_idx, resp = resp)
  } else {
    NULL
  }

  new_mvgam_forecast(object, type, resp, reported, training,
                     hindcasts = hindcasts, family_pars = family_pars,
                     component = component)
}


#' The hindcast of a closure-unit model's latent state
#'
#' Builds an `mvgam_latent_state` object from a fitted closure-unit
#' model. `posterior_latent_N()` gives the per-draw latent state.
#' `closure_unit_meta()` gives the key values of each draw column.
#'
#' @param object Fitted `mvgam` object with a closure-unit family
#' @param ndraws Integer number of draws to take, or `NULL` for all
#' @param draw_ids Integer indices of the draws to take, or `NULL`
#' @return An `mvgam_latent_state` object holding a draws by units
#'   matrix and one row of key values per unit
#' @noRd
hindcast_latent_state <- function(object, ndraws = NULL,
                                  draw_ids = NULL) {
  total_draws <- posterior::ndraws(object)
  draw_idx <- resolve_draw_indices(total_draws, ndraws, draw_ids)

  draws <- posterior_latent_N(object, draw_ids = draw_idx,
                              incl_autocor = TRUE)

  checkmate::assert_matrix(draws, mode = "numeric",
                            any.missing = FALSE,
                            .var.name = "kernel return value")

  # The key values of each unit, in the order of the draw columns
  unit_df <- closure_unit_meta(object, closure_unit_arrays_for(object))

  # `time` is a time axis when a multi-season fit indexes seasons by
  # it, or when a trend evolves along it. A single-season fit without
  # a temporal trend uses it only to identify closure units.
  has_time <- is_multi_season_family(object$family) ||
    !get_trend_type(object) %in% c("None", "ZMVN")

  new_mvgam_latent_state(
    draws       = draws,
    unit        = unit_df,
    family      = object$family,
    has_time    = has_time
  )
}
