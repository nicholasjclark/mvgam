# hindcast.mvgam: posterior hindcasts at the training times, built by
# `build_hindcast_arms()` from the fitted latent states.


#' Generic for posterior hindcasts
#'
#' @param object A model fit object.
#' @param ... Method-specific arguments.
#'
#' @return Method-specific.
#'
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
#' @param object A fitted [mvgam][mvgam::mvgam] object.
#' @param ... Unused. Anything passed here is refused.
#' @param type The scale of the draws, as in [forecast.mvgam()], or
#'   `"latent_state"` for a closure-unit family. `"latent_state"`
#'   returns the occupancy probability `psi` of an [occ()] model or the
#'   abundance `N` of an [nmix()] model at each closure unit, as an
#'   `mvgam_latent_state` object with `print()`, `summary()`,
#'   `as.data.frame()` and `plot()` methods.
#' @param process_error Logical. `FALSE` (the default) takes the
#'   fitted latent state of each draw. `type = "response"` then adds
#'   the observation noise alone. `TRUE` adds fresh trend innovations
#'   to the linear predictor, as [posterior_epred.mvgam()] does with
#'   the same argument.
#'
#' @return An [mvgam_forecast-class][mvgam] object. Its hindcasts are a
#'   named list of `[ndraws, n_times]` matrices, one per series. Its
#'   forecast slots are `NULL`.
#'
#' @seealso [forecast.mvgam()] extrapolates the fitted state.
#'   [posterior_predict.mvgam()] and [posterior_epred.mvgam()] predict
#'   by integrating over the trend's dynamics. The
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
#'   y ~ s(x),
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

  # The latent state of a closure-unit family is indexed by unit. A
  # single-season design has no time axis. The result is then an
  # `mvgam_latent_state` of the per-unit draws of
  # `posterior_latent_N()` or `posterior_occupancy()`.
  if (type == "latent_state") {
    return(hindcast_latent_state(object, ndraws = ndraws,
                                 draw_ids = draw_ids, resp = resp))
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

  draws_mat <- posterior::as_draws_matrix(object$fit)
  total_draws <- nrow(draws_mat)
  draw_idx <- resolve_draw_indices(total_draws, ndraws, draw_ids)

  training <- build_training_arms(object, series_levels, resp = resp)
  hindcasts <- build_hindcast_arms(
    object, training, type, draw_idx, obs_uncertainty,
    process_error = process_error,
    resp = resp,
    series_levels = reported
  )

  family_pars <- if (type == "link") {
    extract_family_pars_for_draws(object, draws_mat, draw_idx,
                                    resp = resp)
  } else {
    NULL
  }

  new_mvgam_forecast(object, type, resp, reported, training,
                     hindcasts = hindcasts, family_pars = family_pars)
}


# Build an mvgam_latent_state object from a fitted closure-unit
# model. The family kernel (`posterior_latent_N()` or
# `posterior_occupancy()`) gives the per-draw latent state. The
# closure-unit arrays give the (`series`, `time`) label of each draw
# column.
#'@noRd
hindcast_latent_state <- function(object, ndraws = NULL,
                                  draw_ids = NULL, resp = NULL) {
  checkmate::assert_string(resp, null.ok = TRUE)
  # `mvn()`, `mvt()` and `diri()` share the closure-unit layout and
  # hold no latent state.
  require_closure_unit_predict_type(object$family, "latent_state")

  total_draws <- posterior::ndraws(object)
  draw_idx <- resolve_draw_indices(total_draws, ndraws, draw_ids)

  kernel <- dispatch_closure_unit_method(object$family,
                                            "latent_state")
  kernel_formals <- names(formals(kernel))
  kernel_args <- list(
    object      = object,
    newdata     = NULL,
    draw_ids    = draw_idx,
    conditional = TRUE
  )
  if ("draw" %in% kernel_formals) kernel_args$draw <- FALSE
  draws <- do.call(kernel, kernel_args)

  checkmate::assert_matrix(draws, mode = "numeric",
                            any.missing = FALSE,
                            .var.name = "kernel return value")

  # `build_closure_unit_arrays()` records the grouping values of each
  # unit in `unit_grid`, in the order of the draw columns.
  meta <- axis_vars(object)
  unit_df <- extract_closure_unit_components(object)$arrays$unit_grid
  names(unit_df)[match(c(meta$series_var, meta$time_var),
                         names(unit_df))] <- c("series", "time")
  rownames(unit_df) <- NULL

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
