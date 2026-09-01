# hindcast.mvgam: posterior hindcasts at the training time grid.
# Thin wrapper around the existing build_hindcast_arms() helper
# in R/forecast.mvgam.R; returns an mvgam_forecast object with
# train_* / hindcasts slots populated and test_* / forecasts
# slots NULL.
#
# Semantic note: by default hindcasts read the Stan-fitted latent
# state directly from the posterior (`process_error = FALSE`).
# `trend[t, s]`, `mu_trend[t, s]` and `innovations_trend[t, s]`
# are all stored in the Stan output, so reconstruction is exact.
# This surfaces overfit-vs-predict gaps directly: a random walk
# with sigma -> 0 absorbs the response into the trend, so its
# hindcast response draws hug the training values even when the
# corresponding forecast() would be flat. Set
# `process_error = TRUE` to draw fresh innovations on top of the
# linpred instead.


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
#' Draw posterior hindcasts at the training time points. The
#' fitted latent state is read directly from the posterior draws
#' and the observation family then maps the trajectory back to
#' the response scale. For predictions at unseen times use
#' [forecast.mvgam].
#'
#' @param object A fitted [mvgam][mvgam::mvgam] object.
#' @param ... Currently unused.
#' @param type One of `"response"`, `"link"`, `"expected"`,
#'   `"trend"`, `"latent_state"`. `"response"` samples from the
#'   observation family (the default); `"expected"` returns the
#'   family's mean (`linkinv(eta)`); `"link"` returns the
#'   link-scale linpred; `"trend"` returns the latent-trend
#'   trajectory on the link scale. `"latent_state"` is only valid
#'   for closure-unit families (`occ()`, `nmix()` variants) and
#'   returns the posterior latent state (`psi` for `occ()`, `N`
#'   for the `nmix` variants) at each closure unit as an
#'   `mvgam_latent_state` object with its own `print()`,
#'   `summary()`, `as.data.frame()` and `plot()` methods.
#' @param ndraws Optional integer; the number of posterior draws
#'   to use. Defaults to all available draws.
#' @param obs_uncertainty Logical. When `FALSE`, skips
#'   observation-family sampling for `type = "response"`,
#'   returning the family mean (`linkinv(eta)`) instead.
#'   Defaults to `TRUE`.
#' @param process_error Logical. When `FALSE` (the default),
#'   hindcasts read the Stan-fitted latent state directly
#'   (`trend[t, s]` and `mu_trend[t, s]` from the posterior), and
#'   `type = "response"` samples only observation-family noise on
#'   top of that exact state. When `TRUE`, fresh innovations are
#'   drawn from the trend's covariance structure and added to the
#'   linear predictor instead, which is what
#'   `posterior_epred()` and `posterior_predict()` do under the
#'   same argument.
#' @inheritParams forecast.mvgam
#'
#' @return An object of class [mvgam_forecast-class][mvgam].
#'   Hindcasts are returned as a named list of `[ndraws, n_times]`
#'   matrices, one per series; `forecasts` / `test_*` slots are
#'   `NULL`.
#'
#' @seealso [mvgam_forecast-class][mvgam], [forecast.mvgam],
#'   [posterior_predict.mvgam] and [posterior_epred.mvgam] for the
#'   alternative marginal-MC prediction surface that integrates
#'   over the trend's stochastic dynamics instead of reading the
#'   fitted latent state.
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
#' hc <- hindcast(mod)
#' plot(hc)
#' }
#'
#' @method hindcast mvgam
#' @export
hindcast.mvgam <- function(object,
                           ...,
                           type = c("response", "link",
                                    "expected", "trend",
                                    "latent_state"),
                           ndraws = NULL,
                           obs_uncertainty = TRUE,
                           process_error = FALSE,
                           resp = NULL) {
  checkmate::assert_class(object, "mvgam")
  type <- match.arg(type)
  checkmate::assert_int(ndraws, lower = 1L, null.ok = TRUE)
  checkmate::assert_flag(obs_uncertainty)
  checkmate::assert_flag(process_error)
  checkmate::assert_string(resp, null.ok = TRUE)

  # latent_state branch: closure-unit families only; returns the
  # standalone mvgam_latent_state object built on per-unit draws
  # from posterior_latent_N() / posterior_occupancy(). The
  # mvgam_forecast container does not fit here because the
  # closure-unit latent state lives on a unit grid, not a
  # time-indexed series (single-season designs have no time axis
  # at all). Routing latent_state through hindcast() keeps the
  # user-facing entry point consistent while the return class
  # honours the unit grain.
  if (type == "latent_state") {
    return(hindcast_latent_state(object, ndraws = ndraws,
                                   resp = resp))
  }

  # Multivariate fan-out via the shared helper: hindcast operates
  # on the obs-side posterior, which returns a per-response list
  # on mv fits; scope per response and return a named list of
  # `mvgam_forecast` objects. Class the outer wrapper as
  # `mvgam_forecast` too so `plot()` / `print()` dispatch is
  # uniform whether the fit is uni- or multivariate.
  fan <- mv_resp_fan_out(object, resp)
  if (!is.null(fan)) {
    class(fan) <- "mvgam_forecast"
    attr(fan, "mv_wrapper") <- TRUE
    return(fan)
  }

  series_info <- resolve_series_info(object)
  series_levels <- series_info$series_levels

  draws_mat <- posterior::as_draws_matrix(object$fit)
  total_draws <- nrow(draws_mat)
  draw_idx <- resolve_draw_indices(total_draws, ndraws, NULL)

  training <- build_training_arms(object, series_levels)
  hindcasts <- build_hindcast_arms(
    object, training, type, draw_idx, obs_uncertainty,
    process_error = process_error,
    resp = resp
  )

  family_pars <- if (type == "link") {
    extract_family_pars_for_draws(object, draws_mat, draw_idx,
                                    resp = resp)
  } else {
    NULL
  }

  structure(
    list(
      family = object$family$family,
      family_pars = family_pars,
      type = type,
      series_names = factor(series_levels,
                            levels = series_levels),
      train_observations = training$observations,
      train_times = training$times,
      test_observations = NULL,
      test_times = NULL,
      hindcasts = hindcasts,
      forecasts = NULL
    ),
    class = "mvgam_forecast"
  )
}


# Build an mvgam_latent_state object from a fitted closure-unit
# model. Pulls per-draw latent state from the family kernel
# (`posterior_latent_N()` / `posterior_occupancy()`), then derives
# the unit grain (`series`, `time`) from the training data so the
# returned object's `unit` slot lines up with the columns of the
# draw matrix. `time` here is whatever column the user supplied as
# the closure-unit identifier (for single-season designs it is just
# the unit index; for multi-season designs it is the season).
#'@noRd
hindcast_latent_state <- function(object, ndraws = NULL,
                                    resp = NULL) {
  checkmate::assert_int(ndraws, lower = 1L, null.ok = TRUE)
  checkmate::assert_string(resp, null.ok = TRUE)
  if (!is_closure_unit_family(object$family)) {
    fam_nm <- resolve_family_name(object$family) %||% "?"
    stop(insight::format_error(c(
      "type = 'latent_state' is only valid for closure-unit fits.",
      x = paste0("Family '", fam_nm,
                  "' is not a closure-unit family."),
      i = paste0(
        "Refit with family = occ() or family = nmix() / ",
        "nmix('royle_nichols') / nmix('poisson_poisson'), or call ",
        "hindcast() with type = 'response' / 'link' / 'expected' / ",
        "'trend' instead."
      )
    )))
  }

  total_draws <- nrow(posterior::as_draws_matrix(object$fit))
  draw_idx <- resolve_draw_indices(total_draws, ndraws, NULL)

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

  # Per-unit grain, taken from the arrays the kernel was built on
  # rather than rebuilt here. `build_closure_unit_arrays()` numbers
  # units by first appearance over the time-major data and records the
  # grouping values in `unit_grid` in that same order, so reading it
  # keeps the labels attached to the draw columns they belong to.
  # Rebuilding the grid and sorting it by series relabelled every unit
  # on any fit carrying more than one series: one species' first year
  # was reported as another's, and a unit could be handed an abundance
  # below the count observed there.
  meta <- object$trend_metadata$variables %||%
    list(time_var = "time", series_var = "series")
  arrays <- extract_closure_unit_components(object)$arrays
  unit_df <- arrays$unit_grid
  if (is.null(unit_df)) {
    # A fit built before `unit_grid` was recorded. Fall back to the
    # first-appearance order of the training data, which is what the
    # arrays used, and do not sort it.
    td <- mvgam_training_data(object)
    unit_df <- unique(td[, c(meta$series_var, meta$time_var),
                         drop = FALSE])
  }
  names(unit_df)[match(c(meta$series_var, meta$time_var),
                         names(unit_df))] <- c("series", "time")
  rownames(unit_df) <- NULL

  if (ncol(draws) != nrow(unit_df)) {
    stop(insight::format_error(c(
      "Kernel draws and unit grid disagree.",
      x = paste0("ncol(draws) = ", ncol(draws),
                  ", unique (series, time) rows = ",
                  nrow(unit_df), ".")
    )))
  }

  fam_nm <- resolve_family_name(object$family) %||% ""
  is_nmix <- grepl("^nmix", fam_nm)
  state_label <- if (is_nmix) {
    "Latent abundance (N)"
  } else {
    "Occupancy probability (psi)"
  }
  state_short <- if (is_nmix) "N" else "psi"

  # Multi-season detection: closure-unit families expose
  # `multi_season` on the family attribute when the fit treats
  # `time` as a true seasonal index. For single-season fits
  # `time` is just the closure-unit identifier and the axis
  # labelling switches to "Closure-unit index" via `has_time`.
  multi_season_attr <- attr(object$family, "multi_season",
                              exact = TRUE)
  has_time <- isTRUE(multi_season_attr)

  new_mvgam_latent_state(
    draws       = draws,
    unit        = unit_df,
    family      = object$family,
    state_label = state_label,
    state_short = state_short,
    has_time    = has_time
  )
}
