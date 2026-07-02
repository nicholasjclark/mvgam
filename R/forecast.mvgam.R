# forecast.mvgam: posterior forecasts for univariate state-space
# trends (RW / AR consecutive / AR sparse-lag / ARMA / ZMVN).
# Multivariate trends (VAR / CAR / cor=TRUE / VARMA(p,1)) and PW
# trends are handled in later phases.
#
# Per-draw loop wiring:
#   1. extract_last_state(fit, d, draws_mat = cached) for the
#      (params, last_state) tuple the kernel consumes.
#   2. extract_trend_linpred(fit, d, newdata, trend_lp_mat =
#      cached) for the trend-formula linpred at the training
#      tail and the forecast horizon (the centred convention).
#   3. propagate_trend() to forward the latent state on the link
#      scale by `h` steps.
#   4. Combine with the obs-side linpred for the same draw, map
#      through the family link, and optionally sample
#      observation-family noise -- all batched after the loop.
#
# Uncertainty-toggle semantics:
#   * `b_uncertainty = FALSE`     -> every forecast draw uses
#     row 1 of the cached obs and trend-submodel linpred
#     matrices, fixing every regression coefficient that feeds
#     either linear predictor (fixed effects, smooths, REs,
#     Hilbert-space GP bases).
#   * `trend_uncertainty = FALSE` -> every forecast draw uses
#     draw 1 of both `extract_last_state` (trend dynamics
#     params + initial state) AND the trend-submodel linpred,
#     so all trend-side quantities are jointly fixed. Fresh
#     innovations are still drawn per propagate call, so the
#     result reflects innovation noise alone.
#   * `obs_uncertainty = FALSE`   -> skip the family sampling
#     and return the family mean `linkinv(eta)` for
#     `type = "response"`.
#
# The returned `mvgam_forecast` object follows the contract
# documented in R/mvgam_forecast-class.R.


#' Posterior forecasts for an mvgam state-space model
#'
#' Draw posterior forecasts at user-supplied future time points,
#' alongside posterior hindcasts at the training time points.
#' The latent trend is propagated forward per draw using the
#' fitted state-space dynamics; the observation family then
#' maps the trajectory back to the response scale.
#'
#' @param object A fitted [mvgam][mvgam::mvgam] object.
#' @param newdata Optional `data.frame` with the same columns as
#'   the training data, containing the time / series cells at
#'   which forecasts are wanted. Rows with time values beyond
#'   the training grid drive the forecast horizon. When `NULL`,
#'   the returned object contains hindcasts only and the
#'   `$forecasts` / `$test_observations` / `$test_times` slots
#'   are `NULL`. The fallback is deliberately strict: if you
#'   want forecasts using the held-out data that was passed to
#'   `mvgam(..., newdata = X)` at fit time, pass it explicitly
#'   here as `forecast(mod, newdata = mod$test_data)`.
#' @param ... Currently unused.
#' @param type One of `"response"`, `"link"`, `"expected"`,
#'   `"trend"`. `"response"` samples from the observation family
#'   (the default); `"expected"` returns the family's mean
#'   (`linkinv(eta)`); `"link"` returns the link-scale linpred;
#'   `"trend"` returns the latent-trend trajectory on the link
#'   scale.
#' @param ndraws Optional integer; the number of posterior draws
#'   to use. Defaults to all available draws.
#' @param b_uncertainty Logical. When `FALSE`, every forecast
#'   draw uses the first posterior draw of all observation- and
#'   trend-formula regression coefficients. This includes fixed
#'   effects, smooth bases, random effect levels, and
#'   Hilbert-space `gp(x, k)` bases -- collapsing every source
#'   of regression-coefficient variation, not only the global
#'   `b` coefficients. Defaults to `TRUE`.
#' @param trend_uncertainty Logical. When `FALSE`, every
#'   forecast draw uses the first posterior draw of all
#'   trend-side quantities: the trend dynamics parameters
#'   (`sigma`, AR / MA / VAR coefficients), the initial latent
#'   state (`last_state$trends`), and the trend-formula linpred
#'   (`mu_trend`). Fresh innovations are still drawn per
#'   forecast call so the resulting spread reflects innovation
#'   noise alone. Defaults to `TRUE`.
#' @param obs_uncertainty Logical. When `FALSE`, skips
#'   observation-family sampling for `type = "response"`,
#'   returning the family mean (`linkinv(eta)`) instead.
#'   Defaults to `TRUE`.
#'
#' @return An object of class [mvgam_forecast-class][mvgam].
#'   Hindcasts and forecasts are returned as named lists of
#'   `[ndraws, n_times]` matrices, one per series.
#'
#' @seealso [mvgam_forecast-class][mvgam], [hindcast.mvgam],
#'   [posterior_predict.mvgam] and [posterior_epred.mvgam] for the
#'   alternative marginal-MC prediction surface that integrates
#'   over the trend's stochastic dynamics instead of extrapolating
#'   the fitted latent state. The online article
#'   \url{https://nicholasjclark.github.io/mvgam/articles/forecast_evaluation.html}
#'   walks through `forecast()` together with `score()`,
#'   `lfo_cv()` and `ensemble()` on a worked count-data example.
#' @examples
#' \donttest{
#' set.seed(11)
#' simdat <- sim_mvgam(family = poisson(), n_series = 1L,
#'                      n_timepoints = 80L, trend_model = AR(),
#'                      proportional_train = 0.75)
#'
#' mod <- mvgam(
#'   y ~ s(x),
#'   trend_formula = ~ AR(p = 1),
#'   data    = simdat$data_train,
#'   newdata = simdat$data_test,  # persisted on mod$test_data
#'   family  = poisson(),
#'   chains  = 2, silent = 2
#' )
#'
#' # Pass the held-out data explicitly to drive the forecast horizon.
#' fc <- forecast(mod, newdata = mod$test_data)
#' plot(fc)
#' }
#'
#' @importFrom generics forecast
#' @method forecast mvgam
#' @export
#' @export forecast
forecast.mvgam <- function(object,
                            newdata = NULL,
                            ...,
                            type = c("response", "link",
                                       "expected", "trend"),
                            ndraws = NULL,
                            b_uncertainty = TRUE,
                            trend_uncertainty = TRUE,
                            obs_uncertainty = TRUE,
                            resp = NULL) {
  checkmate::assert_class(object, "mvgam")
  type <- match.arg(type)
  checkmate::assert_int(ndraws, lower = 1L, null.ok = TRUE)
  checkmate::assert_flag(b_uncertainty)
  checkmate::assert_flag(trend_uncertainty)
  checkmate::assert_flag(obs_uncertainty)
  checkmate::assert_string(resp, null.ok = TRUE)
  fan <- mv_resp_fan_out(object, resp)
  if (!is.null(fan)) return(fan)
  newdata <- ensure_obs_placeholder_in_newdata(newdata, object$data)

  trend_specs <- object$mv_spec$trend_specs
  # Trendless fits forecast by projecting the obs-side linear
  # predictor onto `newdata`: no latent state propagation, just a
  # deterministic prediction at the held-out cells. The hindcast
  # branch already handles trendless fits via
  # `extract_trend_latent_states()` returning NULL; the forecast
  # branch below delegates to a posterior_predict / posterior_epred
  # pass on `fc_grid$data`. `type = "trend"` is undefined for
  # trendless fits (there is no latent state to extract); reject
  # up front so the hindcast and forecast branches both see a
  # valid `type` argument.
  is_trendless <- is.null(trend_specs)
  if (is_trendless && identical(type, "trend")) {
    stop(insight::format_error(c(
      "'type = \"trend\"' is not defined for trendless fits.",
      i = paste0("Use 'type = \"link\"', 'type = \"expected\"', ",
                 "or 'type = \"response\"' (the default).")
    )))
  }
  # `trend_specs` is either a single `mvgam_trend` (univariate
  # response) or a list of them (multivariate). Forecasting for
  # multivariate response models is pending; pick the first
  # trend spec when present and let the trend-type guard below
  # reject anything we don't yet support.
  trend_model <- if (is_trendless) {
    NULL
  } else if (is_multivariate_trend_specs(trend_specs)) {
    trend_specs[[1L]]
  } else {
    trend_specs
  }
  meta <- if (is_trendless) NULL else get_enriched_trend_metadata(object)

  series_info <- resolve_series_info(object)
  series_levels <- series_info$series_levels
  n_series <- length(series_levels)

  draws_mat <- posterior::as_draws_matrix(object$fit)
  total_draws <- nrow(draws_mat)
  if (is.null(ndraws)) ndraws <- total_draws
  if (ndraws > total_draws) {
    stop(insight::format_error(c(
      "'ndraws' exceeds the number of posterior draws.",
      x = paste0("Got ndraws = ", ndraws,
                 ", total draws = ", total_draws, "."),
      i = "Use a smaller value or set ndraws = NULL to use all draws."
    )))
  }
  draw_idx <- if (ndraws == total_draws) {
    seq_len(total_draws)
  } else {
    sort(sample.int(total_draws, ndraws))
  }

  training <- build_training_arms(object, series_levels, resp = resp)
  fc_grid <- resolve_forecast_grid(object, newdata, training,
                                     series_levels)

  # The hindcast slot inside a forecast() result uses the same
  # deterministic-state convention as hindcast()
  # (resample_innovations = FALSE) so the two surfaces agree at
  # the training grid.
  hindcasts <- build_hindcast_arms(
    object, training, type, draw_idx, obs_uncertainty,
    resample_innovations = FALSE, resp = resp
  )

  forecasts <- if (is.null(fc_grid)) {
    NULL
  } else if (is_trendless) {
    build_trendless_forecast_arms(
      object = object,
      fc_grid = fc_grid,
      type = type,
      draw_idx = draw_idx,
      obs_uncertainty = obs_uncertainty,
      series_levels = series_levels
    )
  } else {
    build_forecast_arms(
      object = object,
      trend_model = trend_model,
      meta = meta,
      training = training,
      fc_grid = fc_grid,
      type = type,
      draws_mat = draws_mat,
      draw_idx = draw_idx,
      b_uncertainty = b_uncertainty,
      trend_uncertainty = trend_uncertainty,
      obs_uncertainty = obs_uncertainty,
      series_levels = series_levels,
      resp = resp
    )
  }

  family_pars <- if (type == "link") {
    extract_family_pars_for_draws(object, draws_mat, draw_idx)
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
      test_observations = if (is.null(fc_grid)) NULL else
        fc_grid$observations,
      test_times = if (is.null(fc_grid)) NULL else fc_grid$times,
      hindcasts = hindcasts,
      forecasts = forecasts
    ),
    class = "mvgam_forecast"
  )
}


# ----- Series / training / forecast grids -------------------------

# Internal: resolve the canonical series levels from the fit.
# Falls back to factor levels of the obs data when `series_info`
# is absent.
#'@noRd
resolve_series_info <- function(object) {
  lv <- object$series_info$series_levels
  if (is.null(lv)) {
    d <- mvgam_training_data(object)
    series_var <- object$trend_metadata$variables$series_var %||%
      "series"
    lv <- levels(as.factor(d[[series_var]]))
  }
  list(series_levels = as.character(lv))
}


# Internal: per-series training observations + unique times,
# plus the cached time / series variable names and the obs data
# frame the downstream linpred calls subset.
#'@noRd
build_training_arms <- function(object, series_levels, resp = NULL) {
  d <- mvgam_training_data(object)
  meta_vars <- object$trend_metadata$variables %||%
    list(time_var = "time", series_var = "series")
  time_var <- meta_vars$time_var
  series_var <- meta_vars$series_var
  # Pick the response column. Multi-response (mvbrmsformula) fits
  # set `resp` via the per-outcome fan-out; honour it. Otherwise
  # `response_names` may be length > 1 (e.g. addition terms like
  # `y | trials(n)`); take its first element, which is the actual
  # response column.
  resp <- resp %||%
    (object$mv_spec$response_names %||%
       as.character(object$formula[[2L]]))[1L]

  series_fac <- as.factor(d[[series_var]])
  observations <- lapply(series_levels, function(lv) {
    idx <- series_fac == lv
    if (!any(idx)) return(numeric(0L))
    as.numeric(d[[resp]][idx])
  })
  names(observations) <- series_levels
  times <- lapply(series_levels, function(lv) {
    idx <- series_fac == lv
    if (!any(idx)) return(integer(0L))
    sort(unique(as.integer(d[[time_var]][idx])))
  })
  names(times) <- series_levels

  list(
    data = d,
    time_var = time_var,
    series_var = series_var,
    resp = resp,
    observations = observations,
    times = times
  )
}


# Internal: forecast horizon resolved from newdata. Returns NULL
# when newdata is NULL or has no times beyond training. Otherwise
# returns the per-series forecast times / observations and the
# forecast-only subset of newdata for downstream linpred calls.
#'@noRd
resolve_forecast_grid <- function(object, newdata, training,
                                    series_levels) {
  if (is.null(newdata)) return(NULL)
  checkmate::assert_data_frame(newdata, min.rows = 1L)
  time_var <- training$time_var
  series_var <- training$series_var
  if (!all(c(time_var, series_var) %in% names(newdata))) {
    stop(insight::format_error(c(
      paste0(
        "'newdata' must contain time / series columns '",
        time_var, "' and '", series_var, "'."
      ),
      x = paste0(
        "Got columns: ", paste(names(newdata), collapse = ", "),
        "."
      )
    )))
  }
  resp <- training$resp

  series_fac <- factor(newdata[[series_var]],
                         levels = series_levels)
  if (any(is.na(series_fac))) {
    stop(insight::format_error(c(
      "'newdata' contains series levels not seen at fit time.",
      x = paste0(
        "Unknown levels: ",
        paste(
          unique(as.character(newdata[[series_var]])[
            is.na(series_fac)
          ]),
          collapse = ", "
        ), "."
      )
    )))
  }

  fc_times <- lapply(series_levels, function(lv) {
    idx <- series_fac == lv
    if (!any(idx)) return(integer(0L))
    nt <- sort(unique(as.integer(newdata[[time_var]][idx])))
    setdiff(nt, training$times[[lv]])
  })
  names(fc_times) <- series_levels

  if (all(lengths(fc_times) == 0L)) return(NULL)

  fc_observations <- lapply(series_levels, function(lv) {
    idx <- series_fac == lv &
      newdata[[time_var]] %in% fc_times[[lv]]
    if (!any(idx) || !(resp %in% names(newdata))) return(NULL)
    as.numeric(newdata[[resp]][idx])
  })
  names(fc_observations) <- series_levels

  keep <- vapply(seq_len(nrow(newdata)), function(i) {
    lv <- as.character(series_fac[i])
    if (is.na(lv)) return(FALSE)
    as.integer(newdata[[time_var]][i]) %in% fc_times[[lv]]
  }, logical(1L))
  fc_data <- newdata[keep, , drop = FALSE]

  list(
    data = fc_data,
    times = fc_times,
    observations = fc_observations
  )
}


# Internal: per-series training-tail rows of length up to
# `max_lag`. Used to build the kernel's `linpreds` past-tail
# block via extract_component_linpred on these rows alone.
#
# Sorting each per-series block by time before stacking is
# load-bearing: the kernel reads `linpreds` rows in temporal
# order to centre `lv = trend - mu_trend` at each lag. A
# tail block with reversed or shuffled time order would offset
# the centring by `mu_trend[T] - mu_trend[T-1]`, injecting a
# spurious jump at the first forecast step.
#'@noRd
build_training_tail_data <- function(training, max_lag) {
  if (max_lag <= 0L) return(NULL)
  series_levels <- names(training$observations)
  rows <- lapply(series_levels, function(lv) {
    ts <- training$times[[lv]]
    tail_ts <- tail(ts, max_lag)
    idx <- training$data[[training$series_var]] == lv &
      training$data[[training$time_var]] %in% tail_ts
    sub <- training$data[idx, , drop = FALSE]
    sub[order(sub[[training$time_var]]), , drop = FALSE]
  })
  do.call(rbind, rows)
}


# ----- Hindcast arm -----------------------------------------------

# Internal: per-series hindcast matrices. Delegates to the
# existing posterior_predict / posterior_epred / component-
# linpred infrastructure so dpar / trials / family handling stays
# centralised. Slices to the chosen draws.
#
# `resample_innovations` controls the latent-state pathway:
# FALSE uses the Stan-fitted `trend[t, s]` and `mu_trend[t, s]`
# directly (deterministic-state hindcast); TRUE draws fresh
# innovations from the trend's covariance and adds them on top via
# posterior_epred / posterior_predict's marginal MC pathway.
# Hindcasts default to FALSE so a
# perfectly-fit trend (e.g. RW with sigma -> 0) returns response
# draws that hug the training values, surfacing
# overfit-versus-predict gaps directly.
#'@noRd
build_hindcast_arms <- function(object, training, type, draw_idx,
                                  obs_uncertainty,
                                  resample_innovations = FALSE,
                                  resp = NULL) {
  series_levels <- names(training$observations)
  out <- vector("list", length(series_levels))
  names(out) <- series_levels
  for (s in seq_along(series_levels)) {
    lv <- series_levels[s]
    ts <- training$times[[lv]]
    if (length(ts) == 0L) {
      out[[s]] <- matrix(NA_real_, nrow = length(draw_idx),
                           ncol = 0L)
      next
    }
    sub <- training$data[
      training$data[[training$series_var]] == lv, , drop = FALSE
    ]
    sub <- sub[order(sub[[training$time_var]]), , drop = FALSE]
    # Closure-unit families ship multiple rows per (series, time)
    # for the per-visit detection grain. The `"trend"` and `"link"`
    # surfaces live at the (time, series) grain of the trend matrix,
    # so collapse `sub` to one row per unique time before the
    # linpred call. The detection-marginalised `"expected"` and
    # `"response"` surfaces keep the per-row sub-data because the
    # latent state varies per closure unit.
    if (type %in% c("trend", "link") &&
          is_closure_unit_family(object$family)) {
      sub_for_linpred <- sub[
        !duplicated(sub[[training$time_var]]), , drop = FALSE
      ]
    } else {
      sub_for_linpred <- sub
    }
    out[[s]] <- hindcast_one_series(
      object, sub_for_linpred, type, draw_idx, obs_uncertainty,
      resample_innovations, resp = resp
    )
  }
  out
}


# Internal: dispatch on `type` for a single series's hindcast.
# Standard families pull the per-draw conditional `trend[t, s]`
# directly from the stanfit (the same per-cell value Stan would
# emit as `ypred` in generated quantities), compose with the
# per-draw deterministic obs-side linpred, and dispatch the
# family RNG via `predict_single_response`. Closure-unit
# families keep their joint-over-unit marginalisation entries
# through `posterior_epred` / `posterior_predict`.
# `resample_innovations` is preserved on the signature for the
# closure-unit branch (where it maps to `process_error` in the
# underlying helpers); it is a no-op on the standard-family
# branch because the per-draw conditional state already supplies
# the trajectory.
#'@noRd
hindcast_one_series <- function(object, sub_data, type, draw_idx,
                                  obs_uncertainty,
                                  resample_innovations = FALSE,
                                  resp = NULL) {
  family <- if (!is.null(resp)) {
    get_family_for_resp(object, resp)
  } else {
    object$family
  }
  is_closure <- is_closure_unit_family(family)

  if (is_closure) {
    full <- switch(
      type,
      "trend" = extract_component_linpred(
        mvgam_fit = object, newdata = sub_data,
        component = "trend", incl_latent_state = TRUE
      ),
      "link" = extract_component_linpred(
        mvgam_fit = object, newdata = sub_data,
        component = "obs", resp = resp
      ) + extract_component_linpred(
        mvgam_fit = object, newdata = sub_data,
        component = "trend", incl_latent_state = TRUE
      ),
      "expected" = posterior_epred(
        object, newdata = sub_data, ndraws = NULL,
        process_error = resample_innovations, resp = resp
      ),
      "response" = if (isTRUE(obs_uncertainty)) {
        posterior_predict(
          object, newdata = sub_data, ndraws = NULL,
          process_error = resample_innovations, resp = resp
        )
      } else {
        posterior_epred(
          object, newdata = sub_data, ndraws = NULL,
          process_error = resample_innovations, resp = resp
        )
      }
    )
  } else {
    # Per-draw conditional `trend[t, s]` from the stanfit (`Z @
    # lv_trend + mu_trend` per draw, exactly as Stan composes in
    # transformed parameters). Returns NULL for trendless fits, in
    # which case the trend contribution is the zero matrix.
    draws_mat <- posterior::as_draws_matrix(object$fit)
    trend_state <- extract_trend_latent_states(
      mvgam_fit = object, newdata = sub_data, full_draws = draws_mat
    )
    if (is.null(trend_state)) {
      trend_state <- matrix(0, nrow = nrow(draws_mat),
                              ncol = nrow(sub_data))
    }
    obs_linpred <- extract_component_linpred(
      mvgam_fit = object, newdata = sub_data,
      component = "obs", resp = resp
    )
    if (is.list(obs_linpred) && !is.matrix(obs_linpred)) {
      stop(insight::format_error(c(
        "Hindcast received a list-shaped obs linpred with no 'resp' scope.",
        i = paste0("Multivariate fits should fan out via the ",
                   "'hindcast.mvgam' entry; this is an internal bug.")
      )))
    }
    linpred <- obs_linpred + trend_state
    full <- switch(
      type,
      "trend" = trend_state,
      "link" = linpred,
      "expected" = family$linkinv(linpred),
      "response" = if (isTRUE(obs_uncertainty)) {
        predict_single_response(
          object = object, linpred_resp = linpred,
          resp = resp, draw_ids = seq_len(nrow(linpred)),
          ndraws = nrow(linpred), newdata = sub_data,
          is_multivariate = !is.null(resp)
        )
      } else {
        family$linkinv(linpred)
      }
    )
  }

  if (is.list(full) && !is.matrix(full)) {
    stop(insight::format_error(c(
      "Hindcast received a list-shaped posterior with no 'resp' scope.",
      i = paste0("Multivariate fits should fan out via the ",
                 "'hindcast.mvgam' entry; this is an internal bug.")
    )))
  }
  full[draw_idx, , drop = FALSE]
}


# ----- Forecast arm -----------------------------------------------

# Internal: trendless-forecast arm. When the fit has no
# `trend_formula`, forecasting reduces to evaluating the obs-side
# linear predictor on the held-out newdata; no latent-state
# propagation runs. Returns per-series `[ndraws, n_times]` matrices
# in the same shape `build_forecast_arms()` would, so the downstream
# `mvgam_forecast` slot semantics and `plot.mvgam_forecast` are
# unchanged.
#
# Passes `draw_ids` through to the posterior_* dispatcher so the
# subset is materialised once (no all-draws fetch followed by a
# slice).
#'@noRd
build_trendless_forecast_arms <- function(object, fc_grid, type,
                                            draw_idx,
                                            obs_uncertainty,
                                            series_levels) {
  # `type = "trend"` is rejected at the top of `forecast.mvgam()`
  # before any work runs; the switch below is total over the
  # remaining three types.
  fc_data <- fc_grid$data
  series_var <- object$trend_metadata$variables$series_var %||%
    "series"
  predictor <- switch(
    type,
    "link"     = posterior_linpred,
    "expected" = posterior_epred,
    "response" = if (isTRUE(obs_uncertainty)) {
      posterior_predict
    } else {
      posterior_epred
    }
  )
  full <- predictor(object, newdata = fc_data, draw_ids = draw_idx)

  series_fac <- factor(fc_data[[series_var]], levels = series_levels)
  out <- vector("list", length(series_levels))
  names(out) <- series_levels
  for (s in seq_along(series_levels)) {
    cols <- which(series_fac == series_levels[s])
    out[[s]] <- if (length(cols) == 0L) {
      matrix(NA_real_, nrow = length(draw_idx), ncol = 0L)
    } else {
      full[, cols, drop = FALSE]
    }
  }
  out
}


# Internal: per-series forecast matrices. Two passes:
#   1. Per-draw kernel loop: propagate the latent trend forward
#      `h_max` steps for each chosen draw. Cheap row slices of
#      pre-computed caches; only the C++ kernel runs per draw.
#   2. Batched obs-side + family pass: combine the [ndraws_use,
#      nobs_fc] trend matrix with the cached obs linpred and
#      apply `linkinv` / `sample_from_family` once across all
#      draws.
#'@noRd
build_forecast_arms <- function(object, trend_model, meta,
                                  training, fc_grid, type,
                                  draws_mat, draw_idx,
                                  b_uncertainty,
                                  trend_uncertainty,
                                  obs_uncertainty,
                                  series_levels,
                                  resp = NULL) {
  n_series <- length(series_levels)
  ndraws_use <- length(draw_idx)
  per_series_h <- vapply(fc_grid$times, length, integer(1L))
  h_max <- max(per_series_h)

  max_lag <- as.integer(meta$max_lag %||% 0L)
  tail_data <- build_training_tail_data(training, max_lag)
  obs_struct_fc <- get_observation_structure(object,
                                               newdata = fc_grid$data)
  obs_struct_tail <- if (is.null(tail_data)) NULL else
    get_observation_structure(object, newdata = tail_data)
  nobs_fc <- length(obs_struct_fc$time)

  # Pre-compute trend-formula linpred matrices for the past
  # tail and the forecast grid. extract_component_linpred is the
  # expensive call; do it ONCE per matrix, then slice rows per
  # draw inside the loop.
  has_trend_lp <- !is.null(object$trend_model)
  trend_lp_fc <- if (has_trend_lp) {
    extract_component_linpred(
      mvgam_fit = object, newdata = fc_grid$data,
      component = "trend", incl_latent_state = FALSE
    )
  } else {
    NULL
  }
  trend_lp_tail <- if (has_trend_lp && !is.null(tail_data)) {
    extract_component_linpred(
      mvgam_fit = object, newdata = tail_data,
      component = "trend", incl_latent_state = FALSE
    )
  } else {
    NULL
  }
  if (is.list(trend_lp_fc) && !is.matrix(trend_lp_fc)) {
    stop(insight::format_error(
      "Multivariate response forecasts are not yet supported."
    ))
  }

  # Pre-compute the obs-formula linpred for the forecast grid;
  # this and the trend caches above receive the b_uncertainty
  # toggle (collapsed to row 1 when FALSE).
  obs_full <- if (type %in% c("link", "response", "expected")) {
    extract_component_linpred(
      mvgam_fit = object, newdata = fc_grid$data,
      component = "obs", resp = resp
    )
  } else {
    NULL
  }
  if (isTRUE(!b_uncertainty)) {
    if (!is.null(obs_full)) obs_full <- matrix(
      obs_full[1L, ], nrow = nrow(obs_full), ncol = ncol(obs_full),
      byrow = TRUE
    )
    if (!is.null(trend_lp_fc)) trend_lp_fc <- matrix(
      trend_lp_fc[1L, ], nrow = nrow(trend_lp_fc),
      ncol = ncol(trend_lp_fc), byrow = TRUE
    )
    if (!is.null(trend_lp_tail)) trend_lp_tail <- matrix(
      trend_lp_tail[1L, ], nrow = nrow(trend_lp_tail),
      ncol = ncol(trend_lp_tail), byrow = TRUE
    )
  }

  # CAR forecasts need a per-step time-gap vector for the
  # kernel's `time_dis` parameter. Computed once here because
  # both inputs (per-series last training time + the forecast
  # time grid) are deterministic across draws -- moving this
  # inside the per-draw loop would re-derive the same gaps
  # `ndraws_use` times. NULL for non-CAR trends.
  fc_time <- if (identical(meta$trend_type, "CAR")) {
    compute_car_forecast_time(object, fc_grid, series_levels)
  } else {
    NULL
  }

  # PW forecasts evaluate a piecewise function at user-
  # supplied forecast times. Pre-compute the shared
  # absolute-time vector for the horizon and the training
  # time range that drives the changepoint frequency; these
  # are deterministic across draws, like CAR's gap vector.
  pw_extras <- if (identical(meta$trend_type, "PW")) {
    compute_pw_forecast_extras(object, training, fc_grid,
                                 series_levels)
  } else {
    NULL
  }

  # Factor-model precompute: when the fit is a latent-factor
  # model (n_lv < n_series) the trend recursion runs in
  # n_lv-dimensional LV space and a per-draw Z projects the
  # propagated `[h, n_lv]` LV trajectory back to `[h, n_series]`.
  # Pull the whole Z array once via the shared
  # `extract_Z_loadings()` helper (returns [ndraws, n_series,
  # n_lv], preferring `Z_tilde` when present); the inner loop
  # slices per draw. NULL for full-rank fits.
  n_lv_trend <- as.integer(
    object$standata$N_lv_trend %||% n_series
  )
  Z_arr <- if (n_lv_trend < n_series &&
                 meta$trend_type %in% c("RW", "AR", "VAR")) {
    resolve_Z_loadings(object, draws_mat, n_series, n_lv_trend)
  } else {
    NULL
  }

  # Per-draw kernel loop. `trend_flat[i, j]` holds the trend
  # value at draw `draw_idx[i]`, observation row j of the
  # forecast grid (obs_struct_fc ordering).
  trend_flat <- matrix(NA_real_, nrow = ndraws_use,
                         ncol = nobs_fc)
  for (i in seq_len(ndraws_use)) {
    # When `trend_uncertainty = FALSE` we pin every trend-side
    # quantity to draw 1: the dynamics params + initial state
    # (`d_state`) AND the trend-submodel linpred (`d_lin`).
    # Pinning only `d_state` would leave the centred-convention
    # `mu_trend` varying across draws, producing a mixture that
    # neither marginalises trend-parameter uncertainty cleanly
    # nor matches the "innovation-noise only" intent of the
    # toggle.
    d_state <- if (isTRUE(trend_uncertainty)) draw_idx[i] else 1L
    d_lin <- d_state
    Z_slice_d <- if (is.null(Z_arr)) {
      NULL
    } else {
      Z_arr[d_state, , , drop = FALSE][1L, , , drop = TRUE]
    }
    fc_link_d <- propagate_one_draw(
      object = object,
      trend_model = trend_model,
      meta = meta,
      training = training,
      fc_grid = fc_grid,
      draws_mat = draws_mat,
      d_state = d_state,
      d_lin = d_lin,
      h_max = h_max,
      n_series = n_series,
      tail_data = tail_data,
      trend_lp_tail = trend_lp_tail,
      trend_lp_fc = trend_lp_fc,
      obs_struct_tail = obs_struct_tail,
      obs_struct_fc = obs_struct_fc,
      fc_time = fc_time,
      pw_extras = pw_extras,
      Z_slice = Z_slice_d
    )
    trend_flat[i, ] <- flatten_grid_to_obs_order(fc_link_d,
                                                    obs_struct_fc)
  }

  # Batched obs combination + family pass.
  if (type == "trend") {
    return(slice_per_series(trend_flat, fc_grid, obs_struct_fc,
                              ndraws_use, series_levels))
  }
  eta_full <- obs_full[draw_idx, , drop = FALSE] + trend_flat
  if (type == "link") {
    return(slice_per_series(eta_full, fc_grid, obs_struct_fc,
                              ndraws_use, series_levels))
  }
  family_for_arm <- if (!is.null(resp)) {
    get_family_for_resp(object, resp)
  } else {
    object$family
  }
  mu <- family_for_arm$linkinv(eta_full)
  if (type == "expected" || isTRUE(!obs_uncertainty)) {
    return(slice_per_series(mu, fc_grid, obs_struct_fc,
                              ndraws_use, series_levels))
  }
  resp_mat <- sample_family_batched(object, mu, fc_grid$data,
                                      ndraws_use, draw_idx,
                                      family = family_for_arm,
                                      resp = resp)
  slice_per_series(resp_mat, fc_grid, obs_struct_fc,
                     ndraws_use, series_levels)
}


# Internal: one-draw propagation pipeline. Returns a `[h_max,
# n_series]` link-scale trajectory. In factor mode (Z_slice is
# non-NULL) the recursion runs in n_lv-dimensional latent space
# and the returned matrix is projected back to `n_series`
# columns via `X %*% t(Z_slice)`.
#'@noRd
propagate_one_draw <- function(object, trend_model, meta, training,
                                 fc_grid, draws_mat, d_state, d_lin,
                                 h_max, n_series, tail_data,
                                 trend_lp_tail, trend_lp_fc,
                                 obs_struct_tail, obs_struct_fc,
                                 fc_time = NULL, pw_extras = NULL,
                                 Z_slice = NULL) {
  ls_d <- extract_last_state(object, d_state,
                                draws_mat = draws_mat)

  # PW bypasses the centred-convention linpred machinery: the
  # trend is a closed-form Prophet-style evaluation at user-
  # supplied times. Dispatch to propagate_trend with the
  # PW-specific extras (fc_times, training_times, cap) and
  # return early.
  if (identical(meta$trend_type, "PW")) {
    return(propagate_trend(
      trend_model = trend_model,
      params = ls_d$params,
      h = h_max,
      n_series = n_series,
      fc_times = pw_extras$fc_times,
      training_times = pw_extras$training_times,
      cap = pw_extras$cap,
      changepoint_range = meta$pw_changepoint_range
    ))
  }

  max_lag <- as.integer(meta$max_lag %||% 0L)

  # Factor mode: propagate in the LV-grain space `extract_last_
  # state()` returned (n_lv columns instead of n_series). The
  # trend-side linpred, if any, would enter the observation
  # scale rather than the latent recursion, so passing zero
  # linpreds here matches the standata semantics.
  # `apply_factor_projection()` maps the LV trajectory back to
  # series scale after propagation.
  in_factor_mode <- !is.null(Z_slice) &&
    !is.null(ls_d$n_lv_active)
  n_prop <- if (in_factor_mode) ls_d$n_lv_active else n_series

  lp_history <- if (max_lag == 0L) {
    matrix(0, nrow = 0L, ncol = n_prop)
  } else if (in_factor_mode || is.null(trend_lp_tail)) {
    matrix(0, nrow = max_lag, ncol = n_prop)
  } else {
    grid <- reshape_linpred_to_grid(
      trend_lp_tail[d_lin, ], obs_struct_tail
    )
    pad_or_trim_rows(grid, max_lag)
  }
  lp_forecast <- if (in_factor_mode || is.null(trend_lp_fc)) {
    matrix(0, nrow = h_max, ncol = n_prop)
  } else {
    grid <- reshape_linpred_to_grid(
      trend_lp_fc[d_lin, ], obs_struct_fc
    )
    pad_or_trim_rows(grid, h_max)
  }
  linpreds_combined <- rbind(lp_history, lp_forecast)

  fc_lv <- propagate_trend(
    trend_model = trend_model,
    params = ls_d$params,
    h = h_max,
    n_series = n_prop,
    last_state = ls_d$last_state,
    linpreds = linpreds_combined,
    time = fc_time
  )
  if (in_factor_mode) {
    apply_factor_projection(fc_lv, Z_slice, n_series)
  } else {
    fc_lv
  }
}


# Internal: project an `[h, n_lv]` LV-scale forecast trajectory
# back to `[h, n_series]` observation scale via a `[n_series,
# n_lv]` per-draw loadings slice. One place to change if the
# loading convention ever shifts, and a single call site to
# assert dimensions on.
#'@noRd
apply_factor_projection <- function(lv_traj, Z_slice, n_series) {
  checkmate::assert_matrix(lv_traj, min.rows = 0L)
  checkmate::assert_matrix(Z_slice, nrows = n_series)
  lv_traj %*% t(Z_slice)
}


# Internal: per-step time gap vector for a CAR forecast. CAR(1)
# is continuous-time, so the kernel needs the gap from the last
# observed time to each forecast time. The kernel accepts a
# single length-`h` vector (no per-series matrix), so all
# series must share the same forecast time grid -- this is the
# common case for mvgam CAR fits where `time` is a global
# continuous coordinate and the test split is also shared.
# Heterogeneous per-series forecast times error with a
# message; the kernel-side extension to accept a `[h,
# n_series]` matrix is pending.
#'@noRd
compute_car_forecast_time <- function(object, fc_grid,
                                        series_levels) {
  n_series <- length(series_levels)
  last_times <- extract_last_observed_times(object, n_series)
  gap_per_series <- vector("list", n_series)
  for (s in seq_len(n_series)) {
    lv <- series_levels[s]
    fut_t <- sort(fc_grid$times[[lv]])
    if (length(fut_t) == 0L) {
      gap_per_series[[s]] <- numeric(0L)
      next
    }
    gap_per_series[[s]] <- diff(c(last_times[s], fut_t))
  }
  # Drop empty (no-forecast) series from the consistency check.
  nonempty <- vapply(gap_per_series, length, integer(1L)) > 0L
  if (!any(nonempty)) return(numeric(0L))
  ref <- gap_per_series[[which(nonempty)[1L]]]
  for (s in which(nonempty)) {
    if (!isTRUE(all.equal(gap_per_series[[s]], ref))) {
      stop(insight::format_error(c(
        paste0(
          "CAR forecasts currently require all series to share ",
          "the same forecast time grid."
        ),
        i = paste0(
          "Heterogeneous per-series time gaps will be supported ",
          "after the CAR kernel accepts a per-series time matrix."
        )
      )))
    }
  }
  ref
}


# Internal: extras the PW kernel needs for its closed-form
# evaluation:
#   * `fc_times`: shared horizon time vector (one per step).
#   * `training_times`: training time vector (drives the
#     Prophet-style horizon-changepoint frequency).
#   * `cap`: forecast-horizon carrying capacities [h, n_series]
#     for the logistic growth path; NULL for linear.
#
# All series are assumed to share the same forecast time grid
# (mirrors the CAR shared-time constraint); heterogeneous
# per-series grids would need a per-series PW evaluation loop
# in `propagate_pw`.
#'@noRd
compute_pw_forecast_extras <- function(object, training,
                                          fc_grid, series_levels) {
  n_series <- length(series_levels)
  # Take the first non-empty series' forecast times as the
  # shared grid; validate the others match.
  fc_times <- NULL
  for (lv in series_levels) {
    ts <- as.numeric(fc_grid$times[[lv]])
    if (length(ts) == 0L) next
    if (is.null(fc_times)) {
      fc_times <- sort(unique(ts))
    } else if (!isTRUE(all.equal(sort(unique(ts)), fc_times))) {
      stop(insight::format_error(c(
        paste0(
          "PW forecasts currently require all series to share ",
          "the same forecast time grid."
        ),
        i = paste0(
          "Heterogeneous per-series PW horizons are pending."
        )
      )))
    }
  }
  if (is.null(fc_times)) fc_times <- numeric(0L)

  # Training time range drives change_freq inside propagate_pw.
  training_times <- sort(unique(unlist(training$times)))

  # Logistic PW needs a cap matrix [h, n_series] from newdata.
  # Detect from the trend_model spec. Linear PW leaves cap NULL.
  trend_specs <- object$mv_spec$trend_specs
  spec <- if (is_multivariate_trend_specs(trend_specs)) {
    trend_specs[[1L]]
  } else {
    trend_specs
  }
  growth <- spec$growth %||% "linear"
  cap <- if (identical(growth, "logistic")) {
    extract_pw_cap_matrix(fc_grid, spec, fc_times,
                            series_levels,
                            family = object$family)
  } else {
    NULL
  }

  list(fc_times = fc_times,
       training_times = training_times,
       cap = cap)
}


# Internal: build the `[h, n_series]` cap matrix for PW
# logistic from the forecast newdata. The cap column name is
# stored on the trend spec via `spec$cap` (defaults to "cap").
#
# User-supplied cap is on the RESPONSE scale (e.g. "carrying
# capacity of 100 individuals"); the C++ kernel expects cap on
# the LINK scale because the logistic formula evaluates
# `cap * inv_logit(...)` as the trend's contribution to eta.
# Apply the family's link function to bring cap to the link
# scale. Together with the PW convention of fitting on a
# zero-intercept observation formula (`y ~ -1`), this gives
# `E[Y]` saturating at exactly the user-stated cap.
#
# Cells without an observed cap row error rather than silently
# substituting -- propagating an unknown cap into the
# inverse-logit would produce a meaningless forecast.
#'@noRd
extract_pw_cap_matrix <- function(fc_grid, spec, fc_times,
                                    series_levels, family) {
  cap_var <- spec$cap %||% "cap"
  d <- fc_grid$data
  if (!(cap_var %in% names(d))) {
    stop(insight::format_error(c(
      paste0(
        "PW logistic requires a '", cap_var,
        "' column in 'newdata' for the forecast cells."
      ),
      i = paste0(
        "Each (time, series) forecast cell needs a cap value."
      )
    )))
  }
  series_var <- attr(d, "series_var") %||% "series"
  time_var <- attr(d, "time_var") %||% "time"
  cap_mat <- matrix(NA_real_, nrow = length(fc_times),
                      ncol = length(series_levels))
  for (s in seq_along(series_levels)) {
    lv <- series_levels[s]
    for (k in seq_along(fc_times)) {
      ix <- which(d[[series_var]] == lv &
                    as.numeric(d[[time_var]]) == fc_times[k])
      if (length(ix) == 0L) next
      cap_mat[k, s] <- as.numeric(d[[cap_var]][ix[1L]])
    }
  }
  if (any(is.na(cap_mat))) {
    stop(insight::format_error(c(
      paste0(
        "PW logistic: missing 'cap' values for some forecast ",
        "cells in 'newdata'."
      ),
      i = paste0("Supply 'cap' for every (time, series) cell.")
    )))
  }
  cap_mat <- transform_pw_cap_to_link(cap_mat, family)
  cap_mat
}


# Internal: apply the observation family's link function to a
# response-scale cap matrix. Brms / mvgam families with an
# identity link pass through unchanged; log / logit / probit
# transform as expected. Errors if the result has any
# non-finite cells (e.g. user supplied `cap = 0` to a log-link
# family).
#'@noRd
transform_pw_cap_to_link <- function(cap_mat, family) {
  if (is.null(family) || is.null(family$linkfun)) return(cap_mat)
  out <- family$linkfun(cap_mat)
  if (any(!is.finite(out))) {
    stop(insight::format_error(c(
      paste0(
        "PW logistic: cap values are not finite after applying ",
        "the '", family$link %||% "<unknown>",
        "' link transform."
      ),
      x = paste0(
        "Check that all 'cap' values are valid on the response ",
        "scale (e.g. strictly positive for a log link)."
      )
    )))
  }
  out
}


# Internal: pad or trim a [n_rows, n_series] grid to exactly
# `target_rows`. Pads short grids by repeating the final row;
# trims long ones from the head. Only kicks in when per-series
# horizons differ from `target_rows`; callers slice each series
# back to its own horizon afterwards so the padding never
# pollutes returned forecasts.
#
# Note: for ARMA models with a very short training series
# (n_time < max_lag) the padding repeats the final available
# `mu_trend` row, while `assemble_innovations` zeros the
# corresponding MA seed entries. This produces a small
# asymmetry at the first forecast step. Genuine short-series
# fits are rare in practice; the asymmetry vanishes as
# n_time grows past max_lag.
#'@noRd
pad_or_trim_rows <- function(grid, target_rows) {
  if (nrow(grid) == target_rows) return(grid)
  if (nrow(grid) == 0L) {
    return(matrix(0, nrow = target_rows, ncol = ncol(grid)))
  }
  if (nrow(grid) > target_rows) {
    return(grid[seq.int(nrow(grid) - target_rows + 1L,
                          nrow(grid)), , drop = FALSE])
  }
  out <- matrix(0, nrow = target_rows, ncol = ncol(grid))
  out[seq_len(nrow(grid)), ] <- grid
  last_row <- grid[nrow(grid), , drop = FALSE]
  for (k in (nrow(grid) + 1L):target_rows) {
    out[k, ] <- last_row
  }
  out
}


# Internal: project a `[ndraws_use, nobs_fc]` matrix (obs_struct
# row order) into a per-series list of `[ndraws_use, h_s]`
# matrices, where `h_s` is the per-series forecast horizon. Each
# (raw_time, series) cell is looked up directly from obs_struct.
#
# `obs_struct$time` values are positional within the unique
# time grid of the forecast newdata; the raw time labels live
# on `names(obs_struct$time)` per `ensure_mvgam_variables`.
# Compare raw times here so the per-series slice picks the
# right cell when the user passes future times like 41:45.
#'@noRd
slice_per_series <- function(mat, fc_grid, obs_struct,
                               ndraws_use, series_levels) {
  raw_times <- as.numeric(names(obs_struct$time))
  out <- vector("list", length(series_levels))
  names(out) <- series_levels
  for (s in seq_along(series_levels)) {
    lv <- series_levels[s]
    ts <- fc_grid$times[[lv]]
    if (length(ts) == 0L) {
      out[[s]] <- matrix(NA_real_, nrow = ndraws_use, ncol = 0L)
      next
    }
    sm <- matrix(NA_real_, nrow = ndraws_use, ncol = length(ts))
    for (k in seq_along(ts)) {
      cell_j <- which(raw_times == ts[k] &
                         obs_struct$series_int == s)
      if (length(cell_j) == 0L) next
      sm[, k] <- mat[, cell_j[1L]]
    }
    out[[s]] <- sm
  }
  out
}


# Internal: batched observation-family sampling. Calls
# sample_from_family once on the `[ndraws_use, nobs_fc]` mean
# matrix; draws dpars / trials / truncation bounds with the
# matching `draw_ids` so they line up element-wise.
#'@noRd
sample_family_batched <- function(object, mu, fc_data, ndraws_use,
                                    draw_idx, family = NULL,
                                    resp = NULL) {
  family <- family %||% object$family
  # `get_family_dpars` only matches lowercase keys; R's `Gamma()`
  # constructor stores `family$family = "Gamma"` and brms's mvbf
  # normalises to `"gamma"` -- both must resolve to the `shape`
  # dpar lookup downstream.
  family_name <- tolower(family$family)
  nobs <- ncol(mu)
  dpar_names <- get_family_dpars(family_name)
  dpars <- extract_dpars_from_stanfit(
    stanfit = object$fit,
    dpar_names = dpar_names,
    ndraws = ndraws_use,
    nobs = nobs,
    draw_ids = draw_idx,
    resp = resp
  )
  trials <- extract_trials_for_family(object, family, fc_data)
  trunc_bounds <- extract_truncation_bounds(object, nobs)

  ordinal_families <- c("cumulative", "sratio", "cratio", "acat")
  if (family_name %in% ordinal_families) {
    dpars$thres <- extract_ordinal_thresholds(object,
                                                ndraws = ndraws_use)
    dpars$disc <- extract_ordinal_disc(object,
                                         ndraws = ndraws_use,
                                         nobs = nobs)
  }

  samples <- sample_from_family(
    family_name = family_name,
    ndraws = ndraws_use,
    epred = mu,
    sigma = dpars$sigma,
    phi = dpars$phi,
    shape = dpars$shape,
    nu = dpars$nu,
    trials = trials,
    hu = dpars$hu,
    zi = dpars$zi,
    zoi = dpars$zoi,
    coi = dpars$coi,
    alpha = dpars$alpha,
    ndt = dpars$ndt,
    xi = dpars$xi,
    quantile = dpars$quantile,
    kappa = dpars$kappa,
    beta = dpars$beta,
    bs = dpars$bs,
    bias = dpars$bias,
    disc = dpars$disc,
    thres = dpars$thres,
    lb = trunc_bounds$lb,
    ub = trunc_bounds$ub
  )
  matrix(samples, nrow = ndraws_use, ncol = nobs, byrow = FALSE)
}


# Internal: family-specific dpar draws for `type = "link"`
# output. Picks the chosen draws from the precomputed
# `draws_mat` so the returned `family_pars` slot lines up with
# the `[ndraws_use, ...]` rows in `hindcasts` / `forecasts`.
#'@noRd
extract_family_pars_for_draws <- function(object, draws_mat,
                                            draw_idx, resp = NULL) {
  # For multi-response (mvbrmsformula) fits, `object$family` is the
  # gaussian placeholder; the actual per-response family lives on
  # `object$formula$forms[[resp]]$family`. Honour the per-response
  # family when `resp` is supplied so we look up the correct dpar
  # set (e.g. `shape` for Gamma rather than `sigma` for gaussian).
  # `get_family_dpars` only matches lowercase keys, so case-fold;
  # R's `Gamma()` constructor stores `family$family = "Gamma"` but
  # brms's mvbf normalises to `"gamma"`, and both should resolve.
  fam_name <- tolower(resolve_resp_family(object, resp))
  dpar_names <- get_family_dpars(fam_name)
  if (length(dpar_names) == 0L) return(list())
  # For multivariate (mvbind / mvbrmsformula) fits, brms emits
  # one parameter per response with the response name suffixed
  # (e.g. `sigma_y1`, `sigma_y2`). Caller passes `resp = "<r>"`
  # to scope extraction to that response's columns; without the
  # suffix the base `^<nm>(\\[|$)` pattern never matches and the
  # extractor returns an empty list.
  out <- list()
  for (nm in dpar_names) {
    pat <- if (!is.null(resp) && nzchar(resp)) {
      paste0("^", nm, "_", resp, "(\\[|$)")
    } else {
      paste0("^", nm, "(\\[|$)")
    }
    cols <- grep(pat, colnames(draws_mat), value = TRUE)
    if (length(cols) == 0L) next
    out[[nm]] <- draws_mat[draw_idx, cols, drop = FALSE]
  }
  out
}
