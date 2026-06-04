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
#'   the returned object contains hindcasts only.
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
#' @seealso [mvgam_forecast-class][mvgam], [hindcast.mvgam]
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
                            obs_uncertainty = TRUE) {
  checkmate::assert_class(object, "mvgam")
  type <- match.arg(type)
  checkmate::assert_int(ndraws, lower = 1L, null.ok = TRUE)
  checkmate::assert_flag(b_uncertainty)
  checkmate::assert_flag(trend_uncertainty)
  checkmate::assert_flag(obs_uncertainty)

  trend_specs <- object$mv_spec$trend_specs
  if (is.null(trend_specs)) {
    stop(insight::format_error(c(
      "Fit has no trend specification; cannot forecast.",
      i = paste0("Refit with a trend constructor such as ",
                 "'RW()' or 'AR(p = 1)'.")
    )))
  }
  # `trend_specs` is either a single `mvgam_trend` (univariate
  # response) or a list of them (multivariate). Forecasting for
  # multivariate response models is pending; pick the first
  # trend spec when present and let the trend-type guard below
  # reject anything we don't yet support.
  trend_model <- if (is_multivariate_trend_specs(trend_specs)) {
    trend_specs[[1L]]
  } else {
    trend_specs
  }
  meta <- get_enriched_trend_metadata(object)
  trend_type <- meta$trend_type
  if (trend_type %in% c("VAR", "CAR", "PW")) {
    stop(insight::format_error(c(
      paste0(
        "Forecasting for '", trend_type,
        "' trends is not yet implemented in 'forecast.mvgam'."
      ),
      i = paste0(
        "Univariate RW / AR / ARMA / ZMVN are supported; ",
        "VAR / CAR / PW are pending."
      )
    )))
  }

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
                 ", total draws = ", total_draws, ".")
    )))
  }
  draw_idx <- if (ndraws == total_draws) {
    seq_len(total_draws)
  } else {
    sort(sample.int(total_draws, ndraws))
  }

  training <- build_training_arms(object, series_levels)
  fc_grid <- resolve_forecast_grid(object, newdata, training,
                                     series_levels)

  hindcasts <- build_hindcast_arms(object, training, type,
                                     draw_idx, obs_uncertainty)

  forecasts <- if (is.null(fc_grid)) {
    NULL
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
      series_levels = series_levels
    )
  }

  family_pars <- if (type == "link") {
    extract_family_pars_for_draws(object, draws_mat, draw_idx)
  } else {
    NULL
  }

  structure(
    list(
      call = object$call,
      trend_call = object$trend_call,
      family = object$family$family,
      family_pars = family_pars,
      trend_model = trend_type,
      drift = isTRUE(trend_model$drift),
      use_lv = FALSE,
      fit_engine = object$backend %||% "stan",
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
    d <- object$obs_data %||% object$data
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
build_training_arms <- function(object, series_levels) {
  d <- object$obs_data %||% object$data
  meta_vars <- object$trend_metadata$variables %||%
    list(time_var = "time", series_var = "series")
  time_var <- meta_vars$time_var
  series_var <- meta_vars$series_var
  resp <- object$mv_spec$response_names %||%
    as.character(object$formula[[2L]])[1L]

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
#'@noRd
build_hindcast_arms <- function(object, training, type, draw_idx,
                                  obs_uncertainty) {
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
    out[[s]] <- hindcast_one_series(object, sub, type,
                                      draw_idx, obs_uncertainty)
  }
  out
}


# Internal: dispatch on `type` for a single series's hindcast.
# `trend` / `link` reuse extract_component_linpred; `expected`
# and `response` route through posterior_epred / posterior_predict
# so the family / dpar / trials wiring stays single-sourced.
#'@noRd
hindcast_one_series <- function(object, sub_data, type, draw_idx,
                                  obs_uncertainty) {
  full <- switch(
    type,
    "trend" = extract_component_linpred(
      mvgam_fit = object, newdata = sub_data,
      component = "trend", incl_latent_state = TRUE
    ),
    "link" = extract_component_linpred(
      mvgam_fit = object, newdata = sub_data,
      component = "obs"
    ) + extract_component_linpred(
      mvgam_fit = object, newdata = sub_data,
      component = "trend", incl_latent_state = TRUE
    ),
    "expected" = posterior_epred(object, newdata = sub_data,
                                  ndraws = NULL),
    "response" = if (isTRUE(obs_uncertainty)) {
      posterior_predict(object, newdata = sub_data,
                          ndraws = NULL)
    } else {
      posterior_epred(object, newdata = sub_data,
                        ndraws = NULL)
    }
  )
  if (is.list(full) && !is.matrix(full)) {
    stop(insight::format_error(
      "Multivariate response hindcasts are not yet supported."
    ))
  }
  full[draw_idx, , drop = FALSE]
}


# ----- Forecast arm -----------------------------------------------

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
                                  series_levels) {
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
      component = "obs"
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
      obs_struct_fc = obs_struct_fc
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
  mu <- object$family$linkinv(eta_full)
  if (type == "expected" || isTRUE(!obs_uncertainty)) {
    return(slice_per_series(mu, fc_grid, obs_struct_fc,
                              ndraws_use, series_levels))
  }
  resp_mat <- sample_family_batched(object, mu, fc_grid$data,
                                      ndraws_use, draw_idx)
  slice_per_series(resp_mat, fc_grid, obs_struct_fc,
                     ndraws_use, series_levels)
}


# Internal: one-draw propagation pipeline. Returns a `[h_max,
# n_series]` link-scale trajectory.
#'@noRd
propagate_one_draw <- function(object, trend_model, meta, training,
                                 fc_grid, draws_mat, d_state, d_lin,
                                 h_max, n_series, tail_data,
                                 trend_lp_tail, trend_lp_fc,
                                 obs_struct_tail, obs_struct_fc) {
  ls_d <- extract_last_state(object, d_state,
                                draws_mat = draws_mat)
  max_lag <- as.integer(meta$max_lag %||% 0L)

  lp_history <- if (max_lag == 0L) {
    matrix(0, nrow = 0L, ncol = n_series)
  } else if (is.null(trend_lp_tail)) {
    matrix(0, nrow = max_lag, ncol = n_series)
  } else {
    grid <- reshape_linpred_to_grid(
      trend_lp_tail[d_lin, ], obs_struct_tail
    )
    pad_or_trim_rows(grid, max_lag)
  }
  lp_forecast <- if (is.null(trend_lp_fc)) {
    matrix(0, nrow = h_max, ncol = n_series)
  } else {
    grid <- reshape_linpred_to_grid(
      trend_lp_fc[d_lin, ], obs_struct_fc
    )
    pad_or_trim_rows(grid, h_max)
  }
  linpreds_combined <- rbind(lp_history, lp_forecast)

  propagate_trend(
    trend_model = trend_model,
    params = ls_d$params,
    h = h_max,
    n_series = n_series,
    last_state = ls_d$last_state,
    linpreds = linpreds_combined
  )
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
                                    draw_idx) {
  family <- object$family
  family_name <- family$family
  nobs <- ncol(mu)
  dpar_names <- get_family_dpars(family_name)
  dpars <- extract_dpars_from_stanfit(
    stanfit = object$fit,
    dpar_names = dpar_names,
    ndraws = ndraws_use,
    nobs = nobs,
    draw_ids = draw_idx
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
                                            draw_idx) {
  dpar_names <- get_family_dpars(object$family$family)
  if (length(dpar_names) == 0L) return(list())
  out <- list()
  for (nm in dpar_names) {
    cols <- grep(paste0("^", nm, "(\\[|$)"),
                  colnames(draws_mat), value = TRUE)
    if (length(cols) == 0L) next
    out[[nm]] <- draws_mat[draw_idx, cols, drop = FALSE]
  }
  out
}
