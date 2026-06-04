# Single dispatch surface for forward-in-time trend simulation
# AND (future) forecast extrapolation. Both operations share the
# same C++ kernels and the same parameter contract; the only
# difference is whether `last_state` comes from a burn-in (sim) or
# from posterior draws (forecast).
#
# Used by sim_mvgam (Phase D of the sim_mvgam rewrite) and intended
# for use by the eventual forecast.mvgam once the forecasting
# surface comes online.


#' Propagate an `mvgam` trend forward in time
#'
#' Dispatches on the supplied `mvgam_trend` constructor to the
#' appropriate C++ kernel ([`trend_arma_recursC()`] for
#' AR/RW/ARMA/VAR/VARMA, [`car1_recursC()`] for CAR(1)) or pure-R
#' path (ZMVN, None). Draws innovations with the appropriate
#' covariance structure, applies any burn-in needed when no
#' explicit `last_state` is supplied, and returns the forward
#' trajectory.
#'
#' @param trend_model An `mvgam_trend` object built by one of
#'   [`RW()`], [`AR()`], [`VAR()`], [`CAR()`], [`ZMVN()`], or
#'   the string `"None"`.
#' @param params A named list of dynamics parameters. See *Details*
#'   for the per-trend-type contract.
#' @param h Integer forecast horizon.
#' @param n_series Integer number of series (dimensions).
#' @param last_state Either `NULL` (default; the dispatcher
#'   generates a 200-step burn-in from zero) or a list with
#'   elements `trends`, `errors`, `linpreds`, `time` as documented
#'   in the kernel header. Forecast callers must populate
#'   `trends` with posterior draws of the latent trend state,
#'   NOT observed responses.
#' @param linpreds Optional `[h + max_lag, n_series]` matrix of
#'   obs-side linear-predictor offsets aligned with the trend
#'   index. `NULL` defaults to the zero matrix (correct for
#'   simulation).
#' @param time Required only for `CAR()` trends; length-`h`
#'   numeric vector of time gaps for the forecast steps.
#'
#' @details
#' The `params` list expects different fields per trend type:
#'
#'   * **None**: no fields needed. Returns a zero matrix.
#'   * **ZMVN**: `Sigma` (an `[n_series, n_series]` covariance
#'     matrix) OR `sigma` (a length-`n_series` SD vector for a
#'     diagonal covariance). One MVN(0, Σ) draw is returned per
#'     timestep; there is no temporal recursion.
#'   * **RW** (treated as AR(p=1) with coefficient = 1):
#'     `sigma` (length `n_series` or scalar broadcast),
#'     optional `drift` (length `n_series`), optional `theta`
#'     (MA(1) coefficient) when `trend_model$ma`, optional
#'     `Sigma` (innovation covariance) when `trend_model$cor`.
#'   * **AR(p)**: `ar` (length `length(p)` scalar shared across
#'     series, or `[length(p), n_series]` matrix), `sigma`,
#'     optional `theta`, optional `Sigma`, optional `drift`.
#'   * **VAR(p)**: `A` (`[n_series, n_series, length(p)]` cube
#'     of AR matrices), `Sigma` (required; innovation
#'     covariance), optional `theta_cube`
#'     (`[n_series, n_series, q]` MA matrices) when
#'     `trend_model$ma`, optional `drift`.
#'   * **CAR(1)**: `phi` (length `n_series` or scalar, must lie
#'     in (0, 1) for stationary CAR), `sigma` (length
#'     `n_series` or scalar, must be positive). `time` argument
#'     is required.
#'
#' Defaults that apply if a field is missing: `sigma = 1`,
#' `ar = 0.7` (AR/RW), `drift = 0`, `theta = 0`,
#' `phi = 0.7` (CAR), `Sigma = diag(sigma^2)`.
#'
#' @return Numeric matrix of shape `[h, n_series]`.
#'
#' @author Nicholas J Clark
#'
#' @seealso [`RW()`], [`AR()`], [`VAR()`], [`CAR()`], [`ZMVN()`].
#'
#' @noRd
propagate_trend <- function(trend_model,
                             params = list(),
                             h,
                             n_series = 1L,
                             last_state = NULL,
                             linpreds = NULL,
                             time = NULL) {
  if (is.character(trend_model) && identical(trend_model, "None")) {
    return(matrix(0, nrow = h, ncol = n_series))
  }
  checkmate::assert_class(trend_model, "mvgam_trend")
  checkmate::assert_list(params)
  checkmate::assert_int(h, lower = 0L)
  checkmate::assert_int(n_series, lower = 1L)
  checkmate::assert_list(last_state, null.ok = TRUE)

  trend_type <- trend_model$trend

  switch(
    trend_type,
    "ZMVN" = propagate_zmvn(params, h, n_series),
    "CAR" = propagate_car(params, h, n_series, last_state, time),
    "RW" = propagate_arma(trend_model, params, h, n_series,
                            last_state, linpreds),
    "AR" = propagate_arma(trend_model, params, h, n_series,
                            last_state, linpreds),
    "VAR" = propagate_arma(trend_model, params, h, n_series,
                              last_state, linpreds),
    stop(insight::format_error(c(
      "Trend type not supported by 'propagate_trend'.",
      x = paste0("Got: '", trend_type, "'."),
      i = paste0(
        "Supported: 'None', 'RW', 'AR', 'VAR', 'CAR', 'ZMVN'."
      )
    )))
  )
}


# ------------------------------------------------------------------
# ZMVN: no temporal recursion; one MVN draw per timestep.
# ------------------------------------------------------------------
#'@noRd
propagate_zmvn <- function(params, h, n_series) {
  Sigma <- params$Sigma
  if (is.null(Sigma)) {
    sigma_vec <- params$sigma %||% rep(1, n_series)
    if (length(sigma_vec) == 1L) {
      sigma_vec <- rep(sigma_vec, n_series)
    }
    Sigma <- diag(sigma_vec^2, nrow = n_series)
  }
  rmvn(h, mu = rep(0, n_series), Sigma = Sigma)
}


# ------------------------------------------------------------------
# CAR(1): continuous-time AR, one kernel call per (series, gap).
# ------------------------------------------------------------------
#'@noRd
propagate_car <- function(params, h, n_series, last_state, time) {
  if (is.null(time)) {
    stop(insight::format_error(c(
      "'time' is required for CAR(1) propagation.",
      i = paste0(
        "Pass a length-h numeric vector of time gaps to ",
        "'propagate_trend(..., time = ...)'."
      )
    )))
  }
  checkmate::assert_numeric(time, len = h, lower = 0)
  phi <- params$phi %||% 0.7
  sigma <- params$sigma %||% 1
  if (length(phi) == 1L) phi <- rep(phi, n_series)
  if (length(sigma) == 1L) sigma <- rep(sigma, n_series)
  last_trend <- if (!is.null(last_state) &&
                     !is.null(last_state$trends)) {
    as.numeric(last_state$trends[nrow(last_state$trends), ])
  } else {
    rep(0, n_series)
  }
  innovations <- matrix(
    stats::rnorm(h * n_series), nrow = h, ncol = n_series
  )
  car1_recursC(
    phi = phi, sigma = sigma, time_dis = as.numeric(time),
    innovations = innovations, last_trend = last_trend, h = h
  )
}


# ------------------------------------------------------------------
# AR / RW / ARMA / VAR / VARMA: route through the general kernel.
# Builds the cube + lag vectors per the trend's $p, $ma, $cor flags
# and the supplied params; performs burn-in init when last_state is
# NULL; calls trend_arma_recursC.
# ------------------------------------------------------------------
#'@noRd
propagate_arma <- function(trend_model, params, h, n_series,
                            last_state, linpreds) {
  trend_type <- trend_model$trend
  # mvgam's AR()/VAR() constructors accept `p` as either a scalar
  # (consecutive lags 1..p) or a vector (sparse lag indices). The
  # kernel takes an integer vector of active lags, so expand the
  # scalar form here.
  p_arg <- trend_model$p %||% 1L
  ar_lags <- if (identical(trend_type, "RW")) {
    1L
  } else if (length(p_arg) == 1L) {
    seq_len(as.integer(p_arg))
  } else {
    as.integer(p_arg)
  }
  has_ma <- isTRUE(trend_model$ma)
  ma_lags <- if (has_ma) 1L else integer(0L)

  m_a <- length(ar_lags)
  m_b <- length(ma_lags)
  max_ar <- max(ar_lags)
  max_ma <- if (m_b > 0L) max(ma_lags) else 0L
  max_lag <- max(max_ar, max_ma)
  total <- h + max_lag

  A_cube <- build_arma_A(trend_type, params, ar_lags, n_series)
  B_cube <- build_arma_B(has_ma, params, m_b, n_series)
  drift <- as.numeric(params$drift %||% rep(0, n_series))
  if (length(drift) == 1L) drift <- rep(drift, n_series)

  # Innovation covariance: explicit Sigma when supplied (VAR always
  # has one; AR/RW use one only with cor = TRUE), else diagonal.
  Sigma <- params$Sigma
  if (is.null(Sigma)) {
    sigma_vec <- as.numeric(params$sigma %||% rep(1, n_series))
    if (length(sigma_vec) == 1L) {
      sigma_vec <- rep(sigma_vec, n_series)
    }
    Sigma <- diag(sigma_vec^2, nrow = n_series)
  }

  linpreds_mat <- if (is.null(linpreds)) {
    matrix(0, nrow = total, ncol = n_series)
  } else {
    checkmate::assert_matrix(
      linpreds, nrows = total, ncols = n_series
    )
    linpreds
  }

  # Initial state via burn-in if not supplied. Burn-in runs the
  # kernel for 200 extra steps from zero so the trailing max_lag
  # rows are a draw from the stationary distribution.
  if (is.null(last_state)) {
    burn_in <- 200L
    last_state <- run_burnin(
      ar_lags, ma_lags, drift, A_cube, B_cube, Sigma,
      n_series, burn_in, max_lag
    )
  } else {
    validate_last_state(last_state, max_ar, max_ma, n_series)
  }

  innovations <- assemble_innovations(
    last_state$errors, h, n_series, Sigma, max_lag
  )

  last_trends <- if (max_ar > 0L) {
    tail(last_state$trends, max_ar)
  } else {
    matrix(0, nrow = 0L, ncol = n_series)
  }

  trend_arma_recursC(
    ar_lags = as.integer(ar_lags),
    ma_lags = as.integer(ma_lags),
    drift = drift,
    A = A_cube,
    B = B_cube,
    innovations = innovations,
    linpreds = linpreds_mat,
    last_trends = last_trends,
    h = as.integer(h)
  )
}


# Build the AR coefficient cube. For univariate AR / RW each slice
# is a diagonal matrix; for VAR each slice is a full matrix from
# params$A.
#'@noRd
build_arma_A <- function(trend_type, params, ar_lags, n_series) {
  m_a <- length(ar_lags)
  if (identical(trend_type, "VAR")) {
    A_cube <- params$A
    if (is.null(A_cube)) {
      stop(insight::format_error(c(
        "VAR propagation requires 'A' in params.",
        i = paste0(
          "'A' must be an array of shape ",
          "[n_series, n_series, length(p)]."
        )
      )))
    }
    checkmate::assert_array(A_cube, d = 3L)
    if (any(dim(A_cube) != c(n_series, n_series, m_a))) {
      stop(insight::format_error(c(
        "'A' has the wrong shape.",
        x = paste0(
          "Got: [", paste(dim(A_cube), collapse = ", "),
          "]; expected: [", n_series, ", ", n_series, ", ",
          m_a, "]."
        )
      )))
    }
    return(A_cube)
  }
  # AR / RW: build diagonal A from params$ar (or default 0.7 / 1.0).
  ar_default <- if (identical(trend_type, "RW")) 1.0 else 0.7
  ar_coefs <- params$ar %||% rep(ar_default, m_a)
  if (is.matrix(ar_coefs)) {
    if (any(dim(ar_coefs) != c(m_a, n_series))) {
      stop(insight::format_error(c(
        "'ar' has the wrong shape.",
        x = paste0(
          "Got: [", paste(dim(ar_coefs), collapse = ", "),
          "]; expected: [", m_a, ", ", n_series, "]."
        )
      )))
    }
  } else {
    if (length(ar_coefs) != m_a) {
      stop(insight::format_error(c(
        "'ar' has the wrong length.",
        x = paste0(
          "Got length: ", length(ar_coefs),
          "; expected: ", m_a, "."
        )
      )))
    }
    ar_coefs <- matrix(
      rep(ar_coefs, each = n_series), nrow = m_a, ncol = n_series,
      byrow = TRUE
    )
  }
  A_cube <- array(0, dim = c(n_series, n_series, m_a))
  for (k in seq_len(m_a)) {
    slice <- matrix(0, n_series, n_series)
    diag(slice) <- ar_coefs[k, ]
    A_cube[, , k] <- slice
  }
  A_cube
}


# Build the MA coefficient cube. Empty if no MA terms.
#'@noRd
build_arma_B <- function(has_ma, params, m_b, n_series) {
  if (!has_ma || m_b == 0L) {
    return(array(0, dim = c(n_series, n_series, 0L)))
  }
  B_cube <- params$theta_cube
  if (!is.null(B_cube)) {
    checkmate::assert_array(B_cube, d = 3L)
    if (any(dim(B_cube) != c(n_series, n_series, m_b))) {
      stop(insight::format_error(c(
        "'theta_cube' has the wrong shape.",
        x = paste0(
          "Got: [", paste(dim(B_cube), collapse = ", "),
          "]; expected: [", n_series, ", ", n_series, ", ",
          m_b, "]."
        )
      )))
    }
    return(B_cube)
  }
  # Default scalar theta per series (or shared).
  theta <- params$theta %||% rep(0, n_series)
  if (length(theta) == 1L) theta <- rep(theta, n_series)
  B_cube <- array(0, dim = c(n_series, n_series, m_b))
  for (j in seq_len(m_b)) {
    slice <- matrix(0, n_series, n_series)
    diag(slice) <- theta
    B_cube[, , j] <- slice
  }
  B_cube
}


# Burn in 200 zero-seed steps to draw a stationary initial state.
# Returns a `last_state` list compatible with the propagate_arma
# contract (trends, errors, linpreds, time).
#'@noRd
run_burnin <- function(ar_lags, ma_lags, drift, A_cube, B_cube,
                        Sigma, n_series, burn_in, max_lag) {
  total <- burn_in + max_lag
  innovations <- rmvn(total, mu = rep(0, n_series), Sigma = Sigma)
  bi_last_trends <- if (length(ar_lags) > 0L) {
    matrix(0, nrow = max(ar_lags), ncol = n_series)
  } else {
    matrix(0, nrow = 0L, ncol = n_series)
  }
  bi_states <- trend_arma_recursC(
    ar_lags = as.integer(ar_lags),
    ma_lags = as.integer(ma_lags),
    drift = drift,
    A = A_cube,
    B = B_cube,
    innovations = innovations,
    linpreds = matrix(0, nrow = total, ncol = n_series),
    last_trends = bi_last_trends,
    h = as.integer(burn_in)
  )
  list(
    trends = utils::tail(bi_states, max_lag),
    errors = utils::tail(innovations, max_lag),
    linpreds = matrix(0, nrow = max_lag, ncol = n_series),
    time = NULL
  )
}


# Validate a caller-supplied last_state for arma propagation.
#'@noRd
validate_last_state <- function(last_state, max_ar, max_ma,
                                  n_series) {
  if (max_ar > 0L) {
    checkmate::assert_matrix(
      last_state$trends, nrows = max_ar, ncols = n_series
    )
  }
  if (max_ma > 0L) {
    checkmate::assert_matrix(
      last_state$errors, nrows = max_ma, ncols = n_series
    )
  }
}


# Build the full innovations matrix used by trend_arma_recursC.
# Rows 0..max_lag-1 hold MA history (from last_state$errors when
# supplied, zero otherwise); rows max_lag..(h+max_lag-1) hold the
# new forecast-step innovations drawn from MVN(0, Sigma).
#'@noRd
assemble_innovations <- function(last_errors, h, n_series, Sigma,
                                   max_lag) {
  total <- h + max_lag
  out <- matrix(0, nrow = total, ncol = n_series)
  if (max_lag > 0L) {
    if (!is.null(last_errors) && nrow(last_errors) >= max_lag) {
      out[seq_len(max_lag), ] <-
        last_errors[seq_len(max_lag), , drop = FALSE]
    }
  }
  if (h > 0L) {
    out[(max_lag + 1L):total, ] <- rmvn(
      h, mu = rep(0, n_series), Sigma = Sigma
    )
  }
  out
}


# Internal: mgcv-based multivariate-normal sampler. Same recipe as
# the master-branch helper (R/mvgam_setup.R:182): factor the
# covariance via mgcv::mroot and apply to IID normals.
#'@noRd
rmvn <- function(n, mu, Sigma) {
  L <- mgcv::mroot(Sigma)
  m <- ncol(L)
  t(mu + L %*% matrix(stats::rnorm(m * n), m, n))
}


# ----------------------------------------------------------------
# Fit-time trend metadata enrichment helpers
# ----------------------------------------------------------------
# These derive the kernel-relevant fields the forecasting surface
# (extract_last_state, propagate_trend) needs at every per-draw
# call. Computing them once at fit time and storing them on
# `mvgam_fit$trend_metadata` avoids re-parsing the trend
# constructor on every forecast call.


# Internal: enrich trend_metadata with kernel-relevant extras
# derived from the parsed trend constructor.
#
# Adds these fields to `trend_metadata` when a trend spec is
# present:
#   * `trend_type`  - character; one of "RW", "AR", "VAR", "CAR",
#                     "ZMVN", "PW".
#   * `ar_lags`     - integer vector of active AR lag indices.
#                     `1` for RW / CAR, `seq_len(p)` for
#                     `AR(p = k)` / `VAR(p = k)`, `as.integer(p)`
#                     for sparse-lag `AR(p = c(...))`, empty for
#                     ZMVN / PW.
#   * `ma_lags`     - integer vector of active MA lag indices.
#                     `1L` when `spec$ma == TRUE` (only q = 1 is
#                     supported on this branch), empty otherwise.
#   * `max_lag`     - cached max of ar_lags and ma_lags.
#   * `has_cor`     - logical; `spec$cor`.
#   * `n_lv`        - integer factor-model dimension (or NULL).
#
# Returns the (possibly enriched) `trend_metadata` list. Returns
# NULL if `trend_metadata` is NULL on entry (no trend present in
# the fit).
#'@noRd
enrich_trend_metadata <- function(trend_metadata, trend_specs) {
  if (is.null(trend_metadata)) return(NULL)
  if (is.null(trend_specs)) return(trend_metadata)

  spec <- if (is_multivariate_trend_specs(trend_specs)) {
    trend_specs[[1L]]
  } else {
    trend_specs
  }
  if (is.null(spec) || is.null(spec$trend)) return(trend_metadata)

  trend_metadata$trend_type <- spec$trend
  trend_metadata$ar_lags <- derive_ar_lags(spec)
  trend_metadata$ma_lags <- derive_ma_lags(spec)
  trend_metadata$max_lag <- max(
    c(0L, trend_metadata$ar_lags, trend_metadata$ma_lags)
  )
  trend_metadata$has_cor <- isTRUE(spec$cor)
  trend_metadata$n_lv <- spec$n_lv
  trend_metadata
}


# Internal: derive the active AR lag set from a parsed trend
# constructor. See `enrich_trend_metadata` for the contract.
#'@noRd
derive_ar_lags <- function(spec) {
  switch(
    spec$trend,
    "RW" = 1L,
    "AR" = ,
    "VAR" = resolve_active_lags(spec$p),
    "CAR" = 1L,
    "ZMVN" = integer(0),
    "PW" = integer(0),
    integer(0)
  )
}


# Internal: turn an AR or VAR `p` argument into the integer
# vector of active lag indices. Used by both the trend metadata
# enrichment (above) and the Stan generators (R/stan_assembly.R).
#
# Scalar `p` (e.g. `2`) expands to consecutive lags 1..p:
# `c(1L, 2L)`. Vector `p` (e.g. `c(2, 4)`) is treated as the
# sparse lag set: `c(2L, 4L)`. NULL or zero-length `p` returns
# `integer(0)` (trend has no AR dynamics).
#
# An optional `override` argument lets the Stan generators pass
# in a pre-parsed `trend_specs$ar_lags` when it has already been
# resolved upstream; this keeps both callers using the same
# resolution rule even when one of them caches the result.
#'@noRd
resolve_active_lags <- function(p, override = NULL) {
  if (!is.null(override)) return(as.integer(override))
  if (is.null(p) || length(p) == 0L) return(integer(0))
  if (length(p) == 1L) return(seq_len(as.integer(p)))
  as.integer(p)
}


# Internal: derive the active MA lag set. On this branch only
# q = 1 is supported across all trend types that allow MA, so the
# result is either `c(1L)` or empty.
#'@noRd
derive_ma_lags <- function(spec) {
  if (isTRUE(spec$ma)) 1L else integer(0)
}


# Note: `%||%` is defined package-wide at R/priors.R:1298.
