# One dispatch for forward-in-time trend simulation and forecasting.
# `sim_mvgam()` starts from a burn-in and `forecast.mvgam()` from a
# posterior draw's fitted state. Both advance by the same recursion.


#' Propagate an `mvgam` trend forward in time
#'
#' Dispatches on the supplied `mvgam_trend` constructor to its
#' propagator. The C++ kernel `trend_arma_recursC()` advances RW, AR
#' and VAR, with or without a moving-average term. R code advances CAR,
#' LLT, PW and ZMVN. Each propagator draws innovations with the trend's
#' covariance structure. Without a `last_state`, a burn-in supplies the
#' start.
#'
#' @param trend_model An `mvgam_trend` object built by one of the
#'   trend constructors, or the string `"None"`.
#' @param params A named list of dynamics parameters. See *Details*
#'   for the per-trend-type contract.
#' @param h Integer forecast horizon.
#' @param n_series Integer number of series (dimensions).
#' @param last_state Either `NULL` (default; the dispatcher
#'   generates a 200-step burn-in from zero) or a list with
#'   elements `trends` and `errors` as `extract_last_state()`
#'   documents them. Forecast callers fill `trends` with posterior
#'   draws of the latent trend state, centred on the trend linear
#'   predictor. The recursion advances a zero-mean process.
#' @param time Required only for `CAR()` trends; length-`h`
#'   numeric vector of time gaps for the forecast steps.
#' @param fc_times,training_times,cap,changepoint_range Required only
#'   for `PW()` trends, from `forecast_context_pw()`.
#'
#' @details
#' The `params` list expects different fields per trend type:
#'
#'   * **None**: no fields needed. Returns a zero matrix.
#'   * **ZMVN**: `Sigma` (an `[n_series, n_series]` covariance
#'     matrix) or `sigma` (a length-`n_series` SD vector for a
#'     diagonal covariance). Each timestep takes one independent
#'     multivariate normal draw.
#'   * **RW** (an AR(1) with coefficient 1): `sigma`, optional
#'     `drift` (length `n_series`), optional `theta` (MA(1)
#'     coefficient) when `trend_model$ma`, optional `Sigma`
#'     (innovation covariance) when `trend_model$cor`.
#'   * **AR(p)**: `ar` (length `length(p)` scalar shared across
#'     series, or `[length(p), n_series]` matrix), `sigma`,
#'     optional `theta`, optional `Sigma`, optional `drift`.
#'   * **VAR(p)**: `A` (`[n_series, n_series, length(p)]` cube
#'     of AR matrices), `Sigma` (required; innovation
#'     covariance), optional `theta_cube`
#'     (`[n_series, n_series, q]` MA matrices) when
#'     `trend_model$ma`, optional `drift`.
#'   * **CAR(1)**: `phi` (length `n_series` or scalar, inside
#'     (0, 1)), `sigma`, optional `Sigma` (innovation covariance over
#'     one unit of time). The `time` argument is required.
#'   * **LLT**: `sigma` or `Sigma` for the level shocks. Optional
#'     `sigma_slope` or `Sigma_slope` for the slope shocks.
#'     `phi_slope` under `damped = TRUE`.
#'   * **PW**: `k`, `m`, `delta` and `t_change` from the fit, with the
#'     `fc_times`, `training_times`, `cap` and `changepoint_range`
#'     arguments.
#'
#' `sigma` is a scalar or one SD per series. Every stochastic trend also
#' takes an optional `df`, the degrees of freedom of multivariate t
#' innovations. `Inf` or absent gives Gaussian innovations.
#'
#' Defaults that apply if a field is missing: `sigma = 1`,
#' `ar = 0.7` (AR), `drift = 0`, `theta = 0`,
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
                            time = NULL,
                            fc_times = NULL,
                            training_times = NULL,
                            cap = NULL,
                            changepoint_range = NULL) {
  if (is.character(trend_model) && identical(trend_model, "None")) {
    return(matrix(0, nrow = h, ncol = n_series))
  }
  checkmate::assert_class(trend_model, "mvgam_trend")
  checkmate::assert_list(params)
  checkmate::assert_int(h, lower = 0L)
  checkmate::assert_int(n_series, lower = 1L)
  checkmate::assert_list(last_state, null.ok = TRUE)

  # A forecast supplies each draw's degrees of freedom. A simulation
  # takes the value fixed on the constructor. A constructor asking for
  # an estimate gives Gaussian innovations.
  if (is.null(params$df)) {
    df <- trend_model$df %||% Inf
    params$df <- if (is.na(df)) Inf else df
  }

  # Every trend defines `propagate_<type>(trend_model, params, h,
  # n_series, last_state, context)`. `context` holds the times and
  # capacities that CAR and PW take.
  context <- list(time = time, fc_times = fc_times,
                  training_times = training_times, cap = cap,
                  changepoint_range = changepoint_range)
  trend_method(trend_model$trend, trend_method_templates[["propagate"]])(
    trend_model, params, h, n_series, last_state, context
  )
}

#' The innovation covariance a propagator draws with
#'
#' @param params The dynamics parameters
#' @param n_series Number of series
#' @return `params$Sigma`, or the diagonal of `params$sigma^2` with a
#'   scalar `sigma` given to every series. `sigma` defaults to 1.
#' @noRd
innovation_covariance <- function(params, n_series) {
  checkmate::assert_list(params)
  checkmate::assert_int(n_series, lower = 1L)
  if (!is.null(params$Sigma)) {
    return(params$Sigma)
  }
  sigma <- as.numeric(params$sigma %||% 1)
  if (length(sigma) == 1L) {
    sigma <- rep(sigma, n_series)
  }
  checkmate::assert_numeric(sigma, len = n_series, lower = 0,
                            any.missing = FALSE, .var.name = "sigma")
  diag(sigma^2, nrow = n_series)
}

#' Propagate a ZMVN trend
#'
#' Each timestep takes one independent draw. Nothing carries over
#' from one step to the next.
#'
#' @inheritParams propagate_trend
#' @param context Unused, for the shared propagator signature
#' @return Numeric matrix `[h, n_series]`
#' @noRd
propagate_zmvn <- function(trend_model, params, h, n_series, last_state,
                           context) {
  rmvn(h, mu = rep(0, n_series),
       Sigma = innovation_covariance(params, n_series),
       df = params$df %||% Inf)
}

#' Propagate a CAR trend
#'
#' One exact transition of the continuous-time process per gap. With
#' no fitted state the path starts at the law the Stan program gives
#' its first state.
#'
#' @inheritParams propagate_trend
#' @param context List holding `time`, the gap before each step
#' @return Numeric matrix `[h, n_series]`
#' @noRd
propagate_car <- function(trend_model, params, h, n_series, last_state,
                          context) {
  time <- context$time
  if (is.null(time)) {
    stop_mvgam_fault(
      "CAR(1) propagation needs the forecast time gaps.",
      "The caller passed 'time = NULL'."
    )
  }
  checkmate::assert_numeric(time, len = h, lower = 0)
  phi <- params$phi %||% 0.7
  if (length(phi) == 1L) phi <- rep(phi, n_series)
  if (any(phi <= 0 | phi >= 1)) {
    stop(insight::format_error(c(
      "CAR 'phi' must lie strictly between 0 and 1.",
      x = paste0("Got ", paste(signif(phi, 3), collapse = ", "), "."),
      x = "The damping across a gap is 'phi' raised to the gap."
    )), call. = FALSE)
  }
  df <- params$df %||% Inf
  Sigma <- innovation_covariance(params, n_series)
  checkmate::assert_matrix(Sigma, nrows = n_series, ncols = n_series,
                           any.missing = FALSE)
  gamma <- car_stationary_cov(phi, Sigma)
  last_trend <- if (!is.null(last_state$trends)) {
    as.numeric(last_state$trends[nrow(last_state$trends), ])
  } else {
    as.numeric(rmvn(1L, mu = rep(0, n_series), Sigma = gamma, df = df))
  }
  innovations <- draw_trend_innovations(h, n_series, df)
  states <- matrix(NA_real_, nrow = h, ncol = n_series)
  for (step in seq_len(h)) {
    # The floor the Stan data applies
    gap <- max(time[step], car_min_gap)
    gap_factor <- t(chol(car_gap_cov(phi, gamma, gap)))
    last_trend <- phi^gap * last_trend +
      as.numeric(gap_factor %*% innovations[step, ])
    states[step, ] <- last_trend
  }
  states
}

#' The covariance a CAR state holds at every occasion
#'
#' Series `a` and `b` settle at `Sigma[a, b] / (1 - phi_a * phi_b)`.
#'
#' @param phi Damping of each series over a unit gap
#' @param Sigma Innovation covariance over a unit gap
#' @return Covariance matrix
#' @noRd
car_stationary_cov <- function(phi, Sigma) {
  checkmate::assert_numeric(phi, lower = 0, upper = 1, any.missing = FALSE)
  checkmate::assert_matrix(Sigma, nrows = length(phi), ncols = length(phi))
  Sigma / (1 - tcrossprod(phi))
}

#' The innovation covariance of a CAR state across one gap
#'
#' A gap of one returns the unit-gap covariance `Sigma`. `expm1()` keeps
#' the digits a small gap would lose to `1 - x^gap`.
#'
#' @param phi Damping of each series over a unit gap
#' @param gamma The stationary covariance from `car_stationary_cov()`
#' @param gap Length of the gap
#' @return Covariance matrix
#' @noRd
car_gap_cov <- function(phi, gamma, gap) {
  checkmate::assert_numeric(phi, lower = 0, upper = 1, any.missing = FALSE)
  checkmate::assert_matrix(gamma, nrows = length(phi), ncols = length(phi))
  checkmate::assert_number(gap, lower = 0)
  gamma * -expm1(gap * log(tcrossprod(phi)))
}

#' Propagate a PW trend
#'
#' The Prophet piecewise trend is a deterministic function of `k`, `m`,
#' `delta` and the changepoints. `sample_horizon_changepoints()` adds
#' the changepoints of the horizon. `pw_trendC()` then evaluates the
#' trend.
#'
#' The capacity reaches here on the link scale, from
#' `extract_pw_cap_matrix()`. A log link gives a cap below one a
#' negative value.
#'
#' @inheritParams propagate_trend
#' @param context List of `fc_times`, `training_times`, `cap` and
#'   `changepoint_range`
#' @return Numeric matrix `[h, n_series]`
#' @noRd
propagate_pw <- function(trend_model, params, h, n_series, last_state,
                         context) {
  fc_times <- context$fc_times
  training_times <- context$training_times
  cap <- context$cap
  changepoint_range <- context$changepoint_range
  growth <- pw_growth(trend_model)
  if (is.null(fc_times) || length(fc_times) != h) {
    stop_shape_fault("'fc_times'", length(fc_times), h)
  }
  if (is.null(training_times) || length(training_times) == 0L) {
    stop_mvgam_fault(
      "PW propagation needs the training times.",
      "The caller passed an empty 'training_times'."
    )
  }
  if (identical(growth, "logistic")) {
    if (!is.matrix(cap) || !identical(dim(cap), c(as.integer(h),
                                                  as.integer(n_series)))) {
      stop_shape_fault("'cap'", dim(cap) %||% length(cap), c(h, n_series))
    }
    if (any(!is.finite(cap))) {
      stop_mvgam_fault(
        "PW propagation needs a finite capacity.",
        "The caller passed a 'cap' with non-finite entries."
      )
    }
  }
  # `params$delta` holds the training changes `[n_change, n_series]`
  # at the shared times `params$t_change`. `params$k` is the base rate
  # of each series. `params$m` is the logistic offset, zero for a
  # linear fit.
  t_change_train <- as.numeric(params$t_change %||% numeric(0L))
  delta_train <- params$delta %||% matrix(0, nrow = 0L, ncol = n_series)
  k_vec <- as.numeric(params$k)
  m_vec <- as.numeric(params$m)
  checkmate::assert_numeric(k_vec, len = n_series, any.missing = FALSE,
                            .var.name = "params$k")
  checkmate::assert_numeric(m_vec, len = n_series, any.missing = FALSE,
                            .var.name = "params$m")

  # The share of the history the fit spread its changepoints over,
  # `floor(n_time_observed * changepoint_range)` (Taylor & Letham 2018)
  full_span <- length(unique(training_times))
  hist_size <- if (!is.null(changepoint_range)) {
    floor(full_span * as.numeric(changepoint_range))
  } else {
    full_span
  }
  change_freq <- length(t_change_train) / max(hist_size, 1)
  fc_min <- min(fc_times)
  fc_max <- max(fc_times)

  out <- matrix(NA_real_, nrow = h, ncol = n_series)
  for (s in seq_len(n_series)) {
    sampled <- sample_horizon_changepoints(
      delta_train_s = as.numeric(delta_train[, s]),
      t_change_train = t_change_train,
      change_freq = change_freq,
      fc_min = fc_min,
      fc_max = fc_max,
      fc_horizon = fc_max - fc_min + 1
    )
    out[, s] <- as.numeric(pw_trendC(
      t = as.numeric(fc_times),
      k = k_vec[s],
      m = m_vec[s],
      delta = matrix(sampled$delta, ncol = 1L),
      t_change = sampled$t_change,
      cap = if (identical(growth, "logistic")) {
        cap[, s, drop = FALSE]
      } else {
        matrix(0, 0L, 0L)
      },
      growth_type = growth
    ))
  }
  out
}

#' Laplace draws by the inverse CDF
#'
#' `mu - sigma * sign(u) * log(1 - 2 |u|)` for `u ~ U(-1/2, 1/2)`.
#'
#' @param n Number of draws
#' @param mu Location
#' @param sigma Scale
#' @return Numeric vector of length `n`
#' @noRd
rlaplace_inverse_cdf <- function(n, mu = 0, sigma = 1) {
  checkmate::assert_count(n)
  checkmate::assert_number(mu)
  checkmate::assert_number(sigma, lower = 0)
  u <- stats::runif(n, -0.5, 0.5)
  mu - sigma * sign(u) * log1p(-2 * abs(u))
}

#' Training and horizon changepoints of one series, in time order
#'
#' The horizon's changepoint count is Poisson at `change_freq` per
#' step. Their magnitudes are Laplace on the median absolute training
#' change. Their times are uniform over the horizon. A series without
#' training changes takes none in the horizon.
#'
#' @param delta_train_s Training changes of the series
#' @param t_change_train Training changepoint times
#' @param change_freq Changepoints per step in the training history
#' @param fc_min,fc_max First and last forecast times
#' @param fc_horizon Number of steps the horizon spans
#' @return List of `t_change` and `delta`, ordered by time
#' @noRd
sample_horizon_changepoints <- function(delta_train_s, t_change_train,
                                        change_freq, fc_min, fc_max,
                                        fc_horizon) {
  checkmate::assert_numeric(delta_train_s, any.missing = FALSE)
  checkmate::assert_numeric(t_change_train, any.missing = FALSE)
  checkmate::assert_number(change_freq, lower = 0)
  checkmate::assert_number(fc_min)
  checkmate::assert_number(fc_max, lower = fc_min)
  checkmate::assert_number(fc_horizon, lower = 0)
  n_new <- stats::rpois(1L, change_freq * fc_horizon)
  t_change_new <- numeric(0L)
  delta_new <- numeric(0L)
  if (n_new > 0L && length(delta_train_s) > 0L) {
    t_change_new <- sort(unique(stats::runif(n_new, fc_min, fc_max)))
    delta_new <- rlaplace_inverse_cdf(
      length(t_change_new), mu = 0,
      sigma = stats::median(abs(delta_train_s)) + 1e-8
    )
  }
  t_change <- c(t_change_train, t_change_new)
  ord <- order(t_change)
  list(t_change = as.numeric(t_change[ord]),
       delta = as.numeric(c(delta_train_s, delta_new)[ord]))
}

#' Propagate an LLT trend
#'
#' The level adds the last slope and a level shock. The slope adds its
#' own Gaussian shock, damped by `phi_slope`. The fitted state holds
#' the level and the slope at the last occasion. A simulation starts
#' the level at zero and the slope at `normal(0, 2 / h)`. With no slope
#' scale given, `sigma * sqrt(3) / h` gives the slope and the level
#' shocks equal variance over the horizon.
#'
#' @inheritParams propagate_trend
#' @param context Unused, for the shared propagator signature
#' @return Numeric matrix `[h, n_series]`
#' @noRd
propagate_llt <- function(trend_model, params, h, n_series, last_state,
                          context) {
  df <- params$df %||% Inf
  sigma <- rep_len(as.numeric(params$sigma %||% 1), n_series)
  Sigma <- params$Sigma %||% diag(sigma^2, nrow = n_series)
  phi <- if (isTRUE(trend_model$damped)) {
    rep_len(as.numeric(params$phi_slope %||% 0.9), n_series)
  } else {
    rep(1, n_series)
  }
  sigma_slope <- rep_len(
    as.numeric(params$sigma_slope %||% (sigma * sqrt(3) / h)), n_series
  )
  # The slope shocks take the level's correlation with their own scales
  Sigma_slope <- params$Sigma_slope %||%
    (sigma_slope * stats::cov2cor(Sigma) * rep(sigma_slope, each = n_series))

  if (is.null(last_state)) {
    level <- rep(0, n_series)
    slope <- stats::rnorm(n_series, 0, 2 / h)
  } else {
    level <- as.numeric(last_state$trends[nrow(last_state$trends), ])
    slope <- as.numeric(last_state$slope)
  }
  level_shocks <- rmvn(h, rep(0, n_series), Sigma, df = df)
  slope_shocks <- rmvn(h, rep(0, n_series), Sigma_slope)
  out <- matrix(NA_real_, nrow = h, ncol = n_series)
  for (step in seq_len(h)) {
    level <- level + slope + level_shocks[step, ]
    slope <- phi * slope + slope_shocks[step, ]
    out[step, ] <- level
  }
  out
}

#' Propagate a RW trend: an AR(1) with coefficient one
#'
#' @inheritParams propagate_trend
#' @param context Unused, for the shared propagator signature
#' @return Numeric matrix `[h, n_series]`
#' @noRd
propagate_rw <- function(trend_model, params, h, n_series, last_state,
                         context) {
  params$ar <- params$ar %||% 1
  propagate_arma(trend_model, params, h, n_series, last_state,
                 ar_lags = 1L)
}

#' Propagate an AR trend
#'
#' A simulation without supplied coefficients takes 0.7 at every lag.
#'
#' @inheritParams propagate_trend
#' @param context Unused, for the shared propagator signature
#' @return Numeric matrix `[h, n_series]`
#' @noRd
propagate_ar <- function(trend_model, params, h, n_series, last_state,
                         context) {
  ar_lags <- resolve_active_lags(trend_model$p %||% 1L)
  params$ar <- params$ar %||% rep(0.7, length(ar_lags))
  propagate_arma(trend_model, params, h, n_series, last_state,
                 ar_lags = ar_lags)
}

#' Propagate a VAR trend
#'
#' @inheritParams propagate_trend
#' @param context Unused, for the shared propagator signature
#' @return Numeric matrix `[h, n_series]`
#' @noRd
propagate_var <- function(trend_model, params, h, n_series, last_state,
                          context) {
  if (is.null(params$A)) {
    stop(insight::format_error(c(
      "VAR propagation requires 'A' in 'params'.",
      i = "'A' is an array of shape [n_series, n_series, length(p)]."
    )), call. = FALSE)
  }
  propagate_arma(trend_model, params, h, n_series, last_state,
                 ar_lags = resolve_active_lags(trend_model$p %||% 1L))
}

#' Propagate an ARMA or VARMA recursion with `trend_arma_recursC()`
#'
#' The coefficient cubes come from `params`. With no `last_state` a
#' 200-step burn-in from zero draws the start. The trend's own
#' predictor enters outside the recursion, at series scale, which also
#' covers a factor model. The recursion runs zero-mean.
#'
#' @inheritParams propagate_trend
#' @param ar_lags Active AR lags
#' @return Numeric matrix `[h, n_series]`
#' @noRd
propagate_arma <- function(trend_model, params, h, n_series,
                           last_state, ar_lags) {
  checkmate::assert_integerish(ar_lags, lower = 1L, min.len = 1L)
  ma_lags <- if (isTRUE(trend_model$ma)) 1L else integer(0L)
  max_ar <- max(ar_lags)
  max_ma <- max(c(0L, ma_lags))
  max_lag <- max(max_ar, max_ma)

  A_cube <- build_arma_A(params, ar_lags, n_series)
  B_cube <- build_arma_B(params, length(ma_lags), n_series)
  drift <- as.numeric(params$drift %||% 0)
  if (length(drift) == 1L) drift <- rep(drift, n_series)
  Sigma <- innovation_covariance(params, n_series)
  df <- params$df %||% Inf

  if (is.null(last_state)) {
    last_state <- run_burnin(ar_lags, ma_lags, drift, A_cube, B_cube,
                             Sigma, n_series, burn_in = 200L, max_lag,
                             df = df)
  } else {
    validate_last_state(last_state, max_ar, max_ma, n_series)
  }

  trend_arma_recursC(
    ar_lags = as.integer(ar_lags),
    ma_lags = as.integer(ma_lags),
    drift = drift,
    A = A_cube,
    B = B_cube,
    innovations = assemble_innovations(last_state$errors, h, n_series,
                                       Sigma, max_lag, df = df),
    linpreds = matrix(0, nrow = h + max_lag, ncol = n_series),
    last_trends = utils::tail(last_state$trends, max_ar),
    h = as.integer(h)
  )
}

#' The AR coefficient cube of a recursion
#'
#' The full matrices `params$A` where given. Otherwise one diagonal
#' slice per lag from `params$ar`, a value per lag shared by every
#' series or a `[lags, n_series]` matrix.
#'
#' @param params The dynamics parameters
#' @param ar_lags Active AR lags
#' @param n_series Number of series
#' @return Array `[n_series, n_series, length(ar_lags)]`
#' @noRd
build_arma_A <- function(params, ar_lags, n_series) {
  checkmate::assert_list(params)
  checkmate::assert_integerish(ar_lags, lower = 1L, min.len = 1L)
  checkmate::assert_int(n_series, lower = 1L)
  m_a <- length(ar_lags)
  if (!is.null(params$A)) {
    A_cube <- params$A
    checkmate::assert_array(A_cube, d = 3L)
    if (any(dim(A_cube) != c(n_series, n_series, m_a))) {
      stop_shape_fault("'A'", dim(A_cube), c(n_series, n_series, m_a))
    }
    return(A_cube)
  }
  ar_coefs <- params$ar
  if (is.matrix(ar_coefs)) {
    if (any(dim(ar_coefs) != c(m_a, n_series))) {
      stop_shape_fault("'ar'", dim(ar_coefs), c(m_a, n_series))
    }
  } else {
    if (length(ar_coefs) != m_a) {
      stop_shape_fault("'ar'", length(ar_coefs), m_a)
    }
    ar_coefs <- matrix(ar_coefs, nrow = m_a, ncol = n_series)
  }
  A_cube <- array(0, dim = c(n_series, n_series, m_a))
  for (k in seq_len(m_a)) {
    A_cube[, , k] <- diag(ar_coefs[k, ], nrow = n_series)
  }
  A_cube
}

#' The MA coefficient cube of a recursion
#'
#' `params$theta_cube` where given. Otherwise one diagonal slice per
#' lag from `params$theta`, a scalar or one value per series.
#'
#' @param params The dynamics parameters
#' @param m_b Number of MA lags
#' @param n_series Number of series
#' @return Array `[n_series, n_series, m_b]`
#' @noRd
build_arma_B <- function(params, m_b, n_series) {
  checkmate::assert_list(params)
  checkmate::assert_count(m_b)
  checkmate::assert_int(n_series, lower = 1L)
  B_cube <- params$theta_cube
  if (m_b > 0L && !is.null(B_cube)) {
    checkmate::assert_array(B_cube, d = 3L)
    if (any(dim(B_cube) != c(n_series, n_series, m_b))) {
      stop_shape_fault("'theta_cube'", dim(B_cube),
                       c(n_series, n_series, m_b))
    }
    return(B_cube)
  }
  theta <- as.numeric(params$theta %||% 0)
  if (length(theta) == 1L) theta <- rep(theta, n_series)
  checkmate::assert_numeric(theta, len = n_series, any.missing = FALSE,
                            .var.name = "theta")
  B_cube <- array(0, dim = c(n_series, n_series, m_b))
  for (j in seq_len(m_b)) {
    B_cube[, , j] <- diag(theta, nrow = n_series)
  }
  B_cube
}

#' A start for a recursion, from a burn-in
#'
#' The kernel runs `burn_in` steps from zero. The trailing `max_lag`
#' rows are then a draw from the stationary law.
#'
#' @param ar_lags,ma_lags Active AR and MA lags
#' @param drift Drift of each series
#' @param A_cube,B_cube Coefficient cubes
#' @param Sigma Innovation covariance
#' @param n_series Number of series
#' @param burn_in Number of steps to run
#' @param max_lag The largest AR or MA lag
#' @param df Degrees of freedom of the innovations
#' @return List of `trends` and `errors`, each with `max_lag` rows
#' @noRd
run_burnin <- function(ar_lags, ma_lags, drift, A_cube, B_cube,
                       Sigma, n_series, burn_in, max_lag, df = Inf) {
  checkmate::assert_integerish(ar_lags, lower = 1L, min.len = 1L)
  checkmate::assert_int(burn_in, lower = 1L)
  checkmate::assert_int(max_lag, lower = 1L)
  total <- burn_in + max_lag
  innovations <- rmvn(total, mu = rep(0, n_series), Sigma = Sigma,
                      df = df)
  states <- trend_arma_recursC(
    ar_lags = as.integer(ar_lags),
    ma_lags = as.integer(ma_lags),
    drift = drift,
    A = A_cube,
    B = B_cube,
    innovations = innovations,
    linpreds = matrix(0, nrow = total, ncol = n_series),
    last_trends = matrix(0, nrow = max(ar_lags), ncol = n_series),
    h = as.integer(burn_in)
  )
  list(trends = utils::tail(states, max_lag),
       errors = utils::tail(innovations, max_lag))
}

#' Check a supplied start against the recursion it feeds
#'
#' @param last_state List of `trends` and `errors`
#' @param max_ar,max_ma The largest AR and MA lags
#' @param n_series Number of series
#' @return `NULL`, invisibly
#' @noRd
validate_last_state <- function(last_state, max_ar, max_ma, n_series) {
  checkmate::assert_list(last_state)
  checkmate::assert_matrix(last_state$trends, nrows = max_ar,
                           ncols = n_series)
  if (max_ma > 0L) {
    checkmate::assert_matrix(last_state$errors, nrows = max_ma,
                             ncols = n_series)
  }
  invisible(NULL)
}

#' The innovation matrix `trend_arma_recursC()` takes
#'
#' The first `max_lag` rows hold the innovation history. The rest hold
#' the forecast-step innovations, drawn with covariance `Sigma` from
#' the law `df` names.
#'
#' At its first step the kernel takes the lag-j innovation from history
#' row `max_lag - j + 1`, which makes the last history row `e_T`. A
#' fitted state carries `max_ma` rows and a burn-in carries `max_lag`.
#' Both end at `e_T`. The rows supplied fill the end of the history: an
#' `AR(p = 2, ma = TRUE)` state places its one row at the second of
#' two.
#'
#' @param last_errors Matrix of past innovations, or NULL
#' @param h Forecast horizon
#' @param n_series Number of series
#' @param Sigma Innovation covariance
#' @param max_lag The largest AR or MA lag
#' @param df Degrees of freedom of the innovations
#' @return Matrix `[h + max_lag, n_series]`
#' @noRd
assemble_innovations <- function(last_errors, h, n_series, Sigma,
                                 max_lag, df = Inf) {
  checkmate::assert_matrix(last_errors, ncols = n_series, null.ok = TRUE)
  checkmate::assert_int(h, lower = 0L)
  checkmate::assert_int(max_lag, lower = 0L)
  total <- h + max_lag
  out <- matrix(0, nrow = total, ncol = n_series)
  n_hist <- NROW(last_errors)
  if (n_hist > 0L) {
    checkmate::assert_int(n_hist, upper = max_lag)
    out[seq.int(max_lag - n_hist + 1L, max_lag), ] <- last_errors
  }
  if (h > 0L) {
    out[(max_lag + 1L):total, ] <- rmvn(
      h, mu = rep(0, n_series), Sigma = Sigma, df = df
    )
  }
  out
}

#' Multivariate normal or t draws
#'
#' `mgcv::mroot()` factors the scale matrix, which then multiplies
#' standard variates. `df = Inf` gives a multivariate normal. A finite
#' `df` gives the multivariate t of `draw_trend_innovations()`, whose
#' scale matrix is `Sigma`.
#'
#' @param n Number of draws
#' @param mu Mean vector
#' @param Sigma Scale matrix
#' @param df Degrees of freedom
#' @return Matrix with one draw per row
#' @noRd
rmvn <- function(n, mu, Sigma, df = Inf) {
  checkmate::assert_count(n)
  checkmate::assert_numeric(mu, any.missing = FALSE)
  checkmate::assert_matrix(Sigma, nrows = length(mu), ncols = length(mu))
  L <- mgcv::mroot(Sigma)
  m <- ncol(L)
  z <- if (is_gaussian_df(df)) {
    matrix(stats::rnorm(m * n), m, n)
  } else {
    t(draw_trend_innovations(n, m, df))
  }
  t(mu + L %*% z)
}

#' Trend metadata with the fields every forecast draw uses
#'
#' Stored on the fit, they spare each forecast call a parse of the
#' trend constructor:
#' - `trend_type`, the constructor's name,
#' - `ar_lags`, the active AR lags (`1` for RW, LLT and CAR, none for
#'   ZMVN and PW),
#' - `ma_lags`, `1` with `ma = TRUE`, the order every MA term takes,
#' - `max_lag`, the larger of the two,
#' - `has_cor`, `n_lv`, `df` and `fixed_Z` from the constructor,
#' - for PW, `pw_changepoint_range` and `pw_growth`.
#'
#' @param trend_metadata The fit's trend metadata, or NULL
#' @param trend_specs The parsed trend specifications, or NULL
#' @return `trend_metadata` with the fields added, or NULL for a fit
#'   with no trend
#' @noRd
enrich_trend_metadata <- function(trend_metadata, trend_specs) {
  checkmate::assert_list(trend_metadata, null.ok = TRUE)
  checkmate::assert_list(trend_specs, null.ok = TRUE)
  if (is.null(trend_metadata) || is.null(trend_specs)) {
    return(trend_metadata)
  }
  spec <- trend_spec_head(trend_specs)
  if (is.null(spec$trend)) {
    return(trend_metadata)
  }
  trend_metadata$trend_type <- spec$trend
  trend_metadata$ar_lags <- derive_ar_lags(spec)
  trend_metadata$ma_lags <- derive_ma_lags(spec)
  trend_metadata$max_lag <- max(
    c(0L, trend_metadata$ar_lags, trend_metadata$ma_lags)
  )
  trend_metadata$has_cor <- isTRUE(spec$cor)
  trend_metadata$n_lv <- spec_n_lv(spec)
  # Forecasts draw from the innovation law the model was fitted with
  trend_metadata$df <- spec$df %||% Inf
  # The numeric loadings a `trend_map` gave, which
  # `resolve_factor_loadings()` broadcasts across draws
  trend_metadata$fixed_Z <- spec$fixed_Z
  if (identical(spec$trend, "PW")) {
    trend_metadata$pw_changepoint_range <- spec$changepoint_range %||% 0.8
    trend_metadata$pw_growth <- pw_growth(spec)
  }
  trend_metadata
}

#' The active AR lags of a parsed trend constructor
#'
#' @param spec A parsed trend specification
#' @return Integer vector, empty for a trend without AR dynamics
#' @noRd
derive_ar_lags <- function(spec) {
  checkmate::assert_list(spec)
  checkmate::assert_string(spec$trend)
  switch(
    spec$trend,
    RW = ,
    LLT = ,
    CAR = 1L,
    AR = ,
    VAR = resolve_active_lags(spec$p),
    integer(0)
  )
}

#' The active lags an AR or VAR `p` names
#'
#' A scalar `p` names the lags `1:p`. A vector names a sparse set:
#' `c(2, 4)` gives `c(2L, 4L)`. NULL gives none.
#'
#' @param p The constructor's `p`
#' @return Integer vector
#' @noRd
resolve_active_lags <- function(p) {
  checkmate::assert_integerish(p, lower = 1L, any.missing = FALSE,
                               null.ok = TRUE)
  if (length(p) == 0L) {
    return(integer(0))
  }
  if (length(p) == 1L) {
    return(seq_len(as.integer(p)))
  }
  as.integer(p)
}

#' Is every lag from 1 to the largest present?
#'
#' The companion form then holds the whole autoregression in
#' `max(lags)` states. A sparse set such as `c(1, 12)` fixes its
#' intermediate coefficients at zero.
#'
#' @param ar_lags Active AR lags
#' @return Logical scalar
#' @noRd
ar_lags_contiguous <- function(ar_lags) {
  checkmate::assert_integerish(ar_lags, lower = 1L, any.missing = FALSE)
  lags <- as.integer(ar_lags)
  length(lags) > 0L && identical(lags, seq_len(max(lags)))
}

#' Does a lag set take the partial autocorrelation parameterisation?
#'
#' For the lags `1:p` with `p >= 2` the Levinson-Durbin recursion
#' returns exactly the stationary AR(p) coefficients from partial
#' autocorrelations inside (-1, 1). A single lag needs no recursion:
#' its declared interval is the stationary region. A sparse set fixes
#' its intermediate coefficients at zero, a constraint the recursion
#' cannot express. Those sets keep the bounded coefficients they
#' declare.
#'
#' @param ar_lags Active AR lags
#' @return Logical scalar
#' @noRd
ar_lags_stationary <- function(ar_lags) {
  length(ar_lags) >= 2L && ar_lags_contiguous(ar_lags)
}

#' The infix of an AR coefficient name
#'
#' A lag set on the partial autocorrelation parameterisation spells its
#' sampled parameters `ar{lag}_pacf_*`. The Stan generator and the prior
#' surface share this rule.
#'
#' @param ar_lags Active AR lags
#' @return `"_pacf"` or `""`
#' @noRd
ar_coef_stem <- function(ar_lags) {
  if (ar_lags_stationary(ar_lags)) "_pacf" else ""
}

#' The scalars an `AR()` samples per lag under `coef_sharing = "shared"`
#'
#' The `_trend` suffix places the name on the trend side for
#' `is_trend_parameter()`. The prefix matches `mu_ar{lag}_trend` and
#' `sigma_ar{lag}_trend`, the hyperparameters of the hierarchical mode.
#'
#' @param ar_lags Active AR lags
#' @return Character vector
#' @noRd
ar_shared_coef_names <- function(ar_lags) {
  paste0("shared_ar", ar_lags, ar_coef_stem(ar_lags), "_trend")
}

#' Every AR coefficient parameter mvgam owns for a lag set
#'
#' `generate_monitor_params()` returns it to the prior table and to the
#' set of names mvgam withholds from brms. The withheld set needs the
#' per-series coefficient under every mode.
#' `generate_trend_priors_from_monitor_params()` narrows the list to
#' the rows a user can edit.
#'
#' @param ar_lags Active AR lags
#' @param coef_sharing "none", "shared" or "hierarchical"
#' @return Character vector
#' @noRd
ar_monitor_coef_names <- function(ar_lags, coef_sharing = "none") {
  checkmate::assert_choice(coef_sharing,
                           c("none", "shared", "hierarchical"))
  stem <- ar_coef_stem(ar_lags)
  coefs <- paste0("ar", ar_lags, stem, "_trend")
  switch(
    coef_sharing,
    none = coefs,
    shared = c(ar_shared_coef_names(ar_lags), coefs),
    hierarchical = c(
      coefs,
      paste0("mu_ar", ar_lags, stem, "_trend"),
      paste0("sigma_ar", ar_lags, stem, "_trend")
    )
  )
}

#' The active MA lags of a parsed trend constructor
#'
#' Every trend that takes an MA term fits order one.
#'
#' @param spec A parsed trend specification
#' @return `1L`, or an empty integer vector
#' @noRd
derive_ma_lags <- function(spec) {
  checkmate::assert_list(spec)
  if (isTRUE(spec$ma)) 1L else integer(0)
}
