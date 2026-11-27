# Helpers shared by test-forecast-mvgam.R and test-extra-forecast-mvgam.R.
# Unit tests for `forecast.mvgam()` in `R/forecast.mvgam.R`. The
# function orchestrates a per-draw kernel loop, an obs-side
# linpred combination, and an optional family sampling pass. The
# heavy primitives (extract_component_linpred, propagate_trend,
# posterior_predict / posterior_epred, get_observation_structure)
# are exercised by their own test files, so these tests stub
# them via `local_mocked_bindings()` and assert on:
#   * mvgam_forecast class shape (12 fields, list-of-matrices
#     for hindcasts / forecasts).
#   * uncertainty-toggle semantics (coef_uncertainty,
#     trend_uncertainty, obs_uncertainty).
#   * newdata-handling edge cases (NULL, no-novel-times,
#     unseen-series error).
#   * type dispatch (response / link / expected / trend) and the
#     family-pars slot for type = "link".


# A minimal `mvgam`-class object that satisfies just enough of
# the fit interface for `forecast.mvgam()` to traverse. The
# tests below mock the primitives that would otherwise read
# real Stan output / brms machinery.
make_mock_mvgam <- function(series_levels = "s1", n_time = 10L,
                              trend_type = "AR",
                              ar_lags = 1L, ma_lags = integer(0),
                              max_lag = 1L, n_lv = NULL) {
  d <- data.frame(
    time = rep(seq_len(n_time), length(series_levels)),
    series = factor(rep(series_levels, each = n_time),
                    levels = series_levels),
    y = rep(0L, n_time * length(series_levels))
  )
  spec <- list(trend = trend_type, p = 1, ma = FALSE,
               cor = FALSE, drift = FALSE)
  class(spec) <- "mvgam_trend"

  fit <- list(
    fit = structure(list(), class = "fake_stanfit"),
    data = d,
    formula = stats::as.formula("y ~ 1"),
    call = match.call(),
    trend_call = stats::as.formula("~ AR(p = 1)"),
    family = poisson(),
    trend_model = list(),
    mv_spec = list(
      trend_specs = spec
    ),
    trend_metadata = c(mock_axis_record(d), list(
      trend_type = trend_type,
      ar_lags = ar_lags,
      ma_lags = ma_lags,
      max_lag = max_lag,
      has_cor = FALSE
    )),
    standata = list(
      N_series_trend = length(series_levels),
      N_lv_trend = n_lv %||% length(series_levels),
      N_time_trend = n_time
    ),
    backend = "rstan"
  )
  class(fit) <- "mvgam"
  fit
}


# A frame reaching one occasion past the mock fit's grid, for the
# calls whose subject is something other than the horizon. Built
# from the fit's own data so the series levels and the columns
# match whatever `make_mock_mvgam()` was given.
mock_future_data <- function(fit, h = 1L) {
  d <- fit$data
  levs <- levels(d$series)
  nxt <- max(d$time) + seq_len(h)
  data.frame(
    time = rep(nxt, times = length(levs)),
    series = factor(rep(levs, each = h), levels = levs),
    y = NA_integer_
  )
}


# Stub `posterior::as_draws_matrix` so `forecast.mvgam` sees a
# named numeric matrix without touching the fake stanfit.
make_draws_mat <- function(ndraws = 5L,
                            cols = c("Intercept", "sigma_trend[1]",
                                       "ar1_trend[1]")) {
  m <- matrix(stats::rnorm(ndraws * length(cols)),
              nrow = ndraws, dimnames = list(NULL, cols))
  posterior::as_draws_matrix(m)
}

make_obs_struct_for_grid <- function(times, series_int,
                                       series_levels) {
  unique_times <- sort(unique(times))
  time_pos <- match(times, unique_times)
  names(time_pos) <- as.character(times)
  list(
    time = time_pos,
    series = factor(series_levels[series_int],
                    levels = series_levels),
    series_int = as.integer(series_int),
    series_levels = series_levels,
    n_obs = length(times),
    n_times = length(unique_times),
    n_series = length(series_levels),
    unique_times = seq_along(unique_times)
  )
}
