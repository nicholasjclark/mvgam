# Unit tests for `forecast.mvgam()` in `R/forecast.mvgam.R`. The
# function orchestrates a per-draw kernel loop, an obs-side
# linpred combination, and an optional family sampling pass. The
# heavy primitives (extract_component_linpred, propagate_trend,
# posterior_predict / posterior_epred, get_observation_structure)
# are exercised by their own test files, so these tests stub
# them via `local_mocked_bindings()` and assert on:
#   * mvgam_forecast class shape (16 fields, list-of-matrices
#     for hindcasts / forecasts).
#   * uncertainty-toggle semantics (b_uncertainty,
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
                              max_lag = 1L) {
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
    obs_data = d,
    formula = stats::as.formula("y ~ 1"),
    call = match.call(),
    trend_call = stats::as.formula("~ AR(p = 1)"),
    family = poisson(),
    trend_model = list(),
    mv_spec = list(
      response_names = "y",
      trend_specs = spec
    ),
    series_info = list(series_levels = series_levels),
    trend_metadata = list(
      trend_type = trend_type,
      ar_lags = ar_lags,
      ma_lags = ma_lags,
      max_lag = max_lag,
      has_cor = FALSE,
      variables = list(time_var = "time", series_var = "series"),
      dimensions = list(n_series = length(series_levels))
    ),
    standata = list(
      N_series_trend = length(series_levels),
      N_lv_trend = length(series_levels),
      N_time_trend = n_time
    ),
    backend = "rstan"
  )
  class(fit) <- "mvgam"
  fit
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


# Build a fake obs_struct that mirrors the real fields. Raw
# times live on `names(time)` per `ensure_mvgam_variables`.
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


# ----- Class shape + type dispatch --------------------------------

test_that("Returns mvgam_forecast with 16 contractual fields", {
  fit <- make_mock_mvgam()
  draws <- make_draws_mat(ndraws = 3L)
  newdata <- data.frame(time = 11:12,
                         series = factor("s1", levels = "s1"),
                         y = NA_integer_)

  testthat::local_mocked_bindings(
    `as_draws_matrix` = function(...) draws,
    .package = "posterior"
  )
  testthat::local_mocked_bindings(
    extract_component_linpred = function(mvgam_fit, newdata,
                                            component, ...) {
      matrix(0.1, nrow = 3L, ncol = nrow(newdata))
    },
    get_observation_structure = function(object, newdata, ...) {
      make_obs_struct_for_grid(
        as.integer(newdata$time),
        rep(1L, nrow(newdata)),
        levels(newdata$series)
      )
    },
    propagate_trend = function(...) {
      matrix(c(0.5, 0.6), nrow = 2L, ncol = 1L)
    },
    extract_last_state = function(...) {
      list(
        params = list(sigma = 0.1, ar = matrix(0.5, 1L, 1L)),
        last_state = list(
          trends = matrix(0, 1L, 1L),
          errors = NULL,
          linpreds = matrix(0, 1L, 1L)
        )
      )
    },
    posterior_predict = function(...) {
      matrix(1L, nrow = 3L, ncol = 10L)
    },
    posterior_epred = function(...) {
      matrix(1, nrow = 3L, ncol = 10L)
    }
  )

  # `type = "expected"` avoids the family sampling path, which
  # would require a real stanfit object for dpar extraction; the
  # class-shape contract is the same for every type.
  fc <- forecast(fit, newdata = newdata, type = "expected")
  expect_s3_class(fc, "mvgam_forecast")
  required <- c("call", "trend_call", "family", "family_pars",
                "trend_model", "drift", "use_lv", "fit_engine",
                "type", "series_names", "train_observations",
                "train_times", "test_observations", "test_times",
                "hindcasts", "forecasts")
  expect_true(all(required %in% names(fc)))
  expect_identical(fc$type, "expected")
  expect_true(is.list(fc$forecasts))
  expect_true(is.list(fc$hindcasts))
})


test_that("newdata = NULL returns hindcasts only, no forecasts", {
  fit <- make_mock_mvgam()
  draws <- make_draws_mat(ndraws = 3L)
  testthat::local_mocked_bindings(
    `as_draws_matrix` = function(...) draws,
    .package = "posterior"
  )
  testthat::local_mocked_bindings(
    posterior_predict = function(...) {
      matrix(2L, nrow = 3L, ncol = 10L)
    }
  )
  fc <- forecast(fit, newdata = NULL, type = "response")
  expect_null(fc$forecasts)
  expect_null(fc$test_times)
  expect_null(fc$test_observations)
  expect_identical(dim(fc$hindcasts[["s1"]]), c(3L, 10L))
})


test_that("type = 'link' populates family_pars", {
  fit <- make_mock_mvgam()
  draws <- make_draws_mat(
    ndraws = 4L,
    cols = c("Intercept", "sigma_trend[1]",
              "ar1_trend[1]", "shape")
  )
  # Negative-binomial family so `shape` is a real dpar.
  fit$family <- brms::negbinomial()
  newdata <- data.frame(time = 11:12,
                         series = factor("s1", levels = "s1"),
                         y = NA_integer_)
  testthat::local_mocked_bindings(
    `as_draws_matrix` = function(...) draws,
    .package = "posterior"
  )
  testthat::local_mocked_bindings(
    extract_component_linpred = function(mvgam_fit, newdata,
                                            component, ...) {
      matrix(0.1, nrow = 4L, ncol = nrow(newdata))
    },
    get_observation_structure = function(object, newdata, ...) {
      make_obs_struct_for_grid(
        as.integer(newdata$time),
        rep(1L, nrow(newdata)),
        levels(newdata$series)
      )
    },
    propagate_trend = function(...) {
      matrix(c(0.5, 0.6), nrow = 2L, ncol = 1L)
    },
    extract_last_state = function(...) {
      list(
        params = list(sigma = 0.1, ar = matrix(0.5, 1L, 1L)),
        last_state = list(
          trends = matrix(0, 1L, 1L), errors = NULL,
          linpreds = matrix(0, 1L, 1L)
        )
      )
    },
    posterior_predict = function(...) matrix(1L, 4L, 10L),
    posterior_epred = function(...) matrix(1, 4L, 10L)
  )
  fc <- forecast(fit, newdata = newdata, type = "link",
                  ndraws = 4L)
  expect_true("shape" %in% names(fc$family_pars))
  expect_identical(dim(fc$family_pars$shape), c(4L, 1L))
})


# ----- Validation errors ------------------------------------------

test_that("ndraws beyond available draws errors informatively", {
  fit <- make_mock_mvgam()
  draws <- make_draws_mat(ndraws = 3L)
  testthat::local_mocked_bindings(
    `as_draws_matrix` = function(...) draws,
    .package = "posterior"
  )
  expect_error(
    forecast(fit, newdata = NULL, type = "response", ndraws = 50L),
    "exceeds the number of posterior draws"
  )
})


test_that("All multivariate / PW trend types flow through dispatch", {
  # Every supported trend type should now reach
  # build_hindcast_arms (stubbed via posterior_predict /
  # posterior_epred below). Only an invented "BOGUS" type
  # should still fail before dispatch.
  draws <- make_draws_mat(ndraws = 2L)
  testthat::local_mocked_bindings(
    `as_draws_matrix` = function(...) draws,
    .package = "posterior"
  )
  testthat::local_mocked_bindings(
    posterior_predict = function(...) matrix(1L, 2L, 10L),
    posterior_epred = function(...) matrix(1, 2L, 10L)
  )
  for (tt in c("VAR", "CAR", "PW")) {
    fit_tt <- make_mock_mvgam(trend_type = tt)
    fit_tt$mv_spec$trend_specs$trend <- tt
    expect_no_error(
      forecast(fit_tt, newdata = NULL, type = "response",
                ndraws = 2L)
    )
  }
})


# ----- compute_car_forecast_time ----------------------------------

test_that("compute_car_forecast_time builds the right gap vector", {
  # Fake fit with two series, last training times = c(10, 10).
  fit <- make_mock_mvgam(series_levels = c("a", "b"))
  testthat::local_mocked_bindings(
    extract_last_observed_times = function(fit, n_series) {
      c(10, 10)
    }
  )
  fc_grid <- list(times = list(a = c(11, 14, 16),
                                  b = c(11, 14, 16)))
  out <- compute_car_forecast_time(fit, fc_grid,
                                     series_levels = c("a", "b"))
  expect_equal(out, c(1, 3, 2))
})


test_that("compute_car_forecast_time errors on per-series time mismatch", {
  fit <- make_mock_mvgam(series_levels = c("a", "b"))
  testthat::local_mocked_bindings(
    extract_last_observed_times = function(fit, n_series) {
      c(10, 10)
    }
  )
  # Series 'a' has gaps c(1, 1); series 'b' has gaps c(1, 2).
  fc_grid <- list(times = list(a = c(11, 12),
                                  b = c(11, 13)))
  expect_error(
    compute_car_forecast_time(fit, fc_grid,
                                series_levels = c("a", "b")),
    "share the same forecast"
  )
})


test_that("compute_car_forecast_time ignores series with no forecast rows", {
  fit <- make_mock_mvgam(series_levels = c("a", "b"))
  testthat::local_mocked_bindings(
    extract_last_observed_times = function(fit, n_series) {
      c(10, 10)
    }
  )
  fc_grid <- list(times = list(a = c(11, 14, 16),
                                  b = integer(0L)))
  out <- compute_car_forecast_time(fit, fc_grid,
                                     series_levels = c("a", "b"))
  expect_equal(out, c(1, 3, 2))
})


test_that("Fit with no trend spec errors", {
  fit <- make_mock_mvgam()
  fit$mv_spec$trend_specs <- NULL
  expect_error(
    forecast(fit, newdata = NULL, type = "response"),
    "no trend specification"
  )
})


test_that("Newdata with unseen series levels errors crisply", {
  fit <- make_mock_mvgam()
  draws <- make_draws_mat(ndraws = 2L)
  newdata <- data.frame(time = 11:12,
                         series = factor("ghost", levels = "ghost"),
                         y = NA_integer_)
  testthat::local_mocked_bindings(
    `as_draws_matrix` = function(...) draws,
    .package = "posterior"
  )
  testthat::local_mocked_bindings(
    posterior_predict = function(...) matrix(1L, 2L, 10L)
  )
  expect_error(
    forecast(fit, newdata = newdata, type = "response"),
    "series levels not seen"
  )
})


test_that("Newdata missing time / series columns errors", {
  fit <- make_mock_mvgam()
  draws <- make_draws_mat(ndraws = 2L)
  newdata <- data.frame(notime = 11:12, y = NA_integer_)
  testthat::local_mocked_bindings(
    `as_draws_matrix` = function(...) draws,
    .package = "posterior"
  )
  testthat::local_mocked_bindings(
    posterior_predict = function(...) matrix(1L, 2L, 10L)
  )
  expect_error(
    forecast(fit, newdata = newdata, type = "response"),
    "time / series columns"
  )
})


# ----- pad_or_trim_rows -------------------------------------------

test_that("pad_or_trim_rows pads short grids by repeating final row", {
  g <- matrix(c(1, 2, 3, 4), nrow = 2L, ncol = 2L)
  out <- pad_or_trim_rows(g, target_rows = 4L)
  expect_identical(dim(out), c(4L, 2L))
  expect_equal(out[1, ], c(1, 3))
  expect_equal(out[2, ], c(2, 4))
  # Rows 3 and 4 repeat the final source row.
  expect_equal(out[3, ], c(2, 4))
  expect_equal(out[4, ], c(2, 4))
})


test_that("pad_or_trim_rows trims long grids from the head", {
  g <- matrix(seq_len(6L), nrow = 6L, ncol = 1L)
  out <- pad_or_trim_rows(g, target_rows = 3L)
  expect_identical(dim(out), c(3L, 1L))
  # Trimming from the head keeps the trailing 3 rows.
  expect_equal(as.numeric(out), c(4, 5, 6))
})


test_that("pad_or_trim_rows preserves an exact-fit grid", {
  g <- matrix(c(1, 2), nrow = 2L, ncol = 1L)
  expect_identical(pad_or_trim_rows(g, 2L), g)
})


test_that("pad_or_trim_rows handles zero-row input as a zero pad", {
  g <- matrix(0, nrow = 0L, ncol = 2L)
  out <- pad_or_trim_rows(g, target_rows = 3L)
  expect_identical(dim(out), c(3L, 2L))
  expect_true(all(out == 0))
})


# ----- slice_per_series -------------------------------------------

test_that("slice_per_series picks the right cells per raw time", {
  # 2 series, 3 forecast times each, interleaved in obs_struct
  # row order (t, s) = (41,a), (41,b), (42,a), (42,b), (43,a),
  # (43,b). Mat values encode (time*10 + series_int).
  obs_struct <- make_obs_struct_for_grid(
    times = c(41, 41, 42, 42, 43, 43),
    series_int = c(1, 2, 1, 2, 1, 2),
    series_levels = c("a", "b")
  )
  mat <- matrix(
    c(411, 412, 421, 422, 431, 432),
    nrow = 1L
  )
  fc_grid <- list(times = list(a = c(41, 42, 43),
                                  b = c(41, 42, 43)))
  out <- slice_per_series(mat, fc_grid, obs_struct,
                            ndraws_use = 1L,
                            series_levels = c("a", "b"))
  expect_equal(as.numeric(out[["a"]]), c(411, 421, 431))
  expect_equal(as.numeric(out[["b"]]), c(412, 422, 432))
})


test_that("slice_per_series handles per-series horizon differences", {
  # Series 'a' has 3 forecast times, 'b' has 2.
  obs_struct <- make_obs_struct_for_grid(
    times = c(41, 41, 42, 42, 43),
    series_int = c(1, 2, 1, 2, 1),
    series_levels = c("a", "b")
  )
  mat <- matrix(c(411, 412, 421, 422, 431), nrow = 1L)
  fc_grid <- list(times = list(a = c(41, 42, 43),
                                  b = c(41, 42)))
  out <- slice_per_series(mat, fc_grid, obs_struct,
                            ndraws_use = 1L,
                            series_levels = c("a", "b"))
  expect_identical(dim(out[["a"]]), c(1L, 3L))
  expect_identical(dim(out[["b"]]), c(1L, 2L))
  expect_equal(as.numeric(out[["a"]]), c(411, 421, 431))
  expect_equal(as.numeric(out[["b"]]), c(412, 422))
})
