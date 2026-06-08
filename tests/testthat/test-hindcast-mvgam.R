# Unit tests for `hindcast.mvgam()` in `R/hindcast.mvgam.R`. The
# method is a thin wrapper over `build_hindcast_arms()` (already
# covered by tests in `test-forecast-mvgam.R`), so these tests
# focus on:
#   * mvgam_forecast class shape (10 fields; test_* / forecasts
#     slots NULL).
#   * type dispatch (response / link / expected / trend).
#   * obs_uncertainty toggle for `type = "response"`.
#   * ndraws validation + subset behaviour.


# Reuse `make_mock_mvgam` and `make_draws_mat` from the forecast
# test file; they are defined in that file's source-only top
# section and become available once testthat::test_dir() loads
# both files in the same session. To keep this file independently
# runnable, re-define minimal versions here.
make_hindcast_mock <- function(series_levels = "s1",
                               n_time = 8L,
                               trend_type = "AR") {
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
      ar_lags = 1L,
      ma_lags = integer(0L),
      max_lag = 1L,
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

make_hindcast_draws <- function(ndraws = 3L,
                                cols = c("Intercept",
                                         "sigma_trend[1]",
                                         "ar1_trend[1]")) {
  m <- matrix(stats::rnorm(ndraws * length(cols)),
              nrow = ndraws, dimnames = list(NULL, cols))
  posterior::as_draws_matrix(m)
}


test_that("hindcast.mvgam returns mvgam_forecast with NULL forecast slots", {
  fit <- make_hindcast_mock()
  draws <- make_hindcast_draws(ndraws = 3L)
  testthat::local_mocked_bindings(
    `as_draws_matrix` = function(...) draws,
    .package = "posterior"
  )
  testthat::local_mocked_bindings(
    posterior_predict = function(...) matrix(2L, nrow = 3L, ncol = 8L),
    posterior_epred = function(...) matrix(1, nrow = 3L, ncol = 8L)
  )
  hc <- hindcast(fit, type = "response")
  expect_s3_class(hc, "mvgam_forecast")
  required <- c("family", "family_pars", "type", "series_names",
                "train_observations", "train_times",
                "test_observations", "test_times",
                "hindcasts", "forecasts")
  expect_equal(sort(names(hc)), sort(required))
  dead <- c("call", "trend_call", "trend_model", "drift",
            "use_lv", "fit_engine")
  expect_false(any(dead %in% names(hc)))
  expect_null(hc$forecasts)
  expect_null(hc$test_times)
  expect_null(hc$test_observations)
  expect_identical(hc$type, "response")
  expect_identical(dim(hc$hindcasts[["s1"]]), c(3L, 8L))
})


test_that("hindcast.mvgam type dispatch flows through expected helpers", {
  fit <- make_hindcast_mock()
  draws <- make_hindcast_draws(ndraws = 2L)
  testthat::local_mocked_bindings(
    `as_draws_matrix` = function(...) draws,
    .package = "posterior"
  )
  # Distinct constant per dispatch arm so we can confirm the
  # right helper was hit.
  testthat::local_mocked_bindings(
    extract_component_linpred = function(mvgam_fit, newdata,
                                         component, ...) {
      val <- if (identical(component, "trend")) 7 else 3
      matrix(val, nrow = 2L, ncol = nrow(newdata))
    },
    posterior_epred = function(...) matrix(11, nrow = 2L, ncol = 8L),
    posterior_predict = function(...) matrix(13L, nrow = 2L, ncol = 8L)
  )

  hc_trend <- hindcast(fit, type = "trend")
  expect_true(all(hc_trend$hindcasts[["s1"]] == 7))

  hc_link <- hindcast(fit, type = "link")
  expect_true(all(hc_link$hindcasts[["s1"]] == 7 + 3))

  hc_exp <- hindcast(fit, type = "expected")
  expect_true(all(hc_exp$hindcasts[["s1"]] == 11))

  hc_resp <- hindcast(fit, type = "response")
  expect_true(all(hc_resp$hindcasts[["s1"]] == 13))
})


test_that("obs_uncertainty = FALSE returns family mean for response", {
  fit <- make_hindcast_mock()
  draws <- make_hindcast_draws(ndraws = 2L)
  testthat::local_mocked_bindings(
    `as_draws_matrix` = function(...) draws,
    .package = "posterior"
  )
  testthat::local_mocked_bindings(
    posterior_epred = function(...) matrix(17, nrow = 2L, ncol = 8L),
    posterior_predict = function(...) matrix(19L, nrow = 2L, ncol = 8L)
  )
  hc <- hindcast(fit, type = "response", obs_uncertainty = FALSE)
  # obs_uncertainty = FALSE routes "response" through epred,
  # not posterior_predict.
  expect_true(all(hc$hindcasts[["s1"]] == 17))
})


test_that("type = 'link' populates family_pars", {
  fit <- make_hindcast_mock()
  draws <- make_hindcast_draws(
    ndraws = 4L,
    cols = c("Intercept", "sigma_trend[1]", "ar1_trend[1]", "shape")
  )
  fit$family <- brms::negbinomial()
  testthat::local_mocked_bindings(
    `as_draws_matrix` = function(...) draws,
    .package = "posterior"
  )
  testthat::local_mocked_bindings(
    extract_component_linpred = function(mvgam_fit, newdata,
                                         component, ...) {
      matrix(0.1, nrow = 4L, ncol = nrow(newdata))
    }
  )
  hc <- hindcast(fit, type = "link", ndraws = 4L)
  expect_true("shape" %in% names(hc$family_pars))
  expect_identical(dim(hc$family_pars$shape), c(4L, 1L))
})


test_that("ndraws beyond available draws errors informatively", {
  fit <- make_hindcast_mock()
  draws <- make_hindcast_draws(ndraws = 3L)
  testthat::local_mocked_bindings(
    `as_draws_matrix` = function(...) draws,
    .package = "posterior"
  )
  expect_error(
    hindcast(fit, type = "response", ndraws = 50L),
    "exceeds the number of posterior draws"
  )
})


test_that("ndraws subset yields the requested rows", {
  fit <- make_hindcast_mock()
  draws <- make_hindcast_draws(ndraws = 5L)
  testthat::local_mocked_bindings(
    `as_draws_matrix` = function(...) draws,
    .package = "posterior"
  )
  testthat::local_mocked_bindings(
    posterior_predict = function(...) {
      matrix(seq_len(5L * 8L), nrow = 5L, ncol = 8L)
    }
  )
  hc <- hindcast(fit, type = "response", ndraws = 2L)
  expect_identical(nrow(hc$hindcasts[["s1"]]), 2L)
})


test_that("Multi-series hindcast returns one matrix per series", {
  fit <- make_hindcast_mock(series_levels = c("a", "b"))
  draws <- make_hindcast_draws(ndraws = 2L)
  testthat::local_mocked_bindings(
    `as_draws_matrix` = function(...) draws,
    .package = "posterior"
  )
  testthat::local_mocked_bindings(
    posterior_predict = function(object, newdata, ...) {
      # 2 draws, 8 columns per series subset.
      matrix(0L, nrow = 2L, ncol = nrow(newdata))
    }
  )
  hc <- hindcast(fit, type = "response")
  expect_named(hc$hindcasts, c("a", "b"))
  expect_identical(dim(hc$hindcasts[["a"]]), c(2L, 8L))
  expect_identical(dim(hc$hindcasts[["b"]]), c(2L, 8L))
})
