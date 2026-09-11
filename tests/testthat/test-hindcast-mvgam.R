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
    extract_trend_latent_states = function(...) {
      matrix(0, nrow = 3L, ncol = 8L)
    },
    extract_component_linpred = function(mvgam_fit, newdata,
                                           component, ...) {
      matrix(0, nrow = 3L, ncol = nrow(newdata))
    },
    draw_observations = function(object, linpred, newdata, draw_ids,
                                 resp = NULL) {
      matrix(2L, nrow = nrow(linpred), ncol = nrow(newdata))
    }
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
  fit$family$linkinv <- function(eta) eta + 100  # tag the linkinv call
  draws <- make_hindcast_draws(ndraws = 2L)
  testthat::local_mocked_bindings(
    `as_draws_matrix` = function(...) draws,
    .package = "posterior"
  )
  # Standard families: trend pulled per-draw from stanfit via
  # extract_trend_latent_states; obs linpred via
  # extract_component_linpred(component = 'obs'); response sampled
  # via draw_observations. Distinct constants per arm so we
  # can confirm the right helper was hit.
  testthat::local_mocked_bindings(
    extract_trend_latent_states = function(...) {
      matrix(7, nrow = 2L, ncol = 8L)
    },
    extract_component_linpred = function(mvgam_fit, newdata,
                                           component, ...) {
      matrix(3, nrow = 2L, ncol = nrow(newdata))
    },
    draw_observations = function(object, linpred, newdata, draw_ids,
                                 resp = NULL) {
      matrix(13L, nrow = nrow(linpred), ncol = nrow(newdata))
    }
  )

  hc_trend <- hindcast(fit, type = "trend")
  expect_true(all(hc_trend$hindcasts[["s1"]] == 7))

  hc_link <- hindcast(fit, type = "link")
  expect_true(all(hc_link$hindcasts[["s1"]] == 7 + 3))

  hc_exp <- hindcast(fit, type = "expected")
  expect_true(all(hc_exp$hindcasts[["s1"]] == 100 + 7 + 3))

  hc_resp <- hindcast(fit, type = "response")
  expect_true(all(hc_resp$hindcasts[["s1"]] == 13))
})


test_that("obs_uncertainty = FALSE returns family mean for response", {
  fit <- make_hindcast_mock()
  fit$family$linkinv <- function(eta) eta + 100
  draws <- make_hindcast_draws(ndraws = 2L)
  testthat::local_mocked_bindings(
    `as_draws_matrix` = function(...) draws,
    .package = "posterior"
  )
  testthat::local_mocked_bindings(
    extract_trend_latent_states = function(...) {
      matrix(5, nrow = 2L, ncol = 8L)
    },
    extract_component_linpred = function(mvgam_fit, newdata,
                                           component, ...) {
      matrix(2, nrow = 2L, ncol = nrow(newdata))
    },
    draw_observations = function(...) {
      stop("draw_observations should not be called when obs_uncertainty = FALSE")
    }
  )
  hc <- hindcast(fit, type = "response", obs_uncertainty = FALSE)
  # obs_uncertainty = FALSE returns family$linkinv(obs + trend)
  # without family-RNG noise, matching the fitted-state expected
  # value at each cell. With linkinv = +100, trend = 5, obs = 2,
  # the per-cell value is 107.
  expect_true(all(hc$hindcasts[["s1"]] == 107))
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
    extract_trend_latent_states = function(...) {
      matrix(0.1, nrow = 4L, ncol = 8L)
    },
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
    "more draws than the posterior holds"
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
    extract_trend_latent_states = function(...) {
      matrix(0, nrow = 5L, ncol = 8L)
    },
    extract_component_linpred = function(mvgam_fit, newdata,
                                           component, ...) {
      matrix(0, nrow = 5L, ncol = nrow(newdata))
    },
    draw_observations = function(object, linpred, newdata, draw_ids,
                                 resp = NULL) {
      matrix(seq_len(5L * 8L), nrow = nrow(linpred), ncol = 8L)
    }
  )
  hc <- hindcast(fit, type = "response", ndraws = 2L)
  expect_identical(nrow(hc$hindcasts[["s1"]]), 2L)
})


test_that("mv fan-out wrapper is classed and plot dispatches", {
  # Simulate an mvbf hindcast wrapper: outer list keyed by resp,
  # each element itself an mvgam_forecast. The outer list needs its
  # own class so plot(hc) dispatches to plot.mvgam_forecast
  # (single-panel trend view) instead of falling through to
  # graphics::plot.default.
  make_arm <- function(type_slot) {
    hc_mat <- matrix(rnorm(30L), nrow = 3L, ncol = 10L)
    structure(list(
      family = "gaussian", family_pars = NULL,
      type = type_slot,
      series_names = "s1",
      train_observations = list(s1 = seq_len(10L)),
      train_times = list(s1 = seq_len(10L)),
      test_observations = NULL, test_times = NULL,
      hindcasts = list(s1 = hc_mat),
      forecasts = NULL
    ), class = "mvgam_forecast")
  }
  wrapper <- list(count = make_arm("trend"), pa = make_arm("trend"))
  class(wrapper) <- "mvgam_forecast"
  attr(wrapper, "mv_wrapper") <- TRUE

  p <- plot(wrapper)
  expect_s3_class(p, "ggplot")
  # No obs point layer overlaid on trend-scale ribbon.
  has_point <- vapply(p$layers,
    function(l) inherits(l$geom, "GeomPoint"),
    logical(1L))
  expect_false(any(has_point))

  # Response-scale wrapper renders one panel per response.
  wrapper2 <- list(count = make_arm("response"),
                   pa    = make_arm("response"))
  class(wrapper2) <- "mvgam_forecast"
  attr(wrapper2, "mv_wrapper") <- TRUE
  p2 <- plot(wrapper2)
  # patchwork stacks the per-arm plots when available; if not,
  # we get the last per-arm ggplot back invisibly.
  expect_true(inherits(p2, "ggplot") ||
                inherits(p2, "patchwork"))
})


test_that("Multi-series hindcast returns one matrix per series", {
  fit <- make_hindcast_mock(series_levels = c("a", "b"))
  draws <- make_hindcast_draws(ndraws = 2L)
  testthat::local_mocked_bindings(
    `as_draws_matrix` = function(...) draws,
    .package = "posterior"
  )
  testthat::local_mocked_bindings(
    extract_trend_latent_states = function(mvgam_fit, newdata,
                                             full_draws,
                                             resp = NULL) {
      matrix(0, nrow = 2L, ncol = nrow(newdata))
    },
    extract_component_linpred = function(mvgam_fit, newdata,
                                           component, ...) {
      matrix(0, nrow = 2L, ncol = nrow(newdata))
    },
    draw_observations = function(object, linpred, newdata, draw_ids,
                                 resp = NULL) {
      matrix(0L, nrow = nrow(linpred), ncol = nrow(newdata))
    }
  )
  hc <- hindcast(fit, type = "response")
  expect_named(hc$hindcasts, c("a", "b"))
  expect_identical(dim(hc$hindcasts[["a"]]), c(2L, 8L))
  expect_identical(dim(hc$hindcasts[["b"]]), c(2L, 8L))
})
