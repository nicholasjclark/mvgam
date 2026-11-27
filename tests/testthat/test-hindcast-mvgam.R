

test_that("hindcast.mvgam returns mvgam_forecast with NULL forecast slots", {
  fit <- make_hindcast_mock()
  draws <- make_hindcast_draws(ndraws = 3L)
  testthat::local_mocked_bindings(
    `as_draws_matrix` = function(...) draws,
    ndraws = function(...) nrow(draws),
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
  required <- c("family", "family_pars", "type", "series_names",
                "train_observations", "train_times",
                "test_observations", "test_times",
                "hindcasts", "forecasts", "site_units", "component")
  expect_equal(sort(names(hc)), sort(required))
  expect_null(hc$forecasts)
  expect_null(hc$test_times)
  expect_null(hc$test_observations)
  expect_identical(dim(hc$hindcasts[["s1"]]), c(3L, 8L))
})


test_that("obs_uncertainty = FALSE returns family mean for response", {
  fit <- make_hindcast_mock()
  fit$family$linkinv <- function(eta) eta + 100
  draws <- make_hindcast_draws(ndraws = 2L)
  testthat::local_mocked_bindings(
    `as_draws_matrix` = function(...) draws,
    ndraws = function(...) nrow(draws),
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
      stop("draw_observations ran under obs_uncertainty = FALSE")
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
    ndraws = function(...) nrow(draws),
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
    ndraws = function(...) nrow(draws),
    .package = "posterior"
  )
  expect_error(
    hindcast(fit, type = "response", ndraws = 50L),
    "more draws than the posterior has"
  )
})


test_that("ndraws subset yields the requested rows", {
  fit <- make_hindcast_mock()
  draws <- make_hindcast_draws(ndraws = 5L)
  testthat::local_mocked_bindings(
    `as_draws_matrix` = function(...) draws,
    ndraws = function(...) nrow(draws),
    .package = "posterior"
  )
  # Each extraction returns the rows it is asked for
  testthat::local_mocked_bindings(
    extract_trend_latent_states = function(mvgam_fit, newdata,
                                           full_draws, ...) {
      matrix(0, nrow = nrow(full_draws), ncol = nrow(newdata))
    },
    extract_component_linpred = function(mvgam_fit, newdata, component,
                                         resp = NULL, draw_ids = NULL,
                                         ...) {
      matrix(0, nrow = length(draw_ids %||% 1:5), ncol = nrow(newdata))
    },
    draw_observations = function(object, linpred, newdata, draw_ids,
                                 resp = NULL) {
      linpred
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
    ndraws = function(...) nrow(draws),
    .package = "posterior"
  )
  testthat::local_mocked_bindings(
    extract_trend_latent_states = function(mvgam_fit, newdata,
                                             full_draws, ...) {
      matrix(0, nrow = 2L, ncol = nrow(newdata))
    },
    extract_component_linpred = function(mvgam_fit, newdata,
                                           component, ...) {
      matrix(0, nrow = 2L, ncol = nrow(newdata))
    },
    # Each cell is keyed to the series and the time of the row it
    # was built from. The two series have different keys.
    draw_observations = function(object, linpred, newdata, draw_ids,
                                 resp = NULL) {
      key <- 100L * match(as.character(newdata$series), c("a", "b")) +
        as.integer(newdata$time)
      matrix(rep(key, each = nrow(linpred)),
             nrow = nrow(linpred), ncol = nrow(newdata))
    }
  )
  hc <- hindcast(fit, type = "response")
  expect_named(hc$hindcasts, c("a", "b"))
  expect_identical(hc$hindcasts[["a"]],
                   matrix(rep(100L + 1:8, each = 2L), nrow = 2L))
  expect_identical(hc$hindcasts[["b"]],
                   matrix(rep(200L + 1:8, each = 2L), nrow = 2L))
  expect_equal(hc$train_times[["a"]], 1:8)
  expect_equal(hc$train_times[["b"]], 1:8)
})

