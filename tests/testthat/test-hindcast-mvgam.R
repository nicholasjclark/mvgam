# CI tests for `hindcast.mvgam()` and the `mvgam_forecast` methods on
# mock fits: the object's slots, the dispatch by `type`, the
# `obs_uncertainty` toggle, draw selection and multivariate forecasts.


# A mock fit holding the slots `hindcast()` uses
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
      ar_lags = 1L,
      ma_lags = integer(0L),
      max_lag = 1L,
      has_cor = FALSE
    )),
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
                "hindcasts", "forecasts", "site_units")
  expect_equal(sort(names(hc)), sort(required))
  expect_null(hc$forecasts)
  expect_null(hc$test_times)
  expect_null(hc$test_observations)
  expect_identical(dim(hc$hindcasts[["s1"]]), c(3L, 8L))
})


test_that("hindcast.mvgam type dispatch flows through expected helpers", {
  fit <- make_hindcast_mock()
  fit$family$linkinv <- function(eta) eta + 100  # tag the linkinv call
  draws <- make_hindcast_draws(ndraws = 2L)
  testthat::local_mocked_bindings(
    `as_draws_matrix` = function(...) draws,
    ndraws = function(...) nrow(draws),
    .package = "posterior"
  )
  # Standard families: trend pulled per-draw from stanfit via
  # extract_trend_latent_states; obs linpred via
  # extract_component_linpred(component = 'obs'); response sampled
  # via draw_observations. Each helper returns its own constant,
  # which identifies the helper that ran.
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

  # `process_error = TRUE` takes the marginal draws. The trend is the
  # marginal linear predictor less the observation predictor.
  marginal <- function(value) {
    function(object, newdata, draw_ids, process_error, resp) {
      stopifnot(isTRUE(process_error))
      matrix(value, nrow = length(draw_ids), ncol = nrow(newdata))
    }
  }
  testthat::local_mocked_bindings(
    posterior_linpred = marginal(40),
    posterior_epred = marginal(50),
    posterior_predict = marginal(60)
  )
  pe <- function(type) {
    hindcast(fit, type = type, process_error = TRUE)$hindcasts[["s1"]]
  }
  expect_true(all(pe("trend") == 40 - 3))
  expect_true(all(pe("link") == 40))
  expect_true(all(pe("expected") == 50))
  expect_true(all(pe("response") == 60))
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


test_that("every forecast method takes a multivariate forecast", {
  # A multivariate fit's hindcast holds one `mvgam_forecast` per
  # response. On a long frame each element spans the series. On a
  # wide frame each element is one response's series.
  make_part <- function(type, series = "s1", shift = 0) {
    structure(list(
      family = "gaussian", family_pars = NULL, type = type,
      series_names = factor(series),
      train_observations = stats::setNames(list(seq_len(10L)), series),
      train_times = stats::setNames(list(seq_len(10L)), series),
      test_observations = NULL, test_times = NULL,
      hindcasts = stats::setNames(
        list(matrix(seq_len(30L) + shift, nrow = 3L)), series
      ),
      forecasts = NULL
    ), class = "mvgam_forecast")
  }
  wrap <- function(parts, keyed = FALSE) {
    structure(parts, class = "mvgam_forecast", mv_wrapper = TRUE,
              response_keyed = keyed)
  }
  resps <- c("count", "pa")

  # A trend-scale forecast repeats one state per response. It is drawn
  # once, without observations.
  trend <- wrap(list(count = make_part("trend"), pa = make_part("trend")))
  layers <- plot(trend)$layers
  expect_false(any(vapply(layers, function(l) inherits(l$geom, "GeomPoint"),
                          logical(1L))))

  long <- wrap(list(count = make_part("response"),
                    pa = make_part("response", shift = 100)))
  expect_s3_class(plot(long), "patchwork")
  expect_output(print(long), "Response 'pa'")
  sm <- summary(long)
  expect_identical(sm$resp, rep(resps, each = 10L))
  expect_equal(sm$truth, rep(1:10, 2L))
  # Both responses of an ensemble take the same draw rows
  ens <- ensemble(long, long, ndraws = 6L, seed = 1L)
  expect_identical(names(ens), resps)
  expect_equal(ens$pa$hindcasts$s1 - 100, ens$count$hindcasts$s1)
  expect_error(ensemble(long, make_part("response")),
               "same responses")

  # A wide forecast becomes one forecast whose series are the responses
  wide <- wrap(list(count = make_part("response", "count"),
                    pa = make_part("response", "pa", shift = 100)),
               keyed = TRUE)
  expect_identical(levels(summary(wide)$series), resps)
  expect_length(grep("family:", capture.output(print(wide))), 1L)
  ens_wide <- ensemble(wide, wide, ndraws = 6L, seed = 1L)
  expect_identical(as.character(ens_wide$series_names), resps)
  expect_equal(ens_wide$hindcasts$pa - 100, ens_wide$hindcasts$count)
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
                                             full_draws,
                                             resp = NULL) {
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

