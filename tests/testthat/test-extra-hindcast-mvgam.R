# The tests of test-hindcast-mvgam.R that assemble a model or run for
# 0.2 s or more. CRAN skips this file. CI and local runs take it.
skip_on_cran()


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
