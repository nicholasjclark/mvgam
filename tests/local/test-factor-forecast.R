# End-to-end forecast tests on latent-factor fits with
# posterior-sampled Z (free-loadings branch of
# `resolve_Z_loadings()`). The fixed-Z branch is exercised in
# `test-trend-map-fit.R`. Together they cover both sources of Z
# that `forecast.mvgam()` reads in factor mode.
#
# `val_mvgam_lv_factor.rds` is a 4-series, 2-factor AR(1) fit
# where the loadings matrix Z is sampled unconstrained
# (`trend_map = NULL`, n_lv = 2), so the resolver takes the
# `Z[s, k]` posterior branch. Every draw carries its own Z
# slice, and the projected forecast trajectory rides that draw's
# latent path.


CACHE_DIR <- "fixtures"
fit <- readRDS(file.path(CACHE_DIR, "val_mvgam_lv_factor.rds"))


test_that("fixture is a free-Z factor fit (sanity checks)", {
  # Guard against fixture drift: the tests below rely on the
  # posterior sampling Z, not fixing it.
  expect_equal(fit$standata$N_lv_trend, 2L)
  expect_equal(fit$standata$N_series_trend, 4L)
  expect_true(is.null(fit$mv_spec$trend_specs$fixed_Z))
  expect_true(any(grepl("^Z(_tilde)?\\[", variables(fit))))
})


test_that("forecast on free-Z factor fit returns the right shape", {
  n_series <- fit$standata$N_series_trend
  n_time <- fit$standata$N_time_trend
  h <- 8L
  series_levels <- levels(fit$data$series)
  newdat <- data.frame(
    time = rep((n_time + 1L):(n_time + h), n_series),
    series = factor(
      rep(series_levels, each = h),
      levels = series_levels
    )
  )
  # Match the fitted response's dtype so newdata assembly does
  # not coerce the column into a non-NA sentinel.
  resp <- fit$response_names[1L] %||% "y"
  newdat[[resp]] <- NA_real_
  fc <- forecast(fit, newdata = newdat, ndraws = 30L)
  expect_s3_class(fc, "mvgam_forecast")
  for (s in series_levels) {
    expect_equal(dim(fc$forecasts[[s]]), c(30L, h))
  }
})


test_that("forecast on free-Z factor fit gives finite draws", {
  # Every posterior draw should produce a finite [h, n_series]
  # trajectory once Z projection applies; NAs would indicate
  # either a missing Z column at some draw or a downstream
  # kernel producing non-finite values.
  n_series <- fit$standata$N_series_trend
  n_time <- fit$standata$N_time_trend
  h <- 6L
  series_levels <- levels(fit$data$series)
  newdat <- data.frame(
    time = rep((n_time + 1L):(n_time + h), n_series),
    series = factor(rep(series_levels, each = h),
                     levels = series_levels)
  )
  resp <- fit$response_names[1L] %||% "y"
  newdat[[resp]] <- NA_real_
  fc <- forecast(fit, newdata = newdat, ndraws = 30L,
                  type = "link")
  for (s in series_levels) {
    draws <- fc$forecasts[[s]]
    expect_true(all(is.finite(draws)))
  }
})


test_that("free-Z factor forecast draws respect Z per draw", {
  # For a free-Z fit, per-draw variability in the loadings
  # `Z[s, k]` should show up in the forecast: two series with
  # non-collinear rows of Z should NOT produce identical
  # forecast draws (unlike the fixed-Z case where identical
  # rows collapse to identical draws). This catches a
  # regression where the projection accidentally applies the
  # same Z row across all series -- a silent bug that would
  # only surface here.
  n_series <- fit$standata$N_series_trend
  n_time <- fit$standata$N_time_trend
  h <- 6L
  series_levels <- levels(fit$data$series)
  newdat <- data.frame(
    time = rep((n_time + 1L):(n_time + h), n_series),
    series = factor(rep(series_levels, each = h),
                     levels = series_levels)
  )
  resp <- fit$response_names[1L] %||% "y"
  newdat[[resp]] <- NA_real_
  fc <- forecast(fit, newdata = newdat, ndraws = 30L,
                  type = "link")
  # Compare series 1 to each of series 2, 3, 4; at least one
  # must differ per-draw. The Heaps identification pins the
  # upper-triangular block of Z so series 1 = trend 1 up to a
  # scalar; other series load on both factors and are unlikely
  # to collapse to the same draws.
  diffs_any <- vapply(2:n_series, function(j) {
    !isTRUE(all.equal(fc$forecasts[[1L]], fc$forecasts[[j]]))
  }, logical(1L))
  expect_true(any(diffs_any))
})
