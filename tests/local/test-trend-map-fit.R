# End-to-end fit-time tests for `trend_map` (fixed-Z factor
# models). Reads the cached `val_mvgam_trend_map_fx.rds` fit
# produced by tests/local/build_fixtures.R. The cached fit is a
# 4-series, 2-factor AR(1) model with a dense user-supplied Z.
#
# Step 3 contract checks (Stan emission + standata round-trip)
# live here today. Step 4 introduces the downstream resolver
# (`resolve_factor_loadings`) that lets posterior_predict /
# residuals / residual_cor / extract_factors run on a fixed-Z
# fit; the downstream-method assertions below the `xxx` divider
# are intentionally pending until that resolver lands and should
# be re-enabled in the same commit.

CACHE_DIR <- "fixtures"
fit <- readRDS(file.path(CACHE_DIR, "val_mvgam_trend_map_fx.rds"))
Z_true <- attr(fit$data, "Z_true") %||% attr(fit$obs_data, "Z_true")


test_that("standata carries the fixed Z matrix verbatim", {
  sd <- fit$standata
  expect_true("Z" %in% names(sd))
  expect_equal(dim(sd$Z), dim(Z_true))
  expect_equal(unname(sd$Z), unname(Z_true))
})


test_that("Stan code declares Z in data; no Z_raw / no prior", {
  code_txt <- as.character(fit$stancode)
  expect_true(grepl(
    "matrix[N_series_trend, N_lv_trend] Z;",
    code_txt, fixed = TRUE
  ))
  expect_false(grepl("Z_raw", code_txt, fixed = TRUE))
})


test_that("fit object structure is intact after fixed-Z path", {
  expect_s3_class(fit, "mvgam")
  expect_true(is.list(fit$trend_metadata))
  expect_true("Z" %in% names(fit$standata))
  expect_equal(fit$standata$N_lv_trend, 2L)
  expect_equal(fit$standata$N_series_trend, 4L)
})


test_that("Z does NOT appear as a posterior parameter", {
  # Sampled-Z fits store Z_raw / Z draws in $fit; fixed-Z fits
  # must NOT — the matrix is data, not a parameter. This catches
  # any future regression where Z slips back into the parameter
  # block.
  param_names <- fit$fit@sim$pars_oi %||% character(0)
  expect_false(any(grepl("Z_raw", param_names)))
})


test_that("trend_metadata persists the fixed Z matrix", {
  expect_false(is.null(fit$trend_metadata$fixed_Z))
  expect_equal(unname(fit$trend_metadata$fixed_Z), unname(Z_true))
  expect_equal(fit$trend_metadata$n_lv, ncol(Z_true))
})


# ---- Downstream methods that route through resolve_factor_loadings ----

test_that("posterior_predict returns the right shape on fixed-Z fit", {
  pp <- posterior_predict(fit, ndraws = 30L)
  expect_true(is.matrix(pp))
  expect_equal(nrow(pp), 30L)
  expect_equal(ncol(pp), nrow(fit$data))
})


test_that("posterior_epred returns the right shape on fixed-Z fit", {
  ep <- posterior_epred(fit, ndraws = 30L)
  expect_true(is.matrix(ep))
  expect_equal(nrow(ep), 30L)
  expect_equal(ncol(ep), nrow(fit$data))
  expect_true(all(is.finite(ep)))
  expect_true(all(ep > 0))
})


test_that("residuals return one column per observation", {
  r <- residuals(fit, summary = FALSE, ndraws = 30L)
  expect_true(is.matrix(r))
  expect_equal(nrow(r), 30L)
  expect_equal(ncol(r), nrow(fit$data))
})


test_that("forecast on fixed-Z factor fit returns the right shape", {
  # `val_mvgam_trend_map_fx.rds` is a 4-series, 2-factor AR(1)
  # with a dense user-supplied Z. Since this test's fit has no
  # persisted test_data, build newdata explicitly so the
  # horizon is fixed for the shape assertion below.
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
  newdat$count <- NA_integer_
  fc <- forecast(fit, newdata = newdat, ndraws = 30L)
  expect_s3_class(fc, "mvgam_forecast")
  for (s in series_levels) {
    expect_equal(dim(fc$forecasts[[s]]), c(30L, h))
  }
})


test_that("forecast on fixed-Z factor gives finite link-scale draws", {
  # Every posterior draw should produce a finite [h, n_series]
  # trajectory once the LV-space AR(1) recursion has been
  # projected via the fixed Z matrix. NAs would indicate
  # either a missing lv_trend column or an infinite draw
  # slipping past the projection.
  n_series <- fit$standata$N_series_trend
  n_time <- fit$standata$N_time_trend
  h <- 6L
  series_levels <- levels(fit$data$series)
  newdat <- data.frame(
    time = rep((n_time + 1L):(n_time + h), n_series),
    series = factor(rep(series_levels, each = h),
                     levels = series_levels)
  )
  newdat$count <- NA_integer_
  fc <- forecast(fit, newdata = newdat, ndraws = 30L,
                  type = "link")
  for (s in series_levels) {
    expect_true(all(is.finite(fc$forecasts[[s]])))
  }
})


test_that("residual_cor returns a residcor object", {
  rc <- residual_cor(fit, ndraws = 30L)
  expect_s3_class(rc, "mvgam_residcor")
})


test_that("posterior recovers different per-series means", {
  ep <- posterior_epred(fit, ndraws = 30L)
  series_vec <- fit$data$series
  series_means <- vapply(
    levels(series_vec),
    function(s) mean(ep[, series_vec == s]),
    numeric(1L)
  )
  expect_true(all(is.finite(series_means)))
  expect_true(all(series_means > 0))
  # Dense Z mixes 2 factors with different per-series weights,
  # so expected counts should not collapse to one value.
  expect_gt(stats::sd(series_means), 1e-6)
})
