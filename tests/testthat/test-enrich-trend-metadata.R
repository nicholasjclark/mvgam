# Unit tests for the F0 fit-time enrichment helper
# `enrich_trend_metadata()` in `R/trend_propagation.R`. The
# helper derives the kernel-relevant `ar_lags`, `ma_lags`,
# `max_lag`, `has_cor`, `n_lv`, and `trend_type` fields from the
# parsed trend constructor so the forecasting surface can pull
# them straight off `mvgam_fit$trend_metadata` without re-parsing.


# Helper: minimal trend_metadata stub plus the trend spec under
# test. Calling enrich_trend_metadata with these mimics what
# mvgam_core.R does at fit time.
enrich_for <- function(spec, base = list()) {
  enrich_trend_metadata(base, spec)
}


test_that("enrich preserves the input list when trend_specs is NULL", {
  base <- list(foo = "bar", covariates = c("x", "z"))
  out <- enrich_trend_metadata(base, NULL)
  expect_identical(out, base)
})


test_that("enrich returns NULL when trend_metadata is NULL", {
  expect_null(enrich_trend_metadata(NULL, RW()))
})


test_that("RW() enriches to ar_lags = 1, no MA, no correlation", {
  out <- enrich_for(RW())
  expect_identical(out$trend_type, "RW")
  expect_identical(out$ar_lags, 1L)
  expect_identical(out$ma_lags, integer(0))
  expect_identical(out$max_lag, 1L)
  expect_false(out$has_cor)
  expect_null(out$n_lv)
})


test_that("AR(p = 1) enriches to ar_lags = 1", {
  out <- enrich_for(AR(p = 1L))
  expect_identical(out$trend_type, "AR")
  expect_identical(out$ar_lags, 1L)
  expect_identical(out$max_lag, 1L)
})


test_that("AR(p = 3) expands to consecutive lags 1:3", {
  out <- enrich_for(AR(p = 3L))
  expect_identical(out$ar_lags, 1:3)
  expect_identical(out$max_lag, 3L)
})


test_that("AR(p = c(1, 3, 12)) preserves the sparse lag set", {
  out <- enrich_for(AR(p = c(1L, 3L, 12L)))
  expect_identical(out$ar_lags, c(1L, 3L, 12L))
  expect_identical(out$max_lag, 12L)
})


test_that("AR(p = 2, ma = TRUE) gives ma_lags = 1 and right max", {
  out <- enrich_for(AR(p = 2L, ma = TRUE))
  expect_identical(out$ar_lags, 1:2)
  expect_identical(out$ma_lags, 1L)
  expect_identical(out$max_lag, 2L)
})


test_that("AR(p = 1, cor = TRUE) flags has_cor", {
  out <- enrich_for(AR(p = 1L, cor = TRUE))
  expect_true(out$has_cor)
})


test_that("VAR() defaults to ar_lags = 1 and has_cor = TRUE", {
  out <- enrich_for(VAR())
  expect_identical(out$trend_type, "VAR")
  expect_identical(out$ar_lags, 1L)
  expect_true(out$has_cor)
})


test_that("VAR(p = 2, ma = TRUE) captures VARMA(2, 1)", {
  out <- enrich_for(VAR(p = 2L, ma = TRUE))
  expect_identical(out$ar_lags, 1:2)
  expect_identical(out$ma_lags, 1L)
  expect_identical(out$max_lag, 2L)
})


test_that("CAR() gives ar_lags = 1 and no MA", {
  out <- enrich_for(CAR())
  expect_identical(out$trend_type, "CAR")
  expect_identical(out$ar_lags, 1L)
  expect_identical(out$ma_lags, integer(0))
})


test_that("ZMVN() gives empty lag sets (no temporal recursion)", {
  out <- enrich_for(ZMVN())
  expect_identical(out$trend_type, "ZMVN")
  expect_identical(out$ar_lags, integer(0))
  expect_identical(out$ma_lags, integer(0))
  expect_identical(out$max_lag, 0L)
})


test_that("enrich preserves existing fields on trend_metadata", {
  base <- list(covariates = c("x", "z"),
                variables = list(time_var = "time"))
  out <- enrich_trend_metadata(base, AR(p = 1L))
  expect_identical(out$covariates, c("x", "z"))
  expect_identical(out$variables$time_var, "time")
  expect_identical(out$trend_type, "AR")
})


test_that("enrich handles a multivariate trend_specs list", {
  specs <- list(y1 = AR(p = 1L), y2 = AR(p = 1L))
  out <- enrich_for(specs)
  expect_identical(out$trend_type, "AR")
  expect_identical(out$ar_lags, 1L)
})
