# LOCAL ONLY: end-to-end Stan-fit coverage for the empty-obs-
# formula workaround in R/brms_integration.R. Helper-level tests
# (no fit) live in tests/testthat/test-empty-obs-formula.R and
# run on every CI sweep; this file fits real Stan models, so it
# stays out of CI per the package's "no Stan fits in tests/testthat/"
# rule. Run interactively with:
#   testthat::test_file("tests/local/test-empty-obs-formula.R")

library(testthat)
library(mvgam)


test_that("mvgam fits `y ~ 0` and hides the placeholder downstream", {
  set.seed(1L)
  simdat <- sim_mvgam(family = poisson(), n_series = 2L,
                       n_timepoints = 30L)
  # `threads = 2L` here intentionally exercises the brms-native +
  # `trend_formula` threading gate (issue #411 / #412): mvgam must
  # warn once via class `mvgam_threads_trend_brms_native`, force the
  # downstream brms call to `threads = 1L`, and complete a serial
  # fit. The warning class is suppressed under TESTTHAT, so this
  # block only checks that the fit completes; the stancode-level
  # warning assertion lives at tests/testthat/test-stancode-standata.R.
  mod <- mvgam(
    formula       = y ~ 0,
    trend_formula = ~ AR(p = 1),
    family        = poisson(),
    data          = simdat$data_train,
    silent        = 2,
    chains        = 1,
    samples       = 50,
    burnin        = 100,
    threads       = 2L
  )
  expect_s3_class(mod, "mvgam")
  expect_false(any(grepl(
    mvgam:::MVGAM_EMPTY_OBS_PLACEHOLDER,
    variables(mod),
    fixed = TRUE
  )))
  expect_false(any(grepl(
    mvgam:::MVGAM_EMPTY_OBS_PLACEHOLDER,
    mod$prior$coef,
    fixed = TRUE
  )))
  summary_lines <- capture.output(summary(mod, include_betas = FALSE))
  expect_false(any(grepl(
    mvgam:::MVGAM_EMPTY_OBS_PLACEHOLDER,
    summary_lines,
    fixed = TRUE
  )))
})


test_that("y ~ 0 fit produces calibrated residuals and hindcasts", {
  # Regression guard for the `extract_linpred_univariate` intercept-
  # column drop bug: the helper used to unconditionally drop X[, 1]
  # whenever it was all-1s, on the assumption that brms always reserves
  # column 1 for `b_Intercept`. For `y ~ 0 + <regressor>` formulas
  # (including the `.mvgam_empty_obs` placeholder injected here) there
  # is no `b_Intercept`; the lone all-1s column IS the regressor with
  # coefficient `b[1]`. Dropping it silently zeroed the obs linpred,
  # cascading a factor-of-`exp(b[1])` bias into residuals / hindcast /
  # forecast / predict_*. Calibrated residuals (median ~ 0) and
  # hindcast/obs ratios near 1 collectively guard the whole stack.
  set.seed(3L)
  simdat <- sim_mvgam(family = poisson(), n_series = 3L,
                       n_timepoints = 50L)
  mod <- mvgam(
    formula       = y ~ 0,
    trend_formula = ~ AR(p = 1),
    family        = poisson(),
    data          = simdat$data_train,
    silent        = 2,
    chains        = 2,
    samples       = 250,
    burnin        = 500
  )
  expect_s3_class(mod, "mvgam")
  dat <- mod$data
  # Residual medians per series should sit close to zero.
  r <- residuals(mod, type = "quantile", ndraws = 100, summary = TRUE)
  r_med <- r[, "Estimate"]
  for (sp in levels(dat$series)) {
    ok <- dat$series == sp & !is.na(dat$y)
    expect_lt(abs(median(r_med[ok])), 0.5,
                label = paste0("median DS residual for series ", sp))
  }
  # Per-series hindcast expected value should match observed mean.
  # If `b[1]` were dropped, ratios would land near `exp(-b[1])`.
  hc <- hindcast(mod, type = "response")
  for (s in seq_along(levels(dat$series))) {
    sp <- levels(dat$series)[s]
    ok <- dat$series == sp & !is.na(dat$y)
    mean_obs <- mean(dat$y[ok])
    not_na <- !is.na(dat$y[dat$series == sp])
    mean_hc <- mean(apply(hc$hindcasts[[s]][, not_na], 2L, mean))
    expect_lt(abs(mean_hc / mean_obs - 1), 0.25,
                label = paste0("hindcast/obs ratio for series ", sp))
  }
})


test_that("non-empty obs formulas are not touched by the injection", {
  set.seed(2L)
  simdat <- sim_mvgam(family = poisson(), n_series = 2L,
                       n_timepoints = 30L)
  mod <- mvgam(
    formula       = y ~ x,
    trend_formula = ~ AR(p = 1),
    family        = poisson(),
    data          = simdat$data_train,
    silent        = 2,
    chains        = 1,
    samples       = 50,
    burnin        = 100
  )
  expect_false(any(grepl(
    mvgam:::MVGAM_EMPTY_OBS_PLACEHOLDER,
    mod$prior$coef,
    fixed = TRUE
  )))
  expect_false(any(grepl(
    mvgam:::MVGAM_EMPTY_OBS_PLACEHOLDER,
    variables(mod),
    fixed = TRUE
  )))
})
