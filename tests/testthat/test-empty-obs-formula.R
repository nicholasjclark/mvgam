# Tests for the empty-obs-formula workaround in
# R/brms_integration.R. The brms prior pipeline crashes when the
# obs formula has no RHS terms (`y ~ 0` / `y ~ -1`);
# `inject_obs_zero_placeholder()` adds a pinned constant column so
# the linear predictor contribution stays zero and brms's setup
# completes. The placeholder must not leak into user-facing
# `variables()`, `prior_summary()`, or the printed `Formula:` line.


# ---- Helper-level tests (no Stan fit) -----------------------------

test_that("inject_obs_zero_placeholder fires on empty RHS", {
  out <- mvgam:::inject_obs_zero_placeholder(
    formula = y ~ 0,
    data    = data.frame(y = 1:3),
    prior   = NULL
  )
  expect_true(
    mvgam:::MVGAM_EMPTY_OBS_PLACEHOLDER %in% names(out$data)
  )
  expect_true(grepl(
    mvgam:::MVGAM_EMPTY_OBS_PLACEHOLDER,
    deparse(out$formula),
    fixed = TRUE
  ))
  expect_s3_class(out$prior, "brmsprior")
  expect_true(any(out$prior$coef ==
                    mvgam:::MVGAM_EMPTY_OBS_PLACEHOLDER))
})


test_that("inject_obs_zero_placeholder fires on `~ -1` too", {
  out <- mvgam:::inject_obs_zero_placeholder(
    formula = y ~ -1,
    data    = data.frame(y = 1:3),
    prior   = NULL
  )
  expect_true(
    mvgam:::MVGAM_EMPTY_OBS_PLACEHOLDER %in% names(out$data)
  )
})


test_that("inject_obs_zero_placeholder is a no-op on non-empty RHS", {
  for (f in list(y ~ 1, y ~ x, y ~ 0 + x)) {
    out <- mvgam:::inject_obs_zero_placeholder(
      formula = f,
      data    = data.frame(y = 1:3, x = 1:3),
      prior   = NULL
    )
    expect_identical(deparse(out$formula), deparse(f))
    expect_false(
      mvgam:::MVGAM_EMPTY_OBS_PLACEHOLDER %in% names(out$data)
    )
  }
})


test_that("inject_obs_zero_placeholder passes brmsformula objects through", {
  bf <- brms::bf(y ~ 0)
  out <- mvgam:::inject_obs_zero_placeholder(
    formula = bf,
    data    = data.frame(y = 1:3),
    prior   = NULL
  )
  expect_identical(out$formula, bf)
  expect_false(
    mvgam:::MVGAM_EMPTY_OBS_PLACEHOLDER %in% names(out$data)
  )
})


test_that("safe_brms_prior_call catches the empty-frame crash", {
  empty <- mvgam:::safe_brms_prior_call(stop(
    "replacement has 1 row, data has 0"
  ))
  expect_s3_class(empty, "brmsprior")
  expect_identical(nrow(empty), 0L)
})


test_that("safe_brms_prior_call rethrows unrelated errors", {
  expect_error(
    mvgam:::safe_brms_prior_call(stop("unrelated explosion")),
    "unrelated explosion"
  )
})


test_that("strip_empty_obs_placeholder cleans the displayed formula", {
  s <- mvgam:::strip_empty_obs_placeholder(
    "captures ~ 0 + .mvgam_empty_obs"
  )
  expect_identical(s, "captures ~ 0")
})


# ---- Pipeline smoke test (no Stan fit) ----------------------------
# End-to-end Stan-fit coverage lives in
# tests/local/test-empty-obs-formula.R per the package's "no Stan
# fits in tests/testthat/" rule. The check below confirms the
# injection makes `default_prior.mvgam_formula()` complete on a
# `y ~ 0` formula that would otherwise crash brms's prior
# pipeline.

test_that("default_prior on `y ~ 0` returns without crashing", {
  df <- data.frame(
    time   = seq_len(20L),
    series = factor(rep("s1", 20L)),
    y      = rpois(20L, lambda = 3)
  )
  mf <- mvgam_formula(y ~ 0, trend_formula = ~ AR(p = 1))
  expect_no_error(
    p <- default_prior(mf, data = df, family = poisson())
  )
  expect_s3_class(p, "brmsprior")
  # The placeholder pin row is present at the prior-extraction
  # layer (it is the mechanism by which brms's empty-frame crash
  # is avoided); the post-fit filter in extract_prior_from_setup()
  # strips it before the user-facing `mod$prior` slot is set.
})
