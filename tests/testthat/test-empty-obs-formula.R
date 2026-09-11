# Tests for the empty-obs-formula placeholder in
# R/brms_integration.R. An observation formula with no terms
# (`y ~ 0` / `y ~ -1`) leaves brms no parameter class under a family
# with no distributional parameter, and its prior pipeline stops.
# `inject_obs_zero_placeholder()` adds a column of zeros whose
# coefficient is pinned at zero, and the linear predictor stays the
# one the user wrote. The placeholder must not leak into user-facing
# `variables()`, `prior_summary()`, `get_prior()` or the printed
# `Formula:` line.


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


test_that("inject_obs_zero_placeholder rewrites a brmsformula's main formula", {
  f <- brms::bf(y ~ 0, nl = FALSE)
  out <- mvgam:::inject_obs_zero_placeholder(
    formula = f,
    data    = data.frame(y = 1:3),
    prior   = NULL
  )
  expect_s3_class(out$formula, "brmsformula")
  expect_identical(
    all.vars(out$formula$formula[[3L]]),
    mvgam:::MVGAM_EMPTY_OBS_PLACEHOLDER
  )
  # brms stores `nl` on the main formula. Losing it in the rewrite
  # would read a non-linear formula as a linear one.
  expect_identical(attr(out$formula$formula, "nl"), FALSE)
  expect_true(mvgam:::MVGAM_EMPTY_OBS_PLACEHOLDER %in% names(out$data))
  expect_true(any(out$prior$coef == mvgam:::MVGAM_EMPTY_OBS_PLACEHOLDER))
})


test_that("inject_obs_zero_placeholder rewrites each empty arm of an mvbf", {
  f <- brms::mvbf(brms::bf(y ~ 0), brms::bf(z ~ 1), brms::bf(w ~ -1))
  out <- mvgam:::inject_obs_zero_placeholder(
    formula = f,
    data    = data.frame(y = 1:3, z = 1:3, w = 1:3),
    prior   = NULL
  )
  ph <- mvgam:::MVGAM_EMPTY_OBS_PLACEHOLDER
  named <- vapply(out$formula$forms, function(a) {
    ph %in% all.vars(a$formula)
  }, logical(1L))
  expect_identical(named, c(y = TRUE, z = FALSE, w = TRUE))
  # One pin per rewritten response, each keyed to its own response.
  # brms applies it to that response's coefficient alone.
  pins <- out$prior[out$prior$coef == ph, ]
  expect_setequal(pins$resp, c("y", "w"))
  expect_identical(mvgam:::obs_placeholder_resps(out$formula), c(y = "y", w = "w"))
})


test_that("strip_empty_obs_placeholder cleans the displayed formula", {
  s <- mvgam:::strip_empty_obs_placeholder(
    "captures ~ 0 + .mvgam_empty_obs"
  )
  expect_identical(s, "captures ~ 0")
})


# ---- Pipeline tests (no Stan fit) --------------------------------
# End-to-end Stan-fit coverage lives in
# tests/local/test-empty-obs-formula.R per the package's "no Stan
# fits in tests/testthat/" rule.

empty_obs_frame <- function() {
  data.frame(
    time   = seq_len(20L),
    series = factor(rep("s1", 20L)),
    y      = rpois(20L, lambda = 3)
  )
}


test_that("a `y ~ 0` formula builds under a family with no dpars", {
  # Poisson has no distributional parameter, and `y ~ 0` leaves brms
  # no parameter class to set a default on. `bf(y ~ 0)`, and an `mvbf()`
  # whose every response declines its terms, used to reach brms
  # unrewritten and fail the build with "replacement has 1 row, data
  # has 0".
  for (f in list(y ~ 0, brms::bf(y ~ 0))) {
    pf <- mvgam(f, trend_formula = ~ AR(p = 1), data = empty_obs_frame(),
                family = poisson(), run_model = FALSE)
    sd <- standata(pf)
    expect_identical(sd$K, 1L)
    expect_true(all(sd$X == 0))
    # The pin makes the placeholder's coefficient data, not a
    # parameter the sampler would move.
    expect_match(stancode(pf), "b[1] = 0;", fixed = TRUE)
  }

  d <- empty_obs_frame()
  d$z <- rpois(20L, lambda = 2)
  pf <- mvgam(
    brms::mvbf(brms::bf(y ~ 0), brms::bf(z ~ 0), rescor = FALSE),
    trend_formula = ~ AR(p = 1), data = d, family = poisson(),
    run_model = FALSE
  )
  sd <- standata(pf)
  expect_true(all(sd$X_y == 0) && all(sd$X_z == 0))
  expect_match(stancode(pf), "b_y[1] = 0;", fixed = TRUE)
  expect_match(stancode(pf), "b_z[1] = 0;", fixed = TRUE)
  expect_false(any(prior_summary(pf)$coef ==
                     mvgam:::MVGAM_EMPTY_OBS_PLACEHOLDER))
})


test_that("default_prior on `y ~ 0` names no placeholder", {
  for (f in list(y ~ 0, brms::bf(y ~ 0))) {
    mf <- mvgam_formula(f, trend_formula = ~ AR(p = 1))
    p <- default_prior(mf, data = empty_obs_frame(), family = poisson())
    expect_s3_class(p, "brmsprior")
    # Neither the pin nor the `b` class it was set under: the formula
    # declines every coefficient and has no `b` to set a prior on.
    expect_setequal(p$class, c("sigma_trend", "ar1_trend"))
  }
})
