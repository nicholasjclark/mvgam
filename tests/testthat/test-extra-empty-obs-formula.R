# The tests of test-empty-obs-formula.R that assemble a model or run for
# 0.2 s or more. CRAN skips this file. CI and local runs take it.
skip_on_cran()


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
                family = poisson(), run_model = FALSE, validate = FALSE)
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
    run_model = FALSE, validate = FALSE
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
