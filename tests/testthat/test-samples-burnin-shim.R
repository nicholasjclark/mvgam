# Contract tests for the deprecated `samples` / `burnin` argument
# translation shim exposed by `mvgam()`. The shim is called from
# `mvgam()` before dispatching to inner fitting machinery and lives
# as an internal helper `translate_samples_burnin()` in
# `R/mvgam_core.R`. Prior to the shim both deprecated names
# silently disappeared into `...` and every fit ran with
# cmdstanr's default `iter = 2000, warmup = 1000` regardless of
# what the user asked for.

test_that("translate_samples_burnin returns dots unchanged when neither name is present", {
  dots <- list(iter = 500, warmup = 250, chains = 2)
  out <- mvgam:::translate_samples_burnin(dots)
  expect_identical(out$iter, 500)
  expect_identical(out$warmup, 250)
  expect_identical(out$chains, 2)
  expect_false(attr(out, "translated", exact = TRUE))
})

test_that("translate_samples_burnin maps the pair to iter and warmup", {
  out <- mvgam:::translate_samples_burnin(
    list(samples = 500, burnin = 250, chains = 2)
  )
  expect_identical(out$iter, 750)
  expect_identical(out$warmup, 250)
  expect_null(out$samples)
  expect_null(out$burnin)
  expect_identical(out$chains, 2)
  expect_true(attr(out, "translated", exact = TRUE))
})

test_that("translate_samples_burnin defaults the missing half to 1000", {
  out_s <- mvgam:::translate_samples_burnin(list(samples = 300))
  expect_equal(out_s$iter, 1300)
  expect_equal(out_s$warmup, 1000)

  out_b <- mvgam:::translate_samples_burnin(list(burnin = 400))
  expect_equal(out_b$iter, 1400)
  expect_equal(out_b$warmup, 400)
})

test_that("translate_samples_burnin errors when the deprecated pair is mixed with the brms pair", {
  expect_error(
    mvgam:::translate_samples_burnin(list(samples = 100, iter = 500)),
    "Do not mix 'samples'/'burnin' with 'iter'/'warmup'"
  )
  expect_error(
    mvgam:::translate_samples_burnin(list(burnin = 100, warmup = 200)),
    "Do not mix 'samples'/'burnin' with 'iter'/'warmup'"
  )
  expect_error(
    mvgam:::translate_samples_burnin(list(samples = 100, warmup = 200)),
    "Do not mix 'samples'/'burnin' with 'iter'/'warmup'"
  )
})

test_that("translate_samples_burnin emits a deprecation warning when TESTTHAT is unset", {
  # The shim suppresses the warning when `Sys.getenv('TESTTHAT')
  # == 'true'` so noisy fit tests do not print it repeatedly. Clear
  # that condition to exercise the warning path. `.frequency = "once"`
  # would otherwise stay silent if an earlier call in the suite already
  # emitted the warning, so force verbose warnings to bypass the gate.
  withr::local_options(rlib_warning_verbosity = "verbose")
  withr::local_envvar(TESTTHAT = "")
  expect_warning(
    mvgam:::translate_samples_burnin(list(samples = 100, burnin = 200)),
    "'samples' and 'burnin' are deprecated"
  )
})
