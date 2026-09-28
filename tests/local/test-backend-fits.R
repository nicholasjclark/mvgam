# What reaches the sampler, and what comes back from each algorithm.
#
# `mvgam()` passes every argument it does not use itself to the
# sampler, as `brms::brm()` does. Each fit here is small, and every
# cmdstanr case shares one program, which cmdstanr compiles once.


set.seed(2L)
dat <- data.frame(
  y = rpois(80L, 4), time = rep(1:40, 2L),
  series = factor(rep(c("a", "b"), each = 40L))
)
newdata <- data.frame(
  time = rep(41:43, 2L), series = factor(rep(c("a", "b"), each = 3L))
)

fit_ar <- function(..., iter = 300L) {
  mvgam(y ~ 1, trend_formula = ~ AR(), data = dat, family = poisson(),
        chains = 2L, iter = iter, seed = 1L, ...)
}


test_that("sampler arguments reach cmdstanr", {
  # Iteration lines were printed however `refresh` was set, and the
  # warmup draws were dropped whatever `save_warmup` said.
  out <- capture.output(
    fit <- suppressMessages(fit_ar(refresh = 0, save_warmup = TRUE))
  )
  expect_false(any(grepl("Iteration", out)))
  expect_identical(as.integer(fit$fit@sim$warmup2), c(150L, 150L))
})


test_that("sampler arguments reach rstan", {
  # rstan's own convergence checks need more than 300 iterations here.
  out <- capture.output(
    fit <- suppressMessages(fit_ar(backend = "rstan", iter = 1000L,
                                   refresh = 0, save_warmup = TRUE))
  )
  expect_false(any(grepl("Iteration", out)))
  expect_identical(as.integer(fit$fit@sim$warmup2), c(500L, 500L))
})


test_that("an argument the sampler lacks is refused by name", {
  expect_error(fit_ar(silent = 2L, junk_arg = 1),
               "Unknown argument for cmdstanr's '\\$sample\\(\\)': 'junk_arg'")
})


test_that("every cmdstanr algorithm returns a posterior mvgam can use", {
  # cmdstanr 0.9.0 writes a `path__` column for Pathfinder, and brms
  # 2.23.0 does not know it. It stopped the conversion to a stanfit,
  # and once past that, it shifted every parameter name after it.
  for (algorithm in c("pathfinder", "laplace", "meanfield", "fullrank")) {
    fit <- suppressMessages(fit_ar(algorithm = algorithm, silent = 2L,
                                   refresh = 0))
    vars <- posterior::variables(posterior::as_draws(fit$fit))
    expect_identical(anyDuplicated(vars), 0L)
    expect_false("path__" %in% vars)
    expect_true(all(c("b_Intercept", "ar1_trend[1]", "sigma_trend[1]") %in%
                      vars))
    fc <- forecast(fit, newdata = newdata, ndraws = 20L)
    expect_true(all(is.finite(unlist(fc$forecasts))))
  }
})


test_that("a Pathfinder start keeps the sampler's own arguments", {
  # The start receives the list built for `$sample()`. Pathfinder
  # takes no `save_warmup`, and the sampler still has to receive it.
  fit <- suppressMessages(fit_ar(init = "pathfinder", save_warmup = TRUE,
                                 refresh = 0, silent = 2L))
  expect_identical(as.integer(fit$fit@sim$warmup2), c(150L, 150L))
})


test_that("a trend with no design stores no mu_trend", {
  # Every draw of it is zero, and its R-hat is undefined.
  fit <- suppressMessages(fit_ar(refresh = 0, silent = 2L))
  stored <- posterior::variables(posterior::as_draws(fit$fit))
  expect_false(any(grepl("^mu_trend\\[", stored)))
  expect_false(anyNA(rhat(fit)))
})
