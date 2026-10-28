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
  # `refresh = 0` silences the iteration lines. The warmup draws reach
  # the stanfit and the draws methods.
  out <- capture.output(
    fit <- suppressMessages(fit_ar(refresh = 0, save_warmup = TRUE))
  )
  expect_false(any(grepl("Iteration", out)))
  expect_identical(as.integer(fit$fit@sim$warmup2), c(150L, 150L))
  expect_identical(nrow(as_draws_df(fit, inc_warmup = TRUE)), 600L)
  expect_identical(nrow(as_draws_df(fit)), 300L)
})


test_that("sampler arguments reach rstan", {
  # rstan's own convergence checks warn on this model below 2000
  # iterations.
  out <- capture.output(
    fit <- suppressMessages(fit_ar(backend = "rstan", iter = 2000L,
                                   refresh = 0, save_warmup = TRUE))
  )
  expect_false(any(grepl("Iteration", out)))
  expect_identical(as.integer(fit$fit@sim$warmup2), c(1000L, 1000L))
})


test_that("an argument the sampler lacks is refused by name", {
  expect_error(fit_ar(silent = 2L, junk_arg = 1),
               "hands the work to: 'junk_arg'")
})


test_that("every cmdstanr algorithm returns a posterior mvgam can use", {
  # cmdstanr 0.9.0 writes a `path__` column for Pathfinder, and brms
  # 2.23.0 does not know it. It stopped the conversion to a stanfit,
  # and once past that, it shifted every parameter name after it.
  for (algorithm in c("pathfinder", "laplace", "meanfield", "fullrank")) {
    # Laplace finds its mode with `$optimize()`, which printed its log
    # under `silent = 2`.
    out <- capture.output(
      fit <- fit_ar(algorithm = algorithm, silent = 2L, refresh = 0)
    )
    expect_identical(out, character(0))
    # The stanfit's `@sim` holds no algorithm, and the fit records
    # the one `mvgam()` ran. `update()` inherits the slot, and a
    # wrong record would refit an approximation with NUTS.
    expect_identical(fit$algorithm, algorithm)
    vars <- posterior::variables(posterior::as_draws(fit$fit))
    expect_identical(anyDuplicated(vars), 0L)
    expect_false("path__" %in% vars)
    expect_true(all(c("b_Intercept", "ar1_trend[1]", "sigma_trend[1]") %in%
                      vars))
    fc <- forecast(fit, newdata = newdata, ndraws = 20L)
    expect_true(all(is.finite(unlist(fc$forecasts))))
    # brms records arguments the algorithm never takes as zero-length
    # values. The summary printed an empty `iter` and named NUTS.
    printed <- capture.output(print(summary(fit)))
    expect_true(any(grepl(
      paste0("using ", mvgam:::sampler_label(algorithm), "."), printed,
      fixed = TRUE
    )))
    expect_match(methods_md(fit), "Posterior drawn via", fixed = TRUE)
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
  # A trend formula without terms leaves `mu_trend` at zero. The
  # program declares none. Every stored R-hat is then defined.
  fit <- suppressMessages(fit_ar(refresh = 0, silent = 2L))
  stored <- posterior::variables(posterior::as_draws(fit$fit))
  expect_false(any(grepl("^mu_trend\\[", stored)))
  expect_false(anyNA(rhat(fit)))
})


test_that("a pooled fit reports convergence per imputation", {
  # The pooled chains sample two posteriors. Each parameter's R-hat is
  # its largest over the imputation fits and its ESS their sum.
  imputed <- lapply(1:2, function(i) {
    d <- dat
    d$y[c(5L, 45L)] <- c(i, i + 2L)
    d
  })
  pooled <- suppressMessages(mvgam_multiple(
    y ~ 1, trend_formula = ~ AR(), data_list = imputed,
    family = poisson(), chains = 2L, iter = 300L, seed = 1L,
    refresh = 0, silent = 2L
  ))
  expect_s3_class(pooled, c("mvgam_pooled", "mvgam"))
  fits <- attr(pooled, "individual_fits")
  expect_identical(posterior::ndraws(pooled),
                   2L * posterior::ndraws(fits[[1L]]))
  summ <- summary(pooled)
  per_fit <- lapply(fits, function(fit) summary(fit)$fixed["Intercept", ])
  expect_equal(summ$fixed["Intercept", "Rhat"],
               max(vapply(per_fit, `[[`, numeric(1L), "Rhat")))
  expect_equal(summ$fixed["Intercept", "Bulk_ESS"],
               sum(vapply(per_fit, `[[`, numeric(1L), "Bulk_ESS")))
  printed <- capture.output(print(summ))
  expect_true(any(grepl("Multiple imputation: 2 fits", printed)))
  expect_true(any(grepl("Data: imputed", printed)))
})
