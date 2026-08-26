# Fitting tests for distributional sub-formulas across families and
# across the multivariate path. A parameter given a formula of its own
# is a linear predictor rather than a sampled scalar, and every
# post-fit surface has to rebuild it from its coefficients and put it
# back on its own scale.
#
# The scale is the part worth guarding. A family built by
# `stats::gaussian()` records no link for `sigma`, so reading the
# absence as an identity hands back the parameter still on the log
# scale: a standard deviation of -0.8 where 0.45 belongs, with nothing
# in the output to show for it.
#
# Run with:
#   Rscript -e "devtools::load_all('.'); testthat::test_file('tests/local/test-distributional-dpars.R')"

source("setup_tests_local.R")


test_that("a gaussian sigma sub-formula resolves on its own scale", {
  set.seed(3101)
  n_t <- 80L
  z <- factor(rep(c("a", "b"), each = n_t / 2L))
  df <- data.frame(
    y = rnorm(n_t, 0, ifelse(z == "a", 0.5, 2)),
    x = rnorm(n_t), z = z,
    series = factor("s1"), time = seq_len(n_t)
  )
  fit <- SM(mvgam(
    bf(y ~ x, sigma ~ z), data = df, family = gaussian(),
    chains = 2L, samples = 500L, burnin = 500L, silent = 2L,
    seed = 3101L, backend = "cmdstanr"
  ))

  n_draws <- ndraws(fit)
  expect_equal(dim(posterior_epred(fit)), c(n_draws, n_t))
  expect_equal(dim(log_lik(fit)), c(n_draws, n_t))
  expect_equal(dim(posterior_predict(fit)), c(n_draws, n_t))
  expect_s3_class(SW(loo(fit)), "psis_loo")

  sigma <- mvgam:::resolve_family_pars(
    fit, dpar_names = "sigma", ndraws = n_draws, nobs = n_t
  )$sigma
  draws <- posterior::as_draws_df(fit)
  # sigma is log-linked, so the resolved value is the exponentiated
  # predictor and is necessarily positive.
  expect_true(all(sigma > 0))
  expect_equal(mean(sigma[, df$z == "a"]),
               mean(exp(draws$b_sigma_Intercept)), tolerance = 1e-8)
  expect_equal(mean(sigma[, df$z == "b"]),
               mean(exp(draws$b_sigma_Intercept + draws$b_sigma_zb)),
               tolerance = 1e-8)
  # The two groups were simulated four-fold apart in spread.
  expect_true(mean(sigma[, df$z == "b"]) > mean(sigma[, df$z == "a"]))
})


test_that("a multivariate fit resolves each response's sub-formula", {
  set.seed(3102)
  n_t <- 60L
  z <- factor(rep(c("a", "b"), n_t / 2L))
  df <- data.frame(
    y1 = rnorm(n_t, 0, ifelse(z == "a", 0.5, 2)),
    y2 = rnorm(n_t, 1, 1),
    x = rnorm(n_t), z = z,
    series = factor("s1"), time = seq_len(n_t)
  )
  fit <- SM(SW(mvgam(
    bf(mvbind(y1, y2) ~ x, sigma ~ z), data = df, family = gaussian(),
    chains = 2L, samples = 400L, burnin = 400L, silent = 2L,
    seed = 3102L, backend = "cmdstanr"
  )))

  n_draws <- ndraws(fit)
  draws <- posterior::as_draws_df(fit)
  for (resp in c("y1", "y2")) {
    expect_equal(dim(posterior_epred(fit, resp = resp)), c(n_draws, n_t))
    expect_equal(dim(log_lik(fit, resp = resp)), c(n_draws, n_t))
    expect_equal(dim(posterior_predict(fit, resp = resp)), c(n_draws, n_t))

    # Each arm carries its own coefficients, named after the response,
    # and the resolved parameter has to follow the right ones.
    sigma <- mvgam:::resolve_family_pars(
      fit, dpar_names = "sigma", ndraws = n_draws, nobs = n_t,
      resp = resp
    )$sigma
    icept <- draws[[paste0("b_sigma_", resp, "_Intercept")]]
    slope <- draws[[paste0("b_sigma_", resp, "_zb")]]
    expect_true(all(sigma > 0))
    expect_equal(mean(sigma[, df$z == "a"]), mean(exp(icept)),
                 tolerance = 1e-8)
    expect_equal(mean(sigma[, df$z == "b"]), mean(exp(icept + slope)),
                 tolerance = 1e-8)
  }
})
