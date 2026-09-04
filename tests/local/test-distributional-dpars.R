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


# The two fits are built once at file level so the blocks below can
# be held against the same posterior rather than against a fresh one
# each time. Both carry a covariate on the mean and another on the
# scale, and they differ in how many responses they hold, which is
# what the model-averaging refusal below turns on.

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

set.seed(3102)
n_mv <- 60L
z_mv <- factor(rep(c("a", "b"), n_mv / 2L))
df_mv <- data.frame(
  y1 = rnorm(n_mv, 0, ifelse(z_mv == "a", 0.5, 2)),
  y2 = rnorm(n_mv, 1, 1),
  x = rnorm(n_mv), z = z_mv,
  series = factor("s1"), time = seq_len(n_mv)
)
fit_mv <- SM(SW(mvgam(
  bf(mvbind(y1, y2) ~ x, sigma ~ z), data = df_mv, family = gaussian(),
  chains = 2L, samples = 400L, burnin = 400L, silent = 2L,
  seed = 3102L, backend = "cmdstanr"
)))


test_that("a gaussian sigma sub-formula resolves on its own scale", {
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
  n_draws <- ndraws(fit_mv)
  draws <- posterior::as_draws_df(fit_mv)
  for (resp in c("y1", "y2")) {
    expect_equal(dim(posterior_epred(fit_mv, resp = resp)),
                 c(n_draws, n_mv))
    expect_equal(dim(log_lik(fit_mv, resp = resp)), c(n_draws, n_mv))
    expect_equal(dim(posterior_predict(fit_mv, resp = resp)),
                 c(n_draws, n_mv))

    # Each arm carries its own coefficients, named after the response,
    # and the resolved parameter has to follow the right ones.
    sigma <- mvgam:::resolve_family_pars(
      fit_mv, dpar_names = "sigma", ndraws = n_draws, nobs = n_mv,
      resp = resp
    )$sigma
    icept <- draws[[paste0("b_sigma_", resp, "_Intercept")]]
    slope <- draws[[paste0("b_sigma_", resp, "_zb")]]
    expect_true(all(sigma > 0))
    expect_equal(mean(sigma[, df_mv$z == "a"]), mean(exp(icept)),
                 tolerance = 1e-8)
    expect_equal(mean(sigma[, df_mv$z == "b"]),
                 mean(exp(icept + slope)), tolerance = 1e-8)
  }
})


# -- The leave-one-out prediction methods ------------------------------
#
# These need a posterior PSIS can reweight, which is what the fit
# above supplies: it carries no latent trend, so dropping one
# observation does not move a state the rest are scored against.

test_that("this fit is one PSIS can actually reweight", {
  # The premise the two blocks below rest on, and the contrast that
  # makes them worth having: the trend fits elsewhere in this suite
  # put a fifth to two thirds of their observations past the Pareto
  # threshold, because dropping one moves the state it is scored
  # against. This fit carries no trend, so the diagnostic has to be
  # clean. Nothing is suppressed: a `loo()` that warns here is a
  # warning the suite should report rather than swallow.
  ic <- loo(fit)
  expect_true(is.finite(ic$estimates["elpd_loo", "Estimate"]))
  expect_true(all(ic$diagnostics$pareto_k < 0.7))
})


test_that("loo_epred and loo_linpred agree under an identity link", {
  # The gaussian mean link is the identity, so the expectation and
  # the linear predictor are the same numbers and agreeing is the
  # contract. The complementary claim lives with the log-linked fit
  # in test-draw-alignment.R: a method that skipped the inverse link
  # entirely passes here and returns the wrong scale there.
  e <- loo_epred(fit, type = "mean")
  l <- loo_linpred(fit, type = "mean")
  expect_identical(dim(e), c(nrow(df), 1L))
  expect_identical(dim(l), dim(e))
  expect_true(all(is.finite(e)))
  expect_equal(as.numeric(e), as.numeric(l), tolerance = 1e-8)

  # A leave-one-out expectation is a reweighting of the posterior
  # one, so it tracks it without reproducing it. Equality would mean
  # no weights were applied. With the diagnostic clean above, the
  # weights are mild and the two stay close.
  ep <- colMeans(posterior_epred(fit))
  expect_gt(stats::cor(as.numeric(e), ep), 0.5)
  expect_false(isTRUE(all.equal(as.numeric(e), unname(ep))))
})


test_that("loo_predictive_interval brackets the data it left out", {
  # An interval is read for its coverage, so that is what it is held
  # to here rather than to another package's column names. The
  # nominal 90 per cent has to be approached without the band
  # collapsing or spanning everything.
  pi <- loo_predictive_interval(fit, prob = 0.9)
  expect_identical(dim(pi), c(nrow(df), 2L))
  expect_identical(colnames(pi), c("q5", "q95"))
  expect_true(all(pi[, 2L] >= pi[, 1L]))
  expect_true(all(is.finite(pi)))

  covered <- mean(df$y >= pi[, 1L] & df$y <= pi[, 2L])
  expect_gt(covered, 0.75)
  # Coverage alone is satisfied by a band wide enough to hold
  # anything, so the width is bounded too. The bound rules out the
  # degenerate band rather than calibrating the interval.
  expect_lt(mean(pi[, 2L] - pi[, 1L]), 8 * stats::sd(df$y))

  # A narrower request nests inside the wider one.
  tight <- loo_predictive_interval(fit, prob = 0.5)
  expect_true(all(tight[, 1L] >= pi[, 1L]))
  expect_true(all(tight[, 2L] <= pi[, 2L]))

  # The band is wider where the scale sub-formula says the noise is
  # larger, which is what ties this surface to the parameter the
  # rest of the file is about.
  w <- pi[, 2L] - pi[, 1L]
  expect_gt(mean(w[df$z == "b"]), mean(w[df$z == "a"]))
})


test_that("pp_average refuses two fits of different responses", {
  # Averaging predictive distributions is only defined when the
  # models predict the same thing. These two do not: one holds `y`
  # and the other holds `y1` and `y2`.
  expect_error(
    pp_average(fit, fit_mv, weights = c(0.5, 0.5)),
    "same response"
  )
})
