# Local fitting tests for `com_binomial()`'s dispersion parameter on
# paths the intercept-only recovery sweep in
# `com_binomial_fitting.R` does not reach: `nu` on its own linear
# predictor, and data whose dispersion sits far outside the default
# prior.
#
# Both were reported as unusable. Giving `nu` a sub-formula moves it
# from a scalar to a design matrix plus an intercept, so mvgam's
# injected class-level prior matched nothing and brms rejected the
# whole prior set. Overriding the prior collided with the same
# injection and raised a duplicate-specification error, which left
# the default scale effectively fixed.
#
# Run with:
#   Rscript -e "devtools::load_all('.'); testthat::test_file('tests/local/com_binomial_dispersion.R')"

source("setup_tests_local.R")


test_that("dispersion accepts its own linear predictor", {
  # Two groups with opposite dispersion give the coefficient a sign
  # and a magnitude to recover. p = 0.5 throughout, so the grouping
  # carries no information about the mean.
  set.seed(2028)
  n_t <- 240L
  trials <- pmax(rpois(n_t, 30L), 1L)
  z <- factor(rep(c("a", "b"), each = n_t / 2L))
  df <- data.frame(
    y      = mvgam:::rcmb_vec(mu = rep(0.5, n_t),
                              nu = ifelse(z == "a", -0.30, 1.50),
                              T = trials),
    trials = trials,
    z      = z,
    series = factor("s1"),
    time   = seq_len(n_t)
  )
  fit <- SM(mvgam(
    bf(y | trials(trials) ~ 1, nu ~ z), data = df,
    family = com_binomial(), chains = 2L, samples = 500L,
    burnin = 500L, silent = 2L, seed = 2028L, backend = "cmdstanr"
  ))
  slope <- as.numeric(as.array(fit, variable = "b_nu_zb"))
  contrast <- 1.50 - (-0.30)
  cat(sprintf("  b_nu_zb 90%% CI = [%.3f, %.3f] (contrast %.2f)\n",
              stats::quantile(slope, 0.05),
              stats::quantile(slope, 0.95), contrast))
  expect_true(stats::quantile(slope, 0.05) > 0)
  expect_true(stats::quantile(slope, 0.05) <= contrast)
  expect_true(stats::quantile(slope, 0.95) >= contrast)
})


test_that("a wider prior reaches strongly under-dispersed data", {
  # Strongly under-dispersed counts sit near nu = 3, two prior
  # standard deviations above the default mean. The override has to
  # change the answer rather than merely be accepted, so both fits
  # use the same data and differ only in the prior.
  set.seed(2029)
  n_t <- 200L
  trials <- pmax(rpois(n_t, 30L), 1L)
  df <- data.frame(
    y      = mvgam:::rcmb_vec(mu = rep(0.5, n_t), nu = rep(3.0, n_t),
                              T = trials),
    trials = trials,
    series = factor("s1"),
    time   = seq_len(n_t)
  )
  fit_one <- function(prior) {
    SM(mvgam(bf(y | trials(trials) ~ 1), data = df,
             family = com_binomial(), prior = prior, chains = 2L,
             samples = 500L, burnin = 500L, silent = 2L,
             seed = 2029L, backend = "cmdstanr"))
  }
  nu_default <- as.numeric(as.array(fit_one(NULL), variable = "nu"))
  nu_wide <- as.numeric(as.array(
    fit_one(brms::prior("normal(1, 5)", class = "nu")), variable = "nu"
  ))
  cat(sprintf("  nu truth 3.00: default median = %.3f, wide = %.3f\n",
              stats::median(nu_default), stats::median(nu_wide)))
  # The override admits the truth ...
  expect_true(stats::quantile(nu_wide, 0.05) <= 3.0)
  expect_true(stats::quantile(nu_wide, 0.95) >= 3.0)
  # ... and pulls away from the default's shrunk estimate.
  expect_true(stats::median(nu_wide) > stats::median(nu_default))
})
