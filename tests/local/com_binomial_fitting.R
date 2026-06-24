# Local recovery fixture for the Conway-Maxwell-Binomial family
# (`com_binomial()`). Replicates the dispersion sweep from the
# upstream contributor's `cmb_example/com_binomial_example.r`:
# three fits at nu_true = c(-0.30, 0.50, 1.60) covering super-,
# over-, and under-dispersed regimes. Each fit's posterior 90% CI
# must cover the true nu; loo() must return finite ELPD with all
# Pareto-k below 1; post-fit dispatchers must return the expected
# shapes.
#
# Moved out of tests/testthat/ because each block compiles a Stan
# model and runs short HMC chains; that is too expensive for the
# CI test suite cap.
#
# Run with:
#   Rscript -e "devtools::load_all('.'); testthat::test_file('tests/local/com_binomial_fitting.R')"

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(testthat)
})

# Cache the three fits at file scope so each test_that block can
# reuse them without re-compiling. Each fit runs at the
# contributor's stress-test corner (p = 0.5, symmetric mass) so the
# tests confirm nu is identified even where the data carry the
# least information about dispersion direction.
.cmb_fits <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    set.seed(2026)
    n_t <- 200L
    trials <- pmax(rpois(n_t, 30L), 1L)
    nu_truths <- c(-0.30, 0.50, 1.60)
    fits <- list()
    for (i in seq_along(nu_truths)) {
      nu_true <- nu_truths[i]
      y <- mvgam:::rcmb_vec(
        mu = rep(0.5, n_t),
        nu = rep(nu_true, n_t),
        T  = trials
      )
      df <- data.frame(
        y       = y,
        trials  = trials,
        series  = factor("s1"),
        time    = seq_len(n_t)
      )
      fits[[as.character(nu_true)]] <- mvgam(
        bf(y | trials(trials) ~ 1),
        data    = df,
        family  = com_binomial(),
        chains  = 2L,
        samples = 500L,
        burnin  = 500L,
        silent  = 2L,
        backend = "cmdstanr"
      )
    }
    cached <<- fits
    cached
  }
})

test_that("com_binomial() recovers nu across three dispersion regimes", {
  fits <- .cmb_fits()
  nu_truths <- c(-0.30, 0.50, 1.60)
  for (i in seq_along(nu_truths)) {
    nu_true <- nu_truths[i]
    nu_draws <- as.numeric(as.array(
      fits[[as.character(nu_true)]], variable = "nu"
    ))
    nu_lo90 <- stats::quantile(nu_draws, 0.05)
    nu_hi90 <- stats::quantile(nu_draws, 0.95)
    nu_med <- stats::median(nu_draws)
    # 90% credible interval must cover the true nu used to sim
    # the data. If this fails, log the truth and the recovered
    # interval to a session message before the expectation so the
    # failing-fit context is visible in test output.
    cat(sprintf(
      "  nu_true = %.2f, 90%% CI = [%.3f, %.3f], median = %.3f\n",
      nu_true, nu_lo90, nu_hi90, nu_med
    ))
    expect_true(nu_lo90 <= nu_true)
    expect_true(nu_hi90 >= nu_true)
    # Posterior median within +/- 0.5 of truth ensures the chain
    # has not drifted to a different local mode. The fixture sets
    # theta = 0 (p = 0.5) by construction so the sign of nu is
    # the only signal in the data.
    expect_true(nu_med > nu_true - 0.5)
    expect_true(nu_med < nu_true + 0.5)
  }
})


test_that("convergence diagnostics on (intercept, nu) are clean", {
  # Gate-A stats review identified the p = 0.5 symmetric corner
  # as a likelihood-shape stress test where (intercept, nu)
  # posteriors can develop elongated valleys at small sample
  # sizes. Confirm R-hat < 1.1 on both parameters for all three
  # regimes at this fixture's budget (n = 200, 2 chains x 500
  # iter).
  fits <- .cmb_fits()
  for (nm in names(fits)) {
    rh <- bayesplot::rhat(fits[[nm]]$fit)
    rh_key <- rh[c("b_Intercept", "nu")]
    rh_key <- rh_key[!is.na(rh_key)]
    cat(sprintf(
      "  nu_true = %s: R-hat(b_Intercept, nu) = %s\n",
      nm, paste(round(rh_key, 3), collapse = ", ")
    ))
    expect_true(all(rh_key < 1.1))
  }
})


test_that("loo() returns finite ELPD with all Pareto-k < 1", {
  # Per-fit ELPD must be finite (no NaN log-density) and Pareto-k
  # must stay below 1 (otherwise the leave-one-out approximation
  # is unreliable and the example should advise k-fold instead).
  # The contributor's stress fixture (p = 0.5) typically produces
  # ~5% > 0.7 but < 1, which is borderline acceptable.
  fits <- .cmb_fits()
  for (nm in names(fits)) {
    lo <- suppressWarnings(loo::loo(fits[[nm]]))
    expect_true(is.finite(lo$estimates["elpd_loo", "Estimate"]))
    k <- lo$diagnostics$pareto_k
    cat(sprintf(
      "  nu_true = %s: elpd = %.1f, max Pareto-k = %.3f (%d > 0.7)\n",
      nm, lo$estimates["elpd_loo", "Estimate"],
      max(k), sum(k > 0.7)
    ))
    expect_true(all(k < 1))
  }
})


test_that("posterior dispatchers return expected shapes for com_binomial", {
  # log_lik [ndraws x nobs] finite, posterior_predict integer in
  # [0, T], posterior_epred count-scale numeric in [0, T].
  # Per-row mean of posterior_epred should be near T * 0.5 = T/2
  # (since the fixture sets theta = 0). Use the under-dispersed
  # fit (nu = 1.60) where the posterior is tightest.
  mod <- .cmb_fits()[["1.6"]]
  trials <- mod$data$trials
  expected_ndraws <- posterior::ndraws(
    posterior::as_draws_matrix(mod$fit)
  )

  ll <- log_lik(mod)
  expect_equal(nrow(ll), expected_ndraws)
  expect_equal(ncol(ll), length(trials))
  expect_true(all(is.finite(ll)))

  pp <- posterior_predict(mod, ndraws = 100L)
  expect_equal(dim(pp), c(100L, length(trials)))
  expect_true(is.integer(pp))
  expect_true(all(pp >= 0L))
  expect_true(all(pp <= matrix(trials, 100L, length(trials),
                                 byrow = TRUE)))

  pe <- posterior_epred(mod, ndraws = 100L)
  expect_equal(dim(pe), c(100L, length(trials)))
  expect_true(all(pe >= 0))
  expect_true(all(pe <= matrix(trials, 100L, length(trials),
                                 byrow = TRUE)))
  # Per-obs posterior mean of E[Y] should track T * 0.5
  per_obs_mean <- colMeans(pe)
  ey_over_t <- per_obs_mean / trials
  cat(sprintf(
    "  E[Y]/T range: [%.3f, %.3f] (target 0.5 +/- 0.05)\n",
    min(ey_over_t), max(ey_over_t)
  ))
  expect_true(all(abs(ey_over_t - 0.5) < 0.05))
})
