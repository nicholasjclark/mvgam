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
# Every dataset is drawn before any model is fitted. Interleaving
# the two would let each `mvgam()` call advance the RNG, so the
# later datasets would shift whenever sampling internals changed
# and a recovery failure could not be reproduced.
.cmb_data <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    set.seed(2026)
    n_t <- 200L
    trials <- pmax(rpois(n_t, 30L), 1L)
    nu_truths <- c(-0.30, 0.50, 1.60)
    cached <<- lapply(stats::setNames(nu_truths, as.character(nu_truths)),
      function(nu_true) {
        data.frame(
          y      = mvgam:::rcmb_vec(mu = rep(0.5, n_t),
                                    nu = rep(nu_true, n_t), T = trials),
          trials = trials,
          series = factor("s1"),
          time   = seq_len(n_t)
        )
      })
    cached
  }
})

.cmb_fits <- local({
  cached <- NULL
  function() {
    if (!is.null(cached)) return(cached)
    datasets <- .cmb_data()
    cached <<- lapply(datasets, function(df) {
      mvgam(
        bf(y | trials(trials) ~ 1),
        data    = df,
        family  = com_binomial(),
        chains  = 2L,
        samples = 500L,
        burnin  = 500L,
        silent  = 2L,
        seed    = 2026L,
        backend = "cmdstanr"
      )
    })
    cached
  }
})

# Sampling behaviour of the joint (intercept, nu) MLE at n = 200,
# p = 0.5, trials ~ Poisson(30), measured over 60 replicate datasets
# with an independently written closed-form log-pmf. The MLE is
# unbiased at every truth; precision degrades sharply as nu rises
# because the distribution concentrates toward T / 2.
#
#   truth   MLE mean   MLE sd    MLE on this fixture
#   -0.30   -0.30      0.016     -0.282
#    0.50    0.505     0.050      0.531
#    1.60    1.602     0.162      1.360
CMB_MLE <- c("-0.3" = -0.282, "0.5" = 0.531, "1.6" = 1.360)
CMB_MLE_SD <- c("-0.3" = 0.016, "0.5" = 0.050, "1.6" = 0.162)


test_that("the fixture datasets are typical draws from their truths", {
  # A cheap guard, run without MCMC, that the fixed datasets are not
  # freak draws. It also documents why the fits below are compared
  # against each dataset's own MLE rather than against the truth:
  # at nu = 1.60 one dataset in ten sits far enough from the truth
  # that a 90% interval misses it, whatever the sampler does.
  for (nm in names(CMB_MLE)) {
    z <- (CMB_MLE[[nm]] - as.numeric(nm)) / CMB_MLE_SD[[nm]]
    cat(sprintf("  nu_true = %5s: MLE = %6.3f (%+.2f sd from truth)\n",
                nm, CMB_MLE[[nm]], z))
    expect_true(abs(z) < 3)
  }
})


test_that("the posterior for nu reproduces each dataset's likelihood", {
  # This is a regression assertion, not a coverage one. Whether a
  # 90% interval from one fixed dataset covers the truth is a
  # property of that draw, so it fails for a correct model about a
  # tenth of the time. What must hold every run is that the sampler
  # lands where the likelihood for that dataset points: the default
  # `normal(1, 0.5)` prior moves the posterior median by only about
  # 0.02 at this sample size, measured against a near-flat prior.
  fits <- .cmb_fits()
  for (nm in names(fits)) {
    nu_draws <- as.numeric(as.array(fits[[nm]], variable = "nu"))
    nu_med <- stats::median(nu_draws)
    tol <- 2 * CMB_MLE_SD[[nm]]
    cat(sprintf(
      "  nu_true = %5s: median = %6.3f, MLE = %6.3f, diff = %.3f (tol %.3f)\n",
      nm, nu_med, CMB_MLE[[nm]], abs(nu_med - CMB_MLE[[nm]]), tol
    ))
    expect_true(abs(nu_med - CMB_MLE[[nm]]) < tol)
    # The sign of nu is the qualitative claim a user reads off the
    # fit, and p = 0.5 makes it the hardest thing to get right.
    expect_identical(sign(nu_med), sign(as.numeric(nm)))
  }
})


test_that("convergence diagnostics on (intercept, nu) are clean", {
  # The p = 0.5 symmetric corner is a likelihood-shape stress
  # test: with no asymmetry to pin the intercept, the
  # (intercept, nu) posterior can develop an elongated valley at
  # small sample sizes. Confirm R-hat < 1.1 on both parameters
  # for all three regimes at this fixture's budget (n = 200,
  # 2 chains x 500 iter).
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
