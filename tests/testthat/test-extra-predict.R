# The tests of test-predict.R that assemble a model or run for
# 0.2 s or more. CRAN skips this file. CI and local runs take it.
skip_on_cran()


test_that("the Stan data tells exact gp() terms from approximate ones", {
  dat <- data.frame(y = rnorm(6), x = 1:6, z = c(2, 5, 1, 4, 3, 6))
  forms <- function(f) gp_forms(brms::make_standata(f, dat))
  expect_identical(forms(y ~ gp(x) + gp(z, k = 4)),
                   c(approximate = TRUE, exact = TRUE))
  expect_identical(forms(y ~ gp(x, k = 4)),
                   c(approximate = TRUE, exact = FALSE))
  expect_identical(forms(y ~ gp(x, cov = "matern12")),
                   c(approximate = FALSE, exact = TRUE))
  # A prediction's Stan data copies the fitted values into `_old`
  fit <- brms::brm(y ~ gp(x, k = 4), dat, empty = TRUE)
  expect_identical(
    gp_forms(brms::standata(fit, newdata = dat, internal = TRUE)),
    c(approximate = TRUE, exact = FALSE)
  )
})

test_that("a frame is held to the axis columns the fit needs", {
  # `varying_meta_vars()` keeps a meta var only where it varies in
  # training. A fit on one occasion has no time column in that list.
  # The completeness check still requires the time column.
  set.seed(1)
  flat <- data.frame(y = rpois(30, 3), x = rnorm(30),
                     time = rep(1L, 30),
                     series = factor(rep(c("a", "b", "c"), each = 10)))
  prefit <- mvgam(y ~ x, trend_formula = ~ ZMVN(), data = flat,
                  family = poisson(), run_model = FALSE, silent = 2,
                  validate = FALSE)
  no_time <- flat
  no_time$time <- NULL
  expect_error(
    mvgam:::validate_newdata_complete(no_time, prefit),
    "Absent: 'time'"
  )
  # A trend-free fit places no row in time and needs no time column.
  trend_free <- mvgam(y ~ x, data = flat, family = poisson(),
                      run_model = FALSE, silent = 2, validate = FALSE)
  expect_silent(mvgam:::validate_newdata_complete(no_time, trend_free))

  # A frame omitting the series column reaches prediction with every
  # row unmapped. The column is exempt where a grouping names the
  # series; on a fit whose axis is the column itself it is the only
  # route.
  d <- data.frame(y = rpois(40, 3), x = rnorm(40),
                  time = rep(1:20, 2),
                  series = factor(rep(c("a", "b"), each = 20)))
  fit <- mvgam(y ~ x, trend_formula = ~ AR(p = 1), data = d,
               family = poisson(), run_model = FALSE, silent = 2,
               validate = FALSE)
  no_series <- d
  no_series$series <- NULL
  expect_error(
    mvgam:::validate_newdata_complete(no_series, fit),
    "Absent: 'series'"
  )

  # One series needs no column of its own: every row belongs to it.
  one <- data.frame(y = rpois(20, 3), x = rnorm(20), time = 1:20,
                    series = factor(rep("only", 20)))
  solo <- mvgam(y ~ x, trend_formula = ~ AR(p = 1), data = one,
                family = poisson(), run_model = FALSE, silent = 2,
                validate = FALSE)
  bare <- one
  bare$series <- NULL
  expect_silent(mvgam:::validate_newdata_complete(bare, solo))
})

# `sample_from_family()` and `compute_family_epred()` take the same
# quantity, the inverse link of the linear predictor. The average of the
# draws must equal the mean. A sampler that took that quantity as E[Y]
# would shift its draws and fail the test.
test_that("sampled draws average to the mean the same predictor implies", {
  dpar_value <- function(dpar) {
    switch(dpar,
      sigma = 0.4, shape = 6, phi = 8, nu = 8, hu = 0.3, zi = 0.25,
      zoi = 0.2, coi = 0.6, beta = 1.3, ndt = 0.2, 0.5)
  }
  families <- c(
    "poisson", "negbinomial", "binomial", "bernoulli", "beta",
    "beta_binomial", "lognormal", "hurdle_poisson", "hurdle_negbinomial",
    "hurdle_lognormal", "zero_inflated_poisson",
    "zero_inflated_negbinomial", "zero_inflated_binomial",
    "zero_inflated_beta", "zero_inflated_beta_binomial"
  )
  set.seed(2024)
  ndraws <- 10000L
  # Trial totals differ by observation. Each observation's draws must
  # match its own mean, which fails if a sampler pairs draws with
  # another observation's total.
  trials <- c(0, 3, 40)
  nobs <- length(trials)

  for (family_name in families) {
    family <- brms::brmsfamily(family_name)
    eta <- matrix(0.2, ndraws, nobs)
    mu <- family$linkinv(eta)
    extra <- setdiff(family$dpars, "mu")
    dpars <- stats::setNames(
      lapply(extra, function(d) matrix(dpar_value(d), ndraws, nobs)), extra
    )
    needed <- epred_extra_dpars(family)
    expected <- compute_family_epred(
      linpred = eta, family = family, trials = trials,
      family_pars = if (length(needed)) dpars[needed] else NULL
    )

    args <- dpars[intersect(names(dpars), names(formals(sample_from_family)))]
    drawn <- matrix(do.call(sample_from_family, c(
      list(family_name = family_name, ndraws = ndraws, epred = mu,
           trials = trials, lb = NULL, ub = NULL),
      args
    )), ndraws, nobs)
    # A zero-trial observation has one possible count.
    expect_true(all(drawn[expected == 0] == 0))
    # Monte Carlo error over 10k draws stays near one percent. The
    # defects this guards against were off by a factor.
    expect_equal(colMeans(drawn), expected[1L, ], tolerance = 0.05)
  }
})
