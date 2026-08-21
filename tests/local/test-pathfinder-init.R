# Local tests for `init = "pathfinder"`. These compile and fit real Stan
# models, so they stay out of the CI suite; the validator logic that can
# run without Stan lives in `tests/testthat/test-backends.R`.
#
# Deliberately absent: any assertion that Pathfinder starts are faster.
# Warmup duration varies with machine load and with which chain happens
# to land in a bad region, so a timing threshold would flake. The timing
# is printed for inspection instead.

source("setup_tests_local.R")

pf_test_data <- function() {
  set.seed(42)
  sim <- sim_mvgam(
    family = poisson(), n_series = 3, n_timepoints = 60,
    trend_model = AR()
  )
  sim$data_train
}

dat <- pf_test_data()

fit_with <- function(init_spec) {
  SM(mvgam(
    y ~ s(x),
    trend_formula = ~ AR(p = 1),
    data = dat,
    family = poisson(),
    chains = 2, cores = 2, iter = 1000,
    init = init_spec, silent = 2
  ))
}

test_that("init = 'pathfinder' produces a converged NUTS fit", {
  mod <- fit_with("pathfinder")

  # The posterior must still come from NUTS. Pathfinder supplies only
  # the starting values, so the fit stays exact.
  expect_s3_class(mod, "mvgam")
  expect_identical(mod$algorithm, "sampling")

  rhats <- posterior::summarise_draws(
    posterior::as_draws_array(mod$fit), "rhat"
  )$rhat
  expect_true(max(rhats, na.rm = TRUE) < 1.05)

  # Draw counts must match the requested sampler dimensions, which
  # catches a Pathfinder fit being returned in place of the NUTS one
  expect_equal(posterior::ndraws(as_draws_array(mod$fit)), 1000)
})

test_that("the initial-value request survives onto the fitted object", {
  mod <- fit_with("pathfinder")
  expect_identical(mod$init, "pathfinder")

  # Stan records a temporary JSON path for file-backed starts. That
  # path is machine-local, so the reproduction call must show the
  # keyword the user actually typed.
  md <- paste(methods_md(mod), collapse = "\n")
  expect_true(grepl("init          = \"pathfinder\"", md, fixed = TRUE))
  expect_false(grepl(".json", md, fixed = TRUE))

  # Pathfinder was not the posterior algorithm, so the methods text
  # must not credit it as one
  cite <- paste(utils::capture.output(print(how_to_cite(mod))),
                collapse = " ")
  expect_false(grepl("Pathfinder", cite, fixed = TRUE))
  expect_true(grepl("Hamiltonian Monte Carlo", cite, fixed = TRUE))
})

test_that("pathfinder and random starts agree on the posterior", {
  # Different starting values must not move the answer. Compare the
  # population-level effects, which are the best identified quantities
  # in the model.
  mod_pf <- fit_with("pathfinder")
  mod_rd <- fit_with("random")

  pars <- c("b_Intercept")
  s_pf <- posterior::summarise_draws(
    posterior::subset_draws(posterior::as_draws_array(mod_pf$fit), pars)
  )
  s_rd <- posterior::summarise_draws(
    posterior::subset_draws(posterior::as_draws_array(mod_rd$fit), pars)
  )
  # Within a quarter of a posterior SD of each other
  expect_true(
    abs(s_pf$mean - s_rd$mean) < 0.25 * max(s_pf$sd, s_rd$sd)
  )
})

test_that("downstream methods work on a pathfinder-initialised fit", {
  mod <- fit_with("pathfinder")
  expect_s3_class(forecast(mod), "mvgam_forecast")
  expect_true(is.matrix(posterior_predict(mod)))
  # Each observation informs its own latent state, so PSIS-LOO flags
  # influential points on state-space fits regardless of how the chains
  # were started. Silence the expected notice at this one call and
  # assert the object, matching the other local fitting tests.
  lo <- suppressWarnings(loo(mod))
  expect_s3_class(lo, "psis_loo")
  expect_true(is.finite(lo$estimates["elpd_loo", "Estimate"]))
})

test_that("update() reuses the pathfinder start without being asked", {
  mod <- fit_with("pathfinder")
  upd <- SM(update(mod, chains = 1, iter = 400, silent = 2))
  expect_identical(upd$init, "pathfinder")
})

test_that("init = 'pathfinder' is refused by the rstan backend", {
  # The refusal must happen during setup rather than after the model
  # has been parsed and compiled, which is the expensive failure
  expect_error(
    mvgam(y ~ 1, trend_formula = ~ AR(p = 1), data = dat,
          family = poisson(), backend = "rstan", init = "pathfinder",
          chains = 1, iter = 200, silent = 2),
    "pathfinder"
  )
})
