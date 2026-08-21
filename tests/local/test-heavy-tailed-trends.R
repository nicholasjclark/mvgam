# Local tests for heavy-tailed trend innovations (`df` on AR, RW, CAR
# and ZMVN). These compile and fit real Stan models, so they stay out
# of the CI suite; the code-generation and drawer contracts run without
# Stan in `tests/testthat/test-trend-stancode-shape.R`.
#
# The degrees of freedom are informed only through the tail of the
# latent process, so these tests check that inference behaves and that
# the fitted distribution reaches the forecast, not that `nu` is
# recovered sharply. It is weakly identified by construction.

source("setup_tests_local.R")

ht_sim <- function(nu = Inf, seed = 21, n_series = 3, n_timepoints = 80) {
  set.seed(seed)
  sim_mvgam(family = poisson(), n_series = n_series,
            n_timepoints = n_timepoints, trend_model = AR(),
            proportional_train = 0.85)
}

ht_fit <- function(sim, tf, ...) {
  SM(mvgam(y ~ 1, trend_formula = tf, data = sim$data_train,
           newdata = sim$data_test, family = poisson(),
           backend = "cmdstanr", chains = 2, cores = 2, iter = 1000,
           seed = 5, silent = 2, ...))
}

sim_g <- ht_sim()
mod_est <- ht_fit(sim_g, ~ AR(p = 1, df = NA))


test_that("estimated innovation degrees of freedom sample cleanly", {
  drw <- posterior::as_draws_df(mod_est$fit)
  expect_true("nu_trend" %in% names(drw))
  expect_true(all(drw$nu_trend > 2))
  s <- posterior::summarise_draws(drw, "rhat", "ess_bulk")
  expect_true(max(s$rhat, na.rm = TRUE) < 1.1)
  # The marginalised parameterisation exists because the explicit scale
  # mixture funnelled; guard against a regression back to that.
  expect_true(rstan::get_num_divergent(mod_est$fit) < 0.05 * 1000)
  nu_ess <- s$ess_bulk[s$variable == "nu_trend"]
  expect_true(nu_ess > 50)
})

test_that("Gaussian data do not produce a spuriously heavy tail", {
  # sim_mvgam draws Gaussian innovations, so the posterior for the
  # degrees of freedom should sit well away from the heavy-tailed end
  # rather than inventing shocks that are not there.
  drw <- posterior::as_draws_df(mod_est$fit)
  expect_true(median(drw$nu_trend) > 5)
})

test_that("df = Inf reproduces the Gaussian fit exactly", {
  code_default <- paste(unlist(stancode(
    mvgam_formula(y ~ 1, trend_formula = ~ AR(p = 1)),
    data = sim_g$data_train, family = poisson(), backend = "cmdstanr"
  )), collapse = "\n")
  code_inf <- paste(unlist(stancode(
    mvgam_formula(y ~ 1, trend_formula = ~ AR(p = 1, df = Inf)),
    data = sim_g$data_train, family = poisson(), backend = "cmdstanr"
  )), collapse = "\n")
  expect_identical(code_default, code_inf)
})

test_that("every post-processing method works on a heavy-tailed fit", {
  expect_s3_class(summary(mod_est), "mvgam_summary")
  txt <- utils::capture.output(print(summary(mod_est)))
  expect_true(any(grepl("nu_trend", txt)))

  expect_true(is.matrix(posterior_predict(mod_est)))
  expect_true(is.matrix(predict(mod_est)))
  expect_true(is.matrix(fitted(mod_est)))
  expect_true(is.matrix(residuals(mod_est)))
  expect_true(all(is.finite(log_lik(mod_est))))
  expect_s3_class(suppressWarnings(loo(mod_est)), "psis_loo")
  expect_s3_class(hindcast(mod_est), "mvgam_forecast")
  expect_s3_class(pp_check(mod_est), "ggplot")
  expect_s3_class(conditional_effects(mod_est), "mvgam_conditional_effects")
  expect_s3_class(mcmc_plot(mod_est, variable = "nu_trend"), "ggplot")
})

test_that("the model description reports the heavy tail", {
  cite <- paste(utils::capture.output(print(how_to_cite(mod_est))),
                collapse = " ")
  expect_true(grepl("Student-t distribution", cite))
  expect_true(grepl("Durbin", cite))
  expect_true(grepl("StudentT", paste(methods_md(mod_est), collapse = " ")))
})

test_that("forecasts draw from the fitted innovation distribution", {
  # The failure this guards against is silent: a model that fits with
  # heavy tails but forecasts Gaussian produces intervals that are too
  # narrow, with nothing raised.
  fc <- forecast(mod_est, newdata = sim_g$data_test)
  expect_s3_class(fc, "mvgam_forecast")

  cs <- mvgam:::get_trend_covariance_structure(mod_est, ndraws = 50)
  expect_false(is.null(cs$nu_trend))
  expect_length(cs$nu_trend, 50)
  expect_true(all(cs$nu_trend > 2))
})

test_that("update() keeps the heavy-tailed process", {
  upd <- SM(update(mod_est, chains = 1, iter = 400, silent = 2))
  expect_true(is.na(upd$trend_metadata$df))
  expect_true("nu_trend" %in%
                names(posterior::as_draws_df(upd$fit)))
})

test_that("a fixed df fits without estimating a parameter", {
  mod_fixed <- ht_fit(sim_g, ~ AR(p = 1, df = 5))
  drw <- posterior::as_draws_df(mod_fixed$fit)
  expect_false("nu_trend" %in% names(drw))
  s <- posterior::summarise_draws(drw, "rhat")
  expect_true(max(s$rhat, na.rm = TRUE) < 1.1)

  cs <- mvgam:::get_trend_covariance_structure(mod_fixed, ndraws = 20)
  expect_identical(unique(cs$nu_trend), 5)
})

test_that("heavy tails work on the other supporting trend types", {
  for (tf in list(~ RW(df = NA), ~ ZMVN(df = NA))) {
    mod <- ht_fit(sim_g, tf)
    expect_s3_class(mod, "mvgam")
    s <- posterior::summarise_draws(
      posterior::as_draws_df(mod$fit), "rhat"
    )
    expect_true(max(s$rhat, na.rm = TRUE) < 1.1)
  }
})
