# Local tests for beta_nb(). These compile and fit real Stan models, so
# they stay out of the CI suite; the constructor, stancode and R-side
# density contracts run without Stan in
# `tests/testthat/test-beta-nb-family.R`.
#
# The two regimes matter for different reasons. The moderate tail has
# finite variance and is where recovery should work. The heavy tail has
# `mtail` below 1, so the variance does not exist; the point there is
# that inference and every post-processing method still behave, not
# that the parameters are sharply identified.

source("setup_tests_local.R")

bnb_sim <- function(shape, mtail, seed = 101, n_series = 3,
                    n_timepoints = 90) {
  set.seed(seed)
  sim_mvgam(
    family = beta_nb(), n_series = n_series,
    n_timepoints = n_timepoints, trend_model = AR(),
    proportional_train = 0.85,
    family_pars = list(shape = shape, mtail = mtail)
  )
}

bnb_fit <- function(sim, ...) {
  SM(mvgam(
    y ~ 1, trend_formula = ~ AR(p = 1),
    data = sim$data_train, newdata = sim$data_test,
    family = beta_nb(), backend = "cmdstanr",
    chains = 2, cores = 2, iter = 1500, seed = 7, silent = 2, ...
  ))
}

TRUE_SHAPE <- 2
TRUE_MTAIL <- 2.5
sim_mod <- bnb_sim(TRUE_SHAPE, TRUE_MTAIL)
mod <- bnb_fit(sim_mod)


test_that("beta_nb() recovers its parameters and samples cleanly", {
  drw <- posterior::as_draws_df(mod$fit)
  for (nm in c("shape", "mtail")) {
    truth <- if (nm == "shape") TRUE_SHAPE else TRUE_MTAIL
    q <- unname(quantile(drw[[nm]], c(0.05, 0.95)))
    expect_true(truth >= q[1] && truth <= q[2])
  }
  s <- posterior::summarise_draws(drw, "rhat")
  expect_true(max(s$rhat, na.rm = TRUE) < 1.05)
  expect_true(rstan::get_num_divergent(mod$fit) < 0.05 * 1500)
})

test_that("the mean of the posterior predictive tracks the fitted mean", {
  # The family is parameterised so that E[Y] = mu exactly, so the
  # predictive mean and the expectation must agree up to Monte Carlo
  # error. A mismatch would mean the RNG and the Stan density disagree
  # about the parameterisation.
  pp <- posterior_predict(mod)
  pe <- posterior_epred(mod)
  expect_equal(mean(colMeans(pp)), mean(colMeans(pe)), tolerance = 0.15)
})

test_that("every post-processing method works on a beta_nb fit", {
  pp <- posterior_predict(mod)
  expect_true(is.matrix(pp))
  expect_true(all(pp >= 0))
  expect_true(all(pp == floor(pp)))

  expect_true(is.matrix(posterior_epred(mod)))
  expect_true(is.matrix(posterior_linpred(mod)))
  expect_true(is.matrix(fitted(mod)))
  expect_true(is.matrix(predict(mod)))
  expect_true(is.matrix(residuals(mod)))

  ll <- log_lik(mod)
  expect_true(all(is.finite(ll)))

  # PSIS-LOO reliably flags influential points here for two compounding
  # reasons: every observation informs its own latent state, and a
  # heavy-tailed family makes the largest counts genuinely influential.
  # mvgam treats a high Pareto k as a regime to report rather than an
  # error (see `mvgam_loo_compare_diagnostics`), so the expected notice
  # is silenced at this one call and the returned object is asserted.
  lo <- suppressWarnings(loo(mod))
  expect_s3_class(lo, "psis_loo")
  expect_true(is.finite(lo$estimates["elpd_loo", "Estimate"]))

  expect_s3_class(hindcast(mod), "mvgam_forecast")
  expect_s3_class(pp_check(mod), "ggplot")
  expect_s3_class(conditional_effects(mod), "mvgam_conditional_effects")
  expect_s3_class(summary(mod), "mvgam_summary")
})

test_that("summary() reports both distributional parameters", {
  txt <- utils::capture.output(print(summary(mod)))
  expect_true(any(grepl("\\bshape\\b", txt)))
  expect_true(any(grepl("\\bmtail\\b", txt)))
})

test_that("how_to_cite() and methods_md() describe the family", {
  cite <- paste(utils::capture.output(print(how_to_cite(mod))),
                collapse = " ")
  expect_true(grepl("beta negative binomial", cite))
  expect_true(grepl("Irwin", cite))
  expect_true(grepl("BetaNegBinomial", paste(methods_md(mod), collapse = " ")))
})

test_that("forecast intervals cover the simulated truth at their nominal rate", {
  fc <- forecast(mod, newdata = sim_mod$data_test)
  expect_s3_class(fc, "mvgam_forecast")
  lv <- levels(sim_mod$data_test$series)
  cov90 <- cov50 <- numeric(length(fc$forecasts))
  for (i in seq_along(fc$forecasts)) {
    fmat <- fc$forecasts[[i]]
    truth <- sim_mod$data_test$y[sim_mod$data_test$series == lv[i]]
    n <- min(length(truth), ncol(fmat))
    q <- apply(fmat[, seq_len(n), drop = FALSE], 2, quantile,
               c(0.05, 0.25, 0.75, 0.95))
    truth <- truth[seq_len(n)]
    cov90[i] <- mean(truth >= q[1, ] & truth <= q[4, ])
    cov50[i] <- mean(truth >= q[2, ] & truth <= q[3, ])
  }
  # Generous bands: with 14 held-out points per series the binomial
  # noise on a coverage estimate is large, so this catches gross
  # miscalibration rather than fine departures.
  expect_range(mean(cov90), lower = 0.75, upper = 1.0)
  expect_range(mean(cov50), lower = 0.30, upper = 0.80)
})

test_that("forecasts can be scored with the sample-based rules", {
  fc <- forecast(mod, newdata = sim_mod$data_test)
  for (rule in c("crps", "drps", "energy")) {
    expect_true(!is.null(score(fc, score = rule)))
  }
})

test_that("a heavy tail with infinite variance still fits and predicts", {
  # mtail below 1 means the variance does not exist. Quantile residuals
  # and the sample-based scores need only the distribution function, so
  # they must keep working; this is the regime the family exists for.
  sim_heavy <- bnb_sim(shape = 3, mtail = 0.6, seed = 202)
  mod_heavy <- bnb_fit(sim_heavy)

  drw <- posterior::as_draws_df(mod_heavy$fit)
  expect_true(mean(drw$mtail < 1) > 0)
  s <- posterior::summarise_draws(drw, "rhat")
  expect_true(max(s$rhat, na.rm = TRUE) < 1.1)

  pp <- posterior_predict(mod_heavy)
  expect_true(all(pp >= 0))
  expect_true(all(is.finite(log_lik(mod_heavy))))
  expect_true(is.matrix(residuals(mod_heavy)))
  expect_s3_class(forecast(mod_heavy, newdata = sim_heavy$data_test),
                  "mvgam_forecast")
})

test_that("censored beta_nb observations fit through the lccdf", {
  # Exercises the distribution function in the Stan code rather than
  # just checking that it was emitted.
  dat <- sim_mod$data_train
  dat$ycens <- pmin(dat$y, 25L)
  dat$cn <- ifelse(dat$y > 25, "right", "none")
  mod_cens <- SM(mvgam(
    ycens | cens(cn) ~ 1, trend_formula = ~ AR(p = 1), data = dat,
    family = beta_nb(), backend = "cmdstanr",
    chains = 2, cores = 2, iter = 1000, seed = 3, silent = 2
  ))
  expect_s3_class(mod_cens, "mvgam")
  s <- posterior::summarise_draws(
    posterior::as_draws_df(mod_cens$fit), "rhat"
  )
  expect_true(max(s$rhat, na.rm = TRUE) < 1.1)
})
