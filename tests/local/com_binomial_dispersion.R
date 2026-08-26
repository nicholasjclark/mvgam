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


test_that("a dispersion sub-formula survives the post-fit surfaces", {
  # Recovering `b_nu_zb` says the sampler handled the sub-formula. It
  # says nothing about what happens afterwards, and for a long while
  # the answer was that every post-fit method failed: the dpar
  # extractor looked for a scalar `nu` in the posterior, which a
  # sub-formula never produces. This drives the surfaces a user
  # actually reaches, and checks that the `nu` behind them is the one
  # the coefficients describe rather than a constant.
  set.seed(2030)
  n_t <- 120L
  trials <- pmax(rpois(n_t, 20L), 1L)
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
    burnin = 500L, silent = 2L, seed = 2030L, backend = "cmdstanr"
  ))

  n_draws <- ndraws(fit)
  expect_equal(dim(posterior_epred(fit)), c(n_draws, n_t))
  expect_equal(dim(posterior_predict(fit)), c(n_draws, n_t))
  expect_equal(dim(log_lik(fit)), c(n_draws, n_t))
  expect_equal(nrow(residuals(fit)), n_t)
  expect_equal(nrow(fitted(fit)), n_t)
  expect_s3_class(SW(loo(fit)), "psis_loo")
  expect_s3_class(SW(pp_check(fit, ndraws = 10L)), "ggplot")

  # The resolved `nu` is the sub-formula evaluated per row, so its two
  # levels are the intercept and the intercept plus the slope.
  nu <- mvgam:::resolve_family_pars(
    fit, dpar_names = "nu", ndraws = n_draws, nobs = n_t
  )$nu
  expect_equal(dim(nu), c(n_draws, n_t))
  draws <- posterior::as_draws_df(fit)
  expect_equal(mean(nu[, df$z == "a"]), mean(draws$b_nu_Intercept),
               tolerance = 1e-8)
  expect_equal(mean(nu[, df$z == "b"]),
               mean(draws$b_nu_Intercept + draws$b_nu_zb),
               tolerance = 1e-8)

  # Thinning must not decouple the parameter from the mean. Asked for
  # a subset of draws, the parameter has to come from those same
  # iterations: the two are extracted separately, so a count left
  # unresolved would let each subsample on its own and pair a
  # dispersion with a mean from an unrelated iteration.
  ids <- c(3L, 17L, 200L, 411L)
  nu_ids <- mvgam:::resolve_family_pars(
    fit, dpar_names = "nu", ndraws = length(ids), nobs = n_t,
    draw_ids = ids
  )$nu
  expect_equal(nrow(nu_ids), length(ids))
  expect_equal(as.numeric(nu_ids[, which(df$z == "a")[1]]),
               draws$b_nu_Intercept[ids], tolerance = 1e-8)
  expect_equal(as.numeric(nu_ids[, which(df$z == "b")[1]]),
               (draws$b_nu_Intercept + draws$b_nu_zb)[ids],
               tolerance = 1e-8)
  # A bare count reaches the same shape rather than erroring or
  # returning the whole posterior.
  expect_equal(nrow(posterior_epred(fit, ndraws = 25L)), 25L)
  expect_equal(nrow(log_lik(fit, ndraws = 25L)), 25L)
  expect_equal(nrow(posterior_predict(fit, ndraws = 25L)), 25L)

  # `posterior_linpred()` answers for the named parameter rather than
  # for the mean.
  expect_equal(
    as.numeric(posterior_linpred(fit, dpar = "nu")),
    as.numeric(nu), tolerance = 1e-8
  )
  expect_error(
    posterior_linpred(fit, dpar = "sigma"),
    "not a distributional parameter"
  )

  # The summary reports the sub-formula, its link, and its
  # coefficients in a block of their own rather than folded into the
  # population-level effects.
  smry <- summary(fit)
  expect_true("dpar_nu_fixed" %in% names(smry))
  expect_setequal(rownames(smry$dpar_nu_fixed), c("Intercept", "zb"))
  expect_false(any(grepl("^nu", rownames(smry$fixed %||% matrix(0, 0, 0)))))
  header <- paste(capture.output(print(smry)), collapse = "\n")
  expect_true(grepl("nu ~ z", header, fixed = TRUE))
  expect_true(grepl("nu = identity", header, fixed = TRUE))
})


test_that("an unobserved cell may carry a denominator of zero", {
  # Padding a missing response with `trials = 0` is the natural
  # encoding: no trials were run. brms drops the row from the
  # likelihood and keeps it in the data, so every post-fit method has
  # to cope with a zero denominator on a row it still predicts for.
  set.seed(2031)
  n_t <- 96L
  df <- data.frame(
    trials = 20L,
    z      = factor(rep(c("a", "b"), length.out = n_t)),
    series = factor(rep(c("s1", "s2"), each = n_t / 2L)),
    time   = rep(seq_len(n_t / 2L), 2L)
  )
  df$y <- rbinom(n_t, 20L, 0.35)
  df$y[sample.int(n_t, 12L)] <- NA_integer_
  df$trials[is.na(df$y)] <- 0L

  # Counting rather than suppressing: a condition raised by several
  # internal passes over the same data should reach the user once.
  count_warnings <- function(expr, pattern) {
    n <- 0L
    value <- withCallingHandlers(expr, warning = function(w) {
      if (grepl(pattern, conditionMessage(w))) n <<- n + 1L
      invokeRestart("muffleWarning")
    })
    attr(value, "n_warnings") <- n
    value
  }

  fit <- count_warnings(
    SM(mvgam(
      bf(y | trials(trials) ~ 1), data = df, family = com_binomial(),
      trend_formula = ~ AR(), chains = 2L, samples = 500L,
      burnin = 500L, silent = 2L, seed = 2031L, backend = "cmdstanr"
    )),
    "Rows containing NAs"
  )
  # Assembling the model runs brms's code generator three times over
  # the same frame; the dropped rows are still reported only once.
  expect_equal(attr(fit, "n_warnings"), 1L)
  attr(fit, "n_warnings") <- NULL

  # The likelihood covers the observed rows; predictions cover all.
  expect_equal(fit$standata$N, sum(!is.na(df$y)))
  expect_equal(ncol(posterior_epred(fit)), n_t)
  expect_s3_class(SW(loo(fit)), "psis_loo")
  expect_s3_class(SW(pp_check(fit, ndraws = 10L)), "ggplot")
  expect_s3_class(SW(plot(fit, type = "residuals")), "patchwork")

  # With no trials behind it, a cell has no events to expect.
  epred <- posterior_epred(fit)
  expect_true(all(epred[, is.na(df$y)] == 0))

  # A residual panel runs the same check once per panel, and reports
  # the omitted rows once for the grid rather than four times.
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  resid_panel <- count_warnings(
    plot(fit, type = "residuals"), "missing response"
  )
  expect_equal(attr(resid_panel, "n_warnings"), 1L)
  re_panel <- count_warnings(plot(fit, type = "re"), "missing response")
  expect_equal(attr(re_panel, "n_warnings"), 1L)
})
