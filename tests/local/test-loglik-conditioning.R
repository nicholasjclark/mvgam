# Which surface a density is scored on, fitted in this file and
# checked against a brms twin carrying the same autocorrelation.
#
# `log_lik()` scores each observation under the latent state the model
# inferred at that time. Scoring the marginal instead leaves the loo
# weights describing a series the model never saw, and the symptom is
# quiet: on a 30-observation fixture it put the effective parameter
# count at 784 and pinned `loo_R2` on its clamp. Every number stayed
# finite.
#
# So the claims here are about which of the two answered rather than
# about whether a matrix came back. `incl_autocor = TRUE` reads the
# fitted state; `FALSE` contributes the trend's deterministic
# submodel. Both are legitimate and they are different densities, so
# a method that silently returns one where the other was asked for is
# what this file exists to catch.
#
#   truth: 60 occasions, poisson, one latent AR(1) and a fixed effect
#   model: y ~ 1 + x, trend_formula = ~ AR(p = 1)
#    twin: y ~ 1 + x + ar(time = time, p = 1, cov = TRUE) in brms
#
# The twin carries its autocorrelation as a joint MVN over the series
# rather than observation by observation, so per-observation
# densities are not comparable between the two. What is comparable is
# what they aggregate to: elpd, p_loo, bayes_R2 and the loo-weighted
# predictions.
#
# Run with:
#   testthat::test_file("tests/local/test-loglik-conditioning.R")

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  library(brms)
  library(posterior)
  library(testthat)
})

cache_path <- function(name) {
  dir <- if (dir.exists("fixtures")) {
    "fixtures"
  } else {
    file.path("tests", "local", "fixtures")
  }
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  file.path(dir, name)
}

with_warnings <- function(expr) {
  seen <- character(0)
  value <- withCallingHandlers(expr, warning = function(w) {
    seen <<- c(seen, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  list(value = value, warnings = seen)
}

cached_fit <- function(name, expr) {
  path <- cache_path(name)
  if (file.exists(path)) {
    cat("[cache] ", name, "\n", sep = "")
    return(readRDS(path))
  }
  cat("[fit  ] ", name, "\n", sep = "")
  fit <- with_warnings(force(expr))$value
  saveRDS(fit, path)
  fit
}

set.seed(2718L)

n_time <- 60L
ar_true <- 0.7
sigma_true <- 0.5

# Drawn from the stationary distribution, so the path has the
# marginal variance its parameters imply rather than starting at zero
# and growing into it.
latent <- numeric(n_time)
latent[1L] <- rnorm(1, 0, sigma_true / sqrt(1 - ar_true^2))
for (t in 2:n_time) {
  latent[t] <- ar_true * latent[t - 1L] + rnorm(1, 0, sigma_true)
}

x <- seq(-2, 2, length.out = n_time)
dat <- data.frame(
  y = rpois(n_time, exp(0.5 + 1.5 * x + latent)),
  x = x,
  time = seq_len(n_time),
  series = factor("s1")
)

# A second frame for the multivariate claims: two gaussian responses
# sharing one latent state, which is what makes each response's
# conditional predictor its own observation predictor plus that one
# state and nothing else.
mv_latent <- numeric(n_time)
mv_latent[1L] <- rnorm(1, 0, 0.4)
for (t in 2:n_time) {
  mv_latent[t] <- 0.6 * mv_latent[t - 1L] + rnorm(1, 0, 0.3)
}
mv_dat <- data.frame(
  x = x,
  time = seq_len(n_time),
  series = factor("s1")
)
mv_dat$y1 <- 1.0 + 0.8 * x + mv_latent + rnorm(n_time, 0, 0.3)
mv_dat$y2 <- -0.5 + 1.2 * x + mv_latent + rnorm(n_time, 0, 0.3)


# -- Fits -------------------------------------------------------------

mv <- cached_fit("val_mvgam_loglik_ar1.rds", mvgam(
  y ~ 1 + x, trend_formula = ~ AR(p = 1), data = dat,
  family = poisson(), chains = 2L, iter = 1000L, warmup = 500L,
  silent = 2, backend = "cmdstanr"
))

bm <- cached_fit("val_brms_loglik_ar1.rds", brm(
  y ~ 1 + x + ar(time = time, p = 1, cov = TRUE), data = dat,
  family = poisson(), chains = 2L, iter = 1000L, warmup = 500L,
  silent = 2, refresh = 0, backend = "cmdstanr"
))

mv_joint <- cached_fit("val_mvgam_loglik_mvbf.rds", mvgam(
  bf(y1 ~ x) + bf(y2 ~ x) + set_rescor(FALSE),
  trend_formula = ~ AR(p = 1), data = mv_dat,
  family = gaussian(), chains = 2L, iter = 1000L, warmup = 500L,
  silent = 2, backend = "cmdstanr"
))


test_that("log_lik answers for every row, conditioned or not", {
  for (cond in c(TRUE, FALSE)) {
    ll <- log_lik(mv, incl_autocor = cond, ndraws = 50L)
    expect_identical(dim(ll), c(50L, nrow(dat)))
    expect_true(all(is.finite(ll)))
  }
})


test_that("conditioning and marginalising give different densities", {
  # The whole file rests on this. If they agree, every comparison
  # below is a comparison of one quantity with itself, and a method
  # that ignored the argument would satisfy all of them.
  cond <- log_lik(mv, incl_autocor = TRUE)
  marg <- log_lik(mv, incl_autocor = FALSE)
  expect_identical(dim(cond), dim(marg))
  expect_false(isTRUE(all.equal(cond, marg)))
  # Conditioning on the fitted state fits the data it was fitted to
  # better, by construction, so the difference has a direction.
  expect_gt(sum(colMeans(cond)), sum(colMeans(marg)))
})


test_that("the superseded spelling still selects the surface", {
  expect_equal(log_lik(mv, process_error = TRUE),
               log_lik(mv, incl_autocor = TRUE))
  expect_equal(log_lik(mv, process_error = FALSE),
               log_lik(mv, incl_autocor = FALSE))
  # Named together, the current spelling decides.
  expect_equal(log_lik(mv, incl_autocor = FALSE, process_error = TRUE),
               log_lik(mv, incl_autocor = FALSE))
})


test_that("loo is built on the conditional surface", {
  # An effective parameter count above the observation count is the
  # signature of scoring on the wrong surface: the weights then
  # describe a series the model never saw. The brms twin carries its
  # autocorrelation the same way, so its own count is the reference
  # for what this data should produce.
  got_mv <- with_warnings(loo(mv))
  got_bm <- with_warnings(brms::loo(bm))
  lm_ <- got_mv$value
  lb <- got_bm$value
  n_obs <- nrow(dat)

  expect_lt(lm_$estimates["p_loo", "Estimate"], n_obs)
  expect_lt(
    abs(lm_$estimates["p_loo", "Estimate"] -
          lb$estimates["p_loo", "Estimate"]),
    10
  )
  expect_lt(
    abs(lm_$estimates["elpd_loo", "Estimate"] -
          lb$estimates["elpd_loo", "Estimate"]),
    20
  )
  # Whatever either raised is the Pareto-k notice those numbers
  # already account for, so an unrelated warning still fails. The two
  # packages word it differently -- mvgam says "Some Pareto k
  # diagnostic values are too high", brms names the count and
  # recommends moment matching -- and both spell the diagnostic
  # differently again, so the pattern covers both rather than one.
  k_notice <- "[Pp]areto[ _]k"
  for (got in list(got_mv, got_bm)) {
    expect_true(all(grepl(k_notice, got$warnings)))
  }
  # Both fits strain PSIS here, which is what a latent state-space
  # model does, so each owes the notice rather than staying quiet.
  expect_true(any(grepl(k_notice, got_mv$warnings)))
  expect_true(any(grepl(k_notice, got_bm$warnings)))
})


test_that("waic runs and agrees with loo on this fit", {
  got <- with_warnings(waic(mv))
  w <- got$value
  expect_s3_class(w, "waic")
  expect_true(is.finite(w$estimates["waic", "Estimate"]))
  # Two approximations of the same quantity, so they cannot be far
  # apart on a fit where neither is straining.
  ic <- with_warnings(loo(mv))$value
  expect_lt(
    abs(w$estimates["elpd_waic", "Estimate"] -
          ic$estimates["elpd_loo", "Estimate"]),
    10
  )
})


test_that("the conditional surface is the one hindcast reports", {
  # Both read the fitted state and add it to the observation linear
  # predictor, with no sampling on either side, so this is an
  # equality rather than an agreement: two routes to one quantity.
  ep_cond <- posterior_epred(mv, incl_autocor = TRUE)
  hc <- do.call(cbind, hindcast(mv, type = "expected")$hindcasts)
  expect_equal(unname(ep_cond), unname(hc), tolerance = 1e-12)

  ep_marg <- posterior_epred(mv, incl_autocor = FALSE)
  expect_false(isTRUE(all.equal(unname(ep_marg), unname(hc))))
})


test_that("conditioning tracks the observations, marginalising does not", {
  # The fitted state carries the signal in a state-space fit, and the
  # marginal surface integrates it away on purpose. So the two are
  # told apart by how well each follows the data rather than by any
  # shape, and a method returning the conditional surface for both
  # gives the same correlation twice.
  y <- dat$y
  cor_cond <- stats::cor(colMeans(posterior_epred(mv,
                                                  incl_autocor = TRUE)), y)
  cor_marg <- stats::cor(colMeans(posterior_epred(mv,
                                                  incl_autocor = FALSE)), y)
  expect_gt(cor_cond, 0.9)
  expect_gt(cor_cond, cor_marg)
})


test_that("posterior_predict answers on the surface it is asked for", {
  # Observation noise is drawn per call, so the two are compared
  # through their means rather than draw by draw.
  hc <- do.call(cbind, hindcast(mv, type = "expected")$hindcasts)
  set.seed(3L)
  pp_cond <- posterior_predict(mv, incl_autocor = TRUE, ndraws = 200L)
  set.seed(3L)
  pp_marg <- posterior_predict(mv, incl_autocor = FALSE, ndraws = 200L)
  expect_false(isTRUE(all.equal(unname(pp_cond), unname(pp_marg))))
  expect_gt(
    stats::cor(colMeans(pp_cond), colMeans(hc)),
    stats::cor(colMeans(pp_marg), colMeans(hc))
  )
  expect_gt(stats::cor(colMeans(pp_cond), colMeans(hc)), 0.9)
})


test_that("bayes_R2 agrees with the brms twin", {
  r2_mv <- with_warnings(bayes_R2(mv))$value[, "Estimate"]
  r2_bm <- with_warnings(brms::bayes_R2(bm))$value[, "Estimate"]
  expect_true(is.finite(r2_mv))
  expect_true(r2_mv > 0 && r2_mv <= 1)
  expect_lt(abs(r2_mv - r2_bm), 0.15)
})


test_that("loo_predict pairs its predictions with its weights", {
  # Weights from a conditional density paired with a marginal
  # prediction is the mismatch that pinned `loo_R2` at its clamp.
  # A prediction that tracks the data as poorly as the marginal
  # surface does is the symptom, so the correlation is the check.
  y <- dat$y
  lp_mv <- as.numeric(with_warnings(
    suppressMessages(loo_predict(mv, type = "mean"))
  )$value)
  lp_bm <- as.numeric(with_warnings(
    suppressMessages(brms::loo_predict(bm, type = "mean"))
  )$value)
  expect_length(lp_mv, length(y))
  expect_true(all(is.finite(lp_mv)))
  expect_gt(stats::cor(lp_mv, y), 0.8)
  expect_lt(abs(stats::cor(lp_mv, y) - stats::cor(lp_bm, y)), 0.15)
})


test_that("loo_pit_overlay renders from the PSIS weights", {
  got <- with_warnings(
    pp_check(mv, type = "loo_pit_overlay", ndraws = 100L)
  )
  expect_s3_class(got$value, "ggplot")
})


test_that("logLik is a scalar, and AIC and BIC are built from it", {
  # `logLik()` reduces the matrix to one number so `AIC()` and
  # `BIC()` can dispatch. The two are then fixed arithmetic on it, so
  # a df or nobs read from somewhere else shows up here and nowhere
  # else.
  ll <- logLik(mv)
  expect_s3_class(ll, "logLik")
  expect_length(ll, 1L)
  expect_true(is.finite(as.numeric(ll)))
  expect_gt(attr(ll, "df"), 0L)
  expect_identical(attr(ll, "nobs"), nobs(mv))

  expect_equal(AIC(mv), -2 * as.numeric(ll) + 2 * attr(ll, "df"),
               tolerance = 1e-8)
  expect_equal(BIC(mv),
               -2 * as.numeric(ll) +
                 log(attr(ll, "nobs")) * attr(ll, "df"),
               tolerance = 1e-8)
  # BIC penalises harder than AIC once there are more than seven
  # observations, which fails if `nobs` were the draw count.
  expect_gt(BIC(mv), AIC(mv))
})


test_that("logLik(pointwise = TRUE) is the log_lik matrix itself", {
  ll_mat <- logLik(mv, pointwise = TRUE)
  expect_true(is.matrix(ll_mat))
  expect_identical(ncol(ll_mat), nobs(mv))
  expect_true(all(is.finite(ll_mat)))
  expect_identical(dim(ll_mat), dim(log_lik(mv)))
  # And the scalar is the mean of its row sums, which is the
  # reduction `AIC()` is built on.
  expect_equal(as.numeric(logLik(mv)), mean(rowSums(ll_mat)),
               tolerance = 1e-8)
})


test_that("a joint density is the sum of its responses", {
  # brms's own contract for an `mvbind` fit, and the one place a
  # multivariate fit's arms are combined into a single number. A
  # joint density that dropped an arm is finite, correctly shaped
  # and wrong.
  joint <- log_lik(mv_joint, draw_ids = 1:20)
  per1 <- log_lik(mv_joint, resp = "y1", draw_ids = 1:20)
  per2 <- log_lik(mv_joint, resp = "y2", draw_ids = 1:20)
  expect_identical(dim(joint), dim(per1))
  expect_identical(dim(per1), dim(per2))
  expect_equal(joint, per1 + per2, tolerance = 1e-10)
  # The two arms are different densities, so the sum is not merely
  # one of them doubled.
  expect_false(isTRUE(all.equal(unname(per1), unname(per2))))
})


test_that("each response reads its own predictor plus the one state", {
  # A shared trend is composed against every response the same way,
  # so a response's conditional predictor is its own observation
  # predictor plus that single latent state and nothing else. A
  # method adding the state twice, or adding another response's,
  # stays finite and correctly shaped.
  dm <- as.matrix(posterior::as_draws_matrix(mv_joint$fit))
  state <- mvgam:::extract_trend_latent_states(mv_joint, mv_joint$data,
                                               dm)
  for (r in mv_joint$response_names) {
    obs <- mvgam:::extract_component_linpred(
      mv_joint, newdata = mv_joint$data, component = "obs", resp = r
    )
    expect_equal(
      unname(as.matrix(
        posterior_linpred(mv_joint, resp = r, incl_autocor = TRUE)
      )),
      unname(as.matrix(obs + state))
    )
  }
})


cat("\nDone.\n")
