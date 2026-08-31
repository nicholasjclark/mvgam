# `normalize` decides whether Stan's sampling statements carry their
# normalising constants. It changes what is added to `target` by a
# constant and must leave the posterior alone. Asserting that on two
# real fits is the only check that catches a trend which never reaches
# the likelihood: the broken program compiled, sampled, converged, and
# returned plausible parameters.

context("normalize concordance")

require_fixtures("val_mvgam_normalize_on.rds", "val_mvgam_normalize_off.rds")

on_fit <- load_mvgam("normalize_on")
off_fit <- load_mvgam("normalize_off")

posterior_mean <- function(fit, variable) {
  draws <- as_draws_df(fit)
  mean(draws[[variable]])
}

# The spread of the posterior-mean trend across cells. A fit that
# dropped the trend leaves the latent states at their prior, which
# averages to roughly nothing.
trend_spread <- function(fit) {
  draws <- as_draws_df(fit)
  cols <- grep("^trend\\[", names(draws), value = TRUE)
  stats::sd(vapply(cols, function(cc) mean(draws[[cc]]), numeric(1L)))
}

test_that("normalize = FALSE keeps the trend in the likelihood", {
  # The defect this pins: the program held the untransformed GLM call,
  # so `trend` was computed in transformed parameters and never used.
  expect_true(grepl("mu\\[n\\] \\+= trend\\[", off_fit$stancode))
  expect_true(grepl("to_matrix\\(mu\\)", off_fit$stancode))

  # And the constants stay off, or the setting was silently undone.
  expect_true(grepl("_glm_lup[dm]f", off_fit$stancode))
  expect_false(grepl("_glm_lp[dm]f", off_fit$stancode))

  # The trend has to be doing work, not sitting at its prior.
  expect_gt(trend_spread(off_fit), 0.2)
})

test_that("the two settings reach the same posterior", {
  # Same data, same seed, same sampler budget. Differences are Monte
  # Carlo noise, so the tolerance is generous in absolute terms and
  # still an order of magnitude tighter than the gap a missing trend
  # would open.
  for (variable in c("b_x1", "Intercept", "ar1_trend[1]",
                     "sigma_trend[1]")) {
    expect_equal(
      posterior_mean(off_fit, variable),
      posterior_mean(on_fit, variable),
      tolerance = 0.15,
      label = paste("posterior mean of", variable)
    )
  }

  expect_equal(trend_spread(off_fit), trend_spread(on_fit),
               tolerance = 0.15)

  # Predictive accuracy should match too, since the models are the same.
  elpd <- function(fit) {
    suppressWarnings(loo(fit))$estimates["elpd_loo", "Estimate"]
  }
  expect_equal(elpd(off_fit), elpd(on_fit), tolerance = 0.05)
})
