# One place decides what a parameter name means. These lock that in.
#
# Each predicate replaced a test written out at several call sites. The
# risk they guard against is not that the sites disagree today but that
# the next change touches one of them, so the tests state the rule
# rather than the current behaviour of any one caller.


test_that("a trend parameter is one whose name ends in the suffix", {
  # Every trend parameter the package emits carries `_trend` at the end
  # of the name, or immediately before the index.
  emitted <- c(
    "sigma_trend[1]", "ar1_trend[1]", "b_x_trend", "alpha_cor_trend",
    "Sigma_group_trend[1,1]", "L_Omega_trend[1,1]", "lv_trend[1,1]",
    "innovations_trend[1,1]", "sds_1_trend[1]", "r_1_1_trend[1]"
  )
  expect_true(all(is_trend_parameter(emitted)))

  # A covariate is named by the user, and a user may name one after a
  # trend. Asking whether the name merely contains `_trend` reads
  # `b_pre_trend_score` as trend-side, which moves an
  # observation-level coefficient out of the block it belongs in and
  # leaves it out of the population-level effects entirely.
  covariates <- c("b_pre_trend_score", "b_trending", "b_trend_x",
                  "b_Intercept", "b_x", "sigma", "Intercept")
  expect_false(any(is_trend_parameter(covariates)))
})


test_that("the trend suffix decides which block a parameter prints in", {
  pars <- c("b_Intercept", "b_x", "b_pre_trend_score", "b_trending",
            "sigma_trend[1]", "ar1_trend[1]", "b_x_trend")
  # Observation-side effects keep the coefficients the user asked for.
  expect_setequal(
    pars[match_fixed_pars(pars, character())],
    c("b_Intercept", "b_x", "b_pre_trend_score", "b_trending")
  )
  # The trend block holds only what the trend model emitted.
  expect_setequal(
    pars[match_trend_pars(pars)],
    c("sigma_trend[1]", "ar1_trend[1]", "b_x_trend")
  )
})


test_that("identified factor parameters are recognised in one place", {
  # A free-loading factor fit is rotated to a canonical form and the
  # result emitted alongside the raw draws it came from.
  rotated <- c("Z[1,1]", "Z_tilde[1,1]", "lv_trend[1,1]",
               "lv_trend_tilde[1,1]")
  expect_true(has_identified_loadings(rotated))
  expect_true(has_identified_factor_states(rotated))
  # The pattern helpers answer from the same test, so they cannot
  # disagree with it.
  expect_equal(factor_loading_param_pattern(rotated), "^Z_tilde\\[")
  expect_equal(factor_state_param_pattern(rotated), "^lv_trend_tilde\\[")
  # A rotated fit carries both bases, and asking for the model
  # basis must give the loadings the dynamics are stated for.
  expect_equal(factor_loading_param_pattern(rotated, "model"),
                 "^Z\\[")
  expect_equal(
    factor_loading_param_pattern(rotated, "identified"),
    "^Z_tilde\\["
  )

  # A fixed-loading fit has neither, and falls back to the raw names.
  raw <- c("Z[1,1]", "lv_trend[1,1]", "sigma_trend[1]")
  expect_false(has_identified_loadings(raw))
  expect_false(has_identified_factor_states(raw))
  expect_equal(factor_loading_param_pattern(raw), "^Z\\[")
  expect_equal(factor_state_param_pattern(raw), "^lv_trend\\[")

  # A parameter merely containing the name is not the rotated form.
  expect_false(has_identified_loadings("b_Z_tilde_score"))
  expect_false(has_identified_factor_states("mu_lv_trend_tilde"))
})


test_that("an autoregressive coefficient is told from its hyperparameters", {
  expect_true(all(is_ar_coefficient(c("ar1_trend", "ar2_trend",
                                      "ar12_trend"))))
  # The hierarchical mean and spread of a coefficient are different
  # parameters and carry their own priors.
  expect_false(any(is_ar_coefficient(c("mu_ar1_trend", "sigma_ar1_trend"))))
  # As are the indexed draws and anything merely starting the same way.
  expect_false(any(is_ar_coefficient(c("ar1_trend[1]", "ar_trend",
                                       "artefact_trend"))))
})
