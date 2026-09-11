# One place decides what a parameter name means. These lock that in.
#
# Each predicate answers for several call sites. The tests state the
# rule itself, which holds whichever of those callers changes next.


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
  is_trend_block <- mvgam_par_side(pars) == "trend" &
    mvgam_par_kind(pars) != "state"
  expect_setequal(
    pars[is_trend_block],
    c("sigma_trend[1]", "ar1_trend[1]", "b_x_trend")
  )
})


test_that("one taxonomy answers for every consumer of a name", {
  # The kind and the side are decided once, for the `variable =`
  # keyword resolver, the summary blocks and the bucket builder alike.
  pars <- c(
    "b_Intercept", "b_x", "sigma", "shape",
    "sds_1[1]", "s_1_1[1]", "sd_grp__Intercept", "r_grp[1,1]",
    "b_x_trend", "Intercept_trend", "sigma_trend[1]", "ar1_trend[1]",
    "sds_1_trend[1]", "sd_g__Intercept_trend",
    "trend[1,1]", "lv_trend[1,1]", "mu_trend[1]",
    "innovations_trend[1,1]", "scaled_innovations_trend[1,1]",
    "lscale_1[1]", "zs_1_1[1]", "Z[1,1]", "Z_tilde[1,1]"
  )
  kind <- mvgam_par_kind(pars)
  side <- mvgam_par_side(pars)
  names(kind) <- pars

  # Every state spelling the generated Stan emits is a state, on the
  # one account rather than on three that disagreed.
  expect_setequal(
    pars[kind == "state"],
    c("trend[1,1]", "lv_trend[1,1]", "mu_trend[1]",
      "innovations_trend[1,1]", "scaled_innovations_trend[1,1]")
  )
  # The same prefix means different things on the two sides.
  expect_identical(unname(kind["sigma"]), "family")
  expect_identical(unname(kind["sigma_trend[1]"]), "dynamics")
  # The smooth block is three kinds, because its consumers want
  # three different combinations. `?mvgam_draws` documents the
  # `smooth_params` keyword as the smoothing standard deviations
  # alone, `summary()` reports those with the basis coefficients,
  # and the parameter buckets add the Gaussian-process
  # hyperparameters. Stating the split here is what stops each
  # caller encoding it in a regex of its own.
  expect_setequal(
    pars[kind == "smooth_sd"],
    c("sds_1[1]", "sds_1_trend[1]")
  )
  expect_identical(unname(kind["s_1_1[1]"]), "smooth_coef")
  expect_identical(unname(kind["lscale_1[1]"]), "gp")
  expect_identical(unname(kind["zs_1_1[1]"]), "smooth_coef")
  # A group-level block's scales and the coefficients they govern are
  # two kinds, which is how `tidy()` files them apart.
  expect_identical(unname(kind["sd_grp__Intercept"]), "ranef_sd")
  expect_identical(unname(kind["r_grp[1,1]"]), "ranef_coef")
  expect_identical(unname(kind["sd_g__Intercept_trend"]), "ranef_sd")
  # A rotated fit carries both loading bases and both are loadings;
  # which one a reader sees is settled by the hide filter.
  expect_identical(unname(kind["Z_tilde[1,1]"]), "loading")
  # Loadings bridge the two sides and carry no suffix, so they are
  # named before the side is consulted.
  expect_identical(unname(kind["Z[1,1]"]), "loading")
  expect_identical(unname(side[pars == "Z[1,1]"]), "observation")
  # Nothing falls through unclassified.
  expect_false(any(kind == "other"))
})


test_that("tidy() files each block under broom.mixed's effects class", {
  # A group-level block's standard deviations and correlations are
  # `ran_pars` and its coefficients `ran_vals`; a smooth's penalty and
  # a Gaussian process's scales split from their basis coefficients
  # the same way. brms writes a centred intercept beside `b_Intercept`,
  # one coefficient under two names, and the table reports it once.
  raw <- c(
    "b_Intercept", "Intercept", "b[1]", "sigma",
    "sds_1[1]", "s_1_1[1]", "sdgp_1[1]", "zgp_1[1]",
    "sd_1[1]", "cor_1[1]", "r_1_1[1]", "r_1_1[2]",
    "b_Intercept_trend", "Intercept_trend", "ar1_trend[1]",
    "sd_1_trend[1]", "r_1_1_trend[1]"
  )
  stub <- structure(
    list(fit = posterior::as_draws_matrix(matrix(
      0, nrow = 2L, ncol = length(raw), dimnames = list(NULL, raw)
    ))),
    class = "mvgam"
  )
  spec <- tidy_spec(categorize_mvgam_parameters(stub),
                    stats::setNames(raw, raw))
  filed <- function(type) spec$params[[match(type, spec$type)]]
  effect <- function(type) spec$effect[[match(type, spec$type)]]

  expect_setequal(filed("observation_beta"), c("b_Intercept", "b[1]"))
  expect_identical(filed("observation_family_extra_param"), "sigma")
  expect_setequal(filed("observation_smooth_param"),
                  c("sds_1[1]", "sdgp_1[1]"))
  expect_setequal(filed("observation_smooth_coef"),
                  c("s_1_1[1]", "zgp_1[1]"))
  expect_setequal(filed("random_effect_group_level"),
                  c("sd_1[1]", "cor_1[1]"))
  expect_identical(filed("random_effect_beta"), c("r_1_1[1]", "r_1_1[2]"))
  expect_identical(filed("trend_beta"), "b_Intercept_trend")
  expect_identical(filed("trend_model_param"), "ar1_trend[1]")
  expect_identical(filed("trend_random_effect_group_level"), "sd_1_trend[1]")
  expect_identical(filed("trend_random_effect_beta"), "r_1_1_trend[1]")
  expect_identical(effect("random_effect_beta"), "ran_vals")
  expect_identical(effect("random_effect_group_level"), "ran_pars")
  expect_identical(effect("trend_random_effect_beta"), "ran_vals")

  # The composer reads the trend's intercept on the data's scale, as it
  # reads the observation model's.
  expect_true("b_Intercept_trend" %in% side_parameters(stub, "trend"))
  expect_true("b_Intercept" %in% side_parameters(stub, "obs"))

  # The table itself: one intercept, and a request no parameter
  # answers keeps its columns.
  small <- c("b_Intercept", "Intercept", "sigma")
  stub$fit <- posterior::as_draws_matrix(matrix(
    stats::rnorm(6L), nrow = 2L, dimnames = list(NULL, small)
  ))
  expect_setequal(tidy(stub)$term, c("b_Intercept", "sigma"))
  none <- tidy(stub, effects = "ran_vals")
  expect_identical(nrow(none), 0L)
  expect_identical(names(none),
                   c("term", "type", "estimate", "std.error", "conf.low",
                     "conf.high"))
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
