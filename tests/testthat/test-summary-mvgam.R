# CI-safe tests for the helpers that decide which block of a summary
# each parameter belongs to, and for the header those blocks sit
# under. Nothing here samples: every helper takes a formula, a family
# or a vector of parameter names.


test_that("get_dpar_names() reads the formula rather than the names", {
  # A parameter given its own formula is recorded in `pforms`.
  expect_equal(
    get_dpar_names(brms::bf(y ~ x, sigma ~ x)),
    "sigma"
  )
  expect_equal(
    sort(get_dpar_names(brms::bf(y ~ x, sigma ~ x, nu ~ z))),
    c("nu", "sigma")
  )
  # A plain formula names none.
  expect_equal(get_dpar_names(brms::bf(y ~ x)), character())
  # Reading the parameter names instead would see `b_body_mass` and
  # invent a parameter called `body`, which is why the formula is the
  # source of truth.
  expect_equal(get_dpar_names(brms::bf(y ~ body_mass)), character())
  # Multivariate formulas union over their arms.
  mv <- brms::bf(y1 ~ x, sigma ~ x) + brms::bf(y2 ~ x, nu ~ x)
  expect_equal(sort(get_dpar_names(mv)), c("nu", "sigma"))
})


test_that("dpar_alternation() builds one group, or none", {
  expect_null(dpar_alternation(character()))
  expect_equal(dpar_alternation("sigma"), "(sigma)")
  expect_equal(dpar_alternation(c("sigma", "nu")), "(sigma|nu)")
})


test_that("a parameter's own block is the only one it appears in", {
  pars <- c("b_Intercept", "b_x", "b_sigma_Intercept", "b_sigma_x",
            "s_z_1[1]", "sds_z_1", "s_sigma_z_1[1]", "sds_sigma_z_1")
  dpars <- "sigma"

  # Population-level effects hold the mean's coefficients only.
  expect_equal(
    pars[match_fixed_pars(pars, dpars)],
    c("b_Intercept", "b_x")
  )
  # The dpar's coefficients go to the dpar block instead.
  expect_equal(
    pars[match_dpar_fixed_pars(pars, dpars)],
    c("b_sigma_Intercept", "b_sigma_x")
  )
  # Smooths split the same way.
  expect_equal(
    pars[match_smooth_pars(pars, dpars)],
    c("s_z_1[1]", "sds_z_1")
  )
  expect_equal(
    pars[match_dpar_smooth_pars(pars, dpars)],
    c("s_sigma_z_1[1]", "sds_sigma_z_1")
  )
  # With no distributional parameters nothing is held back.
  expect_equal(
    pars[match_fixed_pars(pars, character())],
    c("b_Intercept", "b_x", "b_sigma_Intercept", "b_sigma_x")
  )
})


test_that("the summary header carries every formula and every link", {
  # A distributional formula is printed under the response formula,
  # as brms does, so a reader can see what was modelled.
  expect_equal(
    format_model_formula(brms::bf(y ~ x, sigma ~ x)),
    c("y ~ x", "sigma ~ x")
  )
  expect_equal(format_model_formula(brms::bf(y ~ x)), "y ~ x")

  # A link is named for every parameter the family carries, not just
  # the mean, so a coefficient in the sigma block can be read on the
  # right scale.
  expect_equal(format_family_links(brms::Beta()), "mu = logit; phi = log")
  expect_equal(format_family_links(com_binomial()),
               "mu = logit; nu = identity")
  # Families declaring no distributional parameters report the mean.
  expect_equal(format_family_links(poisson()), "mu = log")
})


test_that("is_trend_state_param() names the states and nothing else", {
  # The trend's time-indexed states, which no summary block claims.
  expect_true(all(is_trend_state_param(c(
    "trend[1,1]", "lv_trend[3,2]", "lv_trend_tilde[3,2]",
    "innovations_trend[1,1]", "mu_trend[4]",
    "scaled_innovations_trend[2,1]"
  ))))
  # Trend hyperparameters and formula effects are summarised, so they
  # must not be caught by the same predicate.
  expect_false(any(is_trend_state_param(c(
    "sigma_trend", "ar1_trend[1]", "Intercept_trend", "b_trend[1]",
    "Sigma_trend[1,1]", "L_Omega_trend[2,1]", "Z[1,2]"
  ))))
  # `latent_state` is the closure-unit quantity, not this one.
  expect_false(is_trend_state_param("latent_N[1]"))
  expect_equal(is_trend_state_param(character()), logical(0))
})


test_that("no summary block claims a trend state", {
  # Every block is built by a `match_*` predicate. If none of them
  # admits `trend[i, s]`, then carrying those rows further only to
  # discard them is wasted work, which is what the removed
  # `include_states` argument did: it kept them and changed nothing.
  states <- c("trend[1,1]", "lv_trend[2,1]", "innovations_trend[1,1]",
              "mu_trend[3]", "scaled_innovations_trend[1,1]")
  matchers <- list(
    fixed = function(p) match_fixed_pars(p, character()),
    smooth = function(p) match_smooth_pars(p, character()),
    random = match_random_pars,
    family = function(p) match_family_pars(p, character()),
    trend_fixed = match_trend_fixed_pars,
    trend_smooth = match_trend_smooth_pars,
    trend_random = match_trend_random_pars,
    trend_specific = match_trend_specific_pars,
    loadings = match_z_loadings,
    loadings_prior = match_loadings_prior_pars
  )
  for (nm in names(matchers)) {
    expect_false(any(matchers[[nm]](states)))
  }
})


test_that("summary() offers no argument for the trend states", {
  # Removed rather than fixed: 1.1.x never had one, and the states
  # are reachable through hindcast(type = "trend"),
  # as.data.frame(variable = "^trend\\[") and plot(type = "trend").
  expect_false("include_states" %in% names(formals(summary.mvgam)))
  expect_false("include_trend_states" %in% names(formals(summary.mvgam)))
  expect_false(
    "include_trend_states" %in% names(formals(summary.mvgam_pooled))
  )
})
