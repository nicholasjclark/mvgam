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
