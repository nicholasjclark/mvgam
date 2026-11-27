# The tests of test-parameter-predicates.R that assemble a model or run for
# 0.2 s or more. CRAN skips this file. CI and local runs take it.
skip_on_cran()
# One place decides what a parameter name means. These lock that in.
#
# Each predicate answers for several call sites. The tests state the
# rule itself, which holds whichever of those callers changes next.


test_that("a fit keeps the parameters brms keeps", {
  # The working variables a model is built from are named by the
  # response, distributional and non-linear parameters they belong to,
  # and each is reported under another name: `zs_1_1` as `s_1_1`, the
  # standardised deviates `z_1` as `r_g`, the correlation Cholesky
  # `L_1` as `cor_g`.
  dat <- data.frame(
    y = rnorm(20L), x = rnorm(20L), z = rnorm(20L),
    g = factor(rep(letters[1:4], each = 5L))
  )
  form <- brms::bf(y ~ s(x, k = 4) + (1 + x | g), sigma ~ z)
  bfit <- brms::brm(form, data = dat, empty = TRUE)
  sdata <- brms::standata(form, data = dat)

  excluded <- mvgam_excluded_pars(bfit, standata = sdata)
  expect_true(all(
    c("zb", "zbs", "hs_local", "scales", "zs_1_1", "zb_sigma",
      "z_1", "L_1", "Cor_1", "r_1") %in% excluded
  ))
  # What the fit reports is kept: the scaled smooth coefficients, the
  # group-level scales and the coefficients themselves
  expect_false(any(c("s_1_1", "sds_1", "sd_1", "cor_1", "r_1_1", "b",
                     "Intercept", "lprior") %in% excluded))

  # `all = TRUE` keeps everything that is conditional on it
  kept_all <- mvgam_excluded_pars(bfit, standata = sdata,
                                  save_pars = brms::save_pars(all = TRUE))
  expect_false(any(c("zb", "zs_1_1", "z_1", "L_1") %in% kept_all))
  # `manual` names individual parameters to keep
  manual <- mvgam_excluded_pars(
    bfit, standata = sdata, save_pars = brms::save_pars(manual = "zb")
  )
  expect_false("zb" %in% manual)

  # The trend side spells the same names with mvgam's suffix
  trend <- mvgam_excluded_pars(NULL, trend_model = bfit, standata = sdata)
  expect_true(all(c("zb_trend", "z_1_trend", "Cor_1_trend") %in% trend))
})


test_that("a trend with no design declares no mu_trend", {
  # A trend formula without terms leaves `mu_trend` at zero. Declared,
  # it cost an allocation per gradient and stored zeros whose R-hat is
  # undefined. A trend formula with terms builds it and adds it.
  set.seed(3L)
  dat <- data.frame(
    y = rpois(60L, 4), x = rnorm(60L), time = rep(1:30, 2L),
    series = factor(rep(c("a", "b"), each = 30L))
  )
  code_of <- function(trend) {
    stancode(mvgam_formula(y ~ 1, trend_formula = trend), data = dat,
             family = poisson(), validate = FALSE)
  }
  zero <- list(~ RW(), ~ AR(p = 2), ~ VAR(), ~ -1 + AR(cor = TRUE))
  for (trend in zero) {
    expect_false(grepl("\\bmu_trend\\b", code_of(trend)))
  }
  designed <- list(~ x + RW(), ~ s(x, k = 4) + AR())
  for (trend in designed) {
    expect_true(stan_pattern("trend[ : , s] += mu_trend[times_trend[ : , s]];",
                             code_of(trend), fixed = TRUE))
  }
})
