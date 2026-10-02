# CI-safe tests for the helpers that decide which block of a summary
# each parameter belongs to, and for the header those blocks sit
# under. Nothing here samples: every helper takes a formula, a family
# or a vector of parameter names.


test_that("a block's rows print under brms's names", {
  pars <- c("bs_sigma_sz_1", "sds_sigma_sz_1_trend", "sd_g__Intercept",
            "cor_g__Intercept__x")
  tab <- data.frame(Estimate = seq_along(pars), row.names = pars)
  block <- function(key) Filter(function(b) b$key == key, summary_blocks())[[1L]]
  expect_equal(
    c(rownames(summary_block_rows(tab[1, , drop = FALSE], block("fixed"))),
      rownames(summary_block_rows(tab[2, , drop = FALSE], block("smooth")))),
    c("sigma_sz_1", "sds(sigma_sz_1)")
  )
  expect_equal(
    rownames(summary_block_rows(tab[3:4, , drop = FALSE], block("random"), "g")$g),
    c("sd(Intercept)", "cor(Intercept,x)")
  )
})


test_that("the summary header carries every formula and every link", {
  # A distributional formula is printed under the response formula,
  # as brms does, so a reader can see what was modelled.
  expect_equal(
    format_model_formula(brms::bf(y ~ x, sigma ~ x)),
    c("y ~ x", "sigma ~ x")
  )

  # A link is named for every parameter the family carries, not just
  # the mean, so a coefficient in the sigma block can be read on the
  # right scale.
  expect_equal(format_family_links(brms::Beta()), "mu = logit; phi = log")
  expect_equal(format_family_links(com_binomial()),
               "mu = logit; nu = identity")
  # Families declaring no distributional parameters report the mean.
  expect_equal(format_family_links(poisson()), "mu = log")
})


test_that("the trend line names the order and drops an empty formula", {
  # A summary object carries no trend metadata, and the helper that
  # renders the order needs it. `summary()` stores the rendered
  # label. Without the stored label a fitted ARMA printed as
  # `Trends: None`, denying a trend the sampler had estimated.
  expect_equal(
    format_trend_line(list(trend_model = "AR",
                           trend_label = "ARMA(1, 1)")),
    " Trends: ARMA(1, 1) "
  )
  # A fit saved before the label existed still names its type.
  expect_equal(
    format_trend_line(list(trend_model = "AR")),
    " Trends: AR "
  )
  # A formula with an intercept alone names no predictor, and a
  # two-sided one drops its response.
  expect_null(trend_predictors(list(trend_formula = ~1)))
  expect_identical(trend_predictors(list(trend_formula = y ~ elev)), "~elev")
  # The user's spelling prints, less the constructor. The stored copy
  # holds the series column in place of `lv_axis()`, and a `- 1`.
  expect_identical(
    trend_predictors(list(trend_call = ~ s(x, by = lv_axis()) + AR())),
    "~s(x, by = lv_axis())"
  )
  # A fit with no trend prints no line.
  expect_equal(format_trend_line(list()), "")
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
  # `latent_N` is a closure-unit family's latent abundance.
  expect_false(is_trend_state_param("latent_N[1]"))
})

test_that("summaries skip a loading fixed at zero in every draw", {
  draws <- posterior::draws_array(
    `Z_tilde[1,1]` = rnorm(20), `Z_tilde[1,2]` = rep(0, 20)
  )
  expect_identical(
    varying_pars(draws, c("Z_tilde[1,1]", "Z_tilde[1,2]")), "Z_tilde[1,1]"
  )
})
