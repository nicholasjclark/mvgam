# The tests of test-by-lv-axis.R that assemble a model or run for
# 0.2 s or more. CRAN skips this file. CI and local runs take it.
skip_on_cran()

# 2. Validator and codegen via run_model = FALSE ------------------------

# Three series sharing one covariate value per time, as the factor
# grain requires
lv_dat <- function(seed) {
  set.seed(seed)
  dat <- expand.grid(time = 1:30, series = factor(paste0("sp", 1:3)))
  dat$elev <- rep(rnorm(30), times = 3)
  dat$y <- rnorm(nrow(dat))
  dat
}
# A formula naming no constructor takes `ZMVN()`. `n_lv` adds it with
# that many factors.
lv_prefit <- function(trend_formula, n_lv = NULL, dat = lv_dat(1)) {
  if (!is.null(n_lv)) {
    trend_formula <- update(trend_formula,
                            bquote(~ . + ZMVN(n_lv = .(n_lv))))
  }
  mvgam(y ~ -1, trend_formula = trend_formula, data = dat,
        family = gaussian(), run_model = FALSE, silent = 2,
        validate = FALSE)
}

test_that("lv_axis() in a model with no factors is the series column", {
  mod <- lv_prefit(~ s(elev, k = 5, by = lv_axis()) - 1)
  # The trend keeps its (time, series) grain. `had_by_lv` lets display
  # code hide the rewritten series token.
  expect_false(isTRUE(mod$trend_metadata$has_by_lv))
  expect_true(isTRUE(mod$trend_metadata$had_by_lv))
  # `update()` takes the user's own text from `trend_call`
  expect_match(deparse(mod$trend_call), "by = lv_axis()", fixed = TRUE)
  expect_match(deparse(mod$trend_formula), "by = series", fixed = TRUE)
})

test_that("with factors only per-factor slopes rotate with the loadings", {
  # n_lv = n_series is accepted. A post-fit advisor warns of the
  # full-rank funnel.
  smooth <- lv_prefit(~ s(elev, k = 5, by = lv_axis()) - 1, n_lv = 3L)
  expect_true(isTRUE(smooth$trend_metadata$has_by_lv))
  expect_equal(smooth$trend_metadata$n_lv_for_grain, 3L)
  expect_identical(fit_factor_identification(smooth), "none")
  slope <- lv_prefit(~ elev:lv_axis() - 1, n_lv = 2L)
  expect_identical(fit_factor_identification(slope), "rotation")
  expect_match(slope$stancode,
               "b_trend_tilde[by_lv_coef_trend[g]] = Q * b_trend",
               fixed = TRUE)
  expect_identical(
    unname(mvgam_beta_aliases(slope)[paste0("b_elev:.trend", 1:2, "_trend")]),
    c("b_trend_tilde[1]", "b_trend_tilde[2]")
  )
})

test_that("lv_axis() refuses a varying covariate and a single factor", {
  dat <- lv_dat(4)
  dat$elev <- rnorm(nrow(dat))
  refused <- function(trend_formula, ...) {
    mvgam(y ~ 1, trend_formula = trend_formula, data = dat,
          family = gaussian(), run_model = FALSE, silent = 2, ...,
          validate = FALSE)
  }
  # The factor grain holds one covariate value per time, and `elev`
  # differs among the series.
  expect_error(refused(~ elev:lv_axis() + AR(n_lv = 2)),
               "A trend covariate varies within one time")
  expect_error(refused(~ s(time, by = lv_axis()) + AR(n_lv = 1)),
               "requires at least two latent factors")
  expect_error(refused(~ elev:series + AR()),
               "is not supported in a 'trend_formula' term")
  expect_error(lv_prefit(~ s(elev, k = 5, by = lv_axis()) - 1, n_lv = 4L),
               "Got n_lv = 4, n_series = 3")
})

test_that("the n_lv ceiling counts series the responses define", {
  # A frame whose responses are its series has no series column
  set.seed(1)
  wide <- data.frame(time = 1:30, y1 = rpois(30, 3), y2 = rpois(30, 3))
  expect_error(
    mvgam(bf(mvbind(y1, y2) ~ 1) + set_rescor(FALSE),
          trend_formula = ~ AR(n_lv = 3), data = wide, family = poisson(),
          run_model = FALSE, silent = 2, validate = FALSE),
    "Got n_lv = 3, n_series = 2"
  )
})
