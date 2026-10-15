# Tests for `lv_axis()`, the axis a trend term varies along.
# Two layers (CI):
#   1. Unit tests for the AST detector + formula rewriter in
#      `detect_and_rewrite_by_lv()` (cheap, no Stan).
#   2. Validator error paths via `mvgam(... run_model = FALSE)`
#      (validator fires before codegen).
#
# Fitted-model tests are in `tests/local/test-factor-lv-axis.R`.

# 1. AST detector unit tests -------------------------------------------

test_that("detect_and_rewrite_by_lv detects by = lv_axis() and rewrites", {
  f <- ~ s(elev, by = lv_axis()) + gp(lon, lat, by = lv_axis()) - 1
  res <- detect_and_rewrite_by_lv(f)
  expect_true(res$has_by_lv)
  expect_equal(res$n_by_lv, 2L)
  expect_false(res$deprecated_trend_seen)
  expect_match(
    deparse(res$formula),
    "by = \\.trend",
    fixed = FALSE
  )
})

test_that("legacy by = trend triggers deprecation + auto-translate", {
  f <- ~ s(elev, by = trend) - 1
  res <- detect_and_rewrite_by_lv(f)
  expect_true(res$has_by_lv)
  expect_equal(res$n_by_lv, 1L)
  expect_true(res$deprecated_trend_seen)
  expect_match(deparse(res$formula), "by = \\.trend", fixed = FALSE)
})

test_that("lv_axis() is replaced in every position of a term", {
  f <- ~ temp:lv_axis() + (1 + temp | lv_axis()) +
    s(temp, lv_axis(), bs = "fs") + lv_axis()
  res <- detect_and_rewrite_by_lv(f, target = "site")
  expect_equal(res$n_by_lv, 4L)
  expect_equal(
    res$formula,
    ~ temp:site + (1 + temp | site) + s(temp, site, bs = "fs") + site,
    ignore_formula_env = TRUE
  )
})

test_that("the series column is refused in a trend_formula term", {
  # The trend data names the resolved axis `series` whatever the
  # user's column is called, and both names are refused.
  for (f in list(~ s(elev, by = series), ~ elev:site, ~ (1 | site))) {
    expect_error(
      detect_and_rewrite_by_lv(f, series_var = "site"),
      "is not supported in a 'trend_formula' term"
    )
  }
})

test_that("no by-lv terms leaves has_by_lv FALSE", {
  res <- detect_and_rewrite_by_lv(~ s(elev) + s(temp, by = grp) + 1)
  expect_false(res$has_by_lv)
  expect_equal(res$n_by_lv, 0L)
})

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
lv_prefit <- function(trend_formula, n_lv = NULL, dat = lv_dat(1)) {
  mvgam(y ~ -1, trend_formula = trend_formula, data = dat,
        family = gaussian(), run_model = FALSE, silent = 2,
        trend_map = if (!is.null(n_lv)) matrix(NA_real_, 3L, n_lv))
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
          family = gaussian(), run_model = FALSE, silent = 2, ...)
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
          run_model = FALSE, silent = 2),
    "Got n_lv = 3, n_series = 2"
  )
})

# 3. Display: the had_by_lv marker and the rewritten tokens -------------

test_that("display code drops the tokens lv_axis() was rewritten to", {
  expect_false(mvgam_had_by_lv(list()))
  expect_true(mvgam_had_by_lv(list(trend_metadata = list(had_by_lv = TRUE))))
  labs <- list(c("mintemp", "series"), c("ndvi", ".trend"), c("x", "y"),
               "series")
  expect_identical(strip_by_lv_rewrite_tokens(labs, had_by_lv = FALSE), labs)
  # A lone `series` label is kept, which leaves no empty label
  expect_identical(strip_by_lv_rewrite_tokens(labs, had_by_lv = TRUE),
                   list("mintemp", "ndvi", c("x", "y"), "series"))
})
