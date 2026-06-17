# Tests for `s(x, by = lv_axis())` per-latent-factor smooth machinery.
# Two layers (CI):
#   1. Unit tests for the AST detector + formula rewriter in
#      `detect_and_rewrite_by_lv()` (cheap, no Stan).
#   2. Validator error paths via `mvgam(... run_model = FALSE)`
#      (validator fires before codegen).
#
# Stancode + standata contract tests and recovery tests against
# cached fits live in `tests/local/test-by-lv-axis-cached-fits.R`.

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

test_that("by = series in trend_formula is hard rejected", {
  f <- ~ s(elev, by = series) - 1
  expect_error(
    detect_and_rewrite_by_lv(f),
    "'by = series' is not allowed inside 'trend_formula'"
  )
})

test_that("no by-lv terms leaves has_by_lv FALSE", {
  f <- ~ s(elev) + s(temp, by = grp) + 1
  res <- detect_and_rewrite_by_lv(f)
  expect_false(res$has_by_lv)
  expect_equal(res$n_by_lv, 0L)
})

test_that("mixed by_lv + non-by_lv smooths handled correctly", {
  f <- ~ s(elev, by = lv_axis()) + s(temp) - 1
  res <- detect_and_rewrite_by_lv(f)
  expect_true(res$has_by_lv)
  expect_equal(res$n_by_lv, 1L)
  expect_match(deparse(res$formula), "by = \\.trend", fixed = FALSE)
  expect_match(deparse(res$formula), "s\\(temp\\)", fixed = FALSE)
})

test_that("lv_axis() returns NULL invisibly", {
  expect_null(lv_axis())
})

# 2. Validator error paths ----------------------------------------------

test_that("by = lv_axis() without a factor model raises a clear error", {
  set.seed(1)
  dat <- expand.grid(time = 1:30, series = paste0("sp", 1:3))
  dat$series <- factor(dat$series)
  dat$elev <- rep(rnorm(30), times = 3)
  dat$y <- rnorm(nrow(dat))
  # The validator should error before mvgam even reaches its
  # `run_model = FALSE` deprecation path; suppress that warning so
  # it doesn't leak into the testthat summary if the order of
  # checks ever changes.
  suppressWarnings(expect_error(
    mvgam(
      formula = y ~ -1,
      trend_formula = ~ s(elev, k = 5, by = lv_axis()) - 1,
      trend_model = ZMVN(cor = TRUE),
      data = dat,
      family = gaussian(),
      run_model = FALSE,
      silent = 2
    ),
    "requires a factor model"
  ))
})

test_that("by = lv_axis() with n_lv >= n_series raises a clear error", {
  set.seed(2)
  dat <- expand.grid(time = 1:30, series = paste0("sp", 1:3))
  dat$series <- factor(dat$series)
  dat$elev <- rep(rnorm(30), times = 3)
  dat$y <- rnorm(nrow(dat))
  # `validate_n_lv_ceiling()` intercepts this earlier than the
  # factor-model gate now; the iid loadings prior requires
  # `n_lv < n_series` strictly. The MGP relaxation is mentioned in
  # the error so users see the escape hatch.
  suppressWarnings(expect_error(
    mvgam(
      formula = y ~ -1,
      trend_formula = ~ s(elev, k = 5, by = lv_axis()) - 1,
      trend_model = ZMVN(cor = TRUE),
      trend_map = matrix(NA_real_, nrow = 3L, ncol = 3L),
      data = dat,
      family = gaussian(),
      run_model = FALSE,
      silent = 2
    ),
    "n_lv.*strictly less than the number of series"
  ))
})

