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

test_that("the 1.x spelling by = trend is translated with a deprecation", {
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
