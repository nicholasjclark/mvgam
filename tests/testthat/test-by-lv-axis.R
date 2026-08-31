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

# 2. Validator acceptance + factor_active rewrite ------------------------

test_that("detect_and_rewrite_by_lv with factor_active=FALSE rewrites to series", {
  f <- ~ s(elev, by = lv_axis()) - 1
  res <- detect_and_rewrite_by_lv(f, factor_active = FALSE)
  expect_true(res$has_by_lv)
  expect_match(deparse(res$formula), "by = series", fixed = FALSE)
  expect_false(grepl(".trend", deparse(res$formula), fixed = TRUE))
})

test_that("detect_and_rewrite_by_lv with factor_active=TRUE rewrites to .trend", {
  f <- ~ s(elev, by = lv_axis()) - 1
  res <- detect_and_rewrite_by_lv(f, factor_active = TRUE)
  expect_true(res$has_by_lv)
  expect_match(deparse(res$formula), "by = .trend", fixed = TRUE)
})

test_that("by = lv_axis() without n_lv is accepted (non-factor path)", {
  set.seed(1)
  dat <- expand.grid(time = 1:30, series = paste0("sp", 1:3))
  dat$series <- factor(dat$series)
  dat$elev <- rep(rnorm(30), times = 3)
  dat$y <- rnorm(nrow(dat))
  # Reason: when no n_lv is set, `by = lv_axis()` is rewritten
  # internally to `by = series` and the standard (time, series)
  # codepath handles the rest. has_by_lv stays FALSE on the
  # trend metadata.
  mod <- suppressWarnings(
    mvgam(
      formula = y ~ -1,
      trend_formula = ~ s(elev, k = 5, by = lv_axis()) - 1,
      data = dat,
      family = gaussian(),
      run_model = FALSE,
      silent = 2
    )
  )
  expect_false(isTRUE(mod$trend_metadata$has_by_lv))
})

test_that("by = lv_axis() with n_lv = n_series is accepted (factor path)", {
  set.seed(2)
  dat <- expand.grid(time = 1:30, series = paste0("sp", 1:3))
  dat$series <- factor(dat$series)
  dat$elev <- rep(rnorm(30), times = 3)
  dat$y <- rnorm(nrow(dat))
  # Reason: n_lv = n_series is a full-rank factor model. The user
  # owns the identifiability decision; a post-fit advisor warns the
  # user when convergence is poor (see flag_by_lv_full_rank_funnel).
  # The validator only rejects n_lv > n_series.
  mod <- suppressWarnings(
    mvgam(
      formula = y ~ -1,
      trend_formula = ~ s(elev, k = 5, by = lv_axis()) - 1,
      trend_map = matrix(NA_real_, nrow = 3L, ncol = 3L),
      data = dat,
      family = gaussian(),
      run_model = FALSE,
      silent = 2
    )
  )
  expect_true(isTRUE(mod$trend_metadata$has_by_lv))
  expect_equal(mod$trend_metadata$n_lv_for_grain, 3L)
})

test_that("by = lv_axis() with n_lv > n_series still errors", {
  set.seed(3)
  dat <- expand.grid(time = 1:30, series = paste0("sp", 1:3))
  dat$series <- factor(dat$series)
  dat$elev <- rep(rnorm(30), times = 3)
  dat$y <- rnorm(nrow(dat))
  suppressWarnings(expect_error(
    mvgam(
      formula = y ~ -1,
      trend_formula = ~ s(elev, k = 5, by = lv_axis()) - 1,
      trend_map = matrix(NA_real_, nrow = 3L, ncol = 4L),
      data = dat,
      family = gaussian(),
      run_model = FALSE,
      silent = 2
    ),
    "cannot exceed|requires a factor model"
  ))
})

# 3. Display-relabel: had_by_lv marker + by_lv_rewrite_tokens helper -----

test_that("by_lv_rewrite_tokens returns both AST rewrite targets", {
  expect_setequal(by_lv_rewrite_tokens(), c(".trend", "series"))
})

test_that("mvgam_had_by_lv reads the trend_metadata marker", {
  expect_false(mvgam_had_by_lv(list()))
  expect_false(mvgam_had_by_lv(list(trend_metadata = list())))
  expect_false(mvgam_had_by_lv(
    list(trend_metadata = list(had_by_lv = FALSE))
  ))
  expect_true(mvgam_had_by_lv(
    list(trend_metadata = list(had_by_lv = TRUE))
  ))
})

test_that("strip_by_lv_rewrite_tokens is identity when had_by_lv = FALSE", {
  cond_labs <- list(c("mintemp", "series"), c("ndvi", "series"))
  expect_identical(
    strip_by_lv_rewrite_tokens(cond_labs, had_by_lv = FALSE),
    cond_labs
  )
})

test_that("strip_by_lv_rewrite_tokens drops series + .trend when had_by_lv = TRUE", {
  cond_labs <- list(
    c("mintemp", "series"),
    c("ndvi", ".trend"),
    c("x", "y")  # no rewrite token present
  )
  expect_identical(
    strip_by_lv_rewrite_tokens(cond_labs, had_by_lv = TRUE),
    list(c("mintemp"), c("ndvi"), c("x", "y"))
  )
})

test_that("strip_by_lv_rewrite_tokens keeps singleton 'series' grouping", {
  # Reason: a user effects = "series" call (rejected upstream for
  # trend_formula, but defensive here) should not collapse to a
  # zero-length label that breaks vapply/paste downstream.
  expect_identical(
    strip_by_lv_rewrite_tokens(list(c("series")), had_by_lv = TRUE),
    list(c("series"))
  )
})

test_that("had_by_lv is set on non-factor by_lv fits (validation only)", {
  set.seed(11)
  dat <- expand.grid(time = 1:30, series = paste0("sp", 1:3))
  dat$series <- factor(dat$series)
  dat$elev <- rep(rnorm(30), times = 3)
  dat$y <- rnorm(nrow(dat))
  mod <- suppressWarnings(
    mvgam(
      formula = y ~ -1,
      trend_formula = ~ s(elev, k = 5, by = lv_axis()) - 1,
      data = dat, family = gaussian(),
      run_model = FALSE, silent = 2
    )
  )
  # Non-factor path: has_by_lv stays FALSE (codegen takes the
  # standard (time, series) path) but had_by_lv is TRUE so display
  # code hides the internal `series` rewrite token.
  expect_false(isTRUE(mod$trend_metadata$has_by_lv))
  expect_true(isTRUE(mod$trend_metadata$had_by_lv))
})

test_that("had_by_lv is set on factor by_lv fits", {
  set.seed(12)
  dat <- expand.grid(time = 1:30, series = paste0("sp", 1:3))
  dat$series <- factor(dat$series)
  dat$elev <- rep(rnorm(30), times = 3)
  dat$y <- rnorm(nrow(dat))
  mod <- suppressWarnings(
    mvgam(
      formula = y ~ -1,
      trend_formula = ~ s(elev, k = 5, by = lv_axis()) - 1,
      trend_map = matrix(NA_real_, nrow = 3L, ncol = 3L),
      data = dat, family = gaussian(),
      run_model = FALSE, silent = 2
    )
  )
  expect_true(isTRUE(mod$trend_metadata$has_by_lv))
  expect_true(isTRUE(mod$trend_metadata$had_by_lv))
})

test_that("had_by_lv stays FALSE for fits without by_lv", {
  set.seed(13)
  dat <- expand.grid(time = 1:30, series = paste0("sp", 1:3))
  dat$series <- factor(dat$series)
  dat$elev <- rep(rnorm(30), times = 3)
  dat$y <- rnorm(nrow(dat))
  mod <- suppressWarnings(
    mvgam(
      formula = y ~ -1,
      trend_formula = ~ s(elev, k = 5) - 1,
      data = dat, family = gaussian(),
      run_model = FALSE, silent = 2
    )
  )
  expect_false(isTRUE(mod$trend_metadata$has_by_lv))
  expect_false(isTRUE(mod$trend_metadata$had_by_lv))
})

test_that("trend_call preserves original by = lv_axis() for update.mvgam", {
  # Reason: update.mvgam reads the original trend_formula via
  # `object$trend_call` (mvgam_update_inheritance table). The
  # rewrite-to-series happens deep inside the validator, but
  # `trend_call` is captured BEFORE that rewrite, so update() round-
  # trips correctly and the new fit re-detects had_by_lv via the
  # AST.
  set.seed(14)
  dat <- expand.grid(time = 1:30, series = paste0("sp", 1:3))
  dat$series <- factor(dat$series)
  dat$elev <- rep(rnorm(30), times = 3)
  dat$y <- rnorm(nrow(dat))
  mod <- suppressWarnings(
    mvgam(
      formula = y ~ -1,
      trend_formula = ~ s(elev, k = 5, by = lv_axis()) - 1,
      data = dat, family = gaussian(),
      run_model = FALSE, silent = 2
    )
  )
  # `trend_call` carries the user's literal text.
  expect_match(
    deparse(mod$trend_call),
    "by = lv_axis()",
    fixed = TRUE
  )
  # `trend_formula` has been rewritten by the AST detector.
  expect_match(
    deparse(mod$trend_formula),
    "by = series",
    fixed = TRUE
  )
})

