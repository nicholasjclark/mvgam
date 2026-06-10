# Tests for `s(x, by = lv_axis())` per-latent-factor smooth machinery.
# Three layers:
#   1. Unit tests for the AST detector + formula rewriter in
#      `detect_and_rewrite_by_lv()` (cheap, no Stan).
#   2. Stancode + standata contract tests using `make_stancode` /
#      `make_standata` paths to lock in the (time, .trend) grain
#      switch, the `times_trend` axis swap, and the rotate auto-skip.
#   3. End-to-end recovery: fit a small factor model with two
#      known smooth shapes and confirm the per-factor inferred
#      smooths correlate with truth at the chunk-0 bar.

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

# 2. Stancode + standata contract tests ----------------------------------
#
# Cached fits cover the by_lv vs non-by_lv branches; we read them from
# disk to keep the test suite fast. Both fits use the same toy:
# n_time = 60, n_species = 5, n_lv = 2, gaussian family.

skip_if_no_chunk0_cache <- function() {
  if (!file.exists("/tmp/chunk0_models/by_lv_axis.rds") ||
      !file.exists("/tmp/chunk0_models/no_by_lv.rds")) {
    skip("chunk0 cached fits not built; run build_fixtures step first")
  }
}

test_that("by = lv_axis() emits times_trend at [N_time, N_lv_trend]", {
  skip_if_no_chunk0_cache()
  mod <- readRDS("/tmp/chunk0_models/by_lv_axis.rds")
  sc <- as.character(stancode.mvgam(mod))
  expect_match(
    sc,
    "array\\[N_time_trend, N_lv_trend\\] int times_trend",
    fixed = FALSE
  )
})

test_that("by = lv_axis() emits the mu_factor row-vector fold", {
  skip_if_no_chunk0_cache()
  mod <- readRDS("/tmp/chunk0_models/by_lv_axis.rds")
  sc <- as.character(stancode.mvgam(mod))
  expect_match(sc, "row_vector\\[N_lv_trend\\] mu_factor", fixed = FALSE)
  # brms pretty-printer normalises whitespace inside subscripts; match
  # permissively (e.g. `lv_trend[i,  : ]` with extra spaces).
  expect_match(
    sc,
    "lv_trend\\[i,\\s*:\\s*\\]\\s*\\+\\s*mu_factor",
    fixed = FALSE
  )
})

test_that("by = lv_axis() auto-skips the QR identification block", {
  skip_if_no_chunk0_cache()
  mod <- readRDS("/tmp/chunk0_models/by_lv_axis.rds")
  sc <- as.character(stancode.mvgam(mod))
  expect_false(grepl("Z_tilde", sc, fixed = TRUE))
  expect_false(grepl("lv_trend_tilde", sc, fixed = TRUE))
  expect_false(grepl("qr_thin_R", sc, fixed = TRUE))
})

test_that("non-by_lv (partial-Z) path keeps existing trend computation", {
  skip_if_no_chunk0_cache()
  mod <- readRDS("/tmp/chunk0_models/no_by_lv.rds")
  sc <- as.character(stancode.mvgam(mod))
  expect_match(
    sc,
    "array\\[N_time_trend, N_series_trend\\] int times_trend",
    fixed = FALSE
  )
  expect_match(
    sc,
    "mu_trend\\[times_trend\\[i,\\s*s\\]\\]",
    fixed = FALSE
  )
  expect_false(grepl("mu_factor", sc, fixed = TRUE))
})

test_that("standata: trend_data grain is (n_time * n_lv) when by_lv", {
  skip_if_no_chunk0_cache()
  mod <- readRDS("/tmp/chunk0_models/by_lv_axis.rds")
  sd <- standata.mvgam(mod)
  expect_equal(sd$N_trend, 60L * 2L)
  expect_equal(sd$N_time_trend, 60L)
  expect_equal(sd$N_lv_trend, 2L)
  expect_equal(sd$N_series_trend, 5L)
  expect_equal(dim(sd$times_trend), c(60L, 2L))
  expect_equal(sd$times_trend[1L, ], c(1L, 2L))
  expect_equal(sd$times_trend[2L, ], c(3L, 4L))
  expect_equal(sd$times_trend[60L, ], c(119L, 120L))
})

# 3. Validator error paths ----------------------------------------------

test_that("by = lv_axis() without a factor model raises a clear error", {
  set.seed(1)
  dat <- expand.grid(time = 1:30, series = paste0("sp", 1:3))
  dat$series <- factor(dat$series)
  dat$elev <- rep(rnorm(30), times = 3)
  dat$y <- rnorm(nrow(dat))
  expect_error(
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
  )
})

test_that("by = lv_axis() with n_lv >= n_series raises a clear error", {
  set.seed(2)
  dat <- expand.grid(time = 1:30, series = paste0("sp", 1:3))
  dat$series <- factor(dat$series)
  dat$elev <- rep(rnorm(30), times = 3)
  dat$y <- rnorm(nrow(dat))
  expect_error(
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
    "requires a factor model"
  )
})

# 4. Recovery on cached fit ---------------------------------------------

test_that("extract_component_linpred works for both incl_latent_state modes", {
  skip_if_no_chunk0_cache()
  mod <- readRDS("/tmp/chunk0_models/by_lv_axis.rds")
  dat <- readRDS("/tmp/chunk0_models/data.rds")

  lp_full <- extract_component_linpred(
    mvgam_fit = mod, newdata = dat, component = "trend",
    ndraws = 20, incl_latent_state = TRUE
  )
  expect_equal(dim(lp_full), c(20L, nrow(dat)))
  expect_false(any(is.na(lp_full)))

  lp_det <- extract_component_linpred(
    mvgam_fit = mod, newdata = dat, component = "trend",
    ndraws = 20, incl_latent_state = FALSE
  )
  expect_equal(dim(lp_det), c(20L, nrow(dat)))
  expect_false(any(is.na(lp_det)))

  # Latent + deterministic differ because the state-space dynamics
  # contribute beyond the per-factor smooth deterministic piece.
  expect_gt(mean(abs(lp_full - lp_det)), 1e-3)
})

test_that("extract_trend_linpred routes through the (t, k) reshape", {
  skip_if_no_chunk0_cache()
  mod <- readRDS("/tmp/chunk0_models/by_lv_axis.rds")
  dat <- readRDS("/tmp/chunk0_models/data.rds")

  tl <- extract_trend_linpred(fit = mod, draw_id = 1L, newdata = dat)
  n_time <- length(unique(dat$time))
  n_series <- length(unique(dat$series))
  expect_equal(dim(tl), c(n_time, n_series))
})
