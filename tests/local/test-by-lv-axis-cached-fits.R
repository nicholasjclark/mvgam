# Local tests for `s(x, by = lv_axis())` machinery that need a real
# fitted mvgam object on disk. The fits are cached at
# `/tmp/chunk0_models/` after the first interactive run; subsequent
# runs read straight from disk.
#
# Run interactively with:
#   Rscript -e 'devtools::load_all(); testthat::test_file(
#     "tests/local/test-by-lv-axis-cached-fits.R")'
#
# Fixture build (one-off; produces /tmp/chunk0_models/*.rds):
#   - data: n_time = 60, n_species = 5, n_lv = 2, gaussian
#   - by_lv_axis.rds: trend_formula = ~ s(elev, k = 5,
#       by = lv_axis()) - 1, ZMVN(cor = TRUE), trend_map =
#       matrix(NA_real_, 5, 2)
#   - no_by_lv.rds: trend_formula = ~ s(elev, k = 5) - 1, same
#       trend_model + trend_map (partial-Z path without by_lv)

skip_if_no_chunk0_cache <- function() {
  if (!file.exists("/tmp/chunk0_models/by_lv_axis.rds") ||
      !file.exists("/tmp/chunk0_models/no_by_lv.rds")) {
    skip("chunk0 cached fits not built; run build_fixtures step first")
  }
}

# 1. Stancode + standata contract tests on cached fits ------------------

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

# 2. Recovery on cached fit ---------------------------------------------

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
