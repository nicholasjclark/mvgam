# End-to-end pp_check path for closure-unit families on a real
# MCMC fit. Loads the cached occ fixture and asserts that each
# representative ppc_* type returns a ggplot at the per-unit grain.
# Companion to tests/local/test-closure-unit-residuals.R.

if (file.exists("tests/local/concordance_helpers.R")) {
  source("tests/local/concordance_helpers.R")
} else if (file.exists("concordance_helpers.R")) {
  source("concordance_helpers.R")
}

testthat::skip_if_not_installed("bayesplot")
testthat::skip_if_not_installed("ggplot2")

require_fixtures("val_occ_simdata.rds", "val_occ_mvgam.rds")

fdir      <- local_fixture_dir()
mvgam_fit <- readRDS(file.path(fdir, "val_occ_mvgam.rds"))

# ------------------------------------------------------------
# Discrete-friendly ppc types succeed on the per-unit grain and
# return ggplot objects bayesplot built from (y_unit, yrep_unit).
# ------------------------------------------------------------

test_that("pp_check types that operate on the per-unit count distribution all return ggplots", {
  for (t in c("bars", "rootogram", "ecdf_overlay", "hist",
              "freqpoly", "dens_overlay")) {
    p <- suppressMessages(suppressWarnings(
      pp_check(mvgam_fit, type = t, ndraws = 50L)
    ))
    expect_s3_class(p, "ggplot")
  }
})

test_that("pp_check(type = 'stat') and 'stat_2d' work on the per-unit grain", {
  p1 <- suppressMessages(suppressWarnings(
    pp_check(mvgam_fit, type = "stat",
             stat = function(x) mean(x == 0))
  ))
  expect_s3_class(p1, "ggplot")
  p2 <- suppressMessages(suppressWarnings(
    pp_check(mvgam_fit, type = "stat_2d")
  ))
  expect_s3_class(p2, "ggplot")
})

test_that("pp_check(type = 'resid_hist' / 'resid_qq') consume the per-unit residual draws", {
  p1 <- suppressMessages(suppressWarnings(
    pp_check(mvgam_fit, type = "resid_hist", ndraws = 8L)
  ))
  expect_s3_class(p1, "ggplot")
  p2 <- suppressMessages(suppressWarnings(
    pp_check(mvgam_fit, type = "resid_qq", ndraws = 100L)
  ))
  expect_s3_class(p2, "ggplot")
})

# ------------------------------------------------------------
# Closure-unit-incompatible types raise a clear error pointing
# at the supported alternatives.
# ------------------------------------------------------------

test_that("per-row scatter and per-row time-axis types hard-error with a pointer to supported types", {
  for (t in c("scatter_avg", "error_binned", "resid_acf",
              "resid_vs_fitted")) {
    expect_error(
      suppressMessages(pp_check(mvgam_fit, type = t)),
      "not available for"
    )
  }
})

# ------------------------------------------------------------
# group / x covariates that vary within a closure unit are
# rejected; unit-constant covariates (e.g. site elevation in
# the occ fixture) are accepted by the grouped variants.
# ------------------------------------------------------------

test_that("group = 'tod' (per-visit) is rejected with a clear within-unit message", {
  expect_error(
    suppressMessages(pp_check(
      mvgam_fit, type = "bars_grouped",
      group = "tod_c", ndraws = 50L
    )),
    "not constant within every closure unit"
  )
})

test_that("group = 'elev' (unit-constant) routes through bayesplot and returns a ggplot", {
  # elev varies smoothly across sites; bin into 3 levels so
  # bars_grouped panels remain readable.
  d <- mvgam_fit$data
  mvgam_fit$data$elev_bin <- factor(
    cut(d$elev, breaks = 3, labels = c("low", "mid", "high"))
  )
  p <- suppressMessages(suppressWarnings(
    pp_check(mvgam_fit, type = "ecdf_overlay_grouped",
             group = "elev_bin", ndraws = 50L)
  ))
  expect_s3_class(p, "ggplot")
})
