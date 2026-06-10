# End-to-end residuals path for closure-unit families on a real
# MCMC fit. Loads the cached occ-vs-flocker fixture (rebuilt via
# tests/local/build_fixtures_occ.R) so the integration check
# exercises the full residuals(fit) pipeline against actual Stan
# posterior draws, not a stubbed posterior_predict.

if (file.exists("tests/local/concordance_helpers.R")) {
  source("tests/local/concordance_helpers.R")
} else if (file.exists("concordance_helpers.R")) {
  source("concordance_helpers.R")
}

testthat::skip_if_not_installed("posterior")

require_fixtures("val_occ_simdata.rds", "val_occ_mvgam.rds")

fdir      <- local_fixture_dir()
sim       <- readRDS(file.path(fdir, "val_occ_simdata.rds"))
mvgam_fit <- readRDS(file.path(fdir, "val_occ_mvgam.rds"))

# ------------------------------------------------------------
# Per-unit grain: residuals(fit) returns one row per closure unit,
# not per visit. The fixture covers ~60 sites x 4 visits = 240
# rows in newdata; the per-unit residual summary should have 60.
# ------------------------------------------------------------

n_site <- length(unique(mvgam_fit$data$series))

test_that("residuals(fit) summary returns one row per closure unit", {
  rs <- residuals(mvgam_fit)
  expect_identical(nrow(rs), n_site)
  expect_identical(
    colnames(rs),
    c("Estimate", "Est.Error", "Q2.5", "Q97.5")
  )
})

test_that("residuals(fit, summary = FALSE) returns [ndraws x N_unit] draws", {
  ndraws <- 80L
  rs <- residuals(mvgam_fit, summary = FALSE, ndraws = ndraws)
  expect_identical(nrow(rs), ndraws)
  expect_identical(ncol(rs), n_site)
  expect_true(all(is.finite(rs)))
  # Empirical PIT clamp bounds residuals at |qnorm(eps)| ~ 8.13.
  expect_true(all(abs(rs) <= 8.5))
})

# ------------------------------------------------------------
# Calibration: under a correctly specified model the per-DRAW
# quantile residuals come from a randomised PIT on a coarse
# discrete support (n_rep = 4 visits per site -> 5 mass points);
# the per-draw tail is heavier than N(0, 1) under the discrete
# PIT, which is exactly the artefact warned about in
# ?residuals.mvgam. The posterior-mean residual per site
# averages over the per-draw uniform jitter and pulls back to a
# distribution close to N(0, 1) under correct specification, so
# the loose KS test on the per-site posterior means catches
# gross mis-calibration without false-rejecting from the
# discrete-support tail widening. Dunn & Smyth 1996 §3; DHARMa
# vignette §"Residuals for discrete distributions".
# ------------------------------------------------------------

test_that("per-site posterior-mean quantile residuals stay close to N(0, 1) on the recovery fixture", {
  set.seed(1L)
  rs <- suppressWarnings(residuals(mvgam_fit, ndraws = 500L))
  est <- as.numeric(rs[, "Estimate"])
  ks <- suppressWarnings(stats::ks.test(est, "pnorm"))
  expect_gt(ks$p.value, 0.01)
})

test_that("type = 'ordinary' residuals at unit grain are well-defined and finite", {
  rs <- suppressWarnings(residuals(
    mvgam_fit, type = "ordinary", summary = FALSE, ndraws = 100L
  ))
  expect_identical(dim(rs), c(100L, as.integer(n_site)))
  expect_true(all(is.finite(rs)))
  # Per-unit y_unit ranges from 0 to n_rep (4) on this fixture;
  # ordinary residuals = y_unit - yrep_unit have |value| <= n_rep.
  expect_true(all(abs(rs) <= 4))
})

# ------------------------------------------------------------
# augment() recycles the per-unit residual rows back to the per-
# visit obs frame and tags each row with its closure-unit ID.
# ------------------------------------------------------------

test_that("augment(fit) returns a per-visit tibble with recycled .resid and a .unit column", {
  out <- suppressWarnings(generics::augment(mvgam_fit))
  d <- mvgam_fit$data
  expect_identical(NROW(out), NROW(d))
  expect_true(all(c(".fitted", ".resid", ".resid.se", ".unit")
                  %in% colnames(out)))
  # Rows sharing a `.unit` must share the same `.resid*` values
  # (within-unit recycling). Pick the first unit with > 1 visit
  # and verify the residual columns are constant across visits.
  first_unit <- out$.unit[1L]
  block <- out[out$.unit == first_unit, ]
  if (NROW(block) > 1L) {
    expect_equal(length(unique(block$.resid)), 1L)
    expect_equal(length(unique(block$.resid.se)), 1L)
    expect_equal(length(unique(block$.resid.lower)), 1L)
    expect_equal(length(unique(block$.resid.upper)), 1L)
  }
  # Distinct unit IDs equal the number of closure units.
  expect_identical(length(unique(out$.unit)), n_site)
  # .fitted stays per-visit (psi * p), so it varies within a unit
  # whenever the detection covariate `tod_c` differs across visits.
  expect_gt(length(unique(out$.fitted)), n_site)
})
