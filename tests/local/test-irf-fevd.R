## End-to-end coverage of irf.mvgam / fevd.mvgam / stability.mvgam
## against the cached VAR(1) fixture. Sampling-heavy; runs only
## under tests/local (excluded from CI).
##
## Run via:
##   Rscript -e 'devtools::load_all();
##               testthat::test_file("tests/local/test-irf-fevd.R")'

source("setup_tests_local.R")
source("concordance_helpers.R")

require_fixtures("val_mvgam_var_cor.rds")

load_var_fit <- function() {
  readRDS(file.path(local_fixture_dir(), "val_mvgam_var_cor.rds"))
}

test_that("irf() returns the expected nested-list shape from a real VAR(1) fit", {
  fit <- load_var_fit()
  ir <- irf(fit, h = 6L)
  expect_s3_class(ir, "mvgam_irf")
  # One entry per posterior draw, each holding K named matrices.
  ndraws <- length(ir)
  expect_gt(ndraws, 0L)
  K <- length(ir[[1L]])
  expect_identical(K, 3L)
  expect_identical(dim(ir[[1L]][[1L]]), c(6L, K))
  expect_true(all(sapply(ir, function(draw)
    all(sapply(draw, is.finite)))))
})

test_that("irf() respects orthogonal = TRUE vs FALSE", {
  fit <- load_var_fit()
  ir_gen <- irf(fit, h = 4L, orthogonal = FALSE)
  ir_orth <- irf(fit, h = 4L, orthogonal = TRUE)
  expect_identical(attr(ir_gen, "irf_type"), "Generalized")
  expect_identical(attr(ir_orth, "irf_type"), "Orthogonalized")
  # The two parameterisations differ; the orthogonal one is
  # Cholesky-scaled, the generalized one is sigma-scaled.
  expect_false(identical(ir_gen[[1L]], ir_orth[[1L]]))
})

test_that("fevd() produces shares that sum to 1 per response at every horizon", {
  fit <- load_var_fit()
  fv <- fevd(fit, h = 8L)
  expect_s3_class(fv, "mvgam_fevd")
  # For every draw, every response, every horizon, the K shock
  # contributions should sum to 1 exactly.
  row_sums <- unlist(lapply(fv, function(draw) {
    sapply(draw, function(mat) rowSums(mat))
  }))
  expect_true(all(abs(row_sums - 1) < 1e-10))
})

test_that("stability() returns a finite data.frame with all expected metrics", {
  fit <- load_var_fit()
  st <- stability(fit)
  expect_s3_class(st, "data.frame")
  expected_cols <- c(
    "prop_cov_offdiag", "prop_cov_diag", "prop_int", "prop_int_adj",
    "prop_int_offdiag", "prop_int_diag", "reactivity",
    "mean_return_rate", "var_return_rate"
  )
  expect_true(all(expected_cols %in% names(st)))
  for (col in expected_cols) {
    expect_true(all(is.finite(st[[col]])),
                label = paste0("finite: ", col))
  }
  # prop_int is squared determinant, must be in [0, 1) for stable VAR
  expect_true(all(st$prop_int >= 0 & st$prop_int < 1))
  # Mean return rate is max abs eigenvalue, in [0, 1) for stable VAR
  expect_true(all(st$mean_return_rate >= 0 & st$mean_return_rate < 1))
})

test_that("summary() and plot() methods dispatch on irf/fevd outputs", {
  fit <- load_var_fit()
  ir <- irf(fit, h = 4L)
  fv <- fevd(fit, h = 4L)
  expect_s3_class(summary(ir), "data.frame")
  expect_s3_class(summary(fv), "data.frame")
  expect_s3_class(plot(ir, series = 1L), "ggplot")
  expect_s3_class(plot(fv), "ggplot")
})

test_that("irf/fevd/stability all reject non-VAR fits with a consistent message", {
  # Borrow any non-VAR fixture; AR(1) is the simplest available.
  if (!file.exists(file.path(local_fixture_dir(), "val_mvgam_ar1.rds"))) {
    skip("AR(1) fixture not available")
  }
  fit_ar <- readRDS(file.path(local_fixture_dir(), "val_mvgam_ar1.rds"))
  expect_error(irf(fit_ar),       "VAR\\(1\\) latent trend")
  expect_error(fevd(fit_ar),      "VAR\\(1\\) latent trend")
  expect_error(stability(fit_ar), "VAR\\(1\\) latent trend")
})
