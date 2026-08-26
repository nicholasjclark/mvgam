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
  ir <- irf(fit, h = 6L, summary = FALSE)
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
  ir_gen <- irf(fit, h = 4L, orthogonal = FALSE, summary = FALSE)
  ir_orth <- irf(fit, h = 4L, orthogonal = TRUE, summary = FALSE)
  expect_identical(attr(ir_gen, "irf_type"), "Generalized")
  expect_identical(attr(ir_orth, "irf_type"), "Orthogonalized")
  # The two parameterisations differ; the orthogonal one is
  # Cholesky-scaled, the generalized one is sigma-scaled.
  expect_false(identical(ir_gen[[1L]], ir_orth[[1L]]))
})

test_that("fevd() produces shares that sum to 1 per response at every horizon", {
  fit <- load_var_fit()
  fv <- fevd(fit, h = 8L, summary = FALSE)
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
  st <- stability(fit, summary = FALSE)
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
  ir <- irf(fit, h = 4L, summary = FALSE)
  fv <- fevd(fit, h = 4L, summary = FALSE)
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


test_that("the response surfaces summarise unless asked for their draws", {
  fit <- load_var_fit()

  # The default answers with the posterior median and interval of each
  # shock-response pair. The draws behind it are one K by K matrix per
  # horizon per draw, which on a wide panel is orders of magnitude
  # larger than anything a reader wants in hand.
  ir <- irf(fit, h = 5L)
  expect_s3_class(ir, "mvgam_irf_summary")
  expect_true(all(c("shock", "horizon") %in% names(ir)))
  expect_true(any(grepl("Q50$", names(ir))))
  expect_lt(as.numeric(object.size(ir)),
            as.numeric(object.size(irf(fit, h = 5L, summary = FALSE))))

  fv <- fevd(fit, h = 5L)
  expect_s3_class(fv, "mvgam_fevd_summary")
  # Every horizon of every pair is reported once.
  expect_equal(nrow(fv), length(unique(fv$shock)) * 5L)

  # Summarising a summary returns the same table rather than trying to
  # take quantiles of quantiles.
  expect_equal(nrow(summary(ir)), nrow(ir))

  # Both objects plot: the summary from its quantiles, the draws from
  # the bands they imply.
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_s3_class(plot(ir), "ggplot")
  expect_s3_class(plot(fv), "ggplot")
  expect_s3_class(plot(irf(fit, h = 5L, summary = FALSE), series = 1), "ggplot")
  # A pair that was never computed is named rather than silently empty.
  expect_error(plot(ir, shocks = "nonexistent"), "Unknown shock-response")
})


test_that("the response surfaces answer from the draws they were given", {
  fit <- load_var_fit()
  total <- ndraws(fit)

  # Naming draws is honoured, and the coefficients and the innovation
  # covariance must come from the same ones: a response built from a
  # transition matrix of one draw and a covariance of another describes
  # no posterior sample at all.
  ids <- c(2L, 7L, 15L)
  ir <- irf(fit, h = 4L, draw_ids = ids, summary = FALSE)
  expect_length(ir, length(ids))
  expect_equal(irf(fit, h = 4L, draw_ids = ids, summary = FALSE), ir)

  # A count is honoured too, and asking for fewer draws returns fewer.
  expect_length(irf(fit, h = 4L, ndraws = 5L, summary = FALSE), 5L)
  expect_length(fevd(fit, h = 4L, ndraws = 5L, summary = FALSE), 5L)
  expect_length(irf(fit, h = 4L, summary = FALSE), total)

  # Asking for more draws than exist is refused rather than truncated.
  expect_error(irf(fit, h = 4L, ndraws = total + 1L),
               "more draws than the posterior holds")
})


test_that("stability() summarises unless asked for its draws", {
  fit <- load_var_fit()

  # The default reports each metric once, matching every other
  # post-fit surface in the package.
  st <- stability(fit)
  expect_s3_class(st, "mvgam_stability_summary")
  expect_equal(nrow(st), 9L)
  expect_true(all(c("metric", "Estimate", "Est.Error") %in% names(st)))

  # The draws remain available, and are what the histograms are of.
  draws <- stability(fit, summary = FALSE)
  expect_s3_class(draws, "mvgam_stability")
  expect_equal(nrow(draws), ndraws(fit))
  expect_equal(summary(draws)$Estimate, st$Estimate, tolerance = 1e-10)

  # Each draw costs a Lyapunov solve, so the metrics can be answered
  # from a subset.
  expect_equal(nrow(stability(fit, ndraws = 5L, summary = FALSE)), 5L)
  expect_error(stability(fit, ndraws = ndraws(fit) + 1L),
               "more draws than the posterior holds")

  # The summary carries each metric's binned posterior, so it draws the
  # same histogram the draws do. A median and an interval alone would
  # not say whether reactivity's mass crosses zero, which is usually
  # why the metric was asked for.
  bins <- attr(st, "bin_counts")
  expect_equal(length(bins), 9L)
  expect_equal(sum(bins$reactivity$counts), ndraws(fit))
  ref <- graphics::hist(
    draws$reactivity,
    breaks = seq(min(draws$reactivity), max(draws$reactivity),
                 length.out = 31L),
    plot = FALSE
  )
  expect_equal(bins$reactivity$counts, as.integer(ref$counts))

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_s3_class(plot(st), "ggplot")
  expect_s3_class(plot(st, intervals = TRUE), "ggplot")
  expect_s3_class(plot(draws), "ggplot")
})
