# End-to-end residual_cor integration tests on real cached fits.
#
# Lives in tests/local because it requires the val_mvgam_var_cor /
# val_mvgam_hier_ar_cor fixtures from tests/local/build_fixtures.R.
# Run via:
#   Rscript -e 'devtools::load_all();
#               testthat::test_file("tests/local/test-residual-cor.R")'

source("setup_tests_local.R")
source("concordance_helpers.R")


# ---- VAR(1) correlated trend -----------------------------------------

test_that("residual_cor on VAR(1) returns a valid p x p correlation matrix", {
  require_fixtures("val_mvgam_var_cor.rds")
  fit <- load_mvgam("var_cor")
  res <- residual_cor(fit)
  testthat::expect_s3_class(res, "mvgam_residcor")
  testthat::expect_equal(res$pattern, "full_covariance")
  testthat::expect_identical(res$n_series, 3L)
  # Cor matrix invariants
  testthat::expect_true(all(diag(res$cor) == 1))
  testthat::expect_true(isSymmetric(unname(res$cor), tol = 1e-8))
  testthat::expect_true(all(res$cor >= -1 & res$cor <= 1))
  # CI containment on the off-diagonals (Fisher-z back-transformed)
  off <- !diag(TRUE, 3L)
  testthat::expect_true(all(res$cor_lower[off] <= res$cor[off]))
  testthat::expect_true(all(res$cor_upper[off] >= res$cor[off]))
  # Posterior probability bookkeeping
  testthat::expect_true(all(res$prob_positive + res$prob_negative <= 1 + 1e-8))
  testthat::expect_true(all(res$prob_nonzero >= 0 & res$prob_nonzero <= 1))
})


test_that("VAR(1) residual_cor partial = TRUE populates prec fields", {
  require_fixtures("val_mvgam_var_cor.rds")
  fit <- load_mvgam("var_cor")
  res <- residual_cor(fit, partial = TRUE)
  testthat::expect_true(!is.null(res$prec))
  testthat::expect_equal(dim(res$prec), c(3L, 3L))
  testthat::expect_true(all(diag(res$prec) == 1))
  testthat::expect_true(all(res$prec >= -1 & res$prec <= 1))
})


test_that("VAR(1) summary returns tidy tibble with 3 unique pairs", {
  require_fixtures("val_mvgam_var_cor.rds")
  fit <- load_mvgam("var_cor")
  s <- summary(residual_cor(fit))
  testthat::expect_s3_class(s, "tbl_df")
  testthat::expect_equal(nrow(s), 3L)  # 3-choose-2
  testthat::expect_setequal(
    colnames(s),
    c("series_1", "series_2", "Estimate", "Est.Error",
      "Q_lower", "Q_upper", "ESS", "prob_positive", "prob_negative",
      "prob_nonzero", "sig")
  )
  # Sort key: prob_nonzero desc
  testthat::expect_equal(s$prob_nonzero,
                         sort(s$prob_nonzero, decreasing = TRUE))
})


# ---- raw draws mode -------------------------------------------------

test_that("VAR(1) summary = FALSE returns raw [ndraws, p, p] draws", {
  require_fixtures("val_mvgam_var_cor.rds")
  fit <- load_mvgam("var_cor")
  res <- residual_cor(fit, summary = FALSE)
  testthat::expect_s3_class(res, "mvgam_residcor")
  testthat::expect_equal(dim(res[["cor_draws"]])[2:3], c(3L, 3L))
  testthat::expect_equal(dim(res[["cov_draws"]])[2:3], c(3L, 3L))
  testthat::expect_true(all(diag(res[["cor_draws"]][1L, , ]) == 1))
})


# ---- Hierarchical AR(1) cor -------------------------------------------

test_that("residual_cor on hierarchical AR returns global cor by default", {
  require_fixtures("val_mvgam_hier_ar_cor.rds")
  fit <- load_mvgam("hier_ar_cor")
  res <- residual_cor(fit)
  testthat::expect_s3_class(res, "mvgam_residcor")
  testthat::expect_true(isTRUE(res$hierarchical))
  testthat::expect_identical(res$group_label, "_global")
  # Subgroup count = number of subgroups (sp1, sp2, sp3) -> 3 x 3
  testthat::expect_equal(dim(res$cor), c(3L, 3L))
  testthat::expect_true(all(diag(res$cor) == 1))
})


test_that("hierarchical residual_cor(by_group = TRUE) returns named list", {
  require_fixtures("val_mvgam_hier_ar_cor.rds")
  fit <- load_mvgam("hier_ar_cor")
  res <- residual_cor(fit, by_group = TRUE)
  testthat::expect_type(res, "list")
  testthat::expect_true("_global" %in% names(res))
  # Per-group entries: real factor labels from data$region.
  region_levels <- levels(fit$data$region)
  testthat::expect_true(all(region_levels %in% names(res)))
  testthat::expect_equal(length(res), 1L + length(region_levels))
  for (nm in names(res)) {
    testthat::expect_s3_class(res[[nm]], "mvgam_residcor")
    testthat::expect_equal(dim(res[[nm]]$cor), c(3L, 3L))
    # Row / col labels use real factor levels from data$species.
    species_levels <- levels(fit$data$species)
    testthat::expect_identical(rownames(res[[nm]]$cor), species_levels)
    testthat::expect_identical(colnames(res[[nm]]$cor), species_levels)
  }
})


test_that("hierarchical residual_cor (global) labels = data$species levels", {
  require_fixtures("val_mvgam_hier_ar_cor.rds")
  fit <- load_mvgam("hier_ar_cor")
  res <- residual_cor(fit)
  species_levels <- levels(fit$data$species)
  testthat::expect_identical(rownames(res$cor), species_levels)
  testthat::expect_identical(colnames(res$cor), species_levels)
})


# ---- Latent-factor trend (n_lv = 2) ----------------------------------

test_that("residual_cor on AR(n_lv = 2) returns factor_loadings", {
  require_fixtures("val_mvgam_lv_factor.rds")
  fit <- load_mvgam("lv_factor")
  res <- residual_cor(fit)
  testthat::expect_s3_class(res, "mvgam_residcor")
  testthat::expect_equal(res$pattern, "factor_loadings")
  testthat::expect_equal(res$n_series, 4L)
  testthat::expect_true(all(diag(res$cor) == 1))
  testthat::expect_true(isSymmetric(unname(res$cor), tol = 1e-8))
  testthat::expect_true(all(res$cor >= -1 & res$cor <= 1))
  off <- !diag(TRUE, 4L)
  testthat::expect_true(all(res$cor_lower[off] <= res$cor[off]))
  testthat::expect_true(all(res$cor_upper[off] >= res$cor[off]))
})


test_that("residual_cor on LV fit: cov_draws match tcrossprod(Z) per draw", {
  require_fixtures("val_mvgam_lv_factor.rds")
  fit <- load_mvgam("lv_factor")
  res <- residual_cor(fit, summary = FALSE)
  draws <- posterior::as_draws_matrix(fit$fit)
  n_series <- 4L; n_lv <- 2L
  for (d in c(1L, 50L, 200L)) {
    Z_d <- matrix(NA_real_, nrow = n_series, ncol = n_lv)
    for (i in seq_len(n_series)) {
      for (j in seq_len(n_lv)) {
        Z_d[i, j] <- draws[d, sprintf("Z[%d,%d]", i, j)]
      }
    }
    expected <- tcrossprod(Z_d)
    # `residual_cor()` and `tcrossprod()` reach the same product by
    # different summation orders, so they agree to roughly 1e-7
    # relative rather than to the last bit. A wrong loading or a
    # misread draw would differ by orders of magnitude, which this
    # still catches.
    testthat::expect_equal(res[["cov_draws"]][d, , ], expected,
                           tolerance = 1e-6)
  }
})


test_that("summary of LV residcor returns 6 unique upper-tri pairs", {
  require_fixtures("val_mvgam_lv_factor.rds")
  fit <- load_mvgam("lv_factor")
  s <- summary(residual_cor(fit))
  testthat::expect_equal(nrow(s), 6L)  # 4-choose-2
  testthat::expect_equal(s$prob_nonzero,
                         sort(s$prob_nonzero, decreasing = TRUE))
})


# ---- Error paths on existing uncorrelated fixtures --------------------

test_that("residual_cor errors clearly on uncorrelated AR trend", {
  require_fixtures("val_mvgam_ar1_fx.rds")
  fit <- load_mvgam("ar1_fx")
  testthat::expect_error(residual_cor(fit), "independent")
})


test_that("residual_cor errors clearly on mvbind without trend cor", {
  require_fixtures("val_mvgam_mv_gauss.rds")
  fit <- load_mvgam("mv_gauss")
  testthat::expect_error(residual_cor(fit), "independent")
})
