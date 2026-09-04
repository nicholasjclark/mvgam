# End-to-end residual_cor integration tests on real cached fits.
#
# Lives in tests/local because it requires the val_mvgam_var_cor
# fixture from tests/local/build_fixtures.R.
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
    # A factor model writes `trend[t, s] = Z[s, ] . lv[t, ]`, so the
    # covariance between series is the latent covariance projected
    # through the loadings, not `Z Z'`. This trend is an AR(1) with
    # independent innovations, so the latent covariance is the
    # stationary variance of each column, `sigma^2 / (1 - phi^2)`.
    sigma_d <- vapply(
      seq_len(n_lv),
      function(k) draws[d, sprintf("sigma_trend[%d]", k)],
      numeric(1L)
    )
    phi_d <- vapply(
      seq_len(n_lv),
      function(k) draws[d, sprintf("ar1_trend[%d]", k)],
      numeric(1L)
    )
    omega <- diag(sigma_d^2 / (1 - phi_d^2), nrow = n_lv)
    expected <- Z_d %*% omega %*% t(Z_d)
    # The same product reached by a different summation order, so
    # they agree to roughly 1e-7 relative rather than to the last
    # bit. A wrong loading or a misread draw differs by orders of
    # magnitude, which this still catches.
    testthat::expect_equal(res[["cov_draws"]][d, , ], expected,
                           tolerance = 1e-6)
    # And dropping the latent scale would not pass.
    testthat::expect_false(isTRUE(all.equal(
      res[["cov_draws"]][d, , ], tcrossprod(Z_d), tolerance = 1e-6
    )))
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
