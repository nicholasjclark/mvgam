# End-to-end integration tests for the loadings_prior surface on
# a real cached fit.
#
# Lives in tests/local because it requires the
# val_mvgam_loadings_prior fixture from
# tests/local/build_fixtures.R. Run via:
#   Rscript -e 'devtools::load_all();
#               testthat::test_file("tests/local/test-loadings-prior-fit.R")'

source("setup_tests_local.R")
source("concordance_helpers.R")


load_loadings_prior_truth <- function() {
  readRDS(file.path(
    local_fixture_dir(), "val_mvgam_loadings_prior_truth.rds"
  ))
}


test_that("loadings_prior fit carries structured-prior Stan code", {
  require_fixtures("val_mvgam_loadings_prior.rds")
  fit <- load_mvgam("loadings_prior")
  sc <- fit$stancode
  expect_match(sc, "gp_exponential_cov", fixed = TRUE)
  expect_match(sc, "dist_cluster", fixed = TRUE)
  expect_match(sc, "theta_features", fixed = TRUE)
  expect_match(sc, "theta_dist_cluster", fixed = TRUE)
  expect_match(sc, "multi_normal_cholesky", fixed = TRUE)
  expect_match(sc, "qr_thin_R", fixed = TRUE)
  # Named in both spellings: a normalised program writes the
  # density call, so testing only the tilde passes vacuously.
  expect_false(grepl("to_vector\\(Z\\)\\s*~\\s*student_t", sc))
  expect_false(grepl("student_t_lpdf(to_vector(Z)", sc,
                       fixed = TRUE))
})


test_that("loadings_prior standata threads encoded features and distance", {
  require_fixtures("val_mvgam_loadings_prior.rds")
  fit <- load_mvgam("loadings_prior")
  sd <- fit$standata
  # 1 continuous trait + 2 one-hot cluster indicators (both
  # levels retained per Heaps Sect. 6.2).
  expect_equal(sd$N_features_trend, 3L)
  expect_equal(dim(sd$row_features), c(8L, 3L))
  expect_equal(dim(sd$dist_cluster), c(8L, 8L))
  # Distance matrix standardised to max-distance one.
  expect_equal(max(sd$dist_cluster), 1)
})


test_that("loadings_prior fit converges on identified Z_tilde", {
  require_fixtures("val_mvgam_loadings_prior.rds")
  fit <- load_mvgam("loadings_prior")
  draws <- posterior::as_draws_df(fit$fit)
  z_cols <- grep("^Z_tilde\\[", colnames(draws), value = TRUE)
  expect_gt(length(z_cols), 0L)
  diag <- posterior::summarise_draws(
    posterior::subset_draws(draws, variable = z_cols),
    posterior::default_convergence_measures()
  )
  expect_lt(max(diag$rhat, na.rm = TRUE), 1.1)
  expect_gt(min(diag$ess_bulk, na.rm = TRUE), 100)
})


test_that("loadings_prior fit learns finite length-scale posteriors", {
  require_fixtures("val_mvgam_loadings_prior.rds")
  fit <- load_mvgam("loadings_prior")
  draws <- posterior::as_draws_df(fit$fit)
  theta_dist <- posterior::extract_variable(
    draws, "theta_dist_cluster"
  )
  expect_true(all(is.finite(theta_dist)))
  expect_true(all(theta_dist > 0))
  # All three feature length-scales should also be finite + positive.
  theta_feat_cols <- grep(
    "^theta_features\\[", colnames(draws), value = TRUE
  )
  expect_equal(length(theta_feat_cols), 3L)
  theta_feat_med <- vapply(
    theta_feat_cols,
    function(v) median(posterior::extract_variable(draws, v)),
    numeric(1L)
  )
  expect_true(all(is.finite(theta_feat_med)))
  expect_true(all(theta_feat_med > 0))
})


test_that("loadings_prior fit recovers within-cluster residual correlation", {
  require_fixtures("val_mvgam_loadings_prior.rds")
  fit <- load_mvgam("loadings_prior")
  truth <- load_loadings_prior_truth()
  res <- residual_cor(fit)
  cor_mat <- res$cor
  expect_s3_class(res, "mvgam_residcor")
  expect_equal(dim(cor_mat), c(8L, 8L))
  # Within-cluster series share latent structure by design; the
  # implied marginal correlation matrix should be higher within
  # cluster than between cluster on average.
  within_pairs <- which(
    outer(truth$cluster, truth$cluster, "==") &
      upper.tri(cor_mat)
  )
  between_pairs <- which(
    outer(truth$cluster, truth$cluster, "!=") &
      upper.tri(cor_mat)
  )
  within_cor <- mean(cor_mat[within_pairs])
  between_cor <- mean(cor_mat[between_pairs])
  expect_gt(within_cor, between_cor)
})


test_that("shared_variation() recovers block structure of true Delta", {
  require_fixtures("val_mvgam_loadings_prior.rds")
  fit <- load_mvgam("loadings_prior")
  truth <- load_loadings_prior_truth()
  sv <- shared_variation(fit)
  expect_s3_class(sv, "mvgam_shared_variation")
  expect_equal(sv$n_series, 8L)
  expect_equal(sv$n_lv, 3L)
  expect_equal(dim(sv$delta), c(8L, 8L))
  expect_equal(rownames(sv$delta), truth$Z_true |> rownames())
  # Within-cluster Delta should dominate between-cluster Delta
  # because cluster A and cluster B load on disjoint primary
  # factors plus a shared factor 3.
  cl <- truth$cluster
  within_idx <- outer(cl, cl, "==") & upper.tri(sv$delta)
  between_idx <- outer(cl, cl, "!=") & upper.tri(sv$delta)
  expect_gt(mean(sv$delta[within_idx]),
            mean(sv$delta[between_idx]))
  # Diagonal entries are positive variances by construction.
  expect_true(all(diag(sv$delta) > 0))
})


test_that("summary() shows the loadings_prior block when present", {
  require_fixtures("val_mvgam_loadings_prior.rds")
  fit <- load_mvgam("loadings_prior")
  s <- summary(fit)
  expect_false(is.null(s$loadings_prior))
  block_vars <- rownames(s$loadings_prior)
  # One theta_features per encoded column (1 continuous + 2
  # one-hot cluster indicators = 3 columns) plus one
  # theta_dist_<name> per supplied distance matrix.
  expect_true(all(c(
    "theta_features[1]", "theta_features[2]", "theta_features[3]",
    "theta_dist_cluster"
  ) %in% block_vars))
  # varrho_inv is intentionally hidden: uninterpretable on its
  # own (cumulative-product parameter for Psi_diag).
  expect_false(any(grepl("^varrho_inv", block_vars)))
})


test_that("loadings_prior fit recovers the dominant cluster contrast", {
  require_fixtures("val_mvgam_loadings_prior.rds")
  fit <- load_mvgam("loadings_prior")
  truth <- load_loadings_prior_truth()
  draws <- posterior::as_draws_matrix(fit$fit)
  z_cols <- grep("^Z_tilde\\[", colnames(draws), value = TRUE)
  Z_med <- matrix(
    apply(draws[, z_cols], 2L, stats::median),
    nrow = nrow(truth$Z_true),
    ncol = ncol(truth$Z_true)
  )
  # Identified Z_tilde lives in a rotated basis; the strict
  # column-by-column match is unidentifiable in general. The
  # cluster contrast (Z_true[, 1] - Z_true[, 2]) is the
  # dominant rotation-invariant signal in the simulation and
  # should appear among the leading fitted singular components.
  # Test: the cluster contrast is captured by at least one
  # fitted factor at |r| >= 0.6.
  cluster_contrast <- truth$Z_true[, 1L] - truth$Z_true[, 2L]
  best_capture <- max(abs(stats::cor(cluster_contrast, Z_med)))
  expect_gte(best_capture, 0.6)
})
