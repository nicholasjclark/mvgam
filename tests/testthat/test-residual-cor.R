# Unit tests for residual_cor() and its mvgam_residcor S3 methods.
#
# The compute path is exercised by mocking the cov_struct returned by
# get_trend_covariance_structure(). Helper builds a fake "mvgam"
# object whose extracted covariance matches a known target so that
# the reconstructed cor / cov / partial-cor are deterministic.

build_fake_mvgam <- function() {
  structure(list(), class = "mvgam")
}

# Cov struct mocks: full_covariance (VAR-like) with a known Sigma.
mk_full_cov_struct <- function(Sigma) {
  ndraws <- 100L
  p <- nrow(Sigma)
  Sigma_arr <- array(0, dim = c(ndraws, p, p))
  for (d in seq_len(ndraws)) Sigma_arr[d, , ] <- Sigma
  list(
    pattern = "full_covariance",
    n_series = p,
    hierarchical = FALSE,
    has_correlations = TRUE,
    ndraws = ndraws,
    params = list(Sigma_trend = Sigma_arr),
    group_info = NULL
  )
}

# Cov struct mocks: cholesky_scaled (RW/AR/ZMVN cor=TRUE) with known
# sigma + L_Omega.
mk_chol_cov_struct <- function(sigma, L_Omega) {
  ndraws <- 100L
  p <- length(sigma)
  L_arr <- array(0, dim = c(ndraws, p, p))
  sigma_mat <- matrix(sigma, nrow = ndraws, ncol = p, byrow = TRUE)
  for (d in seq_len(ndraws)) L_arr[d, , ] <- L_Omega
  list(
    pattern = "cholesky_scaled",
    n_series = p,
    hierarchical = FALSE,
    has_correlations = TRUE,
    ndraws = ndraws,
    params = list(L_Omega_trend = L_arr, sigma_trend = sigma_mat),
    group_info = NULL
  )
}

# Helper to build a fake mvgam fit carrying just enough state for the
# factor-loadings branch: an n_lv on the trend spec, plus a fit slot
# that returns the requested Z[i,j] columns when handed to
# posterior::as_draws_matrix(). resolve_series_info() is consulted
# via local_mocked_bindings so the helper doesn't need a real fit.
mk_factor_obj <- function(n_series = 3L, n_lv = 2L, ndraws = 50L,
                          Z_target = NULL) {
  if (is.null(Z_target)) {
    Z_target <- matrix(c(0.8, 0.1, -0.3,
                         0.2, 0.5, 0.7), nrow = n_series, ncol = n_lv)
  }
  # Build a fake draws matrix with Z[i,j] columns named in Stan order.
  col_names <- as.vector(outer(seq_len(n_series), seq_len(n_lv),
                                FUN = function(i, j) sprintf("Z[%d,%d]", i, j)))
  draws_mat <- matrix(NA_real_, nrow = ndraws, ncol = length(col_names))
  colnames(draws_mat) <- col_names
  for (i in seq_len(n_series)) {
    for (j in seq_len(n_lv)) {
      nm <- sprintf("Z[%d,%d]", i, j)
      # Constant across draws so per-draw cov is deterministic and
      # comparable to a known target.
      draws_mat[, nm] <- Z_target[i, j]
    }
  }
  list(
    obj = structure(
      list(
        mv_spec = list(trend_specs = structure(
          list(n_lv = n_lv), class = "mvgam_trend"
        )),
        fit = draws_mat
      ),
      class = "mvgam"
    ),
    series_levels = paste0("s", seq_len(n_series)),
    Z_target = Z_target
  )
}


# ---- factor-loadings path --------------------------------------------

test_that("residual_cor reconstructs Sigma = ZZ^T for factor fits", {
  case <- mk_factor_obj()
  testthat::local_mocked_bindings(
    resolve_series_info = function(object) {
      list(series_levels = case$series_levels)
    },
    .package = "mvgam"
  )
  # Mock as_draws_matrix to bypass posterior package validation on
  # our handcrafted draws matrix.
  testthat::local_mocked_bindings(
    as_draws_matrix = function(x, ...) x,
    .package = "posterior"
  )
  res <- residual_cor(case$obj)
  expect_s3_class(res, "mvgam_residcor")
  expect_equal(res$pattern, "factor_loadings")
  expect_equal(res$n_series, 3L)
  # Cor invariants
  expect_true(all(diag(res$cor) == 1))
  expect_true(isSymmetric(unname(res$cor), tol = 1e-8))
  # Expected cor from constant Z
  expected_cov <- tcrossprod(case$Z_target)
  expected_cor <- cov2cor(expected_cov)
  expect_equal(unname(res$cor), expected_cor, tolerance = 1e-8)
  expect_identical(unname(rownames(res$cor)), case$series_levels)
})


test_that("detect_factor_n_lv returns NULL for non-factor specs", {
  obj <- structure(
    list(mv_spec = list(trend_specs = structure(
      list(n_lv = NULL), class = "mvgam_trend"
    ))),
    class = "mvgam"
  )
  expect_null(detect_factor_n_lv(obj))
  # Also NULL when trend_specs is missing entirely
  expect_null(detect_factor_n_lv(structure(list(), class = "mvgam")))
})


test_that("detect_factor_n_lv returns integer when n_lv > 0", {
  obj <- structure(
    list(mv_spec = list(trend_specs = structure(
      list(n_lv = 3L), class = "mvgam_trend"
    ))),
    class = "mvgam"
  )
  expect_identical(detect_factor_n_lv(obj), 3L)
})


# Cov struct mock: hierarchical cholesky.
mk_hier_cov_struct <- function(n_sub = 3L, n_groups = 2L,
                                ndraws = 50L) {
  L_global <- t(chol(matrix(c(1.0, 0.5, 0.3,
                              0.5, 1.0, 0.4,
                              0.3, 0.4, 1.0), 3, 3)))
  L_dev <- t(chol(matrix(c(1.0, 0.0, 0.0,
                           0.0, 1.0, 0.0,
                           0.0, 0.0, 1.0), 3, 3)))
  L_glob_arr <- array(0, dim = c(ndraws, n_sub, n_sub))
  L_dev_arr <- array(0, dim = c(ndraws, n_groups, n_sub, n_sub))
  sigma_arr <- array(1, dim = c(ndraws, n_groups, n_sub))
  for (d in seq_len(ndraws)) {
    L_glob_arr[d, , ] <- L_global
    for (g in seq_len(n_groups)) L_dev_arr[d, g, , ] <- L_dev
  }
  list(
    pattern = "cholesky_scaled",
    n_series = n_sub,
    hierarchical = TRUE,
    has_correlations = TRUE,
    ndraws = ndraws,
    params = list(
      alpha_cor_trend = rep(0.7, ndraws),
      L_Omega_global_trend = L_glob_arr,
      L_deviation_group_trend = L_dev_arr,
      sigma_group_trend = sigma_arr
    ),
    group_info = list(
      n_groups = n_groups, n_subgroups = n_sub,
      group_labels = c("g1", "g2"),
      subgroup_labels = c("s1", "s2", "s3")
    )
  )
}


# ---- full_covariance path --------------------------------------------

test_that("residual_cor reconstructs cor from a known VAR-style Sigma", {
  Sigma <- matrix(c(2.0, 0.6, 0.6,
                    0.6, 1.5, 0.2,
                    0.6, 0.2, 1.0), 3, 3)
  cov_struct <- mk_full_cov_struct(Sigma)
  testthat::local_mocked_bindings(
    get_trend_covariance_structure = function(object) cov_struct,
    .package = "mvgam"
  )
  res <- residual_cor(build_fake_mvgam(), summary = TRUE)
  expect_s3_class(res, "mvgam_residcor")
  # Strip dimnames before comparison so the test does not depend on
  # the labelling convention applied by the summariser.
  cor_actual <- res[["cor"]]; dimnames(cor_actual) <- NULL
  cov_actual <- res[["cov"]]; dimnames(cov_actual) <- NULL
  expect_equal(cor_actual, cov2cor(Sigma), tolerance = 1e-8)
  expect_equal(cov_actual, Sigma, tolerance = 1e-8)
  expect_true(all(diag(res[["cor"]]) == 1))
  expect_true(all(res[["cor_lower"]] <= res[["cor"]]))
  expect_true(all(res[["cor_upper"]] >= res[["cor"]]))
})


test_that("residual_cor returns raw draws when summary = FALSE", {
  Sigma <- diag(c(2, 3)) + matrix(0.5, 2, 2) - diag(0.5, 2)
  cov_struct <- mk_full_cov_struct(Sigma)
  testthat::local_mocked_bindings(
    get_trend_covariance_structure = function(object) cov_struct,
    .package = "mvgam"
  )
  res <- residual_cor(build_fake_mvgam(), summary = FALSE)
  expect_s3_class(res, "mvgam_residcor")
  expect_equal(dim(res[["cor_draws"]]), c(100L, 2L, 2L))
  expect_equal(dim(res[["cov_draws"]]), c(100L, 2L, 2L))
  expect_null(res[["prec_draws"]])
  expect_null(res[["cor"]])
})


test_that("residual_cor(partial = TRUE) populates prec fields", {
  Sigma <- matrix(c(2.0, 0.6, 0.3,
                    0.6, 1.5, 0.1,
                    0.3, 0.1, 1.0), 3, 3)
  cov_struct <- mk_full_cov_struct(Sigma)
  testthat::local_mocked_bindings(
    get_trend_covariance_structure = function(object) cov_struct,
    .package = "mvgam"
  )
  res <- residual_cor(build_fake_mvgam(), partial = TRUE)
  expect_s3_class(res, "mvgam_residcor")
  expect_true(!is.null(res$prec))
  expect_equal(dim(res$prec), c(3L, 3L))
  expect_true(!is.null(res$prec_lower))
  expect_true(!is.null(res$prec_upper))
})


# ---- cholesky_scaled path --------------------------------------------

test_that("residual_cor reconstructs cor from sigma + L_Omega", {
  sigma <- c(1.5, 2.0, 0.8)
  Omega <- matrix(c(1.0, 0.4, 0.2,
                    0.4, 1.0, 0.3,
                    0.2, 0.3, 1.0), 3, 3)
  L_Omega <- t(chol(Omega))
  cov_struct <- mk_chol_cov_struct(sigma, L_Omega)
  testthat::local_mocked_bindings(
    get_trend_covariance_structure = function(object) cov_struct,
    .package = "mvgam"
  )
  res <- residual_cor(build_fake_mvgam())
  # Expected Sigma = diag(sigma) %*% L %*% t(L) %*% diag(sigma).
  L_scaled <- L_Omega * sigma
  Sigma_expected <- tcrossprod(L_scaled)
  cor_actual <- res[["cor"]]; dimnames(cor_actual) <- NULL
  expect_equal(cor_actual, cov2cor(Sigma_expected), tolerance = 1e-8)
})


# ---- hierarchical path ------------------------------------------------

test_that("hierarchical residcor falls back when factor lookup fails", {
  cov_struct <- mk_hier_cov_struct()
  testthat::local_mocked_bindings(
    get_trend_covariance_structure = function(object) cov_struct,
    .package = "mvgam"
  )
  # Trend spec gr/subgr point at columns that do not exist in the
  # fit's data. Resolver must drop back to "group_<i>" / "subgroup_<i>"
  # rather than error.
  obj <- structure(
    list(
      mv_spec = list(trend_specs = structure(
        list(gr = "missing_grp", subgr = "missing_sub"),
        class = "mvgam_trend"
      )),
      data = data.frame(other = 1:6)
    ),
    class = "mvgam"
  )
  res <- residual_cor(obj, groups = TRUE)
  expect_true(all(c("group_1", "group_2") %in% names(res)))
  expect_identical(rownames(res$`_global`$cor),
                   c("subgroup_1", "subgroup_2", "subgroup_3"))
})


test_that("residual_cor hierarchical default returns global cor only", {
  cov_struct <- mk_hier_cov_struct()
  testthat::local_mocked_bindings(
    get_trend_covariance_structure = function(object) cov_struct,
    .package = "mvgam"
  )
  res <- residual_cor(build_fake_mvgam())
  expect_s3_class(res, "mvgam_residcor")
  expect_identical(res[["group_label"]], "_global")
  # Global cor = tcrossprod(L_global) (which is already a valid cor mat).
  L_global <- cov_struct$params$L_Omega_global_trend[1, , ]
  cor_actual <- res[["cor"]]; dimnames(cor_actual) <- NULL
  expect_equal(cor_actual, tcrossprod(L_global), tolerance = 1e-8)
})


test_that("residual_cor hierarchical with groups = TRUE returns list", {
  cov_struct <- mk_hier_cov_struct()
  testthat::local_mocked_bindings(
    get_trend_covariance_structure = function(object) cov_struct,
    .package = "mvgam"
  )
  # Hierarchical group labels are now resolved from object$data via
  # the trend spec's `gr` variable name. Provide both so the
  # resolver finds them; otherwise it falls back to "group_<i>".
  obj <- structure(
    list(
      mv_spec = list(trend_specs = structure(
        list(gr = "region", subgr = "species"),
        class = "mvgam_trend"
      )),
      # Stub data: mirrors what a real hierarchical mvgam fit
      # carries on $data - a long-format data.frame with one row
      # per series x time combination. Levels here must match
      # cov_struct$group_info dimensions (2 groups, 3 subgroups).
      data = data.frame(
        region = factor(rep(c("g1", "g2"), 3L)),
        species = factor(rep(c("s1", "s2", "s3"), each = 2L))
      )
    ),
    class = "mvgam"
  )
  res <- residual_cor(obj, groups = TRUE)
  expect_type(res, "list")
  expect_true("_global" %in% names(res))
  expect_true(all(c("g1", "g2") %in% names(res)))
  for (k in names(res)) expect_s3_class(res[[k]], "mvgam_residcor")
})


# ---- error paths ------------------------------------------------------

test_that("residual_cor errors when trend pattern = none", {
  cov_struct <- list(pattern = "none", n_series = 0L,
                     hierarchical = FALSE, has_correlations = FALSE,
                     ndraws = 0L, params = list(), group_info = NULL)
  testthat::local_mocked_bindings(
    get_trend_covariance_structure = function(object) cov_struct,
    .package = "mvgam"
  )
  expect_error(residual_cor(build_fake_mvgam()), "no covariance")
})


test_that("residual_cor errors when correlations not requested", {
  cov_struct <- list(pattern = "cholesky_scaled", n_series = 3L,
                     hierarchical = FALSE, has_correlations = FALSE,
                     ndraws = 50L, params = list(), group_info = NULL)
  testthat::local_mocked_bindings(
    get_trend_covariance_structure = function(object) cov_struct,
    .package = "mvgam"
  )
  expect_error(residual_cor(build_fake_mvgam()), "independent")
})


test_that("residual_cor.jsdgam errors with pending-port message", {
  obj <- structure(list(), class = c("jsdgam", "mvgam"))
  expect_error(residual_cor(obj), "not yet implemented")
  expect_error(residual_cor(obj), "jsdgam")
})


# ---- Fisher-z CI behaviour -------------------------------------------

test_that("Fisher-z CIs stay strictly inside (-1, 1)", {
  # Build a Sigma whose true correlation is near +/- 1 to stress-test
  # the boundary behaviour. Cor(1,2) ~ 0.99.
  Sigma <- matrix(c(1.0, 0.99, 0.99, 1.0), 2, 2)
  cov_struct <- mk_full_cov_struct(Sigma)
  testthat::local_mocked_bindings(
    get_trend_covariance_structure = function(object) cov_struct,
    .package = "mvgam"
  )
  res <- residual_cor(build_fake_mvgam())
  off_mask <- !diag(TRUE, nrow = 2L)
  expect_true(all(res[["cor_lower"]][off_mask] > -1 &
                  res[["cor_lower"]][off_mask] < 1))
  expect_true(all(res[["cor_upper"]][off_mask] > -1 &
                  res[["cor_upper"]][off_mask] < 1))
  # Off-diagonal lower CI strictly less than 1 even at r = 0.99
  expect_lt(res[["cor_lower"]][1, 2], 1)
})


# ---- prob_nonzero / sig_cor ------------------------------------------

test_that("prob_nonzero and sig_cor reflect off-zero pairs", {
  # Constant Sigma across draws => prob_positive == 1 for positive
  # off-diagonal pairs.
  Sigma <- matrix(c(1.0, 0.6, 0.6, 1.0), 2, 2)
  cov_struct <- mk_full_cov_struct(Sigma)
  testthat::local_mocked_bindings(
    get_trend_covariance_structure = function(object) cov_struct,
    .package = "mvgam"
  )
  res <- residual_cor(build_fake_mvgam())
  expect_equal(res$prob_positive[1, 2], 1)
  expect_equal(res$prob_negative[1, 2], 0)
  expect_equal(res$prob_nonzero[1, 2], 1)
  expect_true(res$sig_cor[1, 2] != 0)
})


# ---- summary.mvgam_residcor tidy --------------------------------------

test_that("summary.mvgam_residcor returns tidy tibble sorted by prob_nonzero", {
  Sigma <- matrix(c(1.0, 0.8, 0.1,
                    0.8, 1.0, 0.5,
                    0.1, 0.5, 1.0), 3, 3)
  cov_struct <- mk_full_cov_struct(Sigma)
  testthat::local_mocked_bindings(
    get_trend_covariance_structure = function(object) cov_struct,
    .package = "mvgam"
  )
  res <- residual_cor(build_fake_mvgam())
  s <- summary(res)
  expect_s3_class(s, "tbl_df")
  expect_equal(nrow(s), 3L)  # 3-choose-2 unique pairs
  expect_setequal(
    colnames(s),
    c("series_1", "series_2", "Estimate", "Est.Error",
      "Q_lower", "Q_upper", "prob_positive", "prob_negative",
      "prob_nonzero", "sig")
  )
  # All three constant-Sigma off-diagonals have prob_nonzero = 1, so
  # the tiebreaker (abs(Estimate) desc) puts 0.8 / 0.5 / 0.1 in order.
  expect_equal(s$Estimate[1L], cov2cor(Sigma)[1, 2], tolerance = 1e-8)
  expect_equal(abs(s$Estimate), sort(abs(s$Estimate), decreasing = TRUE),
               tolerance = 1e-8)
})


test_that("summary.mvgam_residcor errors when summary = FALSE was used", {
  Sigma <- matrix(c(1.0, 0.4, 0.4, 1.0), 2, 2)
  cov_struct <- mk_full_cov_struct(Sigma)
  testthat::local_mocked_bindings(
    get_trend_covariance_structure = function(object) cov_struct,
    .package = "mvgam"
  )
  res <- residual_cor(build_fake_mvgam(), summary = FALSE)
  expect_error(summary(res), "summary = FALSE")
})


# ---- print.mvgam_residcor --------------------------------------------

test_that("print.mvgam_residcor returns the object invisibly", {
  Sigma <- matrix(c(1.0, 0.4, 0.4, 1.0), 2, 2)
  cov_struct <- mk_full_cov_struct(Sigma)
  testthat::local_mocked_bindings(
    get_trend_covariance_structure = function(object) cov_struct,
    .package = "mvgam"
  )
  res <- residual_cor(build_fake_mvgam())
  out <- capture.output(invisible(print(res)))
  expect_true(any(grepl("Residual correlations", out)))
  expect_true(any(grepl("Pattern", out)))
})
