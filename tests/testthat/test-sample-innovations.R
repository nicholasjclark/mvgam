#' Tests for Innovation Sampling Infrastructure
#'
#' Unit-level tests for sample_innovations() and the per-pattern
#' transforms. Hierarchical Cholesky uses synthetic posterior matrices
#' rather than fitted models for fast, deterministic coverage.

# Helper: build a synthetic posterior column matrix containing the columns
# expected by extract_hierarchical_cholesky_params().
make_hier_draws_mat <- function(n_draws, n_groups, n_sub,
                                seed = 1L) {
  set.seed(seed)
  cols <- "alpha_cor_trend"
  for (i in seq_len(n_sub)) {
    for (j in seq_len(n_sub)) {
      cols <- c(cols, sprintf("L_Omega_global_trend[%d,%d]", i, j))
    }
  }
  for (g in seq_len(n_groups)) {
    for (i in seq_len(n_sub)) {
      for (j in seq_len(n_sub)) {
        cols <- c(cols,
                  sprintf("L_deviation_group_trend[%d,%d,%d]", g, i, j))
      }
    }
  }
  for (g in seq_len(n_groups)) {
    for (s in seq_len(n_sub)) {
      cols <- c(cols, sprintf("sigma_group_trend[%d,%d]", g, s))
    }
  }
  m <- matrix(runif(n_draws * length(cols)), n_draws, length(cols))
  colnames(m) <- cols
  m
}

# Helper: build identity-Cholesky params with given per-group sigmas.
make_identity_hier_params <- function(n_draws, n_groups, n_sub,
                                       sigma_per_group, alpha = 1.0) {
  stopifnot(length(sigma_per_group) == n_groups)
  stopifnot(all(vapply(sigma_per_group, length, integer(1)) == n_sub))

  L_glob <- array(rep(diag(n_sub), each = n_draws),
                  c(n_draws, n_sub, n_sub))
  L_dev <- array(rep(diag(n_sub), each = n_draws * n_groups),
                 c(n_draws, n_groups, n_sub, n_sub))
  sigma_arr <- array(0, c(n_draws, n_groups, n_sub))
  for (d in seq_len(n_draws)) {
    for (g in seq_len(n_groups)) {
      sigma_arr[d, g, ] <- sigma_per_group[[g]]
    }
  }
  list(
    alpha_cor_trend = rep(alpha, n_draws),
    L_Omega_global_trend = L_glob,
    L_deviation_group_trend = L_dev,
    sigma_group_trend = sigma_arr
  )
}


test_that("extract_hierarchical_cholesky_params builds correct shapes", {
  n_draws <- 8
  n_groups <- 2
  n_sub <- 2
  draws_mat <- make_hier_draws_mat(n_draws, n_groups, n_sub)
  group_info <- list(
    n_groups = n_groups, n_subgroups = n_sub,
    group_inds = c(1, 1, 2, 2)
  )

  params <- extract_hierarchical_cholesky_params(draws_mat, group_info)

  expect_equal(length(params$alpha_cor_trend), n_draws)
  expect_equal(dim(params$L_Omega_global_trend),
               c(n_draws, n_sub, n_sub))
  expect_equal(dim(params$L_deviation_group_trend),
               c(n_draws, n_groups, n_sub, n_sub))
  expect_equal(dim(params$sigma_group_trend),
               c(n_draws, n_groups, n_sub))
})


test_that("extract_hierarchical_cholesky_params preserves named values", {
  n_draws <- 6
  draws_mat <- make_hier_draws_mat(n_draws, n_groups = 2, n_sub = 2)
  group_info <- list(n_groups = 2, n_subgroups = 2, group_inds = c(1, 1, 2, 2))

  params <- extract_hierarchical_cholesky_params(draws_mat, group_info)

  # Spot-check: posterior column maps to the correct array slot
  expect_equal(
    params$alpha_cor_trend,
    as.numeric(draws_mat[, "alpha_cor_trend"])
  )
  expect_equal(
    params$L_Omega_global_trend[, 2, 1],
    as.numeric(draws_mat[, "L_Omega_global_trend[2,1]"])
  )
  expect_equal(
    params$L_deviation_group_trend[, 2, 1, 2],
    as.numeric(draws_mat[, "L_deviation_group_trend[2,1,2]"])
  )
  expect_equal(
    params$sigma_group_trend[, 1, 2],
    as.numeric(draws_mat[, "sigma_group_trend[1,2]"])
  )
})


test_that("extract_hierarchical_cholesky_params errors on missing param", {
  draws_mat <- matrix(0, 5, 3,
                      dimnames = list(NULL, c("a", "b", "c")))
  group_info <- list(n_groups = 1, n_subgroups = 1)
  expect_error(
    extract_hierarchical_cholesky_params(draws_mat, group_info),
    "alpha_cor_trend"
  )
})


test_that("hierarchical transform: identity Chol + unit sigma is identity", {
  n_draws <- 4
  n_times <- 6
  n_series <- 4
  n_groups <- 2
  n_sub <- 2
  group_info <- list(n_groups = n_groups, n_subgroups = n_sub,
                      group_inds = c(1, 1, 2, 2))
  params <- make_identity_hier_params(
    n_draws, n_groups, n_sub,
    sigma_per_group = list(c(1, 1), c(1, 1))
  )

  set.seed(11)
  z <- matrix(rnorm(n_draws * n_times * n_series),
              n_draws, n_times * n_series)

  innov <- transform_hierarchical_cholesky_innovations(
    z, params, n_times, n_series, n_draws, group_info
  )

  expect_equal(dim(innov), c(n_draws, n_times * n_series))
  # Within-group order matches series order for group_inds c(1,1,2,2),
  # so innov should equal z exactly.
  expect_equal(innov, z)
})


test_that("hierarchical transform: per-group sigma scales per-series var", {
  n_draws <- 30
  n_times <- 80
  n_series <- 4
  n_groups <- 2
  n_sub <- 2
  group_info <- list(n_groups = n_groups, n_subgroups = n_sub,
                      group_inds = c(1, 1, 2, 2))
  sigma_g1 <- c(2, 3)
  sigma_g2 <- c(4, 5)
  params <- make_identity_hier_params(
    n_draws, n_groups, n_sub,
    sigma_per_group = list(sigma_g1, sigma_g2)
  )

  set.seed(22)
  z <- matrix(rnorm(n_draws * n_times * n_series),
              n_draws, n_times * n_series)
  innov <- transform_hierarchical_cholesky_innovations(
    z, params, n_times, n_series, n_draws, group_info
  )

  per_series_var <- function(s) {
    cols <- (s - 1L) * n_times + seq_len(n_times)
    var(as.vector(innov[, cols]))
  }
  # Tolerance is relative; with n_draws=30 and n_times=80 the SE of the
  # sample variance is ~sqrt(2/N) * sigma^2 ~= 3% of the target. Allow
  # 10% to keep the test stable across seeds.
  expect_equal(per_series_var(1), sigma_g1[1]^2, tolerance = 0.10)
  expect_equal(per_series_var(2), sigma_g1[2]^2, tolerance = 0.10)
  expect_equal(per_series_var(3), sigma_g2[1]^2, tolerance = 0.10)
  expect_equal(per_series_var(4), sigma_g2[2]^2, tolerance = 0.10)
})


test_that("hierarchical transform: groups are independent", {
  # With identity within-group correlations and finite sample size,
  # cross-group covariance should be ~0 while within-group covariance
  # for non-identity Cholesky would be non-zero. Here we use identity
  # so all pairs should be ~0; the test guards against accidental
  # cross-group leakage.
  n_draws <- 40
  n_times <- 60
  n_series <- 4
  n_groups <- 2
  n_sub <- 2
  group_info <- list(n_groups = n_groups, n_subgroups = n_sub,
                      group_inds = c(1, 1, 2, 2))
  params <- make_identity_hier_params(
    n_draws, n_groups, n_sub,
    sigma_per_group = list(c(1, 1), c(1, 1))
  )

  set.seed(33)
  z <- matrix(rnorm(n_draws * n_times * n_series),
              n_draws, n_times * n_series)
  innov <- transform_hierarchical_cholesky_innovations(
    z, params, n_times, n_series, n_draws, group_info
  )

  s_cols <- function(s) (s - 1L) * n_times + seq_len(n_times)
  v13 <- cor(as.vector(innov[, s_cols(1)]),
             as.vector(innov[, s_cols(3)]))
  v24 <- cor(as.vector(innov[, s_cols(2)]),
             as.vector(innov[, s_cols(4)]))
  expect_lt(abs(v13), 0.1)
  expect_lt(abs(v24), 0.1)
})


test_that("hierarchical transform errors on dim mismatches", {
  n_draws <- 4
  n_times <- 6
  n_series <- 4
  n_groups <- 2
  n_sub <- 2
  group_info <- list(n_groups = n_groups, n_subgroups = n_sub,
                      group_inds = c(1, 1, 2, 2))
  good <- make_identity_hier_params(
    n_draws, n_groups, n_sub, list(c(1, 1), c(1, 1))
  )
  z <- matrix(rnorm(n_draws * n_times * n_series),
              n_draws, n_times * n_series)

  bad <- good
  bad$L_Omega_global_trend <- array(0, c(n_draws, n_sub + 1, n_sub))
  expect_error(
    transform_hierarchical_cholesky_innovations(
      z, bad, n_times, n_series, n_draws, group_info
    ),
    "L_Omega_global_trend"
  )

  bad <- good
  bad$L_deviation_group_trend <- array(0,
                                       c(n_draws, n_groups + 1, n_sub, n_sub))
  expect_error(
    transform_hierarchical_cholesky_innovations(
      z, bad, n_times, n_series, n_draws, group_info
    ),
    "L_deviation_group_trend"
  )

  bad <- good
  bad$sigma_group_trend <- array(0, c(n_draws, n_groups, n_sub + 1))
  expect_error(
    transform_hierarchical_cholesky_innovations(
      z, bad, n_times, n_series, n_draws, group_info
    ),
    "sigma_group_trend"
  )
})


test_that("simple cholesky transform consumes structured 3D L_Omega array", {
  # transform_cholesky_innovations() takes params built by
  # extract_simple_cholesky_params(): sigma is [ndraws, n_series],
  # L_Omega_trend is [ndraws, n_series, n_series]. With identity Cholesky
  # and unit sigma, the transform must be the identity on z.
  n_draws <- 5
  n_times <- 4
  n_series <- 3
  L_arr <- array(rep(diag(n_series), each = n_draws),
                 c(n_draws, n_series, n_series))
  params <- list(
    sigma_trend = matrix(1, n_draws, n_series),
    L_Omega_trend = L_arr
  )
  set.seed(101)
  z <- matrix(rnorm(n_draws * n_times * n_series),
              n_draws, n_times * n_series)
  out <- transform_cholesky_innovations(
    z, params, n_times, n_series, n_draws
  )
  expect_equal(dim(out), c(n_draws, n_times * n_series))
  expect_equal(out, z)
})


test_that("simple cholesky transform: per-series sigma scales variance", {
  # Apply known sigmas through identity Cholesky and verify variance
  # per series. Uses a fixed Cholesky factor different from identity
  # (lower-tri with off-diagonals) so the test catches incorrect
  # transposition.
  n_draws <- 80
  n_times <- 100
  n_series <- 3
  sigmas <- c(2, 3, 5)
  L_target <- matrix(c(1.0, 0.0, 0.0,
                        0.4, 0.9, 0.0,
                        0.2, 0.3, 0.95), 3, 3, byrow = TRUE)
  L_arr <- array(rep(L_target, each = n_draws),
                 c(n_draws, n_series, n_series))
  sigma_mat <- matrix(rep(sigmas, each = n_draws), n_draws, n_series)
  params <- list(sigma_trend = sigma_mat, L_Omega_trend = L_arr)

  set.seed(202)
  z <- matrix(rnorm(n_draws * n_times * n_series),
              n_draws, n_times * n_series)
  out <- transform_cholesky_innovations(
    z, params, n_times, n_series, n_draws
  )

  # Expected per-series variance is sigma_i^2 * sum(L_target[i, ]^2)
  expected_var <- sigmas^2 * rowSums(L_target^2)
  empirical_var <- vapply(seq_len(n_series), function(s) {
    cols <- (s - 1L) * n_times + seq_len(n_times)
    var(as.vector(out[, cols]))
  }, numeric(1))
  expect_equal(empirical_var, expected_var, tolerance = 0.10)
})


test_that("simple cholesky transform errors on wrong L_Omega shape", {
  n_draws <- 4
  n_series <- 3
  bad_L <- array(0, c(n_draws, n_series + 1, n_series))
  params <- list(
    sigma_trend = matrix(1, n_draws, n_series),
    L_Omega_trend = bad_L
  )
  z <- matrix(rnorm(n_draws * 4 * n_series), n_draws, 4 * n_series)
  expect_error(
    transform_cholesky_innovations(z, params, 4, n_series, n_draws),
    "L_Omega_trend"
  )
})


test_that("extract_simple_cholesky_params reconstructs L correctly", {
  # Build a known L (lower-tri, non-trivial) and a posterior column
  # set in cmdstan order ([1,1] [2,1] [3,1] [1,2] ...). Verify the
  # extraction recovers L exactly via L[i, j] = arr[d, i, j].
  n_draws <- 3
  n_series <- 3
  L_known <- matrix(c(1.0, 0.0, 0.0,
                       0.4, 0.9, 0.0,
                       0.2, 0.3, 0.95), 3, 3, byrow = TRUE)
  cols <- character(0)
  vals <- numeric(0)
  for (j in seq_len(n_series)) {
    for (i in seq_len(n_series)) {
      cols <- c(cols, sprintf("L_Omega_trend[%d,%d]", i, j))
      vals <- c(vals, L_known[i, j])
    }
  }
  for (s in seq_len(n_series)) {
    cols <- c(cols, sprintf("sigma_trend[%d]", s))
    vals <- c(vals, 1.0)
  }
  draws <- matrix(rep(vals, each = n_draws), n_draws,
                   length(cols), dimnames = list(NULL, cols))

  out <- extract_simple_cholesky_params(draws, n_series)
  # Compare entire reconstructed L matrix at draw 1 to known.
  expect_equal(out$L_Omega_trend[1, , ], L_known)
})


test_that("extract_simple_full_cov_params reconstructs Sigma correctly", {
  n_draws <- 2
  n_series <- 3
  Sigma_known <- crossprod(matrix(rnorm(9), 3, 3)) + diag(3)
  cols <- character(0)
  vals <- numeric(0)
  for (j in seq_len(n_series)) {
    for (i in seq_len(n_series)) {
      cols <- c(cols, sprintf("Sigma_trend[%d,%d]", i, j))
      vals <- c(vals, Sigma_known[i, j])
    }
  }
  draws <- matrix(rep(vals, each = n_draws), n_draws,
                   length(cols), dimnames = list(NULL, cols))
  out <- extract_simple_full_cov_params(draws, n_series)
  expect_equal(out$Sigma_trend[1, , ], Sigma_known)
})


test_that("sample_process_errors validates mutually exclusive args", {
  obj <- structure(list(), class = "mvgam")
  expect_error(
    sample_process_errors(obj, ndraws = 5, draw_ids = 1:3),
    "ndraws"
  )
})


test_that("sample_process_errors returns zeros for deterministic trends", {
  # PW maps to "none" covariance pattern; sampler short-circuits
  # without touching any posterior matrix.
  obj <- structure(
    list(
      obs_data = data.frame(
        time = rep(1:3, 2),
        series = factor(rep(c("s1", "s2"), each = 3))
      ),
      trend_components = list(types = "PW")
    ),
    class = "mvgam"
  )
  out <- sample_process_errors(obj, ndraws = 4)
  expect_equal(dim(out), c(4L, 6L))
  expect_true(all(out == 0))

  out_default <- sample_process_errors(obj)
  expect_equal(dim(out_default), c(1L, 6L))

  out_ids <- sample_process_errors(obj, draw_ids = c(2L, 5L, 7L))
  expect_equal(dim(out_ids), c(3L, 6L))
})


test_that("add_innovations_to_linpred adds matrix in univariate case", {
  ndraws <- 4
  nobs <- 6
  linpred <- matrix(seq_len(ndraws * nobs), ndraws, nobs)
  innov <- matrix(0.1, ndraws, nobs)
  out <- add_innovations_to_linpred(linpred, innov)
  expect_true(is.matrix(out))
  expect_equal(dim(out), c(ndraws, nobs))
  expect_equal(out, linpred + innov)
})


test_that("add_innovations_to_linpred adds matrix per-response in mv case", {
  ndraws <- 3
  nobs <- 5
  linpred_list <- list(
    y1 = matrix(0, ndraws, nobs),
    y2 = matrix(10, ndraws, nobs)
  )
  innov <- matrix(0.5, ndraws, nobs)
  out <- add_innovations_to_linpred(linpred_list, innov)
  expect_true(is.list(out) && !is.matrix(out))
  expect_equal(names(out), c("y1", "y2"))
  expect_equal(out$y1, linpred_list$y1 + innov)
  expect_equal(out$y2, linpred_list$y2 + innov)
})


test_that("add_innovations_to_linpred errors on dim mismatch", {
  ndraws <- 3
  nobs <- 5
  linpred <- matrix(0, ndraws, nobs)
  bad <- matrix(0, ndraws, nobs + 1)
  expect_error(
    add_innovations_to_linpred(linpred, bad),
    "dim mismatch"
  )
  expect_error(
    add_innovations_to_linpred(list(y = linpred), bad),
    "dim mismatch"
  )
})


test_that("hierarchical transform produces non-trivial within-group cor", {
  # With a non-identity global Cholesky and alpha=1 (pure global),
  # within-group series should exhibit the prescribed correlation.
  n_draws <- 30
  n_times <- 100
  n_series <- 4
  n_groups <- 2
  n_sub <- 2
  group_info <- list(n_groups = n_groups, n_subgroups = n_sub,
                      group_inds = c(1, 1, 2, 2))

  # Target within-group correlation rho = 0.7
  rho <- 0.7
  L_target <- t(chol(matrix(c(1, rho, rho, 1), 2, 2)))
  L_glob <- array(rep(L_target, each = n_draws), c(n_draws, n_sub, n_sub))
  L_dev <- array(rep(diag(n_sub), each = n_draws * n_groups),
                 c(n_draws, n_groups, n_sub, n_sub))

  params <- list(
    alpha_cor_trend = rep(1.0, n_draws),
    L_Omega_global_trend = L_glob,
    L_deviation_group_trend = L_dev,
    sigma_group_trend = array(1, c(n_draws, n_groups, n_sub))
  )

  set.seed(44)
  z <- matrix(rnorm(n_draws * n_times * n_series),
              n_draws, n_times * n_series)
  innov <- transform_hierarchical_cholesky_innovations(
    z, params, n_times, n_series, n_draws, group_info
  )

  s_cols <- function(s) (s - 1L) * n_times + seq_len(n_times)
  cor_within_g1 <- cor(as.vector(innov[, s_cols(1)]),
                        as.vector(innov[, s_cols(2)]))
  cor_within_g2 <- cor(as.vector(innov[, s_cols(3)]),
                        as.vector(innov[, s_cols(4)]))
  expect_equal(cor_within_g1, rho, tolerance = 0.05)
  expect_equal(cor_within_g2, rho, tolerance = 0.05)
})
