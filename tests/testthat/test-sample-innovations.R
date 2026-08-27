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


test_that("get_trend_covariance_structure routes hier VAR through Cholesky extractor", {
  # Regression guard for the residual_cor hierarchical VAR bug:
  # before the dispatch alias was added,
  # `get_trend_covariance_structure()` left
  # `params$L_Omega_global_trend` NULL for VAR(gr=, subgr=, cor=TRUE)
  # because the switch only recognised "hier.cholesky_scaled",
  # never "hier.full_covariance". The aliasing collapses both keys
  # onto `extract_hierarchical_cholesky_params()` so downstream
  # consumers (`residual_cor()`, `compute_residcor_hierarchical()`)
  # find the population and per-group Cholesky arrays they expect.
  n_draws <- 6
  n_groups <- 2
  n_sub <- 3
  raw_mat <- make_hier_draws_mat(n_draws, n_groups, n_sub)
  draws_mat <- posterior::as_draws_matrix(raw_mat)

  fake_spec <- structure(
    list(n_lv = NULL, gr = "region", subgr = "outcome", cor = TRUE),
    class = "mvgam_trend"
  )
  fake_object <- structure(
    list(
      fit = draws_mat,
      trend_metadata = list(trend_type = "VAR"),
      trend_components = list(
        specifications = fake_spec,
        n_trends = n_groups * n_sub
      ),
      series_info = list(n_series = n_groups * n_sub),
      standata = list(
        N_groups_trend = n_groups,
        N_subgroups_trend = n_sub,
        group_inds_trend = rep(seq_len(n_groups), each = n_sub)
      )
    ),
    class = "mvgam"
  )

  testthat::local_mocked_bindings(
    get_trend_type = function(object) "VAR",
    trend_spec_for_residcor = function(object) fake_spec,
    .package = "mvgam"
  )

  cs <- get_trend_covariance_structure(fake_object)

  expect_equal(cs$pattern, "full_covariance")
  expect_true(cs$hierarchical)
  expect_equal(dim(cs$params$L_Omega_global_trend),
               c(n_draws, n_sub, n_sub))
  expect_equal(dim(cs$params$L_deviation_group_trend),
               c(n_draws, n_groups, n_sub, n_sub))
  expect_equal(length(cs$params$alpha_cor_trend), n_draws)
  # Sanity-check column→slot mapping survives the dispatch.
  expect_equal(
    cs$params$L_Omega_global_trend[, 2, 1],
    as.numeric(raw_mat[, "L_Omega_global_trend[2,1]"])
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


test_that("extract_hierarchical_diagonal_params broadcasts sigma_group_trend per series", {
  # Diagonal-hierarchical path: each series s reads from
  # sigma_group_trend[group_inds[s], sub_idx_of_s_within_group], where
  # sub_idx follows the Stan loop order in
  # generate_hierarchical_correlation_parameters().
  n_draws <- 6
  n_groups <- 2
  n_sub <- 2
  draws_mat <- make_hier_draws_mat(n_draws, n_groups, n_sub)
  group_info <- list(
    n_groups = n_groups, n_subgroups = n_sub,
    group_inds = c(1L, 1L, 2L, 2L)
  )

  params <- extract_hierarchical_diagonal_params(draws_mat, group_info)

  expect_equal(dim(params$sigma_trend), c(n_draws, 4L))
  # Series-to-(group, sub_idx) mapping:
  #   s1 → (g=1, k=1), s2 → (g=1, k=2), s3 → (g=2, k=1), s4 → (g=2, k=2)
  expect_equal(params$sigma_trend[, 1],
               as.numeric(draws_mat[, "sigma_group_trend[1,1]"]))
  expect_equal(params$sigma_trend[, 2],
               as.numeric(draws_mat[, "sigma_group_trend[1,2]"]))
  expect_equal(params$sigma_trend[, 3],
               as.numeric(draws_mat[, "sigma_group_trend[2,1]"]))
  expect_equal(params$sigma_trend[, 4],
               as.numeric(draws_mat[, "sigma_group_trend[2,2]"]))
})


test_that("extract_hierarchical_diagonal_params respects non-contiguous group_inds", {
  # If series are interleaved across groups (e.g. group_inds c(1, 2, 1, 2)),
  # sub_idx must still follow the Stan loop order (cumulative count within
  # each group as series indices are scanned 1..N_lv_trend).
  n_draws <- 4
  draws_mat <- make_hier_draws_mat(n_draws, n_groups = 2, n_sub = 2)
  group_info <- list(
    n_groups = 2L, n_subgroups = 2L,
    group_inds = c(1L, 2L, 1L, 2L)
  )

  params <- extract_hierarchical_diagonal_params(draws_mat, group_info)

  # s1 (g=1, first encounter) → [1, 1]; s2 (g=2, first) → [2, 1];
  # s3 (g=1, second) → [1, 2]; s4 (g=2, second) → [2, 2]
  expect_equal(params$sigma_trend[, 1],
               as.numeric(draws_mat[, "sigma_group_trend[1,1]"]))
  expect_equal(params$sigma_trend[, 2],
               as.numeric(draws_mat[, "sigma_group_trend[2,1]"]))
  expect_equal(params$sigma_trend[, 3],
               as.numeric(draws_mat[, "sigma_group_trend[1,2]"]))
  expect_equal(params$sigma_trend[, 4],
               as.numeric(draws_mat[, "sigma_group_trend[2,2]"]))
})


test_that("extract_hierarchical_diagonal_params errors on missing param", {
  draws_mat <- matrix(0, 5, 3,
                      dimnames = list(NULL, c("a", "b", "c")))
  group_info <- list(n_groups = 1L, n_subgroups = 1L, group_inds = 1L)
  expect_error(
    extract_hierarchical_diagonal_params(draws_mat, group_info),
    "sigma_group_trend"
  )
})


test_that("extract_hierarchical_diagonal_params output feeds transform_diagonal_innovations", {
  # Integration check: the broadcast sigma_trend matrix is the exact
  # shape transform_diagonal_innovations expects, and the per-series
  # variance of the transformed innovations recovers (sigma_group^2).
  n_draws <- 30
  n_times <- 80
  n_groups <- 2
  n_sub <- 2
  n_series <- n_groups * n_sub
  draws_mat <- make_hier_draws_mat(n_draws, n_groups, n_sub)
  group_info <- list(
    n_groups = n_groups, n_subgroups = n_sub,
    group_inds = c(1L, 1L, 2L, 2L)
  )
  params <- extract_hierarchical_diagonal_params(draws_mat, group_info)

  set.seed(33)
  z <- matrix(rnorm(n_draws * n_times * n_series),
              n_draws, n_times * n_series)
  innov <- transform_diagonal_innovations(
    z, params, n_times, n_series, n_draws
  )

  # Per-series sample variance should track sigma_group_trend^2 averaged
  # over draws (since sigma varies per draw via make_hier_draws_mat).
  per_series_var <- function(s) {
    cols <- (s - 1L) * n_times + seq_len(n_times)
    var(as.vector(innov[, cols]))
  }
  expected_var <- function(g, k) {
    mean(as.numeric(
      draws_mat[, sprintf("sigma_group_trend[%d,%d]", g, k)]
    )^2)
  }
  # Loose tolerance: small n_draws + per-draw sigma variation.
  expect_equal(per_series_var(1), expected_var(1, 1), tolerance = 0.25)
  expect_equal(per_series_var(2), expected_var(1, 2), tolerance = 0.25)
  expect_equal(per_series_var(3), expected_var(2, 1), tolerance = 0.25)
  expect_equal(per_series_var(4), expected_var(2, 2), tolerance = 0.25)
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


test_that("compose_linpred_with_noise adds the trend and its noise", {
  ndraws <- 4
  nobs <- 6
  obs <- matrix(seq_len(ndraws * nobs), ndraws, nobs)
  trend <- matrix(2, ndraws, nobs)
  noise <- matrix(0.1, ndraws, nobs)
  out <- compose_linpred_with_noise(obs, trend, noise)
  expect_true(is.matrix(out))
  expect_equal(dim(out), c(ndraws, nobs))
  expect_equal(out, obs + trend + noise)
})


test_that("compose_linpred_with_noise omits noise when there is none", {
  obs <- matrix(1, 3, 5)
  trend <- matrix(2, 3, 5)
  expect_equal(
    compose_linpred_with_noise(obs, trend, NULL),
    obs + trend
  )
})


test_that("compose_linpred_with_noise errors on a trend dim mismatch", {
  obs <- matrix(0, 3, 5)
  bad_trend <- matrix(0, 3, 6)
  expect_error(
    compose_linpred_with_noise(obs, bad_trend, NULL),
    "Dimension mismatch"
  )
  # The response is named so a multivariate fit says which one failed.
  expect_error(
    compose_linpred_with_noise(obs, bad_trend, NULL, resp_name = "y2"),
    "y2"
  )
})


test_that("compose_linpred_with_noise errors on a noise dim mismatch", {
  obs <- matrix(0, 3, 5)
  trend <- matrix(0, 3, 5)
  bad_noise <- matrix(0, 3, 6)
  expect_error(
    compose_linpred_with_noise(obs, trend, bad_noise),
    "Trend-noise dimension mismatch"
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


# ---- innovation degrees of freedom on the forecast path -------------

test_that("extract_nu_trend_draws() covers all three sources", {
  # This is the join between the fitted model and the forecast draw.
  # If it returns the wrong thing, a model fits with heavy tails and
  # forecasts Gaussian without any error being raised.
  dm <- matrix(rnorm(40), nrow = 10, ncol = 4)
  colnames(dm) <- c("Intercept", "sigma_trend[1]", "ar1_trend[1]", "lp__")

  # Gaussian trend: nothing to carry
  gaussian_fit <- list(trend_metadata = list(df = Inf))
  expect_null(mvgam:::extract_nu_trend_draws(dm, gaussian_fit))

  # A fit made before this feature existed has no df at all
  legacy_fit <- list(trend_metadata = list())
  expect_null(mvgam:::extract_nu_trend_draws(dm, legacy_fit))

  # Fixed degrees of freedom are repeated across draws, because no
  # posterior column exists for them
  fixed_fit <- list(trend_metadata = list(df = 7))
  expect_identical(
    mvgam:::extract_nu_trend_draws(dm, fixed_fit), rep(7, 10)
  )

  # Estimated degrees of freedom come from the posterior column and
  # take precedence over whatever the metadata says
  dm_est <- cbind(dm, nu_trend = seq_len(10) + 2)
  est_fit <- list(trend_metadata = list(df = NA_real_))
  expect_identical(
    mvgam:::extract_nu_trend_draws(dm_est, est_fit), as.numeric(2 + 1:10)
  )
  # a stale metadata value must not win over the posterior column
  expect_identical(
    mvgam:::extract_nu_trend_draws(dm_est, fixed_fit), as.numeric(2 + 1:10)
  )
})

test_that("extract_nu_trend_draws() returns nothing for an unfitted df", {
  # `df = NA` means "estimate", so before a fit exists there is no value
  # to forecast with; the caller falls back to Gaussian draws.
  dm <- matrix(0, 5, 1, dimnames = list(NULL, "Intercept"))
  expect_null(
    mvgam:::extract_nu_trend_draws(dm, list(trend_metadata = list(df = NA)))
  )
})


test_that("one naming of the covariance structure serves both readers", {
  # A grouped trend carries its correlations as a population Cholesky
  # factor plus per-group deviations whatever the constructor was, so a
  # hierarchical VAR is parameterised exactly as a hierarchical AR.
  # Parameter extraction and the innovation transform both ask this
  # helper, so they cannot disagree about which shape they hold: when
  # they did, a hierarchical VAR was handed Cholesky parameters and
  # then asked for a full covariance nothing had produced, taking down
  # every post-fit surface that samples process error.
  expect_equal(
    covariance_structure_key(TRUE, "full_covariance"),
    "hier.cholesky_scaled"
  )
  expect_equal(
    covariance_structure_key(TRUE, "cholesky_scaled"),
    "hier.cholesky_scaled"
  )
  # A grouped trend without correlations keeps its own diagonal shape.
  expect_equal(covariance_structure_key(TRUE, "diagonal"), "hier.diagonal")
  # Ungrouped trends are named by their pattern alone; a flat VAR does
  # carry a full covariance and must not be folded into the Cholesky
  # branch.
  expect_equal(
    covariance_structure_key(FALSE, "full_covariance"),
    "flat.full_covariance"
  )
  expect_equal(
    covariance_structure_key(FALSE, "cholesky_scaled"),
    "flat.cholesky_scaled"
  )
  expect_equal(covariance_structure_key(FALSE, "diagonal"), "flat.diagonal")
})


test_that("bin_draws() keeps the shape of a posterior at a fraction of it", {
  set.seed(3L)
  draws <- rnorm(4000)
  b <- bin_draws(draws, bins = 30L)
  # The bins are the histogram the draws themselves would give.
  ref <- graphics::hist(
    draws,
    breaks = seq(min(draws), max(draws), length.out = 31L),
    plot = FALSE
  )
  expect_equal(b$breaks, ref$breaks)
  expect_equal(b$counts, as.integer(ref$counts))
  # Every draw is accounted for, so a plot built from the bins has the
  # same mass as one built from the draws.
  expect_equal(sum(b$counts), length(draws))

  # A metric that never varies has no range to divide, and gets one
  # degenerate bin rather than an error.
  flat <- bin_draws(rep(2.5, 100L), bins = 30L)
  expect_equal(flat$counts, 100L)
  expect_equal(flat$breaks, c(2.5, 2.5))

  # Non-finite draws are dropped rather than poisoning the range.
  mixed <- bin_draws(c(rnorm(50), NA, Inf), bins = 10L)
  expect_equal(sum(mixed$counts), 50L)
  expect_true(all(is.finite(mixed$breaks)))

  # Nothing to bin is empty, not an error.
  expect_equal(bin_draws(numeric(0), bins = 10L)$counts, 0L)
})

test_that("extract_sigma_and_cov: a single series gives a 1x1 covariance", {
  # `diag(x)` for a length-one x builds an x-by-x identity rather than
  # a 1x1 matrix holding x, so a one-series correlated trend used to
  # produce a non-conformable Sigma and abort the forecast.
  one_draw <- c("sigma_trend[1]" = 0.4, "L_Omega_trend[1,1]" = 1)
  out <- extract_sigma_and_cov(one_draw, n_series = 1L, n_lv = 1L,
                               has_cor = TRUE)

  expect_equal(dim(out$Sigma), c(1L, 1L))
  expect_equal(as.numeric(out$Sigma), 0.16)
  expect_equal(out$sigma, 0.4, ignore_attr = TRUE)
})

test_that("extract_sigma_and_cov: two series scale the correlation both ways", {
  one_draw <- c(
    "sigma_trend[1]" = 2, "sigma_trend[2]" = 3,
    "L_Omega_trend[1,1]" = 1,
    "L_Omega_trend[2,1]" = 0.6, "L_Omega_trend[2,2]" = 0.8
  )
  out <- extract_sigma_and_cov(one_draw, n_series = 2L, n_lv = 2L,
                               has_cor = TRUE)

  L <- matrix(c(1, 0.6, 0, 0.8), nrow = 2L)
  expected <- diag(c(2, 3)) %*% tcrossprod(L) %*% diag(c(2, 3))
  expect_equal(out$Sigma, expected, ignore_attr = TRUE)
  expect_equal(diag(out$Sigma), c(4, 9), ignore_attr = TRUE)
})

test_that("extract_sigma_and_cov: a grouped trend reads its own parameters", {
  # A grouped trend carries no `sigma_trend` or `L_Omega_trend` at all:
  # its scales are per group and its correlations are a population
  # factor pulled towards each group's own. Asking for the flat names
  # used to abort the forecast with a subscript error.
  n_sub <- 2L
  n_groups <- 2L
  group_inds <- c(1L, 1L, 2L, 2L)
  one_draw <- c("alpha_cor_trend" = 0.5)
  L_global <- matrix(c(1, 0.5, 0, sqrt(1 - 0.25)), nrow = n_sub)
  for (i in 1:n_sub) for (j in 1:n_sub) {
    one_draw[sprintf("L_Omega_global_trend[%d,%d]", i, j)] <- L_global[i, j]
  }
  L_dev <- list(diag(n_sub), matrix(c(1, -0.4, 0, sqrt(1 - 0.16)), nrow = n_sub))
  sigmas <- list(c(1, 2), c(3, 0.5))
  for (g in seq_len(n_groups)) {
    for (i in 1:n_sub) for (j in 1:n_sub) {
      one_draw[sprintf("L_deviation_group_trend[%d,%d,%d]", g, i, j)] <-
        L_dev[[g]][i, j]
    }
    for (k in 1:n_sub) {
      one_draw[sprintf("sigma_group_trend[%d,%d]", g, k)] <- sigmas[[g]][k]
    }
  }
  group_info <- list(n_groups = n_groups, n_subgroups = n_sub,
                     group_inds = group_inds)

  out <- extract_hierarchical_sigma_and_cov(one_draw, 4L, group_info)

  expect_equal(dim(out$Sigma), c(4L, 4L))
  expect_equal(out$sigma, c(1, 2, 3, 0.5), ignore_attr = TRUE)
  # Groups are independent, so every cross-group entry is zero.
  expect_true(all(out$Sigma[1:2, 3:4] == 0))
  expect_true(all(out$Sigma[3:4, 1:2] == 0))
  # Each block is the covariance the shared helper builds.
  for (g in seq_len(n_groups)) {
    L_full <- hierarchical_group_cholesky(
      alpha = 0.5, L_global = L_global, L_deviation = L_dev[[g]],
      sigma = sigmas[[g]]
    )
    idx <- which(group_inds == g)
    expect_equal(out$Sigma[idx, idx], tcrossprod(L_full),
                 ignore_attr = TRUE)
  }
  expect_equal(diag(out$Sigma), out$sigma^2, ignore_attr = TRUE)
})

test_that("hierarchical_group_cholesky: alpha weights population against group", {
  L_global <- matrix(c(1, 0.8, 0, 0.6), nrow = 2L)
  L_dev <- diag(2L)
  sigma <- c(1, 1)

  # alpha = 1 keeps the population correlation alone.
  all_pop <- hierarchical_group_cholesky(1, L_global, L_dev, sigma)
  expect_equal(tcrossprod(all_pop), tcrossprod(L_global))
  # alpha = 0 keeps the group's own.
  all_grp <- hierarchical_group_cholesky(0, L_global, L_dev, sigma)
  expect_equal(tcrossprod(all_grp), diag(2L))
  # sigma scales the covariance by its outer product.
  scaled <- hierarchical_group_cholesky(1, L_global, L_dev, c(2, 3))
  expect_equal(tcrossprod(scaled),
               diag(c(2, 3)) %*% tcrossprod(L_global) %*% diag(c(2, 3)))
})
