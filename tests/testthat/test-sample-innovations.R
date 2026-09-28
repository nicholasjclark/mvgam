#' Tests for Innovation Sampling Infrastructure
#'
#' Unit-level tests for draw_innovation_grid() and the per-pattern
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


test_that("extract_hierarchical_cholesky_params maps columns to slots", {
  n_draws <- 8
  n_groups <- 2
  n_sub <- 2
  draws_mat <- make_hier_draws_mat(n_draws, n_groups, n_sub)
  group_info <- get_group_info(list(
    N_groups_trend = n_groups, N_subgroups_trend = n_sub,
    group_inds_trend = c(1, 1, 2, 2)
  ))

  params <- extract_hierarchical_cholesky_params(draws_mat, group_info)

  expect_equal(length(params$alpha_cor_trend), n_draws)
  expect_equal(dim(params$L_Omega_global_trend),
               c(n_draws, n_sub, n_sub))
  expect_equal(dim(params$L_deviation_group_trend),
               c(n_draws, n_groups, n_sub, n_sub))
  expect_equal(dim(params$sigma_group_trend),
               c(n_draws, n_groups, n_sub))
  # Each posterior column reaches the array slot its name indexes.
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


test_that("a hier VAR routes through the Cholesky extractor", {
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


test_that("a shared AR coefficient hides its broadcast copies", {
  # `coef_sharing = "shared"` samples one scalar per lag and copies
  # it across the series. The copies repeat that scalar, and the
  # default parameter view keeps the sampled name.
  shared_pars <- c("shared_ar1_trend[1]", "ar1_trend[1]",
                   "ar1_trend[2]", "sigma_trend[1]")
  hidden <- is_hidden_par(shared_pars)
  expect_identical(shared_pars[hidden],
                   c("ar1_trend[1]", "ar1_trend[2]"))

  # The partial-autocorrelation spelling a contiguous `p >= 2` fit
  # uses takes the same rule.
  pacf_pars <- c("shared_ar1_pacf_trend[1]", "ar1_pacf_trend[1]",
                 "ar1_pacf_trend[2]")
  expect_identical(pacf_pars[is_hidden_par(pacf_pars)],
                   c("ar1_pacf_trend[1]", "ar1_pacf_trend[2]"))

  # `coef_sharing = "hierarchical"` draws each series' coefficient
  # from a population distribution, and each one is its own quantity.
  hier_pars <- c("mu_ar1_trend[1]", "sigma_ar1_trend[1]",
                 "ar1_trend[1]", "ar1_trend[2]")
  expect_false(any(is_hidden_par(hier_pars)))
})


test_that("the flat stationary split rebuilds its own covariance", {
  # A posterior stores a Cholesky correlation factor whose rows land
  # a little off unit norm. The scale and correlation it returns have
  # to rebuild `diag(sigma) %*% Gamma %*% diag(sigma)` all the same,
  # which pins the split itself.
  n <- 3L
  ndraws <- 5L
  R <- matrix(0.3, n, n)
  diag(R) <- 1
  L0 <- t(chol(R))
  # A perturbation the size of the precision a stored draw carries.
  L0[2L, 1L] <- L0[2L, 1L] + 1e-8
  expect_gt(max(abs(diag(tcrossprod(L0)) - 1)), 0)

  params <- list(
    sigma_trend = matrix(seq(0.5, 0.9, length.out = ndraws * n),
                         ndraws, n),
    L_Omega_trend = array(rep(as.numeric(L0), each = ndraws),
                          c(ndraws, n, n))
  )
  phi <- matrix(rep(c(0.7, 0.2, -0.3), each = ndraws), ndraws, n)

  out <- stationary_correlated_params(
    params,
    ar_stationary_factor(list(lags = 1L, phi = list(phi), theta = NULL))
  )
  for (d in seq_len(ndraws)) {
    m <- 1 / (1 - outer(phi[d, ], phi[d, ]))
    s0 <- params$sigma_trend[d, ]
    target <- diag(s0, nrow = n) %*% (tcrossprod(L0) * m) %*%
      diag(s0, nrow = n)
    s1 <- out$sigma_trend[d, ]
    rebuilt <- diag(s1, nrow = n) %*%
      tcrossprod(matrix(out$L_Omega_trend[d, , ], n, n)) %*%
      diag(s1, nrow = n)
    expect_equal(rebuilt, target, tolerance = 1e-12)
  }
})


test_that("a grouped VAR takes its stationary blocks from Omega_trend", {
  # The program solves `Omega = A Omega A' + Sigma` over every series
  # and gives its first latent state that covariance. Groups keep to
  # their own block of it, and each block becomes one factor.
  n_g <- 2L
  n_s <- 2L
  n_lv <- n_g * n_s
  ndraws <- 4L
  group_info <- get_group_info(list(
    N_groups_trend = n_g, N_subgroups_trend = n_s,
    group_inds_trend = c(1L, 1L, 2L, 2L)
  ))

  blk1 <- matrix(c(0.40, 0.10, 0.10, 0.25), 2, 2)
  blk2 <- matrix(c(0.30, -0.05, -0.05, 0.20), 2, 2)
  omega <- matrix(0, n_lv, n_lv)
  omega[1:2, 1:2] <- blk1
  omega[3:4, 3:4] <- blk2

  nms <- as.vector(outer(
    seq_len(n_lv), seq_len(n_lv),
    function(i, j) sprintf("Omega_trend[%d,%d]", i, j)
  ))
  draws_mat <- matrix(rep(as.numeric(omega), each = ndraws),
                      nrow = ndraws, dimnames = list(NULL, nms))

  eye <- as.numeric(diag(n_s))
  params <- list(
    alpha_cor_trend = rep(0.5, ndraws),
    L_Omega_global_trend = array(rep(eye, each = ndraws),
                                 c(ndraws, n_s, n_s)),
    L_deviation_group_trend = array(rep(eye, each = ndraws * n_g),
                                    c(ndraws, n_g, n_s, n_s)),
    sigma_group_trend = array(1, c(ndraws, n_g, n_s))
  )

  out <- stationary_group_var_params(params, draws_mat, group_info)
  L_stat <- out$L_group_stationary
  expect_false(is.null(L_stat))
  expect_identical(dim(L_stat), c(ndraws, n_g, n_s, n_s))
  for (d in seq_len(ndraws)) {
    expect_equal(tcrossprod(matrix(L_stat[d, 1, , ], n_s, n_s)), blk1)
    expect_equal(tcrossprod(matrix(L_stat[d, 2, , ], n_s, n_s)), blk2)
  }

  # The identity matrices above make the innovation covariance the
  # unit matrix, which the blocks depart from. A fit whose parameters
  # came back unlifted would fail here.
  expect_gt(max(abs(tcrossprod(matrix(L_stat[1, 1, , ], n_s, n_s)) -
                      diag(n_s))), 0.1)

  # A fit with no grouping gives the parameters back as they were.
  expect_identical(
    stationary_group_var_params(params, draws_mat, list()),
    params
  )
})


test_that("hierarchical transform: identity Chol + unit sigma is identity", {
  n_draws <- 4
  n_times <- 6
  n_series <- 4
  n_groups <- 2
  n_sub <- 2
  group_info <- get_group_info(list(
    N_groups_trend = n_groups, N_subgroups_trend = n_sub,
    group_inds_trend = c(1, 1, 2, 2)
  ))
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
  group_info <- get_group_info(list(
    N_groups_trend = n_groups, N_subgroups_trend = n_sub,
    group_inds_trend = c(1, 1, 2, 2)
  ))
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
  group_info <- get_group_info(list(
    N_groups_trend = n_groups, N_subgroups_trend = n_sub,
    group_inds_trend = c(1, 1, 2, 2)
  ))
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
  group_info <- get_group_info(list(
    N_groups_trend = n_groups, N_subgroups_trend = n_sub,
    group_inds_trend = c(1, 1, 2, 2)
  ))
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
  expect_true(all(out_default == 0))

  out_ids <- sample_process_errors(obj, draw_ids = c(2L, 5L, 7L))
  expect_equal(dim(out_ids), c(3L, 6L))
  expect_true(all(out_ids == 0))
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
  group_info <- get_group_info(list(
    N_groups_trend = n_groups, N_subgroups_trend = n_sub,
    group_inds_trend = c(1, 1, 2, 2)
  ))

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

# One posterior draw in the shape `extract_last_state()` passes its
# helpers: a one-row matrix named by Stan parameter.
one_draw_matrix <- function(values) {
  matrix(values, nrow = 1L, dimnames = list(NULL, names(values)))
}

test_that("extract_sigma_and_cov: a single series gives a 1x1 covariance", {
  # `diag(x)` for a length-one x builds an x-by-x identity rather than
  # a 1x1 matrix holding x, so a one-series correlated trend needs its
  # own path to avoid a non-conformable Sigma.
  one_draw <- one_draw_matrix(c("sigma_trend[1]" = 0.4,
                                "L_Omega_trend[1,1]" = 1))
  out <- extract_sigma_and_cov(one_draw, n_series = 1L, n_lv = 1L,
                               has_cor = TRUE)

  expect_equal(dim(out$Sigma), c(1L, 1L))
  expect_equal(as.numeric(out$Sigma), 0.16)
  expect_equal(out$sigma, 0.4, ignore_attr = TRUE)
})

test_that("extract_sigma_and_cov: two series scale the correlation both ways", {
  # A posterior stores every element of a Cholesky factor, the zero
  # above the diagonal included.
  one_draw <- one_draw_matrix(c(
    "sigma_trend[1]" = 2, "sigma_trend[2]" = 3,
    "L_Omega_trend[1,1]" = 1, "L_Omega_trend[1,2]" = 0,
    "L_Omega_trend[2,1]" = 0.6, "L_Omega_trend[2,2]" = 0.8
  ))
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
  # factor pulled towards each group's own. The extractor must resolve
  # the grouped names rather than the flat ones.
  n_sub <- 2L
  n_groups <- 2L
  group_inds <- c(1L, 1L, 2L, 2L)
  one_draw <- c("alpha_cor_trend" = 0.5)
  L_global <- matrix(c(1, 0.5, 0, sqrt(1 - 0.25)), nrow = n_sub)
  for (i in 1:n_sub) for (j in 1:n_sub) {
    one_draw[sprintf("L_Omega_global_trend[%d,%d]", i, j)] <- L_global[i, j]
  }
  L_dev <- list(
    diag(n_sub),
    matrix(c(1, -0.4, 0, sqrt(1 - 0.16)), nrow = n_sub)
  )
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
  group_info <- get_group_info(list(
    N_groups_trend = n_groups, N_subgroups_trend = n_sub,
    group_inds_trend = group_inds
  ))

  out <- extract_hierarchical_sigma_and_cov(one_draw_matrix(one_draw), 4L,
                                            group_info)

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

test_that("hierarchical_group_cholesky weights by alpha", {
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


# A marginal prediction integrates over the state's own spread, and
# the covariance the innovation transforms take is the stationary one.
# A fitted fixture shows one corner of that arithmetic, and the tests
# below pin the rest against closed forms and the companion definition.

# Leading `m x m` block of the stationary covariance of a companion
# state, `sum_k A^k Q A'^k`. Written from the definition, it calls
# nothing from the package.
neumann_lead <- function(A, Q, m, n_terms = 3000L) {
  out <- matrix(0, nrow(A), ncol(A))
  Ak <- diag(nrow(A))
  for (k in 0:n_terms) {
    out <- out + Ak %*% Q %*% t(Ak)
    Ak <- Ak %*% A
  }
  out[seq_len(m), seq_len(m)]
}

# One draw's coefficients in the shape `ar_coef_draws()` returns.
one_draw_coefs <- function(lags, phi, theta = NULL) {
  list(lags = lags,
       phi = lapply(phi, function(v) matrix(v, nrow = 1L)),
       theta = if (is.null(theta)) NULL else matrix(theta, nrow = 1L))
}

stationary_factor <- function(...) {
  f <- ar_stationary_factor(one_draw_coefs(...))
  matrix(f[1L, , ], dim(f)[2L], dim(f)[3L])
}


test_that("one lag settles at 1 / (1 - phi_a phi_b) for every pair", {
  # The cross-covariance factor of two series takes both coefficients,
  # and the off-diagonal cells of the assertion check that product.
  phi <- c(0.8, 0.15, -0.4)
  expect_equal(stationary_factor(1L, list(phi)),
               1 / (1 - outer(phi, phi)), tolerance = 1e-10)
  # Zero coefficients leave the innovation covariance alone.
  expect_equal(stationary_factor(1L, list(rep(0, 3L))), matrix(1, 3L, 3L))
})


test_that("two lags settle at the AR(2) closed form", {
  # Var = s2 (1 - p2) / ((1 + p2)((1 - p2)^2 - p1^2))
  p1 <- c(0.4, -0.3, 0.5)
  p2 <- c(0.2, 0.25, -0.4)
  got <- diag(stationary_factor(1:2, list(p1, p2)))
  expect_equal(got, (1 - p2) / ((1 + p2) * ((1 - p2)^2 - p1^2)),
               tolerance = 1e-10)
})


test_that("a sparse lag set settles at its own companion's block", {
  # `p = c(1, 12)` carries zeros at the lags in between. The companion
  # below places them by hand. The two series take different
  # coefficients, which sends their stationary correlation to 0.20 from
  # an innovation correlation of 0.6. Scaling each variance on its own
  # would leave it at 0.6.
  phi1 <- c(0.8, -0.5)
  phi12 <- c(0.1, 0.1)
  S <- matrix(c(1, 0.6, 0.6, 1), 2L, 2L)
  m <- 2L
  A <- matrix(0, 12L * m, 12L * m)
  A[1:m, 1:m] <- diag(phi1)
  A[1:m, (11L * m + 1L):(12L * m)] <- diag(phi12)
  A[cbind((m + 1L):(12L * m), 1:(11L * m))] <- 1
  Q <- matrix(0, 12L * m, 12L * m)
  Q[1:m, 1:m] <- S
  want <- neumann_lead(A, Q, m)
  got <- S * stationary_factor(c(1L, 12L), list(phi1, phi12))
  expect_equal(got, want, tolerance = 1e-10)
  expect_lt(abs(stats::cov2cor(got)[1L, 2L] - 0.2), 0.01)
})


test_that("an explosive autoregression takes an NA factor", {
  # A sparse lag set produces such a draw. Each coefficient lies inside
  # the unit interval on its own, and the process they jointly define
  # grows without limit. The second, stationary draw keeps its factor.
  f <- ar_stationary_factor(list(
    lags = c(1L, 12L),
    phi = list(matrix(c(0.9, 0.3), 2L), matrix(c(0.9, 0.2), 2L)),
    theta = NULL
  ))
  expect_true(is.na(f[1L, 1L, 1L]))
  expect_true(is.finite(f[2L, 1L, 1L]))
})


test_that("a draw a step from a unit root settles at its closed form", {
  # `ar1 = 1 - 1e-7` needs 10^8 weights to settle, and a truncated sum
  # would call the draw explosive and keep its innovation variance, a
  # factor of 5 x 10^6 too small. The draw takes the companion solve,
  # which the closed forms below check. The ARMA(1, 1) variance is
  # `(1 + 2 ar1 theta + theta^2) / (1 - ar1^2)`.
  ar1 <- c(1 - 1e-7, 0.99995)
  theta <- c(0, 0.4)
  f <- ar_stationary_factor(list(
    lags = 1L, phi = list(matrix(ar1, 1L)), theta = matrix(theta, 1L)
  ))
  want <- (1 + 2 * ar1 * theta + theta^2) / (1 - ar1^2)
  expect_equal(diag(matrix(f[1L, , ], 2L, 2L)), want, tolerance = 1e-6)
  # A unit root has no stationary distribution.
  expect_true(is.na(stationary_factor(1L, list(1))[1L, 1L]))
})


test_that("the variances alone match the full factor's diagonal", {
  # Independent series ask for no cross-covariance and sum their own
  # weights alone.
  phi <- list(matrix(c(0.5, -0.3, 0.8, 0.1), 2L), matrix(0.2, 2L, 2L))
  co <- list(lags = 1:2, phi = phi, theta = NULL)
  full <- ar_stationary_factor(co)
  own <- ar_stationary_factor(co, cross = FALSE)
  expect_equal(own[, 1L, 1L], full[, 1L, 1L])
  expect_equal(own[, 2L, 2L], full[, 2L, 2L])
  expect_identical(own[, 1L, 2L], c(0, 0))
})


test_that("correlated ARMA(2, 1) series settle at the companion block", {
  # Two lags and a moving-average term leave no closed form. The
  # companion state is `(y_t, y_{t-1}, eps_t)`, and `eps_t` enters
  # the state twice, once in `y_t` and once in its own block.
  n <- 2L
  sigma <- matrix(c(0.8, 1.3), nrow = 1L)
  omega <- matrix(c(1, 0.6, 0.6, 1), n, n)
  L <- array(t(chol(omega)), dim = c(1L, n, n))
  p1 <- c(0.5, -0.2)
  p2 <- c(0.3, 0.4)
  th <- c(0.6, -0.5)
  lift <- function(theta) {
    stationary_correlated_params(
      list(sigma_trend = sigma, L_Omega_trend = L),
      ar_stationary_factor(one_draw_coefs(1:2, list(p1, p2), theta))
    )
  }
  out <- lift(th)
  got <- diag(out$sigma_trend[1L, ]) %*%
    tcrossprod(matrix(out$L_Omega_trend[1L, , ], n, n)) %*%
    diag(out$sigma_trend[1L, ])

  S <- diag(sigma[1L, ]) %*% omega %*% diag(sigma[1L, ])
  Z <- matrix(0, n, n)
  A <- rbind(cbind(diag(p1), diag(p2), diag(th)),
             cbind(diag(n), Z, Z),
             cbind(Z, Z, Z))
  Q <- rbind(cbind(S, Z, S), cbind(Z, Z, Z), cbind(S, Z, S))
  expect_equal(got, neumann_lead(A, Q, n), tolerance = 1e-10)
  # Dropping the moving-average term moves the scales by more than
  # 0.1, which shows the assertion above tests the term.
  expect_gt(max(abs(lift(NULL)$sigma_trend - out$sigma_trend)), 0.1)
})


test_that("a singular stationary covariance keeps innovations", {
  # Perfectly correlated innovations under equal coefficients leave
  # the stationary correlation singular, without a Cholesky factor.
  # That draw is left as it was, and the second draw is still
  # transformed.
  n <- 2L
  sigma <- matrix(c(0.8, 1.3, 0.8, 1.3), nrow = 2L, byrow = TRUE)
  L <- array(0, dim = c(2L, n, n))
  L[1L, , ] <- matrix(c(1, 1, 0, 0), n, n)
  L[2L, , ] <- t(chol(matrix(c(1, 0.6, 0.6, 1), n, n)))
  phi <- matrix(c(0.5, 0.5, 0.8, 0.1), nrow = 2L, byrow = TRUE)
  out <- stationary_correlated_params(
    list(sigma_trend = sigma, L_Omega_trend = L),
    ar_stationary_factor(list(lags = 1L, phi = list(phi), theta = NULL))
  )
  expect_identical(out$sigma_trend[1L, ], sigma[1L, ])
  expect_identical(out$L_Omega_trend[1L, , ], L[1L, , ])
  expect_false(identical(out$sigma_trend[2L, ], sigma[2L, ]))
})


test_that("a draws reader returns NULL for a parameter not carried", {
  dm <- matrix(1, nrow = 4L, ncol = 2L,
                dimnames = list(NULL, c("a[1]", "a[2]")))
  expect_equal(dim(read_draws_vector(dm, "a", 2L)), c(4L, 2L))
  expect_null(read_draws_vector(dm, "b", 2L, required = FALSE))
  expect_error(read_draws_vector(dm, "b", 2L), "incomplete")
})


test_that("the trend type is resolved in one place", {
  # `detect_var_trend()` calls `get_trend_type()`, giving `irf()`,
  # `fevd()`, `stability()` and `posterior_transition_matrix()` the
  # type `summary()` prints.
  obj <- structure(
    list(
      trend_components = list(types = "VAR"),
      trend_formula = ~ VAR(p = 1)
    ),
    class = "mvgam"
  )
  expect_equal(get_trend_type(obj), "VAR")
  expect_equal(detect_var_trend(obj), "VAR")

  # An empty type resolves to "None", and `detect_var_trend()`
  # gives NULL.
  empty <- obj
  empty$trend_components$types <- NA_character_
  expect_equal(get_trend_type(empty), "None")
  expect_null(detect_var_trend(empty))

  # An AR trend gives NULL from `detect_var_trend()`.
  ar <- obj
  ar$trend_components$types <- "AR"
  expect_equal(get_trend_type(ar), "AR")
  expect_null(detect_var_trend(ar))
})
