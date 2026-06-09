## Coverage for irf.mvgam / fevd.mvgam / stability.mvgam after the
## brms-integration port. Three layers of test:
##   1. Trend-type gate (`assert_var_trend`) on a non-VAR mock.
##   2. Posterior extractor (`extract_var_posterior`) on a mock
##      `posterior::draws_matrix` with the right Stan column names.
##   3. Math helpers (`var_phi`, `var_psi`, `var_fecov`, `gen_irf`,
##      `gen_fevd`) on hand-built A / Sigma where the answer is
##      analytically known.
##
## Sampling-end-to-end coverage lives in `tests/local/test-irf-fevd.R`
## (uses the cached val_mvgam_var_cor.rds fixture).

# ---- Build a minimal mvgam stub backed by a draws_matrix ----------

# `extract_var_posterior` only needs (i) object$fit to be something
# `posterior::as_draws_matrix` accepts and (ii) trend_components /
# obs_data to resolve K. A draws_matrix dispatches through the
# posterior generic as the identity, so we can hand one in directly
# without going through Stan.
build_var_mock <- function(K = 3L, ndraws = 50L,
                           trend_type = "VAR1cor", seed = 1L) {
  set.seed(seed)
  cols <- character(0L)
  for (i in seq_len(K)) for (j in seq_len(K)) {
    cols <- c(cols, sprintf("A_trend[1,%d,%d]", i, j))
  }
  for (i in seq_len(K)) for (j in seq_len(K)) {
    cols <- c(cols, sprintf("Sigma_trend[%d,%d]", i, j))
  }
  vals <- matrix(rnorm(ndraws * length(cols), sd = 0.1),
                 nrow = ndraws, ncol = length(cols),
                 dimnames = list(NULL, cols))
  # Force the diagonal of Sigma_trend to be positive so the math
  # helpers (chol, eigen, kronecker inverse) stay numerically sane.
  for (i in seq_len(K)) {
    sig_d <- sprintf("Sigma_trend[%d,%d]", i, i)
    vals[, sig_d] <- abs(vals[, sig_d]) + 0.5
  }
  draws <- posterior::as_draws_matrix(vals)

  structure(
    list(
      fit = draws,
      trend_components = list(types = trend_type),
      standata = list(N_lv_trend = K, N_series_trend = K)
    ),
    class = "mvgam"
  )
}

# ---- Gate ----------------------------------------------------------

test_that("assert_var_trend() rejects non-VAR fits with a clear pointer", {
  fake <- structure(
    list(trend_components = list(types = "RW")),
    class = "mvgam"
  )
  expect_error(
    assert_var_trend(fake, surface = "irf()"),
    "requires a VAR\\(1\\) latent trend"
  )
  expect_error(
    assert_var_trend(fake, surface = "irf()"),
    "'RW'"
  )
})

test_that("detect_var_trend() recognises all four VAR-type spellings", {
  for (tt in c("VAR", "VAR1", "VARcor", "VAR1cor")) {
    fake <- structure(
      list(trend_components = list(types = tt)),
      class = "mvgam"
    )
    expect_identical(detect_var_trend(fake), tt)
  }
  fake_none <- structure(
    list(trend_components = list(types = NULL)),
    class = "mvgam"
  )
  expect_null(detect_var_trend(fake_none))
})

# ---- Extractor -----------------------------------------------------

test_that("extract_var_posterior() returns correctly shaped (A, Sigma) arrays", {
  mock <- build_var_mock(K = 3L, ndraws = 25L)
  vp <- extract_var_posterior(mock)
  expect_identical(vp$K, 3L)
  expect_identical(vp$ndraws, 25L)
  expect_identical(dim(vp$A), c(25L, 3L, 3L))
  expect_identical(dim(vp$Sigma), c(25L, 3L, 3L))
})

test_that("extract_var_posterior() reads the correct column for each (i, j)", {
  # Stamp known values into A_trend[1,2,3] and Sigma_trend[1,1] so we
  # can verify the indexing convention round-trips correctly.
  mock <- build_var_mock(K = 3L, ndraws = 4L)
  mock$fit[1L, "A_trend[1,2,3]"] <- 0.7
  mock$fit[2L, "Sigma_trend[1,1]"] <- 1.25
  vp <- extract_var_posterior(mock)
  expect_equal(vp$A[1L, 2L, 3L], 0.7)
  expect_equal(vp$Sigma[2L, 1L, 1L], 1.25)
})

test_that("extract_var_posterior() errors when the expected column is missing", {
  mock <- build_var_mock(K = 2L, ndraws = 3L)
  drop_col <- "A_trend[1,1,1]"
  mock$fit <- mock$fit[, setdiff(colnames(mock$fit), drop_col), drop = FALSE]
  expect_error(
    extract_var_posterior(mock),
    paste0("'", "A_trend\\[1,1,1\\]", "' not found")
  )
})

# ---- Math helpers --------------------------------------------------

test_that("var_phi() returns identity at lag 0 and zero powers when A = 0", {
  K <- 3L
  x <- list(K = K, A = matrix(0, K, K),
            Sigma = diag(K), p = 1L)
  Phi <- var_phi(x, h = 5L)
  expect_identical(dim(Phi), c(K, K, 6L))
  expect_equal(Phi[, , 1L], diag(K))
  for (i in 2:6) {
    expect_equal(Phi[, , i], matrix(0, K, K))
  }
})

test_that("var_phi() recovers A^k for diagonal A", {
  K <- 2L
  A <- diag(c(0.5, 0.3))
  x <- list(K = K, A = A, Sigma = diag(K), p = 1L)
  Phi <- var_phi(x, h = 3L)
  # Phi[, , k+1] = A^k for the i.i.d. case with diagonal A.
  expect_equal(Phi[, , 2L], A)
  expect_equal(Phi[, , 3L], A %*% A)
  expect_equal(Phi[, , 4L], A %*% A %*% A)
})

test_that("gen_fevd() rows sum to 1 across the columns for each horizon", {
  K <- 3L
  set.seed(7L)
  # Random stable A by shrinking eigenvalues into the unit circle.
  A_raw <- matrix(stats::rnorm(K * K, sd = 0.4), K, K)
  ev <- max(abs(eigen(A_raw)$values))
  A <- A_raw / (1.2 * ev)
  Sigma <- crossprod(matrix(stats::rnorm(K * K), K, K)) + diag(K)
  x <- list(K = K, A = A, Sigma = Sigma, p = 1L)
  fevd <- gen_fevd(x, h = 5L)
  expect_length(fevd, K)
  for (i in seq_len(K)) {
    # For each "responding" series i, the contributions across all K
    # shock origins should sum to 1 at every horizon.
    row_sums <- rowSums(fevd[[i]])
    expect_equal(row_sums, rep(1.0, 5L))
  }
})

test_that("gen_irf() returns finite arrays of the right shape", {
  K <- 2L
  set.seed(11L)
  A <- matrix(c(0.4, 0.1, 0.05, 0.3), K, K)
  Sigma <- diag(K)
  x <- list(K = K, A = A, Sigma = Sigma, p = 1L)
  irf_gen <- gen_irf(x, h = 6L, cumulative = FALSE, orthogonal = FALSE)
  irf_orth <- gen_irf(x, h = 6L, cumulative = FALSE, orthogonal = TRUE)
  irf_cum <- gen_irf(x, h = 6L, cumulative = TRUE, orthogonal = FALSE)
  expect_length(irf_gen, K)
  expect_length(irf_orth, K)
  expect_length(irf_cum, K)
  for (i in seq_len(K)) {
    expect_identical(dim(irf_gen[[i]]), c(6L, K))
    expect_true(all(is.finite(irf_gen[[i]])))
    expect_true(all(is.finite(irf_orth[[i]])))
    expect_true(all(is.finite(irf_cum[[i]])))
  }
  # Cumulative is monotone in absolute terms only when responses
  # don't flip sign; instead verify the obvious identity that the
  # cumulative IRF at horizon 1 equals the non-cumulative IRF.
  expect_equal(irf_cum[[1L]][1L, ], irf_gen[[1L]][1L, ])
})

test_that("orthogonalised IRF at horizon 1 equals P %*% e_j", {
  # For A = 0 and diagonal Sigma the correct OIRF at horizon 1
  # from shock j to response i is P[i, j], where P is the lower
  # Cholesky factor of Sigma. This is the Sims (1980) /
  # Lutkepohl (2007) definition and would not hold if the
  # Cholesky factor were applied twice on the right-hand side.
  K <- 2L
  A <- matrix(0, K, K)
  Sigma <- diag(c(1, 4))
  x <- list(K = K, A = A, Sigma = Sigma, p = 1L)
  irf_orth <- gen_irf(x, h = 1L, cumulative = FALSE, orthogonal = TRUE)
  P <- t(chol(Sigma))
  expect_equal(as.numeric(irf_orth[[1L]][1L, ]), as.numeric(P[, 1L]))
  expect_equal(as.numeric(irf_orth[[2L]][1L, ]), as.numeric(P[, 2L]))
})

test_that("var_fecov() builds a forecast-error covariance with positive diagonal", {
  K <- 2L
  A <- matrix(c(0.3, 0.05, 0.1, 0.2), K, K)
  Sigma <- diag(K) + 0.3
  x <- list(K = K, A = A, Sigma = Sigma, p = 1L)
  msey <- var_fecov(x, h = 4L)
  expect_identical(dim(msey), c(K, K, 4L))
  for (i in seq_len(4L)) {
    expect_true(all(diag(msey[, , i]) > 0))
  }
})
