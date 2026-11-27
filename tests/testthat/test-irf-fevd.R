
# ---- Gate ----------------------------------------------------------

test_that("assert_var_trend() rejects non-VAR fits with a clear pointer", {
  fake <- structure(
    list(trend_metadata = list(trend_type = "RW")),
    class = "mvgam"
  )
  expect_error(
    assert_var_trend(fake, surface = "irf()"),
    "requires a VAR latent trend"
  )
  expect_error(
    assert_var_trend(fake, surface = "irf()"),
    "trend type is 'RW'"
  )
  expect_true(assert_var_trend(build_var_mock(K = 2L, ndraws = 2L),
                               surface = "irf()"))
})

# ---- Extractor -----------------------------------------------------

test_that("extract_var_posterior() reads the correct column for each (i, j)", {
  # Stamp known values into Phi_trend[1,2,3] and Sigma_trend[1,1] so we
  # can verify the indexing convention round-trips correctly.
  mock <- build_var_mock(K = 3L, ndraws = 4L)
  mock$fit[1L, "Phi_trend[1,2,3]"] <- 0.7
  mock$fit[2L, "Sigma_trend[1,1]"] <- 1.25
  vp <- extract_var_posterior(mock)
  expect_equal(vp$A[1L, 2L, 3L, 1L], 0.7)
  expect_equal(vp$Sigma[2L, 1L, 1L], 1.25)
})

test_that(
  "extract_var_posterior() errors on a missing column", {
  mock <- build_var_mock(K = 2L, ndraws = 3L)
  drop_col <- "Phi_trend[1,1,1]"
  mock$fit <- mock$fit[, setdiff(colnames(mock$fit), drop_col), drop = FALSE]
  # The refusal names the cell the posterior lacks.
  expect_error(
    extract_var_posterior(mock),
    "Missing: Phi_trend\\[1,1,1\\]"
  )
})

# ---- Math helpers --------------------------------------------------

test_that("var_phi() recovers A^k for diagonal A", {
  K <- 2L
  A <- diag(c(0.5, 0.3))
  x <- var_kernel_input(K, A)
  Phi <- var_phi(x, h = 3L)
  # Phi[, , k+1] = A^k for the i.i.d. case with diagonal A.
  expect_equal(Phi[, , 1L], diag(K))
  expect_equal(Phi[, , 2L], A)
  expect_equal(Phi[, , 3L], A %*% A)
  expect_equal(Phi[, , 4L], A %*% A %*% A)
})

test_that("var_phi() matches a VAR(2) companion and a VARMA impulse", {
  K <- 2L
  set.seed(2L)
  A <- array(stats::rnorm(K * K * 2L, sd = 0.25), c(K, K, 2L))
  D <- array(stats::rnorm(K * K, sd = 0.3), c(K, K, 1L))
  # A VAR(2)'s weights are the top-left block of the companion powers.
  comp <- rbind(cbind(A[, , 1L], A[, , 2L]), cbind(diag(K), 0 * diag(K)))
  Phi <- var_phi(var_kernel_input(K, A), h = 3L)
  expect_equal(Phi[, , 4L], (comp %*% comp %*% comp)[1:K, 1:K])
  # A VARMA(2, 1)'s first weight adds the moving-average matrix.
  Phi_ma <- var_phi(var_kernel_input(K, A, D = D), h = 2L)
  expect_equal(Phi_ma[, , 2L], A[, , 1L] + D[, , 1L])
  expect_equal(Phi_ma[, , 3L],
               A[, , 1L] %*% Phi_ma[, , 2L] + A[, , 2L])
})

test_that("gen_fevd() rows sum to 1 across the columns for each horizon", {
  K <- 3L
  set.seed(7L)
  # Random stable A by shrinking eigenvalues into the unit circle.
  A_raw <- matrix(stats::rnorm(K * K, sd = 0.4), K, K)
  ev <- max(abs(eigen(A_raw)$values))
  A <- A_raw / (1.2 * ev)
  Sigma <- crossprod(matrix(stats::rnorm(K * K), K, K)) + diag(K)
  x <- var_kernel_input(K, A, Sigma)
  fevd <- gen_fevd(x, h = 5L)
  for (i in seq_len(K)) {
    # For each "responding" series i, the contributions across all K
    # shock origins should sum to 1 at every horizon.
    row_sums <- rowSums(fevd[[i]])
    expect_equal(row_sums, rep(1.0, 5L))
  }
  # At the first horizon a response's shares are the squared row of
  # the lower Cholesky factor of Sigma, normalised
  P <- t(chol(Sigma))
  for (i in seq_len(K)) {
    expect_equal(unname(fevd[[i]][1L, ]), P[i, ]^2 / sum(P[i, ]^2))
  }
})

test_that("the generalised IRF is the scaled column of Sigma, propagated", {
  # Pesaran and Shin (1998): a shock to j moves the system by
  # `Sigma[, j] / sqrt(Sigma[j, j])` and then by A times that. Each
  # list element is one shock, with horizons in rows and responses in
  # columns.
  K <- 2L
  A <- matrix(c(0.4, 0.1, 0.05, 0.3), K, K)
  Sigma <- matrix(c(1, 0.4, 0.4, 2), K, K)
  x <- var_kernel_input(K, A, Sigma)
  irf_gen <- gen_irf(x, h = 3L, cumulative = FALSE, orthogonal = FALSE)
  for (j in seq_len(K)) {
    impact <- Sigma[, j] / sqrt(Sigma[j, j])
    expect_equal(unname(irf_gen[[j]][1L, ]), impact)
    expect_equal(unname(irf_gen[[j]][2L, ]), as.numeric(A %*% impact))
  }
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
  x <- var_kernel_input(K, A, Sigma)
  irf_orth <- gen_irf(x, h = 1L, cumulative = FALSE, orthogonal = TRUE)
  P <- t(chol(Sigma))
  expect_equal(as.numeric(irf_orth[[1L]][1L, ]), as.numeric(P[, 1L]))
  expect_equal(as.numeric(irf_orth[[2L]][1L, ]), as.numeric(P[, 2L]))
})

test_that("var_fecov() accumulates the propagated innovation covariance", {
  K <- 2L
  A <- matrix(c(0.3, 0.05, 0.1, 0.2), K, K)
  Sigma <- diag(K) + 0.3
  msey <- var_fecov(var_kernel_input(K, A, Sigma), h = 3L)
  expect_equal(msey[, , 1L], Sigma)
  expect_equal(msey[, , 2L], Sigma + A %*% Sigma %*% t(A))
})


test_that("posterior_transition_matrix() takes one groups argument", {
  # A non-hierarchical fit refuses `groups`
  mock <- correlate_draws(build_var_mock(K = 3L, ndraws = 200L))
  expect_error(
    posterior_transition_matrix(mock, groups = "all"),
    "only applies to hierarchical VAR fits"
  )
})


# ---- Process labels -------------------------------------------------

test_that("var_process_labels names the series where they are the processes", {
  # A VAR over the observed series names its processes by the series
  # axis, and a reader interprets a shock without the internal order.
  mock <- build_var_mock(K = 3L, ndraws = 5L)
  expect_identical(var_process_labels(mock, 3L),
                   c("Process_1", "Process_2", "Process_3"))

  named <- mock
  named$trend_metadata$axes <- list(
    series = list(levels = c("willow", "ash", "rowan"), n = 3L)
  )
  expect_identical(var_process_labels(named, 3L),
                   c("willow", "ash", "rowan"))

  # A series count disagreeing with the VAR dimension is no labelling
  # of it, and the index answers in place of a recycled name.
  short <- named
  short$trend_metadata$axes$series$levels <- c("willow", "ash")
  expect_identical(var_process_labels(short, 3L),
                   c("Process_1", "Process_2", "Process_3"))
})


test_that("the VAR posterior carries one label per process", {
  mock <- build_var_mock(K = 3L, ndraws = 5L)
  vp <- extract_var_posterior(mock)
  # The mock carries no series names, and the labels take the index
  # form the test above pins for var_process_labels().
  expect_identical(vp$labels, c("Process_1", "Process_2", "Process_3"))
  # Both kernels read the labels off the posterior, so a surface
  # cannot spell them a second way.
  irf_gen <- gen_irf(
    var_kernel_input(3L, matrix(0.1, 3L, 3L), labels = vp$labels), h = 2L
  )
  expect_identical(names(irf_gen), vp$labels)
  fevd_gen <- gen_fevd(
    var_kernel_input(3L, matrix(0.1, 3L, 3L), labels = vp$labels), h = 2L
  )
  expect_identical(names(fevd_gen), vp$labels)
})


test_that("the stability metrics match their closed forms", {
  B <- matrix(c(0.5, 0.2, 0, -0.1, 0.4, 0.1, 0.05, 0, 0.3), 3L)
  Sigma <- matrix(c(1, 0.3, 0.1, 0.3, 0.8, 0.2, 0.1, 0.2, 0.5), 3L)
  # The doubling solver agrees with the Kronecker solve of
  # vec(X) = (I - B %x% B)^-1 vec(Sigma)
  kron <- matrix(solve(diag(9L) - B %x% B, c(Sigma)), 3L)
  expect_equal(solve_dlyap(B, Sigma), kron, tolerance = 1e-10)
  m <- stability_metrics(B, Sigma)
  expect_equal(m$prop_int, det(B)^2)
  expect_equal(m$reactivity, log(max(svd(B)$d)))
  expect_equal(m$var_return_rate,
               max(Mod(eigen(B %x% B, only.values = TRUE)$values)))
})
