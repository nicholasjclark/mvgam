# Checks several local files share. testthat sources this file before
# each local test file in this directory.


# A cached fit tests the program it was fitted with. A change to the
# generator leaves the cache holding the old program, and every
# identity checked against its draws then tests a model no user can
# obtain. The program is rebuilt from the fit's own call through the
# path `update()` takes, which needs no formula or data restated.
#
# @param fit A cached `mvgam` fit
expect_current_program <- function(fit) {
  rebuilt <- mvgam_dry_stancode(mvgam_update_call(fit, NULL, NULL, list()))
  testthat::expect_identical(mvgam_normalise_stancode(rebuilt),
                             mvgam_normalise_stancode(fit$stancode))
}


# Ground truth for the stationary distribution of an AR() trend,
# written from the definition.
#
# The companion state is `(y_t, ..., y_{t-p+1}[, eps_t])`, block i at
# rows `(i - 1) m + 1` to `i m`, with diagonal coefficients. A
# moving-average term adds the block holding `eps_t`, which enters
# `y_{t+1}` through `theta`. The stationary covariance is the Neumann
# sum `Omega = sum_k A^k Q A'^k`. It calls nothing from the package,
# and every assertion taking it as ground truth tests the package
# against the definition.
#
# @param Sigma Innovation covariance (m x m)
# @param phi List of coefficient vectors, one per lag from 1 to p
# @param theta Moving-average coefficient vector, or NULL
# @param n_terms Terms of the sum. At a spectral radius of 0.99 the
#   terms past 3000 total less than the precision a stored draw holds.
# @return The covariance of the companion state, side `(p + q) m`
neumann_omega <- function(Sigma, phi, theta = NULL, n_terms = 3000L) {
  m <- nrow(Sigma)
  p <- length(phi)
  n <- p + if (is.null(theta)) 0L else 1L
  d <- n * m
  blk <- function(i) ((i - 1L) * m + 1L):(i * m)
  A <- matrix(0, d, d)
  for (k in seq_len(p)) A[blk(1L), blk(k)] <- diag(phi[[k]], nrow = m)
  for (i in seq_len(p)[-1L]) A[blk(i), blk(i - 1L)] <- diag(m)
  Q <- matrix(0, d, d)
  Q[blk(1L), blk(1L)] <- Sigma
  if (!is.null(theta)) {
    A[blk(1L), blk(n)] <- diag(theta, nrow = m)
    Q[blk(n), blk(n)] <- Sigma
    Q[blk(1L), blk(n)] <- Sigma
    Q[blk(n), blk(1L)] <- Sigma
  }
  Omega <- matrix(0, d, d)
  Ak <- diag(d)
  for (k in 0:n_terms) {
    Omega <- Omega + Ak %*% Q %*% t(Ak)
    Ak <- Ak %*% A
  }
  (Omega + t(Omega)) / 2
}
