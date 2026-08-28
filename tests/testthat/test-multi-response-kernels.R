# Log-density kernels for the five multi-response families.
#
# These are ordinary R functions taking a linear predictor, a response
# and a list of family parameters, so they can be checked against a
# reference density without sampling. Every fixture that fits one of
# these families only drives `residual_cor()` and `as.data.frame()` on
# the result, so nothing else exercised the densities themselves.
#
# Each reference is arrived at independently of the kernel: a
# two-category Dirichlet against a Beta, a multinomial against
# `stats::dmultinom`, a diagonal multivariate normal against
# `mvtnorm::dmvnorm`, a multivariate t against `mvtnorm::dmvt`.
# Restating the kernel's own arithmetic would test nothing.


# The closure-unit arrays these kernels index by: `N_unit` sites, each
# with `n_rep` rows, `visit_idx` giving the row numbers per site.
unit_arrays <- function(n_unit, k) {
  list(
    N_unit = n_unit,
    n_rep = rep(k, n_unit),
    visit_idx = matrix(seq_len(n_unit * k), nrow = n_unit, byrow = TRUE)
  )
}


test_that("log_lik_diri matches a Beta density when K = 2", {
  # Dirichlet(a1, a2) on the simplex has its first component
  # Beta(a1, a2) distributed, which is a reference the kernel's own
  # arithmetic plays no part in.
  ndraws <- 3L
  y <- c(0.3, 0.7)
  prob <- matrix(c(0.4, 0.6), nrow = ndraws, ncol = 2L, byrow = TRUE)
  phi <- matrix(10, nrow = ndraws, ncol = 2L)
  out <- log_lik_diri(
    linpred = matrix(0, ndraws, 2L), link = "identity", y = y,
    family_pars = list(prob_row = prob, phi = phi,
                        arrays = unit_arrays(1L, 2L)),
    trials = NULL
  )
  expected <- stats::dbeta(y[1L], 0.4 * 10, 0.6 * 10, log = TRUE)
  # The unit's density is attributed to its first row.
  expect_equal(out[, 1L], rep(expected, ndraws))
  expect_true(all(out[, 2L] == 0))
})


test_that("log_lik_diri matches a three-category Dirichlet", {
  skip_if_not_installed("extraDistr")
  ndraws <- 2L
  y <- c(0.2, 0.5, 0.3)
  p <- c(0.25, 0.45, 0.30)
  phi_val <- 8
  prob <- matrix(p, nrow = ndraws, ncol = 3L, byrow = TRUE)
  phi <- matrix(phi_val, nrow = ndraws, ncol = 3L)
  out <- log_lik_diri(
    linpred = matrix(0, ndraws, 3L), link = "identity", y = y,
    family_pars = list(prob_row = prob, phi = phi,
                        arrays = unit_arrays(1L, 3L)),
    trials = NULL
  )
  expected <- extraDistr::ddirichlet(matrix(y, nrow = 1L),
                                      alpha = p * phi_val, log = TRUE)
  expect_equal(out[1L, 1L], as.numeric(expected))
})


test_that("log_lik_multi matches stats::dmultinom", {
  ndraws <- 2L
  y <- c(3, 5, 2)
  p <- c(0.2, 0.5, 0.3)
  prob <- matrix(p, nrow = ndraws, ncol = 3L, byrow = TRUE)
  out <- log_lik_multi(
    linpred = matrix(0, ndraws, 3L), link = "identity", y = y,
    family_pars = list(prob_row = prob, arrays = unit_arrays(1L, 3L)),
    trials = NULL
  )
  expected <- stats::dmultinom(y, size = sum(y), prob = p, log = TRUE)
  expect_equal(out[1L, 1L], expected)
})


test_that("log_lik_categ is the log probability of the chosen category", {
  ndraws <- 2L
  p <- c(0.2, 0.5, 0.3)
  prob <- matrix(p, nrow = ndraws, ncol = 3L, byrow = TRUE)
  for (chosen in 1:3) {
    y <- rep(0L, 3L)
    y[chosen] <- 1L
    out <- log_lik_categ(
      linpred = matrix(0, ndraws, 3L), link = "identity", y = y,
      family_pars = list(prob_row = prob, arrays = unit_arrays(1L, 3L)),
      trials = NULL
    )
    # A one-hot draw is a multinomial of size one, so the two agree.
    expect_equal(out[1L, 1L],
                 stats::dmultinom(y, size = 1L, prob = p, log = TRUE))
    expect_equal(out[1L, 1L], log(p[chosen]))
  }
})


test_that("log_lik_categ refuses an observation that is not one-hot", {
  prob <- matrix(c(0.2, 0.5, 0.3), nrow = 2L, ncol = 3L, byrow = TRUE)
  pars <- list(prob_row = prob, arrays = unit_arrays(1L, 3L))
  expect_error(
    log_lik_categ(matrix(0, 2L, 3L), "identity", c(1L, 1L, 0L),
                  pars, NULL),
    "single one-hot"
  )
  expect_error(
    log_lik_categ(matrix(0, 2L, 3L), "identity", c(0L, 0L, 0L),
                  pars, NULL),
    "single one-hot"
  )
})


test_that("log_lik_mvn sums to a diagonal multivariate normal", {
  skip_if_not_installed("mvtnorm")
  # The kernel returns one normal log-density per row. Summed over a
  # unit those must equal the joint density under a diagonal Sigma,
  # which is what `Sigma = diag(Psi^2)` claims.
  ndraws <- 2L
  y <- c(1.2, -0.4, 0.8)
  mu <- c(1.0, 0.0, 0.5)
  psi <- c(0.7, 1.3, 0.9)
  linpred <- matrix(mu, nrow = ndraws, ncol = 3L, byrow = TRUE)
  Psi_row <- matrix(psi, nrow = ndraws, ncol = 3L, byrow = TRUE)
  out <- log_lik_mvn(linpred, "identity", y,
                      list(Psi_row = Psi_row), NULL)
  expected <- mvtnorm::dmvnorm(y, mean = mu, sigma = diag(psi^2),
                                log = TRUE)
  expect_equal(sum(out[1L, ]), expected)
  expect_equal(dim(out), c(ndraws, 3L))
})


test_that("log_lik_mvt matches a scaled univariate t per element", {
  skip_if_not_installed("mvtnorm")
  # The kernel scales by Psi and corrects with the Jacobian, so each
  # element is a location-scale t. A multivariate t with diagonal
  # scale is not the product of those, so the per-element form is what
  # is checked, against `mvtnorm::dmvt` in one dimension.
  ndraws <- 2L
  y <- c(1.2, -0.4)
  mu <- c(1.0, 0.0)
  psi <- c(0.7, 1.3)
  nu <- c(5, 5)
  out <- log_lik_mvt(
    linpred = matrix(mu, nrow = ndraws, ncol = 2L, byrow = TRUE),
    link = "identity", y = y,
    family_pars = list(
      Psi_row = matrix(psi, nrow = ndraws, ncol = 2L, byrow = TRUE),
      nu = nu
    ),
    trials = NULL
  )
  for (k in seq_along(y)) {
    expected <- mvtnorm::dmvt(y[k], delta = mu[k],
                               sigma = matrix(psi[k]^2, 1, 1),
                               df = nu[1L], log = TRUE)
    expect_equal(out[1L, k], as.numeric(expected))
  }
})


test_that("the multi-response kernels return one row per draw", {
  # Shape contract shared by all five: `[ndraws x N_obs]`, with a
  # unit's density on its first row and zeros on the rest.
  ndraws <- 4L
  arrays <- unit_arrays(2L, 2L)
  prob <- matrix(0.5, nrow = ndraws, ncol = 4L)
  phi <- matrix(6, nrow = ndraws, ncol = 4L)
  lin <- matrix(0, nrow = ndraws, ncol = 4L)

  d <- log_lik_diri(lin, "identity", c(0.4, 0.6, 0.3, 0.7),
                     list(prob_row = prob, phi = phi, arrays = arrays),
                     NULL)
  expect_equal(dim(d), c(ndraws, 4L))
  expect_true(all(d[, c(2L, 4L)] == 0))
  expect_true(all(is.finite(d[, c(1L, 3L)])))

  m <- log_lik_multi(lin, "identity", c(2, 3, 1, 4),
                      list(prob_row = prob, arrays = arrays), NULL)
  expect_equal(dim(m), c(ndraws, 4L))
  expect_true(all(m[, c(2L, 4L)] == 0))
})
