test_that("procrustes_rotate produces an orthogonal alignment", {
  # Build Z_a and rotate by a known orthogonal matrix to make Z_b.
  set.seed(7L)
  z_a <- matrix(rnorm(10L * 2L), nrow = 10L, ncol = 2L)
  theta <- pi / 4
  R_true <- matrix(c(cos(theta), -sin(theta),
                     sin(theta),  cos(theta)),
                   nrow = 2L, byrow = TRUE)
  z_b <- z_a %*% t(R_true)
  z_b_aligned <- mvgam:::procrustes_rotate(z_a, z_b)
  # The alignment should recover z_a (up to numerical tolerance).
  expect_equal(z_b_aligned, z_a, tolerance = 1e-10)
})

test_that("procrustes_rotate minimises Frobenius distance vs z_a", {
  set.seed(42L)
  z_a <- matrix(rnorm(20L), nrow = 10L, ncol = 2L)
  # Random non-orthogonal noise on top of a rotation; alignment
  # should bring z_b closer to z_a than the raw z_b.
  theta <- pi / 3
  R <- matrix(c(cos(theta), -sin(theta),
                sin(theta),  cos(theta)),
              nrow = 2L, byrow = TRUE)
  z_b <- z_a %*% t(R) + matrix(rnorm(20L, sd = 0.05), nrow = 10L)
  d_raw  <- sum((z_a - z_b)^2)
  d_alig <- sum((z_a - mvgam:::procrustes_rotate(z_a, z_b))^2)
  expect_true(d_alig < d_raw)
})

test_that("validate_loadings_alignment rejects mismatched species sets", {
  z_a <- matrix(0, nrow = 3L, ncol = 2L,
                dimnames = list(c("a", "b", "c"), NULL))
  z_b <- matrix(0, nrow = 3L, ncol = 2L,
                dimnames = list(c("a", "b", "X"), NULL))
  expect_error(
    mvgam:::validate_loadings_alignment(z_a, z_b, c(1L, 2L)),
    regexp = "Species do not align"
  )
})

test_that("validate_loadings_alignment rejects too few latent factors", {
  z_a <- matrix(0, nrow = 3L, ncol = 2L,
                dimnames = list(c("a", "b", "c"), NULL))
  z_b <- matrix(0, nrow = 3L, ncol = 2L,
                dimnames = list(c("a", "b", "c"), NULL))
  expect_error(
    mvgam:::validate_loadings_alignment(z_a, z_b, c(1L, 3L)),
    regexp = "at least"
  )
})

test_that("compare_loadings rejects non-mvgam input", {
  expect_error(
    compare_loadings(list(foo = 1L), list(bar = 2L)),
    regexp = "Must inherit"
  )
})
