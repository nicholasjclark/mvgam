
test_that("matrix_panel keeps every cell or the lower triangle", {
  m <- matrix(seq_len(9L), nrow = 3L,
              dimnames = list(letters[1:3], letters[1:3]))
  expect_identical(nrow(mvgam:::matrix_panel(m)), 9L)
  # Naming the kept cells checks that the diagonal and the upper
  # triangle both go
  kept <- mvgam:::matrix_panel(m, lower_only = TRUE)
  expect_identical(paste0(kept$Var1, kept$Var2), c("ba", "ca", "cb"))
  expect_identical(kept$value, c(2L, 3L, 6L))
  # The triangle's axes hold no empty row or column
  expect_identical(levels(kept$Var1), c("b", "c"))
  expect_identical(levels(kept$Var2), c("a", "b"))
})

test_that("cluster_cormat reorders a scrambled chain", {
  natural <- matrix(c(
     1.0, 0.9, 0.3, 0.0,
     0.9, 1.0, 0.7, 0.2,
     0.3, 0.7, 1.0, 0.8,
     0.0, 0.2, 0.8, 1.0
  ), nrow = 4L, byrow = TRUE)
  perm <- c(3L, 1L, 4L, 2L)
  scrambled <- natural[perm, perm]
  idx <- mvgam:::cluster_cormat(scrambled)
  # The chain comes back exactly
  expect_identical(perm[idx], 1:4)
  expect_equal(scrambled[idx, idx], natural)
})

test_that("type = 'precision' errors when the partial surface is absent", {
  cor_mat <- diag(3L)
  rc <- .make_residcor(cor_mat)
  # The message names the argument of `residual_cor()` that adds them
  expect_error(
    plot(rc, type = "precision"),
    "partial = TRUE",
    fixed = TRUE
  )
})
