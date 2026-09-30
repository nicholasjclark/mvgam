# Unit tests for plot.mvgam_residcor + the shared diverging
# scale / matrix-melting / Robinson-ordering helpers in
# R/plot_helpers.R. Built on hand-rolled mvgam_residcor objects
# so no Stan fit or residual_cor() call is needed.

.make_residcor <- function(cor_mat) {
  structure(
    list(
      cor = cor_mat,
      sig_cor = cor_mat,
      prob_threshold = 0.95,
      series_names = colnames(cor_mat) %||%
        paste0("s", seq_len(nrow(cor_mat))),
      n_series = nrow(cor_mat)
    ),
    class = "mvgam_residcor"
  )
}

test_that("gather_matrix masks upper triangle and diagonal", {
  m <- matrix(seq_len(9L), nrow = 3L,
              dimnames = list(letters[1:3], letters[1:3]))
  out <- mvgam:::gather_matrix(m)
  expect_identical(colnames(out), c("Var1", "Var2", "value"))
  expect_identical(nrow(out), 9L)
  # Counting the masked diagonal left the other half of the name
  # untested. A helper masking the diagonal alone keeps six values
  # and passed. The cells that survive are the lower triangle, and
  # naming them covers both halves at once.
  kept <- out[!is.na(out$value), ]
  expect_identical(paste0(kept$Var1, kept$Var2), c("ba", "ca", "cb"))
  expect_identical(kept$value, c(2L, 3L, 6L))
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
  # The chain comes back exactly. Requiring one adjacent pair to
  # clear 0.7 asserted something the scrambled matrix already
  # managed.
  expect_identical(perm[idx], 1:4)
  expect_equal(scrambled[idx, idx], natural)
})

test_that("type = 'precision' errors when the partial surface is absent", {
  cor_mat <- diag(3L)
  rc <- .make_residcor(cor_mat)
  # The message has to name the argument that fixes it. It named
  # `compute_precision`, which `residual_cor()` does not take.
  expect_error(
    plot(rc, type = "precision"),
    "partial = TRUE",
    fixed = TRUE
  )
})

test_that("type = 'precision' works when the partial surface is present", {
  cor_mat <- diag(3L)
  rc <- .make_residcor(cor_mat)
  rc$prec <- matrix(c(
    1, 0.3, -0.1,
    0.3, 1, 0.2,
    -0.1, 0.2, 1
  ), nrow = 3L, byrow = TRUE,
     dimnames = list(letters[1:3], letters[1:3]))
  p <- plot(rc, type = "precision")
  expect_ggplot(p)
})


test_that("the panel draws the estimate, not the thresholded matrix", {
  # `sig_cor` zeroes any pair whose evidence misses 0.95, so drawing
  # it made a correlation of 0.4 white and a whole jsdgam panel
  # blank. Fill carries the estimate of every pair.
  cor_mat <- matrix(c(
    1.0, 0.40, 0.18,
    0.40, 1.0, 0.58,
    0.18, 0.58, 1.0
  ), nrow = 3L, byrow = TRUE,
     dimnames = list(letters[1:3], letters[1:3]))
  rc <- .make_residcor(cor_mat)
  rc$sig_cor[] <- 0
  p <- plot(rc)
  # Only the lower triangle is drawn, and every estimate survives.
  expect_equal(nrow(p$data), 3L)
  expect_setequal(round(p$data$value, 2), c(0.40, 0.18, 0.58))
})


test_that("rescale switches the colour limits", {
  cor_mat <- matrix(c(
    1.0, 0.3, 0.2,
    0.3, 1.0, 0.1,
    0.2, 0.1, 1.0
  ), nrow = 3L, byrow = TRUE,
     dimnames = list(letters[1:3], letters[1:3]))
  rc <- .make_residcor(cor_mat)
  fill_limits <- function(p) {
    ggplot2::ggplot_build(p)$plot$scales$get_scales("fill")$limits
  }
  # A correlation runs on [-1, 1], so that is the honest default and
  # keeps panels from different fits comparable.
  expect_equal(fill_limits(plot(rc)), c(-1, 1))
  rescaled <- fill_limits(plot(rc, rescale = TRUE))
  expect_equal(rescaled, c(-0.3, 0.3))
})


