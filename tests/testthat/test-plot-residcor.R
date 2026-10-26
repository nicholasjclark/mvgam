# `plot.mvgam_residcor()` and its heatmap helpers, on hand-built
# `mvgam_residcor` objects.

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

test_that("matrix_panel keeps every cell or the lower triangle", {
  m <- matrix(seq_len(9L), nrow = 3L,
              dimnames = list(letters[1:3], letters[1:3]))
  expect_identical(nrow(mvgam:::matrix_panel(m)), 9L)
  # Naming the kept cells checks that the diagonal and the upper
  # triangle both go
  kept <- mvgam:::matrix_panel(m, lower_only = TRUE)
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
  # The panel draws the partial correlations. The correlations are all 0.
  expect_setequal(p$data$value, c(0.3, -0.1, 0.2))
})


test_that("the panel draws the estimate, not the thresholded matrix", {
  # `sig_cor` zeroes a pair whose evidence misses 0.95. The fill
  # carries the estimate of every pair.
  cor_mat <- matrix(c(
    1.0, 0.40, 0.18,
    0.40, 1.0, 0.58,
    0.18, 0.58, 1.0
  ), nrow = 3L, byrow = TRUE,
     dimnames = list(letters[1:3], letters[1:3]))
  rc <- .make_residcor(cor_mat)
  rc$sig_cor[] <- 0
  # Only the pair c-b has an interval clear of zero
  rc$cor_lower <- cor_mat - 0.5
  rc$cor_upper <- cor_mat + 0.5
  rc$cor_lower["c", "b"] <- 0.1
  p <- plot(rc)
  # The panel holds the lower triangle with every estimate in it
  expect_equal(nrow(p$data), 3L)
  expect_setequal(round(p$data$value, 2), c(0.40, 0.18, 0.58))
  outlined <- p$layers[[2L]]$data
  expect_identical(as.character(c(outlined$Var1, outlined$Var2)),
                   c("c", "b"))
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
  # The default limits are those of a correlation. Panels from
  # different fits are then comparable.
  expect_equal(fill_limits(plot(rc)), c(-1, 1))
  rescaled <- fill_limits(plot(rc, rescale = TRUE))
  expect_equal(rescaled, c(-0.3, 0.3))
})
