# Unit tests for plot.mvgam_residcor + the shared diverging
# scale / matrix-melting / Robinson-ordering helpers in
# R/plot_helpers.R. Built on hand-rolled mvgam_residcor objects
# so no Stan fit or residual_cor() call is needed.

.make_residcor <- function(cor_mat, prob = NULL) {
  # The panel reads `cor` for fill and `prob_nonzero` for opacity,
  # so a stub needs both. Where no evidence is supplied, spread it
  # across the three bins so every level is exercised.
  if (is.null(prob)) {
    prob <- matrix(
      seq(0.5, 1, length.out = length(cor_mat)),
      nrow = nrow(cor_mat), dimnames = dimnames(cor_mat)
    )
  }
  structure(
    list(
      cor = cor_mat,
      sig_cor = cor_mat,
      prob_nonzero = prob,
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
  # setequal, not sorted equality: R CMD check runs under LC_COLLATE=C
  # where uppercase sorts before lowercase, so a sorted comparison is
  # locale-dependent.
  expect_setequal(colnames(out), c("value", "Var1", "Var2"))
  expect_equal(nrow(out), 9L)
  na_rows <- out[is.na(out$value), ]
  diag_rows <- na_rows[as.character(na_rows$Var1) ==
                         as.character(na_rows$Var2), ]
  expect_equal(nrow(diag_rows), 3L)
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
  reordered <- scrambled[idx, idx]
  # The recovered ordering should bring large-correlation
  # neighbours back into adjacency: at least one of the abs(
  # diagonal-adjacent) entries should exceed 0.7.
  adj <- abs(c(reordered[1, 2], reordered[2, 3], reordered[3, 4]))
  expect_gt(max(adj), 0.7)
})

test_that("cluster_cormat short-circuits for n <= 2", {
  expect_equal(
    mvgam:::cluster_cormat(matrix(1, nrow = 1L)),
    1L
  )
  expect_equal(
    mvgam:::cluster_cormat(matrix(c(1, 0.5, 0.5, 1), nrow = 2L)),
    1:2
  )
})

test_that("mvgam_diverging_scale exposes na.value", {
  s_default <- mvgam:::mvgam_diverging_scale()
  s_white <- mvgam:::mvgam_diverging_scale(na.value = "white")
  expect_true(inherits(s_default, "ScaleContinuous"))
  expect_equal(s_default$na.value, "transparent")
  expect_equal(s_white$na.value, "white")
})

test_that("plot.mvgam_residcor returns a ggplot for type = correlation", {
  cor_mat <- matrix(c(
    1.0, 0.6, 0.3, 0.0,
    0.6, 1.0, 0.5, 0.1,
    0.3, 0.5, 1.0, 0.4,
    0.0, 0.1, 0.4, 1.0
  ), nrow = 4L, byrow = TRUE,
     dimnames = list(letters[1:4], letters[1:4]))
  rc <- .make_residcor(cor_mat)
  p <- plot(rc)
  expect_ggplot(p)
  has_tile <- any(vapply(
    p$layers,
    function(l) inherits(l$geom, "GeomTile"),
    logical(1L)
  ))
  expect_true(has_tile)
})

test_that("type = 'precision' errors when the partial surface is absent", {
  cor_mat <- diag(3L)
  rc <- .make_residcor(cor_mat)
  expect_error(
    plot(rc, type = "precision"),
    "precision matrix"
  )
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
  rc$prec_prob_nonzero <- matrix(
    seq(0.5, 1, length.out = 9L), nrow = 3L,
    dimnames = dimnames(rc$prec)
  )
  p <- plot(rc, type = "precision")
  expect_ggplot(p)
})


test_that("the panel draws the estimate, not the thresholded matrix", {
  # `sig_cor` zeroes any pair whose evidence misses 0.95, so drawing
  # it made a correlation of 0.4 white and a whole jsdgam panel
  # blank. Fill carries the estimate; opacity carries the evidence.
  cor_mat <- matrix(c(
    1.0, 0.40, 0.18,
    0.40, 1.0, 0.58,
    0.18, 0.58, 1.0
  ), nrow = 3L, byrow = TRUE,
     dimnames = list(letters[1:3], letters[1:3]))
  prob <- matrix(c(
    1.00, 0.84, 0.68,
    0.84, 1.00, 0.98,
    0.68, 0.98, 1.00
  ), nrow = 3L, byrow = TRUE, dimnames = dimnames(cor_mat))
  rc <- .make_residcor(cor_mat, prob = prob)
  # Only one of the three pairs would survive a hard cut at 0.95.
  rc$sig_cor <- cor_mat
  rc$sig_cor[prob <= 0.95] <- 0
  expect_equal(sum(rc$sig_cor[lower.tri(rc$sig_cor)] != 0), 1L)

  p <- plot(rc)
  # Only the lower triangle is drawn, and every estimate survives.
  expect_equal(nrow(p$data), 3L)
  expect_setequal(round(p$data$value, 2), c(0.40, 0.18, 0.58))

  drawn <- ggplot2::layer_data(p, 1L)
  expect_equal(nrow(drawn), 3L)
  # Nothing is erased: each cell keeps a visible opacity.
  expect_true(all(drawn$alpha > 0))
  # And opacity rises with the evidence rather than switching on it.
  expect_false(is.unsorted(drawn$alpha[order(p$data$ev)]))
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


test_that("every evidence level keeps a legend key", {
  # ggplot builds a key from the rows a layer holds, so on a fit
  # where nothing clears 0.75 the other two levels drew a label with
  # no swatch. The filler layer keeps all three present.
  cor_mat <- matrix(c(
    1.0, 0.1, 0.05,
    0.1, 1.0, 0.08,
    0.05, 0.08, 1.0
  ), nrow = 3L, byrow = TRUE,
     dimnames = list(letters[1:3], letters[1:3]))
  weak <- matrix(0.6, nrow = 3L, ncol = 3L,
                 dimnames = dimnames(cor_mat))
  rc <- .make_residcor(cor_mat, prob = weak)
  keys <- ggplot2::get_guide_data(plot(rc), "alpha")
  expect_equal(nrow(keys), 3L)
  expect_setequal(as.character(keys$.label),
                  mvgam:::residcor_evidence_levels())
  # And the keys grade, so the legend reads as ordered.
  expect_false(is.unsorted(keys$alpha))
})

test_that("cluster = TRUE on a scrambled fixture changes the panel", {
  natural <- matrix(c(
     1.0, 0.9, 0.3, 0.0,
     0.9, 1.0, 0.7, 0.2,
     0.3, 0.7, 1.0, 0.8,
     0.0, 0.2, 0.8, 1.0
  ), nrow = 4L, byrow = TRUE,
     dimnames = list(letters[1:4], letters[1:4]))
  perm <- c(3L, 1L, 4L, 2L)
  scrambled <- natural[perm, perm]
  rc <- .make_residcor(scrambled)
  p_no <- plot(rc, cluster = FALSE)
  p_yes <- plot(rc, cluster = TRUE)
  expect_false(identical(p_no$data, p_yes$data))
})
