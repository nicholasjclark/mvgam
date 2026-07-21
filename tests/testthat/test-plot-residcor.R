# Unit tests for plot.mvgam_residcor + the shared diverging
# scale / matrix-melting / Robinson-ordering helpers in
# R/plot_helpers.R. Built on hand-rolled mvgam_residcor objects
# so no Stan fit or residual_cor() call is needed.

.make_residcor <- function(cor_mat) {
  structure(
    list(
      cor = cor_mat,
      sig_cor = cor_mat,
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
  expect_equal(s_default$na.value, "grey30")
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

test_that("type = 'precision' errors when sig_prec is absent", {
  cor_mat <- diag(3L)
  rc <- .make_residcor(cor_mat)
  expect_error(
    plot(rc, type = "precision"),
    "precision matrix"
  )
})

test_that("type = 'precision' works when sig_prec is supplied", {
  cor_mat <- diag(3L)
  rc <- .make_residcor(cor_mat)
  rc$sig_prec <- matrix(c(
    0, 0.3, -0.1,
    0.3, 0, 0.2,
    -0.1, 0.2, 0
  ), nrow = 3L, byrow = TRUE,
     dimnames = list(letters[1:3], letters[1:3]))
  p <- plot(rc, type = "precision")
  expect_ggplot(p)
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
