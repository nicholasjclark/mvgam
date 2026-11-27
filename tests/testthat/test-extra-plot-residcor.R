# The tests of test-plot-residcor.R that assemble a model or run for
# 0.2 s or more. CRAN skips this file. CI and local runs take it.
skip_on_cran()

test_that("type = 'precision' works when the partial surface is present", {
  cor_mat <- diag(3L)
  rc <- .make_residcor(cor_mat)
  rc$prec <- matrix(c(
    1, 0.3, -0.1,
    0.3, 1, 0.2,
    -0.1, 0.2, 1
  ), nrow = 3L, byrow = TRUE,
     dimnames = list(letters[1:3], letters[1:3]))
  rc$prec_prob_nonzero <- rc$prec * 0 + 0.6
  p <- plot(rc, type = "precision")
  # The panel draws the partial correlations. The correlations are all 0.
  expect_setequal(p$data$value, c(0.3, -0.1, 0.2))
})


test_that("the panel outlines the pairs whose sign is supported", {
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
  # Only the pair c-b has prob_nonzero above 0.95
  rc$prob_nonzero["c", "b"] <- 0.99
  p <- plot(rc)
  # The panel holds the lower triangle with every estimate in it
  expect_equal(nrow(p$data), 3L)
  expect_setequal(round(p$data$value, 2), c(0.40, 0.18, 0.58))
  outlined <- p$layers[[2L]]$data
  expect_identical(as.character(c(outlined$Var1, outlined$Var2)),
                   c("c", "b"))
  # The pairs below the threshold are left blank on request
  expect_identical(is.na(plot(rc, supported_only = TRUE)$data$value),
                   c(TRUE, TRUE, FALSE))
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
