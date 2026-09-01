test_that("plot.mvgam_active_factors returns a ggplot", {
  af <- structure(
    list(
      count = list(median = 4, q025 = 3, q975 = 6),
      per_factor = data.frame(
        factor          = 1:5,
        prob_active     = c(1, 1, 0.95, 0.6, 0.1),
        median_norm_sq  = c(10, 8, 6, 2, 0.1),
        is_active       = c(TRUE, TRUE, TRUE, TRUE, FALSE)
      ),
      threshold = list(
        fraction = 0.01, prob_threshold = 0.5, epsilon = 0.1
      ),
      n_lv = 5L
    ),
    class = "mvgam_active_factors"
  )
  p <- plot(af)
  expect_s3_class(p, "ggplot")
  # Title carries the n_lv ceiling so the reader sees the truncation.
  expect_match(p$labels$title, "n_lv ceiling = 5")
  # Subtitle reports the median + CI from the count slot.
  expect_match(p$labels$subtitle, "4\\.0")
  expect_match(p$labels$subtitle, "3\\.0")
  expect_match(p$labels$subtitle, "6\\.0")
})

test_that("plot.mvgam_active_factors threshold line tracks prob_threshold", {
  af <- structure(
    list(
      count = list(median = 1, q025 = 1, q975 = 2),
      per_factor = data.frame(
        factor = 1:2, prob_active = c(1, 0.3),
        median_norm_sq = c(5, 0.2), is_active = c(TRUE, FALSE)
      ),
      threshold = list(
        fraction = 0.01, prob_threshold = 0.8, epsilon = 0.05
      ),
      n_lv = 2L
    ),
    class = "mvgam_active_factors"
  )
  p <- plot(af)
  # geom_hline uses yintercept = 1 - prob_threshold so the bar must
  # clear that line to count as active.
  hline_layers <- vapply(
    p$layers,
    function(l) inherits(l$geom, "GeomHline"),
    logical(1L)
  )
  hline_y <- p$layers[[which(hline_layers)]]$data$yintercept
  expect_equal(hline_y, 1 - 0.8)
})

test_that("plot.mvgam_active_factors rejects non-class input", {
  expect_error(
    plot.mvgam_active_factors(list(foo = "bar")),
    regexp = "Must inherit"
  )
})


test_that("a column's activity is measured with its own scale", {
  # A factor column contributes `sigma_trend[k] * Z[, k]`. Reading
  # `Z` alone was right while `Z` carried the column magnitude, and
  # wrong once multiplicative gamma process shrinkage moved that
  # magnitude into `sigma_trend` and left `Z` at unit scale: every
  # column's norm then has the same distribution, so a shrunk
  # column reports as active.
  ndraws <- 40L
  draws <- posterior::as_draws_matrix(
    matrix(
      c(rep(2, ndraws), rep(0.01, ndraws)),
      nrow = ndraws,
      dimnames = list(NULL, c("sigma_trend[1]", "sigma_trend[2]"))
    )
  )
  scales <- resolve_column_scales(NULL, draws, n_lv = 2L)
  expect_equal(dim(scales), c(ndraws, 2L))
  expect_equal(unname(scales[1L, ]), c(2, 0.01))

  # A fit with no per-column scale multiplies by one, so every
  # other route is left exactly as it was.
  bare <- posterior::as_draws_matrix(
    matrix(1, nrow = ndraws, ncol = 1L,
           dimnames = list(NULL, "lp__"))
  )
  expect_equal(
    resolve_column_scales(NULL, bare, n_lv = 3L),
    matrix(1, nrow = ndraws, ncol = 3L)
  )

  # Equal raw norms, unequal scales: the second column is not active.
  Z <- array(1, dim = c(ndraws, 4L, 2L))
  raw <- apply(Z, c(1L, 3L), function(col) sum(col^2))
  expect_equal(raw[1L, 1L], raw[1L, 2L])
  scaled <- raw * scales^2
  expect_gt(scaled[1L, 1L], scaled[1L, 2L] * 1000)
})
