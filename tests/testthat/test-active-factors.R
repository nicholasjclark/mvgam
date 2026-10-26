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
        criterion = "variance", value = 0.05, prob_threshold = 0.5
      ),
      n_lv = 5L
    ),
    class = "mvgam_active_factors"
  )
  p <- plot(af)
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
        criterion = "variance", value = 0.05, prob_threshold = 0.8
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
  # A factor column contributes `sigma_trend[k] * Z[, k]`.
  # Multiplicative gamma process shrinkage acts on `sigma_trend` and
  # draws `Z` at unit scale. A shrunk column differs only in its
  # scale.
  ndraws <- 40L
  draws <- posterior::as_draws_matrix(
    matrix(
      c(rep(2, ndraws), rep(0.01, ndraws)),
      nrow = ndraws,
      dimnames = list(NULL, c("sigma_trend[1]", "sigma_trend[2]"))
    )
  )
  scales <- resolve_column_scales(NULL, draws, n_lv = 2L)
  expect_equal(unname(scales[1L, ]), c(2, 0.01))

  # A fit with no per-column scale multiplies by one
  bare <- posterior::as_draws_matrix(
    matrix(1, nrow = ndraws, ncol = 1L,
           dimnames = list(NULL, "lp__"))
  )
  expect_equal(
    resolve_column_scales(NULL, bare, n_lv = 3L),
    matrix(1, nrow = ndraws, ncol = 3L)
  )

})


test_that("each activity rule counts the columns it describes", {
  # Shares 0.6, 0.3, 0.07 and 0.03 in every draw
  norm_sq <- matrix(c(6, 3, 0.7, 0.3), nrow = 2L, ncol = 4L, byrow = TRUE)
  active <- function(...) active_factor_draws(norm_sq, ...)[1L, ]
  expect_identical(active("variance", 0.05), c(TRUE, TRUE, TRUE, FALSE))
  expect_identical(active("cumulative", 0.95), c(TRUE, TRUE, TRUE, FALSE))
  expect_identical(active("cumulative", 0.9), c(TRUE, TRUE, FALSE, FALSE))
  expect_identical(active("relative", 0.1), c(TRUE, TRUE, TRUE, FALSE))
  # The ranking is within each draw, whatever the column order
  expect_identical(
    active_factor_draws(norm_sq[, 4:1, drop = FALSE], "cumulative",
                        0.9)[1L, ],
    c(FALSE, FALSE, TRUE, TRUE)
  )
  # One column reaches any cumulative share alone
  expect_true(all(active_factor_draws(norm_sq[, 1L, drop = FALSE],
                                      "cumulative", 1)))
})
