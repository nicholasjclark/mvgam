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
