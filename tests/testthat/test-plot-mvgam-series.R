# Unit tests for the internal `plot_mvgam_series` helpers.
# Exercises validation, the long-format extractor, and each
# small panel builder without requiring a Stan fit.

.make_stub_fit <- function(
  n_series = 1L,
  n_time = 30L,
  with_test = FALSE,
  seed = 1L
) {
  set.seed(seed)
  series_levels <- paste0("s", seq_len(n_series))
  d <- data.frame(
    y = rnorm(n_time * n_series),
    time = rep(seq_len(n_time), n_series),
    series = factor(
      rep(series_levels, each = n_time),
      levels = series_levels
    )
  )
  test_d <- if (with_test) {
    data.frame(
      y = rnorm(5L * n_series),
      time = rep(n_time + seq_len(5L), n_series),
      series = factor(
        rep(series_levels, each = 5L), levels = series_levels
      )
    )
  } else {
    NULL
  }
  obj <- list(
    data = d,
    test_data = test_d,
    formula = stats::as.formula("y ~ 1"),
    family = stats::gaussian(),
    trend_metadata = mock_axis_record(d)
  )
  class(obj) <- c("mvgam", "brmsfit")
  obj
}


test_that("resolve_series_index defaults make sense", {
  lv1 <- "s1"
  lv4 <- c("s1", "s2", "s3", "s4")
  expect_equal(mvgam:::resolve_series_index(NULL, lv1), 1L)
  expect_equal(mvgam:::resolve_series_index(NULL, lv4), "all")
  # A name reaches the index every other series-aware surface gives
  # it. This plot once took indices alone, and a reader of its panel
  # labels had no value to pass back.
  expect_equal(mvgam:::resolve_series_index("s3", lv4), 3L)
})

test_that("resolve_series_index rejects invalid input", {
  lv4 <- c("s1", "s2", "s3", "s4")
  # The name and the count are checked against the model's own
  # levels. An unknown name reports which names exist.
  expect_error(
    mvgam:::resolve_series_index("foo", lv4),
    "Unknown series name"
  )
  expect_error(
    mvgam:::resolve_series_index(c(1L, 2L), lv4),
    "selects one series"
  )
  # Out of range and fractional indices are refused.
  expect_error(mvgam:::resolve_series_index(99L, lv4), "out of range")
  expect_error(mvgam:::resolve_series_index(1.5, lv4))
})

test_that("series_long_df extracts the right columns and labels rows", {
  df <- data.frame(
    y = 1:5, time = 1:5, series = factor(rep("s1", 5L))
  )
  out <- mvgam:::series_long_df(df, "y", df$series, "time",
                                label = "train")
  expect_equal(sort(colnames(out)), c("data", "series", "time", "y"))
  # The series is the one the caller names, not a column of the frame.
  out_named <- mvgam:::series_long_df(df, "y", "count", "time",
                                      label = "train")
  expect_true(all(out_named$series == "count"))
})

test_that("series_long_df errors when response missing", {
  df <- data.frame(time = 1:3, series = factor(rep("s1", 3L)))
  expect_error(
    mvgam:::series_long_df(df, "yy", df$series, "time", label = "train"),
    "Response variable not found"
  )
})

test_that("plot_mvgam_series takes one series in time order", {
  fit <- .make_stub_fit()
  # The ACF follows time order on a frame holding its rows out of
  # order.
  shuffled <- fit
  shuffled$data <- fit$data[sample(nrow(fit$data)), ]
  acf_panel <- mvgam:::plot_mvgam_series(shuffled, series = 1L)[[3L]]
  expect_equal(
    acf_panel$data$acf[acf_panel$data$lag == 1],
    stats::acf(fit$data$y, plot = FALSE)$acf[2L]
  )
})

test_that("plot_mvgam_series multi-series default is faceted ggplot", {
  fit <- .make_stub_fit(n_series = 3L)
  p <- mvgam:::plot_mvgam_series(fit)
  expect_true(inherits(p$facet, "FacetWrap"))
})

test_that("plot_mvgam_series auto-fetches object$test_data", {
  fit <- .make_stub_fit(with_test = TRUE)
  p <- mvgam:::plot_mvgam_series(fit, series = 1L)
  # Cut line should appear because newdata was attached. Walk
  # the patchwork's first panel (TS) layers for a GeomVline.
  ts_panel <- p[[1L]]
  geoms <- vapply(
    ts_panel$layers,
    function(l) class(l$geom)[1L], character(1L)
  )
  expect_true("GeomVline" %in% geoms)
})

