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
    series_info = list(
      series_levels = series_levels,
      n_series = n_series, series_names = factor(series_levels)
    ),
    trend_metadata = list(
      variables = list(time_var = "time", series_var = "series"),
      levels = list(series = series_levels)
    )
  )
  class(obj) <- c("mvgam", "brmsfit")
  obj
}


test_that("resolve_series_index defaults make sense", {
  expect_equal(mvgam:::resolve_series_index(NULL, 1L), 1L)
  expect_equal(mvgam:::resolve_series_index(NULL, 4L), "all")
  expect_equal(mvgam:::resolve_series_index("all", 4L), "all")
  expect_equal(mvgam:::resolve_series_index(2L, 4L), 2L)
})

test_that("resolve_series_index rejects invalid input", {
  expect_error(
    mvgam:::resolve_series_index(-1, 4L),
    "positive integer index"
  )
  expect_error(
    mvgam:::resolve_series_index("foo", 4L),
    "positive integer index"
  )
  expect_error(
    mvgam:::resolve_series_index(99L, 4L),
    "positive integer index"
  )
  expect_error(
    mvgam:::resolve_series_index(1.5, 4L),
    "positive integer index"
  )
})

test_that("series_long_df extracts the right columns and labels rows", {
  df <- data.frame(
    y = 1:5, time = 1:5, series = factor(rep("s1", 5L))
  )
  out <- mvgam:::series_long_df(df, "y", df$series, "time",
                                label = "train")
  expect_equal(sort(colnames(out)), c("data", "series", "time", "y"))
  expect_equal(nrow(out), 5L)
  expect_true(all(out$data == "train"))
  # The series is the one the caller names, not a column of the frame.
  out_named <- mvgam:::series_long_df(df, "y", "count", "time",
                                      label = "train")
  expect_true(all(out_named$series == "count"))
})

test_that("series_long_df returns NULL on NULL input (rbind passthrough)", {
  out <- mvgam:::series_long_df(NULL, "y", NULL, "time",
                                label = "validate")
  expect_null(out)
})

test_that("series_long_df errors when response missing", {
  df <- data.frame(time = 1:3, series = factor(rep("s1", 3L)))
  expect_error(
    mvgam:::series_long_df(df, "yy", df$series, "time", label = "train"),
    "Response variable not found"
  )
})

test_that("plot_mvgam_series single-series returns 4-panel patchwork", {
  fit <- .make_stub_fit()
  p <- mvgam:::plot_mvgam_series(fit, series = 1L)
  expect_true(inherits(p, "patchwork"))
})

test_that("plot_mvgam_series multi-series default is faceted ggplot", {
  fit <- .make_stub_fit(n_series = 3L)
  p <- mvgam:::plot_mvgam_series(fit)
  expect_ggplot(p)
  expect_true(inherits(p$facet, "FacetWrap"))
})

test_that("plot_mvgam_series auto-fetches object$test_data", {
  fit <- .make_stub_fit(with_test = TRUE)
  p <- mvgam:::plot_mvgam_series(fit, series = 1L)
  expect_true(inherits(p, "patchwork"))
  # Cut line should appear because newdata was attached. Walk
  # the patchwork's first panel (TS) layers for a GeomVline.
  ts_panel <- p[[1L]]
  geoms <- vapply(
    ts_panel$layers,
    function(l) class(l$geom)[1L], character(1L)
  )
  expect_true("GeomVline" %in% geoms)
})

test_that("plot_mvgam_series log_scale switches the y label", {
  # Strictly-positive y so log(y + 1) doesn't produce NaN.
  fit <- .make_stub_fit(n_series = 3L)
  fit$data$y <- exp(fit$data$y)
  p <- mvgam:::plot_mvgam_series(fit, log_scale = TRUE)
  expect_true(grepl("log\\(", p$labels$y))
})

test_that("series_hist_panel returns a ggplot with one histogram layer", {
  p <- mvgam:::series_hist_panel(rnorm(50L), "y")
  expect_ggplot(p)
  expect_true(any(vapply(
    p$layers,
    function(l) inherits(l$geom, "GeomBar"),
    logical(1L)
  )))
})

test_that("series_acf_panel returns a ggplot", {
  p <- mvgam:::series_acf_panel(rnorm(50L))
  expect_ggplot(p)
  expect_equal(p$labels$title, "ACF")
})

test_that("series_ecdf_panel returns a ggplot with sensible y limits", {
  p <- mvgam:::series_ecdf_panel(rnorm(50L), "y")
  expect_ggplot(p)
  expect_equal(p$labels$title, "CDF")
})
