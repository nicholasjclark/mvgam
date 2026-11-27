

test_that("resolve_series_index defaults make sense", {
  lv1 <- "s1"
  lv4 <- c("s1", "s2", "s3", "s4")
  expect_equal(mvgam:::resolve_series_index(NULL, lv1), 1L)
  expect_equal(mvgam:::resolve_series_index(NULL, lv4), "all")
  # A name resolves to the index every other series-aware surface
  # gives it, which lets a panel label be passed back as `series`.
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


test_that("the response fan-out forwards dots past a dot-free caller", {
  # `plot(type = "series")` reaches `plot_mvgam_series()`, which takes
  # no dots, through a call that forwards `plot()`'s dots.
  stub <- structure(
    list(formula = brms::bf(y1 ~ 1) + brms::bf(y2 ~ 1)),
    class = "mvgam"
  )
  inner <- function(object, resp = NULL, label = "") {
    fan <- mvgam:::mv_resp_fan_out(object, resp)
    if (!is.null(fan)) return(fan)
    paste(resp, label)
  }
  outer <- function(x, ...) inner(object = x, ...)
  expect_identical(outer(stub, label = "a"),
                   list(y1 = "y1 a", y2 = "y2 a"))
})
