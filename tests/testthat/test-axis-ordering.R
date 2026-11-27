

test_that("the axis record takes its series source from the frame", {
  # `ensure_mvgam_variables()` records how the series were named. A
  # frame holding the series without that record is refused.
  frame <- data.frame(
    time = rep(1:4, 2L), series = factor(rep(c("a", "b"), each = 4L)),
    y = 1:8
  )
  prepared <- ensure_mvgam_variables(frame, NULL, "time", "series",
                                     c(y = "y"))
  dims <- extract_time_series_dimensions(prepared, "time", "series")
  expect_identical(dims$axes$series$source, "explicit")
  attr(prepared, "mvgam_series_source") <- NULL
  expect_error(
    extract_time_series_dimensions(prepared, "time", "series"),
    "mvgam_series_source"
  )
})


test_that("spec_axis_vars resolves every shape a spec arrives in", {
  # The pair is spelled `time_var` or `time`, and written flat or
  # under `$trend_model`. A layer resolving it alone can name a
  # different column from the one the model was fitted on.
  expect_identical(
    spec_axis_vars(NULL),
    list(time_var = "time", series_var = "series")
  )
  expect_identical(
    spec_axis_vars(list(time_var = "wk", series_var = "sp")),
    list(time_var = "wk", series_var = "sp")
  )
  expect_identical(
    spec_axis_vars(list(time = "wk", series = "sp")),
    list(time_var = "wk", series_var = "sp")
  )
  expect_identical(
    spec_axis_vars(list(trend_model = list(time = "wk",
                                           series = "sp"))),
    list(time_var = "wk", series_var = "sp")
  )
  # A named set holds one specification per response, and the first
  # names the axis.
  expect_identical(
    spec_axis_vars(list(
      yA = list(trend = "AR", time = "wk", series = "sp"),
      yB = list(trend = "AR", time = "zz", series = "qq")
    )),
    list(time_var = "wk", series_var = "sp")
  )
  # `trend_spec_head()` takes a first element for a named
  # multivariate set alone. An unnamed list of one gives the
  # defaults, and a caller holding that shape takes the head first.
  expect_identical(
    spec_axis_vars(list(list(trend = "ZMVN", time = "wk",
                             series = "sp"))),
    list(time_var = "time", series_var = "series")
  )
})
