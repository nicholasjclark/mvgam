# The `newdata` interface of mvgam(): the structural check and the
# persistence of held-out data, without a Stan fit.

test_that("check_newdata requires the axis columns and training levels", {
  expect_null(check_newdata(NULL, data.frame(time = 1)))
  data <- data.frame(
    time = 1:3,
    series = factor(rep("s1", 3L), levels = c("s1", "s2"))
  )
  # The rows still name s2. Checking the level set alone passed on a
  # coercion putting every row on s1.
  out <- check_newdata(data.frame(time = 4:5, series = "s2"), data)
  expect_identical(out$series,
                   factor(c("s2", "s2"), levels = c("s1", "s2")))
  expect_error(
    check_newdata(data.frame(time = 4:5, series = "unknown"), data),
    "has levels absent from the training data"
  )
  expect_error(check_newdata(data.frame(series = "s1"), data),
               "Absent: 'time'")
})

test_that("check_newdata takes its columns from the trend constructor", {
  data <- data.frame(week = rep(1:3, 2L),
                     sp = factor(rep(c("a", "b"), each = 3L)),
                     region = factor(rep(c("n", "s"), each = 3L)))
  # Axis names the trend gives were refused with an assertion on an
  # empty column list, on every mvgam() call passing 'newdata'.
  axes <- ~ AR(time = week, series = sp)
  expect_silent(check_newdata(data[4:6, ], data, axes))
  expect_error(check_newdata(data[, c("week", "region")], data, axes),
               "Absent: 'sp'")
  # A grouping names the series, and its columns are required instead.
  grouped <- ~ ZMVN(time = week, gr = region, subgr = sp)
  expect_error(check_newdata(data[, c("week", "sp")], data, grouped),
               "Absent: 'region'")
})

test_that("mvgam() forwards newdata through to test_data persistence", {
  # Verify the wiring without invoking the Stan compile path:
  # mock mvgam_single so the call captures the validated
  # newdata it would receive in the live pipeline.
  data_train <- data.frame(
    y = 1:5,
    time = 1:5,
    series = factor(rep("s1", 5L), levels = c("s1", "s2"))
  )
  test_df <- data.frame(
    y = NA_real_,
    time = 6:8,
    series = c("s1", "s2", "s1")
  )
  captured <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    mvgam_single = function(formula, trend_formula, data, backend,
                            family, data_name = NULL,
                            newdata = NULL, ...) {
      captured$newdata <- newdata
      structure(list(test_data = newdata, data = data),
                 class = c("mvgam", "brmsfit"))
    }
  )
  fit <- mvgam(
    y ~ 1, data = data_train, newdata = test_df,
    family = gaussian()
  )
  # The validator coerced newdata$series to the training levels and
  # kept each row on the series it named.
  expect_identical(
    captured$newdata$series,
    factor(c("s1", "s2", "s1"), levels = c("s1", "s2"))
  )
  expect_identical(fit$test_data, captured$newdata)
})
