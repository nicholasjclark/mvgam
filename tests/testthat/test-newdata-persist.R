# Unit tests for the `newdata` interface on mvgam() (N2).
# Exercises the lightweight validator and the test_data
# persistence path without spinning up a Stan fit.

test_that("validate_newdata accepts NULL", {
  data <- data.frame(time = 1:3, series = factor("s1"))
  expect_null(mvgam:::validate_newdata(NULL, data))
})

test_that("validate_newdata coerces newdata$series to training levels", {
  data <- data.frame(
    time = 1:3,
    series = factor(rep("s1", 3L), levels = c("s1", "s2"))
  )
  nd <- data.frame(time = 4:5, series = "s2")
  out <- mvgam:::validate_newdata(nd, data)
  expect_true(is.factor(out$series))
  expect_equal(levels(out$series), c("s1", "s2"))
})

test_that("validate_newdata errors on series outside training levels", {
  data <- data.frame(time = 1:3, series = factor("s1"))
  nd <- data.frame(time = 4:5, series = "unknown")
  expect_error(
    mvgam:::validate_newdata(nd, data),
    "not present"
  )
})

test_that("validate_newdata errors on missing required columns", {
  data <- data.frame(time = 1:3, series = factor("s1"), y = 0)
  nd <- data.frame(series = "s1")
  expect_error(mvgam:::validate_newdata(nd, data))
})

test_that("validate_newdata errors on non-data.frame input", {
  data <- data.frame(time = 1, series = factor("s1"))
  expect_error(
    mvgam:::validate_newdata(list(time = 1), data)
  )
})

test_that("validate_newdata is a no-op when data has no series factor", {
  data <- data.frame(time = 1:3, series = c("s1", "s1", "s1"))
  nd <- data.frame(time = 4:5, series = c("s1", "s1"))
  out <- mvgam:::validate_newdata(nd, data)
  expect_identical(out$series, c("s1", "s1"))
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
  # The validator coerced newdata$series to the training levels.
  expect_true(is.factor(captured$newdata$series))
  expect_equal(levels(captured$newdata$series), c("s1", "s2"))
  expect_identical(fit$test_data, captured$newdata)
})
