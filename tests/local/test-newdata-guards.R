# What a prediction frame has to carry.
#
# A prediction places every row on the trend's two axes. The series
# axis has substitutes: the record holds the levels, and a grouping
# names the series with no series column present. An occasion has no
# substitute. A frame omitting its time column was handed predictions
# at whatever position the observation structure fell back to, of the
# right shape and finite throughout, and indistinguishable from a
# correct result.
#
# The guard that owns this is `validate_newdata_complete()`. It tested
# the columns the frame carried, and a column left out entirely was
# dropped from the test before it ran.

fit <- readRDS("fixtures/val_mvgam_var_trend.rds")
time_var <- mvgam:::axis_vars(fit)$time_var
series_var <- mvgam:::axis_vars(fit)$series_var
train <- mvgam:::mvgam_training_data(fit)


test_that("a complete frame predicts", {
  ep <- posterior_epred(fit, newdata = train)
  expect_true(is.matrix(ep))
  expect_identical(ncol(ep), nrow(train))
  expect_false(anyNA(ep))
})


test_that("a frame omitting the time column is refused", {
  nd <- train
  nd[[time_var]] <- NULL
  expect_error(
    posterior_epred(fit, newdata = nd),
    "Absent"
  )
  # The refusal names the column the frame left out. The caller then
  # knows which one to supply.
  expect_error(posterior_epred(fit, newdata = nd), time_var)
})


test_that("a missing time value is refused and its row named", {
  nd <- train
  nd[[time_var]][3L] <- NA
  expect_error(
    posterior_epred(fit, newdata = nd),
    "first at row 3"
  )

  all_na <- train
  all_na[[time_var]] <- NA
  expect_error(posterior_epred(fit, newdata = all_na), time_var)
})


test_that("a frame omitting the series column still predicts", {
  # The axis record places a row the frame never keyed. This is the
  # half of the axis carrying a substitute, and refusing it would
  # refuse a hierarchical frame the model was fitted on.
  nd <- train
  nd[[series_var]] <- NULL
  ep <- posterior_epred(fit, newdata = nd)
  expect_identical(ncol(ep), nrow(train))
})


test_that("a series level absent from the fit is refused", {
  nd <- train
  nd[[series_var]] <- as.character(nd[[series_var]])
  nd[[series_var]][1L] <- "ghost"
  expect_error(posterior_epred(fit, newdata = nd), "ghost")
})
