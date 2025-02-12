context("tidier methods")

# `tidy()` tests
test_that("`tidy()` snapshot value of `mvgam_example1`", {
  local_edition(3)
  expect_snapshot_value(tidy.mvgam(mvgam_example1), style = "json2")
})

test_that("`tidy()` snapshot value of `mvgam_example2`", {
  local_edition(3)
  expect_snapshot_value(tidy.mvgam(mvgam_example2), style = "json2")
})

test_that("`tidy()` snapshot value of `mvgam_example3`", {
  local_edition(3)
  expect_snapshot_value(tidy.mvgam(mvgam_example3), style = "json2")
})

test_that("`tidy()` snapshot value of `mvgam_example4`", {
  local_edition(3)
  expect_snapshot_value(tidy.mvgam(mvgam_example4), style = "json2")
})

test_that("`tidy()` snapshot value of `mvgam_example5`", {
  local_edition(3)
  expect_snapshot_value(tidy.mvgam(mvgam_example5), style = "json2")
})

test_that("`tidy()` snapshot value of `mvgam_example6`", {
  local_edition(3)
  expect_snapshot_value(tidy.mvgam(mvgam_example6), style = "json2")
})

# `augment()` tests
test_that("augment doesn't error", {
  expect_no_error(augment(mvgam:::mvgam_example1))
  expect_no_error(augment(mvgam:::mvgam_example5))
})

test_that("augment returns correct types", {
  out1 <- augment(mvgam:::mvgam_example1)
  out5 <- augment(mvgam:::mvgam_example5)

  expect_equal(class(out1)[[1]], "tbl_df")
  expect_equal(class(out5), "list")

  # Lengths of augment output and of obs data should be equal
  expect_equal(NROW(out1), NROW(mvgam:::mvgam_example1$obs_data))
  expect_equal(length(out5$y), length(mvgam:::mvgam_example5$obs_data$y))

  # NAs in obs data should equal NAs in residuals
  expect_equal(which(is.na(out5$.resid)),
               which(is.na(mvgam:::mvgam_example5$obs_data$y)))
})
