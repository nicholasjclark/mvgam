context("tidier methods")

test_that("augment doesn't error", {
  expect_no_error(augment(mvgam:::mvgam_example1))
  expect_no_error(augment(mvgam:::mvgam_example4))
})

test_that("augment returns correct types", {
  out1 <- augment(mvgam:::mvgam_example1)
  out4 <- augment(mvgam:::mvgam_example4)

  expect_equal(class(out1)[[1]], "tbl_df")
  expect_equal(class(out4), "list")

  # Lengths of augment output and of obs data should be equal
  expect_equal(NROW(out1), NROW(mvgam:::mvgam_example1$obs_data))
  expect_equal(length(out4$y), length(mvgam:::mvgam_example4$obs_data$y))

  # NAs in obs data should equal NAs in residuals
  expect_true(all(which(is.na(mvgam:::mvgam_example1$obs_data$y)) %in%
                 which(is.na(out1$.resid))))
})
