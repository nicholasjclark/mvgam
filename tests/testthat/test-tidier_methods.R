test_that("augment doesn't error", {
  expect_no_error(augment(mvgam_example1))
  expect_no_error(augment(mvgam_example5))
})

test_that("augment return types", {
  out1 = augment(mvgam_example1)
  out5 = augment(mvgam_example5)
  expect_equal(class(out1)[[1]], "tbl_df")
  expect_equal(class(out5), "list")
})
