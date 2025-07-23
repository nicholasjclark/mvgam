context("summary structure")

test_that("summary.mvgam returns structured object", {
  # Test with internal example object
  summary_obj <- summary(mvgam:::mvgam_example1)

  # Check class
  expect_s3_class(summary_obj, "mvgam_summary")
  expect_s3_class(summary_obj, "list")

  # Check structure
  expect_named(summary_obj, c("model_spec", "parameters", "diagnostics", "sampling_info"))

  # Check that each component is a list
  expect_type(summary_obj$model_spec, "list")
  expect_type(summary_obj$parameters, "list")
  expect_type(summary_obj$diagnostics, "list")
  expect_type(summary_obj$sampling_info, "list")

  # Check that model_spec has expected components
  expect_named(summary_obj$model_spec,
               c("formulas", "family", "link", "trend_model", "upper_bounds",
                 "latent_variables", "dimensions", "is_jsdgam"))
})

test_that("print.mvgam_summary works", {
  summary_obj <- summary(mvgam:::mvgam_example1)

  # Should print without error
  expect_no_error(capture_output(print(summary_obj)))

  # Should return object invisibly
  result <- capture_output(returned_obj <- print(summary_obj))
  expect_identical(summary_obj, returned_obj)
})

test_that("summary can be saved and reloaded", {
  summary_obj <- summary(mvgam:::mvgam_example1)

  # Save to temporary file
  temp_file <- tempfile(fileext = ".rds")
  saveRDS(summary_obj, temp_file)

  # Reload
  reloaded_obj <- readRDS(temp_file)

  # Should be identical
  expect_identical(summary_obj, reloaded_obj)

  # Should still print correctly
  expect_no_error(capture_output(print(reloaded_obj)))

  # Clean up
  unlink(temp_file)
})
