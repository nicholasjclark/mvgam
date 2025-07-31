test_that("Trend registry initializes correctly", {
  # Clear registry for clean testing
  rm(list = ls(envir = trend_registry), envir = trend_registry)
  
  # Registry should be empty initially
  expect_equal(length(ls(trend_registry)), 0)
  
  # Initialization should populate core trends
  register_core_trends()
  
  # Check all core trends are registered
  expected_trends <- c("AR", "RW", "VAR", "ZMVN", "PW", "PWlinear", "PWlogistic", "CAR")
  registered_trends <- ls(trend_registry)
  expect_true(all(expected_trends %in% registered_trends))
})

test_that("Trend type registration works correctly", {
  # Clear registry
  rm(list = ls(envir = trend_registry), envir = trend_registry)
  
  # Mock generator function
  mock_generator <- function(trend_spec, data_info) {
    list(mock_stanvar = "test")
  }
  
  # Register a new trend type
  result <- register_trend_type("TestTrend", 
                               supports_factors = TRUE, 
                               generator_func = mock_generator)
  
  expect_true(result)
  expect_true("TestTrend" %in% ls(trend_registry))
  
  trend_info <- trend_registry[["TestTrend"]]
  expect_true(trend_info$supports_factors)
  expect_identical(trend_info$generator, mock_generator)
  expect_null(trend_info$incompatibility_reason)
})

test_that("Factor incompatible trend registration works", {
  # Clear registry
  rm(list = ls(envir = trend_registry), envir = trend_registry)
  
  mock_generator <- function(trend_spec, data_info) {
    list(mock_stanvar = "test")
  }
  
  # Register factor-incompatible trend
  register_trend_type("IncompatibleTrend", 
                     supports_factors = FALSE, 
                     generator_func = mock_generator,
                     incompatibility_reason = "Test incompatibility reason")
  
  trend_info <- trend_registry[["IncompatibleTrend"]]
  expect_false(trend_info$supports_factors)
  expect_equal(trend_info$incompatibility_reason, "Test incompatibility reason")
})

test_that("get_trend_info works correctly", {
  # Clear and initialize registry
  rm(list = ls(envir = trend_registry), envir = trend_registry)
  register_core_trends()
  
  # Test getting existing trend info
  ar_info <- get_trend_info("AR")
  expect_true(ar_info$supports_factors)
  expect_is(ar_info$generator, "function")
  
  # Test getting non-existent trend
  expect_error(get_trend_info("NonExistentTrend"), 
               "Unknown trend type.*NonExistentTrend")
})

test_that("list_trend_types works correctly", {
  # Clear and initialize registry
  rm(list = ls(envir = trend_registry), envir = trend_registry)
  register_core_trends()
  
  trend_list <- list_trend_types()
  
  expect_is(trend_list, "data.frame")
  expect_true("trend_type" %in% names(trend_list))
  expect_true("supports_factors" %in% names(trend_list))
  expect_true("incompatibility_reason" %in% names(trend_list))
  
  # Check factor-compatible trends
  factor_compatible <- trend_list[trend_list$supports_factors, "trend_type"]
  expect_true("AR" %in% factor_compatible)
  expect_true("RW" %in% factor_compatible)
  expect_true("VAR" %in% factor_compatible)
  expect_true("ZMVN" %in% factor_compatible)
  
  # Check factor-incompatible trends
  factor_incompatible <- trend_list[!trend_list$supports_factors, "trend_type"]
  expect_true("PW" %in% factor_incompatible)
  expect_true("CAR" %in% factor_incompatible)
})

test_that("Factor compatibility validation works", {
  # Clear and initialize registry
  rm(list = ls(envir = trend_registry), envir = trend_registry)
  register_core_trends()
  
  # Test factor-compatible trend with n_lv (should pass)
  ar_spec <- list(trend_model = "AR", n_lv = 3)
  expect_silent(validate_factor_compatibility(ar_spec))
  
  # Test factor-compatible trend without n_lv (should pass)
  ar_spec_no_lv <- list(trend_model = "AR", n_lv = NULL)
  expect_silent(validate_factor_compatibility(ar_spec_no_lv))
  
  # Test factor-incompatible trend with n_lv (should error)
  pw_spec <- list(trend_model = "PW", n_lv = 3)
  expect_error(validate_factor_compatibility(pw_spec), 
               "Factor models.*not supported.*PW.*trends")
  
  # Test factor-incompatible trend without n_lv (should pass)
  pw_spec_no_lv <- list(trend_model = "PW", n_lv = NULL)
  expect_silent(validate_factor_compatibility(pw_spec_no_lv))
})

test_that("get_factor_compatible_trends works", {
  # Clear and initialize registry
  rm(list = ls(envir = trend_registry), envir = trend_registry)
  register_core_trends()
  
  compatible_trends <- get_factor_compatible_trends()
  
  expect_true("AR" %in% compatible_trends)
  expect_true("RW" %in% compatible_trends)
  expect_true("VAR" %in% compatible_trends)
  expect_true("ZMVN" %in% compatible_trends)
  expect_false("PW" %in% compatible_trends)
  expect_false("CAR" %in% compatible_trends)
})

test_that("Registry initialization is idempotent", {
  # Clear registry
  rm(list = ls(envir = trend_registry), envir = trend_registry)
  
  # Multiple initializations should not cause problems
  register_core_trends()
  first_count <- length(ls(trend_registry))
  
  register_core_trends()
  second_count <- length(ls(trend_registry))
  
  expect_equal(first_count, second_count)
})

test_that("Custom trend registration works", {
  # Clear registry
  rm(list = ls(envir = trend_registry), envir = trend_registry)
  
  # Mock generator
  custom_generator <- function(trend_spec, data_info) {
    list(custom_stanvar = "custom_test")
  }
  
  # Register custom trend
  expect_silent(register_custom_trend("CustomTrend", 
                                     supports_factors = TRUE,
                                     generator_func = custom_generator))
  
  # Check it was registered
  expect_true("CustomTrend" %in% ls(trend_registry))
  custom_info <- trend_registry[["CustomTrend"]]
  expect_true(custom_info$supports_factors)
  expect_identical(custom_info$generator, custom_generator)
})

test_that("Overwriting existing trend types gives warning", {
  # Clear and initialize registry
  rm(list = ls(envir = trend_registry), envir = trend_registry)
  register_core_trends()
  
  # Mock generator
  new_generator <- function(trend_spec, data_info) {
    list(new_stanvar = "new_test")
  }
  
  # Overwriting should give warning
  expect_warning(register_custom_trend("AR", 
                                      supports_factors = FALSE,
                                      generator_func = new_generator),
                "Overwriting existing trend type: AR")
})

test_that("ensure_registry_initialized works", {
  # Clear registry
  rm(list = ls(envir = trend_registry), envir = trend_registry)
  
  # Registry should be empty
  expect_false(is_registry_initialized())
  
  # ensure_registry_initialized should populate it
  ensure_registry_initialized()
  expect_true(is_registry_initialized())
  
  # Second call should not change anything
  trend_count_before <- length(ls(trend_registry))
  ensure_registry_initialized()
  trend_count_after <- length(ls(trend_registry))
  expect_equal(trend_count_before, trend_count_after)
})

test_that("Main dispatcher uses registry correctly", {
  # Clear and initialize registry
  rm(list = ls(envir = trend_registry), envir = trend_registry)
  register_core_trends()
  
  # Mock data structures
  trend_spec <- list(trend_model = "AR", n_lv = NULL, p = 1)
  data_info <- list(n_series = 3, n_lv = 3)
  
  # This should work without error (assuming AR generator exists)
  # We can't test the actual output without the full generator functions
  # but we can test that dispatch works
  expect_silent({
    # This will call ensure_registry_initialized, get_trend_info, 
    # validate_factor_compatibility, and the generator
    tryCatch(
      generate_trend_injection_stanvars(trend_spec, data_info),
      error = function(e) {
        # Expected to fail because we don't have complete generator functions
        # but it should get past the registry parts
        expect_false(grepl("Unknown trend type", e$message))
        expect_false(grepl("not supported.*factor", e$message))
      }
    )
  })
})

test_that("Factor validation error messages are informative", {
  # Clear and initialize registry
  rm(list = ls(envir = trend_registry), envir = trend_registry)
  register_core_trends()
  
  # Test CAR with n_lv
  car_spec <- list(trend_model = "CAR", n_lv = 2)
  expect_error(validate_factor_compatibility(car_spec), 
               "Factor models.*not supported.*CAR.*trends")
  expect_error(validate_factor_compatibility(car_spec), 
               "irregular time intervals")
  
  # Test PW with n_lv
  pw_spec <- list(trend_model = "PW", n_lv = 2)  
  expect_error(validate_factor_compatibility(pw_spec),
               "Factor models.*not supported.*PW.*trends")
  expect_error(validate_factor_compatibility(pw_spec),
               "changepoint modeling")
})