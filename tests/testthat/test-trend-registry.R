test_that("Trend registry initializes correctly", {
  # Clear registry for clean testing
  rm(list = ls(envir = trend_registry), envir = trend_registry)

  # Registry should be empty initially
  expect_equal(length(ls(trend_registry)), 0)

  # Initialization should populate core trends
  register_core_trends()

  # Check all core trends are registered (modern trend system, no "None" trend)
  expected_trends <- c("AR", "RW", "VAR", "ZMVN", "CAR", "PW")
  registered_trends <- ls(trend_registry)
  expect_true(all(expected_trends %in% registered_trends))
})

test_that("Trend type registration works correctly", {
  # Clear registry
  rm(list = ls(envir = trend_registry), envir = trend_registry)

  # Mock generator function
  mock_generator <- function(trend_specs, data_info) {
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

  mock_generator <- function(trend_specs, data_info) {
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

  # Test getting existing trend info (AR now supports factors)
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

  # Check factor-compatible trends (AR now factor-compatible, plus ZMVN)
  factor_compatible <- trend_list[trend_list$supports_factors, "trend_type"]
  expect_true("AR" %in% factor_compatible)
  expect_true("RW" %in% factor_compatible)
  expect_true("VAR" %in% factor_compatible)
  expect_true("ZMVN" %in% factor_compatible)

  # Check factor-incompatible trends (CAR, PW)
  factor_incompatible <- trend_list[!trend_list$supports_factors, "trend_type"]
  expect_true("CAR" %in% factor_incompatible)
  expect_true("PW" %in% factor_incompatible)
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
  custom_generator <- function(trend_specs, data_info) {
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
  new_generator <- function(trend_specs, data_info) {
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
  trend_specs <- list(trend_model = "AR", n_lv = NULL, p = 1)
  data_info <- list(n_series = 3, n_lv = 3)

  # This should work without error (assuming AR generator exists)
  # We can't test the actual output without the full generator functions
  # but we can test that dispatch works
  expect_silent({
    # This will call ensure_registry_initialized, get_trend_info,
    # validate_factor_compatibility, and the generator
    tryCatch(
      generate_trend_injection_stanvars(trend_specs, data_info),
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
               "series-specific irregular time intervals")

  # Test PW with n_lv
  pw_spec <- list(trend_model = "PW", n_lv = 2)
  expect_error(validate_factor_compatibility(pw_spec),
               "Factor models.*not supported.*PW.*trends")
  expect_error(validate_factor_compatibility(pw_spec),
               "changepoint modeling")
})

# Tests for parameter processing function
test_that("process_trend_params handles simple parameters correctly", {
  # Test basic parameter processing with trend_param objects
  param_specs <- trend_param("sigma", bounds = c(0, Inf)) +
                 trend_param("theta", bounds = c(-1, 1)) +
                 trend_param("alpha", bounds = c(0, 1))

  result <- mvgam:::process_trend_params(param_specs)

  # Check tpars
  expect_equal(length(result$tpars), 3)
  expect_true("sigma_trend" %in% result$tpars)
  expect_true("theta_trend" %in% result$tpars)
  expect_true("alpha_trend" %in% result$tpars)

  # Check bounds
  expect_equal(length(result$bounds), 3)
  expect_equal(result$bounds$sigma_trend, c(0, Inf))
  expect_equal(result$bounds$theta_trend, c(-1, 1))
  expect_equal(result$bounds$alpha_trend, c(0, 1))
})

test_that("process_trend_params handles conditional parameters correctly", {
  # Test with conditional parameters using trend_param objects
  param_specs <- trend_param("sigma", bounds = c(0, Inf)) +
                 trend_param("theta", bounds = c(-1, 1), condition = FALSE) +  # Excluded condition
                 trend_param("Sigma", bounds = c(-1, 1))

  result <- mvgam:::process_trend_params(param_specs)

  # Check tpars (should exclude conditional FALSE entries)
  expect_equal(length(result$tpars), 2)
  expect_true("sigma_trend" %in% result$tpars)
  expect_true("Sigma_trend" %in% result$tpars)
  expect_false("theta_trend" %in% result$tpars)

  # Check bounds (should exclude conditional FALSE entries)
  expect_equal(length(result$bounds), 2)
  expect_true("sigma_trend" %in% names(result$bounds))
  expect_true("Sigma_trend" %in% names(result$bounds))
  expect_false("theta_trend" %in% names(result$bounds))
})

test_that("process_trend_params handles empty input correctly", {
  # Test NULL parameter specs
  result <- mvgam:::process_trend_params(NULL)

  expect_equal(length(result$tpars), 0)
  expect_equal(length(result$bounds), 0)
})

test_that("process_trend_params preserves existing _trend suffix", {
  # Test parameters that already have _trend suffix
  param_specs <- trend_param("sigma_trend", bounds = c(0, Inf)) +  # Already has suffix
                 trend_param("theta", bounds = c(-1, 1))            # Needs suffix

  result <- mvgam:::process_trend_params(param_specs)

  expect_equal(length(result$tpars), 2)
  expect_true("sigma_trend" %in% result$tpars)
  expect_true("theta_trend" %in% result$tpars)

  # Should not have sigma_trend_trend
  expect_false("sigma_trend_trend" %in% result$tpars)
})

test_that("simplified RW constructor works correctly", {
  # Test simplified RW constructor using helper functions
  suppressWarnings({
    # Test basic RW constructor
    rw_trend <- RW()
    expect_s3_class(rw_trend, "mvgam_trend")
    expect_equal(rw_trend$trend, "RW")
    expect_false(rw_trend$ma)
    expect_false(rw_trend$cor)
    expect_null(rw_trend$n_lv)
    
    # Test validation rules are automatically assigned
    expect_true(length(rw_trend$validation_rules) > 0)
    expect_true("requires_regular_intervals" %in% rw_trend$validation_rules)
    expect_true("supports_factors" %in% rw_trend$validation_rules)
    expect_true("supports_hierarchical" %in% rw_trend$validation_rules)
    
    # Test RW with parameters
    rw_ma_trend <- RW(ma = TRUE, cor = TRUE)
    expect_true(rw_ma_trend$ma)
    expect_true(rw_ma_trend$cor)
    
    # Test RW with custom variables
    rw_custom <- RW(time = week, series = species, ma = TRUE)
    expect_equal(rw_custom$time, "week")
    expect_equal(rw_custom$series, "species")
    expect_true(rw_custom$ma)
  })
})

test_that("helper functions work correctly", {
  # Test get_mvgam_trend_defaults
  defaults <- get_mvgam_trend_defaults()
  expect_type(defaults, "list")
  expect_equal(defaults$time, "time")
  expect_equal(defaults$series, "series")
  expect_equal(defaults$gr, "NA")
  expect_false(defaults$ma)
  expect_false(defaults$cor)
  expect_null(defaults$n_lv)
  
  # Test get_default_validation_rules
  rw_rules <- get_default_validation_rules("RW")
  expect_true("requires_regular_intervals" %in% rw_rules)
  expect_true("supports_factors" %in% rw_rules)
  expect_true("supports_hierarchical" %in% rw_rules)
  
  ar_rules <- get_default_validation_rules("AR")
  expect_equal(rw_rules, ar_rules)  # Should be same for stationary trends
  
  car_rules <- get_default_validation_rules("CAR")
  expect_true("allows_irregular_intervals" %in% car_rules)
  expect_true("incompatible_with_factors" %in% car_rules)
  
  # Test apply_mvgam_trend_defaults
  partial_trend <- list(trend = "RW", ma = TRUE)
  complete_trend <- apply_mvgam_trend_defaults(partial_trend)
  expect_equal(complete_trend$trend, "RW")
  expect_true(complete_trend$ma)
  expect_equal(complete_trend$time, "time")  # Filled by defaults
  expect_equal(complete_trend$series, "series")  # Filled by defaults
  expect_true(length(complete_trend$validation_rules) > 0)  # Auto-assigned
  
  # Test create_mvgam_trend helper
  suppressWarnings({
    trend_obj <- create_mvgam_trend("RW", ma = TRUE, cor = FALSE)
    expect_s3_class(trend_obj, "mvgam_trend")
    expect_equal(trend_obj$trend, "RW")
    expect_true(trend_obj$ma)
    expect_false(trend_obj$cor)
    expect_equal(trend_obj$time, "time")
    expect_equal(trend_obj$series, "series")
  })
})

test_that("trend constructors use process_trend_params correctly", {
  # Test that actual trend constructors produce correct parameter names
  suppressWarnings({
    # Test basic RW constructor (parameter processing moved to Stan assembly)
    rw_trend <- RW()
    expect_s3_class(rw_trend, "mvgam_trend")
    expect_equal(rw_trend$trend, "RW")
    expect_false(rw_trend$ma)
    
    # Test RW with ma = TRUE
    rw_ma_trend <- RW(ma = TRUE)
    expect_equal(rw_ma_trend$trend, "RW")
    expect_true(rw_ma_trend$ma)

    # Test AR constructor (complex parameter processing moved to Stan assembly)
    ar1_trend <- AR(p = 1)
    expect_equal(ar1_trend$trend, "AR")  # Base type for all dispatch
    expect_equal(ar1_trend$p, 1)

    ar2_trend <- AR(p = 2)
    expect_equal(ar2_trend$trend, "AR")  # Base type for all dispatch
    expect_equal(ar2_trend$p, 2)

    # Test VAR constructor
    var_trend <- VAR(p = 1)
    expect_equal(var_trend$trend, "VAR1")
    expect_equal(var_trend$p, 1)
    expect_true(var_trend$cor)  # VAR always has cor = TRUE

    # Test CAR constructor (basic structure only - detailed tests will be added after simplification)
    car_trend <- CAR()
    expect_equal(car_trend$trend, "CAR")

    # Test AR with MA
    ar_ma_trend <- AR(p = 1, ma = TRUE)
    expect_equal(ar_ma_trend$trend, "AR")  # Base type for all dispatch
    expect_equal(ar_ma_trend$p, 1)
    expect_true(ar_ma_trend$ma)
  })
})

test_that("validation rules vocabulary is complete", {
  # Test that all validation rule constants are defined
  expect_equal(rule_requires_regular_intervals, "requires_regular_intervals")
  expect_equal(rule_allows_irregular_intervals, "allows_irregular_intervals")
  expect_equal(rule_supports_factors, "supports_factors")
  expect_equal(rule_incompatible_with_factors, "incompatible_with_factors")
  expect_equal(rule_supports_hierarchical, "supports_hierarchical")
  expect_equal(rule_requires_hierarchical, "requires_hierarchical")
  expect_equal(rule_incompatible_with_hierarchical, "incompatible_with_hierarchical")
  expect_equal(rule_requires_seasonal_period, "requires_seasonal_period")
  expect_equal(rule_supports_multiple_seasonality, "supports_multiple_seasonality")
  expect_equal(rule_incompatible_with_seasonal_smooths, "incompatible_with_seasonal_smooths")
  expect_equal(rule_requires_balanced_panels, "requires_balanced_panels")
  expect_equal(rule_requires_minimum_series_count, "requires_minimum_series_count")
  
  # Test that validation rule assignment works for all trend types
  trend_types <- c("RW", "AR", "VAR", "CAR", "PW", "ZMVN")
  for (trend_type in trend_types) {
    rules <- get_default_validation_rules(trend_type)
    expect_true(length(rules) > 0, info = paste("No rules assigned for", trend_type))
    expect_true(all(rules %in% c(
      "requires_regular_intervals", "allows_irregular_intervals",
      "supports_factors", "incompatible_with_factors", 
      "supports_hierarchical", "incompatible_with_hierarchical",
      "requires_hierarchical", "requires_seasonal_period",
      "supports_multiple_seasonality", "incompatible_with_seasonal_smooths",
      "requires_balanced_panels", "requires_minimum_series_count"
    )), info = paste("Invalid rules found for", trend_type))
  }
})

test_that("parameter suffix validation is robust", {
  # Test validation of parameter input - should only accept trend_param objects
  expect_error(
    mvgam:::process_trend_params(c("sigma", "theta")),
    "Assertion on 'param_specs' failed"
  )

  # Test that function rejects simple lists
  expect_error(
    mvgam:::process_trend_params(list(sigma = c(0, 1), theta = c(-1, 1))),
    "Assertion on 'param_specs' failed"
  )
})

test_that("simplified AR constructor works correctly", {
  # Test simplified AR constructor using create_mvgam_trend helper
  suppressWarnings({
    # Test basic AR constructor
    ar_trend <- AR()
    expect_s3_class(ar_trend, "mvgam_trend")
    expect_equal(ar_trend$trend, "AR")  # Base type for dispatch
    expect_equal(ar_trend$p, 1)  # Default p = 1
    expect_false(ar_trend$ma)
    expect_false(ar_trend$cor)
    expect_null(ar_trend$n_lv)
    
    # Test AR with custom parameters
    ar_custom <- AR(p = 3, ma = TRUE, cor = TRUE, n_lv = 2)
    expect_equal(ar_custom$trend, "AR")  # Always base type
    expect_equal(ar_custom$p, 3)
    expect_true(ar_custom$ma)
    expect_true(ar_custom$cor)
    expect_equal(ar_custom$n_lv, 2)
    
    # Test AR with vector p
    ar_vector <- AR(p = c(1, 12))
    expect_equal(ar_vector$trend, "AR")  # Still base type
    expect_equal(ar_vector$p, c(1, 12))
    
    # Test AR with custom time/series variables
    ar_vars <- AR(time = month, series = site)
    expect_equal(ar_vars$time, "month")
    expect_equal(ar_vars$series, "site")
    
    # Test AR with grouping variables
    ar_grouped <- AR(gr = region, subgr = species)
    expect_equal(ar_grouped$gr, "region")
    expect_equal(ar_grouped$subgr, "species")
  })
})

test_that("consistent dispatch metadata is added automatically", {
  suppressWarnings({
    # Test RW gets correct dispatch metadata
    rw_trend <- RW()
    expect_equal(rw_trend$stanvar_generator, "generate_rw_trend_stanvars")
    expect_equal(rw_trend$monitor_generator, "generate_rw_monitor_params")
    expect_equal(rw_trend$forecast_metadata$function_name, "forecast_rw_rcpp")
    
    # Test AR gets correct dispatch metadata
    ar_trend <- AR()
    expect_equal(ar_trend$stanvar_generator, "generate_ar_trend_stanvars")
    expect_equal(ar_trend$monitor_generator, "generate_ar_monitor_params")
    expect_equal(ar_trend$forecast_metadata$function_name, "forecast_ar_rcpp")
  })
})

test_that("trend dispatch consistency validation works", {
  # Create trend with consistent naming
  consistent_trend <- structure(
    list(
      trend = "AR",
      forecast_metadata = list(function_name = "forecast_ar_rcpp")
    ),
    class = "mvgam_trend"
  )
  expect_silent(validate_trend_dispatch_consistency(consistent_trend))
  
  # Create trend with inconsistent naming
  inconsistent_trend <- structure(
    list(
      trend = "AR",
      forecast_metadata = list(function_name = "forecast_wrong_rcpp")
    ),
    class = "mvgam_trend"
  )
  expect_error(
    validate_trend_dispatch_consistency(inconsistent_trend),
    "Inconsistent forecast function naming"
  )
})

test_that("get_trend_dispatch_function generates correct names", {
  # Test stanvar generator names
  expect_equal(get_trend_dispatch_function("AR", "stanvar"), "generate_ar_trend_stanvars")
  expect_equal(get_trend_dispatch_function("RW", "stanvar"), "generate_rw_trend_stanvars")
  expect_equal(get_trend_dispatch_function("VAR", "stanvar"), "generate_var_trend_stanvars")
  
  # Test forecast function names
  expect_equal(get_trend_dispatch_function("AR", "forecast"), "forecast_ar_rcpp")
  expect_equal(get_trend_dispatch_function("RW", "forecast"), "forecast_rw_rcpp")
  expect_equal(get_trend_dispatch_function("VAR", "forecast"), "forecast_var_rcpp")
  
  # Test monitor generator names
  expect_equal(get_trend_dispatch_function("AR", "monitor"), "generate_ar_monitor_params")
  expect_equal(get_trend_dispatch_function("RW", "monitor"), "generate_rw_monitor_params")
  expect_equal(get_trend_dispatch_function("VAR", "monitor"), "generate_var_monitor_params")
})

test_that("create_mvgam_trend handles all parameters consistently", {
  suppressWarnings({
    # Test that all parameters use dot-prefix convention
    # Use a valid trend type (RW) for this test
    trend_obj <- create_mvgam_trend(
      "RW",
      .time = quote(month),
      .series = quote(site),
      .gr = quote(region),
      .subgr = quote(species),
      custom_param = 42
    )
    
    expect_equal(trend_obj$trend, "RW")
    expect_equal(trend_obj$time, "month")
    expect_equal(trend_obj$series, "site")
    expect_equal(trend_obj$gr, "region")
    expect_equal(trend_obj$subgr, "species")
    expect_equal(trend_obj$custom_param, 42)
  })
})

