# Stan Assembly System Tests
# ===========================
# Tests for two-stage Stan code assembly system that combines brms-generated
# Stan code with trend stanvars to create complete mvgam models.

# Test Data Setup
# ===============
setup_test_data <- function() {
  list(
    simple_univariate = data.frame(
      y = rnorm(30, 0, 1),
      x = rnorm(30, 0, 1),
      time = 1:30,
      series = factor(rep("series1", 30))
    ),
    multivariate = data.frame(
      y = rnorm(60, 0, 1),
      x = rnorm(60, 0, 1),
      time = rep(1:30, 2),
      series = factor(rep(c("series1", "series2"), each = 30))
    ),
    distributional = data.frame(
      y = rnorm(30, 0, 1),
      x = rnorm(30, 0, 1),
      z = rnorm(30, 0, 1),
      time = 1:30,
      series = factor(rep("series1", 30))
    )
  )
}

# Test 1: Registry System Functions
# =================================
test_that("trend registry system functions exist and work", {
  # Check that key registry functions exist
  expect_true(exists("register_trend_type", envir = asNamespace("mvgam")))
  expect_true(exists("register_custom_trend", envir = asNamespace("mvgam")))
  expect_true(exists("get_trend_info", envir = asNamespace("mvgam")))
  expect_true(exists("list_trend_types", envir = asNamespace("mvgam")))
  
  # Test basic registry functionality
  test_generator <- function(trend_spec, data_info) {
    list(test_stanvar = list(
      name = "test_injection",
      scode = "// test injection code"
    ))
  }
  
  # Register a test trend type
  expect_silent({
    mvgam:::register_custom_trend(
      name = "TEST_TREND",
      supports_factors = TRUE,
      generator_func = test_generator
    )
  })
  
  # Verify it was registered correctly
  trend_info <- mvgam:::get_trend_info("TEST_TREND")
  expect_true(trend_info$supports_factors)
  expect_identical(trend_info$generator, test_generator)
  
  # Test listing trends includes our test trend
  all_trends <- mvgam:::list_trend_types()
  expect_true("TEST_TREND" %in% all_trends$trend_type)
})

# Test 2: Core Stan Assembly Functions
# ====================================
test_that("core stan assembly functions exist", {
  # Check main assembly functions
  expect_true(exists("assemble_mvgam_stan_code", envir = asNamespace("mvgam")))
  expect_true(exists("assemble_mvgam_stan_data", envir = asNamespace("mvgam")))
  
  # Check stage-specific functions
  expect_true(exists("generate_base_brms_stancode", envir = asNamespace("mvgam")))
  expect_true(exists("generate_base_brms_standata", envir = asNamespace("mvgam")))
  
  # Check trend injection functions
  expect_true(exists("inject_trend_into_linear_predictor", envir = asNamespace("mvgam")))
  expect_true(exists("generate_trend_injection_stanvars", envir = asNamespace("mvgam")))
  
  # Check validation functions
  expect_true(exists("validate_stan_code_structure", envir = asNamespace("mvgam")))
  expect_true(exists("validate_stan_syntax", envir = asNamespace("mvgam")))
})

# Test 3: Stanvar Validation System
# =================================
test_that("stanvar validation system works", {
  # Test valid stanvar structure
  valid_stanvar <- list(
    name = "test_stanvar",
    scode = "parameters { real test_param; }"
  )
  
  expect_true(mvgam:::is_valid_stanvar(valid_stanvar))
  
  # Test invalid stanvar structures
  invalid_no_name <- list(
    scode = "parameters { real test_param; }"
  )
  expect_false(mvgam:::is_valid_stanvar(invalid_no_name))
  
  invalid_no_scode <- list(
    name = "test_stanvar"
  )
  expect_false(mvgam:::is_valid_stanvar(invalid_no_scode))
  
  invalid_empty_scode <- list(
    name = "test_stanvar",
    scode = ""
  )
  expect_false(mvgam:::is_valid_stanvar(invalid_empty_scode))
})

# Test 4: Factor Model Compatibility System
# =========================================
test_that("factor model compatibility validation works", {
  # Test with factor-compatible trends
  compatible_trend_spec <- list(
    trend_model = "RW",
    n_lv = 2
  )
  
  expect_silent({
    mvgam:::validate_factor_compatibility(compatible_trend_spec)
  })
  
  # Test with factor-incompatible trends (should error)
  incompatible_trend_spec <- list(
    trend_model = "PW",
    n_lv = 2
  )
  
  expect_error({
    mvgam:::validate_factor_compatibility(incompatible_trend_spec)
  }, regexp = "Factor models.*not supported")
  
  # Test without n_lv (should be silent)
  no_factor_trend_spec <- list(
    trend_model = "PW"
  )
  
  expect_silent({
    mvgam:::validate_factor_compatibility(no_factor_trend_spec)
  })
})

# Test 5: Stan Code Structure Validation
# ======================================
test_that("stan code structure validation works", {
  # Valid Stan code with required blocks
  valid_stan_code <- "
  data {
    int<lower=0> N;
  }
  parameters {
    real mu;
  }
  model {
    mu ~ normal(0, 1);
  }
  "
  
  expect_silent({
    mvgam:::validate_stan_code_structure(valid_stan_code)
  })
  
  # Missing required block should error
  invalid_stan_code <- "
  parameters {
    real mu;
  }
  model {
    mu ~ normal(0, 1);
  }
  "
  
  expect_error({
    mvgam:::validate_stan_code_structure(invalid_stan_code)
  }, regexp = "Missing required Stan block")
})

# Test 6: Stan Syntax Validation
# ==============================
test_that("stan syntax validation works", {
  # Balanced braces should pass
  balanced_code <- "{ { } }"
  expect_true(mvgam:::are_braces_balanced(balanced_code))
  
  # Unbalanced braces should fail
  unbalanced_code <- "{ { }"
  expect_false(mvgam:::are_braces_balanced(unbalanced_code))
  
  # Stan syntax validation should catch unbalanced braces
  expect_error({
    mvgam:::validate_stan_syntax(unbalanced_code)
  }, regexp = "Unbalanced braces")
})

# Test 7: Data Component Detection
# ================================
test_that("data component detection works", {
  # Test time component detection
  time_stanvars <- list(
    time_stanvar = list(
      name = "time_test",
      scode = "data { int n_time; vector[n_time] time_vals; }"
    )
  )
  
  expect_true(mvgam:::has_time_component(time_stanvars))
  
  # Test series component detection
  series_stanvars <- list(
    series_stanvar = list(
      name = "series_test",
      scode = "data { int n_series; }"
    )
  )
  
  expect_true(mvgam:::has_series_component(series_stanvars))
  
  # Test correlation component detection
  corr_stanvars <- list(
    corr_stanvar = list(
      name = "corr_test",
      scode = "parameters { cov_matrix[3] Sigma; }"
    )
  )
  
  expect_true(mvgam:::has_correlation_component(corr_stanvars))
  
  # Test stanvars without special components
  basic_stanvars <- list(
    basic_stanvar = list(
      name = "basic_test",
      scode = "parameters { real basic_param; }"
    )
  )
  
  expect_false(mvgam:::has_time_component(basic_stanvars))
  expect_false(mvgam:::has_series_component(basic_stanvars))
  expect_false(mvgam:::has_correlation_component(basic_stanvars))
})

# Test 8: Data Extraction Functions
# =================================
test_that("data extraction functions work", {
  test_data <- setup_test_data()
  
  # Test time data extraction
  time_data <- mvgam:::extract_time_data(test_data$simple_univariate)
  expect_named(time_data, c("time", "n_time"))
  expect_equal(time_data$time, 1:30)
  expect_equal(time_data$n_time, 30)
  
  # Test series data extraction
  series_data <- mvgam:::extract_series_data(test_data$multivariate)
  expect_named(series_data, c("series", "n_series"))
  expect_equal(series_data$n_series, 2)
  
  # Test with missing variables
  empty_data <- data.frame(x = 1:10)
  time_data_empty <- mvgam:::extract_time_data(empty_data)
  expect_length(time_data_empty, 0)
})

# Test 9: Stanvar Preparation for brms
# ====================================
test_that("stanvar preparation for brms integration works", {
  # Valid stanvars should be preserved
  valid_stanvars <- list(
    param_stanvar = list(
      name = "test_param",
      scode = "parameters { real test_param; }"
    ),
    data_stanvar = list(
      name = "test_data",
      scode = "data { int test_n; }"
    )
  )
  
  prepared_stanvars <- mvgam:::prepare_stanvars_for_brms(valid_stanvars)
  expect_type(prepared_stanvars, "list")
  expect_length(prepared_stanvars, 2)
  
  # Mixed valid/invalid stanvars should filter invalid ones
  mixed_stanvars <- list(
    valid = list(
      name = "valid_stanvar",
      scode = "parameters { real valid_param; }"
    ),
    invalid = list(
      name = "invalid_stanvar"
      # missing scode
    )
  )
  
  expect_warning({
    prepared_mixed <- mvgam:::prepare_stanvars_for_brms(mixed_stanvars)
  }, regexp = "Skipping invalid stanvar")
  
  expect_length(prepared_mixed, 1)
  expect_named(prepared_mixed, "valid")
})

# Test 10: Stan Data Merging System
# =================================
test_that("stan data merging works correctly", {
  base_data <- list(
    N = 30,
    y = rnorm(30),
    base_param = 1
  )
  
  trend_data <- list(
    time_data = list(
      n_time = 30,
      time_vals = 1:30
    ),
    series_data = list(
      n_series = 1
    )
  )
  
  merged_data <- mvgam:::merge_stan_data(base_data, trend_data)
  
  # Should contain all base data
  expect_true(all(names(base_data) %in% names(merged_data)))
  
  # Should contain trend data components
  expect_true("n_time" %in% names(merged_data))
  expect_true("time_vals" %in% names(merged_data))
  expect_true("n_series" %in% names(merged_data))
  
  # Test conflict resolution
  conflicting_trend_data <- list(
    time_data = list(
      N = 25,  # conflicts with base_data$N
      n_time = 30
    )
  )
  
  expect_warning({
    merged_with_conflict <- mvgam:::merge_stan_data(base_data, conflicting_trend_data)
  }, regexp = "Data conflict")
  
  # Trend data should take precedence in conflicts
  expect_equal(merged_with_conflict$N, 25)
})

# Test 11: Stan Data Structure Validation
# =======================================
test_that("stan data structure validation works", {
  # Valid Stan data should pass
  valid_stan_data <- list(
    N = 30,
    y = rnorm(30),
    x = matrix(rnorm(60), ncol = 2)
  )
  
  expect_silent({
    mvgam:::validate_stan_data_structure(valid_stan_data)
  })
  
  # Empty data should error
  expect_error({
    mvgam:::validate_stan_data_structure(list())
  }, regexp = "Empty Stan data")
  
  # Invalid data types should warn
  invalid_data <- list(
    N = 30,
    bad_data = "string_value"  # Stan doesn't accept strings
  )
  
  expect_warning({
    mvgam:::validate_stan_data_structure(invalid_data)
  }, regexp = "Unexpected data type")
})

# Test 12: Stan Code Block Extraction
# ===================================
test_that("stan code block extraction works", {
  complete_stan_code <- "
  functions {
    real test_function() {
      return 1.0;
    }
  }
  data {
    int N;
  }
  parameters {
    real mu;
  }
  model {
    mu ~ normal(0, 1);
  }
  "
  
  code_lines <- strsplit(complete_stan_code, "\n")[[1]]
  
  # Test extracting different blocks
  functions_block <- mvgam:::extract_code_block(code_lines, "functions")
  expect_true(grepl("test_function", functions_block))
  
  data_block <- mvgam:::extract_code_block(code_lines, "data")
  expect_true(grepl("int N", data_block))
  
  parameters_block <- mvgam:::extract_code_block(code_lines, "parameters")
  expect_true(grepl("real mu", parameters_block))
  
  model_block <- mvgam:::extract_code_block(code_lines, "model")
  expect_true(grepl("mu ~ normal", model_block))
  
  # Test non-existent block
  missing_block <- mvgam:::extract_code_block(code_lines, "nonexistent")
  expect_null(missing_block)
})

# Test 13: Brace Matching Utility
# ===============================
test_that("brace matching utility works", {
  # Simple balanced braces
  simple_code <- c("{", "  content", "}")
  start_line <- 1
  end_line <- mvgam:::find_matching_brace(simple_code, start_line)
  expect_equal(end_line, 3)
  
  # Nested braces
  nested_code <- c("{", "  {", "    content", "  }", "}")
  end_line_nested <- mvgam:::find_matching_brace(nested_code, 1)
  expect_equal(end_line_nested, 5)
  
  # Unmatched braces
  unmatched_code <- c("{", "  content")
  end_line_unmatched <- mvgam:::find_matching_brace(unmatched_code, 1)
  expect_length(end_line_unmatched, 0)
  
  # Empty input
  empty_input <- mvgam:::find_matching_brace(character(0), integer(0))
  expect_length(empty_input, 0)
})