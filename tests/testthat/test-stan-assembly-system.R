# Stan Assembly System Tests
# Tests for the two-stage Stan code assembly with validation

library(testthat)
library(mvgam)

# Test data setup helpers
setup_test_data <- function() {
  set.seed(123)
  n <- 50
  
  # Simple univariate time series
  simple_univariate <- data.frame(
    time = 1:n,
    series = factor("series1"),
    y = rnorm(n),
    x = rnorm(n)
  )
  
  # Multivariate time series
  multivariate <- data.frame(
    time = rep(1:(n/2), 2),
    series = factor(rep(c("series1", "series2"), each = n/2)),
    y = rnorm(n),
    x = rnorm(n)
  )
  
  list(
    simple_univariate = simple_univariate,
    multivariate = multivariate
  )
}

# Tests for brms setup function
test_that("setup_brms_lightweight creates valid brms setup", {
  data <- setup_test_data()$simple_univariate
  
  # Test basic observation model setup
  obs_setup <- mvgam:::setup_brms_lightweight(
    formula = y ~ x,
    data = data,
    family = gaussian()
  )
  
  expect_type(obs_setup, "list")
  expect_true("stancode" %in% names(obs_setup))
  expect_true("standata" %in% names(obs_setup))
  expect_type(obs_setup$stancode, "character")
  expect_type(obs_setup$standata, "list")
  expect_gt(nchar(obs_setup$stancode), 50)
})

test_that("setup_brms_lightweight handles different families", {
  data <- setup_test_data()$simple_univariate
  data$y_count <- rpois(nrow(data), 5)  # Count data
  
  # Test Poisson family
  pois_setup <- mvgam:::setup_brms_lightweight(
    formula = y_count ~ x,
    data = data,
    family = poisson()
  )
  
  expect_type(pois_setup, "list")
  expect_type(pois_setup$stancode, "character")
  expect_match(pois_setup$stancode, "poisson")
})

# Tests for trend registry system
test_that("trend registry basic functionality works", {
  # Test custom trend registration
  mock_generator <- function(trend_spec, data_info) {
    list(test_stanvar = "mock content")
  }
  
  mvgam:::register_trend_type("TEST_TREND", 
                             supports_factors = TRUE, 
                             mock_generator)
  
  # Test retrieval
  trend_info <- mvgam:::get_trend_info("TEST_TREND")
  expect_type(trend_info, "list")
  expect_true(trend_info$supports_factors)
  expect_identical(trend_info$generator, mock_generator)
  
  # Test listing
  available_trends <- mvgam:::list_trend_types()
  expect_true("TEST_TREND" %in% available_trends$trend_type)
})

test_that("factor model compatibility validation works", {
  # Test with factor-compatible trends (should be silent)
  compatible_trend_spec <- list(
    trend_model = "RW",
    n_lv = 2
  )
  
  expect_silent({
    mvgam:::validate_factor_compatibility(compatible_trend_spec)
  })
  
  # Test with factor-incompatible trends (should error)
  incompatible_trend_spec <- list(
    trend_model = "CAR",
    n_lv = 2
  )
  
  expect_error({
    mvgam:::validate_factor_compatibility(incompatible_trend_spec)
  }, regexp = "Factor models.*not supported")
  
  # Test without n_lv (should be silent)
  no_factor_trend_spec <- list(
    trend_model = "CAR"
  )
  
  expect_silent({
    mvgam:::validate_factor_compatibility(no_factor_trend_spec)
  })
})

# Tests for trend injection generators
test_that("trend injection generators produce valid output", {
  data_info <- list(
    n_obs = 50,
    n_series = 1,
    series_var = "series"
  )
  
  # Test RW generator
  rw_spec <- list(
    trend_type = "RW",
    n_lv = 1,
    correlation = FALSE
  )
  
  rw_stanvars <- mvgam:::generate_trend_injection_stanvars(rw_spec, data_info)
  expect_type(rw_stanvars, "list")
  expect_gt(length(rw_stanvars), 0)
  
  # Test VAR generator
  var_spec <- list(
    trend_type = "VAR",
    n_lv = 2,
    lags = 1
  )
  
  var_stanvars <- mvgam:::generate_trend_injection_stanvars(var_spec, data_info)
  expect_type(var_stanvars, "list")
  expect_gt(length(var_stanvars), 0)
})

test_that("trend generators handle different specifications", {
  data_info <- list(n_obs = 50, n_series = 1, series_var = "series")
  
  # Test correlated RW
  rw_corr_spec <- list(
    trend_type = "RW",
    n_lv = 3,
    correlation = TRUE
  )
  
  rw_corr_stanvars <- mvgam:::generate_trend_injection_stanvars(rw_corr_spec, data_info)
  expect_type(rw_corr_stanvars, "list")
  expect_gt(length(rw_corr_stanvars), 0)
  
  # Test AR generator
  ar_spec <- list(
    trend_type = "AR",
    n_lv = 2,
    lags = 2
  )
  
  ar_stanvars <- mvgam:::generate_trend_injection_stanvars(ar_spec, data_info)
  expect_type(ar_stanvars, "list")
  expect_gt(length(ar_stanvars), 0)
})

test_that("trend generators handle edge cases", {
  data_info <- list(n_obs = 10, n_series = 1, series_var = "series")
  
  # Test with None trend
  none_spec <- list(
    trend_type = "None"
  )
  
  none_stanvars <- mvgam:::generate_trend_injection_stanvars(none_spec, data_info)
  expect_type(none_stanvars, "list")
  # Should return empty list for None trend
  expect_equal(length(none_stanvars), 0)
  
  # Test with minimal specification
  minimal_spec <- list(
    trend_type = "RW"
  )
  
  minimal_stanvars <- mvgam:::generate_trend_injection_stanvars(minimal_spec, data_info)
  expect_type(minimal_stanvars, "list")
  expect_gt(length(minimal_stanvars), 0)
})

# Tests for Stan code generation pipeline
test_that("extract_trend_stanvars_from_setup handles valid inputs", {
  # Mock trend setup
  trend_setup <- list(
    stancode = "mock trend code",
    standata = list(n = 50)
  )
  
  # Mock trend spec
  trend_spec <- list(
    trend_type = "RW",
    n_lv = 1
  )
  
  result <- mvgam:::extract_trend_stanvars_from_setup(trend_setup, trend_spec)
  expect_type(result, "list")
  expect_gt(length(result), 0)
})

test_that("extract_trend_stanvars_from_setup handles None trend", {
  trend_setup <- list(
    stancode = "mock code",
    standata = list()
  )
  
  trend_spec <- list(
    trend_type = "None"
  )
  
  result <- mvgam:::extract_trend_stanvars_from_setup(trend_setup, trend_spec)
  expect_type(result, "list")
  expect_equal(length(result), 0)
})

# Tests for combine_stan_components
test_that("combine_stan_components merges observation and trend code", {
  obs_code <- "
  data {
    int<lower=1> N;
    vector[N] y;
  }
  parameters {
    real alpha;
  }
  model {
    y ~ normal(alpha, 1);
  }
  "
  
  obs_data <- list(N = 50, y = rnorm(50))
  
  # Mock trend stanvars (empty for simple test)
  trend_stanvars <- list()
  
  result <- mvgam:::combine_stan_components(obs_code, obs_data, trend_stanvars)
  
  expect_type(result, "list")
  expect_true("stancode" %in% names(result))
  expect_true("standata" %in% names(result))
  expect_true("has_trends" %in% names(result))
  expect_type(result$stancode, "character")
  expect_type(result$standata, "list")
  expect_type(result$has_trends, "logical")
  expect_false(result$has_trends)  # No trends added
})

test_that("combine_stan_components handles trend addition", {
  obs_code <- "
  data {
    int<lower=1> N;
  }
  parameters {
    real alpha;
  }
  model {
    alpha ~ normal(0, 1);
  }
  "
  
  obs_data <- list(N = 50)
  
  # Mock trend stanvars with simple additions
  trend_stanvars <- list(
    trend_params = structure(list(
      name = "trend_params",
      scode = "
      parameters {
        real trend_param;
      }
      ",
      block = "parameters"
    ), class = "stanvar")
  )
  
  result <- mvgam:::combine_stan_components(obs_code, obs_data, trend_stanvars)
  
  expect_type(result, "list")
  expect_true(result$has_trends)
  expect_match(result$stancode, "trend_param")
})

# Tests for Stan code validation
test_that("validate_stan_syntax catches syntax errors", {
  # Valid Stan code
  valid_code <- "
  data {
    int<lower=1> N;
  }
  parameters {
    real alpha;
  }
  model {
    alpha ~ normal(0, 1);
  }
  "
  
  expect_true(mvgam:::validate_stan_syntax(valid_code, silent = TRUE))
  
  # Invalid Stan code (missing semicolon)
  invalid_code <- "
  data {
    int<lower=1> N
  }
  parameters {
    real alpha;
  }
  model {
    alpha ~ normal(0, 1);
  }
  "
  
  expect_false(mvgam:::validate_stan_syntax(invalid_code, silent = TRUE))
})

test_that("validate_stan_syntax handles edge cases", {
  # Empty code
  expect_false(mvgam:::validate_stan_syntax("", silent = TRUE))
  
  # Very minimal valid code
  minimal_code <- "
  parameters {
    real x;
  }
  model {
    x ~ normal(0, 1);
  }
  "
  
  expect_true(mvgam:::validate_stan_syntax(minimal_code, silent = TRUE))
})

# Tests for comprehensive validation
test_that("validate_combined_stancode performs comprehensive checks", {
  data <- setup_test_data()$simple_univariate
  
  # Create a complete, valid stancode structure
  complete_result <- list(
    stancode = "
    data {
      int<lower=1> N;
      vector[N] y;
    }
    parameters {
      real alpha;
      real<lower=0> sigma;
    }
    model {
      y ~ normal(alpha, sigma);
      alpha ~ normal(0, 1);
      sigma ~ student_t(3, 0, 2.5);
    }
    ",
    standata = list(
      N = nrow(data),
      y = data$y
    ),
    has_trends = FALSE
  )
  
  validation_result <- mvgam:::validate_combined_stancode(complete_result, silent = TRUE)
  expect_type(validation_result, "list")
  expect_true("valid" %in% names(validation_result))
  expect_true("syntax_valid" %in% names(validation_result))
  expect_true("data_valid" %in% names(validation_result))
})

test_that("validate_combined_stancode catches data mismatches", {
  # Stan code expecting different data structure
  mismatched_result <- list(
    stancode = "
    data {
      int<lower=1> N;
      vector[N] y;
      vector[N] x;  // Expected but not provided
    }
    parameters {
      real alpha;
    }
    model {
      y ~ normal(alpha, 1);
    }
    ",
    standata = list(
      N = 50,
      y = rnorm(50)
      # x is missing
    ),
    has_trends = FALSE
  )
  
  validation_result <- mvgam:::validate_combined_stancode(mismatched_result, silent = TRUE)
  expect_false(validation_result$data_valid)
})

# Tests for production generate_combined_stancode function
test_that("production generate_combined_stancode works with observation-only model", {
  data <- setup_test_data()$simple_univariate
  
  # Create observation setup only
  obs_setup <- mvgam:::setup_brms_lightweight(
    formula = y ~ x,
    data = data,
    family = gaussian()
  )
  
  # Test without trends
  combined_result <- mvgam:::generate_combined_stancode(
    obs_setup = obs_setup,
    trend_setup = NULL,
    trend_spec = NULL,
    validate = FALSE,  # Skip validation to focus on generation
    silent = 2
  )
  
  # Basic structure validation
  expect_type(combined_result, "list")
  expect_false(combined_result$has_trends)
  expect_type(combined_result$stancode, "character")
  expect_gt(nchar(combined_result$stancode), 100)
  expect_match(combined_result$stancode, "model\\s*\\{")
})

test_that("production generate_combined_stancode works with trend models", {
  data <- setup_test_data()$simple_univariate
  
  # Create observation setup
  obs_setup <- mvgam:::setup_brms_lightweight(
    formula = y ~ x,
    data = data,
    family = gaussian()
  )
  
  # Create trend setup (simplified trend model)
  trend_setup <- mvgam:::setup_brms_lightweight(
    formula = ~ 1,  # Minimal trend formula
    data = data,
    family = gaussian()
  )
  
  # Create basic trend specification
  trend_spec <- list(
    trend_type = "RW",  # Fixed: was trend_model, now trend_type
    time_var = "time",
    series_var = "series",
    n_series = 1
  )
  
  # Test production function with both observation and trend components
  combined_result <- mvgam:::generate_combined_stancode(
    obs_setup = obs_setup,
    trend_setup = trend_setup,
    trend_spec = trend_spec,
    validate = FALSE,  # Skip validation to focus on generation
    silent = 2
  )
  
  # Validate structure with trends
  expect_type(combined_result, "list")
  expect_true(combined_result$has_trends)
  expect_type(combined_result$stancode, "character")
  expect_gt(nchar(combined_result$stancode), 100)
  expect_type(combined_result$standata, "list")
})

# Tests for production Stan code validation
test_that("production Stan code validation works", {
  data <- setup_test_data()$simple_univariate
  
  # Generate valid Stan code using production pipeline
  obs_setup <- mvgam:::setup_brms_lightweight(
    formula = y ~ x,
    data = data,
    family = gaussian()
  )
  
  combined_result <- mvgam:::generate_combined_stancode(
    obs_setup = obs_setup,
    trend_setup = NULL,
    trend_spec = NULL,
    validate = FALSE,
    silent = 2
  )
  
  # Validate the generated code
  validation_result <- mvgam:::validate_combined_stancode(combined_result, silent = TRUE)
  
  expect_type(validation_result, "list")
  expect_true(validation_result$valid)
  expect_true(validation_result$syntax_valid)
  expect_true(validation_result$data_valid)
})

test_that("production validation catches real issues", {
  # Create intentionally broken setup
  broken_setup <- list(
    stancode = "
    data {
      int<lower=1> N;
      vector[N] y;
      vector[N] missing_var;  // This will cause data validation to fail
    }
    parameters {
      real alpha;
    }
    model {
      y ~ normal(alpha, 1);
    }
    ",
    standata = list(
      N = 50,
      y = rnorm(50)
      # missing_var not provided
    )
  )
  
  broken_result <- list(
    stancode = broken_setup$stancode,
    standata = broken_setup$standata,
    has_trends = FALSE
  )
  
  validation_result <- mvgam:::validate_combined_stancode(broken_result, silent = TRUE)
  expect_false(validation_result$valid)
  expect_false(validation_result$data_valid)
})

# Tests for error handling and edge cases
test_that("system handles missing components gracefully", {
  data <- setup_test_data()$simple_univariate
  
  # Test with NULL observation setup
  expect_error({
    mvgam:::generate_combined_stancode(
      obs_setup = NULL,
      trend_setup = NULL, 
      trend_spec = NULL
    )
  })
  
  # Test with invalid trend specification
  obs_setup <- mvgam:::setup_brms_lightweight(
    formula = y ~ x,
    data = data,
    family = gaussian()
  )
  
  invalid_trend_spec <- list(
    trend_type = "NONEXISTENT_TREND"
  )
  
  expect_error({
    mvgam:::generate_combined_stancode(
      obs_setup = obs_setup,
      trend_setup = NULL,
      trend_spec = invalid_trend_spec,
      validate = FALSE
    )
  })
})

test_that("system provides informative error messages", {
  data <- setup_test_data()$simple_univariate
  obs_setup <- mvgam:::setup_brms_lightweight(
    formula = y ~ x,
    data = data,
    family = gaussian()
  )
  
  # Test error message quality
  expect_error({
    mvgam:::generate_combined_stancode(
      obs_setup = obs_setup,
      trend_spec = list(trend_type = "FAKE_TREND"),
      validate = FALSE
    )
  }, regexp = "Unknown trend type")
})

# Integration tests
test_that("full pipeline integration works end-to-end", {
  data <- setup_test_data()$multivariate
  
  # Test complete pipeline with multivariate data
  obs_setup <- mvgam:::setup_brms_lightweight(
    formula = y ~ x,
    data = data,
    family = gaussian()
  )
  
  trend_setup <- mvgam:::setup_brms_lightweight(
    formula = ~ 1,
    data = data,
    family = gaussian()
  )
  
  trend_spec <- list(
    trend_type = "VAR",
    n_lv = 2,
    lags = 1,
    time_var = "time",
    series_var = "series"
  )
  
  # Full pipeline with validation
  final_result <- mvgam:::generate_combined_stancode(
    obs_setup = obs_setup,
    trend_setup = trend_setup,
    trend_spec = trend_spec,
    validate = TRUE,
    silent = 1
  )
  
  expect_type(final_result, "list")
  expect_true(final_result$has_trends)
  expect_true(final_result$validation_passed)
  expect_type(final_result$stancode, "character")
  expect_type(final_result$standata, "list")
  expect_gt(nchar(final_result$stancode), 200)  # Should be substantial code
})

test_that("system handles complex trend specifications", {
  data <- setup_test_data()$multivariate
  
  obs_setup <- mvgam:::setup_brms_lightweight(
    formula = y ~ x,
    data = data,
    family = poisson()  # Different family
  )
  
  trend_setup <- mvgam:::setup_brms_lightweight(
    formula = ~ 1,
    data = data,
    family = gaussian()
  )
  
  # Complex trend specification
  complex_trend_spec <- list(
    trend_type = "RW",
    n_lv = 3,
    correlation = TRUE,  # Correlated random walk
    time_var = "time",
    series_var = "series"
  )
  
  complex_result <- mvgam:::generate_combined_stancode(
    obs_setup = obs_setup,
    trend_setup = trend_setup,
    trend_spec = complex_trend_spec,
    validate = TRUE,
    silent = 2
  )
  
  expect_type(complex_result, "list")
  expect_true(complex_result$has_trends)
  expect_true(complex_result$validation_passed)
  
  # Should contain correlation-specific code
  expect_match(complex_result$stancode, "(cholesky|correlation|L_Omega)")
})