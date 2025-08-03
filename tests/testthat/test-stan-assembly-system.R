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
  
  # Test RW generator (non-factor model, no n_lv specified)
  rw_spec <- list(
    trend_type = "RW",
    correlation = FALSE
  )
  
  rw_stanvars <- mvgam:::generate_trend_injection_stanvars(rw_spec, data_info)
  expect_type(rw_stanvars, "list")
  expect_gt(length(rw_stanvars), 0)
  
  # Test VAR generator (for valid factor model, need n_series > n_lv)
  data_info_multivariate <- list(n_obs = 50, n_series = 3, series_var = "series")
  var_spec <- list(
    trend_type = "VAR",
    n_lv = 2,  # Valid factor model: n_lv < n_series (2 < 3)
    lags = 1
  )
  
  var_stanvars <- mvgam:::generate_trend_injection_stanvars(var_spec, data_info_multivariate)
  expect_type(var_stanvars, "list")
  expect_gt(length(var_stanvars), 0)
})

test_that("trend generators handle different specifications", {
  # Use multivariate data for factor model tests
  data_info_factor <- list(n_obs = 50, n_series = 4, series_var = "series")
  
  # Test correlated RW with valid factor model
  rw_corr_spec <- list(
    trend_type = "RW",
    n_lv = 3,  # Valid factor model: n_lv < n_series (3 < 4)
    correlation = TRUE
  )
  
  rw_corr_stanvars <- mvgam:::generate_trend_injection_stanvars(rw_corr_spec, data_info_factor)
  expect_type(rw_corr_stanvars, "list")
  expect_gt(length(rw_corr_stanvars), 0)
  
  # Test AR generator with valid factor model
  ar_spec <- list(
    trend_type = "AR",
    n_lv = 2,  # Valid factor model: n_lv < n_series (2 < 4)
    lags = 2
  )
  
  ar_stanvars <- mvgam:::generate_trend_injection_stanvars(ar_spec, data_info_factor)
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

# Comprehensive tests for all trend types and new features
test_that("CAR trend generator works correctly", {
  data_info <- list(n_obs = 50, n_series = 3, series_var = "series")
  
  # Test simple CAR (factor models not supported for CAR)
  car_spec <- list(
    trend_type = "CAR",
    n_lv = 3
  )
  car_stanvars <- mvgam:::generate_trend_injection_stanvars(car_spec, data_info)
  expect_type(car_stanvars, "list")
  expect_gt(length(car_stanvars), 0)
  expect_true(any(grepl("car", names(car_stanvars), ignore.case = TRUE)))
  
  # Test hierarchical CAR
  car_hierarchical_spec <- list(
    trend_type = "CAR",
    n_lv = 3,
    gr = "group",
    unit = "site"
  )
  car_hier_stanvars <- mvgam:::generate_trend_injection_stanvars(car_hierarchical_spec, 
    c(data_info, list(n_groups = 2, n_subgroups = 3)))
  expect_type(car_hier_stanvars, "list")
  expect_gt(length(car_hier_stanvars), 0)
  expect_true(any(grepl("hierarchical", names(car_hier_stanvars), ignore.case = TRUE)))
})

test_that("ZMVN trend generator works correctly", {
  data_info <- list(n_obs = 50, n_series = 4, series_var = "series")
  
  # Test simple ZMVN
  zmvn_spec <- list(
    trend_type = "ZMVN",
    n_lv = 2
  )
  zmvn_stanvars <- mvgam:::generate_trend_injection_stanvars(zmvn_spec, data_info)
  expect_type(zmvn_stanvars, "list")
  expect_gt(length(zmvn_stanvars), 0)
  expect_true(any(grepl("zmvn", names(zmvn_stanvars), ignore.case = TRUE)))
  
  # Test hierarchical ZMVN
  zmvn_hierarchical_spec <- list(
    trend_type = "ZMVN",
    n_lv = 2,
    gr = "group",
    unit = "site"
  )
  zmvn_hier_stanvars <- mvgam:::generate_trend_injection_stanvars(zmvn_hierarchical_spec, 
    c(data_info, list(n_groups = 3, n_subgroups = 2)))
  expect_type(zmvn_hier_stanvars, "list")
  expect_gt(length(zmvn_hier_stanvars), 0)
  expect_true(any(grepl("hierarchical", names(zmvn_hier_stanvars), ignore.case = TRUE)))
})

test_that("PW trend generator works correctly", {
  data_info <- list(n_obs = 50, n_series = 2, series_var = "series")
  
  # Test simple PW 
  pw_spec <- list(
    trend_type = "PW",
    n_lv = 2,
    n_changepoints = 2
  )
  pw_stanvars <- mvgam:::generate_trend_injection_stanvars(pw_spec, data_info)
  expect_type(pw_stanvars, "list")
  expect_gt(length(pw_stanvars), 0)
  expect_true(any(grepl("pw", names(pw_stanvars), ignore.case = TRUE)))
})

test_that("Factor model support works for all compatible trends", {
  # Test with n_lv < n_series (valid factor model)
  data_info <- list(n_obs = 50, n_series = 4, series_var = "series")
  
  # Test AR with factor model
  ar_factor_spec <- list(
    trend_type = "AR",
    n_lv = 2,  # n_lv < n_series
    lags = 1
  )
  ar_factor_stanvars <- mvgam:::generate_trend_injection_stanvars(ar_factor_spec, data_info)
  expect_type(ar_factor_stanvars, "list")
  expect_gt(length(ar_factor_stanvars), 0)
  expect_true(any(grepl("z_matrix", names(ar_factor_stanvars), ignore.case = TRUE)))
  
  # Test RW with factor model
  rw_factor_spec <- list(
    trend_type = "RW",
    n_lv = 3,  # n_lv < n_series
    correlation = TRUE
  )
  rw_factor_stanvars <- mvgam:::generate_trend_injection_stanvars(rw_factor_spec, data_info)
  expect_type(rw_factor_stanvars, "list")
  expect_gt(length(rw_factor_stanvars), 0)
  expect_true(any(grepl("z_matrix", names(rw_factor_stanvars), ignore.case = TRUE)))
  
  # Test VAR with factor model
  var_factor_spec <- list(
    trend_type = "VAR",
    n_lv = 2,  # n_lv < n_series
    lags = 1
  )
  var_factor_stanvars <- mvgam:::generate_trend_injection_stanvars(var_factor_spec, data_info)
  expect_type(var_factor_stanvars, "list")
  expect_gt(length(var_factor_stanvars), 0)
  expect_true(any(grepl("z_matrix", names(var_factor_stanvars), ignore.case = TRUE)))
  
  # Test ZMVN with factor model
  zmvn_factor_spec <- list(
    trend_type = "ZMVN", 
    n_lv = 3   # n_lv < n_series
  )
  zmvn_factor_stanvars <- mvgam:::generate_trend_injection_stanvars(zmvn_factor_spec, data_info)
  expect_type(zmvn_factor_stanvars, "list")
  expect_gt(length(zmvn_factor_stanvars), 0)
  expect_true(any(grepl("z_matrix", names(zmvn_factor_stanvars), ignore.case = TRUE)))
})

test_that("Hierarchical correlation support works for all trends", {
  data_info <- list(n_obs = 50, n_series = 4, series_var = "series", 
                   n_groups = 2, n_subgroups = 3)
  
  # Test AR with hierarchical correlations (valid factor model)
  ar_hier_spec <- list(
    trend_type = "AR",
    n_lv = 3,  # Valid factor model: n_lv < n_series (3 < 4)
    gr = "group",
    unit = "site"
  )
  ar_hier_stanvars <- mvgam:::generate_trend_injection_stanvars(ar_hier_spec, data_info)
  expect_type(ar_hier_stanvars, "list")
  expect_gt(length(ar_hier_stanvars), 0)
  expect_true(any(grepl("hierarchical", names(ar_hier_stanvars), ignore.case = TRUE)))
  
  # Test VAR with hierarchical correlations
  var_hier_spec <- list(
    trend_type = "VAR",
    n_lv = 3,
    gr = "group", 
    unit = "site"
  )
  var_hier_stanvars <- mvgam:::generate_trend_injection_stanvars(var_hier_spec, data_info)
  expect_type(var_hier_stanvars, "list")
  expect_gt(length(var_hier_stanvars), 0)
  expect_true(any(grepl("hierarchical", names(var_hier_stanvars), ignore.case = TRUE)))
})

test_that("Universal trend computation pattern is used", {
  data_info <- list(n_obs = 50, n_series = 4, series_var = "series")
  
  trend_types <- c("AR", "RW", "VAR", "ZMVN")
  
  for (trend_type in trend_types) {
    spec <- list(
      trend_type = trend_type,
      n_lv = 3  # Valid factor model: n_lv < n_series (3 < 4)
    )
    stanvars <- mvgam:::generate_trend_injection_stanvars(spec, data_info)
    expect_type(stanvars, "list")
    expect_gt(length(stanvars), 0)
    
    # Check for universal trend computation pattern
    expect_true(any(grepl("trend_computation", names(stanvars), ignore.case = TRUE)))
  }
})

test_that("Shared utility functions work correctly", {
  # Test matrix Z generation
  z_factor <- mvgam:::generate_matrix_z_stanvars(TRUE, 2, 4)
  expect_type(z_factor, "list")
  expect_gt(length(z_factor), 0)
  
  z_non_factor <- mvgam:::generate_matrix_z_stanvars(FALSE, 3, 3) 
  expect_type(z_non_factor, "list")
  expect_gt(length(z_non_factor), 0)
  
  # Test trend computation generation
  trend_comp <- mvgam:::generate_trend_computation_code(2, 3)
  expect_type(trend_comp, "list")
  expect_gt(length(trend_comp), 0)
  
  # Test factor model priors
  factor_priors <- mvgam:::generate_factor_model_priors(TRUE, 2)
  expect_type(factor_priors, "list")
  expect_gt(length(factor_priors), 0)
  
  # Test hierarchical functions
  hier_funcs <- mvgam:::generate_hierarchical_functions()
  expect_type(hier_funcs, "list")
  expect_gt(length(hier_funcs), 0)
  
  hier_params <- mvgam:::generate_hierarchical_correlation_params(2, 3)
  expect_type(hier_params, "list")
  expect_gt(length(hier_params), 0)
  
  hier_priors <- mvgam:::generate_hierarchical_correlation_priors(2)
  expect_type(hier_priors, "list")
  expect_gt(length(hier_priors), 0)
})