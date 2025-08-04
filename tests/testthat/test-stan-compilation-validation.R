# Stan Compilation Validation Tests
# 
# Comprehensive test suite for systematic Stan compilation validation
# Testing Stan compilation across all registered trend types with systematic
# formula patterns and model configurations.

# Test Data Setup Functions ----

#' Create consistent test datasets for Stan compilation validation
#' @noRd
setup_compilation_test_data <- function() {
  set.seed(42)  # Consistent seed for reproducible tests
  n_time <- 20
  n_series <- 3
  
  # Simple univariate time series
  univariate_data <- data.frame(
    time = 1:n_time,
    series = factor(rep("series1", n_time)),
    y = rnorm(n_time, mean = 10, sd = 2),
    x1 = rnorm(n_time),
    x2 = runif(n_time, -1, 1),
    group = factor(rep(c("A", "B"), length.out = n_time))
  )
  
  # Multivariate time series
  multivariate_data <- data.frame(
    time = rep(1:n_time, n_series),
    series = factor(rep(paste0("series", 1:n_series), each = n_time)),
    y = rnorm(n_time * n_series, mean = 10, sd = 2),
    x1 = rnorm(n_time * n_series),
    x2 = runif(n_time * n_series, -1, 1),
    group = factor(rep(c("A", "B", "C"), length.out = n_time * n_series))
  )
  
  # Data with different response types
  count_data <- univariate_data
  count_data$y <- rpois(n_time, lambda = 5)
  
  binary_data <- univariate_data  
  binary_data$y <- rbinom(n_time, size = 1, prob = 0.3)
  
  list(
    univariate = univariate_data,
    multivariate = multivariate_data,
    count = count_data,
    binary = binary_data
  )
}

#' Get all registered trend types for systematic testing
#' @noRd
get_test_trend_types <- function() {
  # Ensure registry is initialized
  mvgam:::ensure_registry_initialized()
  
  # Get all registered trend types
  trend_info <- mvgam:::list_trend_types()
  
  # Remove "None" as it's not a real trend for compilation testing
  trend_types <- trend_info$trend_type[trend_info$trend_type != "None"]
  
  list(
    all_types = trend_types,
    factor_compatible = trend_info$trend_type[trend_info$supports_factors & trend_info$trend_type != "None"],
    factor_incompatible = trend_info$trend_type[!trend_info$supports_factors & trend_info$trend_type != "None"]
  )
}

# Core Trend Type Validation ----

test_that("AR trend stanvar generation works", {
  data <- setup_compilation_test_data()$univariate
  
  # Generate trend stanvars
  trend_spec <- list(
    trend = "AR",
    trend_formula = ~ AR(p = 1),
    n_lv = NULL
  )
  
  data_info <- list(
    data = data,
    n_series = 1,
    n_obs = nrow(data),
    time_var = "time",
    series_var = "series"
  )
  
  # Test stanvar generation
  stanvars <- mvgam:::generate_trend_injection_stanvars(trend_spec, data_info)
  expect_type(stanvars, "list")
  expect_true(length(stanvars) > 0)
  
  # Verify stanvar structure based on debug output
  expect_equal(length(stanvars), 6)
  expected_names <- c("n_lv_data", "n_series_data", "z_matrix_diagonal", "ar_params", "ar_model", "trend_computation")
  expect_true(all(expected_names %in% names(stanvars)))
  
  # Check that stanvars have the correct class structure
  for (i in seq_along(stanvars)) {
    expect_true(inherits(stanvars[[i]], "stanvars") || inherits(stanvars[[i]], "stanvar"))
  }
})

test_that("brms lightweight setup works", {
  data <- setup_compilation_test_data()$univariate
  
  # Test basic brms setup
  obs_setup <- mvgam:::setup_brms_lightweight(
    formula = y ~ 1,
    data = data,
    family = gaussian(),
    backend = "rstan"
  )
  
  expect_type(obs_setup, "list")
  expect_true("stancode" %in% names(obs_setup))
  expect_true("standata" %in% names(obs_setup))
  expect_type(obs_setup$stancode, "character")
  expect_type(obs_setup$standata, "list")
})

test_that("test stanvar combination logic", {
  data <- setup_compilation_test_data()$univariate
  
  # Test creating individual stanvars with brms
  sv1 <- brms::stanvar(x = 1, name = "test1", scode = "int<lower=1> test1;", block = "data")
  sv2 <- brms::stanvar(x = 2, name = "test2", scode = "int<lower=1> test2;", block = "data")
  
  cat("Individual stanvar classes:\n")
  cat("sv1:", paste(class(sv1), collapse = ", "), "\n")
  cat("sv2:", paste(class(sv2), collapse = ", "), "\n")
  
  # Test proper combination using brms c() method for stanvars
  combined_proper <- c(sv1, sv2)
  cat("Properly combined class:", paste(class(combined_proper), collapse = ", "), "\n")
  
  # Test if properly combined stanvars work with brms
  test_proper <- try({
    brms::make_stancode(
      formula = y ~ 1,
      data = data,
      family = gaussian(),
      stanvars = combined_proper
    )
  }, silent = TRUE)
  
  expect_false(inherits(test_proper, "try-error"))
})

test_that("RW trend simple intercept model compiles", {
  data <- setup_compilation_test_data()$univariate
  
  # Generate trend stanvars
  trend_spec <- list(
    trend = "RW",
    trend_formula = ~ RW(),
    n_lv = NULL
  )
  
  data_info <- list(
    data = data,
    n_series = 1,
    n_obs = nrow(data),
    time_var = "time",
    series_var = "series"
  )
  
  # Test stanvar generation
  stanvars <- mvgam:::generate_trend_injection_stanvars(trend_spec, data_info)
  expect_type(stanvars, "list")
  expect_true(length(stanvars) > 0)
  
  # Test basic Stan code assembly
  obs_setup <- mvgam:::setup_brms_lightweight(
    formula = y ~ 1,
    data = data,
    family = gaussian(),
    backend = "rstan"
  )
  
  # Generate combined Stan code
  combined_code <- mvgam:::generate_base_stancode_with_stanvars(
    obs_setup, 
    stanvars, 
    backend = "rstan",
    silent = 2
  )
  
  expect_type(combined_code, "character")
  expect_gt(nchar(combined_code), 100)
  
  # Validate Stan compilation
  validation_result <- mvgam:::validate_stan_code(
    combined_code, 
    backend = "rstan", 
    silent = TRUE
  )
  
  expect_type(validation_result, "character")
})

test_that("VAR trend simple intercept model compiles", {
  data <- setup_compilation_test_data()$univariate
  
  # Generate trend stanvars
  trend_spec <- list(
    trend = "VAR",
    trend_formula = ~ VAR(p = 1),
    n_lv = NULL
  )
  
  data_info <- list(
    data = data,
    n_series = 1,
    n_obs = nrow(data),
    time_var = "time",
    series_var = "series"
  )
  
  # Test stanvar generation
  stanvars <- mvgam:::generate_trend_injection_stanvars(trend_spec, data_info)
  expect_type(stanvars, "list")
  expect_true(length(stanvars) > 0)
  
  # Test basic Stan code assembly
  obs_setup <- mvgam:::setup_brms_lightweight(
    formula = y ~ 1,
    data = data,
    family = gaussian(),
    backend = "rstan"
  )
  
  # Generate combined Stan code
  combined_code <- mvgam:::generate_base_stancode_with_stanvars(
    obs_setup, 
    stanvars, 
    backend = "rstan",
    silent = 2
  )
  
  expect_type(combined_code, "character")
  expect_gt(nchar(combined_code), 100)
  
  # Validate Stan compilation
  validation_result <- mvgam:::validate_stan_code(
    combined_code, 
    backend = "rstan", 
    silent = TRUE
  )
  
  expect_type(validation_result, "character")
})

test_that("CAR trend simple intercept model compiles", {
  data <- setup_compilation_test_data()$univariate
  
  # Generate trend stanvars
  trend_spec <- list(
    trend = "CAR",
    trend_formula = ~ CAR(),
    n_lv = NULL
  )
  
  data_info <- list(
    data = data,
    n_series = 1,
    n_obs = nrow(data),
    time_var = "time",
    series_var = "series"
  )
  
  # Test stanvar generation
  stanvars <- mvgam:::generate_trend_injection_stanvars(trend_spec, data_info)
  expect_type(stanvars, "list")
  expect_true(length(stanvars) > 0)
  
  # Test basic Stan code assembly
  obs_setup <- mvgam:::setup_brms_lightweight(
    formula = y ~ 1,
    data = data,
    family = gaussian(),
    backend = "rstan"
  )
  
  # Generate combined Stan code
  combined_code <- mvgam:::generate_base_stancode_with_stanvars(
    obs_setup, 
    stanvars, 
    backend = "rstan",
    silent = 2
  )
  
  expect_type(combined_code, "character")
  expect_gt(nchar(combined_code), 100)
  
  # Validate Stan compilation
  validation_result <- mvgam:::validate_stan_code(
    combined_code, 
    backend = "rstan", 
    silent = TRUE
  )
  
  expect_type(validation_result, "character")
})

test_that("ZMVN trend simple intercept model compiles", {
  data <- setup_compilation_test_data()$univariate
  
  # Generate trend stanvars
  trend_spec <- list(
    trend = "ZMVN",
    trend_formula = ~ ZMVN(),
    n_lv = NULL
  )
  
  data_info <- list(
    data = data,
    n_series = 1,
    n_obs = nrow(data),
    time_var = "time",
    series_var = "series"
  )
  
  # Test stanvar generation
  stanvars <- mvgam:::generate_trend_injection_stanvars(trend_spec, data_info)
  expect_type(stanvars, "list")
  expect_true(length(stanvars) > 0)
  
  # Test basic Stan code assembly
  obs_setup <- mvgam:::setup_brms_lightweight(
    formula = y ~ 1,
    data = data,
    family = gaussian(),
    backend = "rstan"
  )
  
  # Generate combined Stan code
  combined_code <- mvgam:::generate_base_stancode_with_stanvars(
    obs_setup, 
    stanvars, 
    backend = "rstan",
    silent = 2
  )
  
  expect_type(combined_code, "character")
  expect_gt(nchar(combined_code), 100)
  
  # Validate Stan compilation
  validation_result <- mvgam:::validate_stan_code(
    combined_code, 
    backend = "rstan", 
    silent = TRUE
  )
  
  expect_type(validation_result, "character")
})

test_that("PW trend simple intercept model compiles", {
  data <- setup_compilation_test_data()$univariate
  
  # Generate trend stanvars
  trend_spec <- list(
    trend = "PW",
    trend_formula = ~ PW(),
    n_lv = NULL
  )
  
  data_info <- list(
    data = data,
    n_series = 1,
    n_obs = nrow(data),
    time_var = "time",
    series_var = "series"
  )
  
  # Test stanvar generation
  stanvars <- mvgam:::generate_trend_injection_stanvars(trend_spec, data_info)
  expect_type(stanvars, "list")
  expect_true(length(stanvars) > 0)
  
  # Test basic Stan code assembly
  obs_setup <- mvgam:::setup_brms_lightweight(
    formula = y ~ 1,
    data = data,
    family = gaussian(),
    backend = "rstan"
  )
  
  # Generate combined Stan code
  combined_code <- mvgam:::generate_base_stancode_with_stanvars(
    obs_setup, 
    stanvars, 
    backend = "rstan",
    silent = 2
  )
  
  expect_type(combined_code, "character")
  expect_gt(nchar(combined_code), 100)
  
  # Validate Stan compilation
  validation_result <- mvgam:::validate_stan_code(
    combined_code, 
    backend = "rstan", 
    silent = TRUE
  )
  
  expect_type(validation_result, "character")
})

# Covariate Integration Testing ----

test_that("AR trend with covariates compiles", {
  data <- setup_compilation_test_data()$univariate
  
  trend_spec <- list(
    trend = "AR",
    trend_formula = ~ AR(p = 1),
    n_lv = NULL
  )
  
  data_info <- list(
    data = data,
    n_series = 1,
    n_obs = nrow(data),
    time_var = "time",
    series_var = "series"
  )
  
  # Generate stanvars
  stanvars <- mvgam:::generate_trend_injection_stanvars(trend_spec, data_info)
  
  # Test with covariates in observation model
  obs_setup <- mvgam:::setup_brms_lightweight(
    formula = y ~ x1 + x2,
    data = data,
    family = gaussian(),
    backend = "rstan"
  )
  
  # Generate and validate Stan code
  combined_code <- mvgam:::generate_base_stancode_with_stanvars(
    obs_setup, 
    stanvars, 
    backend = "rstan",
    silent = 2
  )
  
  validation_result <- mvgam:::validate_stan_code(
    combined_code, 
    backend = "rstan", 
    silent = TRUE
  )
  
  expect_type(validation_result, "character")
})

# Family Compatibility Testing ----

test_that("AR trend with poisson family compiles", {
  data <- setup_compilation_test_data()$count
  
  trend_spec <- list(
    trend = "AR",
    trend_formula = ~ AR(p = 1),
    n_lv = NULL
  )
  
  data_info <- list(
    data = data,
    n_series = 1,
    n_obs = nrow(data),
    time_var = "time",
    series_var = "series"
  )
  
  stanvars <- mvgam:::generate_trend_injection_stanvars(trend_spec, data_info)
  
  obs_setup <- mvgam:::setup_brms_lightweight(
    formula = y ~ 1,
    data = data,
    family = poisson(),
    backend = "rstan"
  )
  
  combined_code <- mvgam:::generate_base_stancode_with_stanvars(
    obs_setup, 
    stanvars, 
    backend = "rstan",
    silent = 2
  )
  
  validation_result <- mvgam:::validate_stan_code(
    combined_code, 
    backend = "rstan", 
    silent = TRUE
  )
  
  expect_type(validation_result, "character")
})

# Factor Model Compatibility Validation ----

test_that("AR trend accepts n_lv parameter for factor models", {
  data <- setup_compilation_test_data()$multivariate
  
  trend_spec <- list(
    trend = "AR",
    trend_formula = ~ AR(p = 1, n_lv = 2),
    n_lv = 2
  )
  
  data_info <- list(
    data = data,
    n_series = 3,
    time_var = "time", 
    series_var = "series"
  )
  
  # Should not throw factor compatibility error
  stanvars <- mvgam:::generate_trend_injection_stanvars(trend_spec, data_info)
  expect_type(stanvars, "list")
  
  # Check that matrix Z is estimated (factor model)
  z_stanvars <- stanvars[grepl("matrix.*Z", names(stanvars))]
  expect_true(length(z_stanvars) > 0)
})

test_that("CAR trend rejects n_lv parameter", {
  data <- setup_compilation_test_data()$multivariate
  
  trend_spec <- list(
    trend = "CAR",
    trend_formula = ~ CAR(n_lv = 2),
    n_lv = 2
  )
  
  # Should throw factor compatibility error
  expect_error({
    mvgam:::validate_factor_compatibility(trend_spec)
  }, "Factor models.*not supported")
})

test_that("PW trend rejects n_lv parameter", {
  data <- setup_compilation_test_data()$multivariate
  
  trend_spec <- list(
    trend = "PW",
    trend_formula = ~ PW(n_lv = 2),
    n_lv = 2
  )
  
  # Should throw factor compatibility error
  expect_error({
    mvgam:::validate_factor_compatibility(trend_spec)
  }, "Factor models.*not supported")
})

# Hierarchical Correlation Testing ----

test_that("AR trend supports hierarchical correlations", {
  data <- setup_compilation_test_data()$multivariate
  
  # Add grouping variable for hierarchical correlations
  data$group <- factor(rep(c("A", "B"), length.out = nrow(data)))
  
  trend_spec <- list(
    trend = "AR",
    trend_formula = ~ AR(p = 1, gr = group),
    n_lv = NULL,
    gr = "group"
  )
  
  data_info <- list(
    data = data,
    n_series = 3,
    n_obs = nrow(data),
    time_var = "time",
    series_var = "series"
  )
  
  stanvars <- mvgam:::generate_trend_injection_stanvars(trend_spec, data_info)
  expect_type(stanvars, "list")
  
  # Check for hierarchical correlation components
  hierarchical_stanvars <- stanvars[grepl("hierarchical|L_Omega|alpha_cor", names(stanvars))]
  expect_true(length(hierarchical_stanvars) > 0)
  
  # Test Stan code compilation
  obs_setup <- mvgam:::setup_brms_lightweight(
    formula = y ~ 1,
    data = data,
    family = gaussian(),
    backend = "rstan"
  )
  
  combined_code <- mvgam:::generate_base_stancode_with_stanvars(
    obs_setup, 
    stanvars, 
    backend = "rstan",
    silent = 2
  )
  
  validation_result <- mvgam:::validate_stan_code(
    combined_code, 
    backend = "rstan", 
    silent = TRUE
  )
  
  expect_type(validation_result, "character")
})

test_that("RW trend ignores grouping parameter", {
  data <- setup_compilation_test_data()$multivariate
  data$group <- factor(rep(c("A", "B"), length.out = nrow(data)))
  
  expect_warning({
    trend_spec <- list(
      trend = "RW",
      trend_formula = ~ RW(gr = group),
      n_lv = NULL,
      gr = "group"
    )
    
    data_info <- list(
      data = data,
      n_series = 3,
      time_var = "time",
      series_var = "series"
    )
    
    stanvars <- mvgam:::generate_trend_injection_stanvars(trend_spec, data_info)
    
    # Should not contain hierarchical correlation components
    hierarchical_stanvars <- stanvars[grepl("hierarchical|L_Omega|alpha_cor", names(stanvars))]
    expect_equal(length(hierarchical_stanvars), 0)
    
  }, "correlations.*ignored")
})