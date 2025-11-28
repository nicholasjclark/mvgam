# Stan Compilation Validation Tests
#
# Comprehensive test suite for systematic Stan compilation validation
# Testing Stan compilation across all registered trend types with systematic
# formula patterns and model configurations.

# Test Data Setup Functions ----

#' Create proper trend specifications using the full validation pipeline
#' @param trend_formula Trend formula (e.g., ~ AR(p = 1))
#' @param data Data frame with time series
#' @return Properly validated trend_specs with dimensions
#' @noRd
create_validated_trend_specs <- function(trend_formula, data, main_formula = y ~ 1) {
  # Use our standardized pipeline to parse trends
  mv_spec <- mvgam:::parse_multivariate_trends(main_formula, trend_formula)

  if (mv_spec$has_trends) {
    # Validate time series and inject dimensions (our critical fix!)
    validation_result <- mvgam:::validate_time_series_for_trends(data, mv_spec$trend_specs)

    # Inject dimensions using our standardized structure
    if (mvgam:::is_multivariate_trend_specs(mv_spec$trend_specs)) {
      # Multivariate: inject into each response-specific trend spec
      for (response_name in names(mv_spec$trend_specs)) {
        mv_spec$trend_specs[[response_name]]$dimensions <- validation_result$dimensions
      }
    } else {
      # Univariate: inject directly into trend object
      mv_spec$trend_specs$dimensions <- validation_result$dimensions
    }

    return(mv_spec$trend_specs)
  } else {
    return(NULL)
  }
}

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

  # Use proper pipeline to create validated trend specs with dimensions
  trend_specs <- create_validated_trend_specs(~ AR(p = 1), data)

  # Verify dimensions were properly injected
  expect_true("dimensions" %in% names(trend_specs))
  expect_equal(trend_specs$dimensions$n_obs, nrow(data))
  expect_equal(trend_specs$dimensions$n_series, 1)

  data_info <- list(
    data = data,
    n_series = trend_specs$dimensions$n_series,
    n_obs = trend_specs$dimensions$n_obs,
    time_var = trend_specs$dimensions$time_var,
    series_var = trend_specs$dimensions$series_var
  )

  # Test stanvar generation with properly validated specs
  stanvars <- mvgam:::generate_trend_injection_stanvars(trend_specs, data_info)

  # Check that the result has the correct class structure
  expect_true(inherits(stanvars, "stanvars"))
  expect_true(length(stanvars) > 0)

  # Test that stanvars work with brms
  test_code <- try({
    brms::make_stancode(
      formula = y ~ 1,
      data = data,
      family = gaussian(),
      stanvars = stanvars
    )
  }, silent = TRUE)

  expect_false(inherits(test_code, "try-error"))
  expect_type(test_code, "character")
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

  # Test proper combination using brms c() method for stanvars
  combined_proper <- c(sv1, sv2)

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

  # Use proper pipeline to create validated trend specs with dimensions
  trend_specs <- create_validated_trend_specs(~ RW(), data)

  # Verify dimensions were properly injected
  expect_true("dimensions" %in% names(trend_specs))
  expect_equal(trend_specs$dimensions$n_obs, nrow(data))

  data_info <- list(
    data = data,
    n_series = trend_specs$dimensions$n_series,
    n_obs = trend_specs$dimensions$n_obs,
    time_var = trend_specs$dimensions$time_var,
    series_var = trend_specs$dimensions$series_var
  )

  # Test stanvar generation with properly validated specs
  stanvars <- mvgam:::generate_trend_injection_stanvars(trend_specs, data_info)
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

  # Use proper pipeline to create validated trend specs with dimensions
  trend_specs <- create_validated_trend_specs(~ VAR(p = 1), data)

  # Verify dimensions were properly injected
  expect_true("dimensions" %in% names(trend_specs))
  expect_equal(trend_specs$dimensions$n_obs, nrow(data))

  data_info <- list(
    data = data,
    n_series = trend_specs$dimensions$n_series,
    n_obs = trend_specs$dimensions$n_obs,
    time_var = trend_specs$dimensions$time_var,
    series_var = trend_specs$dimensions$series_var
  )

  # Test stanvar generation with properly validated specs
  stanvars <- mvgam:::generate_trend_injection_stanvars(trend_specs, data_info)
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
  )

  expect_type(combined_code, "character")
  expect_gt(nchar(combined_code), 100)

  # Validate Stan compilation
  validation_result <- mvgam:::validate_stan_code(
    combined_code,
    backend = "rstan"
  )

  expect_type(validation_result, "character")
})

test_that("CAR trend simple intercept model compiles", {
  data <- setup_compilation_test_data()$univariate

  # Use proper pipeline to create validated trend specs with dimensions
  trend_specs <- create_validated_trend_specs(~ CAR(), data)

  # Verify dimensions were properly injected
  expect_true("dimensions" %in% names(trend_specs))
  expect_equal(trend_specs$dimensions$n_obs, nrow(data))

  data_info <- list(
    data = data,
    n_series = trend_specs$dimensions$n_series,
    n_obs = trend_specs$dimensions$n_obs,
    time_var = trend_specs$dimensions$time_var,
    series_var = trend_specs$dimensions$series_var
  )

  # Test stanvar generation with properly validated specs
  stanvars <- mvgam:::generate_trend_injection_stanvars(trend_specs, data_info)
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
  )

  expect_type(combined_code, "character")
  expect_gt(nchar(combined_code), 100)

  # Validate Stan compilation
  validation_result <- mvgam:::validate_stan_code(
    combined_code,
    backend = "rstan"
  )

  expect_type(validation_result, "character")
})

test_that("ZMVN trend simple intercept model compiles", {
  data <- setup_compilation_test_data()$univariate

  # Use proper pipeline to create validated trend specs with dimensions
  trend_specs <- create_validated_trend_specs(~ ZMVN(), data)

  # Verify dimensions were properly injected
  expect_true("dimensions" %in% names(trend_specs))
  expect_equal(trend_specs$dimensions$n_obs, nrow(data))

  data_info <- list(
    data = data,
    n_series = trend_specs$dimensions$n_series,
    n_obs = trend_specs$dimensions$n_obs,
    time_var = trend_specs$dimensions$time_var,
    series_var = trend_specs$dimensions$series_var
  )

  # Test stanvar generation with properly validated specs
  stanvars <- mvgam:::generate_trend_injection_stanvars(trend_specs, data_info)
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
  )

  expect_type(combined_code, "character")
  expect_gt(nchar(combined_code), 100)

  # Validate Stan compilation
  validation_result <- mvgam:::validate_stan_code(
    combined_code,
    backend = "rstan"
  )

  expect_type(validation_result, "character")
})

test_that("PW trend simple intercept model compiles", {
  data <- setup_compilation_test_data()$univariate

  # Use proper pipeline to create validated trend specs with dimensions
  trend_specs <- create_validated_trend_specs(~ PW(), data)

  # Verify dimensions were properly injected
  expect_true("dimensions" %in% names(trend_specs))
  expect_equal(trend_specs$dimensions$n_obs, nrow(data))

  data_info <- list(
    data = data,
    n_series = trend_specs$dimensions$n_series,
    n_obs = trend_specs$dimensions$n_obs,
    time_var = trend_specs$dimensions$time_var,
    series_var = trend_specs$dimensions$series_var
  )

  # Test stanvar generation with properly validated specs
  stanvars <- mvgam:::generate_trend_injection_stanvars(trend_specs, data_info)
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
  )

  expect_type(combined_code, "character")
  expect_gt(nchar(combined_code), 100)

  # Validate Stan compilation
  validation_result <- mvgam:::validate_stan_code(
    combined_code,
    backend = "rstan"
  )

  expect_type(validation_result, "character")
})

# Covariate Integration Testing ----

test_that("AR trend with covariates compiles", {
  data <- setup_compilation_test_data()$univariate

  trend_specs <- list(
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
  stanvars <- mvgam:::generate_trend_injection_stanvars(trend_specs, data_info)

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
  )

  validation_result <- mvgam:::validate_stan_code(
    combined_code,
    backend = "rstan"
  )

  expect_type(validation_result, "character")
})

# Family Compatibility Testing ----

test_that("AR trend with poisson family compiles", {
  data <- setup_compilation_test_data()$count

  trend_specs <- list(
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

  stanvars <- mvgam:::generate_trend_injection_stanvars(trend_specs, data_info)

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
  )

  validation_result <- mvgam:::validate_stan_code(
    combined_code,
    backend = "rstan"
  )

  expect_type(validation_result, "character")
})

# Factor Model Compatibility Validation ----

test_that("AR trend accepts n_lv parameter for factor models", {
  data <- setup_compilation_test_data()$multivariate

  trend_specs <- list(
    trend = "AR",
    trend_formula = ~ AR(p = 1, n_lv = 2),
    n_lv = 2
  )

  data_info <- list(
    data = data,
    n_series = 3,
    n_obs = nrow(data),
    time_var = "time",
    series_var = "series"
  )

  # Should not throw factor compatibility error
  stanvars <- mvgam:::generate_trend_injection_stanvars(trend_specs, data_info)
  expect_true(inherits(stanvars, "stanvars"))

  # Test that stanvars work with brms and contain factor model components
  obs_setup <- mvgam:::setup_brms_lightweight(
    formula = y ~ 1,
    data = data,
    family = gaussian(),
    backend = "rstan"
  )

  # Generate Stan code to check for matrix Z
  combined_code <- mvgam:::generate_base_stancode_with_stanvars(
    obs_setup,
    stanvars,
    backend = "rstan",
  )

  # Check that matrix Z is in parameters block (factor model indicator)
  expect_true(grepl("parameters\\s*\\{[^}]*matrix\\[n_series, n_lv\\] Z;", combined_code))
})

test_that("CAR trend rejects n_lv parameter", {
  data <- setup_compilation_test_data()$multivariate

  trend_specs <- list(
    trend = "CAR",
    trend_formula = ~ CAR(n_lv = 2),
    n_lv = 2
  )

  # Should throw factor compatibility error
  expect_error({
    mvgam:::validate_factor_compatibility(trend_specs)
  }, "Factor models.*not supported")
})

test_that("PW trend rejects n_lv parameter", {
  data <- setup_compilation_test_data()$multivariate

  trend_specs <- list(
    trend = "PW",
    trend_formula = ~ PW(n_lv = 2),
    n_lv = 2
  )

  # Should throw factor compatibility error
  expect_error({
    mvgam:::validate_factor_compatibility(trend_specs)
  }, "Factor models.*not supported")
})

# Hierarchical Correlation Testing ----

test_that("AR trend supports hierarchical correlations", {
  data <- setup_compilation_test_data()$multivariate

  # Add grouping variable for hierarchical correlations
  data$group <- factor(rep(c("A", "B"), length.out = nrow(data)))

  trend_specs <- list(
    trend = "AR",
    trend_formula = ~ AR(p = 1, gr = group),
    n_lv = NULL,
    gr = "group",
    n_groups = 2,
    n_subgroups = 3
  )

  data_info <- list(
    data = data,
    n_series = 3,
    n_obs = nrow(data),
    time_var = "time",
    series_var = "series",
    n_groups = 2,
    n_subgroups = 3
  )

  stanvars <- mvgam:::generate_trend_injection_stanvars(trend_specs, data_info)
  expect_true(inherits(stanvars, "stanvars"))

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
  )

  # Check for hierarchical correlation components in generated code
  expect_true(grepl("L_Omega_global", combined_code))
  expect_true(grepl("alpha_cor_trend", combined_code))

  validation_result <- mvgam:::validate_stan_code(
    combined_code,
    backend = "rstan"
  )

  expect_type(validation_result, "character")
})

test_that("RW trend ignores grouping parameter", {
  data <- setup_compilation_test_data()$multivariate
  data$group <- factor(rep(c("A", "B"), length.out = nrow(data)))

  # RW doesn't support hierarchical correlations - should warn
  expect_warning({
    trend_specs <- list(
      trend = "RW",
      trend_formula = ~ RW(gr = group),
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

    stanvars <- mvgam:::generate_trend_injection_stanvars(trend_specs, data_info)
    expect_true(inherits(stanvars, "stanvars"))

    # Generate Stan code to verify no hierarchical components
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
      )

    # Should not contain hierarchical correlation components
    expect_false(grepl("L_Omega_global", combined_code))
    expect_false(grepl("alpha_cor_trend", combined_code))

  }, "correlations.*ignored")
})


# Tests for Complete Formula-to-Stan Pipeline with Dimension Tracking
# ===================================================================

test_that("complete pipeline tracks dimensions from validation to stanvar generation", {
  # Setup test data
  data <- data.frame(
    time = rep(1:30, 3),
    series = factor(rep(c("A", "B", "C"), each = 30)),
    y = rnorm(90),
    x1 = rnorm(90),
    x2 = runif(90)
  )

  # Test 1: Dimensions flow through parse_multivariate_trends
  mv_spec <- mvgam:::parse_multivariate_trends(
    formula = y ~ x1 + x2,
    trend_formula = ~ RW(time = time, series = series)
  )

  expect_true(mv_spec$has_trends)
  expect_type(mv_spec$trend_specs, "list")

  # Test 2: Dimensions calculated and attached during validation
  validation_result <- mvgam:::validate_time_series_for_trends(
    data = data,
    trend_specs = mv_spec$trend_specs
  )

  expect_type(validation_result, "list")
  expect_true("data" %in% names(validation_result))
  expect_true("dimensions" %in% names(validation_result))
  expect_equal(validation_result$dimensions$n_time, 30)
  expect_equal(validation_result$dimensions$n_series, 3)
  expect_equal(validation_result$dimensions$n_obs, 90)

  # Test 3: Dimensions properly injected into trend_specs
  mv_spec$trend_specs$dimensions <- validation_result$dimensions

  # Test 4: Stanvar generation uses pre-calculated dimensions
  obs_setup <- mvgam:::setup_brms_lightweight(
    formula = y ~ x1 + x2,
    data = data,
    family = gaussian()
  )

  trend_setup <- mvgam:::setup_brms_lightweight(
    formula = ~ 1,
    data = data,
    family = gaussian()
  )

  # This should NOT fail with missing n_obs error
  trend_stanvars <- mvgam:::extract_trend_stanvars_from_setup(
    trend_setup = trend_setup,
    trend_specs = mv_spec$trend_specs  # Now has dimensions
  )

  expect_type(trend_stanvars, "list")
  expect_true(inherits(trend_stanvars, "stanvars"))

  # Test 5: Complete generation works
  combined_result <- mvgam:::generate_combined_stancode(
    obs_setup = obs_setup,
    trend_setup = trend_setup,
    trend_specs = mv_spec$trend_specs,
    validate = FALSE,
    silent = 2
  )

  expect_true(combined_result$has_trends)
  expect_type(combined_result$stancode, "character")
  expect_type(combined_result$standata, "list")
})

test_that("pipeline handles multivariate models with response-specific trends", {
  data <- data.frame(
    time = rep(1:25, 2),
    series = factor(rep(c("S1", "S2"), each = 25)),
    count = rpois(50, 10),
    biomass = rnorm(50, 100, 10),
    temp = rnorm(50)
  )

  # Parse multivariate formula with response-specific trends
  # Note: observation formula uses brms::bf() for multivariate responses
  formula <- brms::bf(count ~ temp, biomass ~ temp)

  # IMPORTANT: trend_formula for response-specific trends must be a named list
  # The names must match the response variable names from the observation formula
  # bf() cannot be used here because it doesn't support the named argument syntax
  trend_formula <- list(
    count = ~ AR(p = 1, time = time, series = series),
    biomass = ~ RW(time = time, series = series)
  )

  mv_spec <- mvgam:::parse_multivariate_trends(formula, trend_formula)

  expect_true(mv_spec$has_trends)
  expect_type(mv_spec$trend_specs, "list")
  expect_equal(names(mv_spec$trend_specs), c("count", "biomass"))

  # Validate each response's time series structure
  for (resp_name in names(mv_spec$trend_specs)) {
    resp_data <- data  # In practice, might subset
    resp_validation <- mvgam:::validate_time_series_for_trends(
      data = resp_data,
      trend_specs = mv_spec$trend_specs[[resp_name]]
    )

    # Inject dimensions
    mv_spec$trend_specs[[resp_name]]$dimensions <- resp_validation$dimensions

    expect_equal(resp_validation$dimensions$n_time, 25)
    expect_equal(resp_validation$dimensions$n_series, 2)
  }

  # Setup brms models
  obs_setup <- mvgam:::setup_brms_lightweight(
    formula = formula,
    data = data,
    family = list(poisson(), gaussian())
  )

  trend_setup <- mvgam:::setup_brms_lightweight(
    formula = ~ 1,
    data = data,
    family = gaussian()
  )

  # Generate combined stancode with multivariate trends
  combined_result <- mvgam:::generate_combined_stancode(
    obs_setup = obs_setup,
    trend_setup = trend_setup,
    trend_specs = mv_spec$trend_specs,
    validate = FALSE,
    silent = 2
  )

  expect_true(combined_result$has_trends)
  expect_true(combined_result$is_multivariate)
  expect_equal(sort(combined_result$responses_with_trends), c("biomass", "count"))

  # Check for response-specific parameters in Stan code
  expect_match(combined_result$stancode, "mu_trend_count")
  expect_match(combined_result$stancode, "mu_trend_biomass")
})

test_that("pipeline correctly handles CAR trends with irregular time intervals", {
  # CAR allows irregular time intervals
  data <- data.frame(
    time = c(1, 2.5, 4, 7, 10, 1.5, 3, 5.5, 8, 11),
    series = factor(rep(c("A", "B"), each = 5)),
    y = rnorm(10),
    x = rnorm(10)
  )

  mv_spec <- mvgam:::parse_multivariate_trends(
    formula = y ~ x,
    trend_formula = ~ CAR(time = time, series = series)
  )

  # CAR should skip regular interval validation but still get dimensions
  validation_result <- mvgam:::validate_time_series_for_trends(
    data = data,
    trend_specs = mv_spec$trend_specs
  )

  expect_type(validation_result$dimensions, "list")
  expect_equal(validation_result$dimensions$n_series, 2)
  expect_equal(validation_result$dimensions$n_obs, 10)

  # Inject dimensions
  mv_spec$trend_specs$dimensions <- validation_result$dimensions

  # Setup and generate
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

  # CAR stanvar generation should calculate time distances
  trend_stanvars <- mvgam:::extract_trend_stanvars_from_setup(
    trend_setup = trend_setup,
    trend_specs = mv_spec$trend_specs
  )

  expect_type(trend_stanvars, "list")

  # Complete generation
  combined_result <- mvgam:::generate_combined_stancode(
    obs_setup = obs_setup,
    trend_setup = trend_setup,
    trend_specs = mv_spec$trend_specs,
    validate = FALSE,
    silent = 2
  )

  expect_true(combined_result$has_trends)
  expect_match(combined_result$stancode, "car_rho")
  expect_true("time_dis" %in% names(combined_result$standata))
})

test_that("pipeline fails gracefully when dimensions are missing", {
  data <- data.frame(
    time = 1:20,
    series = factor("A"),
    y = rnorm(20)
  )

  # Create trend_specs without dimensions (simulating the bug)
  trend_specs_no_dims <- list(
    trend = "RW",
    time = "time",
    series = "series"
    # Missing dimensions field
  )

  obs_setup <- mvgam:::setup_brms_lightweight(
    formula = y ~ 1,
    data = data,
    family = gaussian()
  )

  trend_setup <- mvgam:::setup_brms_lightweight(
    formula = ~ 1,
    data = data,
    family = gaussian()
  )

  # This should fail with informative error
  expect_error({
    mvgam:::extract_trend_stanvars_from_setup(
      trend_setup = trend_setup,
      trend_specs = trend_specs_no_dims
    )
  }, "dimensions|n_obs|n_time")
})

test_that("pipeline handles factor models with proper dimension validation", {
  data <- data.frame(
    time = rep(1:20, 5),
    series = factor(rep(LETTERS[1:5], each = 20)),
    y = rnorm(100),
    x = rnorm(100)
  )

  # Valid factor model: n_lv < n_series
  mv_spec <- mvgam:::parse_multivariate_trends(
    formula = y ~ x,
    trend_formula = ~ AR(p = 1, n_lv = 3, time = time, series = series)
  )

  validation_result <- mvgam:::validate_time_series_for_trends(
    data = data,
    trend_specs = mv_spec$trend_specs
  )

  expect_equal(validation_result$dimensions$n_series, 5)
  expect_equal(validation_result$dimensions$n_time, 20)

  # Check n_lv < n_series constraint
  expect_true(mv_spec$trend_specs$n_lv < validation_result$dimensions$n_series)

  # Inject dimensions
  mv_spec$trend_specs$dimensions <- validation_result$dimensions

  # Generate stanvars
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

  trend_stanvars <- mvgam:::extract_trend_stanvars_from_setup(
    trend_setup = trend_setup,
    trend_specs = mv_spec$trend_specs
  )

  # Should contain factor model components
  expect_true(any(grepl("^Z$", names(trend_stanvars))))

  # Complete generation
  combined_result <- mvgam:::generate_combined_stancode(
    obs_setup = obs_setup,
    trend_setup = trend_setup,
    trend_specs = mv_spec$trend_specs,
    validate = FALSE,
    silent = 2
  )

  # Check for factor model indicators in Stan code
  expect_match(combined_result$stancode, "matrix\\[.*\\] Z")
  expect_match(combined_result$stancode, "to_vector\\(Z\\) ~ normal\\(0, 1\\)")
})
