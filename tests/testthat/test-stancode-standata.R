# stancode and standata Tests
#
# Comprehensive integrated tests for stancode.mvgam_formula() and
# standata.mvgam_formula() following brms testing patterns.
# Tests cover model specification validation, Stan code generation,
# data preparation, class assignment, and integration with trend systems.

# Test Data Setup ----

#' Create standardized test datasets for stancode/standata function testing
#' @noRd
setup_stan_test_data <- function() {
  set.seed(42)  # Reproducible tests
  n_time <- 24
  n_series <- 3

  # Simple univariate dataset
  univariate <- data.frame(
    time = 1:n_time,
    series = factor(rep("series1", n_time)),
    y = rpois(n_time, lambda = 5),
    x = rnorm(n_time),
    temperature = rnorm(n_time, mean = 15, sd = 3)
  )

  # Multivariate dataset with balanced design
  multivariate <- data.frame(
    time = rep(1:n_time, n_series),
    series = factor(rep(paste0("series", 1:n_series), each = n_time)),
    count = rpois(n_time * n_series, lambda = 4),
    biomass = rlnorm(n_time * n_series, meanlog = 1, sdlog = 0.5),
    presence = rbinom(n_time * n_series, size = 1, prob = 0.7),
    x = rnorm(n_time * n_series),
    habitat = factor(sample(c("forest", "grassland"), n_time * n_series, replace = TRUE))
  )

  # Dataset with missing values
  with_missings <- univariate
  with_missings$y[c(3, 7, 15)] <- NA

  list(
    univariate = univariate,
    multivariate = multivariate,
    with_missings = with_missings
  )
}

# Helper function for string pattern matching
expect_match2 <- function(object, regexp, ...) {
  expect_true(grepl(regexp, object, ...))
}

# stancode Tests ----

test_that("stancode.mvgam_formula returns correct class structure", {
  data <- setup_stan_test_data()$univariate

  # Simple observation-only model
  mf_obs_only <- mvgam_formula(y ~ x)
  code_obs_only <- stancode(mf_obs_only, data = data, family = poisson(), validate = FALSE)

  # Check class structure follows mvgam convention with brms compatibility
  expect_s3_class(code_obs_only, "mvgamstancode")
  expect_s3_class(code_obs_only, "stancode")
  expect_s3_class(code_obs_only, "character")
  expect_equal(class(code_obs_only), c("mvgamstancode", "stancode", "character"))

  # Model with trends - generate without validation first
  mf_with_trend <- mvgam_formula(y ~ x, trend_formula = ~ RW())
  code_with_trend <- stancode(mf_with_trend, data = data, family = poisson(), validate = FALSE)

  # Should have same class structure
  expect_s3_class(code_with_trend, "mvgamstancode")
  expect_s3_class(code_with_trend, "stancode")
  expect_equal(class(code_with_trend), c("mvgamstancode", "stancode", "character"))

  # Should be longer than observation-only model
  expect_gt(nchar(code_with_trend), nchar(code_obs_only))
  
  # Final validation: ensure both models compile correctly
  expect_no_error(stancode(mf_obs_only, data = data, family = poisson(), validate = TRUE))
  expect_no_error(stancode(mf_with_trend, data = data, family = poisson(), validate = TRUE))
})

test_that("stancode handles different observation families", {
  data <- setup_stan_test_data()$univariate
  mf <- mvgam_formula(y ~ x, trend_formula = ~ RW())

  # Test major families
  families_to_test <- list(
    poisson = poisson(),
    gaussian = gaussian(),
    binomial = binomial()
  )

  for (fam_name in names(families_to_test)) {
    family <- families_to_test[[fam_name]]

    # Adjust data for family
    test_data <- data
    if (fam_name == "binomial") {
      test_data$y <- rbinom(nrow(data), size = 10, prob = 0.3)
    } else if (fam_name == "gaussian") {
      test_data$y <- rnorm(nrow(data))
    }

    code <- stancode(mf, data = test_data, family = family)

    # Basic validation
    expect_s3_class(code, "mvgamstancode")
    expect_s3_class(code, "stancode")
    expect_gt(nchar(code), 200)

    # Should contain family-specific elements
    if (fam_name == "poisson") {
      expect_match2(code, "poisson")
    } else if (fam_name == "gaussian") {
      expect_match2(code, "normal")
    } else if (fam_name == "binomial") {
      expect_match2(code, "binomial")
    }
  }
})

test_that("stancode generates correct Stan blocks", {
  data <- setup_stan_test_data()$univariate
  mf <- mvgam_formula(y ~ s(x), trend_formula = ~ AR(p = 1))
  
  # Generate Stan code without validation for structure inspection
  code <- stancode(mf, data = data, family = poisson(), validate = FALSE)

  # Each Stan block should appear exactly once
  expect_equal(length(gregexpr("data\\s*\\{", code)[[1]]), 1)
  expect_equal(length(gregexpr("parameters\\s*\\{", code)[[1]]), 1)
  expect_equal(length(gregexpr("transformed parameters\\s*\\{", code)[[1]]), 1)
  expect_equal(length(gregexpr("model\\s*\\{", code)[[1]]), 1)
  expect_equal(length(gregexpr("generated quantities\\s*\\{", code)[[1]]), 1)

  # Stan blocks should be in correct order
  data_pos <- regexpr("data\\s*\\{", code)
  param_pos <- regexpr("parameters\\s*\\{", code)  
  tp_pos <- regexpr("transformed parameters\\s*\\{", code)
  model_pos <- regexpr("model\\s*\\{", code)
  gq_pos <- regexpr("generated quantities\\s*\\{", code)
  
  expect_true(data_pos < param_pos)
  expect_true(param_pos < tp_pos)
  expect_true(tp_pos < model_pos)
  expect_true(model_pos < gq_pos)

  # Should have exactly one lprior declaration (not duplicated)
  lprior_decls <- gregexpr("real\\s+lprior\\s*=\\s*0;", code)[[1]]
  expect_equal(length(lprior_decls), 1)
  
  # Required variable declarations should be present
  expect_match2(code, "vector\\[N\\]\\s+mu")
  expect_match2(code, "vector\\[.*\\]\\s+mu_trend")
  expect_match2(code, "matrix\\[.*\\]\\s+trend;")
  
  # Trend parameters should be declared in parameters block
  expect_match2(code, "real.*ar1_trend")
  expect_match2(code, "vector<lower=0>\\[.*\\]\\s+sigma_trend")

  # Data block should contain mapping arrays
  expect_match2(code, "array\\[N\\]\\s+int\\s+obs_trend_time")
  expect_match2(code, "array\\[N\\]\\s+int\\s+obs_trend_series")

  # Trend injection should use correct pattern
  expect_match2(code, "mu\\[n\\]\\s*\\+=\\s*trend\\[obs_trend_time\\[n\\],\\s*obs_trend_series\\[n\\]\\]")
  expect_false(grepl("obs_ind", code))

  # Universal trend computation pattern should be present
  expect_match2(code, "for\\s*\\(\\s*i\\s+in\\s+1:n_trend\\s*\\)")
  expect_match2(code, "trend\\[i,\\s*s\\]\\s*=.*dot_product")

  # Should contain sigma_trend prior but not duplicate sigma prior
  expect_match2(code, "sigma_trend\\s*~")
  
  # All braces should be properly matched
  open_braces <- length(gregexpr("\\{", code)[[1]])
  close_braces <- length(gregexpr("\\}", code)[[1]])
  expect_equal(open_braces, close_braces)
  
  # Generated Stan code should compile without errors
  expect_no_error(stancode(mf, data = data, family = poisson(), validate = TRUE))
})

test_that("stancode handles multivariate specifications", {
  data <- setup_stan_test_data()$multivariate

  # Multivariate with shared trend
  mf_shared <- mvgam_formula(
    mvbind(count, biomass) ~ x,
    trend_formula = ~ RW(cor = TRUE)
  )
  
  # Generate without validation first for structure inspection
  code_shared <- stancode(mf_shared, data = data, validate = FALSE)

  expect_s3_class(code_shared, "stancode")
  expect_gt(nchar(code_shared), 500)

  # Check for exactly one of each Stan block (no duplicates)
  expect_equal(length(gregexpr("data\\s*\\{", code_shared)[[1]]), 1, info = "Should have exactly one data block")
  expect_equal(length(gregexpr("parameters\\s*\\{", code_shared)[[1]]), 1, info = "Should have exactly one parameters block") 
  expect_equal(length(gregexpr("transformed parameters\\s*\\{", code_shared)[[1]]), 1, info = "Should have exactly one transformed parameters block")
  expect_equal(length(gregexpr("model\\s*\\{", code_shared)[[1]]), 1, info = "Should have exactly one model block")
  expect_equal(length(gregexpr("generated quantities\\s*\\{", code_shared)[[1]]), 1, info = "Should have exactly one generated quantities block")

  # Should contain multivariate elements
  expect_match2(code_shared, "count")
  expect_match2(code_shared, "biomass")
  expect_match2(code_shared, "L_Omega_trend")  # Correlation matrix

  # Multivariate mapping functionality: should have response-specific arrays
  expect_match2(code_shared, "obs_trend_time_count")
  expect_match2(code_shared, "obs_trend_series_count")
  expect_match2(code_shared, "obs_trend_time_biomass")
  expect_match2(code_shared, "obs_trend_series_biomass")
  
  # Check that response-specific arrays are declared as data arrays
  expect_match2(code_shared, "array\\[.*\\]\\s+int.*obs_trend_time_count")
  expect_match2(code_shared, "array\\[.*\\]\\s+int.*obs_trend_series_count")
  expect_match2(code_shared, "array\\[.*\\]\\s+int.*obs_trend_time_biomass")
  expect_match2(code_shared, "array\\[.*\\]\\s+int.*obs_trend_series_biomass")

  # Multivariate mapping: should have response-specific injection patterns
  expect_match2(code_shared, "mu_count\\[n\\] \\+= trend_count\\[obs_trend_time_count\\[n\\], obs_trend_series_count\\[n\\]\\]")
  expect_match2(code_shared, "mu_biomass\\[n\\] \\+= trend_biomass\\[obs_trend_time_biomass\\[n\\], obs_trend_series_biomass\\[n\\]\\]")
  
  # Should have response-specific trend matrices
  expect_match2(code_shared, "matrix.*trend_count")
  expect_match2(code_shared, "matrix.*trend_biomass")
  
  # Check proper block ordering
  tp_pos <- regexpr("transformed parameters\\s*\\{", code_shared)
  model_pos <- regexpr("model\\s*\\{", code_shared)
  expect_true(tp_pos < model_pos, info = "transformed parameters should come before model block")
  
  # Check that all braces are properly matched
  open_braces <- length(gregexpr("\\{", code_shared)[[1]])
  close_braces <- length(gregexpr("\\}", code_shared)[[1]])
  expect_equal(open_braces, close_braces, info = "All braces should be properly matched")
  
  # Final validation: ensure multivariate model compiles correctly
  expect_no_error(stancode(mf_shared, data = data, validate = TRUE))
})

test_that("stancode integrates custom priors correctly", {
  data <- setup_stan_test_data()$univariate
  mf <- mvgam_formula(y ~ x, trend_formula = ~ AR(p = 1))

  # Custom priors using brms syntax
  custom_priors <- brms::prior("normal(0, 0.5)", class = "ar1_trend") +
                   brms::prior("exponential(2)", class = "sigma_trend")

  code_with_priors <- stancode(mf, data = data, family = poisson(),
                               prior = custom_priors)
  code_default <- stancode(mf, data = data, family = poisson())

  expect_s3_class(code_with_priors, "stancode")
  expect_s3_class(code_default, "stancode")

  # Custom priors should be reflected in code
  expect_match2(code_with_priors, "normal\\(0,\\s*0\\.5\\)")
  expect_match2(code_with_priors, "exponential\\(2\\)")
})

test_that("stancode validates input parameters", {
  data <- setup_stan_test_data()$univariate

  # Invalid formula object
  expect_error(
    stancode("y ~ x", data = data),
    "object.*mvgam_formula"
  )

  # Invalid data
  mf <- mvgam_formula(y ~ x)
  expect_error(
    stancode(mf, data = "not_a_dataframe")
  )

  # Missing data parameter
  expect_error(
    stancode(mf, family = poisson()),
    "data"
  )
})

# standata Tests ----

test_that("standata.mvgam_formula returns proper list structure", {
  data <- setup_stan_test_data()$univariate
  mf <- mvgam_formula(y ~ x, trend_formula = ~ RW())

  standata_result <- standata(mf, data = data, family = poisson())

  # Should be a named list
  expect_type(standata_result, "list")
  expect_gt(length(standata_result), 0)
  expect_true(all(names(standata_result) != ""))

  # Should contain key data elements
  expect_true("N" %in% names(standata_result))  # Number of observations
  expect_true("Y" %in% names(standata_result))  # Response variable

  # Should contain trend-related data
  expect_true(any(grepl("trend", names(standata_result))))

  # Mapping functionality: should contain observation-to-trend mapping arrays
  expect_true("obs_trend_time" %in% names(standata_result))
  expect_true("obs_trend_series" %in% names(standata_result))

  # Mapping arrays should have correct dimensions
  expect_equal(length(standata_result$obs_trend_time), standata_result$N)
  expect_equal(length(standata_result$obs_trend_series), standata_result$N)

  # Mapping arrays should contain valid indices
  expect_true(all(standata_result$obs_trend_time >= 1))
  expect_true(all(standata_result$obs_trend_series >= 1))
})

test_that("standata handles different data structures", {
  test_data <- setup_stan_test_data()

  # Test with different datasets
  for (data_name in names(test_data)) {
    data <- test_data[[data_name]]
    mf <- mvgam_formula(y ~ 1, trend_formula = ~ RW())

    # Skip multivariate data for simple formula
    if (data_name == "multivariate") next

    standata_result <- standata(mf, data = data, family = poisson())

    expect_type(standata_result, "list")
    expect_true("N" %in% names(standata_result))
    expect_equal(standata_result$N, nrow(data))
  }
})

test_that("standata processes missing data correctly", {
  data <- setup_stan_test_data()$with_missings
  mf <- mvgam_formula(y ~ x, trend_formula = ~ RW())

  standata_result <- standata(mf, data = data, family = poisson())

  expect_type(standata_result, "list")

  # Should handle missing values appropriately
  # Exact behavior depends on mvgam's missing data handling
  expect_true("N" %in% names(standata_result))
  expect_gte(standata_result$N, 0)  # Should have some observations

  # Missing data mapping: mapping arrays should match reduced observation count
  expect_true("obs_trend_time" %in% names(standata_result))
  expect_true("obs_trend_series" %in% names(standata_result))

  # Mapping arrays should align with non-missing observations
  expect_equal(length(standata_result$obs_trend_time), standata_result$N)
  expect_equal(length(standata_result$obs_trend_series), standata_result$N)

  # Should have fewer observations than original data due to missings
  original_data <- setup_stan_test_data()$univariate
  expect_lt(standata_result$N, nrow(original_data))
})

test_that("standata integrates with trend systems", {
  data <- setup_stan_test_data()$univariate

  # Test different trend types
  trend_specs <- list(
    rw = ~ RW(),
    ar = ~ AR(p = 1),
    var = ~ VAR(p = 1, cor = TRUE)
  )

  for (trend_name in names(trend_specs)) {
    mf <- mvgam_formula(y ~ x, trend_formula = trend_specs[[trend_name]])
    standata_result <- standata(mf, data = data, family = poisson())

    expect_type(standata_result, "list")
    expect_gt(length(standata_result), 5)  # Should have multiple data components

    # Should contain trend-specific elements
    expect_true(any(grepl("trend", names(standata_result))))
  }
})

test_that("standata validates input consistency", {
  data <- setup_stan_test_data()$univariate

  # Data validation should occur
  mf <- mvgam_formula(y ~ x, trend_formula = ~ RW())

  # Valid call should work
  expect_silent({
    standata_result <- standata(mf, data = data, family = poisson())
  })

  # Invalid time series structure should error
  bad_data <- data
  bad_data$time <- c(1, 3, 5, 7:nrow(data))  # Irregular intervals

  expect_error(
    standata(mf, data = bad_data, family = poisson())
  )
})

# Integration Tests ----

test_that("stancode and standata are consistent", {
  data <- setup_stan_test_data()$univariate
  mf <- mvgam_formula(y ~ s(x), trend_formula = ~ AR(p = 1))

  code <- stancode(mf, data = data, family = poisson())
  standata_result <- standata(mf, data = data, family = poisson())

  # Both should succeed
  expect_s3_class(code, "stancode")
  expect_type(standata_result, "list")

  # Code should reference data elements that exist in standata
  # This is a simplified check - real validation would parse Stan code
  if ("N" %in% names(standata_result)) {
    expect_match2(code, "int.*N")
  }
})

test_that("stan functions work with complex model specifications", {
  data <- setup_stan_test_data()$multivariate

  # Complex multivariate model with priors
  mf <- mvgam_formula(
    mvbind(count, biomass) ~ s(x, by = habitat) + habitat,
    trend_formula = ~ AR(p = 1, cor = TRUE, n_lv = 2)
  )

  priors <- brms::prior("normal(0, 1)", class = "ar1_trend")

  # Should handle complex specifications
  expect_silent({
    code <- stancode(mf, data = data, prior = priors)
    standata_result <- standata(mf, data = data, prior = priors)
  })

  expect_s3_class(code, "stancode")
  expect_type(standata_result, "list")

  # Should contain complex model elements
  expect_match2(code, "count")
  expect_match2(code, "biomass")
  expect_match2(code, "ar1_trend")
})

test_that("stan functions preserve object attributes and metadata", {
  data <- setup_stan_test_data()$univariate
  mf <- mvgam_formula(y ~ x, trend_formula = ~ RW())

  code <- stancode(mf, data = data, family = poisson())
  standata_result <- standata(mf, data = data, family = poisson())

  # stancode should have correct class
  expect_equal(class(code), c("mvgamstancode", "stancode", "character"))

  # standata should preserve key information
  expect_type(standata_result, "list")
  expect_true(length(names(standata_result)) > 0)  # Should have named elements
})

# Edge Cases and Error Handling ----

test_that("stan functions handle edge cases gracefully", {
  # Very small dataset
  small_data <- data.frame(
    time = 1:3,
    series = factor(rep("s1", 3)),
    y = c(1, 2, 1),
    x = c(0.1, 0.2, 0.3)
  )

  mf <- mvgam_formula(y ~ 1, trend_formula = ~ RW())

  # Should handle small datasets
  expect_silent({
    code <- stancode(mf, data = small_data, family = poisson())
    standata_result <- standata(mf, data = small_data, family = poisson())
  })

  expect_s3_class(code, "stancode")
  expect_type(standata_result, "list")
})

test_that("stan functions provide informative error messages", {
  data <- setup_stan_test_data()$univariate

  # Missing required data components
  bad_data <- data[, c("y", "x")]  # Missing time and series
  mf <- mvgam_formula(y ~ x, trend_formula = ~ RW())

  # Should provide informative errors
  expect_error(
    stancode(mf, data = bad_data, family = poisson()),
    class = c("rlang_error", "error")
  )

  expect_error(
    standata(mf, data = bad_data, family = poisson()),
    class = c("rlang_error", "error")
  )
})
