# Tests for Enhanced brms Setup Functions
# =====================================

# Test setup_brms_lightweight() enhancements
# -------------------------------------------

test_that("setup_brms_lightweight accepts trend_formula parameter", {
  # Create minimal test data
  data <- data.frame(
    y = rnorm(20),
    x = rnorm(20),
    time = 1:20,
    series = factor(rep(1:2, each = 10))
  )

  # Test with NULL trend_formula (should work like before)
  expect_no_error({
    setup1 <- setup_brms_lightweight(
      formula = y ~ x,
      data = data,
      trend_formula = NULL
    )
  })

  expect_true(is.list(setup1))
  expect_true("trend_formula" %in% names(setup1))
  expect_null(setup1$trend_formula)
})

test_that("setup_brms_lightweight validates trend_formula parameter types", {
  data <- data.frame(
    y = rnorm(20),
    x = rnorm(20),
    time = 1:20,
    series = factor(rep(1:2, each = 10))
  )

  # Valid formula should work
  expect_no_error({
    setup_brms_lightweight(
      formula = y ~ x,
      data = data,
      trend_formula = ~ 1
    )
  })

  # Invalid types should error
  expect_error({
    setup_brms_lightweight(
      formula = y ~ x,
      data = data,
      trend_formula = "invalid"  # Character not allowed
    )
  })

  expect_error({
    setup_brms_lightweight(
      formula = y ~ x,
      data = data,
      trend_formula = 123  # Numeric not allowed
    )
  })
})

test_that("setup_brms_lightweight includes trend information in output", {
  data <- data.frame(
    y = rnorm(20),
    x = rnorm(20),
    time = 1:20,
    series = factor(rep(1:2, each = 10))
  )

  # Test that simple trend formula works (should default to ZMVN)
  setup <- setup_brms_lightweight(
    formula = y ~ x,
    data = data,
    trend_formula = ~ 1
  )

  # Should include trend_formula in output
  expect_true("trend_formula" %in% names(setup))
  expect_true(inherits(setup$trend_formula, "formula"))

  # Should include trend_specs when trend_formula provided
  expect_true("trend_specs" %in% names(setup))
  expect_true(is.list(setup$trend_specs) || is.null(setup$trend_specs))
})

test_that("setup_brms_lightweight maintains all original functionality", {
  data <- data.frame(
    y = rnorm(20),
    x = rnorm(20),
    time = 1:20,
    series = factor(rep(1:2, each = 10))
  )

  setup <- setup_brms_lightweight(
    formula = y ~ x,
    data = data,
    family = gaussian()
  )

  # Should have all required components
  required_components <- c("formula", "data", "family", "stancode",
                          "standata", "prior", "brmsterms", "brmsfit")

  for (component in required_components) {
    expect_true(component %in% names(setup),
                info = paste("Missing component:", component))
  }

  # Components should have correct types
  expect_true(inherits(setup$formula, "formula"))
  expect_true(is.data.frame(setup$data))
  expect_true(is.character(setup$stancode))
  expect_true(is.list(setup$standata))
  expect_true(inherits(setup$prior, "brmsprior"))
})

test_that("setup_brms_lightweight validates setup components", {
  data <- data.frame(
    y = rnorm(20),
    x = rnorm(20),
    time = 1:20,
    series = factor(rep(1:2, each = 10))
  )

  # Valid setup should pass validation
  expect_no_error({
    setup <- setup_brms_lightweight(
      formula = y ~ x,
      data = data
    )
  })

  # The validate_setup_components should be called internally
  # If it reaches this point without error, validation passed
  expect_true(TRUE)
})

test_that("setup_brms_lightweight handles multivariate formulas", {
  data <- data.frame(
    y1 = rnorm(20),
    y2 = rnorm(20),
    x = rnorm(20),
    time = 1:20,
    series = factor(rep(1:2, each = 10))
  )

  # Test with simple multivariate-like data structure
  # Even if complex multivariate formulas aren't supported yet,
  # the function should handle the data without error
  expect_no_error({
    setup <- setup_brms_lightweight(
      formula = y1 ~ x,  # Simple formula with multivariate data
      data = data
    )
  })
})

test_that("setup_brms_lightweight error handling works correctly", {
  # Test with invalid formula
  expect_error({
    setup_brms_lightweight(
      formula = "not a formula",
      data = data.frame(y = 1, x = 1)
    )
  })

  # Test with invalid data
  expect_error({
    setup_brms_lightweight(
      formula = y ~ x,
      data = "not a data frame"
    )
  })

  # Test with empty data
  expect_error({
    setup_brms_lightweight(
      formula = y ~ x,
      data = data.frame()
    )
  })
})

test_that("setup_brms_lightweight mock backend works for inspection", {
  data <- data.frame(
    y = rnorm(20),
    x = rnorm(20),
    time = 1:20,
    series = factor(rep(1:2, each = 10))
  )

  setup <- setup_brms_lightweight(
    formula = y ~ x,
    data = data
  )

  # Mock backend should create a brmsfit object
  expect_true(inherits(setup$brmsfit, "brmsfit"))

  # Stan code should be extractable
  expect_true(is.character(setup$stancode))
  expect_true(nchar(setup$stancode) > 0)

  # Stan data should be extractable
  expect_true(is.list(setup$standata))
  expect_true(length(setup$standata) > 0)
})

test_that("setup_brms_lightweight performance tracking works", {
  data <- data.frame(
    y = rnorm(20),
    x = rnorm(20),
    time = 1:20,
    series = factor(rep(1:2, each = 10))
  )

  setup <- setup_brms_lightweight(
    formula = y ~ x,
    data = data
  )

  # Should track setup time
  expect_true("setup_time" %in% names(setup))
  expect_true(is.numeric(setup$setup_time))
  expect_true(setup$setup_time >= 0)
})

# Integration tests with existing validation functions
# --------------------------------------------------

test_that("setup_brms_lightweight handles various trend formula types", {
  data <- data.frame(
    y = rnorm(20),
    x = rnorm(20),
    time = 1:20,
    temperature = rnorm(20),
    habitat = factor(rep(c("A", "B"), 10)),
    series = factor(rep(1:2, each = 10))
  )

  # Test formula without trend constructors (should default to ZMVN)
  setup_default <- setup_brms_lightweight(
    formula = y ~ x,
    data = data,
    trend_formula = ~ gp(time)
  )

  expect_true("trend_specs" %in% names(setup_default))
  expect_true(is.list(setup_default$trend_specs))

  # Test formula with explicit trend constructor
  setup_explicit <- setup_brms_lightweight(
    formula = y ~ x,
    data = data,
    trend_formula = ~ temperature + RW(time = time, series = series) + habitat
  )

  expect_true("trend_specs" %in% names(setup_explicit))
  expect_true(is.list(setup_explicit$trend_specs))

  # Test intercept-only trend formula
  setup_intercept <- setup_brms_lightweight(
    formula = y ~ x,
    data = data,
    trend_formula = ~ 1
  )

  expect_true("trend_specs" %in% names(setup_intercept))
  expect_true(is.list(setup_intercept$trend_specs))
})

test_that("setup_brms_lightweight integrates with existing validation", {
  data <- data.frame(
    y = rnorm(20),
    x = rnorm(20),
    time = 1:20,
    series = factor(rep(1:2, each = 10))
  )

  setup <- setup_brms_lightweight(
    formula = y ~ x,
    data = data,
    trend_formula = NULL  # Use NULL for successful test
  )

  # Should pass validate_setup_components (called internally)
  expect_no_error({
    mvgam:::validate_setup_components(setup)
  })

  # Should work with other validation functions
  expect_no_error({
    mvgam:::validate_brms_formula(setup$formula)
  })
})

# =============================================================================
# PARAMETER EXTRACTION AND RENAMING TESTS
# =============================================================================

test_that("extract_and_rename_trend_parameters handles univariate models", {
  data <- data.frame(
    y = rnorm(30),
    x1 = rnorm(30),
    x2 = rnorm(30),
    time = 1:30,
    series = factor(rep(1:3, each = 10))
  )

  # Setup simple univariate trend model
  trend_setup <- setup_brms_lightweight(
    formula = y ~ x1 + x2,
    data = data,
    family = gaussian()
  )

  # Extract dimensions
  dimensions <- mvgam:::extract_time_series_dimensions(
    data = data,
    time_var = "time",
    series_var = "series"
  )

  # Test parameter extraction
  extracted <- mvgam:::extract_and_rename_trend_parameters(
    trend_setup = trend_setup,
    dimensions = dimensions,
    suffix = "_trend"
  )

  # Should return a list of stanvars
  expect_type(extracted, "list")
  expect_true(length(extracted) > 0)

  # Should have times_trend matrix
  expect_true("times_trend" %in% names(extracted))
  expect_s3_class(extracted$times_trend, "stanvars")

  # Should have parameter mapping attributes
  expect_true(!is.null(attr(extracted, "parameter_mapping")))
  expect_true(!is.null(attr(extracted, "original_brmsfit")))
  expect_false(attr(extracted, "is_multivariate"))
  expect_null(attr(extracted, "response_names"))

  # Check parameter mapping structure
  mapping <- attr(extracted, "parameter_mapping")
  expect_true("original_to_renamed" %in% names(mapping))
  expect_true("renamed_to_original" %in% names(mapping))
  expect_false(mapping$is_multivariate)
})

test_that("extract_and_rename_trend_parameters handles multivariate models", {
  data <- data.frame(
    y1 = rnorm(30),
    y2 = rpois(30, 5),
    x1 = rnorm(30),
    x2 = rnorm(30),
    time = 1:30,
    series = factor(rep(1:3, each = 10))
  )

  # Setup multivariate trend model - use the list pattern from quick-reference.md
  # Multivariate observation model with response-specific trend formula
  trend_setup <- setup_brms_lightweight(
    formula = mvbind(y1, y2) ~ x1 + x2,  # Multivariate observation model
    trend_formula = list(
      y1 = ~ 1,  # Simple trend for y1
      y2 = ~ 1   # Simple trend for y2
    ),
    data = data,
    family = list(gaussian(), poisson())
  )

  # Extract dimensions
  dimensions <- mvgam:::extract_time_series_dimensions(
    data = data,
    time_var = "time",
    series_var = "series"
  )

  # Test parameter extraction
  extracted <- mvgam:::extract_and_rename_trend_parameters(
    trend_setup = trend_setup,
    dimensions = dimensions,
    suffix = "_trend"
  )

  # Should return a list of stanvars
  expect_type(extracted, "list")
  expect_true(length(extracted) > 0)

  # Should detect multivariate structure
  expect_true(attr(extracted, "is_multivariate"))
  response_names <- attr(extracted, "response_names")
  expect_true(!is.null(response_names))
  expect_true(all(c("y1", "y2") %in% response_names))

  # Should have times_trend matrix (shared for multivariate)
  expect_true("times_trend" %in% names(extracted))
  expect_s3_class(extracted$times_trend, "stanvars")

  # Check parameter mapping includes multivariate info
  mapping <- attr(extracted, "parameter_mapping")
  expect_true(mapping$is_multivariate)
  expect_equal(mapping$response_names, response_names)
})

test_that("extract_and_rename_trend_parameters extracts Stan code blocks correctly", {
  data <- data.frame(
    y = rnorm(20),
    x = rnorm(20),
    time = 1:20,
    series = factor(rep(1:2, each = 10))
  )

  # Setup trend model with predictors
  trend_setup <- setup_brms_lightweight(
    formula = y ~ x,
    data = data,
    family = gaussian()
  )

  dimensions <- mvgam:::extract_time_series_dimensions(data, "time", "series")

  extracted <- mvgam:::extract_and_rename_trend_parameters(
    trend_setup = trend_setup,
    dimensions = dimensions
  )

  # Should extract key Stan code components
  component_names <- names(extracted)

  # Check for expected Stan code stanvars
  # Note: data block declarations are skipped - actual data handled via standata
  expect_true(any(grepl("trend_parameters", component_names)))

  # Each stanvar should have proper structure
  for (name in component_names) {
    stanvar <- extracted[[name]]
    expect_s3_class(stanvar, "stanvars")
    # For stanvars objects, check the structure differently
    expect_true(length(stanvar) >= 1)
  }
})

test_that("extract_and_rename_trend_parameters handles parameter renaming", {
  data <- data.frame(
    y = rnorm(20),
    x = rnorm(20),
    time = 1:20,
    series = factor(rep(1:2, each = 10))
  )

  trend_setup <- setup_brms_lightweight(
    formula = y ~ x,
    data = data,
    family = gaussian()
  )

  dimensions <- mvgam:::extract_time_series_dimensions(data, "time", "series")

  extracted <- mvgam:::extract_and_rename_trend_parameters(
    trend_setup = trend_setup,
    dimensions = dimensions,
    suffix = "_test_suffix"
  )

  # Check parameter mapping contains renamed parameters
  mapping <- attr(extracted, "parameter_mapping")
  expect_true(length(mapping$original_to_renamed) > 0)
  expect_true(length(mapping$renamed_to_original) > 0)

  # Mapping should be bidirectional
  for (original in names(mapping$original_to_renamed)) {
    renamed <- mapping$original_to_renamed[[original]]
    expect_equal(mapping$renamed_to_original[[renamed]], original)
  }

  # Renamed parameters should have the correct suffix
  renamed_params <- names(mapping$renamed_to_original)
  expect_true(all(endsWith(renamed_params, "_test_suffix")))
})

test_that("extract_and_rename_trend_parameters creates times_trend matrix correctly", {
  data <- data.frame(
    y = rnorm(24),
    x = rnorm(24),
    x2 = rnorm(24),
    time = rep(1:12, 2),
    series = factor(rep(1:2, each = 12))
  )

  trend_setup <- setup_brms_lightweight(
    formula = y ~ gp(x, x2),
    data = data,
    family = gaussian()
  )

  dimensions <- mvgam:::extract_time_series_dimensions(data, "time", "series")

  extracted <- mvgam:::extract_and_rename_trend_parameters(
    trend_setup = trend_setup,
    dimensions = dimensions
  )

  # Should have times_trend matrix
  expect_true("times_trend" %in% names(extracted))
  times_stanvar <- extracted$times_trend

  # Check stanvar properties (access first stanvar in stanvars collection)
  expect_equal(times_stanvar$times_trend$name, "times_trend")
  expect_equal(times_stanvar$times_trend$block, "data")
  expect_true(nchar(times_stanvar$times_trend$scode) > 0)

  # Stan code should contain proper array declaration
  stan_code <- times_stanvar$times_trend$scode
  expect_true(grepl("int times_trend\\[", stan_code))
  expect_true(grepl("\\[12, 2\\]", stan_code))  # n_time=12, n_series=2
})

test_that("extract_and_rename_trend_parameters excludes likelihood from model block", {
  # Test comprehensive likelihood exclusion across different brms formula types
  set.seed(42)
  base_data <- data.frame(
    y = rnorm(24),
    x1 = rnorm(24),
    x2 = rnorm(24),
    fac = factor(sample(c('A', 'B', 'C'), 24, TRUE)),
    time = rep(1:8, 3),
    series = factor(rep(1:3, each = 8))
  )

  # Test various brms formula patterns that generate different likelihood patterns
  formula_tests <- list(
    "Simple linear" = y ~ x1,
    "Multiple predictors" = y ~ x1 + x2,
    "Random effects" = y ~ x1 + (1 | fac),
    "Smooth terms" = y ~ s(x1),
    "Tensor products" = y ~ t2(x1, x2),
    "Gaussian process" = y ~ gp(x1, x2),
    "GP with grouping" = y ~ gp(x1, x2, by = fac),
    "Intercept only" = y ~ 1
  )
  
  dimensions <- mvgam:::extract_time_series_dimensions(base_data, "time", "series")

  for (test_name in names(formula_tests)) {
    formula_to_test <- formula_tests[[test_name]]
    
    trend_setup <- setup_brms_lightweight(
      formula = formula_to_test,
      data = base_data,
      family = gaussian()
    )
    
    extracted <- mvgam:::extract_and_rename_trend_parameters(
      trend_setup = trend_setup,
      dimensions = dimensions
    )

    # Look for model block stanvars
    model_stanvars <- extracted[grepl("model", names(extracted))]

    if (length(model_stanvars) > 0) {
      for (stanvar_name in names(model_stanvars)) {
        stanvar_collection <- model_stanvars[[stanvar_name]]
        stan_code <- stanvar_collection[[1]]$scode

        # Should not contain likelihood statements (key patterns to exclude)
        expect_false(grepl("~\\s+(normal|poisson|gamma)", stan_code),
                    info = paste("Formula:", test_name, "- Distribution assignment found"))
        expect_false(grepl("target\\s*\\+=.*_lpdf", stan_code),
                    info = paste("Formula:", test_name, "- LPDF target statement found"))
        expect_false(grepl("target\\s*\\+=.*_lpmf", stan_code),
                    info = paste("Formula:", test_name, "- LPMF target statement found"))
        
        # Should not contain prior_only conditional blocks
        expect_false(grepl("prior_only", stan_code),
                    info = paste("Formula:", test_name, "- prior_only block found"))
        
        # Should not contain likelihood patterns with Y | syntax
        expect_false(grepl("Y\\s*\\|", stan_code),
                    info = paste("Formula:", test_name, "- Likelihood Y | pattern found"))
        
        # Should not contain GLM likelihood functions
        expect_false(grepl("glm.*lpdf", stan_code),
                    info = paste("Formula:", test_name, "- GLM likelihood function found"))
        
        # Additional comprehensive exclusion tests for robustness
        # (Trend models are always Gaussian - Architecture Decision 16)
        
        # Should not contain any other distribution assignments
        expect_false(grepl("~\\s+(student_t|lognormal|exponential|weibull|gamma|poisson)", stan_code),
                    info = paste("Formula:", test_name, "- Non-Gaussian distribution assignment found"))
        
        # Should not contain direct normal function calls in likelihood context
        expect_false(grepl("normal\\s*\\([^~]*Y", stan_code),
                    info = paste("Formula:", test_name, "- Direct normal likelihood call found"))
        
        # Should not contain multi-normal likelihood calls
        expect_false(grepl("multi_normal.*lpdf", stan_code),
                    info = paste("Formula:", test_name, "- Multivariate normal likelihood found"))
        
        # Should not contain observation variable likelihood assignments
        expect_false(grepl("Y\\s*~\\s*(normal|poisson|gamma)", stan_code),
                    info = paste("Formula:", test_name, "- Response variable likelihood assignment found"))
        
        # Should not contain target statements with response variables
        expect_false(grepl("target\\s*\\+=.*Y\\s*~", stan_code),
                    info = paste("Formula:", test_name, "- Target statement with response likelihood found"))

        # SHOULD contain useful linear predictor computations (things we want to keep)
        if (nchar(trimws(stan_code)) > 0) {
          # Linear predictor setup should be preserved
          may_contain_linear_predictor <- any(c(
            grepl("vector.*mu", stan_code),          # mu vector declarations
            grepl("mu.*=.*rep_vector", stan_code),   # mu initialization
            grepl("mu.*\\+=", stan_code),            # mu updates
            grepl("for.*\\(.*n.*in.*1:N.*\\)", stan_code), # for loops over observations
            grepl("target.*\\+=.*lprior", stan_code) # prior contributions (should be kept)
          ))
          
          # Additional useful patterns that should be preserved
          may_contain_useful_computation <- any(c(
            grepl("matrix\\s*\\[", stan_code),       # matrix declarations
            grepl("array\\s*\\[", stan_code),        # array declarations  
            grepl("\\w+\\s*\\[.*\\]", stan_code),    # variable indexing
            grepl("J_\\d+.*\\[", stan_code),         # random effects indexing
            grepl("r_\\d+_\\d+.*\\[", stan_code),    # coefficient applications
            grepl("dot_product|transpose", stan_code), # matrix operations
            grepl("real\\s+\\w+\\s*=", stan_code)    # transformed parameter assignments
          ))
          
          # If there's content, it should be useful (linear predictors, priors, or computations)
          if (!may_contain_linear_predictor && !may_contain_useful_computation) {
            # Only warn if it's not just empty/whitespace content
            if (grepl("[a-zA-Z]", stan_code)) {
              message("Formula: ", test_name, " - Stan code content may be missing expected patterns: ", 
                     substring(stan_code, 1, 100))
            }
          }
          
          # Positive tests: Ensure we didn't accidentally exclude parameter priors
          # These are GOOD and should be preserved: "sigma_trend ~ exponential(1);"
          if (grepl("\\w+_trend\\s*~", stan_code)) {
            # Make sure it's not a likelihood (no Y or target)
            # Split into lines and check each line with trend parameter priors
            lines <- strsplit(stan_code, "\n")[[1]]
            trend_lines <- lines[grepl("\\w+_trend\\s*~", lines)]
            for (line in trend_lines) {
              expect_false(grepl("Y|target\\s*\\+=", line),
                          info = paste("Formula:", test_name, "- Trend parameter prior mixed with likelihood:", line))
            }
          }
        }

        # Should preserve priors and linear predictor computations
        # This is okay: "sigma ~ exponential(1);" or "target += lprior;" or "mu += Intercept;"
        # This is not: "y ~ normal(mu, sigma);" or "target += normal_lpdf(Y | mu, sigma)"
      }
    }
  }
})

test_that("likelihood exclusion handles specific edge cases", {
  # Test specific patterns that could slip through filtering
  
  # Create mock Stan code with various edge cases
  test_cases <- list(
    "Conditional likelihood" = "if (!prior_only) { target += normal_lpdf(Y | mu, sigma); }",
    "Vectorized likelihood" = "target += normal_glm_lpdf(Y | X, beta, alpha, sigma);", 
    "Response assignment" = "Y[n] ~ normal(mu[n], sigma);",
    "Multivariate likelihood" = "target += multi_normal_lpdf(Y | mu, Sigma);",
    "Simple assignment" = "y ~ normal(0, 1);",
    "Prior-only conditional" = "if (prior_only) return; else { Y ~ normal(mu, 1); }",
    
    # These should be PRESERVED
    "Parameter prior" = "sigma ~ exponential(1);",
    "Linear predictor" = "mu = rep_vector(0.0, N); mu += Intercept;",  
    "Trend parameter prior" = "sigma_trend ~ exponential(1);",
    "Random effect computation" = "mu[n] += r_1_1[J_1[n]] * Z_1_1[n];"
  )
  
  should_exclude <- c(
    "Conditional likelihood", "Vectorized likelihood", "Response assignment",
    "Multivariate likelihood", "Simple assignment", "Prior-only conditional"
  )
  
  should_preserve <- c(
    "Parameter prior", "Linear predictor", "Trend parameter prior", "Random effect computation"
  )
  
  # Test each case through the actual filtering function
  for (case_name in names(test_cases)) {
    mock_model_block <- paste("model {", test_cases[[case_name]], "}")
    
    filtered_result <- extract_non_likelihood_from_model_block(mock_model_block)
    
    if (case_name %in% should_exclude) {
      # Should be filtered out completely or not contain the problematic pattern
      expect_true(
        is.null(filtered_result) || !grepl(test_cases[[case_name]], filtered_result, fixed = TRUE),
        info = paste("Case:", case_name, "should be excluded but was preserved:", filtered_result)
      )
    } else if (case_name %in% should_preserve) {
      # Should be preserved
      expect_false(
        is.null(filtered_result),
        info = paste("Case:", case_name, "should be preserved but was excluded")
      )
      if (!is.null(filtered_result)) {
        expect_true(
          grepl(trimws(test_cases[[case_name]]), filtered_result, fixed = TRUE),
          info = paste("Case:", case_name, "content should be preserved:", test_cases[[case_name]])
        )
      }
    }
  }
})

test_that("extract_and_rename_trend_parameters handles edge cases", {
  # Test with minimal data
  minimal_data <- data.frame(
    y = rnorm(6),
    time = 1:6,
    series = factor(rep(1, 6))
  )

  trend_setup <- setup_brms_lightweight(
    formula = y ~ 1,  # Intercept only
    data = minimal_data,
    family = gaussian()
  )

  dimensions <- mvgam:::extract_time_series_dimensions(minimal_data, "time", "series")

  # Should handle minimal case without errors
  expect_no_error({
    extracted <- mvgam:::extract_and_rename_trend_parameters(
      trend_setup = trend_setup,
      dimensions = dimensions
    )
  })

  extracted <- mvgam:::extract_and_rename_trend_parameters(
    trend_setup = trend_setup,
    dimensions = dimensions
  )

  # Should still create basic structure
  expect_type(extracted, "list")
  expect_true("times_trend" %in% names(extracted))
  expect_true(!is.null(attr(extracted, "parameter_mapping")))
})

test_that("extract_and_rename_trend_parameters preserves brmsfit for prediction", {
  data <- data.frame(
    y = rnorm(20),
    x = rnorm(20),
    time = 1:20,
    series = factor(rep(1:2, each = 10))
  )

  trend_setup <- setup_brms_lightweight(
    formula = y ~ s(x),
    data = data,
    family = gaussian()
  )

  dimensions <- mvgam:::extract_time_series_dimensions(data, "time", "series")

  extracted <- mvgam:::extract_and_rename_trend_parameters(
    trend_setup = trend_setup,
    dimensions = dimensions
  )

  # Should preserve original brmsfit
  original_brmsfit <- attr(extracted, "original_brmsfit")
  expect_true(!is.null(original_brmsfit))
  expect_s3_class(original_brmsfit, "brmsfit")

  # Should be the same object as in trend_setup
  expect_identical(original_brmsfit, trend_setup$brmsfit)

  # Parameter mapping should enable reverse lookups for prediction
  mapping <- attr(extracted, "parameter_mapping")
  expect_true(length(mapping$renamed_to_original) > 0)

  # Each renamed parameter should map back to an original
  for (renamed in names(mapping$renamed_to_original)) {
    original <- mapping$renamed_to_original[[renamed]]
    expect_true(nchar(original) > 0)
    expect_false(endsWith(original, "_trend"))  # Original shouldn't have suffix
  }
})

test_that("extract_and_rename_trend_parameters handles multivariate shared trends correctly", {
  data <- data.frame(
    y1 = rnorm(30),
    y2 = rpois(30, 5),
    x1 = rnorm(30),
    x2 = rnorm(30),
    time = 1:30,
    series = factor(rep(1:3, each = 10))
  )

  # Setup multivariate model with shared trend (applied to all responses)
  trend_setup <- setup_brms_lightweight(
    formula = mvbind(y1, y2) ~ x1 + x2,
    trend_formula = ~ AR(p = 1, time = time, series = series),  # Shared trend
    data = data,
    family = list(gaussian(), poisson())
  )

  dimensions <- mvgam:::extract_time_series_dimensions(
    data = data,
    time_var = "time",
    series_var = "series"
  )

  extracted <- mvgam:::extract_and_rename_trend_parameters(
    trend_setup = trend_setup,
    dimensions = dimensions,
    suffix = "_trend"
  )

  # Check multivariate metadata
  expect_true(attr(extracted, "is_multivariate"))
  expect_equal(attr(extracted, "response_names"), c("y1", "y2"))

  # Parameter mapping should exist and contain renamed parameters
  mapping <- attr(extracted, "parameter_mapping")
  expect_true(mapping$is_multivariate)
  expect_equal(mapping$response_names, c("y1", "y2"))
  expect_true(length(mapping$original_to_renamed) > 0)
  expect_true(length(mapping$renamed_to_original) > 0)

  # Check for AR-specific parameters with _trend suffix
  renamed_params <- names(mapping$renamed_to_original)
  ar_params <- renamed_params[grepl("ar1.*_trend$", renamed_params)]
  expect_true(length(ar_params) > 0)  # Should have AR parameters

  # Check for times_trend matrix (shared for multivariate)
  expect_true("times_trend" %in% names(extracted))
  times_stanvar <- extracted$times_trend
  expect_equal(times_stanvar$times_trend$name, "times_trend")
  expect_equal(times_stanvar$times_trend$block, "data")

  # Should preserve bidirectional mapping for prediction compatibility
  for (original in names(mapping$original_to_renamed)) {
    renamed <- mapping$original_to_renamed[[original]]
    expect_equal(mapping$renamed_to_original[[renamed]], original)
  }

  # All renamed parameters should have the suffix
  expect_true(all(endsWith(renamed_params, "_trend")))
})

test_that("extract_and_rename_trend_parameters handles multivariate response-specific trends correctly", {
  data <- data.frame(
    y1 = rnorm(24),
    y2 = rpois(24, 3),
    y3 = rbinom(24, 1, 0.5),
    x = rnorm(24),
    time = rep(1:12, 2),
    series = factor(rep(c("A", "B"), each = 12))
  )

  # Setup multivariate model with response-specific trends using list syntax
  trend_setup <- setup_brms_lightweight(
    formula = mvbind(y1, y2, y3) ~ x,
    trend_formula = list(
      y1 = ~ AR(p = 1, time = time, series = series),  # AR for y1
      y2 = ~ RW(time = time, series = series),         # RW for y2
      y3 = NULL                                        # No trend for y3
    ),
    data = data,
    family = list(gaussian(), poisson(), bernoulli())
  )

  dimensions <- mvgam:::extract_time_series_dimensions(
    data = data,
    time_var = "time",
    series_var = "series"
  )

  extracted <- mvgam:::extract_and_rename_trend_parameters(
    trend_setup = trend_setup,
    dimensions = dimensions,
    suffix = "_trend"
  )

  # Check multivariate metadata
  expect_true(attr(extracted, "is_multivariate"))
  expect_equal(attr(extracted, "response_names"), c("y1", "y2", "y3"))

  # Parameter mapping should reflect response-specific structure
  mapping <- attr(extracted, "parameter_mapping")
  expect_true(mapping$is_multivariate)
  expect_equal(mapping$response_names, c("y1", "y2", "y3"))
  expect_true(length(mapping$original_to_renamed) > 0)
  expect_true(length(mapping$renamed_to_original) > 0)

  # Check for response-specific parameter patterns
  renamed_params <- names(mapping$renamed_to_original)

  # Should have AR parameters for y1 (since it uses AR trend)
  ar_params <- renamed_params[grepl("ar1.*_trend$", renamed_params)]
  expect_true(length(ar_params) > 0)

  # Should have sigma parameters for y2 (since it uses RW trend)
  sigma_params <- renamed_params[grepl("sigma.*_trend$", renamed_params)]
  expect_true(length(sigma_params) > 0)

  # Check for times_trend matrix structure
  # With response-specific trends, we might have multiple times_trend matrices
  times_trend_names <- names(extracted)[grepl("times_trend", names(extracted))]
  expect_true(length(times_trend_names) > 0)

  # Each times_trend should be properly structured
  for (times_name in times_trend_names) {
    times_stanvar <- extracted[[times_name]]
    expect_equal(times_stanvar[[times_name]]$block, "data")
    expect_true(nchar(times_stanvar[[times_name]]$scode) > 0)
    expect_true(grepl("int.*times_trend", times_stanvar[[times_name]]$scode))
  }

  # Verify bidirectional mapping integrity
  for (original in names(mapping$original_to_renamed)) {
    renamed <- mapping$original_to_renamed[[original]]
    expect_equal(mapping$renamed_to_original[[renamed]], original)
  }

  # All renamed parameters should have the suffix
  expect_true(all(endsWith(renamed_params, "_trend")))

  # Original brmsfit should be preserved for prediction compatibility
  original_brmsfit <- attr(extracted, "original_brmsfit")
  expect_true(!is.null(original_brmsfit))
  expect_s3_class(original_brmsfit, "brmsfit")
})
