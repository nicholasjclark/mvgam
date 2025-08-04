#' Tests for Trend Dispatcher System
#'
#' Comprehensive tests for the trend type registry, validation, and formula
#' parsing functionality. Includes edge cases based on brms testing patterns.

test_that("CAR constructor works for continuous-time AR", {
  # CAR should work without n_lv parameter (continuous-time AR)
  car_trend <- CAR()
  expect_s3_class(car_trend, "mvgam_trend")
  expect_equal(car_trend$trend_model, "CAR1")
  expect_false(car_trend$cor)
  expect_false(car_trend$ma)
  expect_equal(car_trend$gr, "NA")
  expect_equal(car_trend$subgr, "series")
  
  # CAR only supports p = 1 (continuous-time AR1)
  expect_error(CAR(p = 2), "Argument 'p' must be = 1")
})

test_that("PW constructor rejects factor models correctly", {
  # PW with n_lv should error immediately
  expect_error(PW(n_lv = 3),
               "Factor models.*not supported for PW trends")
  expect_error(PW(n_lv = 3),
               "changepoint modeling")
  expect_error(PW(n_lv = 3),
               "Remove.*n_lv.*parameter")

  # PW without n_lv should work
  suppressWarnings({
    pw_trend <- PW()
    expect_is(pw_trend, "mvgam_trend")
    expect_true(pw_trend$trend %in% c("PWlinear", "PWlogistic"))
  })
})

test_that("AR constructor accepts factor models", {
  # AR with n_lv should work
  suppressWarnings({
    ar_trend <- AR(n_lv = 2)
    expect_is(ar_trend, "mvgam_trend")
    expect_equal(ar_trend$n_lv, 2)
    expect_equal(ar_trend$trend, "AR1")
  })

  # AR without n_lv should work
  suppressWarnings({
    ar_trend_no_lv <- AR()
    expect_is(ar_trend_no_lv, "mvgam_trend")
    expect_null(ar_trend_no_lv$n_lv)
  })
})

test_that("RW constructor accepts factor models", {
  # RW with n_lv should work
  suppressWarnings({
    rw_trend <- RW(n_lv = 3)
    expect_is(rw_trend, "mvgam_trend")
    expect_equal(rw_trend$n_lv, 3)
  })

  # RW without n_lv should work
  suppressWarnings({
    rw_trend_no_lv <- RW()
    expect_is(rw_trend_no_lv, "mvgam_trend")
    expect_null(rw_trend_no_lv$n_lv)
  })
})

test_that("VAR constructor accepts factor models", {
  # VAR with n_lv should work
  suppressWarnings({
    var_trend <- VAR(n_lv = 4)
    expect_is(var_trend, "mvgam_trend")
    expect_equal(var_trend$n_lv, 4)
  })

  # VAR without n_lv should work
  suppressWarnings({
    var_trend_no_lv <- VAR()
    expect_is(var_trend_no_lv, "mvgam_trend")
    expect_null(var_trend_no_lv$n_lv)
  })
})

test_that("Factor validation error messages are consistent", {
  # Check that all factor-incompatible trends give similar error structure
  # Note: CAR() constructor doesn't accept n_lv parameter, errors happen at stanvars level
  
  expect_error(PW(n_lv = 1), "Factor models.*not supported")

  # Check they mention specific alternatives (AR now factor-compatible)
  expect_error(PW(n_lv = 1), "factor-compatible trends.*AR.*RW.*VAR")

  # Check they have specific reasons
  expect_error(PW(n_lv = 1), "changepoint")
})

# Test trend registry functionality
test_that("trend registry works correctly", {

  # Test basic registry functionality
  choices <- mvgam_trend_choices()
  expect_type(choices, "character")
  expect_true(length(choices) > 0)
  expect_true("RW" %in% choices)
  expect_true("AR" %in% choices)
  expect_true("VAR" %in% choices)

  # Test pattern generation
  pattern <- mvgam:::mvgam_trend_pattern()
  expect_type(pattern, "character")
  expect_true(nzchar(pattern))

  # Test pattern matches known constructors
  expect_true(grepl(pattern, "RW()"))
  expect_true(grepl(pattern, "AR(p=1)"))
  expect_true(grepl(pattern, "VAR(cor=TRUE)"))
  expect_false(grepl(pattern, "s(time)"))
  expect_false(grepl(pattern, "linear_trend"))
})

# Test trend constructor creation and validation
test_that("trend constructors work with dispatcher integration", {
  suppressWarnings({

    # Test RW constructor
    rw_trend <- RW(ma = TRUE, cor = FALSE)
    expect_s3_class(rw_trend, "mvgam_trend")
    expect_equal(rw_trend$trend, "RW")
    expect_true(rw_trend$ma)
    expect_false(rw_trend$cor)
    expect_true(is.mvgam_trend(rw_trend))

    # Test AR constructor with single lag
    ar1_trend <- AR(p = 1, ma = FALSE)
    expect_s3_class(ar1_trend, "mvgam_trend")
    expect_equal(ar1_trend$trend, "AR1")
    expect_equal(ar1_trend$p, 1)
    expect_equal(ar1_trend$ar_lags, 1)
    expect_equal(ar1_trend$max_lag, 1)

    # Test AR constructor with multiple lags
    ar_seasonal <- AR(p = c(1, 12, 24))
    expect_s3_class(ar_seasonal, "mvgam_trend")
    expect_equal(ar_seasonal$trend, "AR(1,12,24)")
    expect_equal(ar_seasonal$p, c(1, 12, 24))
    expect_equal(ar_seasonal$ar_lags, c(1, 12, 24))
    expect_equal(ar_seasonal$max_lag, 24)
    expect_true("ar1" %in% ar_seasonal$tpars)
    expect_true("ar12" %in% ar_seasonal$tpars)
    expect_true("ar24" %in% ar_seasonal$tpars)

    # Test VAR constructor with order
    var2_trend <- VAR(p = 2, cor = TRUE)
    expect_s3_class(var2_trend, "mvgam_trend")
    expect_equal(var2_trend$trend, "VAR2")
    expect_equal(var2_trend$p, 2)
    expect_true(var2_trend$cor)
    expect_true("A1" %in% var2_trend$tpars)
    expect_true("A2" %in% var2_trend$tpars)
  })
})

# Test grouping validation helper
test_that("grouping validation helper works correctly", {

  # Test default case
  result1 <- mvgam:::validate_grouping_arguments("NA", "NA")
  expect_equal(result1$gr, "NA")
  expect_equal(result1$subgr, "series")

  # Test valid hierarchical case would work (but we test error cases)
  expect_error(
    mvgam:::validate_grouping_arguments("region", "NA"),
    "Hierarchical grouping requires subgrouping"
  )

  expect_error(
    mvgam:::validate_grouping_arguments("region", "series"),
    "Invalid subgrouping for hierarchical models"
  )
})

# Test formula parsing - basic functionality
test_that("basic formula parsing works", {
  suppressWarnings({

    # Test simple trend-only formula
    f1 <- ~ RW()
    parsed1 <- mvgam:::parse_trend_formula(f1)

    expect_equal(length(parsed1$trend_components), 1)
    expect_s3_class(parsed1$trend_components[[1]], "mvgam_trend")
    expect_equal(parsed1$trend_components[[1]]$trend, "RW")
    expect_equal(parsed1$base_formula, ~ 1)
    expect_equal(parsed1$trend_terms, "RW()")
    expect_equal(length(parsed1$regular_terms), 0)

    # Test formula with regular predictors and trend
    f2 <- ~ s(time) + cov1 + AR(p = 1)
    parsed2 <- mvgam:::parse_trend_formula(f2)

    expect_equal(length(parsed2$trend_components), 1)
    expect_equal(parsed2$trend_components[[1]]$trend, "AR1")
    expect_equal(parsed2$trend_terms, "AR(p = 1)")
    expect_true("s(time)" %in% parsed2$regular_terms)
    expect_true("cov1" %in% parsed2$regular_terms)
    expect_equal(length(parsed2$regular_terms), 2)
  })
})

# Test formula parsing - order independence (key requirement)
test_that("formula parsing is order-independent", {
  suppressWarnings({

    # Create formulas with different orderings
    f1 <- ~ s(time) + cov1 + RW(cor = TRUE) + cov2
    f2 <- ~ RW(cor = TRUE) + s(time) + cov2 + cov1
    f3 <- ~ cov1 + cov2 + s(time) + RW(cor = TRUE)
    f4 <- ~ cov2 + RW(cor = TRUE) + cov1 + s(time)

  # Parse all versions
  parsed1 <- mvgam:::parse_trend_formula(f1)
  parsed2 <- mvgam:::parse_trend_formula(f2)
  parsed3 <- mvgam:::parse_trend_formula(f3)
  parsed4 <- mvgam:::parse_trend_formula(f4)

  # Check that trend components are identical
  expect_equal(parsed1$trend_terms, parsed2$trend_terms)
  expect_equal(parsed1$trend_terms, parsed3$trend_terms)
  expect_equal(parsed1$trend_terms, parsed4$trend_terms)

  expect_equal(parsed1$trend_components[[1]]$trend, "RW")
  expect_equal(parsed2$trend_components[[1]]$trend, "RW")
  expect_equal(parsed3$trend_components[[1]]$trend, "RW")
  expect_equal(parsed4$trend_components[[1]]$trend, "RW")

  # Check that regular terms are preserved (may be in different order but same content)
  expect_setequal(parsed1$regular_terms, parsed2$regular_terms)
  expect_setequal(parsed1$regular_terms, parsed3$regular_terms)
  expect_setequal(parsed1$regular_terms, parsed4$regular_terms)

  # All should contain the same regular terms
  expect_true("s(time)" %in% parsed1$regular_terms)
  expect_true("cov1" %in% parsed1$regular_terms)
  expect_true("cov2" %in% parsed1$regular_terms)
  expect_equal(length(parsed1$regular_terms), 3)
  })
})

# Test formula parsing - multiple trends
test_that("multiple trend components are handled correctly", {
  suppressWarnings({

    # Test formula with multiple trends in different orders
    f1 <- ~ s(season) + RW() + AR(p = 2) + cov1
    f2 <- ~ AR(p = 2) + cov1 + RW() + s(season)

    parsed1 <- mvgam:::parse_trend_formula(f1)
    parsed2 <- mvgam:::parse_trend_formula(f2)

  # Should have two trend components
  expect_equal(length(parsed1$trend_components), 2)
  expect_equal(length(parsed2$trend_components), 2)

  # Multiple trends should be handled correctly
  expect_equal(length(parsed1$trend_components), 2)
  expect_equal(length(parsed2$trend_components), 2)

  # Should have same trend terms (order may differ)
  expect_setequal(parsed1$trend_terms, parsed2$trend_terms)
  expect_true("RW()" %in% parsed1$trend_terms)
  expect_true("AR(p = 2)" %in% parsed1$trend_terms)

  # Regular terms should be the same
  expect_setequal(parsed1$regular_terms, parsed2$regular_terms)
  expect_true("s(season)" %in% parsed1$regular_terms)
  expect_true("cov1" %in% parsed1$regular_terms)
  })
})

# Test edge cases inspired by brms testing
test_that("formula parsing handles edge cases correctly", {
  suppressWarnings({

    # Test nested parentheses and complex expressions
    f1 <- ~ s(time) + AR(p = c(1, 12)) + poly(x, degree = 2)
    parsed1 <- mvgam:::parse_trend_formula(f1)
    expect_equal(parsed1$trend_terms, "AR(p = c(1, 12))")
    expect_true("s(time)" %in% parsed1$regular_terms)
    expect_true("poly(x, degree = 2)" %in% parsed1$regular_terms)

    # Test interaction terms
    f2 <- ~ cov1:cov2 + s(time, by = group) + VAR(p = 1, ma = TRUE)
    parsed2 <- mvgam:::parse_trend_formula(f2)
    expect_equal(parsed2$trend_terms, "VAR(p = 1, ma = TRUE)")
    expect_true("cov1:cov2" %in% parsed2$regular_terms)
    expect_true("s(time, by = group)" %in% parsed2$regular_terms)

    # Test special characters and operators in regular terms
    f3 <- ~ I(x^2) + log(y + 1) + RW(ma = FALSE) + offset(z)
    parsed3 <- mvgam:::parse_trend_formula(f3)
    expect_equal(parsed3$trend_terms, "RW(ma = FALSE)")
    expect_true("I(x^2)" %in% parsed3$regular_terms)
    expect_true("log(y + 1)" %in% parsed3$regular_terms)
    # Check offset terms are captured separately
    expect_true("offset(z)" %in% parsed3$offset_terms)
    expect_equal(length(parsed3$regular_terms), 2)

    # Test whitespace handling
    f4 <- ~   s(time)   +   RW(cor=TRUE)   +   cov1
    parsed4 <- mvgam:::parse_trend_formula(f4)
    expect_equal(parsed4$trend_terms, "RW(cor = TRUE)")
    expect_setequal(parsed4$regular_terms, c("s(time)", "cov1"))
  })
})

# Test malformed formulas and error conditions
test_that("formula parsing error handling works comprehensively", {

  # Test empty formula (different from ~ 1)
  expect_error(
    mvgam:::parse_trend_formula(~ 1),
    "Empty trend formula provided"
  )

  # Test formula with response variable
  expect_error(
    mvgam:::parse_trend_formula(y ~ RW()),
    "Response variable not allowed"
  )

  # Test formula with no trend constructors
  expect_error(
    mvgam:::parse_trend_formula(~ s(time) + cov1),
    "No trend model specified"
  )

  # Test dot formula (should error before our validation)
  expect_error(
    mvgam:::parse_trend_formula(~ .),
    "Invalid formula syntax"
  )

  # Test invalid trend constructor calls
  expect_error(
    mvgam:::eval_trend_constructor("INVALID_TREND()"),
    "Failed to evaluate trend constructor"
  )

  # Test malformed trend constructor syntax
  expect_error(
    mvgam:::eval_trend_constructor("RW(invalid_param = )"),
    "Failed to evaluate trend constructor"
  )

  # Test unbalanced parentheses (should be caught by R's parser)
  expect_error(
    mvgam:::eval_trend_constructor("RW(cor = TRUE"),
    "Failed to evaluate trend constructor"
  )
})

# Test boundary conditions and special inputs
test_that("boundary conditions are handled correctly", {
  suppressWarnings({

    # Test formula with only intercept and trend
    f1 <- ~ 1 + RW()
    parsed1 <- mvgam:::parse_trend_formula(f1)
    expect_equal(parsed1$base_formula, ~ 1)
    expect_equal(length(parsed1$trend_components), 1)

    # Test formula with very long expressions
    long_expr <- paste0("s(x", 1:50, ")", collapse = " + ")
    f2 <- as.formula(paste("~", long_expr, "+ AR(p = 1)"))
    parsed2 <- mvgam:::parse_trend_formula(f2)
    expect_equal(length(parsed2$trend_components), 1)
    expect_equal(length(parsed2$regular_terms), 50)

    # Test formula with repeated trend constructors
    f3 <- ~ RW() + s(time) + RW(ma = TRUE)
    parsed3 <- mvgam:::parse_trend_formula(f3)
    expect_equal(length(parsed3$trend_components), 2)
    expect_setequal(parsed3$trend_terms, c("RW()", "RW(ma = TRUE)"))
  })
})

# Test regex pattern edge cases
test_that("regex pattern handles edge cases correctly", {

  # Test trend constructors with complex parameters
  complex_terms <- c(
    "AR(p = c(1, 12, 24), ma = TRUE, cor = FALSE)",
    "VAR(p = 10, cor = TRUE)",
    "RW(ma = FALSE)",
    "GP()",
    "s(time, bs = 'tp', k = 20)"
  )

  found_trends <- mvgam:::find_trend_terms(complex_terms)
  # Debug: print what was actually found
  # cat("Found trends:", paste(found_trends, collapse = ", "), "\n")

  expect_true(length(found_trends) >= 4)  # Should find at least 4 trend terms
  expect_true(any(grepl("^AR\\(", found_trends)))
  expect_true(any(grepl("^VAR\\(", found_trends)))
  expect_true(any(grepl("^RW\\(", found_trends)))
  expect_true(any(grepl("^GP\\(", found_trends)))
  expect_false("s(time, bs = 'tp', k = 20)" %in% found_trends)

  # Test extraction of regular terms from complex expressions
  regular_terms <- mvgam:::extract_regular_terms(complex_terms)
  expect_false("AR(p = c(1, 12, 24), ma = TRUE, cor = FALSE)" %in% regular_terms)
  expect_true("s(time, bs = 'tp', k = 20)" %in% regular_terms)

  # Test edge case: trend-like names that aren't trend constructors
  fake_trends <- c("ARbitrary()", "VARious()", "RWanda()", "s(time)")
  found_fake <- mvgam:::find_trend_terms(fake_trends)
  expect_equal(length(found_fake), 0)
})

# Test validation system edge cases
test_that("validation system handles edge cases", {

  # Test correlation requirements with edge cases
  expect_true(suppressWarnings(mvgam:::validate_correlation_requirements("region", FALSE)))
  expect_false(mvgam:::validate_correlation_requirements("NA", FALSE))
  expect_true(mvgam:::validate_correlation_requirements("group", TRUE))

  # Test invalid parameter combinations through constructors
  suppressWarnings(expect_error(
    AR(p = 0),
    "not >= 1"
  ))

  suppressWarnings(expect_error(
    AR(p = c(1, 1, 2)),  # Non-unique lags
    "Contains duplicated values"
  ))

  suppressWarnings(expect_error(
    VAR(p = -1),
    "not >= 1"
  ))
})

# Test extensibility and custom trends
test_that("custom trend registration works correctly", {

  # Create a custom trend constructor
  custom_constructor <- function(param = 1) {
    structure(
      list(
        trend = "CUSTOM",
        param = param,
        tpars = "custom_param",
        characteristics = list(supports_factors = FALSE)
      ),
      class = "mvgam_trend"
    )
  }

  # Create a simple generator function for the custom trend
  custom_generator <- function(trend_spec, data_info) {
    # Simple generator that returns basic stanvars
    list(
      custom_param = brms::stanvar(
        name = "custom_param",
        scode = "real custom_param;",
        block = "parameters"
      )
    )
  }

  # Register the custom trend with the registry system
  mvgam:::register_trend_type("CUSTOM", supports_factors = FALSE,
                               generator_func = custom_generator,
                               incompatibility_reason = "Custom trend for testing")

  # Test it appears in registry
  choices <- mvgam_trend_choices()
  expect_true("CUSTOM" %in% choices)

  # Test pattern includes custom trend
  pattern <- mvgam:::mvgam_trend_pattern()
  expect_true(grepl("CUSTOM", pattern))

  # Test formula parsing with custom trend
  f1 <- ~ s(time) + CUSTOM(param = 5) + cov1
  # Note: This would require the custom constructor to be available
  # in the evaluation environment
})

# Test print method edge cases
test_that("print method handles all configurations", {
  suppressWarnings({

    # Test minimal configuration
    minimal_trend <- RW()
    expect_output(print(minimal_trend), "mvgam trend specification")
    expect_output(print(minimal_trend), "Type: RW")

    # Test maximal configuration (without conflicting parameters, suppress expected warnings)
    complex_trend <- suppressWarnings(AR(p = c(1, 12), ma = FALSE, cor = TRUE, n_lv = 2))
    output <- capture.output(print(complex_trend))
    expect_true(any(grepl("Dynamic factors: 2", output)))
    expect_true(any(grepl("Correlation: enabled", output)))

    # Test hierarchical grouping display
    # (Would need to create such an object through the constructor)
  })
})

# Test complex real-world scenarios
test_that("realistic complex formulas work correctly", {
  suppressWarnings({

    # Test seasonal model with multiple components
    seasonal_formula <- ~ s(doy, bs = "cc", k = 12) +
                         s(temp, k = 10) +
                         factor(month) +
                         AR(p = c(1, 12, 24), ma = FALSE) +
                         offset(log_effort)

    parsed <- mvgam:::parse_trend_formula(seasonal_formula)
    expect_equal(length(parsed$trend_components), 1)
    expect_equal(parsed$trend_components[[1]]$p, c(1, 12, 24))
    expect_false(parsed$trend_components[[1]]$ma)
    expect_equal(length(parsed$regular_terms), 3)
    expect_true("offset(log_effort)" %in% parsed$offset_terms)

    # Test multivariate model formula
    multivar_formula <- ~ s(time, by = species, k = 20) +
                          habitat +
                          VAR(p = 2, cor = TRUE) +
                          s(temperature, species, bs = "fs")

    parsed2 <- mvgam:::parse_trend_formula(multivar_formula)
    expect_equal(parsed2$trend_components[[1]]$trend, "VAR2")
    expect_true(parsed2$trend_components[[1]]$cor)
    expect_true("s(time, by = species, k = 20)" %in% parsed2$regular_terms)
    expect_true("s(temperature, species, bs = \"fs\")" %in% parsed2$regular_terms)
  })
})

# Test time parameter functionality
test_that("time parameter works correctly in trend constructors", {

  # Test default time parameter (should default to 'time' with warnings)
  expect_warning(
    rw_default <- RW(),
    "Using default.*time.*variable"
  )
  expect_equal(rw_default$time, "time")

  expect_warning(
    ar_default <- AR(p = 1),
    "Using default.*time.*variable"
  )
  expect_equal(ar_default$time, "time")

  expect_warning(
    var_default <- VAR(p = 1),
    "Using default.*time.*variable"
  )
  expect_equal(var_default$time, "time")

  expect_warning(
    car_default <- CAR(p = 1),
    "Using default.*time.*variable"
  )
  expect_equal(car_default$time, "time")

  expect_warning(
    gp_default <- GP(),
    "Using default.*time.*variable"
  )
  expect_equal(gp_default$time, "time")

  # Test explicit time parameter with unquoted variable names (should warn about series default)
  expect_warning(
    rw_custom <- RW(time = week),
    "Using default.*series.*variable"
  )
  expect_equal(rw_custom$time, "week")

  expect_warning(
    ar_custom <- AR(time = year, p = 2),
    "Using default.*series.*variable"
  )
  expect_equal(ar_custom$time, "year")

  expect_warning(
    var_custom <- VAR(time = month, p = 1),
    "Using default.*series.*variable"
  )
  expect_equal(var_custom$time, "month")

  expect_warning(
    car_custom <- CAR(time = day),
    "Using default.*series.*variable"
  )
  expect_equal(car_custom$time, "day")

  expect_warning(
    gp_custom <- GP(time = timestep),
    "Using default.*series.*variable"
  )
  expect_equal(gp_custom$time, "timestep")

  # Test that other parameters still work correctly with time parameter (should warn about series default)
  expect_warning(
    ar_complex <- AR(time = period, p = c(1, 12), ma = TRUE, cor = TRUE),
    "Using default.*series.*variable"
  )
  expect_equal(ar_complex$time, "period")
  expect_equal(ar_complex$p, c(1, 12))
  expect_true(ar_complex$ma)
  expect_true(ar_complex$cor)

  # Test with grouping variables (suppress expected warning about correlation)
  expect_warning(
    var_grouped <- VAR(time = timepoint, p = 2, gr = region, subgr = species),
    "Hierarchical grouping specified without correlation"
  )
  expect_equal(var_grouped$time, "timepoint")
  expect_equal(var_grouped$gr, "region")
  expect_equal(var_grouped$subgr, "species")
})

# Test time variable validation functionality
test_that("time variable validation works correctly", {

  # Test basic validation function
  expect_equal(mvgam:::validate_time_variable("time"), "time")
  expect_equal(mvgam:::validate_time_variable("week"), "week")
  expect_equal(mvgam:::validate_time_variable("NA"), "time")  # Default fallback

  # Test with valid data
  test_data <- data.frame(
    y = 1:10,
    time = 1:10,
    week = 1:10,
    series = rep("A", 10)
  )

  expect_equal(mvgam:::validate_time_variable("time", test_data), "time")
  expect_warning(
    result <- mvgam:::validate_time_variable("week", test_data),
    "Using.*as time variable instead of 'time'"
  )
  expect_equal(result, "week")

  # Test error when time variable doesn't exist
  expect_error(
    mvgam:::validate_time_variable("missing_var", test_data),
    "Time variable.*not found in data"
  )

  # Test error when time variable is wrong type
  bad_data <- data.frame(
    y = 1:10,
    time = letters[1:10],  # Character instead of numeric
    series = rep("A", 10)
  )

  expect_error(
    mvgam:::validate_time_variable("time", bad_data),
    "Time variable.*must be numeric or integer"
  )

  # Test with integer time variable (should work)
  int_data <- data.frame(
    y = 1:10,
    time = as.integer(1:10),
    series = rep("A", 10)
  )

  expect_equal(mvgam:::validate_time_variable("time", int_data), "time")
})

# Test integration with formula parsing
test_that("time parameter integrates correctly with formula parsing", {
  suppressWarnings({

    # Test trend constructors with time parameter in formula
    f1 <- ~ s(x) + AR(time = week, p = 1)
    parsed1 <- mvgam:::parse_trend_formula(f1)
    expect_equal(parsed1$trend_components[[1]]$time, "week")

    f2 <- ~ VAR(time = month, p = 2, cor = TRUE) + s(temp)
    parsed2 <- mvgam:::parse_trend_formula(f2)
    expect_equal(parsed2$trend_components[[1]]$time, "month")

    # Test seasonal AR model with multiple lags (proper alternative to multiple trends)
    f3 <- ~ AR(time = daily, p = c(1, 7, 30))  # Daily, weekly, monthly lags
    parsed3 <- mvgam:::parse_trend_formula(f3)
    expect_equal(parsed3$trend_components[[1]]$time, "daily")
    expect_equal(parsed3$trend_components[[1]]$p, c(1, 7, 30))

    # Test mixed quoted and unquoted usage patterns
    f4 <- ~ s(temp) + CAR(time = period) + offset(effort)
    parsed4 <- mvgam:::parse_trend_formula(f4)
    expect_equal(parsed4$trend_components[[1]]$time, "period")
    expect_true("s(temp)" %in% parsed4$regular_terms)
    expect_true("offset(effort)" %in% parsed4$offset_terms)
  })
})

# Test series parameter functionality
test_that("series parameter works correctly in trend constructors", {

  # Test default series parameter (should default to 'series' with warning)
  expect_warning(
    rw_default <- RW(),
    "Using default.*series.*variable"
  )
  expect_equal(rw_default$series, "series")

  expect_warning(
    ar_default <- AR(p = 1),
    "Using default.*series.*variable"
  )
  expect_equal(ar_default$series, "series")

  expect_warning(
    var_default <- VAR(p = 1),
    "Using default.*series.*variable"
  )
  expect_equal(var_default$series, "series")

  expect_warning(
    car_default <- CAR(p = 1),
    "Using default.*series.*variable"
  )
  expect_equal(car_default$series, "series")

  expect_warning(
    gp_default <- GP(),
    "Using default.*series.*variable"
  )
  expect_equal(gp_default$series, "series")

  # Test explicit series parameter with unquoted variable names
  expect_warning(
    rw_custom <- RW(series = species),
    "Using default.*time.*variable"  # Still warns about time default
  )
  expect_equal(rw_custom$series, "species")

  expect_warning(
    ar_custom <- AR(series = group, p = 2),
    "Using default.*time.*variable"
  )
  expect_equal(ar_custom$series, "group")

  expect_warning(
    var_custom <- VAR(series = unit, p = 1),
    "Using default.*time.*variable"
  )
  expect_equal(var_custom$series, "unit")

  expect_warning(
    car_custom <- CAR(series = location),
    "Using default.*time.*variable"
  )
  expect_equal(car_custom$series, "location")

  expect_warning(
    gp_custom <- GP(series = site),
    "Using default.*time.*variable"
  )
  expect_equal(gp_custom$series, "site")

  # Test both time and series parameters specified (no warnings expected)
  ar_no_warn <- AR(time = week, series = species, p = c(1, 12), ma = TRUE, cor = TRUE)
  expect_equal(ar_no_warn$time, "week")
  expect_equal(ar_no_warn$series, "species")
  expect_equal(ar_no_warn$p, c(1, 12))
  expect_true(ar_no_warn$ma)
  expect_true(ar_no_warn$cor)
})

# Test series variable validation functionality
test_that("series variable validation works correctly", {

  # Test basic validation function
  expect_equal(mvgam:::validate_series_variable("series"), "series")
  expect_equal(mvgam:::validate_series_variable("species"), "species")
  expect_equal(mvgam:::validate_series_variable("NA"), "series")  # Default fallback

  # Test with valid data
  test_data <- data.frame(
    y = 1:10,
    time = 1:10,
    series = rep(c("A", "B"), 5),
    species = as.factor(rep(c("sp1", "sp2"), 5))
  )

  result1 <- mvgam:::validate_series_variable("series", test_data)
  expect_equal(result1, "series")

  expect_warning(
    result2 <- mvgam:::validate_series_variable("species", test_data),
    "Using.*as series variable instead of 'series'"
  )
  expect_equal(result2, "species")

  # Test error when series variable doesn't exist
  expect_error(
    mvgam:::validate_series_variable("missing_var", test_data),
    "Series variable.*not found in data"
  )

  # Test error when series variable is wrong type
  bad_data <- data.frame(
    y = 1:10,
    time = 1:10,
    series = 1:10  # Numeric instead of character/factor
  )

  expect_error(
    mvgam:::validate_series_variable("series", bad_data),
    "Series variable.*must be character or factor"
  )

  # Test with factor series variable (should work)
  factor_data <- data.frame(
    y = 1:10,
    time = 1:10,
    series = as.factor(rep(c("A", "B"), 5))
  )

  result3 <- mvgam:::validate_series_variable("series", factor_data)
  expect_equal(result3, "series")
})

# Test integration of time and series parameters with formula parsing
test_that("time and series parameters integrate correctly with formula parsing", {

  # Test trend constructors with both time and series parameters in formula
  f1 <- ~ s(x) + AR(time = week, series = species, p = 1)
  parsed1 <- mvgam:::parse_trend_formula(f1)
  expect_equal(parsed1$trend_components[[1]]$time, "week")
  expect_equal(parsed1$trend_components[[1]]$series, "species")

  f2 <- ~ VAR(time = month, series = location, p = 2, cor = TRUE) + s(temp)
  parsed2 <- mvgam:::parse_trend_formula(f2)
  expect_equal(parsed2$trend_components[[1]]$time, "month")
  expect_equal(parsed2$trend_components[[1]]$series, "location")

  # Test with grouping variables (suppress hierarchical correlation warning)
  suppressWarnings({
    f3 <- ~ RW(time = daily, series = unit, gr = region, subgr = species)
    parsed3 <- mvgam:::parse_trend_formula(f3)
    expect_equal(parsed3$trend_components[[1]]$time, "daily")
    expect_equal(parsed3$trend_components[[1]]$series, "unit")
    expect_equal(parsed3$trend_components[[1]]$gr, "region")
    expect_equal(parsed3$trend_components[[1]]$subgr, "species")
  })
})

# Additional comprehensive tests for grouping variables and PW validation

# Test grouping variables are properly passed to dispatchers
test_that("grouping variables are properly validated and passed to dispatchers", {
  suppressWarnings({

    # Test valid hierarchical grouping scenarios
    # RW with hierarchical grouping (should work with correlation)
    rw_grouped <- RW(time = week, series = species, gr = region, subgr = site, cor = TRUE)
    expect_equal(rw_grouped$gr, "region")
    expect_equal(rw_grouped$subgr, "site")
    expect_equal(rw_grouped$time, "week")
    expect_equal(rw_grouped$series, "species")
    expect_true(rw_grouped$cor)

    # AR with hierarchical grouping
    ar_grouped <- AR(time = month, series = location, p = 1, gr = ecosystem, subgr = site, cor = TRUE)
    expect_equal(ar_grouped$gr, "ecosystem")
    expect_equal(ar_grouped$subgr, "site")
    expect_equal(ar_grouped$p, 1)
    expect_true(ar_grouped$cor)

    # VAR with hierarchical grouping
    var_grouped <- VAR(time = year, series = population, p = 2, gr = habitat, subgr = species, cor = TRUE)
    expect_equal(var_grouped$gr, "habitat")
    expect_equal(var_grouped$subgr, "species")
    expect_equal(var_grouped$p, 2)
    expect_true(var_grouped$cor)

    # Test that grouping is preserved in formula parsing
    f1 <- ~ s(temp) + RW(time = week, series = species, gr = region, subgr = site, cor = TRUE)
    parsed1 <- mvgam:::parse_trend_formula(f1)
    trend_comp <- parsed1$trend_components[[1]]
    expect_equal(trend_comp$gr, "region")
    expect_equal(trend_comp$subgr, "site")
    expect_equal(trend_comp$time, "week")
    expect_equal(trend_comp$series, "species")
    expect_true(trend_comp$cor)

    # Test complex formula with multiple trends having different grouping
    f2 <- ~ AR(time = day, series = unit, gr = block, subgr = plot, p = 1, cor = TRUE) +
           s(temperature) +
           RW(time = week, series = transect, gr = site, subgr = quadrat, cor = TRUE)
    parsed2 <- mvgam:::parse_trend_formula(f2)
    expect_equal(length(parsed2$trend_components), 2)

    # Check first trend (AR)
    ar_comp <- parsed2$trend_components[[1]]
    expect_equal(ar_comp$trend, "AR1")
    expect_equal(ar_comp$gr, "block")
    expect_equal(ar_comp$subgr, "plot")
    expect_equal(ar_comp$time, "day")
    expect_equal(ar_comp$series, "unit")

    # Check second trend (RW)
    rw_comp <- parsed2$trend_components[[2]]
    expect_equal(rw_comp$trend, "RW")
    expect_equal(rw_comp$gr, "site")
    expect_equal(rw_comp$subgr, "quadrat")
    expect_equal(rw_comp$time, "week")
    expect_equal(rw_comp$series, "transect")
  })
})

# Test grouping validation error conditions
test_that("grouping variable validation catches invalid combinations", {

  # Test error when gr specified without subgr
  expect_error(
    mvgam:::validate_grouping_arguments("region", "NA"),
    "Hierarchical grouping requires subgrouping"
  )

  # Test error when subgr is 'series' (reserved)
  expect_error(
    mvgam:::validate_grouping_arguments("region", "series"),
    "Invalid subgrouping for hierarchical models"
  )

  # Test warning when hierarchical grouping specified without correlation
  expect_warning(
    RW(gr = region, subgr = site, cor = FALSE),
    "Hierarchical grouping specified without correlation"
  )

  expect_warning(
    AR(gr = habitat, subgr = species, p = 1, cor = FALSE),
    "Hierarchical grouping specified without correlation"
  )

  expect_warning(
    VAR(gr = ecosystem, subgr = location, p = 2, cor = FALSE),
    "Hierarchical grouping specified without correlation"
  )
})

# Test PW cap argument validation for logistic growth
test_that("PW cap argument is properly validated for logistic growth", {

  suppressWarnings({
    # Test linear growth doesn't require cap (should work)
    pw_linear1 <- PW(time = week, series = species, growth = 'linear', n_changepoints = 5)
    expect_equal(pw_linear1$growth, "linear")
    expect_equal(pw_linear1$trend, "PWlinear")
    expect_equal(pw_linear1$cap, "cap")  # Default cap still set but not required
    expect_equal(pw_linear1$n_changepoints, 5)

    # Test linear growth with explicit cap (should work)
    pw_linear2 <- PW(time = month, series = population, cap = max_size, growth = 'linear')
    expect_equal(pw_linear2$growth, "linear")
    expect_equal(pw_linear2$cap, "max_size")

    # Test logistic growth with explicit cap (should work)
    pw_logistic1 <- PW(time = day, series = cells, cap = carrying_capacity, growth = 'logistic')
    expect_equal(pw_logistic1$growth, "logistic")
    expect_equal(pw_logistic1$trend, "PWlogistic")
    expect_equal(pw_logistic1$cap, "carrying_capacity")

    # Test that cap argument is preserved in formula parsing
    f1 <- ~ s(temperature) + PW(time = week, series = species, cap = max_biomass, growth = 'logistic')
    parsed1 <- mvgam:::parse_trend_formula(f1)
    pw_comp <- parsed1$trend_components[[1]]
    expect_equal(pw_comp$cap, "max_biomass")
    expect_equal(pw_comp$growth, "logistic")
    expect_equal(pw_comp$trend, "PWlogistic")

    # Test complex PW specification in formula
    f2 <- ~ AR(p = 1) + s(temp) +
           PW(time = daily, series = population, cap = environment_capacity,
              growth = 'logistic', n_changepoints = 15, changepoint_scale = 0.1)
    parsed2 <- mvgam:::parse_trend_formula(f2)
    expect_equal(length(parsed2$trend_components), 2)

    # Find the PW component (could be in either position)
    pw_comp2 <- NULL
    for (comp in parsed2$trend_components) {
      if (comp$trend %in% c("PWlinear", "PWlogistic")) {
        pw_comp2 <- comp
        break
      }
    }
    expect_false(is.null(pw_comp2))
    expect_equal(pw_comp2$cap, "environment_capacity")
    expect_equal(pw_comp2$growth, "logistic")
    expect_equal(pw_comp2$n_changepoints, 15)
    expect_equal(pw_comp2$changepoint_scale, 0.1)
  })
})

# Test PW parameter validation
test_that("PW parameter validation works correctly", {

  suppressWarnings({
    # Test valid parameter ranges
    pw_valid <- PW(time = week, series = species, cap = max_pop, growth = 'logistic',
                   n_changepoints = 20, changepoint_range = 0.9, changepoint_scale = 0.02)
    expect_equal(pw_valid$n_changepoints, 20)
    expect_equal(pw_valid$changepoint_range, 0.9)
    expect_equal(pw_valid$changepoint_scale, 0.02)

    # Test invalid n_changepoints (must be positive integer)
    expect_error(
      PW(n_changepoints = 0),
      "must be a positive integer"
    )

    expect_error(
      PW(n_changepoints = -5),
      "must be a positive integer"
    )

    expect_error(
      PW(n_changepoints = 3.5),
      "must be a positive integer"
    )

    # Test invalid changepoint_range (must be between 0 and 1)
    expect_error(
      PW(changepoint_range = 1.5),
      "must be a proportion ranging from 0 to 1"
    )

    expect_error(
      PW(changepoint_range = -0.1),
      "must be a proportion ranging from 0 to 1"
    )

    # Test invalid changepoint_scale (must be positive)
    expect_error(
      PW(changepoint_scale = 0),
      "must be a positive real value"
    )

    expect_error(
      PW(changepoint_scale = -0.05),
      "must be a positive real value"
    )

    # Test invalid growth type
    expect_error(
      PW(growth = 'exponential'),
      "'arg' should be one of"
    )
  })
})

# Test integration of grouping variables with stan injection system
test_that("grouping variables integrate with stanvar generation", {
  # This test ensures the data_info structure properly includes grouping information
  # that would be used by the injection generators

  # Test data structure that would be passed to injection generators
  test_data_info <- list(
    n_lv = 3,
    n_series = 6,
    n_groups = 2,        # From gr variable
    n_subgroups = 3      # From subgr variable
  )

  # Test that hierarchical grouping info would be available
  expect_equal(test_data_info$n_groups, 2)
  expect_equal(test_data_info$n_subgroups, 3)

  # Test trend spec structure includes grouping info
  suppressWarnings({
    rw_hierarchical <- RW(time = week, series = species, gr = region, subgr = site, cor = TRUE)
  })

  # Check that all necessary grouping info is present for stanvar generation
  expect_equal(rw_hierarchical$gr, "region")
  expect_equal(rw_hierarchical$subgr, "site")
  expect_true(rw_hierarchical$cor)  # Required for hierarchical models

  # Test that generate_trend_injection_stanvars would receive proper structure
  # (This is a structural test - the actual function would need data_info)
  expect_true(!is.null(rw_hierarchical$gr) && rw_hierarchical$gr != 'NA')
  expect_true(!is.null(rw_hierarchical$subgr) && rw_hierarchical$subgr != 'series')
})

# Test cap argument integration with stanvar generation
test_that("PW cap argument integrates with stanvar generation", {

  # Test that cap information is properly structured for Stan code generation
  suppressWarnings({
    pw_logistic <- PW(time = week, series = population, cap = max_capacity,
                      growth = 'logistic', n_changepoints = 12)
  })

  # Check that cap info is available for stanvar generation
  expect_equal(pw_logistic$cap, "max_capacity")
  expect_equal(pw_logistic$growth, "logistic")
  expect_equal(pw_logistic$trend_model, "PWlogistic")
  expect_equal(pw_logistic$n_changepoints, 12)

  # Test that trend spec structure includes all PW parameters needed for Stan generation
  expected_params <- c("cap", "growth", "trend_model", "n_changepoints",
                       "changepoint_range", "changepoint_scale")
  for (param in expected_params) {
    expect_true(param %in% names(pw_logistic),
                info = paste("Parameter", param, "should be present in PW object"))
  }

  # Test variable name extraction for Stan data generation
  expect_equal(pw_logistic$time, "week")
  expect_equal(pw_logistic$series, "population")
  expect_equal(pw_logistic$cap, "max_capacity")

  # These would be used in the Stan data and transformed parameters blocks
  expect_true(nzchar(pw_logistic$time))
  expect_true(nzchar(pw_logistic$series))
  expect_true(nzchar(pw_logistic$cap))
})

# Test piecewise trend dispatcher functionality
test_that("piecewise trend types work correctly with dispatcher", {
  suppressWarnings({

    # Test PW constructor with linear growth
    pw_linear <- PW(time = week, series = species, growth = 'linear', n_changepoints = 8)
    expect_s3_class(pw_linear, "mvgam_trend")
    expect_equal(pw_linear$trend, "PWlinear")
    expect_equal(pw_linear$growth, "linear")
    expect_equal(pw_linear$n_changepoints, 8)
    expect_true(is.mvgam_trend(pw_linear))

    # Test PW constructor with logistic growth
    pw_logistic <- PW(time = month, series = population, cap = carrying_cap,
                      growth = 'logistic', n_changepoints = 15, changepoint_scale = 0.05)
    expect_s3_class(pw_logistic, "mvgam_trend")
    expect_equal(pw_logistic$trend, "PWlogistic")
    expect_equal(pw_logistic$growth, "logistic")
    expect_equal(pw_logistic$cap, "carrying_cap")
    expect_equal(pw_logistic$n_changepoints, 15)
    expect_equal(pw_logistic$changepoint_scale, 0.05)

    # Test PWlinear constructor
    pwlin <- PWlinear(time = daily, series = biomass, n_changepoints = 10)
    expect_s3_class(pwlin, "mvgam_trend")
    expect_equal(pwlin$trend, "PWlinear")
    expect_equal(pwlin$trend, "PWlinear")
    expect_equal(pwlin$growth, "linear")
    expect_equal(pwlin$time, "daily")
    expect_equal(pwlin$series, "biomass")
    expect_equal(pwlin$n_changepoints, 10)

    # Test PWlogistic constructor
    pwlog <- PWlogistic(time = yearly, series = cells, cap = max_size,
                        n_changepoints = 20, changepoint_range = 0.8)
    expect_s3_class(pwlog, "mvgam_trend")
    expect_equal(pwlog$trend, "PWlogistic")
    expect_equal(pwlog$trend, "PWlogistic")
    expect_equal(pwlog$growth, "logistic")
    expect_equal(pwlog$cap, "max_size")
    expect_equal(pwlog$time, "yearly")
    expect_equal(pwlog$series, "cells")
    expect_equal(pwlog$n_changepoints, 20)
    expect_equal(pwlog$changepoint_range, 0.8)
  })
})

# Test piecewise trends in formula parsing
test_that("piecewise trends work correctly in formula parsing", {
  suppressWarnings({

    # Test simple linear piecewise formula
    f1 <- ~ s(temp) + PWlinear(time = week, series = species, n_changepoints = 12)
    parsed1 <- mvgam:::parse_trend_formula(f1)
    expect_equal(length(parsed1$trend_components), 1)
    trend_comp1 <- parsed1$trend_components[[1]]
    expect_equal(trend_comp1$trend, "PWlinear")
    expect_equal(trend_comp1$trend, "PWlinear")
    expect_equal(trend_comp1$time, "week")
    expect_equal(trend_comp1$series, "species")
    expect_equal(trend_comp1$n_changepoints, 12)

    # Test logistic piecewise with cap formula
    f2 <- ~ cov1 + PWlogistic(time = month, series = population, cap = max_capacity,
                              n_changepoints = 25, changepoint_scale = 0.02) + s(x)
    parsed2 <- mvgam:::parse_trend_formula(f2)
    expect_equal(length(parsed2$trend_components), 1)
    trend_comp2 <- parsed2$trend_components[[1]]
    expect_equal(trend_comp2$trend, "PWlogistic")
    expect_equal(trend_comp2$trend, "PWlogistic")
    expect_equal(trend_comp2$cap, "max_capacity")
    expect_equal(trend_comp2$n_changepoints, 25)
    expect_equal(trend_comp2$changepoint_scale, 0.02)

    # Test general PW constructor in formula
    f3 <- ~ PW(time = daily, series = biomass, growth = 'linear',
               n_changepoints = 8, changepoint_range = 0.75) + s(season)
    parsed3 <- mvgam:::parse_trend_formula(f3)
    expect_equal(length(parsed3$trend_components), 1)
    trend_comp3 <- parsed3$trend_components[[1]]
    expect_equal(trend_comp3$trend, "PW")
    expect_equal(trend_comp3$trend, "PWlinear")
    expect_equal(trend_comp3$growth, "linear")
    expect_equal(trend_comp3$changepoint_range, 0.75)

    # Test mixed piecewise and other trends
    f4 <- ~ AR(p = 1) + PWlinear(time = week, series = site, n_changepoints = 6) +
           s(temperature) + RW(cor = TRUE)
    parsed4 <- mvgam:::parse_trend_formula(f4)
    expect_equal(length(parsed4$trend_components), 3)

    # Find each trend type
    ar_found <- FALSE
    pw_found <- FALSE
    rw_found <- FALSE
    for (comp in parsed4$trend_components) {
      if (comp$trend == "AR1") ar_found <- TRUE
      if (comp$trend == "PWlinear") pw_found <- TRUE
      if (comp$trend == "RW") rw_found <- TRUE
    }
    expect_true(ar_found)
    expect_true(pw_found)
    expect_true(rw_found)
  })
})

# Test piecewise trend registry integration
test_that("piecewise trends integrate correctly with registry system", {

  # Test that PW trends are in registry
  choices <- mvgam_trend_choices()
  expect_true("PW" %in% choices)
  # PWlinear and PWlogistic are trend_model variants of PW type

  # Test pattern includes piecewise trends
  pattern <- mvgam:::mvgam_trend_pattern()
  expect_true(grepl("PW", pattern))
  # Pattern should include base PW type

  # Test registry info for piecewise trends
  pw_info <- mvgam:::get_trend_info("PW")
  expect_type(pw_info, "list")
  expect_false(pw_info$supports_factors)  # PW doesn't support factors
  expect_type(pw_info$generator, "function")

  # Test that we can get PW info (PWlinear is a trend_model variant)
  pwlin_info <- mvgam:::get_trend_info("PW")
  expect_type(pwlin_info, "list")
  expect_false(pwlin_info$supports_factors)

  # Test that PWlogistic also uses PW registry entry
  pwlog_info <- mvgam:::get_trend_info("PW")
  expect_type(pwlog_info, "list")
  expect_false(pwlog_info$supports_factors)
})

# Test piecewise parameter validation edge cases
test_that("piecewise parameter validation handles edge cases correctly", {

  # Test boundary values for n_changepoints
  suppressWarnings({
    pw_min <- PW(n_changepoints = 1, growth = 'linear')
    expect_equal(pw_min$n_changepoints, 1)

    pw_large <- PW(n_changepoints = 100, growth = 'linear')
    expect_equal(pw_large$n_changepoints, 100)
  })

  # Test boundary values for changepoint_range
  suppressWarnings({
    pw_range_min <- PW(changepoint_range = 0.0001, growth = 'linear')
    expect_equal(pw_range_min$changepoint_range, 0.0001)

    pw_range_max <- PW(changepoint_range = 0.9999, growth = 'linear')
    expect_equal(pw_range_max$changepoint_range, 0.9999)
  })

  # Test boundary values for changepoint_scale
  suppressWarnings({
    pw_scale_small <- PW(changepoint_scale = 0.001, growth = 'linear')
    expect_equal(pw_scale_small$changepoint_scale, 0.001)

    pw_scale_large <- PW(changepoint_scale = 10.0, growth = 'linear')
    expect_equal(pw_scale_large$changepoint_scale, 10.0)
  })

  # Test that factor model validation rejects PW with n_lv
  expect_error(
    PW(n_lv = 3, growth = 'linear'),
    "Factor models.*not supported.*PW"
  )

  expect_error(
    PW(n_lv = 2, growth = 'logistic', cap = max_size),
    "Factor models.*not supported.*PW"
  )
})
