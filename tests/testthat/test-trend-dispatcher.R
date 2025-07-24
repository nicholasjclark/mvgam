#' Tests for Trend Dispatcher System
#'
#' Comprehensive tests for the trend type registry, validation, and formula
#' parsing functionality. Includes edge cases based on brms testing patterns.

# Setup - ensure we have access to required functions
library(checkmate)
library(insight)
library(rlang)

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
  pattern <- mvgam_trend_pattern()
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

# Test grouping validation helper
test_that("grouping validation helper works correctly", {
  
  # Test default case
  result1 <- validate_grouping_arguments("NA", "NA")
  expect_equal(result1$gr, "NA")
  expect_equal(result1$subgr, "series")
  
  # Test valid hierarchical case would work (but we test error cases)
  expect_error(
    validate_grouping_arguments("region", "NA"),
    "Hierarchical grouping requires subgrouping"
  )
  
  expect_error(
    validate_grouping_arguments("region", "series"),
    "Invalid subgrouping for hierarchical models"
  )
})

# Test formula parsing - basic functionality
test_that("basic formula parsing works", {
  
  # Test simple trend-only formula
  f1 <- ~ RW()
  parsed1 <- parse_trend_formula(f1)
  
  expect_equal(length(parsed1$trend_components), 1)
  expect_s3_class(parsed1$trend_components[[1]], "mvgam_trend")
  expect_equal(parsed1$trend_components[[1]]$trend, "RW")
  expect_equal(parsed1$base_formula, ~ 1)
  expect_equal(parsed1$trend_terms, "RW()")
  expect_equal(length(parsed1$regular_terms), 0)
  
  # Test formula with regular predictors and trend
  f2 <- ~ s(time) + cov1 + AR(p = 1)
  parsed2 <- parse_trend_formula(f2)
  
  expect_equal(length(parsed2$trend_components), 1)
  expect_equal(parsed2$trend_components[[1]]$trend, "AR1")
  expect_equal(parsed2$trend_terms, "AR(p = 1)")
  expect_true("s(time)" %in% parsed2$regular_terms)
  expect_true("cov1" %in% parsed2$regular_terms)
  expect_equal(length(parsed2$regular_terms), 2)
})

# Test formula parsing - order independence (key requirement)
test_that("formula parsing is order-independent", {
  
  # Create formulas with different orderings
  f1 <- ~ s(time) + cov1 + RW(cor = TRUE) + cov2
  f2 <- ~ RW(cor = TRUE) + s(time) + cov2 + cov1  
  f3 <- ~ cov1 + cov2 + s(time) + RW(cor = TRUE)
  f4 <- ~ cov2 + RW(cor = TRUE) + cov1 + s(time)
  
  # Parse all versions
  parsed1 <- parse_trend_formula(f1)
  parsed2 <- parse_trend_formula(f2)
  parsed3 <- parse_trend_formula(f3)
  parsed4 <- parse_trend_formula(f4)
  
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

# Test formula parsing - multiple trends
test_that("multiple trend components are handled correctly", {
  
  # Test formula with multiple trends in different orders
  f1 <- ~ s(season) + RW() + AR(p = 2) + cov1
  f2 <- ~ AR(p = 2) + cov1 + RW() + s(season)
  
  parsed1 <- parse_trend_formula(f1)
  parsed2 <- parse_trend_formula(f2)
  
  # Should have two trend components
  expect_equal(length(parsed1$trend_components), 2)
  expect_equal(length(parsed2$trend_components), 2)
  
  # Trend model should be NULL for multiple trends
  expect_null(parsed1$trend_model)
  expect_null(parsed2$trend_model)
  
  # Should have same trend terms (order may differ)
  expect_setequal(parsed1$trend_terms, parsed2$trend_terms)
  expect_true("RW()" %in% parsed1$trend_terms)
  expect_true("AR(p = 2)" %in% parsed1$trend_terms)
  
  # Regular terms should be the same
  expect_setequal(parsed1$regular_terms, parsed2$regular_terms)
  expect_true("s(season)" %in% parsed1$regular_terms)
  expect_true("cov1" %in% parsed1$regular_terms)
})

# Test edge cases inspired by brms testing
test_that("formula parsing handles edge cases correctly", {
  
  # Test nested parentheses and complex expressions
  f1 <- ~ s(time, k = c(5, 10)) + AR(p = c(1, 12)) + poly(x, degree = 2)
  parsed1 <- parse_trend_formula(f1)
  expect_equal(parsed1$trend_terms, "AR(p = c(1, 12))")
  expect_true("s(time, k = c(5, 10))" %in% parsed1$regular_terms)
  expect_true("poly(x, degree = 2)" %in% parsed1$regular_terms)
  
  # Test interaction terms
  f2 <- ~ cov1:cov2 + s(time)*group + VAR(p = 1, ma = TRUE)
  parsed2 <- parse_trend_formula(f2)
  expect_equal(parsed2$trend_terms, "VAR(p = 1, ma = TRUE)")
  expect_true("cov1:cov2" %in% parsed2$regular_terms)
  expect_true("s(time)*group" %in% parsed2$regular_terms)
  
  # Test special characters and operators in regular terms
  f3 <- ~ I(x^2) + log(y + 1) + RW(ma = FALSE) + offset(z)
  parsed3 <- parse_trend_formula(f3)
  expect_equal(parsed3$trend_terms, "RW(ma = FALSE)")
  expect_true("I(x^2)" %in% parsed3$regular_terms)
  expect_true("log(y + 1)" %in% parsed3$regular_terms)
  expect_true("offset(z)" %in% parsed3$regular_terms)
  
  # Test whitespace handling
  f4 <- ~   s(time)   +   RW(cor=TRUE)   +   cov1   
  parsed4 <- parse_trend_formula(f4)
  expect_equal(parsed4$trend_terms, "RW(cor=TRUE)")
  expect_setequal(parsed4$regular_terms, c("s(time)", "cov1"))
})

# Test malformed formulas and error conditions
test_that("formula parsing error handling works comprehensively", {
  
  # Test empty formula (different from ~ 1)
  expect_error(
    parse_trend_formula(~ 1),
    "No trend model specified"
  )
  
  # Test formula with response variable
  expect_error(
    parse_trend_formula(y ~ RW()),
    "Response variable not allowed"
  )
  
  # Test formula with no trend constructors
  expect_error(
    parse_trend_formula(~ s(time) + cov1),
    "No trend model specified"
  )
  
  # Test truly empty formula
  expect_error(
    parse_trend_formula(~ .),
    "No trend model specified"
  )
  
  # Test invalid trend constructor calls
  expect_error(
    eval_trend_constructor("INVALID_TREND()"),
    "Failed to evaluate trend constructor"
  )
  
  # Test malformed trend constructor syntax
  expect_error(
    eval_trend_constructor("RW(invalid_param = )"),
    "Failed to evaluate trend constructor"
  )
  
  # Test unbalanced parentheses (should be caught by R's parser)
  expect_error(
    eval_trend_constructor("RW(cor = TRUE"),
    "Failed to evaluate trend constructor"
  )
})

# Test boundary conditions and special inputs
test_that("boundary conditions are handled correctly", {
  
  # Test formula with only intercept and trend
  f1 <- ~ 1 + RW()
  parsed1 <- parse_trend_formula(f1)
  expect_equal(parsed1$base_formula, ~ 1)
  expect_equal(length(parsed1$trend_components), 1)
  
  # Test formula with very long expressions
  long_expr <- paste0("s(x", 1:50, ")", collapse = " + ")
  f2 <- as.formula(paste("~", long_expr, "+ AR(p = 1)"))
  parsed2 <- parse_trend_formula(f2)
  expect_equal(length(parsed2$trend_components), 1)
  expect_equal(length(parsed2$regular_terms), 50)
  
  # Test formula with repeated trend constructors
  f3 <- ~ RW() + s(time) + RW(ma = TRUE)
  parsed3 <- parse_trend_formula(f3)
  expect_equal(length(parsed3$trend_components), 2)
  expect_setequal(parsed3$trend_terms, c("RW()", "RW(ma = TRUE)"))
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
  
  found_trends <- find_trend_terms(complex_terms)
  expect_true("AR(p = c(1, 12, 24), ma = TRUE, cor = FALSE)" %in% found_trends)
  expect_true("VAR(p = 10, cor = TRUE)" %in% found_trends)
  expect_true("RW(ma = FALSE)" %in% found_trends)
  expect_true("GP()" %in% found_trends)
  expect_false("s(time, bs = 'tp', k = 20)" %in% found_trends)
  
  # Test extraction of regular terms from complex expressions
  regular_terms <- extract_regular_terms(complex_terms)
  expect_false("AR(p = c(1, 12, 24), ma = TRUE, cor = FALSE)" %in% regular_terms)
  expect_true("s(time, bs = 'tp', k = 20)" %in% regular_terms)
  
  # Test edge case: trend-like names that aren't trend constructors
  fake_trends <- c("ARbitrary()", "VARious()", "RWanda()", "s(time)")
  found_fake <- find_trend_terms(fake_trends)
  expect_equal(length(found_fake), 0)
})

# Test validation system edge cases
test_that("validation system handles edge cases", {
  
  # Test correlation requirements with edge cases
  expect_true(validate_correlation_requirements("region", FALSE))
  expect_false(validate_correlation_requirements("NA", FALSE))
  expect_true(validate_correlation_requirements("group", TRUE))
  
  # Test invalid parameter combinations through constructors
  expect_error(
    AR(p = 0),
    class = "checkmate_error"
  )
  
  expect_error(
    AR(p = c(1, 1, 2)),  # Non-unique lags
    class = "checkmate_error"
  )
  
  expect_error(
    VAR(p = -1),
    class = "checkmate_error"
  )
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
  
  # Register the custom trend
  register_custom_trend("CUSTOM", custom_constructor)
  
  # Test it appears in registry
  choices <- mvgam_trend_choices()
  expect_true("CUSTOM" %in% choices)
  
  # Test pattern includes custom trend
  pattern <- mvgam_trend_pattern()
  expect_true(grepl("CUSTOM", pattern))
  
  # Test formula parsing with custom trend
  f1 <- ~ s(time) + CUSTOM(param = 5) + cov1
  # Note: This would require the custom constructor to be available
  # in the evaluation environment
})

# Test print method edge cases
test_that("print method handles all configurations", {
  
  # Test minimal configuration
  minimal_trend <- RW()
  expect_output(print(minimal_trend), "mvgam trend specification")
  expect_output(print(minimal_trend), "Type: RW")
  
  # Test maximal configuration
  complex_trend <- AR(p = c(1, 12), ma = TRUE, cor = TRUE, n_lv = 2)
  output <- capture.output(print(complex_trend))
  expect_true(any(grepl("Dynamic factors: 2", output)))
  expect_true(any(grepl("Moving average: enabled", output)))
  expect_true(any(grepl("Correlation: enabled", output)))
  
  # Test hierarchical grouping display
  # (Would need to create such an object through the constructor)
})

# Test complex real-world scenarios
test_that("realistic complex formulas work correctly", {
  
  # Test seasonal model with multiple components
  seasonal_formula <- ~ s(doy, bs = "cc", k = 12) + 
                       s(temp, k = 10) + 
                       factor(month) + 
                       AR(p = c(1, 12, 24), ma = TRUE) +
                       offset(log_effort)
  
  parsed <- parse_trend_formula(seasonal_formula)
  expect_equal(length(parsed$trend_components), 1)
  expect_equal(parsed$trend_components[[1]]$p, c(1, 12, 24))
  expect_true(parsed$trend_components[[1]]$ma)
  expect_equal(length(parsed$regular_terms), 4)
  
  # Test multivariate model formula
  multivar_formula <- ~ s(time, by = species, k = 20) +
                        habitat +
                        VAR(p = 2, cor = TRUE) +
                        s(temperature, species, bs = "fs")
  
  parsed2 <- parse_trend_formula(multivar_formula)
  expect_equal(parsed2$trend_components[[1]]$trend, "VAR2")
  expect_true(parsed2$trend_components[[1]]$cor)
  expect_true("s(time, by = species, k = 20)" %in% parsed2$regular_terms)
  expect_true("s(temperature, species, bs = \"fs\")" %in% parsed2$regular_terms)
})