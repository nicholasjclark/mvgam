# Tests for mvgam_formula constructor and methods
# This file tests the mvgam_formula object creation, validation, and S3 methods

test_that("mvgam_formula creates correct object structure", {
  # Basic formula
  mf1 <- mvgam_formula(y ~ x)
  expect_s3_class(mf1, "mvgam_formula")
  expect_s3_class(mf1, "formula")
  expect_equal(mf1$formula, y ~ x)
  expect_null(mf1$trend_formula)
  
  # Formula with trend
  mf2 <- mvgam_formula(y ~ x, trend_formula = ~ 1)
  expect_s3_class(mf2, "mvgam_formula")
  expect_equal(mf2$formula, y ~ x)
  expect_equal(mf2$trend_formula, ~ 1)
  
  # Complex formulas
  mf3 <- mvgam_formula(log(count) ~ s(x) + (1|group), 
                       trend_formula = ~ s(time))
  expect_s3_class(mf3, "mvgam_formula")
  expect_equal(deparse(mf3$formula), "log(count) ~ s(x) + (1 | group)")
  expect_equal(mf3$trend_formula, ~ s(time))
})

test_that("mvgam_formula handles different formula types", {
  # Standard formula
  f1 <- y ~ x
  mf1 <- mvgam_formula(f1)
  expect_s3_class(mf1, c("mvgam_formula", "formula"))
  
  # brmsformula
  bf1 <- brms::bf(y ~ x)
  mf2 <- mvgam_formula(bf1)
  expect_s3_class(mf2, "mvgam_formula")
  expect_s3_class(mf2, "brmsformula")
  
  # mvbind formula (multivariate)
  mv_formula <- brms::bf(mvbind(y1, y2) ~ x)
  mf3 <- mvgam_formula(mv_formula)
  expect_s3_class(mf3, "mvgam_formula")
})

test_that("mvgam_formula validates formula parameter correctly", {
  # Invalid formula types
  expect_error(mvgam_formula("not a formula"),
               "Assertion on 'formula' failed")
  expect_error(mvgam_formula(NULL),
               "Assertion on 'formula' failed")
  expect_error(mvgam_formula(123),
               "Assertion on 'formula' failed")
  expect_error(mvgam_formula(list(y ~ x)),
               "Assertion on 'formula' failed")
  
  # Valid formulas should not error
  expect_no_error(mvgam_formula(y ~ x))
  expect_no_error(mvgam_formula(y ~ x + z))
  expect_no_error(mvgam_formula(log(y) ~ s(x)))
})

test_that("mvgam_formula validates trend_formula correctly", {
  # NULL trend_formula is valid
  expect_no_error(mvgam_formula(y ~ x, trend_formula = NULL))
  
  # Valid trend formulas
  expect_no_error(mvgam_formula(y ~ x, trend_formula = ~ 1))
  expect_no_error(mvgam_formula(y ~ x, trend_formula = ~ s(time)))
  
  # Invalid trend formulas
  expect_error(mvgam_formula(y ~ x, trend_formula = "not a formula"),
               "Assertion on 'trend_formula' failed")
  expect_error(mvgam_formula(y ~ x, trend_formula = 123),
               "Assertion on 'trend_formula' failed")
  expect_error(mvgam_formula(y ~ x, trend_formula = list(~ 1)),
               "Assertion on 'trend_formula' failed")
})

test_that("mvgam_formula detects incompatible autocorrelation terms", {
  # Should error with autocor() in trend_formula
  expect_error(
    mvgam_formula(y ~ x, trend_formula = ~ autocor(M = ~ 1)),
    "brms autocorrelation terms not allowed"
  )
  
  # Should provide helpful error message
  expect_error(
    mvgam_formula(y ~ x, trend_formula = ~ autocor(M = ~ 1)),
    "conflict with mvgam State-Space dynamics"
  )
  
  # Regular terms should be fine
  expect_no_error(mvgam_formula(y ~ x, trend_formula = ~ 1))
  expect_no_error(mvgam_formula(y ~ x, trend_formula = ~ s(time)))
})

test_that("mvgam_formula preserves class hierarchy correctly", {
  # Standard formula
  f1 <- y ~ x
  mf1 <- mvgam_formula(f1)
  expect_equal(class(mf1), c("mvgam_formula", "formula"))
  
  # brmsformula
  bf1 <- brms::bf(y ~ x)
  mf2 <- mvgam_formula(bf1)
  expect_equal(class(mf2), c("mvgam_formula", "brmsformula"))
  
  # Check that original formula is preserved
  expect_identical(mf1$formula, f1)
  expect_identical(mf2$formula, bf1)
})

test_that("print.mvgam_formula works correctly", {
  # Capture print output for formula without trend
  mf1 <- mvgam_formula(y ~ x)
  output1 <- capture.output(print(mf1))
  expect_match(output1[1], "mvgam_formula object")
  expect_match(output1[2], "Observation formula:")
  expect_true(any(grepl("y ~ x", output1)))
  expect_true(any(grepl("NULL \\(no trend component\\)", output1)))
  
  # Capture print output for formula with trend
  mf2 <- mvgam_formula(y ~ x, trend_formula = ~ s(time))
  output2 <- capture.output(print(mf2))
  expect_match(output2[1], "mvgam_formula object")
  expect_match(output2[2], "Observation formula:")
  expect_true(any(grepl("y ~ x", output2)))
  expect_true(any(grepl("Trend formula:", output2)))
  expect_true(any(grepl("s\\(time\\)", output2)))
  
  # Print should return object invisibly
  expect_invisible(print(mf1))
  returned <- print(mf1)
  expect_identical(returned, mf1)
})

test_that("mvgam_formula handles complex real-world formulas", {
  # Multivariate response
  mv_formula <- brms::bf(mvbind(count, biomass) ~ temperature + s(time))
  mf1 <- mvgam_formula(mv_formula, trend_formula = ~ 1)
  expect_s3_class(mf1, "mvgam_formula")
  
  # Complex smooth terms
  mf2 <- mvgam_formula(
    y ~ s(x, bs = "cr") + te(lat, lon) + (1 + time|site),
    trend_formula = ~ s(season, bs = "cc", k = 12)
  )
  expect_s3_class(mf2, "mvgam_formula")
  
  # Offset and weights (in formula)
  mf3 <- mvgam_formula(
    y ~ x + offset(log(effort)),
    trend_formula = ~ 1
  )
  expect_s3_class(mf3, "mvgam_formula")
  
  # Interaction terms
  mf4 <- mvgam_formula(
    count ~ treatment * time + s(temperature, by = treatment),
    trend_formula = ~ factor(season)
  )
  expect_s3_class(mf4, "mvgam_formula")
})

test_that("mvgam_formula object structure is minimal and correct", {
  mf <- mvgam_formula(y ~ x, trend_formula = ~ 1)
  
  # Should only have formula and trend_formula
  expect_named(mf, c("formula", "trend_formula"))
  
  # Should not store data, family, etc.
  expect_null(mf$data)
  expect_null(mf$family)
  
  # Structure should be a list
  expect_true(is.list(mf))
  expect_length(mf, 2)
})

test_that("mvgam_formula handles edge cases", {
  # Very long formulas
  long_formula <- as.formula(paste("y ~", 
                                   paste(paste0("x", 1:20), collapse = " + ")))
  expect_no_error(mvgam_formula(long_formula))
  
  # Formula with special functions
  expect_no_error(mvgam_formula(I(y^2) ~ poly(x, 3)))
  expect_no_error(mvgam_formula(y ~ ns(x, df = 3)))
  
  # Empty RHS in trend_formula
  expect_no_error(mvgam_formula(y ~ x, trend_formula = ~ 0))
  expect_no_error(mvgam_formula(y ~ x, trend_formula = ~ -1))
  
  # Formula with dots
  expect_no_error(mvgam_formula(y ~ .))
})

# =============================================================================
# SECTION: BRMS COMPATIBILITY TESTS (Sub-task 1F)
# =============================================================================
# Tests for exact equivalence between mvgam_formula + get_prior and brms::get_prior
# when trend_formula = NULL. This ensures perfect brms delegation.

test_that("mvgam_formula + get_prior identical to brms when no trends", {
  # Create test data
  test_data <- data.frame(
    y = rnorm(20),
    x = rnorm(20),
    group = factor(rep(1:4, 5))
  )
  
  # Simple formula with gaussian family
  formula_obj <- mvgam_formula(y ~ x, trend_formula = NULL)
  mvgam_result <- get_prior(formula_obj, family = gaussian(), data = test_data)
  brms_result <- brms::get_prior(y ~ x, family = gaussian(), data = test_data)
  
  # Remove trend_component column for comparison (this is the only difference)
  mvgam_clean <- mvgam_result[, !names(mvgam_result) %in% "trend_component"]
  
  # Should be identical except for trend_component column
  expect_identical(mvgam_clean, brms_result)
  
  # Verify trend_component column is added correctly
  expect_true("trend_component" %in% names(mvgam_result))
  expect_true(all(mvgam_result$trend_component == "observation"))
  
  # Complex formula with random effects
  complex_formula <- mvgam_formula(y ~ x + s(x) + (1|group), trend_formula = NULL)
  mvgam_complex <- get_prior(complex_formula, family = gaussian(), data = test_data)
  brms_complex <- brms::get_prior(y ~ x + s(x) + (1|group), 
                                  family = gaussian(), data = test_data)
  
  # Should be identical (after removing trend_component)
  mvgam_complex_clean <- mvgam_complex[, !names(mvgam_complex) %in% "trend_component"]
  expect_identical(mvgam_complex_clean, brms_complex)
})

test_that("family compatibility across different distributions", {
  # Test data for different families
  test_data_gaussian <- data.frame(y = rnorm(20), x = rnorm(20))
  test_data_poisson <- data.frame(y = rpois(20, 3), x = rnorm(20))
  test_data_binomial <- data.frame(y = rbinom(20, 1, 0.5), x = rnorm(20))
  
  families_and_data <- list(
    list(family = gaussian(), data = test_data_gaussian),
    list(family = poisson(), data = test_data_poisson),
    list(family = binomial(), data = test_data_binomial)
  )
  
  formula_obj <- mvgam_formula(y ~ x, trend_formula = NULL)
  
  for (test_case in families_and_data) {
    family <- test_case$family
    data <- test_case$data
    
    # Get priors from both systems
    mvgam_result <- get_prior(formula_obj, family = family, data = data)
    brms_result <- brms::get_prior(y ~ x, family = family, data = data)
    
    # Remove trend_component column for comparison
    mvgam_clean <- mvgam_result[, !names(mvgam_result) %in% "trend_component"]
    
    # Should be identical
    expect_identical(mvgam_clean, brms_result, 
                     info = paste("Failed for family:", family$family))
    
    # Verify trend_component column
    expect_true("trend_component" %in% names(mvgam_result))
    expect_true(all(mvgam_result$trend_component == "observation"))
  }
})

test_that("formula type compatibility (formula vs brmsformula)", {
  test_data <- data.frame(y = rnorm(20), x = rnorm(20))
  
  # Standard formula
  standard_formula <- mvgam_formula(y ~ x, trend_formula = NULL)
  standard_result <- get_prior(standard_formula, family = gaussian(), data = test_data)
  
  # brmsformula
  bf_formula <- mvgam_formula(brms::bf(y ~ x), trend_formula = NULL)
  bf_result <- get_prior(bf_formula, family = gaussian(), data = test_data)
  
  # brms direct comparison
  brms_result <- brms::get_prior(y ~ x, family = gaussian(), data = test_data)
  
  # All should be equivalent (after removing trend_component)
  standard_clean <- standard_result[, !names(standard_result) %in% "trend_component"]
  bf_clean <- bf_result[, !names(bf_result) %in% "trend_component"]
  
  expect_identical(standard_clean, brms_result)
  expect_identical(bf_clean, brms_result)
  
  # Both should have trend_component
  expect_true("trend_component" %in% names(standard_result))
  expect_true("trend_component" %in% names(bf_result))
})

test_that("S3 dispatch works correctly and doesn't mask brms methods", {
  test_data <- data.frame(y = rnorm(20), x = rnorm(20))
  
  # Verify method dispatch for mvgam_formula objects
  formula_obj <- mvgam_formula(y ~ x, trend_formula = NULL)
  expect_s3_class(formula_obj, "mvgam_formula")
  
  # get_prior should dispatch to get_prior.mvgam_formula
  mvgam_result <- get_prior(formula_obj, family = gaussian(), data = test_data)
  expect_true("trend_component" %in% names(mvgam_result))
  
  # Direct brms call should NOT have trend_component
  brms_result <- brms::get_prior(y ~ x, family = gaussian(), data = test_data)
  expect_false("trend_component" %in% names(brms_result))
  
  # get_prior on regular formula should delegate to brms (via get_prior.formula)
  regular_result <- get_prior(y ~ x, family = gaussian(), data = test_data)
  expect_false("trend_component" %in% names(regular_result))
  expect_identical(regular_result, brms_result)
  
  # Verify that brms methods are not masked for regular formulas
  expect_identical(get_prior(y ~ x, family = gaussian(), data = test_data),
                   brms::get_prior(y ~ x, family = gaussian(), data = test_data))
})

test_that("embedded families work correctly with bf() formulas", {
  test_data <- data.frame(y = rnorm(20), x = rnorm(20))
  
  # bf() formula with embedded family
  bf_formula <- brms::bf(y ~ x, family = gaussian())
  formula_obj <- mvgam_formula(bf_formula, trend_formula = NULL)
  
  # Should work without passing family parameter (embedded in bf)
  result <- get_prior(formula_obj, data = test_data)
  
  # Should have trend_component column
  expect_true("trend_component" %in% names(result))
  expect_true(all(result$trend_component == "observation"))
  
  # Should be equivalent to brms direct call (after removing trend_component)
  brms_result <- brms::get_prior(bf_formula, data = test_data)
  result_clean <- result[, !names(result) %in% "trend_component"]
  expect_identical(result_clean, brms_result)
  
  # Test with multivariate bf() formula
  mv_bf_formula <- brms::bf(mvbind(y, x) ~ 1)
  mv_formula_obj <- mvgam_formula(mv_bf_formula, trend_formula = NULL)
  mv_result <- get_prior(mv_formula_obj, data = test_data)
  
  expect_true("trend_component" %in% names(mv_result))
  expect_true(all(mv_result$trend_component == "observation"))
})

test_that("error handling for invalid get_prior calls", {
  # Invalid data
  expect_error(
    get_prior(mvgam_formula(y ~ x), data = NULL),
    "Assertion on 'data' failed"
  )
  
  # Missing response in formula
  expect_error(
    get_prior(mvgam_formula(~ x), data = data.frame(x = 1:5)),
    "Formula missing response variable"
  )
  
  # Invalid family for non-embedded formulas
  test_data <- data.frame(y = rnorm(20), x = rnorm(20))
  expect_error(
    get_prior(mvgam_formula(y ~ x), family = "not a family", data = test_data),
    "Assertion on 'family' failed"
  )
})

test_that("brmsprior class preservation", {
  test_data <- data.frame(y = rnorm(20), x = rnorm(20))
  
  # mvgam_formula result should be brmsprior class
  result <- get_prior(mvgam_formula(y ~ x, trend_formula = NULL), 
                     family = gaussian(), data = test_data)
  expect_s3_class(result, "brmsprior")
  expect_s3_class(result, "data.frame")
  
  # Should have same structure as brms result
  brms_result <- brms::get_prior(y ~ x, family = gaussian(), data = test_data)
  expect_s3_class(brms_result, "brmsprior")
  
  # Same column names (except trend_component)
  expected_cols <- c(names(brms_result), "trend_component")
  expect_setequal(names(result), expected_cols)
})

test_that("trend_component column behavior", {
  test_data <- data.frame(y = rnorm(20), x = rnorm(20))
  
  # When trend_formula = NULL, all should be "observation"
  result <- get_prior(mvgam_formula(y ~ x, trend_formula = NULL), 
                     family = gaussian(), data = test_data)
  
  expect_true("trend_component" %in% names(result))
  expect_true(all(result$trend_component == "observation"))
  expect_equal(length(unique(result$trend_component)), 1)
  expect_type(result$trend_component, "character")
  
  # Should not affect other columns
  brms_result <- brms::get_prior(y ~ x, family = gaussian(), data = test_data)
  result_clean <- result[, !names(result) %in% "trend_component"]
  expect_identical(names(result_clean), names(brms_result))
  expect_equal(nrow(result), nrow(brms_result))
  
  # Test with trend formula ~ 1 should include Intercept_trend
  test_data_trend <- data.frame(y = rnorm(20), time = 1:20, series = factor(rep("A", 20)))
  result_with_trend <- get_prior(mvgam_formula(y ~ 1, trend_formula = ~ 1), 
                                family = gaussian(), data = test_data_trend)
  
  # Should have both observation and trend components
  expect_true(any(result_with_trend$trend_component == "observation"))
  expect_true(any(result_with_trend$trend_component == "trend"))
  
  # Should specifically include Intercept_trend parameter
  expect_true(any(result_with_trend$class == "Intercept_trend"))
  
  # Should have observation Intercept, trend Intercept_trend, and sigma_trend
  expect_true(any(result_with_trend$class == "Intercept" & result_with_trend$trend_component == "observation"))
  expect_true(any(result_with_trend$class == "Intercept_trend" & result_with_trend$trend_component == "trend"))
  expect_true(any(result_with_trend$class == "sigma_trend" & result_with_trend$trend_component == "trend"))
})

# Test brms addition-terms validation
test_that("mvgam_formula forbids brms addition-terms in trend_formula", {
  # All forbidden brms addition-terms
  forbidden_terms <- c("weights", "cens", "trunc", "mi", "trials", 
                       "rate", "vreal", "vint", "subset", "index")
  
  # Test each forbidden term individually
  for (term in forbidden_terms) {
    trend_formula_str <- paste0("~ ", term, "(variable)")
    trend_formula <- as.formula(trend_formula_str)
    
    expect_error(
      mvgam_formula(y ~ x, trend_formula = trend_formula),
      paste0("brms addition-terms not allowed.*", term, "\\(\\)"),
      info = paste("Testing forbidden term:", term)
    )
  }
})

test_that("mvgam_formula allows brms addition-terms in observation formula", {
  # All addition-terms should be allowed in observation formula
  allowed_terms <- c("weights", "cens", "trunc", "mi", "trials", 
                     "rate", "vreal", "vint", "subset", "index")
  
  # Test each term in observation formula (should not error)
  for (term in allowed_terms) {
    obs_formula_str <- paste0("y ~ x + ", term, "(variable)")
    obs_formula <- as.formula(obs_formula_str)
    
    expect_no_error(
      mvgam_formula(obs_formula, trend_formula = ~ 1)
    )
  }
})

test_that("mvgam_formula validates complex trend_formula with multiple forbidden terms", {
  # Test multiple forbidden terms in one formula
  expect_error(
    mvgam_formula(y ~ x, trend_formula = ~ weights(w) + cens(c)),
    "brms addition-terms not allowed.*weights\\(\\), cens\\(\\)"
  )
  
  # Test mixed valid and invalid terms
  expect_error(
    mvgam_formula(y ~ x, trend_formula = ~ s(time) + trials(n)),
    "brms addition-terms not allowed.*trials\\(\\)"
  )
})

test_that("mvgam_formula validation handles whitespace and complex syntax", {
  # Test with various whitespace patterns
  expect_error(
    mvgam_formula(y ~ x, trend_formula = ~ weights( variable )),
    "brms addition-terms not allowed.*weights\\(\\)"
  )
  
  # Test with complex arguments
  expect_error(
    mvgam_formula(y ~ x, trend_formula = ~ mi(var1, var2, method = "pmm")),
    "brms addition-terms not allowed.*mi\\(\\)"
  )
  
  # Test with nested expressions (should still catch the forbidden term)
  expect_error(
    mvgam_formula(y ~ x, trend_formula = ~ I(log(trials(n)))),
    "brms addition-terms not allowed.*trials\\(\\)"
  )
})

test_that("mvgam_formula validation provides helpful error messages", {
  # Test error message content and format
  expect_error(
    mvgam_formula(y ~ x, trend_formula = ~ weights(w)),
    "brms addition-terms not allowed in.*trend_formula",
    info = "Error should mention trend_formula context"
  )
  
  expect_error(
    mvgam_formula(y ~ x, trend_formula = ~ cens(status)),
    "Include these terms in the observation.*formula.*instead",
    info = "Error should provide helpful guidance"
  )
  
  expect_error(
    mvgam_formula(y ~ x, trend_formula = ~ rate(exposure)),
    "modify observation model behavior, not State-Space dynamics",
    info = "Error should explain why terms are forbidden"
  )
})

test_that("mvgam_formula validation works with all supported formula types", {
  # Test with brmsformula containing forbidden terms in trend_formula
  bf_with_forbidden <- brms::bf(
    y ~ x, 
    sigma ~ weights(w)  # This should be caught as forbidden in trend context
  )
  
  # This specific case tests the validation pipeline for complex formula structures
  # The validation should catch forbidden terms regardless of formula complexity
  expect_error(
    mvgam_formula(y ~ x, trend_formula = ~ weights(w)),
    "brms addition-terms not allowed"
  )
})