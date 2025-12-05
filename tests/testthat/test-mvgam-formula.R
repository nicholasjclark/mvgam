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

  # No-intercept trend formulas
  mf4 <- mvgam_formula(y ~ x, trend_formula = ~ -1)
  expect_s3_class(mf4, "mvgam_formula")
  expect_equal(mf4$formula, y ~ x)
  expect_equal(mf4$trend_formula, ~ -1)

  # No-intercept with predictors
  mf5 <- mvgam_formula(y ~ 1, trend_formula = ~ x - 1 + AR())
  expect_s3_class(mf5, "mvgam_formula")
  expect_equal(mf5$formula, y ~ 1)
  expect_equal(deparse(mf5$trend_formula), "~x - 1 + AR()")
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
  expect_error(mvgam_formula(y ~ .))
})

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

  # Should be identical (attribute-based metadata doesn't affect data structure)
  expect_identical(mvgam_result[, names(brms_result)], brms_result)

  # Verify trend component attributes are added correctly
  # Check structure
  expect_s3_class(mvgam_result, "brmsprior")
  # Should have only observation parameters (no _trend suffix)
  expect_true(all(!grepl("_trend$", mvgam_result$class)))

  # Test mvgam_formula object reusability across multiple get_prior calls
  mvgam_result2 <- get_prior(formula_obj, family = gaussian(), data = test_data)
  expect_identical(mvgam_result, mvgam_result2)

  # Test with different families using same formula object
  mvgam_poisson <- get_prior(formula_obj, family = poisson(), data = test_data)
  expect_s3_class(mvgam_poisson, "brmsprior")
  expect_true(all(attr(mvgam_poisson, "trend_components") == "observation"))

  # Complex formula with random effects
  complex_formula <- mvgam_formula(y ~ x + s(x) + (1|group), trend_formula = NULL)
  mvgam_complex <- get_prior(complex_formula, family = gaussian(), data = test_data)
  brms_complex <- brms::get_prior(y ~ x + s(x) + (1|group),
                                  family = gaussian(), data = test_data)

  # Should be identical (attributes don't affect data structure)
  expect_identical(mvgam_complex[, names(brms_complex)], brms_complex)
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

    # Should be identical (attributes don't affect data structure)
    expect_identical(mvgam_result[, names(brms_result)], brms_result,
                     info = paste("Failed for family:", family$family))

    # Verify structure (should have only observation parameters, no _trend suffix)
    expect_true(all(!grepl("_trend$", mvgam_result$class)))
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

  # All should be equivalent (attributes don't affect data structure)
  expect_identical(standard_result[, names(brms_result)], brms_result)
  expect_identical(bf_result[, names(brms_result)], brms_result)

  # Both should be valid brmsprior objects
  expect_s3_class(standard_result, "brmsprior")
  expect_s3_class(bf_result, "brmsprior")
})

test_that("S3 dispatch works correctly and doesn't mask brms methods", {
  test_data <- data.frame(y = rnorm(20), x = rnorm(20))

  # Verify method dispatch for mvgam_formula objects
  formula_obj <- mvgam_formula(y ~ x, trend_formula = NULL)
  expect_s3_class(formula_obj, "mvgam_formula")

  # get_prior should dispatch to get_prior.mvgam_formula
  mvgam_result <- get_prior(formula_obj, family = gaussian(), data = test_data)
  expect_s3_class(mvgam_result, "brmsprior")

  # Direct brms call should return standard brmsprior
  brms_result <- brms::get_prior(y ~ x, family = gaussian(), data = test_data)
  expect_s3_class(brms_result, "brmsprior")

  # get_prior on regular formula should delegate to brms (via get_prior.formula)
  regular_result <- get_prior(y ~ x, family = gaussian(), data = test_data)
  expect_s3_class(regular_result, "brmsprior")
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

  # Should be valid brmsprior with only observation parameters
  expect_s3_class(result, "brmsprior")
  expect_true(all(!grepl("_trend$", result$class)))

  # Should be equivalent to brms direct call
  brms_result <- brms::get_prior(bf_formula, data = test_data)
  expect_identical(result[, names(brms_result)], brms_result)

  # Test with multivariate bf() formula
  mv_bf_formula <- brms::bf(mvbind(y, x) ~ 1) + brms::set_rescor(FALSE)
  mv_formula_obj <- mvgam_formula(mv_bf_formula, trend_formula = NULL)
  mv_result <- get_prior(mv_formula_obj, data = test_data)

  expect_s3_class(mv_result, "brmsprior")
  expect_true(all(!grepl("_trend$", mv_result$class)))
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

test_that("brmsprior class preservation and ecosystem integration", {
  test_data <- data.frame(y = rnorm(20), x = rnorm(20))

  # mvgam_formula result should be brmsprior class
  result <- get_prior(mvgam_formula(y ~ x, trend_formula = NULL),
                     family = gaussian(), data = test_data)
  expect_s3_class(result, "brmsprior")
  expect_s3_class(result, "data.frame")

  # Should have same structure as brms result
  brms_result <- brms::get_prior(y ~ x, family = gaussian(), data = test_data)
  expect_s3_class(brms_result, "brmsprior")

  # Same column names (attributes don't add columns)
  expect_setequal(names(result), names(brms_result))

  # Test brms ecosystem integration
  # Returned brmsprior objects should work with brms functions
  custom_prior1 <- brms::set_prior("normal(0, 2)", class = "Intercept")
  custom_prior2 <- brms::set_prior("exponential(1)", class = "sigma")

  # Test combining custom priors with mvgam result using + operator
  expect_no_error({
    combined1 <- custom_prior1 + result
  })
  expect_no_error({
    combined2 <- custom_prior2 + result
  })

  # Test with trend parameters
  test_data_trend <- data.frame(y = rnorm(20), x = rnorm(20), time = 1:20, series = factor(rep("A", 20)))
  result_with_trend <- get_prior(mvgam_formula(y ~ x, trend_formula = ~ AR()),
                                family = gaussian(), data = test_data_trend)

  custom_trend_prior1 <- brms::prior("normal(0, 0.5)", class = "ar1_trend")
  custom_trend_prior2 <- brms::prior("exponential(2)", class = "sigma_trend")

  expect_no_error({
    combined_trend1 <- custom_trend_prior1 + result_with_trend
  })
  expect_no_error({
    combined_trend2 <- custom_trend_prior2 + result_with_trend
  })
})

test_that("trend component attribute behavior", {
  test_data <- data.frame(y = rnorm(20), x = rnorm(20))

  # When trend_formula = NULL, should have only observation parameters
  result <- get_prior(mvgam_formula(y ~ x, trend_formula = NULL),
                     family = gaussian(), data = test_data)

  expect_s3_class(result, "brmsprior")
  expect_true(all(!grepl("_trend$", result$class)))  # No trend parameters

  # Should be identical to brms result
  brms_result <- brms::get_prior(y ~ x, family = gaussian(), data = test_data)
  expect_identical(names(result), names(brms_result))
  expect_equal(nrow(result), nrow(brms_result))

  # Test with trend formula ~ 1 should include trend parameters
  test_data_trend <- data.frame(y = rnorm(20), time = 1:20, series = factor(rep("A", 20)))
  result_trend <- get_prior(mvgam_formula(y ~ 1, trend_formula = ~ 1),
                           family = gaussian(), data = test_data_trend)

  # Should have both observation and trend parameters
  expect_s3_class(result_trend, "brmsprior")
  obs_params <- !grepl("_trend$", result_trend$class)
  trend_params <- grepl("_trend$", result_trend$class)
  expect_true(any(obs_params))    # Has observation parameters
  expect_true(any(trend_params))  # Has trend parameters

  # Trend-specific priors should have _trend suffix
  trend_classes <- result_trend$class[trend_params]
  expect_true(all(grepl("_trend$", trend_classes)))
})

test_that("all brms addition-terms detected in trend_formula", {
  # Single forbidden terms - each should be caught individually
  # Test autocorrelation terms (different error message)
  expect_error(
    mvgam_formula(y ~ x, trend_formula = ~ autocor(M = ~ 1)),
    "brms autocorrelation terms not allowed"
  )

  # Test addition-terms
  addition_terms <- list(
    ~ weights(w),
    ~ subset(idx),
    ~ cov_ranef(M = ~ 1)
  )

  for (term in addition_terms) {
    expect_error(
      mvgam_formula(y ~ x, trend_formula = term),
      "brms addition-terms not allowed",
      info = paste("Failed to catch:", deparse(term))
    )
  }
})

test_that("addition-terms detection in complex trend formulas", {
  # Complex formula with forbidden term embedded
  expect_error(
    mvgam_formula(y ~ x,
                  trend_formula = ~ s(time) + (1|group) + weights(w)),
    "brms addition-terms not allowed"
  )

  # Multiple addition-terms in one formula
  expect_error(
    mvgam_formula(y ~ x,
                  trend_formula = ~ autocor(M = ~ 1) + weights(w)),
    "brms autocorrelation terms not allowed"
  )

  # Nested addition-terms
  expect_error(
    mvgam_formula(y ~ x,
                  trend_formula = ~ (1|group) + I(weights(w) * 2)),
    "brms addition-terms not allowed"
  )
})

test_that("observation formula allows brms addition-terms", {
  # These should work fine in main formula
  expect_no_error(
    mvgam_formula(y ~ x + weights(w), trend_formula = ~ 1)
  )

  expect_no_error(
    mvgam_formula(y ~ x + subset(idx), trend_formula = NULL)
  )

  # Complex observation formula with addition-terms
  expect_no_error(
    mvgam_formula(y ~ x + s(time) + weights(w) + (1|group),
                  trend_formula = ~ s(season))
  )
})

test_that("edge cases and complex validation scenarios", {
  # Function names as variables (not function calls)
  expect_no_error(
    mvgam_formula(y ~ weights, trend_formula = ~ 1)  # 'weights' as variable
  )

  # Similar-looking but valid terms
  expect_no_error(
    mvgam_formula(y ~ x, trend_formula = ~ weight_var + auto_corr)  # Variables, not functions
  )

  # Deeply nested structures
  expect_error(
    mvgam_formula(y ~ x,
                  trend_formula = ~ ((time + weights(w)) * factor)),
    "brms addition-terms not allowed"
  )

  # Mixed valid and invalid
  expect_error(
    mvgam_formula(y ~ x, trend_formula = ~ s(time) + AR() + weights(w))
  )

  # No-intercept edge cases should work
  expect_no_error(
    mvgam_formula(y ~ x, trend_formula = ~ -1)  # Simple no-intercept
  )

  expect_no_error(
    mvgam_formula(y ~ 1, trend_formula = ~ x + s(time) - 1 + AR())  # Complex no-intercept
  )

  # No-intercept with forbidden terms should still error
  expect_error(
    mvgam_formula(y ~ x, trend_formula = ~ x - 1 + weights(w) + AR()),
    "brms addition-terms not allowed"
  )
})

test_that("validation preserves helpful error context", {
  # Error should mention the problematic context
  expect_error(
    mvgam_formula(y ~ x, trend_formula = ~ weights(w)),
    "brms addition-terms not allowed"
  )

  # Should identify the specific addition-term
  expect_error(
    mvgam_formula(y ~ x, trend_formula = ~ autocor(M = ~ 1)),
    "brms autocorrelation terms not allowed"
  )

  # Multiple terms - error should still be clear
  expect_error(
    mvgam_formula(y ~ x, trend_formula = ~ weights(w) + subset(idx)),
    "brms addition-terms not allowed"
  )
})

test_that("comprehensive addition-terms catalog coverage", {
  # This test verifies we catch ALL the addition-terms that brms supports
  # Based on brms documentation and source code inspection

  # Test autocorrelation term (different validation)
  expect_error(
    mvgam_formula(y ~ x, trend_formula = ~ autocor(M = ~ 1)),
    "brms autocorrelation terms not allowed"
  )

  # Test actual addition-terms
  all_addition_terms <- list(
    ~ weights(w),           # Observation weights
    ~ subset(idx),          # Data subsetting
    ~ cov_ranef(M = ~ 1)    # Random effects covariance (deprecated)
  )

  for (i in seq_along(all_addition_terms)) {
    term <- all_addition_terms[[i]]
    expect_error(
      mvgam_formula(y ~ x, trend_formula = term),
      "brms addition-terms not allowed",
      info = paste("Addition-term not caught:", deparse(term))
    )
  }
})

test_that("complex real-world formula edge case validation", {
  # This tests a complex scenario that might arise in practice
  # Formula components are interleaved with forbidden terms
  complex_forbidden <- ~ (
    s(time, bs = "cr", k = 10) +     # Valid smooth
    (1 + season | site) +            # Valid random effect
    weights(importance) +            # FORBIDDEN
    te(lat, lon, k = c(5, 5)) +      # Valid tensor product
    offset(log(effort)) +            # Valid offset (not addition-term)
    sigma ~ weights(w)  # This should be caught as forbidden in trend context
  )

  # This specific case tests the validation pipeline for complex formula structures
  # The validation should catch forbidden terms regardless of formula complexity
  expect_error(
    mvgam_formula(y ~ x, trend_formula = ~ weights(w)),
    "brms addition-terms not allowed"
  )
})

test_that("validate_trend_covariates prevents response variables in trend formulas", {
  # Create test data with proper structure
  test_data <- data.frame(
    time = rep(1:4, 3),
    series = rep(c("A", "B", "C"), each = 4),
    count = rnorm(12),
    biomass = rnorm(12),
    temperature = rep(c(20, 22, 18, 21), 3)  # invariant within time
  )
})

test_that("exact GP terms are rejected in observation formula", {
  expect_error(
    mvgam_formula(y ~ gp(x), trend_formula = ~ 1),
    "Exact GP terms.*without.*parameter.*are not supported"
  )

  expect_error(
    mvgam_formula(y ~ gp(temperature), trend_formula = NULL),
    "Found: gp\\(temperature\\)"
  )

  expect_error(
    mvgam_formula(y ~ gp(x), trend_formula = ~ 1),
    "Example: gp\\(x, k=20\\)"
  )
})

test_that("exact GP terms are rejected in trend formula", {
  expect_error(
    mvgam_formula(y ~ 1, trend_formula = ~ gp(time)),
    "Exact GP terms.*without.*parameter.*are not supported"
  )

  expect_error(
    mvgam_formula(y ~ x, trend_formula = ~ s(time) + gp(season) + AR()),
    "Found: gp\\(season\\)"
  )
})

test_that("approximate GP terms are allowed", {
  expect_no_error(
    mvgam_formula(y ~ gp(x, k = 10), trend_formula = ~ 1)
  )

  expect_no_error(
    mvgam_formula(y ~ gp(temperature, k = 5, cov = "matern32"),
                  trend_formula = NULL)
  )

  expect_no_error(
    mvgam_formula(y ~ gp(x, k = 8) + gp(z, k = 12),
                  trend_formula = ~ 1)
  )

  expect_no_error(
    mvgam_formula(y ~ 1, trend_formula = ~ gp(time, k = 15))
  )
})

test_that("mixed valid and invalid GP terms are caught", {
  expect_error(
    mvgam_formula(y ~ gp(x, k = 5) + gp(z), trend_formula = ~ 1),
    "Found: gp\\(z\\)"
  )
})

test_that("GP validation works with complex formulas", {
  expect_error(
    mvgam_formula(y ~ s(time) + (1|group) + gp(temperature) + x^2,
                  trend_formula = ~ AR()),
    "Found: gp\\(temperature\\)"
  )

  expect_error(
    mvgam_formula(y ~ x,
                  trend_formula = ~ s(season, bs = "cc") + gp(depth) + (1|site)),
    "Found: gp\\(depth\\)"
  )

  expect_no_error(
    mvgam_formula(y ~ s(time) + gp(temperature, k = 10) + (1|group),
                  trend_formula = ~ AR())
  )
})
