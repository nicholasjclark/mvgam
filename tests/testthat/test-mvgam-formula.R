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
  
  # brmsformula (if brms is available)
  skip_if_not_installed("brms")
  bf1 <- brms::bf(y ~ x)
  mf2 <- mvgam_formula(bf1)
  expect_s3_class(mf2, "mvgam_formula")
  expect_s3_class(mf2, "brmsformula")
  
  # mvbind formula (multivariate)
  skip_if_not_installed("brms")
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
    "incompatible autocorrelation terms"
  )
  
  # Should provide helpful error message
  expect_error(
    mvgam_formula(y ~ x, trend_formula = ~ autocor(M = ~ 1)),
    "State-Space trends cannot be combined with brms autocorr"
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
  skip_if_not_installed("brms")
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
  skip_if_not_installed("brms")
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