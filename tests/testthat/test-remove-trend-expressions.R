# Tests for remove_trend_expressions function
# This tests the formula parsing fixes for handling subtraction operations

test_that("remove_trend_expressions handles subtraction correctly", {
  trend_patterns <- c("RW", "AR", "VAR", "CAR", "PW", "ZMVN", "GP")
  
  # Test ~ RW() becomes ~ 0 (not ~ 1)
  expr1 <- rlang::parse_expr("RW()")
  result1 <- mvgam:::remove_trend_expressions(expr1, trend_patterns)
  expect_null(result1)  # RW() removed, nothing left
  
  # Test ~ RW() - 1 becomes ~ -1
  expr2 <- rlang::parse_expr("RW() - 1")
  result2 <- mvgam:::remove_trend_expressions(expr2, trend_patterns)
  expect_equal(deparse(result2), "-1")
  
  # Test ~ RW() + x - 1 becomes ~ x - 1
  expr3 <- rlang::parse_expr("RW() + x - 1")
  result3 <- mvgam:::remove_trend_expressions(expr3, trend_patterns)
  expect_equal(deparse(result3), "x - 1")
  
  # Test ~ x + RW() - 1 becomes ~ x - 1  
  expr4 <- rlang::parse_expr("x + RW() - 1")
  result4 <- mvgam:::remove_trend_expressions(expr4, trend_patterns)
  expect_equal(deparse(result4), "x - 1")
  
  # Test ~ -1 + RW() becomes ~ -1
  expr5 <- rlang::parse_expr("-1 + RW()")
  result5 <- mvgam:::remove_trend_expressions(expr5, trend_patterns)
  expect_equal(deparse(result5), "-1")
  
  # Test ~ -1 + RW() + x becomes ~ -1 + x
  expr6 <- rlang::parse_expr("-1 + RW() + x")
  result6 <- mvgam:::remove_trend_expressions(expr6, trend_patterns)
  # Result could be "-1 + x" or "x - 1" depending on rlang expression reconstruction
  expect_true(deparse(result6) %in% c("-1 + x", "x - 1"))
})

test_that("remove_trend_expressions preserves non-trend terms", {
  trend_patterns <- c("RW", "AR", "VAR", "CAR", "PW", "ZMVN", "GP")
  
  # Test ~ x - 1 stays as ~ x - 1
  expr1 <- rlang::parse_expr("x - 1")
  result1 <- mvgam:::remove_trend_expressions(expr1, trend_patterns)
  expect_equal(deparse(result1), "x - 1")
  
  # Test ~ -1 stays as ~ -1
  expr2 <- rlang::parse_expr("-1")
  result2 <- mvgam:::remove_trend_expressions(expr2, trend_patterns)
  expect_equal(deparse(result2), "-1")
  
  # Test ~ 0 stays as ~ 0
  expr3 <- rlang::parse_expr("0")
  result3 <- mvgam:::remove_trend_expressions(expr3, trend_patterns)
  expect_equal(deparse(result3), "0")
  
  # Test ~ 1 stays as ~ 1
  expr4 <- rlang::parse_expr("1")
  result4 <- mvgam:::remove_trend_expressions(expr4, trend_patterns)
  expect_equal(deparse(result4), "1")
})

test_that("remove_trend_expressions handles complex formulas", {
  trend_patterns <- c("RW", "AR", "VAR", "CAR", "PW", "ZMVN", "GP")
  
  # Test complex formula with multiple trend terms
  expr1 <- rlang::parse_expr("RW() + AR() + x + s(time)")
  result1 <- mvgam:::remove_trend_expressions(expr1, trend_patterns)
  expect_equal(deparse(result1), "x + s(time)")
  
  # Test with gp() function (not GP trend)
  expr2 <- rlang::parse_expr("RW() + gp(x, k = 5) - 1")
  result2 <- mvgam:::remove_trend_expressions(expr2, trend_patterns)
  expect_equal(deparse(result2), "gp(x, k = 5) - 1")
})

test_that("parse_base_formula_safe uses correct defaults", {
  # Test that when all terms are removed, formula becomes ~ 0
  trend_patterns <- c("RW", "AR", "VAR", "CAR", "PW", "ZMVN", "GP")
  
  # Formula with only trend terms
  formula1 <- ~ RW()
  result1 <- mvgam:::parse_base_formula_safe(formula1, trend_patterns)
  expect_equal(deparse(result1), "~0")
  
  # Formula with trend and intercept removal  
  formula2 <- ~ RW() - 1
  result2 <- mvgam:::parse_base_formula_safe(formula2, trend_patterns)
  expect_equal(deparse(result2), "~-1")
  
  # Formula with trend and covariate
  formula3 <- ~ RW() + x
  result3 <- mvgam:::parse_base_formula_safe(formula3, trend_patterns)
  expect_equal(deparse(result3), "~x")
})