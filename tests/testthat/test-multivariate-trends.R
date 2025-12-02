test_that("parse_multivariate_trends handles missing trend formula", {
  formula <- y ~ s(x)
  
  result <- mvgam:::parse_multivariate_trends(formula, NULL)
  
  expect_false(result$has_trends)
  expect_false(result$is_multivariate)
  expect_null(result$response_names)
  expect_null(result$trend_specs)
  expect_null(result$base_formula)
})

test_that("parse_multivariate_trends identifies univariate formulas", {
  formula <- y ~ s(x) + temp
  trend_formula <- ~ RW()
  
  # Skip validation since it expects brmsformula objects
  with_mocked_bindings(
    validate_bf_trend_formula = function(x) list(valid = TRUE),
    {
      result <- mvgam:::parse_multivariate_trends(formula, trend_formula)
      
      expect_true(result$has_trends)
      expect_false(result$is_multivariate)
      expect_null(result$response_names)
      expect_named(result$trend_specs, "main")
      expect_equal(result$base_formula, trend_formula)
    }
  )
})

test_that("parse_multivariate_trends handles multivariate formulas with mvbind", {
  formula <- mvbind(count, biomass) ~ temp + precip
  trend_formula <- ~ AR(p = 1)
  
  # Mock brms validation
  with_mocked_bindings(
    validate_bf_trend_formula = function(x) list(valid = TRUE),
    {
      result <- mvgam:::parse_multivariate_trends(formula, trend_formula)
      
      expect_true(result$has_trends)
      expect_true(result$is_multivariate)
      expect_equal(result$response_names, c("count", "biomass"))
      expect_length(result$trend_specs, 2)
      expect_named(result$trend_specs, c("count", "biomass"))
    }
  )
})

test_that("parse_multivariate_trends handles multivariate formulas with cbind", {
  formula <- cbind(successes, failures) ~ treatment
  trend_formula <- ~ RW(cor = TRUE)
  
  with_mocked_bindings(
    validate_bf_trend_formula = function(x) list(valid = TRUE),
    {
      result <- mvgam:::parse_multivariate_trends(formula, trend_formula)
      
      expect_true(result$has_trends)
      expect_true(result$is_multivariate)
      expect_equal(result$response_names, c("successes", "failures"))
      expect_length(result$trend_specs, 2)
    }
  )
})

test_that("parse_multivariate_trends handles list syntax for response-specific trends", {
  formula <- mvbind(count, biomass) ~ temp
  trend_formula <- list(
    count = ~ AR(p = 1),
    biomass = ~ RW(cor = TRUE)
  )
  
  # No mocking needed - test real implementation
  result <- mvgam:::parse_multivariate_trends(formula, trend_formula)
  
  expect_true(result$has_trends)
  expect_true(result$is_multivariate)
  expect_equal(result$response_names, c("count", "biomass"))
  expect_length(result$trend_specs, 2)
  expect_named(result$trend_specs, c("count", "biomass"))
  expect_true(!is.null(result$trend_specs$count))
  expect_true(!is.null(result$trend_specs$biomass))
})

test_that("parse_multivariate_trends validates list syntax correctly", {
  formula <- mvbind(y1, y2) ~ x
  
  # Test error when list used with univariate formula
  expect_error(
    mvgam:::parse_multivariate_trends(y ~ x, list(y = ~ AR(p = 1))),
    "List.*trend_formula.*requires multivariate main formula"
  )
  
  # Test error with unknown response names
  expect_error(
    mvgam:::parse_multivariate_trends(formula, list(y3 = ~ AR(p = 1))),
    "Unknown responses.*trend_formula"
  )
  
  # Test success with NULL for some responses
  expect_no_error({
    result <- mvgam:::parse_multivariate_trends(
      formula, 
      list(y1 = ~ AR(p = 1), y2 = NULL)
    )
    expect_true(result$has_trends)
    expect_true(result$is_multivariate)
    expect_length(result$trend_specs, 2)
    expect_named(result$trend_specs, c("y1", "y2"))
    expect_true(!is.null(result$trend_specs$y1))
    expect_null(result$trend_specs$y2)
  })
})

test_that("is_multivariate_formula correctly identifies multivariate formulas", {
  # Univariate cases
  expect_false(mvgam:::is_multivariate_formula(y ~ x))
  expect_false(mvgam:::is_multivariate_formula(log(y) ~ s(x)))
  
  # Multivariate cases
  expect_true(mvgam:::is_multivariate_formula(mvbind(y1, y2) ~ x))
  expect_false(mvgam:::is_multivariate_formula(cbind(success, failure) ~ treatment))
  expect_true(mvgam:::is_multivariate_formula(mvbind(count, biomass, presence) ~ temp + precip))
})

test_that("extract_response_names handles various multivariate structures", {
  # mvbind cases
  expect_equal(
    mvgam:::extract_response_names(mvbind(y1, y2) ~ x),
    c("y1", "y2")
  )
  
  expect_equal(
    mvgam:::extract_response_names(mvbind(count, biomass, presence) ~ temp),
    c("count", "biomass", "presence")
  )
  
  # cbind cases
  expect_equal(
    mvgam:::extract_response_names(cbind(success, failure) ~ treatment),
    c("success", "failure")
  )
  
  # Handles whitespace
  expect_equal(
    mvgam:::extract_response_names(mvbind( y1 , y2 , y3 ) ~ x),
    c("y1", "y2", "y3")
  )
  
  # Returns NULL for univariate
  expect_null(mvgam:::extract_response_names(y ~ x))
})

test_that("parse_multivariate_trends handles response-specific trend formulas", {
  formula <- mvbind(count, biomass) ~ temp
  
  # Mock bf object for response-specific trends
  mock_bf <- structure(list(), class = "brmsterms")
  
  with_mocked_bindings(
    validate_bf_trend_formula = function(x) list(valid = TRUE),
    extract_response_trends = function(tf, rn) {
      list(count = ~ AR(p = 1), biomass = ~ RW(cor = TRUE))
    },
    create_trend_base_formula = function(ts) ~ 1,
    {
      result <- mvgam:::parse_multivariate_trends(formula, mock_bf)
      
      expect_true(result$has_trends)
      expect_true(result$is_multivariate)  
      expect_equal(result$response_names, c("count", "biomass"))
      expect_named(result$trend_specs, c("count", "biomass"))
    }
  )
})

test_that("extract_response_trends handles brms mvbrmsterms objects", {
  # Mock mvbrmsterms structure
  mock_terms <- structure(
    list(
      terms = list(
        count = list(formula = ~ AR(p = 1)),
        biomass = list(formula = ~ RW(cor = TRUE))
      )
    ),
    class = "mvbrmsterms"
  )
  
  response_names <- c("count", "biomass")
  
  # Mock brms::brmsterms to avoid actual parsing
  with_mocked_bindings(
    brmsterms = function(x) mock_terms,
    {
      result <- mvgam:::extract_response_trends(mock_terms, response_names)
      
      expect_named(result, c("count", "biomass"))
      expect_equal(result$count, ~ AR(p = 1))
      expect_equal(result$biomass, ~ RW(cor = TRUE))
    }
  )
})

test_that("extract_response_trends handles single brmsterms objects", {
  # Mock single brmsterms structure
  mock_terms <- structure(
    list(formula = ~ AR(p = 2)),
    class = "brmsterms"
  )
  
  response_names <- c("abundance")
  
  # Mock brms::brmsterms to avoid actual parsing
  with_mocked_bindings(
    brmsterms = function(x) mock_terms,
    {
      result <- mvgam:::extract_response_trends(mock_terms, response_names) 
      
      expect_named(result, "abundance")
      expect_equal(result$abundance, mock_terms$formula)
    }
  )
})

test_that("create_trend_base_formula handles various trend specifications", {
  # Formula-based trend specs
  trend_specs <- list(
    series1 = ~ AR(p = 1),
    series2 = ~ RW(cor = TRUE)
  )
  
  result <- mvgam:::create_trend_base_formula(trend_specs)
  expect_equal(result, ~ AR(p = 1))
  
  # brmsterms-based trend specs
  mock_terms <- list(
    series1 = structure(list(formula = ~ VAR(p = 2)), class = "brmsterms")
  )
  
  result <- mvgam:::create_trend_base_formula(mock_terms)
  expect_equal(result, ~ VAR(p = 2))
  
  # Fallback case
  weird_specs <- list(
    series1 = "not_a_formula"
  )
  
  result <- mvgam:::create_trend_base_formula(weird_specs)
  expect_equal(result, ~ 1)
})

test_that("parse_multivariate_trends input validation works", {
  # Non-formula input
  expect_error(
    mvgam:::parse_multivariate_trends("not a formula", ~ RW()),
    "Assertion on 'formula' failed"
  )
  
  # Invalid trend formula should trigger validation
  formula <- y ~ s(x)
  bad_trend <- "bad trend formula"
  
  with_mocked_bindings(
    validate_bf_trend_formula = function(x) stop("Invalid trend formula"),
    {
      expect_error(
        mvgam:::parse_multivariate_trends(formula, bad_trend),
        "Invalid trend formula"
      )
    }
  )
})

test_that("parse_multivariate_trends preserves validation results", {
  formula <- y ~ s(x)
  trend_formula <- ~ AR(p = 1)
  
  mock_validation <- list(valid = TRUE, warnings = "Test warning")
  
  with_mocked_bindings(
    validate_bf_trend_formula = function(x) mock_validation,
    {
      result <- mvgam:::parse_multivariate_trends(formula, trend_formula)
      
      expect_equal(result$validation, mock_validation)
    }
  )
})

test_that("Complex multivariate trend parsing works end-to-end", {
  # Complex multivariate formula
  formula <- mvbind(count, biomass, presence) ~ s(temp) + precip + offset(log_effort)
  
  # Mock complex bf trend formula
  mock_bf_trends <- structure(list(), class = "mvbrmsterms")
  
  with_mocked_bindings(
    validate_bf_trend_formula = function(x) list(valid = TRUE),
    extract_response_trends = function(tf, rn) {
      list(
        count = ~ AR(p = 1),
        biomass = ~ RW(cor = TRUE), 
        presence = ~ ZMVN()
      )
    },
    create_trend_base_formula = function(ts) ~ 1,
    {
      result <- mvgam:::parse_multivariate_trends(formula, mock_bf_trends)
      
      expect_true(result$has_trends)
      expect_true(result$is_multivariate)
      expect_equal(result$response_names, c("count", "biomass", "presence"))
      expect_length(result$trend_specs, 3)
      expect_named(result$trend_specs, c("count", "biomass", "presence"))
      expect_true(result$validation$valid)
    }
  )
})