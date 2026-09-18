#' Tests for Trend Dispatcher System
#'
#' Comprehensive tests for the trend type registry, validation, and formula
#' parsing functionality. Includes edge cases based on brms testing patterns.

test_that("CAR constructor works for continuous-time AR", {
  # CAR should work without n_lv parameter (continuous-time AR)
  # Suppress expected default warnings
  suppressWarnings({
    car_trend <- CAR()
  })
  expect_s3_class(car_trend, "mvgam_trend")
  expect_equal(car_trend$trend, "CAR")
  expect_false(car_trend$cor)
  expect_false(car_trend$ma)
  expect_equal(car_trend$p, 1)  # CAR always has p = 1

  # CAR's signature excludes `p`, `gr` and `subgr`, and R raises for
  # them. The factor arguments CAR takes in order to refuse them are
  # covered in test-trend-map.R.
  expect_error(CAR(p = 2), "unused argument")
  expect_error(CAR(gr = "group"), "unused argument")
  expect_error(CAR(subgr = "subgroup"), "unused argument")

  # CAR accepts time and series parameters
  car_custom <- CAR(time = "month", series = "species")
  expect_equal(car_custom$time, "month")
  expect_equal(car_custom$series, "species")
})

test_that("ZMVN constructor works for zero-mean multivariate normal", {
  # Basic ZMVN creation
  # Suppress expected default warnings
  suppressWarnings({
    zmvn_trend <- ZMVN()
  })
  expect_s3_class(zmvn_trend, "mvgam_trend")
  expect_equal(zmvn_trend$trend, "ZMVN")
  expect_true(zmvn_trend$cor)  # ZMVN always has correlation
  expect_false(zmvn_trend$ma)  # ZMVN doesn't support MA

  # ZMVN with factor model
  suppressWarnings({
    zmvn_factor <- ZMVN(n_lv = 3)
  })
  expect_equal(zmvn_factor$n_lv, 3)

  # ZMVN with hierarchical structure
  suppressWarnings({
    zmvn_hier <- ZMVN(gr = "group", subgr = "subgroup")
  })
  expect_equal(zmvn_hier$gr, "group")
  expect_equal(zmvn_hier$subgr, "subgroup")

  # ZMVN with custom time and series
  zmvn_custom <- ZMVN(time = "week", series = "location")
  expect_equal(zmvn_custom$time, "week")
  expect_equal(zmvn_custom$series, "location")

  # Parameter validation
  expect_error(ZMVN(n_lv = 0), "Assertion on 'n_lv' failed")
  expect_error(ZMVN(n_lv = -1), "Assertion on 'n_lv' failed")
  expect_error(ZMVN(n_lv = 1.5), "Assertion on 'n_lv' failed")
})

test_that("PW constructor rejects factor models correctly", {
  # PW with n_lv should error immediately
  expect_error(PW(n_lv = 3),
               "Factor models.*not supported for PW trends")
  expect_error(PW(n_lv = 3),
               "changepoint modeling")
  expect_error(PW(n_lv = 3),
               "Drop 'n_lv' and 'trend_map'")

  # PW without n_lv should work
  suppressWarnings({
    pw_trend <- PW()
    expect_is(pw_trend, "mvgam_trend")
    expect_equal(pw_trend$trend, "PW")  # Base type for dispatch
  })
})

test_that("AR constructor accepts factor models", {
  # AR with n_lv should work
  suppressWarnings({
    ar_trend <- AR(n_lv = 2)
    expect_is(ar_trend, "mvgam_trend")
    expect_equal(ar_trend$n_lv, 2)
    expect_equal(ar_trend$trend, "AR")  # Base type for dispatch
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
    expect_s3_class(rw_trend, "mvgam_trend")
    expect_equal(rw_trend$n_lv, 3)
    expect_equal(rw_trend$trend, "RW")

  })

  # RW without n_lv should work
  suppressWarnings({
    rw_trend_no_lv <- RW()
    expect_s3_class(rw_trend_no_lv, "mvgam_trend")
    expect_null(rw_trend_no_lv$n_lv)
    expect_equal(rw_trend_no_lv$trend, "RW")

    # The one declaration a trend carries.
    expect_identical(rw_trend_no_lv$validation_rules,
                     "requires_regular_intervals")
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
  # CAR()'s signature omits n_lv, and the error is raised at the
  # stanvars level

  expect_error(PW(n_lv = 1), "Factor models.*not supported")

  # Check they mention specific alternatives (AR is factor-compatible)
  expect_error(PW(n_lv = 1), "AR, RW, VAR, ZMVN")

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
  expect_true(grepl(pattern, "VAR()"))
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
    expect_equal(ar1_trend$trend, "AR")  # Base type for dispatch
    expect_equal(ar1_trend$p, 1)
    # ar_lags and max_lag are computed in the Stan assembly layer

    # Test AR constructor with multiple lags
    ar_seasonal <- AR(p = c(1, 12, 24))
    expect_s3_class(ar_seasonal, "mvgam_trend")
    expect_equal(ar_seasonal$trend, "AR")  # Base type for dispatch
    expect_equal(ar_seasonal$p, c(1, 12, 24))
    # Parameter processing lives in the Stan assembly layer; the
    # constructor carries no tpars field

    # Test VAR constructor with order
    var2_trend <- VAR(p = 2)
    expect_s3_class(var2_trend, "mvgam_trend")
    expect_equal(var2_trend$trend, "VAR")  # Base type for dispatch
    expect_equal(var2_trend$p, 2)
    expect_true(var2_trend$cor)
    # Parameter names are generated in the Stan assembly layer
  })
})

# Test grouping validation helper
test_that("grouping validation helper works correctly", {

  # Test default case
  result1 <- mvgam:::validate_grouping_arguments("NA", "NA")
  expect_null(result1$gr)
  expect_null(result1$subgr)

  # gr without subgr auto-fills subgr to "series" because the
  # hierarchical codegen derives subgroups from the existing series
  # column when no explicit subgr variable is given.
  result_gr_only <- mvgam:::validate_grouping_arguments("region", "NA")
  expect_equal(result_gr_only$gr, "region")
  expect_equal(result_gr_only$subgr, "series")

  # gr with explicit subgr = "series" is allowed and matches the
  # auto-fill default.
  result_series_subgr <- mvgam:::validate_grouping_arguments(
    "region", "series"
  )
  expect_equal(result_series_subgr$gr, "region")
  expect_equal(result_series_subgr$subgr, "series")

  # subgr without gr is still rejected since there is no main
  # grouping variable to nest within.
  expect_error(
    mvgam:::validate_grouping_arguments("NA", "site"),
    "Subgrouping requires main grouping variable"
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
    # Compare formula structure - trend-only formulas use ~0 as base
    expect_true(deparse(parsed1$base_formula) %in% c("~1", "~0"))
    expect_equal(parsed1$trend_terms, "RW()")
    expect_equal(length(parsed1$regular_terms), 0)

    # Test formula with regular predictors and trend
    f2 <- ~ s(time) + cov1 + AR(p = 1)
    parsed2 <- mvgam:::parse_trend_formula(f2)

    expect_equal(length(parsed2$trend_components), 1)
    expect_equal(parsed2$trend_components[[1]]$trend, "AR")  # Base type
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

  # Check that regular terms are preserved (order may differ, the
  # content is the same)
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

# Test formula parsing - multiple trends should fail
test_that("multiple trend components are properly rejected", {

  # Test formula with multiple trends should throw error
  f1 <- ~ s(season) + RW() + AR(p = 2) + cov1

  expect_error(
    mvgam:::validate_single_trend_formula(f1),
    "Multiple trend constructors found"
  )

  # Test in different order should also fail
  f2 <- ~ AR(p = 2) + cov1 + RW() + s(season)

  expect_error(
    mvgam:::validate_single_trend_formula(f2),
    "Multiple trend constructors found"
  )
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
    f3 <- ~ I(x^2) + log(y + 1) + RW(ma = FALSE)
    parsed3 <- mvgam:::parse_trend_formula(f3)
    expect_equal(parsed3$trend_terms, "RW(ma = FALSE)")
    expect_true("I(x^2)" %in% parsed3$regular_terms)
    expect_true("log(y + 1)" %in% parsed3$regular_terms)
    expect_equal(length(parsed3$regular_terms), 2)

    # Test that offset terms are properly rejected in trend formulas
    expect_error(
      mvgam:::parse_trend_formula(~ I(x^2) + RW() + offset(z)),
      "Offsets apply to the observation model"
    )

    # Test whitespace handling
    f4 <- ~   s(time)   +   RW(cor=TRUE)   +   cov1
    parsed4 <- mvgam:::parse_trend_formula(f4)
    expect_equal(parsed4$trend_terms, "RW(cor = TRUE)")
    expect_setequal(parsed4$regular_terms, c("s(time)", "cov1"))
  })
})

# Test malformed formulas and error conditions
test_that("formula parsing error handling works comprehensively", {

  # Test intercept-only formula (should default to ZMVN)
  result_intercept <- mvgam:::parse_trend_formula(~ 1)
  expect_equal(result_intercept$trend_model$trend, "ZMVN")
  expect_equal(result_intercept$base_formula, ~ 1)

  # Test formula with response variable
  expect_error(
    mvgam:::parse_trend_formula(y ~ RW()),
    "names predictors only"
  )

  # Test formula with no trend constructors - should default to ZMVN
  result_no_constructors <- mvgam:::parse_trend_formula(~ s(time) + cov1)
  expect_equal(result_no_constructors$trend_model$trend, "ZMVN")
  expect_equal(result_no_constructors$regular_terms, c("s(time)", "cov1"))

  # A dot with no data to expand it: `terms()` says what is missing,
  # where a caught error once blamed the parentheses.
  expect_error(
    mvgam:::parse_trend_formula(~ .),
    "no 'data' argument"
  )

  # Test invalid trend constructor calls
  expect_error(
    mvgam:::eval_trend_constructor("INVALID_TREND()"),
    "could not find function"
  )

  # Test malformed trend constructor syntax
  expect_error(
    mvgam:::eval_trend_constructor("RW(invalid_param = )"),
    "unused argument"
  )

  # Test unbalanced parentheses (should be caught by R's parser)
  expect_error(
    mvgam:::eval_trend_constructor("RW(cor = TRUE"),
    "unexpected end of input"
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

    # Test formula with repeated trend constructors should fail
    f3 <- ~ RW() + s(time) + RW(ma = TRUE)
    expect_error(
      mvgam:::validate_single_trend_formula(f3),
      "Multiple trend constructors found"
    )
  })
})

# Test regex pattern edge cases
test_that("regex pattern handles edge cases correctly", {

  # Test trend constructors with complex parameters
  complex_terms <- c(
    "AR(p = c(1, 12, 24), ma = TRUE, cor = FALSE)",
    "VAR(p = 10)",
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
  expect_false("s(time, bs = 'tp', k = 20)" %in% found_trends)

  # Test extraction of regular terms from complex expressions
  regular_terms <- mvgam:::extract_regular_terms(complex_terms)
  expect_false(
    "AR(p = c(1, 12, 24), ma = TRUE, cor = FALSE)" %in% regular_terms
  )
  expect_true("s(time, bs = 'tp', k = 20)" %in% regular_terms)

  # Test edge case: trend-like names that aren't trend constructors
  fake_trends <- c("ARbitrary()", "VARious()", "RWanda()", "s(time)")
  found_fake <- mvgam:::find_trend_terms(fake_trends)
  expect_equal(length(found_fake), 0)
})

# Test validation system edge cases
test_that("validation system handles edge cases", {


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
  custom_generator <- function(trend_specs, data_info) {
    list(
      custom_param = brms::stanvar(
        name = "custom_param",
        scode = "real custom_param;",
        block = "parameters"
      )
    )
  }

  # The registry is one environment for the whole session and nothing
  # removes a name from it. Leaving CUSTOM in place gave every later
  # file a seventh trend type: measured, `mvgam_trend_choices()`
  # returned AR, CAR, CUSTOM, PW, RW, VAR, ZMVN once this file ran.
  before <- mvgam_trend_choices()
  withr::defer(rm("CUSTOM", envir = mvgam:::trend_registry))

  mvgam:::register_trend_type(
    "CUSTOM", supports_factors = FALSE,
    generator_func = custom_generator,
    incompatibility_reason = "Custom trend for testing"
  )

  # Registering adds the one name and leaves the rest alone.
  expect_setequal(setdiff(mvgam_trend_choices(), before), "CUSTOM")
  expect_true(grepl("CUSTOM", mvgam:::mvgam_trend_pattern()))
  # The registered entry is what the accessor returns, generator
  # included. This test also defined a constructor it never called
  # and a formula it never asserted on; neither one reached the
  # registry.
  info <- mvgam:::get_trend_info("CUSTOM")
  expect_false(info$supports_factors)
  expect_identical(info$generator, custom_generator)
  expect_identical(
    info$incompatibility_reason, "Custom trend for testing"
  )
})

# Test print method edge cases
test_that("print method handles all configurations", {
  suppressWarnings({

    # Test minimal configuration
    minimal_trend <- RW()
    expect_output(print(minimal_trend), "mvgam trend specification")
    expect_output(print(minimal_trend), "Type: RW")

    # Test maximal configuration, with the expected warnings muted
    complex_trend <- suppressWarnings(
      AR(p = c(1, 12), ma = FALSE, cor = TRUE, n_lv = 2)
    )
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
                         AR(p = c(1, 12, 24), ma = FALSE)

    parsed <- mvgam:::parse_trend_formula(seasonal_formula)
    expect_equal(length(parsed$trend_components), 1)
    expect_equal(parsed$trend_components[[1]]$p, c(1, 12, 24))
    expect_false(parsed$trend_components[[1]]$ma)
    expect_equal(length(parsed$regular_terms), 3)

    # Test multivariate model formula
    multivar_formula <- ~ s(time, by = species, k = 20) +
                          habitat +
                          VAR(p = 2) +
                          s(temperature, species, bs = "fs")

    parsed2 <- mvgam:::parse_trend_formula(multivar_formula)
    expect_equal(parsed2$trend_components[[1]]$trend, "VAR")  # Base type
    expect_true(parsed2$trend_components[[1]]$cor)
    expect_true("s(time, by = species, k = 20)" %in% parsed2$regular_terms)
    expect_true(
      "s(temperature, species, bs = \"fs\")" %in% parsed2$regular_terms
    )
  })
})

# Test time parameter functionality
test_that("time parameter works correctly in trend constructors", {

  # Every constructor defaults `time` to the column name "time". The
  # disjunction in each of these also accepted NA, which is what a
  # constructor that stopped defaulting would return.
  for (spec in list(RW(), AR(p = 1), VAR(p = 1), CAR())) {
    expect_identical(spec$time, "time")
  }

  # Test explicit time parameter with unquoted variable names
  rw_custom <- RW(time = week)
  expect_equal(rw_custom$time, "week")

  ar_custom <- AR(time = year, p = 2)
  expect_equal(ar_custom$time, "year")

  var_custom <- VAR(time = month, p = 1)
  expect_equal(var_custom$time, "month")

  car_custom <- CAR(time = day)
  expect_equal(car_custom$time, "day")

  # Test that other parameters still work correctly with time parameter
  ar_complex <- AR(time = period, p = c(1, 12), ma = TRUE, cor = TRUE)
  expect_equal(ar_complex$time, "period")
  expect_equal(ar_complex$p, c(1, 12))
  expect_true(ar_complex$ma)
  expect_true(ar_complex$cor)

  # Test with grouping variables (may get default variable warnings)
  suppressWarnings(
    var_grouped <- VAR(time = timepoint, p = 2, gr = region, subgr = species)
  )
  expect_equal(var_grouped$time, "timepoint")
  expect_equal(var_grouped$gr, "region")
  expect_equal(var_grouped$subgr, "species")
})

# Test integration with formula parsing
test_that("time parameter integrates correctly with formula parsing", {
  suppressWarnings({

    # Test trend constructors with time parameter in formula
    f1 <- ~ s(x) + AR(time = week, p = 1)
    parsed1 <- mvgam:::parse_trend_formula(f1)
    expect_equal(parsed1$trend_components[[1]]$time, "week")

    f2 <- ~ VAR(time = month, p = 2) + s(temp)
    parsed2 <- mvgam:::parse_trend_formula(f2)
    expect_equal(parsed2$trend_components[[1]]$time, "month")

    # Test seasonal AR model with multiple lags, which is the
    # supported alternative to multiple trends
    f3 <- ~ AR(time = daily, p = c(1, 7, 30))  # Daily, weekly, monthly lags
    parsed3 <- mvgam:::parse_trend_formula(f3)
    expect_equal(parsed3$trend_components[[1]]$time, "daily")
    expect_equal(parsed3$trend_components[[1]]$p, c(1, 7, 30))

    # Test mixed quoted and unquoted usage patterns
    f4 <- ~ s(temp) + CAR(time = period)
    parsed4 <- mvgam:::parse_trend_formula(f4)
    expect_equal(parsed4$trend_components[[1]]$time, "period")
    expect_true("s(temp)" %in% parsed4$regular_terms)

    # Test that offset rejection works with various formulas
    expect_error(
      mvgam:::parse_trend_formula(~ s(temp) + CAR() + offset(effort)),
      "Offsets apply to the observation model"
    )
  })
})

# Test series parameter functionality
test_that("series parameter works correctly in trend constructors", {

  # Every constructor defaults `series` to the column name "series".
  # The disjunction in each of these also accepted NA.
  for (spec in list(RW(), AR(p = 1), VAR(p = 1), CAR())) {
    expect_identical(spec$series, "series")
  }

  # Test explicit series parameter with unquoted variable names
  rw_custom <- RW(series = species)
  expect_equal(rw_custom$series, "species")

  ar_custom <- AR(series = group, p = 2)
  expect_equal(ar_custom$series, "group")

  var_custom <- VAR(series = unit, p = 1)
  expect_equal(var_custom$series, "unit")

  car_custom <- CAR(series = location)
  expect_equal(car_custom$series, "location")

  # Test both time and series parameters specified
  ar_no_warn <- AR(time = week, series = species, p = c(1, 12),
                   ma = TRUE, cor = TRUE)
  expect_equal(ar_no_warn$time, "week")
  expect_equal(ar_no_warn$series, "species")
  expect_equal(ar_no_warn$p, c(1, 12))
  expect_true(ar_no_warn$ma)
  expect_true(ar_no_warn$cor)
})

# Test integration of time and series parameters with formula parsing
test_that(
  "time and series parameters integrate with formula parsing", {

  # Test trend constructors with both time and series parameters in formula
  f1 <- ~ s(x) + AR(time = week, series = species, p = 1)
  parsed1 <- mvgam:::parse_trend_formula(f1)
  expect_equal(parsed1$trend_components[[1]]$time, "week")
  expect_equal(parsed1$trend_components[[1]]$series, "species")

  f2 <- ~ VAR(time = month, series = location, p = 2) + s(temp)
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

test_that("trend constructors record gr and subgr", {
  # RW, AR and VAR each stored the pair, asserted across three blocks
  # under six locals. The table names one case per constructor and
  # states the cor default each one sets.
  suppressWarnings({
    cases <- list(
      list(spec = RW(time = week, series = species, gr = region,
                     subgr = site, cor = TRUE),
           want = list(gr = "region", subgr = "site", time = "week",
                       series = "species", cor = TRUE)),
      list(spec = AR(time = month, series = location, p = 1,
                     gr = ecosystem, subgr = site, cor = TRUE),
           want = list(gr = "ecosystem", subgr = "site", p = 1,
                       cor = TRUE)),
      list(spec = VAR(time = year, series = population, p = 2,
                      gr = habitat, subgr = species),
           want = list(gr = "habitat", subgr = "species", p = 2,
                       cor = TRUE)),
      # cor = FALSE is stored as given.
      list(spec = RW(gr = region, subgr = site, cor = FALSE),
           want = list(gr = "region", subgr = "site", cor = FALSE)),
      list(spec = AR(gr = habitat, subgr = species, p = 1,
                     cor = FALSE),
           want = list(gr = "habitat", subgr = "species",
                       cor = FALSE)),
      list(spec = VAR(gr = ecosystem, subgr = location, p = 2),
           want = list(gr = "ecosystem", subgr = "location"))
    )
  })
  for (case in cases) {
    for (fld in names(case$want)) {
      expect_equal(case$spec[[fld]], case$want[[fld]])
    }
  }
})

test_that("grouping is preserved through formula parsing", {
  suppressWarnings({
    f1 <- ~ s(temp) + RW(time = week, series = species, gr = region,
                         subgr = site, cor = TRUE)
    parsed1 <- mvgam:::parse_trend_formula(f1)
    trend_comp <- parsed1$trend_components[[1]]
    expect_equal(trend_comp$gr, "region")
    expect_equal(trend_comp$subgr, "site")
    expect_equal(trend_comp$time, "week")
    expect_equal(trend_comp$series, "species")
    expect_true(trend_comp$cor)

    # Two trend constructors under different groupings are still two
    # trend constructors.
    f2 <- ~ AR(time = day, series = unit, gr = block, subgr = plot,
               p = 1, cor = TRUE) +
      s(temperature) +
      RW(time = week, series = transect, gr = site, subgr = quadrat,
         cor = TRUE)
    expect_error(
      mvgam:::validate_single_trend_formula(f2),
      "Multiple trend constructors found"
    )
  })
})

test_that("validate_gr_balanced_groups errors on unbalanced groups", {
  # 3 forest + 2 grassland series: tail entries of fixed-size
  # group_innov would be NaN at Stan init.
  unbalanced <- data.frame(
    time = rep(1:6, 5),
    series = factor(rep(paste0("s", 1:5), each = 6)),
    habitat = factor(rep(
      c("forest", "forest", "forest", "grassland", "grassland"),
      each = 6
    ))
  )
  expect_error(
    mvgam:::validate_gr_balanced_groups(
      list(gr = "habitat", subgr = "NA", series = "series"),
      unbalanced
    ),
    "unbalanced groups"
  )

  # Error message names the offending group counts so the user can
  # see exactly which groups are off.
  err <- conditionMessage(expect_error(
    mvgam:::validate_gr_balanced_groups(
      list(gr = "habitat", subgr = "NA", series = "series"),
      unbalanced
    )
  ))
  expect_match(err, "forest=3")
  expect_match(err, "grassland=2")
})

test_that("validate_gr_balanced_groups passes balanced groups silently", {
  balanced <- data.frame(
    time = rep(1:6, 4),
    series = factor(rep(paste0("s", 1:4), each = 6)),
    habitat = factor(rep(
      c("forest", "forest", "grassland", "grassland"),
      each = 6
    ))
  )
  expect_silent(
    result <- mvgam:::validate_gr_balanced_groups(
      list(gr = "habitat", subgr = "NA", series = "series"),
      balanced
    )
  )
  expect_null(result)
})

test_that("a user-supplied subgr is counted, not waved through", {
  # `gr` and `subgr` together name the series, so the balance of a
  # design written that way is exactly the balance of its groups.
  # Returning early whenever `subgr` was supplied skipped every
  # model written the way the documentation shows: the design
  # reached Stan, `N_subgroups_trend` took the largest group, and
  # the smaller group was handed a slice of a correlation matrix
  # whose prior was written for the larger one. Nothing raised,
  # because every index stayed in range.
  unbalanced <- data.frame(
    time = rep(1:6, 5),
    series = factor(rep(paste0("s", 1:5), each = 6)),
    habitat = factor(rep(
      c("forest", "forest", "forest", "grassland", "grassland"),
      each = 6
    )),
    site = factor(rep(paste0("site", 1:5), each = 6))
  )
  expect_error(
    mvgam:::validate_gr_balanced_groups(
      list(gr = "habitat", subgr = "site", series = "series"),
      unbalanced
    ),
    # `insight` wraps the rendered message, so the pattern is short
    # enough to sit inside one line of it.
    regexp = "has unbalanced groups"
  )

  # The same design with the groups evened up passes, and passes on
  # the grouping rather than on the series column: `site` is what
  # names the series here, and dropping the column the grouping
  # supersedes must not change the answer.
  balanced <- unbalanced[unbalanced$series != "s3", ]
  expect_silent(
    result <- mvgam:::validate_gr_balanced_groups(
      list(gr = "habitat", subgr = "site", series = "series"),
      balanced
    )
  )
  expect_null(result)
  expect_silent(
    mvgam:::validate_gr_balanced_groups(
      list(gr = "habitat", subgr = "site", series = "series"),
      balanced[, setdiff(names(balanced), "series")]
    )
  )
})

test_that("PW records every constructor argument in the spec", {
  # PW's constructor records its arguments in the returned spec. Ten
  # constructions across five blocks asserted that one claim, each
  # naming its own local. One case per argument shape states it.
  suppressWarnings({
    cases <- list(
      # Linear growth needs no cap; the field still takes its default.
      list(
        spec = PW(time = week, series = species, growth = "linear",
                  n_changepoints = 5),
        want = list(trend = "PW", growth = "linear", cap = "cap",
                    time = "week", series = "species",
                    n_changepoints = 5)
      ),
      list(
        spec = PW(time = month, series = population, cap = max_size,
                  growth = "linear"),
        want = list(trend = "PW", growth = "linear", cap = "max_size",
                    time = "month", series = "population")
      ),
      list(
        spec = PW(time = day, series = cells, cap = carrying_capacity,
                  growth = "logistic"),
        want = list(trend = "PW", growth = "logistic",
                    cap = "carrying_capacity", time = "day",
                    series = "cells")
      ),
      list(
        spec = PW(time = yearly, series = biomass, cap = max_pop,
                  growth = "logistic", n_changepoints = 20,
                  changepoint_range = 0.9, changepoint_scale = 0.02),
        want = list(trend = "PW", growth = "logistic",
                    cap = "max_pop", time = "yearly",
                    series = "biomass", n_changepoints = 20,
                    changepoint_range = 0.9, changepoint_scale = 0.02)
      )
    )
  })
  for (case in cases) {
    expect_s3_class(case$spec, "mvgam_trend")
    expect_true(is.mvgam_trend(case$spec))
    for (fld in names(case$want)) {
      expect_equal(case$spec[[fld]], case$want[[fld]])
    }
  }
  # Every field the Stan generator requires is present on the spec.
  expect_true(all(
    c("cap", "growth", "trend", "n_changepoints",
      "changepoint_range", "changepoint_scale") %in%
      names(cases[[1L]]$spec)
  ))
})

# Test PW parameter validation
test_that("PW parameter validation works correctly", {

  suppressWarnings({
    # The accepted ranges are covered by the constructor table above.
    # These add the refusal at each boundary.

    # Test invalid n_changepoints (must be positive integer)
    expect_error(
      PW(n_changepoints = 0),
      "Element 1 is not >= 1"
    )

    expect_error(
      PW(n_changepoints = -5),
      "Element 1 is not >= 1"
    )

    expect_error(
      PW(n_changepoints = 3.5),
      "single integerish value"
    )

    # Test invalid changepoint_range (must be between 0 and 1)
    expect_error(
      PW(changepoint_range = 1.5),
      "Element 1 is not <= 1"
    )

    expect_error(
      PW(changepoint_range = -0.1),
      "Element 1 is not >= 0"
    )

    # Test invalid changepoint_scale (must be non-negative)
    # Note: 0 is valid, only negative values are invalid
    expect_error(
      PW(changepoint_scale = -0.05),
      "Element 1 is not >= 0"
    )

    # Test invalid growth type
    expect_error(
      PW(growth = 'exponential'),
      "'arg' should be one of"
    )
  })
})

test_that("piecewise trends work correctly in formula parsing", {
  suppressWarnings({
    # The parser preserves the constructor's arguments through a
    # formula. Three blocks asserted that over three parses, and the
    # constructor table above covers the fields themselves.
    cases <- list(
      list(
        f = ~ s(temp) + PW(growth = "linear", time = week,
                           series = species, n_changepoints = 12),
        want = list(trend = "PW", time = "week", series = "species",
                    n_changepoints = 12)
      ),
      list(
        f = ~ cov1 + PW(growth = "logistic", time = month,
                        series = population, cap = max_capacity,
                        n_changepoints = 25,
                        changepoint_scale = 0.02) + s(x),
        want = list(trend = "PW", cap = "max_capacity",
                    n_changepoints = 25, changepoint_scale = 0.02)
      ),
      list(
        f = ~ PW(time = daily, series = biomass, growth = "linear",
                 n_changepoints = 8, changepoint_range = 0.75) +
          s(season),
        want = list(trend = "PW", growth = "linear",
                    changepoint_range = 0.75)
      ),
      list(
        f = ~ s(temperature) + PW(time = week, series = species,
                                  cap = max_biomass,
                                  growth = "logistic"),
        want = list(trend = "PW", cap = "max_biomass",
                    growth = "logistic")
      )
    )
    for (case in cases) {
      parsed <- mvgam:::parse_trend_formula(case$f)
      expect_equal(length(parsed$trend_components), 1)
      comp <- parsed$trend_components[[1]]
      for (fld in names(case$want)) {
        expect_equal(comp[[fld]], case$want[[fld]])
      }
    }
  })
})

# Test piecewise trend registry integration
test_that("piecewise trends integrate correctly with registry system", {

  # Test that PW trends are in registry
  choices <- mvgam_trend_choices()
  expect_true("PW" %in% choices)
  # PWlinear and PWlogistic are trend variants of PW type

  # Test pattern includes piecewise trends
  pattern <- mvgam:::mvgam_trend_pattern()
  expect_true(grepl("PW", pattern))
  # Pattern should include base PW type

  # Registry info for the piecewise entry. The same call appeared
  # three times under three comments about PW variants, with "PW" as
  # the argument each time.
  pw_info <- mvgam:::get_trend_info("PW")
  expect_type(pw_info, "list")
  expect_false(pw_info$supports_factors)
  expect_type(pw_info$generator, "closure")
  # A factor-incompatible entry stores the reason shown to a user.
  expect_true(nzchar(pw_info$incompatibility_reason))
  # The variant spellings name no registry entry of their own.
  expect_error(
    mvgam:::get_trend_info("PWlinear"), "Unknown trend type"
  )
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


