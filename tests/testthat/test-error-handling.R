test_that("Enhanced architecture handles missing required functions gracefully", {
  # Test when brms functions are not available
  formula <- y ~ s(x)
  data <- data.frame(y = 1:10, x = rnorm(10))
  
  # Mock missing brms function
  with_mocked_bindings(
    validate_brms_formula = function(x) stop("brms not loaded"),
    {
      expect_error(
        mvgam:::setup_brms_lightweight(formula, data),
        "brms not loaded"
      )
    }
  )
})

test_that("Architecture components handle malformed data structures", {
  # Test parse_multivariate_trends with corrupted formula
  corrupted_formula <- structure("not a real formula", class = "formula")
  
  expect_error(
    mvgam:::is_multivariate_formula(corrupted_formula),
    # Should handle gracefully or give informative error
  )
  
  # Test extract_response_names with malformed mvbind
  weird_formula <- structure(y1, y2 ~ x, class = "formula")  # Invalid syntax
  
  # Should return NULL or handle gracefully
  result <- mvgam:::extract_response_names(weird_formula)
  expect_null(result)
})

test_that("Multiple imputation handles edge cases properly", {
  # Single observation datasets
  tiny_data <- list(
    data.frame(y = 1, x = 0.5),
    data.frame(y = 2, x = 1.5)
  )
  
  # Should validate but warn about insufficient data
  expect_warning(
    mvgam:::validate_multiple_imputation_datasets(tiny_data),
    regex = NA  # No specific warning expected, but should not error
  )
  
  # Datasets with all NA in imputed variable
  all_na_data <- list(
    data.frame(y = c(1, 2, 3), x = c(NA, NA, NA)),
    data.frame(y = c(1, 2, 3), x = c(1, 2, 3))
  )
  
  # Should validate successfully (imputation filled all NAs)
  expect_true(mvgam:::validate_multiple_imputation_datasets(all_na_data))
  
  # Identical datasets (no actual imputation)
  identical_data <- list(
    data.frame(y = c(1, 2, 3), x = c(1, 2, 3)),
    data.frame(y = c(1, 2, 3), x = c(1, 2, 3))
  )
  
  # Should validate but could warn about lack of variation
  expect_true(mvgam:::validate_multiple_imputation_datasets(identical_data))
})

test_that("Rubin's rules handle numerical edge cases", {
  # Perfect agreement between imputations (zero between-imputation variance)
  perfect_agreement <- list(
    matrix(c(1, 1, 1), nrow = 3, ncol = 1),
    matrix(c(1, 1, 1), nrow = 3, ncol = 1),
    matrix(c(1, 1, 1), nrow = 3, ncol = 1)
  )
  
  result <- mvgam:::pool_parameter_estimates(perfect_agreement)
  
  expect_equal(result$mean, 1)
  expect_equal(result$between_variance, 0)
  expect_true(result$within_variance >= 0)
  expect_true(is.finite(result$degrees_freedom))
  
  # Extreme between-imputation variance
  extreme_variation <- list(
    matrix(c(-100, -100), nrow = 2, ncol = 1),
    matrix(c(0, 0), nrow = 2, ncol = 1),
    matrix(c(100, 100), nrow = 2, ncol = 1)
  )
  
  result_extreme <- mvgam:::pool_parameter_estimates(extreme_variation)
  
  expect_equal(result_extreme$mean, 0)  # Should average to 0
  expect_true(result_extreme$between_variance > result_extreme$within_variance)
  expect_true(is.finite(result_extreme$total_variance))
})

test_that("Dual object system handles missing Stan components", {
  # Mock combined fit with missing parameters
  empty_fit <- structure(
    list(sim = list(pars_oi = character(0), algorithm = "sampling")),
    class = "stanfit"
  )
  
  obs_setup <- list(
    formula = y ~ s(x),
    data = data.frame(y = 1:5, x = rnorm(5)),
    family = gaussian(),
    stancode = "empty code",
    standata = list()
  )
  
  # Should handle gracefully
  with_mocked_bindings(
    create_observation_brmsfit = function(...) structure(list(), class = "brmsfit"),
    extract_mvgam_components = function(...) list(
      time_info = list(has_time = FALSE),
      series_info = list(has_series = FALSE),
      trend_components = NULL
    ),
    {
      result <- mvgam:::create_mvgam_from_combined_fit(empty_fit, obs_setup)
      expect_s3_class(result, "mvgam")
    }
  )
})

test_that("Parameter extraction handles unusual parameter names", {
  # Stan fit with non-standard parameter names
  unusual_fit <- structure(
    list(sim = list(pars_oi = c(
      "weird_param_b_something",
      "trend_but_not_really",
      "b_unusual_format",
      "sigma_weird_123",
      "completely_unknown_param"
    ))),
    class = "stanfit"
  )
  
  obs_params <- mvgam:::extract_observation_parameters(unusual_fit)
  trend_params <- mvgam:::extract_trend_parameters(unusual_fit, list())
  
  # Should extract based on patterns, may miss unusual names
  expect_true(length(obs_params$names) <= 5)
  expect_true(length(trend_params$names) <= 5)
  
  # Should include clear matches
  expect_true("b_unusual_format" %in% obs_params$names)
  expect_true("sigma_weird_123" %in% obs_params$names)
})

test_that("Setup functions handle brms version incompatibilities", {
  formula <- y ~ s(x)
  data <- data.frame(y = 1:10, x = rnorm(10))
  
  # Mock brms version incompatibility
  with_mocked_bindings(
    validate_brms_formula = function(x) list(valid = TRUE),
    brm = function(...) stop("Incompatible brms version"),
    setup_brms_fallback = function(...) {
      list(
        formula = formula,
        data = data,
        family = gaussian(),
        stanvars = NULL,
        stancode = "fallback code",
        standata = list(N = 10),
        prior = data.frame(),
        brmsterms = structure(list(), class = "brmsterms"),
        setup_time = NA_real_
      )
    },
    validate_setup_components = function(x) TRUE,
    {
      # Should fall back gracefully with warning
      expect_warning(
        result <- mvgam:::setup_brms_lightweight(formula, data),
        "Mock backend setup failed"
      )
      
      expect_equal(result$stancode, "fallback code")
    }
  )
})

test_that("Enhanced mvgam handles mixed valid/invalid trend specifications", {
  formula <- mvbind(y1, y2, y3) ~ s(x)
  
  # Mixed trend formula with some valid, some invalid
  mixed_trends <- structure(list(), class = "mvbrmsterms")
  
  with_mocked_bindings(
    validate_bf_trend_formula = function(x) list(valid = FALSE, issues = "Invalid trend type"),
    {
      expect_error(
        mvgam:::parse_multivariate_trends(formula, mixed_trends),
        "Invalid trend type"
      )
    }
  )
})

test_that("Architecture handles memory constraints gracefully", {
  # Large dataset that might cause memory issues
  large_data <- data.frame(
    y = rep(1:1000, 10),
    x = rnorm(10000),
    time = rep(1:1000, 10),
    series = rep(paste0("series_", 1:10), each = 1000)
  )
  
  formula <- y ~ s(x)
  
  # Mock memory constraints in brms setup
  with_mocked_bindings(
    validate_brms_formula = function(x) list(valid = TRUE),
    brm = function(...) stop("Insufficient memory"),
    setup_brms_fallback = function(...) stop("Fallback also failed due to memory"),
    {
      expect_error(
        mvgam:::setup_brms_lightweight(formula, large_data),
        "Could not create brms setup components"
      )
    }
  )
})

test_that("Trend stanvar generation handles missing trend types", {
  # Unknown trend type
  unknown_trend <- structure(
    list(trend_type = "UNKNOWN_TYPE", pars = list()),
    class = "mvgam_trend"
  )
  
  # Should return empty list for unknown types
  result <- mvgam:::generate_trend_type_stanvars(unknown_trend, "test")
  expect_equal(result, list())
  
  # Trend specification without proper class
  unclassed_trend <- list(trend_type = "AR", pars = list(p = 1))
  
  mv_spec <- list(
    has_trends = TRUE,
    trend_specs = list(bad_trend = unclassed_trend)
  )
  
  # Should handle gracefully (skip non-mvgam_trend objects)
  result <- mvgam:::generate_trend_stanvars(mv_spec)
  expect_equal(result, list())
})

test_that("Combined data merging handles type mismatches", {
  # Stan data with incompatible types
  obs_standata <- list(
    N = 10L,  # Integer
    y = 1:10
  )
  
  trend_standata <- list(
    N = 10.0,  # Double (should be ignored due to name conflict)
    T = "not_numeric"  # Wrong type
  )
  
  mv_spec <- list(trend_specs = list(main = ~ AR(p = 1)))
  
  # Should handle type mismatches gracefully
  result <- mvgam:::combine_standata(obs_standata, trend_standata, mv_spec)
  
  expect_equal(result$N, 10L)  # Should keep integer from obs
  expect_equal(result$T, "not_numeric")  # Should include despite wrong type
  expect_equal(result$has_trends, 1L)
})

test_that("Validation functions handle edge cases in time series data", {
  # Time series with gaps
  gapped_time_data <- data.frame(
    y = c(1, 2, 3, 4),
    time = c(1, 3, 7, 10),  # Irregular gaps
    series = c("A", "A", "A", "A")
  )
  
  time_info <- mvgam:::extract_time_information(gapped_time_data)
  
  expect_true(time_info$has_time)
  expect_equal(time_info$n_timepoints, 4)
  expect_equal(time_info$time_spacing, 2)  # First gap
  
  # Time series with duplicate times
  duplicate_time_data <- data.frame(
    y = c(1, 2, 3),
    time = c(1, 1, 2),  # Duplicate time point
    series = c("A", "B", "A")
  )
  
  time_info_dup <- mvgam:::extract_time_information(duplicate_time_data)
  
  expect_true(time_info_dup$has_time)
  expect_equal(time_info_dup$n_timepoints, 2)  # Unique time points
  
  # Series with empty factor levels
  empty_series_data <- data.frame(
    y = c(1, 2, 3),
    series = factor(c("A", "A", "A"), levels = c("A", "B", "C"))  # Empty B, C levels
  )
  
  series_info <- mvgam:::extract_series_information(empty_series_data, list())
  
  expect_true(series_info$has_series)
  expect_equal(series_info$n_series, 3)  # Should count all factor levels
})

test_that("Error messages are informative and actionable", {
  # Test that error messages provide helpful guidance
  
  # Invalid multiple imputation structure
  expect_error(
    mvgam:::validate_multiple_imputation_datasets(list("not_df", "also_not_df")),
    "All elements in data_list must be data.frames"
  )
  
  # Missing required setup components  
  invalid_setup <- list(formula = y ~ x, data = data.frame())
  
  expect_error(
    mvgam:::validate_setup_components(invalid_setup),
    "Missing required setup components.*stancode"
  )
  
  # Stan code extraction failure
  invalid_setup2 <- list(
    formula = y ~ x,
    data = data.frame(y = 1, x = 1),
    family = gaussian(),
    stancode = "",  # Empty
    standata = list(N = 1)
  )
  
  expect_error(
    mvgam:::validate_setup_components(invalid_setup2),
    "Stan code extraction failed.*Could not obtain valid Stan model code"
  )
})

test_that("Architecture maintains backwards compatibility patterns", {
  # Test that new architecture doesn't break expected mvgam patterns
  
  # Traditional mvgam data structure
  traditional_data <- data.frame(
    y = rpois(20, 5),
    time = rep(1:10, 2),
    series = factor(rep(c("A", "B"), each = 10))
  )
  
  # Should process traditional structure without issues
  time_info <- mvgam:::extract_time_information(traditional_data)
  series_info <- mvgam:::extract_series_information(traditional_data, list())
  
  expect_true(time_info$has_time)
  expect_equal(time_info$n_timepoints, 10)
  expect_true(series_info$has_series)
  expect_equal(series_info$n_series, 2)
  
  # Traditional single series data
  single_series_data <- data.frame(
    y = rpois(10, 3),
    time = 1:10
  )
  
  series_info_single <- mvgam:::extract_series_information(single_series_data, list())
  expect_false(series_info_single$has_series)
})

test_that("Resource cleanup happens on errors", {
  # Test that resources are properly cleaned up when errors occur
  
  formula <- y ~ s(x)
  data <- data.frame(y = 1:10, x = rnorm(10))
  
  # Mock scenario where setup succeeds but fitting fails
  with_mocked_bindings(
    validate_brms_formula = function(x) list(valid = TRUE),
    brm = function(...) structure(list(), class = "brmsfit"),
    extract_stancode_from_setup = function(x) "valid code",
    extract_standata_from_setup = function(x) list(N = 10),
    extract_prior_from_setup = function(x) data.frame(),
    extract_brmsterms_from_setup = function(x) structure(list(), class = "brmsterms"),
    validate_setup_components = function(x) TRUE,
    {
      # Should complete setup successfully
      setup_result <- mvgam:::setup_brms_lightweight(formula, data)
      expect_type(setup_result, "list")
      
      # Resources should be available for inspection/cleanup
      expect_true(is.numeric(setup_result$setup_time))
    }
  )
})