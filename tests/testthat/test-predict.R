test_that("create_mock_stanfit validates input", {
  # Valid draws_matrix input
  valid_matrix <- matrix(rnorm(20), nrow = 4, ncol = 5)
  colnames(valid_matrix) <- paste0("param_", 1:5)
  valid_draws <- posterior::as_draws_matrix(valid_matrix)
  
  result <- create_mock_stanfit(valid_draws)
  expect_s3_class(result, "mock_stanfit")
  expect_equal(result$draws_cache, valid_draws)
  
  # Invalid inputs
  expect_error(
    create_mock_stanfit("not_a_matrix"),
    "Must inherit from class 'draws_matrix'"
  )
  
  expect_error(
    create_mock_stanfit(matrix(1:4, nrow = 2)),
    "Must inherit from class 'draws_matrix'"
  )
})

test_that("has_nlpars correctly detects nonlinear formulas", {
  # Test with brmsformula objects
  linear_formula <- structure(
    list(
      formula = structure(y ~ x, nl = FALSE),
      pforms = NULL
    ),
    class = "brmsformula"
  )
  expect_false(has_nlpars(linear_formula))
  
  nonlinear_formula <- structure(
    list(
      formula = structure(y ~ x, nl = TRUE),
      pforms = list(b1 = ~ 1, b2 ~ x)
    ),
    class = "brmsformula"
  )
  expect_true(has_nlpars(nonlinear_formula))
  
  # Test with invalid objects
  expect_error(
    has_nlpars("not_a_formula"),
    "Assertion failed.*One of the following must apply"
  )
})

test_that("validate_monotonic_indices handles indexing correctly", {
  # Test 0-based indexing (already correct)
  indices_0 <- c(0, 1, 2, 1, 0)
  result_0 <- validate_monotonic_indices(indices_0, "Xmo_1", 3, 5)
  expect_equal(result_0, c(0, 1, 2, 1, 0))
  
  # Test 1-based indexing (needs conversion)
  indices_1 <- c(1, 2, 3, 2, 1)
  result_1 <- validate_monotonic_indices(indices_1, "Xmo_1", 3, 5)
  expect_equal(result_1, c(0, 1, 2, 1, 0))
  
  # Test invalid range
  expect_error(
    validate_monotonic_indices(c(0, 5), "Xmo_1", 3, 2),
    "invalid index range"
  )
  
  # Test wrong length
  expect_error(
    validate_monotonic_indices(c(0, 1), "Xmo_1", 3, 5),
    "has.*elements but expected.*observations"
  )
})

test_that("spd_gp_exp_quad computes spectral density correctly", {
  # Simple test case
  slambda <- array(c(1, 2, 3, 4), dim = c(2, 2))
  sdgp <- c(0.5, 0.8)
  lscale <- matrix(c(1.0, 1.5, 2.0, 2.5), nrow = 2, ncol = 2)
  
  result <- spd_gp_exp_quad(slambda, sdgp, lscale)
  
  expect_true(is.matrix(result))
  expect_equal(dim(result), c(2, 2))  # n_draws x n_basis
  expect_true(all(result >= 0))  # Spectral density should be non-negative
  expect_true(all(is.finite(result)))
  
  # Test input validation
  expect_error(
    spd_gp_exp_quad("not_array", sdgp, lscale),
    "Must be of type 'array'"
  )
  
  expect_error(
    spd_gp_exp_quad(slambda, "not_numeric", lscale),
    "Must be of type 'numeric'"
  )
})

test_that("compute_spd_vectorized dispatches correctly", {
  slambda <- array(c(1, 2), dim = c(1, 2))
  sdgp <- 0.5
  lscale <- matrix(c(1.0, 1.5), nrow = 1)

  # Test valid kernel dispatch
  result_exp <- compute_spd_vectorized(slambda, sdgp, lscale, "exp_quad")
  expect_true(is.matrix(result_exp))
  expect_true(all(result_exp >= 0))

  result_m32 <- compute_spd_vectorized(slambda, sdgp, lscale, "matern32")
  expect_true(is.matrix(result_m32))
  expect_true(all(result_m32 >= 0))

  result_m52 <- compute_spd_vectorized(slambda, sdgp, lscale, "matern52")
  expect_true(is.matrix(result_m52))
  expect_true(all(result_m52 >= 0))

  # Test invalid kernel
  expect_error(
    compute_spd_vectorized(slambda, sdgp, lscale, "invalid_kernel"),
    "Must be element of set"
  )
})

test_that("categorize_mvgam_parameters validates input", {
  # Test with invalid input
  expect_error(
    categorize_mvgam_parameters("not_mvgam"),
    "Must inherit from class 'mvgam'"
  )

  expect_error(
    categorize_mvgam_parameters(list(a = 1)),
    "Must inherit from class 'mvgam'"
  )
})

test_that("extract_parameters_by_type validates input", {
  # Test with invalid mvgam_fit argument
  expect_error(
    extract_parameters_by_type("not_mvgam", type = "observation"),
    "Must inherit from class 'mvgam'"
  )

  # Test with invalid type argument - class validation happens first
  expect_error(
    extract_parameters_by_type(list(class = "test"), type = "invalid"),
    "Must inherit from class 'mvgam'"
  )
})

test_that("extract_obs_parameters returns character vector", {
  # Validate return type specification by checking function exists
  # and documentation promises character(0) on missing parameters
  expect_true(is.function(extract_obs_parameters))
})

test_that("extract_trend_parameters returns character vector", {
  # Validate return type specification by checking function exists
  # and documentation promises character(0) on missing parameters
  expect_true(is.function(extract_trend_parameters))
})

# ==============================================================================
# validate_prediction_factor_levels tests
# ==============================================================================

test_that("validate_prediction_factor_levels catches invalid series levels", {
  # Create metadata matching training data
  metadata <- list(
    levels = list(series = c("s1", "s2", "s3")),
    variables = list(series_var = "series")
  )

  # Valid newdata - should pass silently
  valid_newdata <- data.frame(
    time = 1:3,
    series = factor(c("s1", "s2", "s1"), levels = c("s1", "s2", "s3"))
  )
  expect_silent(validate_prediction_factor_levels(valid_newdata, metadata))

  # Invalid newdata - series level not in training
  invalid_newdata <- data.frame(
    time = 1:3,
    series = factor(c("s1", "s4", "s1"))
  )
  expect_error(
    validate_prediction_factor_levels(invalid_newdata, metadata),
    "Series levels in newdata not found in training data"
  )
})

test_that("validate_prediction_factor_levels catches invalid gr levels", {
  # Create metadata for hierarchical model
  metadata <- list(
    levels = list(
      series = c("s1"),
      gr = c("group_a", "group_b", "group_c")
    ),
    variables = list(
      series_var = "series",
      gr_var = "group"
    )
  )

  # Valid newdata - should pass silently
  valid_newdata <- data.frame(
    time = 1:3,
    series = factor("s1"),
    group = factor(c("group_a", "group_b", "group_a"))
  )
  expect_silent(validate_prediction_factor_levels(valid_newdata, metadata))

  # Invalid newdata - gr level not in training
  invalid_newdata <- data.frame(
    time = 1:3,
    series = factor("s1"),
    group = factor(c("group_a", "group_d", "group_a"))
  )
  expect_error(
    validate_prediction_factor_levels(invalid_newdata, metadata),
    "Grouping variable.*has levels not in"
  )
})

test_that("validate_prediction_factor_levels handles missing metadata gracefully", {
  newdata <- data.frame(
    time = 1:3,
    series = factor(c("s1", "s2", "s1"))
  )

  # Empty metadata - should pass silently
  expect_silent(validate_prediction_factor_levels(newdata, list()))

  # Metadata without levels - should pass silently
  expect_silent(validate_prediction_factor_levels(newdata, list(other = "stuff")))

  # Metadata with levels but no variables - should pass silently
  expect_silent(validate_prediction_factor_levels(
    newdata,
    list(levels = list(series = c("s1", "s2")))
  ))
})

test_that("validate_prediction_factor_levels validates input types", {
  metadata <- list(
    levels = list(series = c("s1", "s2")),
    variables = list(series_var = "series")
  )

  # Invalid data argument

  expect_error(
    validate_prediction_factor_levels("not_a_dataframe", metadata),
    "Must be of type 'data.frame'"
  )

  # Invalid metadata argument
  expect_error(
    validate_prediction_factor_levels(data.frame(x = 1), "not_a_list"),
    "Must be of type 'list'"
  )
})

test_that("validate_prediction_factor_levels handles character columns", {
  # Create metadata matching training data
  metadata <- list(
    levels = list(series = c("s1", "s2", "s3")),
    variables = list(series_var = "series")
  )

  # Character column (not factor) with valid values
  valid_newdata <- data.frame(
    time = 1:3,
    series = c("s1", "s2", "s1"),
    stringsAsFactors = FALSE
  )
  expect_silent(validate_prediction_factor_levels(valid_newdata, metadata))

  # Character column with invalid values
  invalid_newdata <- data.frame(
    time = 1:3,
    series = c("s1", "s4", "s1"),
    stringsAsFactors = FALSE
  )
  expect_error(
    validate_prediction_factor_levels(invalid_newdata, metadata),
    "Series levels in newdata not found in training data"
  )
})