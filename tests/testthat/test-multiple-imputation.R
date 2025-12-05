test_that("mvgam_multiple validates input correctly", {
  # Valid inputs
  formula <- y ~ s(x)
  trend_formula <- ~ AR(p = 1)
  data_list <- list(
    data.frame(y = 1:10, x = rnorm(10), time = 1:10, series = "A"),
    data.frame(y = 1:10, x = rnorm(10), time = 1:10, series = "A")
  )

  with_mocked_bindings(
    validate_multiple_imputation_datasets = function(x) TRUE,
    fit_multiple_imputation_models = function(...) list(
      fit1 = structure(list(), class = "mvgam"),
      fit2 = structure(list(), class = "mvgam")
    ),
    pool_mvgam_fits = function(x) structure(list(), class = c("mvgam_pooled", "mvgam")),
    {
      # Should work with valid inputs
      result <- mvgam:::mvgam_multiple(formula, trend_formula, data_list)
      expect_s3_class(result, "mvgam_pooled")
    }
  )

  # Invalid formula
  expect_error(
    mvgam:::mvgam_multiple("not a formula", trend_formula, data_list),
    "Assertion on 'formula' failed"
  )

  # Invalid data_list (not a list)
  expect_error(
    mvgam:::mvgam_multiple(formula, trend_formula, data.frame()),
    "Assertion on 'data_list' failed"
  )

  # Too few datasets
  expect_error(
    mvgam:::mvgam_multiple(formula, trend_formula, list(data.frame())),
    "Assertion on 'data_list' failed"
  )
})

test_that("mvgam_multiple handles combine parameter correctly", {
  formula <- y ~ s(x)
  data_list <- list(
    data.frame(y = 1:5, x = rnorm(5)),
    data.frame(y = 1:5, x = rnorm(5))
  )

  mock_fits <- list(
    structure(list(imputation_id = 1), class = "mvgam"),
    structure(list(imputation_id = 2), class = "mvgam")
  )

  with_mocked_bindings(
    validate_multiple_imputation_datasets = function(x) TRUE,
    fit_multiple_imputation_models = function(...) mock_fits,
    pool_mvgam_fits = function(x) structure(list(pooled = TRUE), class = c("mvgam_pooled", "mvgam")),
    {
      # combine = TRUE should return pooled object
      result1 <- mvgam:::mvgam_multiple(formula, NULL, data_list, combine = TRUE)
      expect_s3_class(result1, "mvgam_pooled")
      expect_true(result1$pooled)

      # combine = FALSE should return list of individual fits
      result2 <- mvgam:::mvgam_multiple(formula, NULL, data_list, combine = FALSE)
      expect_type(result2, "list")
      expect_length(result2, 2)
      expect_s3_class(result2[[1]], "mvgam")
    }
  )
})

test_that("validate_multiple_imputation_datasets checks structure consistency", {
  # Valid datasets
  valid_data <- list(
    data.frame(y = 1:5, x = c(1, 2, NA, 4, 5), time = 1:5, series = "A"),
    data.frame(y = 1:5, x = c(1, 2, 3, 4, 5), time = 1:5, series = "A") # x[3] imputed
  )

  expect_true(mvgam:::validate_multiple_imputation_datasets(valid_data))

  # Different column names
  invalid_names <- list(
    data.frame(y = 1:5, x = rnorm(5)),
    data.frame(y = 1:5, z = rnorm(5))  # Different column name
  )

  expect_error(
    mvgam:::validate_multiple_imputation_datasets(invalid_names),
    "Dataset 2 has different column names"
  )

  # Different number of rows
  invalid_rows <- list(
    data.frame(y = 1:5, x = rnorm(5)),
    data.frame(y = 1:3, x = rnorm(3))  # Different number of rows
  )

  expect_error(
    mvgam:::validate_multiple_imputation_datasets(invalid_rows),
    "Dataset 2 has 3 rows, expected 5"
  )

  # Non-data.frame elements
  invalid_type <- list(
    data.frame(y = 1:5, x = rnorm(5)),
    "not a data frame"
  )

  expect_error(
    mvgam:::validate_multiple_imputation_datasets(invalid_type),
    "All elements in data_list must be data.frames"
  )
})

test_that("validate_multiple_imputation_datasets checks essential columns", {
  # Time column differs between datasets
  time_differs <- list(
    data.frame(y = 1:3, time = 1:3, series = c("A", "A", "A")),
    data.frame(y = 1:3, time = 2:4, series = c("A", "A", "A"))  # Different time values
  )

  expect_error(
    mvgam:::validate_multiple_imputation_datasets(time_differs),
    "Column time differs between datasets"
  )

  # Series column differs between datasets
  series_differs <- list(
    data.frame(y = 1:3, time = 1:3, series = c("A", "A", "B")),
    data.frame(y = 1:3, time = 1:3, series = c("A", "B", "B"))  # Different series values
  )

  expect_error(
    mvgam:::validate_multiple_imputation_datasets(series_differs),
    "Column series differs between datasets"
  )
})

test_that("validate_missing_patterns warns about structural variables", {
  # Weights should not vary between imputations
  data_with_varying_weights <- list(
    data.frame(y = 1:3, x = rnorm(3), weights = c(1, 1, 1)),
    data.frame(y = 1:3, x = rnorm(3), weights = c(1, 2, 1))  # weights differ
  )

  expect_warning(
    mvgam:::validate_missing_patterns(data_with_varying_weights),
    "Column weights varies between imputed datasets"
  )

  # Trials should not vary
  data_with_varying_trials <- list(
    data.frame(y = 1:3, x = rnorm(3), trials = c(10, 10, 10)),
    data.frame(y = 1:3, x = rnorm(3), trials = c(10, 10, 15))  # trials differ
  )

  expect_warning(
    mvgam:::validate_missing_patterns(data_with_varying_trials),
    "Column trials varies between imputed datasets"
  )
})

test_that("fit_multiple_imputation_models processes all datasets", {
  formula <- y ~ s(x)
  trend_formula <- ~ RW()

  data_list <- list(
    data.frame(y = 1:5, x = rnorm(5)),
    data.frame(y = 1:5, x = rnorm(5)),
    data.frame(y = 1:5, x = rnorm(5))
  )

  # Mock individual fitting function
  fit_call_count <- 0

  with_mocked_bindings(
    format_message = function(...) invisible(),
    mvgam_single_imputation = function(formula, trend_formula, data, backend, imputation_id, ...) {
      fit_call_count <<- fit_call_count + 1
      structure(
        list(imputation_id = imputation_id, data_nrow = nrow(data)),
        class = "mvgam"
      )
    },
    {
      result <- mvgam:::fit_multiple_imputation_models(
        formula, trend_formula, data_list, "cmdstanr"
      )

      expect_length(result, 3)
      expect_equal(fit_call_count, 3)
      expect_named(result, paste0("imputation_", 1:3))

      # Check that each fit has correct metadata
      expect_equal(result[[1]]$imputation_id, 1)
      expect_equal(result[[1]]$n_imputations, 3)
      expect_equal(result[[2]]$imputation_id, 2)
      expect_equal(result[[3]]$imputation_id, 3)
    }
  )
})

test_that("mvgam_single_imputation integrates architecture components", {
  formula <- y ~ s(x)
  trend_formula <- ~ AR(p = 1)
  data <- data.frame(y = 1:10, x = rnorm(10))

  mock_mv_spec <- list(has_trends = TRUE, base_formula = trend_formula)
  mock_obs_setup <- list(formula = formula, data = data)
  mock_trend_setup <- list(formula = trend_formula, data = data)
  mock_combined <- list(stancode = "combined code", standata = list())
  mock_fit <- structure(list(), class = "stanfit")
  mock_mvgam <- structure(list(), class = "mvgam")

  with_mocked_bindings(
    parse_multivariate_trends = function(f, tf) mock_mv_spec,
    validate_autocor_separation = function(f, tf) TRUE,
    setup_brms_lightweight = function(formula, data, ...) {
      if (identical(formula, trend_formula)) mock_trend_setup else mock_obs_setup
    },
    extract_trend_stanvars_from_setup = function(ts, mvs) list(),
    generate_combined_stancode_and_data = function(...) mock_combined,
    fit_mvgam_model = function(...) mock_fit,
    create_mvgam_from_combined_fit = function(...) mock_mvgam,
    {
      result <- mvgam:::mvgam_single_imputation(
        formula, trend_formula, data, "cmdstanr", 1
      )

      expect_s3_class(result, "mvgam")
    }
  )
})

test_that("pool_mvgam_fits validates inputs and creates pooled object", {
  # Valid mvgam fits
  mock_fits <- list(
    structure(list(fit_id = 1), class = "mvgam"),
    structure(list(fit_id = 2), class = "mvgam"),
    structure(list(fit_id = 3), class = "mvgam")
  )

  mock_estimates <- list(
    observation = list(mean = c(1, 2), variance = c(0.1, 0.2)),
    trend = list(mean = c(0.5), variance = c(0.05))
  )

  mock_pooled_estimates <- list(
    observation = list(
      mean = c(1.1, 2.1),
      variance = c(0.12, 0.22),
      n_imputations = 3
    )
  )

  with_mocked_bindings(
    format_message = function(...) invisible(),
    extract_fit_estimates = function(fit) mock_estimates,
    apply_rubins_rules = function(est_list) mock_pooled_estimates,
    create_pooled_mvgam = function(template, pooled) {
      structure(list(template_id = template$fit_id, pooled = TRUE), class = "mvgam")
    },
    {
      result <- mvgam:::pool_mvgam_fits(mock_fits)

      expect_s3_class(result, c("mvgam_pooled", "mvgam", "brmsfit"))
      expect_equal(attr(result, "n_imputations"), 3)
      expect_equal(attr(result, "pooling_method"), "rubins_rules")
      expect_s3_class(attr(result, "pooled_time"), "POSIXct")
    }
  )

  # Invalid fits (not all mvgam objects)
  invalid_fits <- list(
    structure(list(), class = "mvgam"),
    "not an mvgam object"
  )

  expect_error(
    mvgam:::pool_mvgam_fits(invalid_fits),
    "All fits must be mvgam objects"
  )
})

test_that("extract_fit_estimates processes mvgam objects correctly", {
  # Mock mvgam object with obs_fit, trend_fit, and combined_fit
  mock_mvgam <- structure(
    list(
      obs_fit = structure(list(params = "obs"), class = "brmsfit"),
      trend_fit = structure(list(params = "trend"), class = "brmsfit"),
      combined_fit = structure(list(params = "combined"), class = "stanfit")
    ),
    class = "mvgam"
  )

  with_mocked_bindings(
    extract_posterior_samples = function(fit) {
      if (inherits(fit, "brmsfit")) {
        matrix(rnorm(20), nrow = 10, ncol = 2)
      } else {
        matrix(rnorm(30), nrow = 10, ncol = 3)
      }
    },
    {
      result <- mvgam:::extract_fit_estimates(mock_mvgam)

      expect_named(result, c("observation", "trend", "combined"))
      expect_true(is.matrix(result$observation))
      expect_true(is.matrix(result$trend))
      expect_true(is.matrix(result$combined))
    }
  )

  # mvgam object without trends
  mock_mvgam_no_trends <- structure(
    list(
      obs_fit = structure(list(), class = "brmsfit"),
      trend_fit = NULL,
      combined_fit = structure(list(), class = "stanfit")
    ),
    class = "mvgam"
  )

  with_mocked_bindings(
    extract_posterior_samples = function(fit) matrix(rnorm(20), nrow = 10, ncol = 2),
    {
      result <- mvgam:::extract_fit_estimates(mock_mvgam_no_trends)

      expect_named(result, c("observation", "trend", "combined"))
      expect_null(result$trend)
    }
  )
})

test_that("apply_rubins_rules implements correct pooling", {
  # Mock parameter estimates from 3 imputations
  estimates_list <- list(
    # Imputation 1
    list(
      observation = matrix(c(1.0, 2.0, 1.1, 2.1, 0.9, 1.9), nrow = 3, ncol = 2),
      trend = matrix(c(0.5, 0.6, 0.4), nrow = 3, ncol = 1)
    ),
    # Imputation 2
    list(
      observation = matrix(c(1.2, 2.2, 1.0, 2.0, 1.1, 2.1), nrow = 3, ncol = 2),
      trend = matrix(c(0.7, 0.5, 0.6), nrow = 3, ncol = 1)
    ),
    # Imputation 3
    list(
      observation = matrix(c(0.8, 1.8, 1.2, 2.2, 1.0, 2.0), nrow = 3, ncol = 2),
      trend = matrix(c(0.4, 0.7, 0.5), nrow = 3, ncol = 1)
    )
  )

  with_mocked_bindings(
    pool_parameter_estimates = function(param_list) {
      # Simple mock pooling result
      list(
        mean = apply(param_list[[1]], 2, mean),
        variance = apply(param_list[[1]], 2, var),
        n_imputations = length(param_list)
      )
    },
    {
      result <- mvgam:::apply_rubins_rules(estimates_list)

      expect_named(result, c("observation", "trend"))
      expect_equal(result$observation$n_imputations, 3)
      expect_equal(result$trend$n_imputations, 3)
    }
  )
})

test_that("pool_parameter_estimates implements Rubin's rules correctly", {
  # Simple test case with known results
  param_matrices <- list(
    matrix(c(1, 2), nrow = 2, ncol = 1), # Mean = 1.5, Var = 0.5
    matrix(c(2, 3), nrow = 2, ncol = 1), # Mean = 2.5, Var = 0.5
    matrix(c(0, 1), nrow = 2, ncol = 1)  # Mean = 0.5, Var = 0.5
  )

  result <- mvgam:::pool_parameter_estimates(param_matrices)

  # Check structure
  expect_named(result, c("mean", "variance", "se", "within_variance",
                        "between_variance", "relative_increase",
                        "degrees_freedom", "n_imputations"))

  # Check pooled mean (should be average of imputation means)
  expected_pooled_mean <- mean(c(1.5, 2.5, 0.5))  # 1.5
  expect_equal(result$mean, expected_pooled_mean, tolerance = 1e-10)

  # Check within-imputation variance (average of individual variances)
  expected_within_var <- 0.5  # All have same within variance
  expect_equal(result$within_variance, expected_within_var, tolerance = 1e-10)

  # Check that total variance > within variance (accounts for between-imputation uncertainty)
  expect_true(result$variance > result$within_variance)

  expect_equal(result$n_imputations, 3)
})

test_that("create_pooled_mvgam creates proper structure", {
  template_fit <- structure(
    list(
      formula = y ~ s(x),
      family = poisson(),
      data = data.frame(y = 1:10, x = rnorm(10))
    ),
    class = "mvgam"
  )

  pooled_estimates <- list(
    observation = list(mean = c(1, 2), variance = c(0.1, 0.2)),
    trend = list(mean = c(0.5), variance = c(0.05))
  )

  with_mocked_bindings(
    extract_pooling_diagnostics = function(pe) {
      list(
        observation = list(avg_relative_increase = 0.1),
        trend = list(avg_relative_increase = 0.05)
      )
    },
    {
      result <- mvgam:::create_pooled_mvgam(template_fit, pooled_estimates)

      # Should preserve template structure
      expect_equal(result$formula, template_fit$formula)
      expect_equal(result$family, template_fit$family)

      # Should add pooled components
      expect_equal(result$pooled_estimates, pooled_estimates)
      expect_true(result$pooled)
      expect_type(result$pooling_diagnostics, "list")
    }
  )
})

test_that("extract_pooling_diagnostics computes appropriate metrics", {
  pooled_estimates <- list(
    observation = list(
      relative_increase = c(0.1, 0.2, 0.15),
      degrees_freedom = c(100, 120, 110),
      n_imputations = 5
    ),
    trend = list(
      relative_increase = c(0.05),
      degrees_freedom = c(150),
      n_imputations = 5
    )
  )

  result <- mvgam:::extract_pooling_diagnostics(pooled_estimates)

  expect_named(result, c("observation", "trend"))

  # Check observation diagnostics
  obs_diag <- result$observation
  expect_equal(obs_diag$n_imputations, 5)
  expect_equal(obs_diag$avg_relative_increase, mean(c(0.1, 0.2, 0.15)))
  expect_equal(obs_diag$avg_degrees_freedom, mean(c(100, 120, 110)))

  # Check fraction missing information calculation
  expected_fmi <- c(0.1, 0.2, 0.15) / (1 + c(0.1, 0.2, 0.15))
  expect_equal(obs_diag$fraction_missing_info, expected_fmi)
})
