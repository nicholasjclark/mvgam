test_that("create_mvgam_from_combined_fit validates inputs", {
  # Mock combined fit
  mock_fit <- structure(
    list(sim = list(pars_oi = c("b_Intercept", "sigma"), algorithm = "sampling")),
    class = "stanfit"
  )
  
  # Mock observation setup
  obs_setup <- list(
    formula = y ~ s(x),
    data = data.frame(y = 1:10, x = rnorm(10)),
    family = gaussian(),
    stancode = "obs stan code",
    standata = list(N = 10)
  )
  
  # Valid inputs should work
  with_mocked_bindings(
    create_observation_brmsfit = function(...) structure(list(), class = "brmsfit"),
    extract_mvgam_components = function(...) list(
      time_info = list(has_time = FALSE),
      series_info = list(has_series = FALSE),
      trend_components = NULL
    ),
    {
      result <- mvgam:::create_mvgam_from_combined_fit(mock_fit, obs_setup)
      
      expect_s3_class(result, "mvgam")
      expect_s3_class(result, "brmsfit")
    }
  )
  
  # Invalid combined fit
  expect_error(
    mvgam:::create_mvgam_from_combined_fit("not a stanfit", obs_setup),
    "Assertion on 'combined_fit' failed"
  )
  
  # Invalid obs_setup
  expect_error(
    mvgam:::create_mvgam_from_combined_fit(mock_fit, "not a list"),
    "Assertion on 'obs_setup' failed"
  )
})

test_that("create_mvgam_from_combined_fit creates proper structure without trends", {
  # Mock combined fit
  mock_fit <- structure(
    list(sim = list(pars_oi = c("b_Intercept", "sigma"), algorithm = "sampling")),
    class = "stanfit"
  )
  
  # Mock observation setup
  obs_setup <- list(
    formula = y ~ s(x),
    data = data.frame(y = 1:10, x = rnorm(10)),
    family = poisson(),
    stancode = "obs stan code",
    standata = list(N = 10)
  )
  
  # Mock mv_spec without trends
  mv_spec <- list(
    has_trends = FALSE,
    is_multivariate = FALSE,
    response_names = NULL
  )
  
  with_mocked_bindings(
    create_observation_brmsfit = function(cf, os, mvs) {
      structure(list(formula = os$formula, family = os$family), class = "brmsfit")
    },
    extract_mvgam_components = function(...) list(
      time_info = list(has_time = TRUE, n_timepoints = 10),
      series_info = list(has_series = TRUE, n_series = 2),
      trend_components = NULL
    ),
    {
      result <- mvgam:::create_mvgam_from_combined_fit(mock_fit, obs_setup, NULL, mv_spec)
      
      # Check basic structure
      expect_s3_class(result, c("mvgam", "brmsfit"))
      expect_s3_class(result$obs_fit, "brmsfit")
      expect_null(result$trend_fit)
      expect_equal(result$combined_fit, mock_fit)
      
      # Check stored specifications
      expect_equal(result$formula, obs_setup$formula)
      expect_null(result$trend_formula)
      expect_equal(result$family, obs_setup$family)
      expect_equal(result$data, obs_setup$data)
      
      # Check metadata
      expect_null(result$response_names)
      expect_true(result$series_info$has_series)
      expect_equal(result$series_info$n_series, 2)
    }
  )
})

test_that("create_mvgam_from_combined_fit creates proper structure with trends", {
  # Mock combined fit
  mock_fit <- structure(
    list(sim = list(pars_oi = c("b_Intercept", "sigma", "trend"), algorithm = "sampling")),
    class = "stanfit"
  )
  
  # Mock setups
  obs_setup <- list(formula = y ~ s(x), data = data.frame(y = 1:10, x = rnorm(10)), family = gaussian())
  trend_setup <- list(formula = ~ AR(p = 1), data = data.frame(y = 1:10, x = rnorm(10)), family = gaussian())
  
  # Mock mv_spec with trends
  mv_spec <- list(
    has_trends = TRUE,
    is_multivariate = TRUE,
    response_names = c("count", "biomass"),
    trend_specs = list(count = ~ AR(p = 1), biomass = ~ RW())
  )
  
  with_mocked_bindings(
    create_observation_brmsfit = function(...) structure(list(), class = "brmsfit"),
    create_trend_brmsfit = function(cf, ts, mvs) {
      structure(list(
        formula = ts$formula,
        trend_specs = mvs$trend_specs
      ), class = "brmsfit")
    },
    extract_mvgam_components = function(...) list(
      time_info = list(has_time = TRUE),
      series_info = list(is_multivariate = TRUE),
      trend_components = list(n_trends = 2)
    ),
    {
      result <- mvgam:::create_mvgam_from_combined_fit(mock_fit, obs_setup, trend_setup, mv_spec)
      
      # Check dual object structure
      expect_s3_class(result$obs_fit, "brmsfit")
      expect_s3_class(result$trend_fit, "brmsfit")
      
      # Check trend components
      expect_equal(result$trend_formula, trend_setup$formula)
      expect_equal(result$response_names, c("count", "biomass"))
      expect_equal(result$trend_components$n_trends, 2)
    }
  )
})

test_that("create_observation_brmsfit extracts observation parameters", {
  # Mock combined fit with mixed parameters
  mock_fit <- structure(
    list(sim = list(
      pars_oi = c("b_Intercept", "b_x", "sd_series", "sigma", "trend", "sigma_trend"),
      algorithm = "sampling"
    )),
    class = "stanfit"
  )
  
  obs_setup <- list(
    formula = y ~ s(x),
    data = data.frame(y = 1:10, x = rnorm(10)),
    family = poisson(),
    prior = data.frame(),
    stancode = "obs code",
    standata = list(N = 10)
  )
  
  with_mocked_bindings(
    extract_observation_parameters = function(cf) {
      list(names = c("b_Intercept", "b_x", "sd_series", "sigma"), count = 4)
    },
    subset_stanfit_parameters = function(fit, params) {
      attr(fit, "mvgam_subset") <- params
      fit
    },
    brms_version = function() "2.19.0",
    {
      result <- mvgam:::create_observation_brmsfit(mock_fit, obs_setup, NULL)
      
      expect_s3_class(result, "brmsfit")
      expect_equal(result$formula, obs_setup$formula)
      expect_equal(result$family, obs_setup$family)
      expect_equal(result$stancode, obs_setup$stancode)
      expect_equal(result$backend, "cmdstanr")
    }
  )
})

test_that("create_trend_brmsfit extracts trend parameters", {
  # Mock combined fit
  mock_fit <- structure(
    list(sim = list(
      pars_oi = c("b_Intercept", "trend", "sigma_trend", "phi_trend"),
      algorithm = "sampling"
    )),
    class = "stanfit"
  )
  
  trend_setup <- list(
    formula = ~ AR(p = 1),
    data = data.frame(trend = rnorm(10)),
    prior = data.frame(),
    stancode = "trend code",
    standata = list(N = 10)
  )
  
  mv_spec <- list(
    trend_specs = list(
      main = structure(list(trend_type = "AR"), class = "mvgam_trend")
    )
  )
  
  with_mocked_bindings(
    extract_trend_parameters = function(cf, mvs) {
      list(
        names = c("trend", "sigma_trend", "phi_trend"),
        count = 3,
        types = c(main = "AR")
      )
    },
    subset_stanfit_parameters = function(fit, params) {
      attr(fit, "mvgam_subset") <- params
      fit
    },
    brms_version = function() "2.19.0",
    {
      result <- mvgam:::create_trend_brmsfit(mock_fit, trend_setup, mv_spec)
      
      expect_s3_class(result, "brmsfit")
      expect_equal(result$formula, trend_setup$formula)
      expect_equal(result$family, gaussian()) # Trends are gaussian
      expect_equal(result$trend_specs, mv_spec$trend_specs)
      expect_equal(result$trend_types, c(main = "AR"))
    }
  )
})

test_that("extract_observation_parameters identifies correct parameters", {
  # Mock stanfit with mixed parameters
  mock_fit <- structure(
    list(sim = list(pars_oi = c(
      "b_Intercept", "b_x1", "b_x2",     # Fixed effects
      "sd_series", "sd_subject",          # Random effects SDs
      "cor_series", "r_series",           # Correlations and random effects
      "s_x1", "sds_x1",                  # Smooth terms
      "sigma", "shape", "nu",             # Family parameters
      "trend", "sigma_trend",             # Trend parameters (should be excluded)
      "phi_trend", "mu_trend",           # More trend parameters
      "lp__", "lprior"                   # Posterior components
    ))),
    class = "stanfit"
  )
  
  result <- mvgam:::extract_observation_parameters(mock_fit)
  
  # Should include observation parameters but exclude trend parameters
  expected_obs_params <- c(
    "b_Intercept", "b_x1", "b_x2",
    "sd_series", "sd_subject", 
    "cor_series", "r_series",
    "s_x1", "sds_x1",
    "sigma", "shape", "nu",
    "lp__", "lprior"
  )
  
  expect_equal(sort(result$names), sort(expected_obs_params))
  expect_equal(result$count, length(expected_obs_params))
})

test_that("extract_trend_parameters identifies correct parameters", {
  # Mock stanfit with mixed parameters
  mock_fit <- structure(
    list(sim = list(pars_oi = c(
      "b_Intercept", "sigma",             # Observation parameters (should be excluded)
      "trend", "sigma_trend",             # Trend parameters
      "phi_trend", "mu_trend",            # More trend parameters  
      "L_trend", "rho_trend"              # Correlation trend parameters
    ))),
    class = "stanfit"
  )
  
  # Mock mv_spec with trend specifications
  mv_spec <- list(
    trend_specs = list(
      series1 = structure(list(trend_type = "AR"), class = "mvgam_trend"),
      series2 = structure(list(trend_type = "RW"), class = "mvgam_trend")
    )
  )
  
  result <- mvgam:::extract_trend_parameters(mock_fit, mv_spec)
  
  # Should include only trend parameters
  expected_trend_params <- c("trend", "sigma_trend", "phi_trend", "mu_trend", "L_trend", "rho_trend")
  
  expect_equal(sort(result$names), sort(expected_trend_params))
  expect_equal(result$count, length(expected_trend_params))
  expect_equal(result$types, c(series1 = "AR", series2 = "RW"))
})

test_that("extract_mvgam_components processes time and series information", {
  mock_fit <- structure(list(), class = "stanfit")
  
  # Data with time and series information
  obs_setup <- list(
    data = data.frame(
      y = 1:20,
      time = rep(1:10, 2),
      series = rep(c("A", "B"), each = 10),
      x = rnorm(20)
    )
  )
  
  # Multivariate specification
  mv_spec <- list(
    response_names = c("count", "biomass"),
    has_trends = TRUE,
    trend_specs = list(
      count = structure(list(trend_type = "AR"), class = "mvgam_trend")
    )
  )
  
  with_mocked_bindings(
    extract_trend_component_info = function(cf, mvs) {
      list(n_trends = 1, types = c(count = "AR"))
    },
    {
      result <- mvgam:::extract_mvgam_components(mock_fit, obs_setup, NULL, mv_spec)
      
      # Check time information
      expect_true(result$time_info$has_time)
      expect_equal(result$time_info$n_timepoints, 10)
      expect_equal(result$time_info$time_range, c(1, 10))
      
      # Check series information  
      expect_true(result$series_info$has_series)
      expect_equal(result$series_info$n_series, 2)
      expect_equal(result$series_info$series_names, c("A", "B"))
      expect_true(result$series_info$is_multivariate)
      expect_equal(result$series_info$response_names, c("count", "biomass"))
      
      # Check trend components
      expect_equal(result$trend_components$n_trends, 1)
    }
  )
})

test_that("extract_time_information handles various time structures", {
  # Data with regular time intervals
  data_regular <- data.frame(
    y = 1:15,
    time = rep(1:5, 3),
    series = rep(letters[1:3], each = 5)
  )
  
  result1 <- mvgam:::extract_time_information(data_regular)
  expect_true(result1$has_time)
  expect_equal(result1$n_timepoints, 5)
  expect_equal(result1$time_range, c(1, 5))
  expect_equal(result1$time_spacing, 1)
  
  # Data without time variable
  data_no_time <- data.frame(y = 1:10, x = rnorm(10))
  result2 <- mvgam:::extract_time_information(data_no_time)
  expect_false(result2$has_time)
  
  # Data with irregular time spacing
  data_irregular <- data.frame(
    y = 1:6,
    time = c(1, 3, 7, 10, 12, 20)
  )
  
  result3 <- mvgam:::extract_time_information(data_irregular)
  expect_true(result3$has_time)
  expect_equal(result3$n_timepoints, 6)
  expect_equal(result3$time_spacing, 2) # First interval
})

test_that("extract_series_information handles multivariate responses", {
  # Univariate case
  data_uni <- data.frame(y = 1:10, series = rep(c("A", "B"), each = 5))
  mv_spec_uni <- list(response_names = NULL)
  
  result1 <- mvgam:::extract_series_information(data_uni, mv_spec_uni)
  expect_true(result1$has_series)
  expect_equal(result1$n_series, 2)
  expect_false(result1$is_multivariate)
  
  # Multivariate case
  data_multi <- data.frame(y1 = 1:10, y2 = 11:20, series = rep(c("X", "Y"), each = 5))
  mv_spec_multi <- list(response_names = c("count", "biomass"))
  
  result2 <- mvgam:::extract_series_information(data_multi, mv_spec_multi)
  expect_true(result2$is_multivariate)
  expect_equal(result2$response_names, c("count", "biomass"))
  expect_equal(result2$n_responses, 2)
  
  # No series variable
  data_no_series <- data.frame(y = 1:10, x = rnorm(10))
  result3 <- mvgam:::extract_series_information(data_no_series, mv_spec_uni)
  expect_false(result3$has_series)
})

test_that("subset_stanfit_parameters adds appropriate metadata", {
  mock_fit <- structure(
    list(sim = list(pars_oi = c("b_Intercept", "sigma", "trend"))),
    class = "stanfit"
  )
  
  param_subset <- c("b_Intercept", "sigma")
  
  result <- mvgam:::subset_stanfit_parameters(mock_fit, param_subset)
  
  expect_s3_class(result, "stanfit")
  expect_equal(attr(result, "mvgam_subset"), param_subset)
})