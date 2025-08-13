test_that("mvgam_enhanced validates inputs correctly", {
  # Valid single dataset
  formula <- y ~ s(x)
  data <- data.frame(y = 1:10, x = rnorm(10))
  
  with_mocked_bindings(
    mvgam_single_dataset = function(...) structure(list(), class = "mvgam"),
    {
      result <- mvgam:::mvgam_enhanced(formula, data = data)
      expect_s3_class(result, "mvgam")
    }
  )
  
  # Invalid formula
  expect_error(
    mvgam:::mvgam_enhanced("not a formula", data = data),
    "Assertion on 'formula' failed"
  )
  
  # Invalid data (not data.frame or list)
  expect_error(
    mvgam:::mvgam_enhanced(formula, data = "not valid"),
    "Assertion on 'data' failed"
  )
  
  # Invalid backend
  expect_error(
    mvgam:::mvgam_enhanced(formula, data = data, backend = 123),
    "Assertion on 'backend' failed"
  )
  
  # Invalid combine parameter
  expect_error(
    mvgam:::mvgam_enhanced(formula, data = data, combine = "not logical"),
    "Assertion on 'combine' failed"
  )
})

test_that("mvgam_enhanced detects and handles multiple imputation", {
  formula <- y ~ s(x)
  
  # Multiple imputation data (list of data.frames)
  mi_data <- list(
    data.frame(y = c(1, 2, NA, 4, 5), x = rnorm(5)),
    data.frame(y = c(1, 2, 3, 4, 5), x = rnorm(5)),
    data.frame(y = c(1, 2, 2.5, 4, 5), x = rnorm(5))
  )
  
  # Mock multiple imputation functions
  mi_call_count <- 0
  
  with_mocked_bindings(
    mvgam_multiple = function(formula, trend_formula, data, backend, combine, ...) {
      mi_call_count <<- mi_call_count + 1
      expect_equal(length(data), 3)
      expect_equal(combine, TRUE)  # Default combine = TRUE
      structure(list(pooled = TRUE), class = c("mvgam_pooled", "mvgam"))
    },
    {
      # Should route to mvgam_multiple with combine = TRUE
      result1 <- mvgam:::mvgam_enhanced(formula, data = mi_data)
      expect_equal(mi_call_count, 1)
      expect_s3_class(result1, "mvgam_pooled")
      
      # Should route to mvgam_multiple with combine = FALSE
      result2 <- mvgam:::mvgam_enhanced(formula, data = mi_data, combine = FALSE)
      expect_equal(mi_call_count, 2)
    }
  )
})

test_that("mvgam_enhanced processes single datasets correctly", {
  formula <- y ~ s(x) + temp
  trend_formula <- ~ AR(p = 1)
  data <- data.frame(y = 1:10, x = rnorm(10), temp = rnorm(10))
  backend <- "cmdstanr"
  family <- poisson()
  
  single_dataset_args <- NULL
  
  with_mocked_bindings(
    mvgam_single_dataset = function(...) {
      single_dataset_args <<- list(...)
      structure(list(
        formula = single_dataset_args$formula,
        trend_formula = single_dataset_args$trend_formula,
        backend = single_dataset_args$backend
      ), class = "mvgam")
    },
    {
      result <- mvgam:::mvgam_enhanced(
        formula = formula,
        trend_formula = trend_formula,
        data = data,
        backend = backend,
        family = family
      )
      
      # Check that correct arguments were passed to single dataset function
      expect_equal(single_dataset_args$formula, formula)
      expect_equal(single_dataset_args$trend_formula, trend_formula)
      expect_equal(single_dataset_args$data, data)
      expect_equal(single_dataset_args$backend, backend)
      expect_equal(single_dataset_args$family, family)
      
      # Check result structure
      expect_s3_class(result, "mvgam")
      expect_equal(result$formula, formula)
      expect_equal(result$trend_formula, trend_formula)
    }
  )
})

test_that("mvgam_single_dataset integrates all architecture components", {
  formula <- y ~ s(x) + temp
  trend_formula <- ~ AR(p = 1)
  data <- data.frame(y = 1:10, x = rnorm(10), temp = rnorm(10))
  
  # Mock all components
  mock_mv_spec <- list(
    has_trends = TRUE,
    is_multivariate = FALSE,
    base_formula = trend_formula,
    trend_specs = list(main = trend_formula)
  )
  
  mock_obs_setup <- list(
    formula = formula,
    data = data,
    family = gaussian(),
    stancode = "obs code",
    standata = list(N = 10)
  )
  
  mock_trend_setup <- list(
    formula = trend_formula,
    data = data,
    family = gaussian(),
    stancode = "trend code",
    standata = list(N = 10)
  )
  
  mock_combined <- list(
    stancode = "combined code",
    standata = list(N = 10, has_trends = 1)
  )
  
  mock_fit <- structure(
    list(sim = list(pars_oi = c("b_Intercept", "trend"), algorithm = "sampling")),
    class = "stanfit"
  )
  
  mock_mvgam <- structure(
    list(
      formula = formula,
      trend_formula = trend_formula,
      obs_fit = structure(list(), class = "brmsfit"),
      trend_fit = structure(list(), class = "brmsfit")
    ),
    class = c("mvgam", "brmsfit")
  )
  
  # Track function calls
  function_calls <- list()
  
  with_mocked_bindings(
    parse_multivariate_trends = function(f, tf) {
      function_calls$parse <<- list(formula = f, trend_formula = tf)
      mock_mv_spec
    },
    validate_autocor_separation = function(f, tf) {
      function_calls$validate <<- list(formula = f, trend_formula = tf)
      TRUE
    },
    setup_brms_lightweight = function(formula, data, family, ...) {
      function_calls$setup <<- c(function_calls$setup, list(list(formula = formula, family = family)))
      if (identical(formula, trend_formula)) mock_trend_setup else mock_obs_setup
    },
    extract_trend_stanvars_from_setup = function(ts, mvs) {
      function_calls$stanvars <<- list(trend_setup = ts, mv_spec = mvs)
      list()
    },
    generate_combined_stancode_and_data = function(obs_setup, trend_setup, mv_spec, trend_stanvars) {
      function_calls$combine <<- list(has_obs = !is.null(obs_setup), has_trend = !is.null(trend_setup))
      mock_combined
    },
    fit_mvgam_model = function(stancode, standata, backend, ...) {
      function_calls$fit <<- list(backend = backend, has_trends = standata$has_trends)
      mock_fit
    },
    create_mvgam_from_combined_fit = function(combined_fit, obs_setup, trend_setup, mv_spec) {
      function_calls$create <<- list(has_combined = !is.null(combined_fit))
      mock_mvgam
    },
    {
      result <- mvgam:::mvgam_single_dataset(
        formula = formula,
        trend_formula = trend_formula,
        data = data,
        backend = "cmdstanr",
        family = gaussian()
      )
      
      # Verify integration flow
      expect_equal(function_calls$parse$formula, formula)
      expect_equal(function_calls$parse$trend_formula, trend_formula)
      expect_true(function_calls$validate$formula == formula)
      expect_length(function_calls$setup, 2)  # obs and trend setup
      expect_true(function_calls$combine$has_obs)
      expect_true(function_calls$combine$has_trend)
      expect_equal(function_calls$fit$backend, "cmdstanr")
      expect_true(function_calls$create$has_combined)
      
      # Check final result
      expect_s3_class(result, c("mvgam", "brmsfit"))
      expect_equal(result$formula, formula)
      expect_equal(result$trend_formula, trend_formula)
    }
  )
})

test_that("mvgam_single_dataset handles cases without trends", {
  formula <- y ~ s(x)
  data <- data.frame(y = 1:10, x = rnorm(10))
  
  # Mock specification without trends
  mock_mv_spec <- list(
    has_trends = FALSE,
    is_multivariate = FALSE,
    trend_specs = NULL
  )
  
  mock_obs_setup <- list(
    formula = formula,
    data = data,
    family = poisson(),
    stancode = "obs only code",
    standata = list(N = 10)
  )
  
  setup_call_count <- 0
  
  with_mocked_bindings(
    parse_multivariate_trends = function(f, tf) mock_mv_spec,
    setup_brms_lightweight = function(...) {
      setup_call_count <<- setup_call_count + 1
      mock_obs_setup
    },
    generate_combined_stancode_and_data = function(obs_setup, trend_setup, mv_spec, trend_stanvars) {
      expect_null(trend_setup)  # Should be NULL when no trends
      expect_null(trend_stanvars)
      list(stancode = "obs only", standata = list(N = 10, has_trends = 0))
    },
    fit_mvgam_model = function(...) structure(list(), class = "stanfit"),
    create_mvgam_from_combined_fit = function(combined_fit, obs_setup, trend_setup, mv_spec) {
      expect_null(trend_setup)  # Should be NULL
      structure(list(), class = "mvgam")
    },
    {
      result <- mvgam:::mvgam_single_dataset(formula, NULL, data, "cmdstanr", poisson())
      
      # Should only call setup once (for observation model)
      expect_equal(setup_call_count, 1)
      expect_s3_class(result, "mvgam")
    }
  )
})

test_that("extract_trend_stanvars_from_setup combines stanvars correctly", {
  trend_setup <- list(
    stanvars = list(
      base1 = "base stanvar 1",
      base2 = "base stanvar 2"
    )
  )
  
  mv_spec <- list(
    has_trends = TRUE,
    trend_specs = list(
      series1 = structure(list(trend_type = "AR"), class = "mvgam_trend")
    )
  )
  
  with_mocked_bindings(
    generate_trend_stanvars = function(mvs) {
      list(
        trend1 = "trend stanvar 1",
        trend2 = "trend stanvar 2"
      )
    },
    {
      result <- mvgam:::extract_trend_stanvars_from_setup(trend_setup, mv_spec)
      
      expect_named(result, c("base1", "base2", "trend1", "trend2"))
      expect_equal(result$base1, "base stanvar 1")
      expect_equal(result$trend1, "trend stanvar 1")
    }
  )
  
  # Handle missing base stanvars
  trend_setup_no_base <- list(stanvars = NULL)
  
  with_mocked_bindings(
    generate_trend_stanvars = function(mvs) list(trend1 = "only trend"),
    {
      result <- mvgam:::extract_trend_stanvars_from_setup(trend_setup_no_base, mv_spec)
      expect_named(result, "trend1")
    }
  )
})

test_that("generate_trend_stanvars processes multiple trend types", {
  mv_spec <- list(
    has_trends = TRUE,
    trend_specs = list(
      count = structure(list(trend_type = "AR", pars = list(p = 1)), class = "mvgam_trend"),
      biomass = structure(list(trend_type = "RW", pars = list(cor = TRUE)), class = "mvgam_trend"),
      presence = structure(list(trend_type = "VAR", pars = list(lags = 1)), class = "mvgam_trend")
    )
  )
  
  # Mock trend type generators
  ar_stanvars <- list(ar_code = "AR implementation")
  rw_stanvars <- list(rw_code = "RW implementation")  
  var_stanvars <- list(var_code = "VAR implementation")
  
  with_mocked_bindings(
    generate_trend_type_stanvars = function(trend_specs, resp_name) {
      if (trend_specs$trend_type == "AR") ar_stanvars
      else if (trend_specs$trend_type == "RW") rw_stanvars
      else if (trend_specs$trend_type == "VAR") var_stanvars
      else list()
    },
    {
      result <- mvgam:::generate_trend_stanvars(mv_spec)
      
      # Should combine stanvars from all trend types
      expect_named(result, c("ar_code", "rw_code", "var_code"))
      expect_equal(result$ar_code, "AR implementation")
      expect_equal(result$rw_code, "RW implementation")
      expect_equal(result$var_code, "VAR implementation")
    }
  )
  
  # Handle specification without trends
  no_trends_spec <- list(has_trends = FALSE, trend_specs = NULL)
  result_empty <- mvgam:::generate_trend_stanvars(no_trends_spec)
  expect_equal(result_empty, list())
})

test_that("generate_trend_type_stanvars dispatches to correct generators", {
  # Test AR trend
  ar_spec <- structure(
    list(trend_type = "AR", pars = list(p = 2)),
    class = "mvgam_trend"
  )
  
  with_mocked_bindings(
    generate_ar_stanvars = function(pars, resp_name) {
      expect_equal(pars$p, 2)
      expect_equal(resp_name, "abundance")
      list(ar_specific = "AR code")
    },
    {
      result <- mvgam:::generate_trend_type_stanvars(ar_spec, "abundance")
      expect_equal(result$ar_specific, "AR code")
    }
  )
  
  # Test RW trend
  rw_spec <- structure(
    list(trend_type = "RW", pars = list(cor = TRUE)),
    class = "mvgam_trend"
  )
  
  with_mocked_bindings(
    generate_rw_stanvars = function(pars, resp_name) {
      expect_true(pars$cor)
      list(rw_specific = "RW code")
    },
    {
      result <- mvgam:::generate_trend_type_stanvars(rw_spec, "biomass")
      expect_equal(result$rw_specific, "RW code")
    }
  )
  
  # Test VAR trend
  var_spec <- structure(
    list(trend_type = "VAR", pars = list(p = 3, cor = FALSE)),
    class = "mvgam_trend"
  )
  
  with_mocked_bindings(
    generate_var_stanvars = function(pars, resp_name) {
      expect_equal(pars$p, 3)
      list(var_specific = "VAR code")
    },
    {
      result <- mvgam:::generate_trend_type_stanvars(var_spec, "counts")
      expect_equal(result$var_specific, "VAR code")
    }
  )
})

test_that("generate_combined_stancode_and_data handles trend injection", {
  obs_setup <- list(
    stancode = "observation model code",
    standata = list(N = 10, y = 1:10)
  )
  
  trend_setup <- list(
    stancode = "trend model code", 
    standata = list(N = 10, T = 5)
  )
  
  mv_spec <- list(
    has_trends = TRUE,
    trend_specs = list(main = ~ AR(p = 1))
  )
  
  trend_stanvars <- list(ar_implementation = "AR code")
  
  with_mocked_bindings(
    inject_trend_into_linear_predictor = function(base_stancode, trend_stanvars) {
      expect_equal(base_stancode, obs_setup$stancode)
      expect_equal(trend_stanvars$ar_implementation, "AR code")
      "modified stan code with trends"
    },
    combine_standata = function(obs_standata, trend_standata, mv_spec) {
      expect_equal(obs_standata$N, 10)
      expect_equal(trend_standata$T, 5)
      list(N = 10, y = 1:10, T = 5, has_trends = 1, n_trends = 1)
    },
    {
      result <- mvgam:::generate_combined_stancode_and_data(
        obs_setup, trend_setup, mv_spec, trend_stanvars
      )
      
      expect_equal(result$stancode, "modified stan code with trends")
      expect_equal(result$standata$has_trends, 1)
      expect_equal(result$standata$n_trends, 1)
    }
  )
})

test_that("generate_combined_stancode_and_data handles no trends case", {
  obs_setup <- list(
    stancode = "observation only code",
    standata = list(N = 10, y = 1:10)
  )
  
  mv_spec <- list(has_trends = FALSE)
  
  result <- mvgam:::generate_combined_stancode_and_data(
    obs_setup, NULL, mv_spec, NULL
  )
  
  # Should return observation setup unchanged
  expect_equal(result$stancode, "observation only code")
  expect_equal(result$standata, obs_setup$standata)
})

test_that("combine_standata merges data correctly and avoids conflicts", {
  obs_standata <- list(
    N = 10,
    y = 1:10,
    X = matrix(rnorm(20), nrow = 10),
    shared_param = "obs_value"
  )
  
  trend_standata <- list(
    N = 10,  # Same as obs (should be ignored to avoid conflict)
    T = 5,
    trend_matrix = matrix(rnorm(50), nrow = 10),
    shared_param = "trend_value"  # Conflict (should be ignored)
  )
  
  mv_spec <- list(trend_specs = list(series1 = ~ AR(p = 1), series2 = ~ RW()))
  
  result <- mvgam:::combine_standata(obs_standata, trend_standata, mv_spec)
  
  # Should keep observation data
  expect_equal(result$N, 10)
  expect_equal(result$y, 1:10)
  expect_equal(result$shared_param, "obs_value")  # Obs takes precedence
  
  # Should add non-conflicting trend data
  expect_equal(result$T, 5)
  expect_equal(dim(result$trend_matrix), c(10, 5))
  
  # Should add trend metadata
  expect_equal(result$has_trends, 1L)
  expect_equal(result$n_trends, 2L)
})

test_that("fit_mvgam_model creates appropriate mock structure", {
  stancode <- "mock stan code"
  standata <- list(N = 100, y = 1:100)
  backend <- "cmdstanr"
  
  result <- mvgam:::fit_mvgam_model(stancode, standata, backend, 
                                   chains = 4, iter = 1000)
  
  expect_s3_class(result, "stanfit")
  expect_equal(result$stancode, stancode)
  expect_equal(result$standata, standata)
  expect_equal(result$backend, backend)
  expect_s3_class(result$fit_time, "POSIXct")
})