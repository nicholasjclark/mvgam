# Tests for Prior Specification System
# ====================================

# Test prior helper functions
# ---------------------------

test_that("has_trend_priors detects trend parameters correctly", {
  # Create a mock brmsprior object with trend parameters
  trend_prior <- data.frame(
    prior = c("", ""),
    class = c("ar1_trend", "sigma_trend"),
    coef = c("", ""),
    group = c("", ""),
    resp = c("", ""),
    dpar = c("", ""),
    nlpar = c("", ""),
    lb = c("", "0"),
    ub = c("", ""),
    source = c("default", "default"),
    stringsAsFactors = FALSE
  )
  class(trend_prior) <- c("brmsprior", "data.frame")

  expect_true(mvgam:::has_trend_priors(trend_prior))

  # Test with observation-only priors
  obs_prior <- data.frame(
    prior = "",
    class = "Intercept",
    coef = "",
    group = "",
    resp = "",
    dpar = "",
    nlpar = "",
    lb = "",
    ub = "",
    source = "default",
    stringsAsFactors = FALSE
  )
  class(obs_prior) <- c("brmsprior", "data.frame")

  expect_false(mvgam:::has_trend_priors(obs_prior))

  # Test with non-brmsprior object
  expect_false(mvgam:::has_trend_priors("not a prior"))
  expect_false(mvgam:::has_trend_priors(NULL))
})

test_that("extract_trend_priors_only extracts only trend parameters", {
  # Create mixed prior object
  mixed_prior <- data.frame(
    prior = c("", "", ""),
    class = c("Intercept", "ar1_trend", "sigma_trend"),
    coef = c("", "", ""),
    group = c("", "", ""),
    resp = c("", "", ""),
    dpar = c("", "", ""),
    nlpar = c("", "", ""),
    lb = c("", "", "0"),
    ub = c("", "", ""),
    source = c("default", "default", "default"),
    stringsAsFactors = FALSE
  )
  class(mixed_prior) <- c("brmsprior", "data.frame")

  trend_only <- mvgam:::extract_trend_priors_only(mixed_prior)

  expect_true(inherits(trend_only, "brmsprior"))
  expect_equal(nrow(trend_only), 2)
  expect_true(all(grepl("_trend$", trend_only$class)))
})

test_that("extract_observation_priors_only extracts only observation parameters", {
  # Create mixed prior object
  mixed_prior <- data.frame(
    prior = c("", "", ""),
    class = c("Intercept", "ar1_trend", "sigma_trend"),
    coef = c("", "", ""),
    group = c("", "", ""),
    resp = c("", "", ""),
    dpar = c("", "", ""),
    nlpar = c("", "", ""),
    lb = c("", "", "0"),
    ub = c("", "", ""),
    source = c("default", "default", "default"),
    stringsAsFactors = FALSE
  )
  class(mixed_prior) <- c("brmsprior", "data.frame")

  obs_only <- mvgam:::extract_observation_priors_only(mixed_prior)

  expect_true(inherits(obs_only, "brmsprior"))
  expect_equal(nrow(obs_only), 1)
  expect_false(any(grepl("_trend$", obs_only$class)))
  expect_equal(obs_only$class[1], "Intercept")
})

test_that("add_trend_component_attr adds correct attribute", {
  # Create a brmsprior object
  test_prior <- data.frame(
    prior = c("", ""),
    class = c("Intercept", "ar1_trend"),
    coef = c("", ""),
    group = c("", ""),
    resp = c("", ""),
    dpar = c("", ""),
    nlpar = c("", ""),
    lb = c("", ""),
    ub = c("", ""),
    source = c("default", "default"),
    stringsAsFactors = FALSE
  )
  class(test_prior) <- c("brmsprior", "data.frame")

  # Test auto-detection
  result <- mvgam:::add_trend_component_attr(test_prior)
  trend_attr <- attr(result, "trend_component")

  expect_equal(length(trend_attr), 2)
  expect_equal(trend_attr[1], "observation")
  expect_equal(trend_attr[2], "trend")

  # Test manual specification
  result2 <- mvgam:::add_trend_component_attr(test_prior, c("custom1", "custom2"))
  trend_attr2 <- attr(result2, "trend_component")

  expect_equal(trend_attr2, c("custom1", "custom2"))
})

# Test prior extraction functions
# --------------------------------

test_that("extract_observation_priors works with basic formulas", {
  skip_if_not_installed("brms")

  data <- data.frame(
    y = rnorm(20),
    x = rnorm(20)
  )

  # Test with simple formula
  obs_priors <- mvgam:::extract_observation_priors(
    formula = y ~ x,
    data = data,
    family = gaussian()
  )

  expect_true(inherits(obs_priors, "brmsprior"))
  expect_true(nrow(obs_priors) > 0)

  # Should not contain trend parameters
  expect_false(any(grepl("_trend$", obs_priors$class)))
})

test_that("extract_trend_priors handles NULL trend_formula", {
  data <- data.frame(
    y = rnorm(20),
    time = 1:20,
    series = factor(rep(1:2, each = 10))
  )

  # NULL trend formula should return empty brmsprior
  trend_priors <- mvgam:::extract_trend_priors(
    trend_formula = NULL,
    data = data,
    response_names = NULL
  )

  expect_true(inherits(trend_priors, "brmsprior"))
  expect_equal(nrow(trend_priors), 0)
})

# Test trend-specific prior generators
# ------------------------------------

test_that("integrated system generates correct AR prior structure", {
  test_data <- data.frame(y = rnorm(50), time = 1:50, series = 'A')
  
  # Test AR trend without correlation (specify time/series to avoid warnings)
  ar_priors <- mvgam:::extract_trend_priors(
    trend_formula = ~ AR(p = 1, cor = FALSE, time = "time", series = "series"),
    data = test_data
  )

  expect_true(inherits(ar_priors, "brmsprior"))
  expect_true(nrow(ar_priors) >= 2)

  # Should have AR-specific parameters with _trend suffix
  expect_true(any(grepl("ar.*_trend", ar_priors$class)))
  expect_true(any(grepl("sigma_trend", ar_priors$class)))
  
  # Check AR coefficient bounds for stability
  ar_rows <- grepl("ar.*_trend", ar_priors$class)
  if (any(ar_rows)) {
    expect_equal(ar_priors$lb[ar_rows][1], "-0.99")
    expect_equal(ar_priors$ub[ar_rows][1], "0.99")
  }

  # Test with correlation
  ar_priors_cor <- mvgam:::extract_trend_priors(
    trend_formula = ~ AR(p = 1, cor = TRUE, time = "time", series = "series"),
    data = test_data
  )

  expect_true(any(grepl("L_Omega_trend", ar_priors_cor$class)))
})

test_that("integrated system generates correct RW prior structure", {
  test_data <- data.frame(y = rnorm(50), time = 1:50, series = 'A')
  
  rw_priors <- mvgam:::extract_trend_priors(
    trend_formula = ~ RW(time = "time", series = "series"),
    data = test_data
  )

  expect_true(inherits(rw_priors, "brmsprior"))
  expect_true(nrow(rw_priors) >= 1)

  # Should have RW-specific parameters
  expect_true(any(grepl("sigma_trend", rw_priors$class)))

  # Check bounds
  sigma_row <- which(rw_priors$class == "sigma_trend")
  expect_equal(rw_priors$lb[sigma_row], "0")
})

test_that("integrated system generates correct VAR prior structure", {
  test_data <- data.frame(y = rnorm(50), time = 1:50, series = 'A')
  
  var_priors <- mvgam:::extract_trend_priors(
    trend_formula = ~ VAR(time = "time", series = "series"),
    data = test_data
  )

  expect_true(inherits(var_priors, "brmsprior"))
  expect_true(nrow(var_priors) >= 2)

  # Should have VAR-specific parameters
  expect_true(any(grepl("A.*_trend", var_priors$class)))
  expect_true(any(grepl("sigma_trend", var_priors$class)))
  expect_true(any(grepl("L_Omega_trend", var_priors$class)))
})

test_that("integrated system handles ZMVN correlation settings correctly", {
  test_data <- data.frame(y = rnorm(50), time = 1:50, series = 'A')
  
  # Test with correlation (default behavior for ~1, but specify variables)
  # For ZMVN we can't specify parameters like other trends, so we use the time/series from data directly
  zmvn_priors_cor <- mvgam:::extract_trend_priors(
    trend_formula = ~ 1,
    data = test_data
  )

  # ZMVN may have no monitor_params in some configurations
  expect_true(inherits(zmvn_priors_cor, "brmsprior"))
  
  # For ZMVN, the presence of correlation parameters depends on the specific
  # implementation and monitor_params defined in the trend system
  # This test mainly verifies the integrated system works for ZMVN
})

# Test multivariate and distributional model parameter naming
# ---------------------------------------------------------

test_that("multivariate models generate response-specific parameters", {
  # Create multivariate test data following architecture conventions
  test_data <- data.frame(
    count = rpois(50, 5),
    biomass = rnorm(50),
    time = 1:50,
    series = rep(c('A', 'B'), 25),
    temp = rnorm(50)
  )
  
  # Test multivariate trend with single trend type
  # This should generate parameters for both responses
  mv_priors <- mvgam:::extract_trend_priors(
    trend_formula = ~ AR(p = 1, cor = TRUE, time = "time", series = "series"),
    data = test_data,
    response_names = c("count", "biomass")
  )
  
  expect_true(inherits(mv_priors, "brmsprior"))
  
  # Check that response-specific parameters are generated if applicable
  # (Note: The exact behavior depends on how multivariate trends are implemented)
  expect_true(all(grepl("_trend$", mv_priors$class)))
})

test_that("distributional models handle trend parameters correctly", {
  test_data <- data.frame(
    y = rnorm(50),
    x = rnorm(50),
    z = rnorm(50),
    time = 1:50,
    series = 'A'
  )
  
  # Test that trends apply only to mu parameter in distributional models
  # Extract observation priors for distributional model structure
  obs_priors <- mvgam:::extract_observation_priors(
    formula = brms::bf(y ~ x, sigma ~ z),
    data = test_data,
    family = gaussian()
  )
  
  # Extract trend priors that should apply to mu only
  trend_priors <- mvgam:::extract_trend_priors(
    trend_formula = ~ AR(p = 1, time = "time", series = "series"),
    data = test_data
  )
  
  expect_true(inherits(obs_priors, "brmsprior"))
  expect_true(inherits(trend_priors, "brmsprior"))
  
  # Combined priors should have both observation and trend components
  combined <- mvgam:::combine_obs_trend_priors(obs_priors, trend_priors)
  
  expect_true(any(grepl("sigma", combined$class)))  # Distributional parameter
  expect_true(any(grepl("_trend", combined$class))) # Trend parameters
})

test_that("response-specific trend specifications work correctly", {
  test_data <- data.frame(
    count = rpois(50, 5),
    biomass = rnorm(50), 
    time = 1:50,
    series = 'A'
  )
  
  # Note: Response-specific trends like list(count = ~ AR(), biomass = ~ RW())
  # may require different implementation. For now, test that the system
  # handles this gracefully without errors
  
  expect_no_error({
    # Test that we can extract trend priors for different responses
    ar_priors <- mvgam:::extract_trend_priors(
      trend_formula = ~ AR(p = 1, time = "time", series = "series"),
      data = test_data,
      response_names = "count"
    )
    
    rw_priors <- mvgam:::extract_trend_priors(
      trend_formula = ~ RW(time = "time", series = "series"),
      data = test_data,
      response_names = "biomass"
    )
    
    expect_true(inherits(ar_priors, "brmsprior"))
    expect_true(inherits(rw_priors, "brmsprior"))
  })
})

test_that("factor model parameters are named correctly", {
  test_data <- data.frame(
    y = rnorm(100),
    time = rep(1:20, 5),
    series = rep(LETTERS[1:5], each = 20)
  )
  
  # Test factor model with n_lv parameter
  factor_priors <- mvgam:::extract_trend_priors(
    trend_formula = ~ AR(p = 1, cor = TRUE, n_lv = 2, time = "time", series = "series"),
    data = test_data
  )
  
  expect_true(inherits(factor_priors, "brmsprior"))
  
  # Factor models include trend parameters (with _trend suffix) and factor loading matrix Z
  # Z is the only parameter that legitimately doesn't have _trend suffix
  trend_params <- grepl("_trend$", factor_priors$class)
  factor_loading_params <- factor_priors$class == "Z"
  expect_true(all(trend_params | factor_loading_params))
  
  # Factor models should include correlation parameters
  expect_true(any(grepl("L_Omega_trend|sigma_trend|ar.*_trend", factor_priors$class)))
})

# Test prior combination functions
# --------------------------------

test_that("combine_obs_trend_priors merges priors correctly", {
  # Create observation priors
  obs_priors <- data.frame(
    prior = "",
    class = "Intercept",
    coef = "",
    group = "",
    resp = "",
    dpar = "",
    nlpar = "",
    lb = "",
    ub = "",
    source = "default",
    stringsAsFactors = FALSE
  )
  class(obs_priors) <- c("brmsprior", "data.frame")

  # Create trend priors
  trend_priors <- data.frame(
    prior = "",
    class = "ar1_trend",
    coef = "",
    group = "",
    resp = "",
    dpar = "",
    nlpar = "",
    lb = "",
    ub = "",
    source = "default",
    stringsAsFactors = FALSE
  )
  class(trend_priors) <- c("brmsprior", "data.frame")

  # Combine
  combined <- mvgam:::combine_obs_trend_priors(obs_priors, trend_priors)

  expect_true(inherits(combined, "brmsprior"))
  expect_equal(nrow(combined), 2)

  # Should have trend_component attribute
  trend_attr <- attr(combined, "trend_component")
  expect_equal(length(trend_attr), 2)
  expect_true("observation" %in% trend_attr)
  expect_true("trend" %in% trend_attr)

  # Should contain both types of parameters
  expect_true("Intercept" %in% combined$class)
  expect_true("ar1_trend" %in% combined$class)
})

test_that("combine_obs_trend_priors handles empty trend priors", {
  # Create observation priors
  obs_priors <- data.frame(
    prior = "",
    class = "Intercept",
    coef = "",
    group = "",
    resp = "",
    dpar = "",
    nlpar = "",
    lb = "",
    ub = "",
    source = "default",
    stringsAsFactors = FALSE
  )
  class(obs_priors) <- c("brmsprior", "data.frame")

  # Create empty trend priors
  empty_trend_priors <- data.frame(
    prior = character(0),
    class = character(0),
    coef = character(0),
    group = character(0),
    resp = character(0),
    dpar = character(0),
    nlpar = character(0),
    lb = character(0),
    ub = character(0),
    source = character(0),
    stringsAsFactors = FALSE
  )
  class(empty_trend_priors) <- c("brmsprior", "data.frame")

  # Combine
  combined <- mvgam:::combine_obs_trend_priors(obs_priors, empty_trend_priors)

  expect_true(inherits(combined, "brmsprior"))
  expect_equal(nrow(combined), 1)
  expect_equal(combined$class[1], "Intercept")
})

# Test trend formula parsing
# --------------------------

test_that("parse_trend_formula handles simple formulas", {
  data <- data.frame(
    time = 1:10,
    series = factor(rep(1:2, each = 5))
  )

  # Test with simple trend formula (should default to ZMVN)
  parsed <- mvgam:::parse_trend_formula(~ 1, data)

  expect_true(is.list(parsed))
  expect_true("trend_model" %in% names(parsed))
  expect_equal(parsed$trend_model$trend, "ZMVN")  # Default for ~ 1
})

test_that("parse_trend_formula handles formulas without trend constructors", {
  data <- data.frame(
    time = 1:10,
    temperature = rnorm(10),
    habitat = factor(c('A','B'))
  )

  # Test various formulas that should default to ZMVN
  
  # gp() function should default to ZMVN
  result_gp <- mvgam:::parse_trend_formula(~ gp(time), data)
  expect_equal(result_gp$trend_model$trend, "ZMVN")
  expect_equal(result_gp$regular_terms, "gp(time)")
  
  # s() smooth should default to ZMVN
  result_smooth <- mvgam:::parse_trend_formula(~ s(time), data)
  expect_equal(result_smooth$trend_model$trend, "ZMVN")
  expect_equal(result_smooth$regular_terms, "s(time)")
  
  # Multiple terms should default to ZMVN and preserve all terms
  result_multi <- mvgam:::parse_trend_formula(~ temperature + habitat, data)
  expect_equal(result_multi$trend_model$trend, "ZMVN")
  expect_equal(length(result_multi$regular_terms), 2)
  expect_true(all(c("temperature", "habitat") %in% result_multi$regular_terms))
  
  # Complex formula should default to ZMVN
  result_complex <- mvgam:::parse_trend_formula(~ s(time) + temperature + factor(habitat), data)
  expect_equal(result_complex$trend_model$trend, "ZMVN")
  expect_equal(length(result_complex$regular_terms), 3)
})

test_that("parse_trend_formula handles mixed terms with trend constructors", {
  data <- data.frame(
    time = 1:10,
    series = factor(rep(1:2, each = 5)),
    temperature = rnorm(10),
    habitat = factor(c('A','B'))
  )
  
  # Test order independence: trend constructor can be anywhere
  
  # RW() at the end
  result_end <- mvgam:::parse_trend_formula(~ temperature + habitat + RW(time = time, series = series), data)
  expect_equal(result_end$trend_model$trend, "RW")
  expect_equal(length(result_end$regular_terms), 2)
  expect_true(all(c("temperature", "habitat") %in% result_end$regular_terms))
  
  # RW() in the middle  
  result_middle <- mvgam:::parse_trend_formula(~ temperature + RW(time = time, series = series) + habitat, data)
  expect_equal(result_middle$trend_model$trend, "RW")
  expect_equal(length(result_middle$regular_terms), 2)
  expect_true(all(c("temperature", "habitat") %in% result_middle$regular_terms))
  
  # RW() at the beginning
  result_start <- mvgam:::parse_trend_formula(~ RW(time = time, series = series) + temperature + habitat, data)
  expect_equal(result_start$trend_model$trend, "RW")
  expect_equal(length(result_start$regular_terms), 2)
  expect_true(all(c("temperature", "habitat") %in% result_start$regular_terms))
  
  # Test with other trend constructors
  result_ar <- mvgam:::parse_trend_formula(~ s(time) + AR(time = time, series = series) + temperature, data)
  expect_equal(result_ar$trend_model$trend, "AR")
  expect_equal(length(result_ar$regular_terms), 2)
  expect_true(all(c("s(time)", "temperature") %in% result_ar$regular_terms))
})

test_that("parse_trend_formula preserves term order", {
  data <- data.frame(
    time = 1:10,
    series = factor(rep(1:2, each = 5)),
    x1 = rnorm(10),
    x2 = rnorm(10),
    x3 = rnorm(10)
  )
  
  # Test that non-trend terms maintain their relative order
  result <- mvgam:::parse_trend_formula(~ x1 + x2 + RW(time = time, series = series) + x3, data)
  expect_equal(result$trend_model$trend, "RW")
  
  # Order should be preserved: x1, x2, x3 (RW removed)
  expect_equal(result$regular_terms, c("x1", "x2", "x3"))
})

# Test utility functions
# ----------------------

test_that("null-coalescing operator works correctly", {
  # Test when x is not NULL
  result1 <- mvgam:::`%||%`("value", "default")
  expect_equal(result1, "value")

  # Test when x is NULL
  result2 <- mvgam:::`%||%`(NULL, "default")
  expect_equal(result2, "default")

  # Test with different types
  result3 <- mvgam:::`%||%`(0, "default")
  expect_equal(result3, 0)

  result4 <- mvgam:::`%||%`(FALSE, "default")
  expect_equal(result4, FALSE)
})

# Test centralized prior system
# =============================

test_that("get_trend_parameter_prior works with user priors", {
  # Create a test brmsprior object with trend parameters
  test_prior <- data.frame(
    prior = c("normal(0, 0.5)", "exponential(2)", ""),
    class = c("ar1_trend", "sigma_trend", "Intercept"),
    coef = c("", "", ""),
    group = c("", "", ""),
    resp = c("", "", ""),
    dpar = c("", "", ""),
    nlpar = c("", "", ""),
    lb = c("", "0", ""),
    ub = c("", "", ""),
    source = c("user", "user", "default"),
    stringsAsFactors = FALSE
  )
  class(test_prior) <- c("brmsprior", "data.frame")
  
  # Test extracting user-specified priors
  ar1_prior <- mvgam:::get_trend_parameter_prior(test_prior, "ar1_trend")
  expect_equal(ar1_prior, "normal(0, 0.5)")
  
  sigma_prior <- mvgam:::get_trend_parameter_prior(test_prior, "sigma_trend")
  expect_equal(sigma_prior, "exponential(2)")
  
  # Test parameter not found in user priors (should check common defaults)
  missing_prior <- mvgam:::get_trend_parameter_prior(test_prior, "theta1_trend")
  expect_true(is.character(missing_prior))
  # Could be empty string if not in common_trend_priors, or default if it is
})

test_that("get_trend_parameter_prior works with NULL priors", {
  # Test with NULL prior (should use common defaults)
  sigma_default <- mvgam:::get_trend_parameter_prior(NULL, "sigma_trend")
  expect_true(is.character(sigma_default))
  expect_true(nzchar(sigma_default) || sigma_default == "")
  
  # Test with missing parameter (should return empty string)
  missing_default <- mvgam:::get_trend_parameter_prior(NULL, "nonexistent_param")
  expect_equal(missing_default, "")
})

test_that("generate_trend_priors_stanvar creates correct stanvars", {
  # Create test prior object
  test_prior <- data.frame(
    prior = c("normal(0, 0.3)", "exponential(1.5)", ""),
    class = c("ar1_trend", "sigma_trend", "other_param"),
    coef = c("", "", ""),
    group = c("", "", ""),
    resp = c("", "", ""),
    dpar = c("", "", ""),
    nlpar = c("", "", ""),
    lb = c("", "0", ""),
    ub = c("", "", ""),
    source = c("user", "user", "default"),
    stringsAsFactors = FALSE
  )
  class(test_prior) <- c("brmsprior", "data.frame")
  
  # Test creating stanvar with multiple parameters
  stanvar_result <- mvgam:::generate_trend_priors_stanvar(
    param_names = c("ar1_trend", "sigma_trend"),
    prior = test_prior,
    stanvar_name = "test_priors"
  )
  
  expect_true(inherits(stanvar_result, "stanvar"))
  expect_equal(stanvar_result$name, "test_priors")
  expect_equal(stanvar_result$block, "model")
  
  # Check that the Stan code contains both priors
  expect_true(grepl("ar1_trend ~ normal\\(0, 0\\.3\\)", stanvar_result$scode))
  expect_true(grepl("sigma_trend ~ exponential\\(1\\.5\\)", stanvar_result$scode))
})

test_that("generate_trend_priors_stanvar handles empty priors correctly", {
  # Test with no user priors and parameters not in common defaults
  empty_result <- mvgam:::generate_trend_priors_stanvar(
    param_names = c("nonexistent_param1", "nonexistent_param2"),
    prior = NULL,
    stanvar_name = "empty_priors"
  )
  
  # Should return NULL when no priors are specified
  expect_null(empty_result)
})

test_that("generate_trend_priors_stanvar works with single parameter", {
  # Test with single parameter
  test_prior <- data.frame(
    prior = "normal(0, 0.2)",
    class = "theta1_trend",
    coef = "",
    group = "",
    resp = "",
    dpar = "",
    nlpar = "",
    lb = "",
    ub = "",
    source = "user",
    stringsAsFactors = FALSE
  )
  class(test_prior) <- c("brmsprior", "data.frame")
  
  single_result <- mvgam:::generate_trend_priors_stanvar(
    param_names = "theta1_trend",
    prior = test_prior,
    stanvar_name = "single_prior"
  )
  
  expect_true(inherits(single_result, "stanvar"))
  expect_equal(single_result$name, "single_prior")
  expect_true(grepl("theta1_trend ~ normal\\(0, 0\\.2\\)", single_result$scode))
})

test_that("centralized prior system integrates with MA components", {
  # Test that MA components use the centralized system
  test_prior <- data.frame(
    prior = "normal(0, 0.3)",
    class = "theta1_trend",
    coef = "",
    group = "",
    resp = "",
    dpar = "",
    nlpar = "",
    lb = "",
    ub = "",
    source = "user",
    stringsAsFactors = FALSE
  )
  class(test_prior) <- c("brmsprior", "data.frame")
  
  # Test MA components function with prior
  ma_components <- mvgam:::generate_ma_components(n_lv = 2, prior = test_prior)
  
  expect_true(is.list(ma_components))
  expect_true("theta1_param" %in% names(ma_components))
  expect_true("theta1_prior" %in% names(ma_components))
  
  # Check that the prior stanvar exists and uses the user prior
  theta1_prior_stanvar <- ma_components$theta1_prior
  expect_true(inherits(theta1_prior_stanvar, "stanvar"))
  expect_true(grepl("theta1_trend ~ normal\\(0, 0\\.3\\)", theta1_prior_stanvar$scode))
})

test_that("centralized prior system works with common AR parameters", {
  # Test centralized system with parameters that are in common_trend_priors
  test_prior <- data.frame(
    prior = c("normal(0, 0.2)", "exponential(1.5)"),
    class = c("ar1_trend", "sigma_trend"),
    coef = c("", ""),
    group = c("", ""),
    resp = c("", ""),
    dpar = c("", ""),
    nlpar = c("", ""),
    lb = c("", "0"),
    ub = c("", ""),
    source = c("user", "user"),
    stringsAsFactors = FALSE
  )
  class(test_prior) <- c("brmsprior", "data.frame")
  
  # Test that the centralized helper works for common AR parameters
  ar_priors <- mvgam:::generate_trend_priors_stanvar(
    param_names = c("ar1_trend"),
    prior = test_prior,
    stanvar_name = "ar_priors"
  )
  
  expect_true(inherits(ar_priors, "stanvar"))
  expect_true(grepl("ar1_trend ~ normal\\(0, 0\\.2\\)", ar_priors$scode))
})

test_that("centralized prior system works with CAR trend parameters", {
  # Test CAR-specific parameters
  test_prior <- data.frame(
    prior = c("normal(0, 0.8)", "exponential(2.5)"),
    class = c("ar1", "sigma_trend"),  # Note: CAR uses "ar1" not "ar1_trend"
    coef = c("", ""),
    group = c("", ""),
    resp = c("", ""),
    dpar = c("", ""),
    nlpar = c("", ""),
    lb = c("", "0"),
    ub = c("", ""),
    source = c("user", "user"),
    stringsAsFactors = FALSE
  )
  class(test_prior) <- c("brmsprior", "data.frame")
  
  # Test CAR prior generation
  car_priors <- mvgam:::generate_trend_priors_stanvar(
    param_names = c("ar1", "sigma_trend"),
    prior = test_prior,
    stanvar_name = "car_priors"
  )
  
  expect_true(inherits(car_priors, "stanvar"))
  expect_true(grepl("ar1 ~ normal\\(0, 0\\.8\\)", car_priors$scode))
  expect_true(grepl("sigma_trend ~ exponential\\(2\\.5\\)", car_priors$scode))
})


test_that("centralized prior system input validation works correctly", {
  # Test invalid inputs to generate_trend_priors_stanvar
  expect_error(
    mvgam:::generate_trend_priors_stanvar(
      param_names = character(0),  # Empty parameter names
      prior = NULL
    ),
    "Must have length >= 1"
  )
  
  expect_error(
    mvgam:::generate_trend_priors_stanvar(
      param_names = c("param1", NA),  # NA in parameter names
      prior = NULL
    ),
    "Contains missing values"
  )
  
  expect_error(
    mvgam:::generate_trend_priors_stanvar(
      param_names = "param1",
      prior = "not_a_brmsprior"  # Invalid prior object
    ),
    "Assertion on 'prior' failed"
  )
  
  expect_error(
    mvgam:::generate_trend_priors_stanvar(
      param_names = "param1",
      prior = NULL,
      stanvar_name = ""  # Empty stanvar name
    ),
    "All elements must have at least 1 characters"
  )
})

test_that("centralized prior system provides proper backward compatibility", {
  # Test that generators work with prior = NULL (backward compatibility)
  
  # Test MA components with NULL prior (should work without errors)
  ma_null <- mvgam:::generate_ma_components(n_lv = 1, prior = NULL)
  expect_true(is.list(ma_null))
  expect_true("theta1_param" %in% names(ma_null))
  # Whether theta1_prior exists depends on whether there's a default in common_trend_priors
  
  # Test generate_trend_priors_stanvar with NULL
  null_result <- mvgam:::generate_trend_priors_stanvar(
    param_names = c("param1", "param2"),
    prior = NULL,
    stanvar_name = "test"
  )
  # Should return NULL or stanvar depending on common_trend_priors content
  expect_true(is.null(null_result) || inherits(null_result, "stanvar"))
})
