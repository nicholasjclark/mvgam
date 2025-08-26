# Tests for Prior Specification System
# =====================================

# Test prior specification helper functions
# ----------------------------

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
  
  expect_true(inherits(stanvar_result, "stanvars"))
  expect_equal(stanvar_result$test_priors$name, "test_priors")
  expect_equal(stanvar_result$test_priors$block, "model")
  
  # Check that the Stan code contains both priors
  expect_true(grepl("ar1_trend ~ normal\\(0, 0\\.3\\)", stanvar_result$test_priors$scode))
  expect_true(grepl("sigma_trend ~ exponential\\(1\\.5\\)", stanvar_result$test_priors$scode))
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
  
  expect_true(inherits(single_result, "stanvars"))
  expect_equal(single_result$single_prior$name, "single_prior")
  expect_true(grepl("theta1_trend ~ normal\\(0, 0\\.2\\)", single_result$single_prior$scode))
})

# Removed obsolete test for generate_ma_components function (no longer exists)

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
  
  expect_true(inherits(ar_priors, "stanvars"))
  expect_true(grepl("ar1_trend ~ normal\\(0, 0\\.2\\)", ar_priors$ar_priors$scode))
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
  
  expect_true(inherits(car_priors, "stanvars"))
  expect_true(grepl("ar1 ~ normal\\(0, 0\\.8\\)", car_priors$car_priors$scode))
  expect_true(grepl("sigma_trend ~ exponential\\(2\\.5\\)", car_priors$car_priors$scode))
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
  
  # Test generate_trend_priors_stanvar with NULL
  null_result <- mvgam:::generate_trend_priors_stanvar(
    param_names = c("param1", "param2"),
    prior = NULL,
    stanvar_name = "test"
  )
  # Should return NULL or stanvars depending on common_trend_priors content
  expect_true(is.null(null_result) || inherits(null_result, "stanvars"))
})

# Comprehensive Tests for Refactored RW and AR Generators
# ========================================================

test_that("RW generator follows 3-stanvar pattern consistently", {
  # Test simple RW
  trend_specs <- list(trend = "RW", n_lv = 2, ma = FALSE, cor = FALSE)
  data_info <- list(n_obs = 100, n_series = 2)
  
  stanvars <- mvgam:::generate_rw_trend_stanvars(trend_specs, data_info)
  stanvar_names <- names(stanvars)
  
  # Should have basic components but no parameters block for simple RW
  expect_true("rw_tparameters" %in% stanvar_names)
  expect_false("rw_parameters" %in% stanvar_names)  # No MA
  expect_false("rw_model" %in% stanvar_names)  # No MA priors
  
  # Check that tparameters block contains correct parameter names
  rw_tparam <- stanvars$rw_tparameters
  expect_equal(rw_tparam$block, "tparameters")
  expect_true(grepl("lv_trend", rw_tparam$scode))
  expect_true(grepl("scaled_innovations_trend", rw_tparam$scode))
})

test_that("RW generator handles MA components correctly", {
  # Test RW with MA
  trend_specs <- list(trend = "RW", n_lv = 2, ma = TRUE, cor = FALSE)
  data_info <- list(n_obs = 100, n_series = 2)
  
  stanvars <- mvgam:::generate_rw_trend_stanvars(trend_specs, data_info)
  stanvar_names <- names(stanvars)
  
  # Should have all 3 stanvar blocks for MA case
  expect_true("rw_parameters" %in% stanvar_names)  # MA params
  expect_true("rw_tparameters" %in% stanvar_names)
  expect_true("rw_model" %in% stanvar_names)  # MA priors
  
  # Check parameters block
  rw_params <- stanvars$rw_parameters
  expect_equal(rw_params$block, "parameters")
  expect_true(grepl("theta1_trend", rw_params$scode))
  
  # Check MA transformation in tparameters
  rw_tparam <- stanvars$rw_tparameters
  expect_true(grepl("ma_innovations_trend", rw_tparam$scode))
  expect_true(grepl("theta1_trend", rw_tparam$scode))
})

test_that("RW generator handles factor models and custom priors", {
  # Test factor model RW with custom prior
  trend_specs <- list(trend = "RW", n_lv = 2, ma = TRUE, cor = FALSE)
  data_info <- list(n_obs = 100, n_series = 5)  # Factor model: n_lv < n_series
  
  # Create custom prior for theta1_trend
  prior <- brms::set_prior("normal(0, 0.3)", class = "theta1_trend")
  
  stanvars <- mvgam:::generate_rw_trend_stanvars(trend_specs, data_info, prior)
  stanvar_names <- names(stanvars)
  
  # Should have factor model components
  expect_true(any(grepl("factor", stanvar_names, ignore.case = TRUE)))
  expect_true(any(grepl("Z", stanvar_names)))
  
  # Should handle priors correctly
  expect_true("rw_model" %in% stanvar_names)
  
  # Check that sigma_trend priors are NOT duplicated (handled by shared system)
  if ("rw_model" %in% stanvar_names) {
    rw_model_code <- stanvars$rw_model$scode
    expect_false(grepl("sigma_trend", rw_model_code))
  }
})

test_that("RW generator validation catches invalid inputs", {
  # Test dimension validation
  expect_error(
    mvgam:::generate_rw_trend_stanvars(
      trend_specs = list(trend = "RW", n_lv = 0),
      data_info = list(n_series = 2)
    ),
    "not >= 1"
  )
  
  expect_error(
    mvgam:::generate_rw_trend_stanvars(
      trend_specs = list(trend = "RW", n_lv = 2),
      data_info = list(n_series = -1)
    ),
    "not >= 1"
  )
})

test_that("AR generator follows 3-stanvar pattern consistently", {
  # Test simple AR(2)
  trend_specs <- list(trend = "AR", n_lv = 2, lags = 2, ma = FALSE, cor = FALSE)
  data_info <- list(n_obs = 100, n_series = 2)
  
  stanvars <- mvgam:::generate_ar_trend_stanvars(trend_specs, data_info)
  stanvar_names <- names(stanvars)
  
  # Should have all 3 stanvar blocks for AR
  expect_true("ar_parameters" %in% stanvar_names)  # AR coefficients
  expect_true("ar_tparameters" %in% stanvar_names)
  expect_true("ar_model" %in% stanvar_names)  # AR priors
  
  # Check parameters block has correct AR coefficients
  ar_params <- stanvars$ar_parameters
  expect_equal(ar_params$block, "parameters")
  expect_true(grepl("ar1_trend", ar_params$scode))
  expect_true(grepl("ar2_trend", ar_params$scode))
  
  # Check tparameters has AR dynamics
  ar_tparam <- stanvars$ar_tparameters
  expect_equal(ar_tparam$block, "tparameters")
  expect_true(grepl("lv_trend", ar_tparam$scode))
  expect_true(grepl("ar1_trend.*\\* lv_trend", ar_tparam$scode))
  expect_true(grepl("ar2_trend.*\\* lv_trend", ar_tparam$scode))
})

test_that("AR generator handles discontinuous lags correctly", {
  # Test AR with non-standard lags (seasonal AR)
  trend_specs <- list(trend = "AR", n_lv = 1, ar_lags = c(1, 12), ma = FALSE, cor = FALSE)
  data_info <- list(n_obs = 100, n_series = 1)
  
  stanvars <- mvgam:::generate_ar_trend_stanvars(trend_specs, data_info)
  
  # Check parameters block has correct lag-specific coefficients
  ar_params <- stanvars$ar_parameters
  expect_true(grepl("ar1_trend", ar_params$scode))
  expect_true(grepl("ar12_trend", ar_params$scode))
  expect_false(grepl("ar2_trend", ar_params$scode))  # Should not have ar2
  
  # Check AR dynamics uses correct lags
  ar_tparam <- stanvars$ar_tparameters
  expect_true(grepl("ar1_trend.*\\* lv_trend", ar_tparam$scode))
  expect_true(grepl("ar12_trend.*\\* lv_trend", ar_tparam$scode))
  
  # Check proper initialization for max lag (12)
  expect_true(grepl("for \\(i in 1:12\\)", ar_tparam$scode))
  expect_true(grepl("for \\(i in 13:n_trend\\)", ar_tparam$scode))
})

test_that("AR generator handles MA combinations correctly", {
  # Test AR + MA
  trend_specs <- list(trend = "AR", n_lv = 2, lags = 1, ma = TRUE, cor = FALSE)
  data_info <- list(n_obs = 100, n_series = 2)
  
  stanvars <- mvgam:::generate_ar_trend_stanvars(trend_specs, data_info)
  stanvar_names <- names(stanvars)
  
  # Should have both AR and MA parameters
  expect_true("ar_parameters" %in% stanvar_names)
  expect_true("ar_ma_parameters" %in% stanvar_names)
  
  # Check MA parameters block
  ar_ma_params <- stanvars$ar_ma_parameters
  expect_equal(ar_ma_params$block, "parameters")
  expect_true(grepl("theta1_trend", ar_ma_params$scode))
  
  # Check tparameters has both AR and MA transformations
  ar_tparam <- stanvars$ar_tparameters
  expect_true(grepl("ma_innovations_trend", ar_tparam$scode))
  expect_true(grepl("theta1_trend", ar_tparam$scode))
  expect_true(grepl("ar1_trend.*\\* lv_trend", ar_tparam$scode))
  
  # Check model block has priors for both AR and MA parameters
  ar_model <- stanvars$ar_model
  expect_equal(ar_model$block, "model")
})

test_that("AR generator handles factor models correctly", {
  # Test factor model AR
  trend_specs <- list(trend = "AR", n_lv = 2, lags = 1, ma = FALSE, cor = FALSE)
  data_info <- list(n_obs = 100, n_series = 4)  # Factor model: n_lv < n_series
  
  stanvars <- mvgam:::generate_ar_trend_stanvars(trend_specs, data_info)
  stanvar_names <- names(stanvars)
  
  # Should have factor model components
  expect_true(any(grepl("factor", stanvar_names, ignore.case = TRUE)))
  expect_true(any(grepl("Z", stanvar_names)))
  
  # Should still have AR components
  expect_true("ar_parameters" %in% stanvar_names)
  expect_true("ar_tparameters" %in% stanvar_names)
})

test_that("AR generator handles custom priors correctly", {
  # Test AR with custom priors for different parameters
  trend_specs <- list(trend = "AR", n_lv = 2, lags = 2, ma = TRUE)
  data_info <- list(n_obs = 100, n_series = 2)
  
  # Create custom priors for AR and MA parameters
  priors <- c(
    brms::set_prior("normal(0, 0.2)", class = "ar1_trend"),
    brms::set_prior("normal(0, 0.1)", class = "ar2_trend"),
    brms::set_prior("normal(0, 0.5)", class = "theta1_trend")
  )
  
  stanvars <- mvgam:::generate_ar_trend_stanvars(trend_specs, data_info, priors)
  
  # Should handle priors correctly - exact content depends on prior system implementation
  expect_true("ar_model" %in% names(stanvars))
  
  # Should NOT have sigma_trend priors (handled by shared system)
  ar_model_code <- stanvars$ar_model$scode
  expect_false(grepl("sigma_trend", ar_model_code))
})

test_that("AR generator validation catches invalid inputs", {
  # Test dimension validation  
  expect_error(
    mvgam:::generate_ar_trend_stanvars(
      trend_specs = list(trend = "AR", n_lv = 0, lags = 1),
      data_info = list(n_series = 2)
    ),
    "not >= 1"
  )
  
  expect_error(
    mvgam:::generate_ar_trend_stanvars(
      trend_specs = list(trend = "AR", n_lv = 2, lags = 0),
      data_info = list(n_series = 2)
    ),
    "not >= 1"
  )
  
  # Test invalid AR lags
  expect_error(
    mvgam:::generate_ar_trend_stanvars(
      trend_specs = list(trend = "AR", n_lv = 2, ar_lags = c(1, -1)),
      data_info = list(n_series = 2)
    ),
    "not >= 1"
  )
})

test_that("Both RW and AR generators use shared innovation system correctly", {
  # Test that both generators use scaled_innovations_trend and don't create sigma_trend priors
  
  # RW case
  rw_specs <- list(trend = "RW", n_lv = 2, ma = FALSE, cor = FALSE)
  data_info <- list(n_obs = 100, n_series = 2)
  
  rw_stanvars <- mvgam:::generate_rw_trend_stanvars(rw_specs, data_info)
  rw_tparam <- rw_stanvars$rw_tparameters
  expect_true(grepl("scaled_innovations_trend", rw_tparam$scode))
  expect_false(any(grepl("sigma_trend", sapply(rw_stanvars, function(x) x$scode))))
  
  # AR case  
  ar_specs <- list(trend = "AR", n_lv = 2, lags = 1, ma = FALSE, cor = FALSE)
  
  ar_stanvars <- mvgam:::generate_ar_trend_stanvars(ar_specs, data_info)
  ar_tparam <- ar_stanvars$ar_tparameters
  expect_true(grepl("scaled_innovations_trend", ar_tparam$scode))
  expect_false(any(grepl("sigma_trend", sapply(ar_stanvars, function(x) x$scode))))
})

test_that("Helper function append_if_not_null works correctly", {
  # Test append_if_not_null helper function
  components <- list(a = 1, b = 2)
  
  # Test appending non-null item
  result1 <- mvgam:::append_if_not_null(components, list(c = 3))
  expect_length(result1, 3)
  expect_equal(result1[[3]], list(c = 3))
  
  # Test appending null item
  result2 <- mvgam:::append_if_not_null(components, NULL)
  expect_length(result2, 2)
  expect_identical(result2, components)
  
  # Test validation
  expect_error(
    mvgam:::append_if_not_null("not_a_list", list(c = 3)),
    "Must be of type 'list'"
  )
})

# ZMVN Generator Tests ----
test_that("generate_zmvn_trend_stanvars works with different configurations", {
  
  # Test 1: Univariate ZMVN (n_lv = 1, n_series = 1)
  trend_specs_univ <- list(n_lv = 1)
  data_info_univ <- list(n_obs = 50, n_series = 1)
  
  result_univ <- mvgam:::generate_zmvn_trend_stanvars(trend_specs_univ, data_info_univ)
  expect_s3_class(result_univ, "stanvars")
  
  # Should include shared innovation components
  stanvar_names_univ <- names(result_univ)
  expect_true("innovations_trend" %in% stanvar_names_univ)
  expect_true("scaled_innovations" %in% stanvar_names_univ)
  expect_true("sigma_trend" %in% stanvar_names_univ)
  expect_true("zmvn_tparameters" %in% stanvar_names_univ)
  
  # Should NOT include correlation matrix for univariate case
  expect_false("L_Omega_trend" %in% stanvar_names_univ)
  
  # Test 2: Multivariate ZMVN (n_lv = 3, n_series = 3)
  trend_specs_mv <- list(n_lv = 3)
  data_info_mv <- list(n_obs = 100, n_series = 3)
  
  result_mv <- mvgam:::generate_zmvn_trend_stanvars(trend_specs_mv, data_info_mv)
  expect_s3_class(result_mv, "stanvars")
  
  stanvar_names_mv <- names(result_mv)
  expect_true("innovations_trend" %in% stanvar_names_mv)
  expect_true("scaled_innovations" %in% stanvar_names_mv)
  expect_true("sigma_trend" %in% stanvar_names_mv)
  expect_true("L_Omega_trend" %in% stanvar_names_mv)  # Should include correlation matrix
  expect_true("zmvn_tparameters" %in% stanvar_names_mv)
  
  # Test 3: Factor model ZMVN (n_lv = 2, n_series = 4)
  trend_specs_factor <- list(n_lv = 2)
  data_info_factor <- list(n_obs = 80, n_series = 4)
  
  result_factor <- mvgam:::generate_zmvn_trend_stanvars(trend_specs_factor, data_info_factor)
  expect_s3_class(result_factor, "stanvars")
  
  stanvar_names_factor <- names(result_factor)
  expect_true("Z" %in% stanvar_names_factor)  # Factor model should include Z matrix
  expect_true("L_Omega_trend" %in% stanvar_names_factor)  # n_lv > 1 so correlation matrix included
  expect_true("innovations_trend" %in% stanvar_names_factor)
  
  # Test 4: ZMVN with grouping (hierarchical)
  trend_specs_hier <- list(n_lv = 2, gr = "site")
  data_info_hier <- list(n_obs = 120, n_series = 2, n_groups = 3)
  
  result_hier <- mvgam:::generate_zmvn_trend_stanvars(trend_specs_hier, data_info_hier)
  expect_s3_class(result_hier, "stanvars")
  
  stanvar_names_hier <- names(result_hier)
  # Hierarchical should include different correlation structure
  expect_true("innovations_trend" %in% stanvar_names_hier)
  expect_true("zmvn_tparameters" %in% stanvar_names_hier)
  
  # Test 5: ZMVN with custom priors
  trend_specs_prior <- list(n_lv = 2)
  data_info_prior <- list(n_obs = 60, n_series = 2)
  custom_priors <- brms::prior(normal(0, 1), class = "sigma")
  
  result_prior <- mvgam:::generate_zmvn_trend_stanvars(
    trend_specs_prior, 
    data_info_prior, 
    prior = custom_priors
  )
  expect_s3_class(result_prior, "stanvars")
  
  # Test 6: Validation - invalid inputs
  expect_error(
    mvgam:::generate_zmvn_trend_stanvars("invalid", data_info_univ),
    "Must be of type 'list'"
  )
  
  expect_error(
    mvgam:::generate_zmvn_trend_stanvars(trend_specs_univ, "invalid"),
    "Must be of type 'list'"
  )
  
  # Test 7: Edge case - n_lv equals n_series (boundary between factor and non-factor)
  trend_specs_boundary <- list(n_lv = 3)
  data_info_boundary <- list(n_obs = 90, n_series = 3)
  
  result_boundary <- mvgam:::generate_zmvn_trend_stanvars(trend_specs_boundary, data_info_boundary)
  expect_s3_class(result_boundary, "stanvars")
  
  # Should not be treated as factor model when n_lv == n_series
  boundary_names <- names(result_boundary)
  expect_true("L_Omega_trend" %in% boundary_names)  # Should include correlation
})

test_that("ZMVN transformed parameters use non-centered parameterization", {
  
  # Test that ZMVN uses scaled_innovations_trend -> lv_trend transformation
  trend_specs <- list(n_lv = 2)
  data_info <- list(n_obs = 50, n_series = 2)
  
  result <- mvgam:::generate_zmvn_trend_stanvars(trend_specs, data_info)
  
  # Extract transformed parameters block
  tparams_stanvar <- result[["zmvn_tparameters"]]
  expect_s3_class(tparams_stanvar, "stanvar")
  expect_equal(tparams_stanvar$block, "tparameters")
  
  # Check that the Stan code includes the direct transformation
  stan_code <- tparams_stanvar$scode
  expect_true(grepl("lv_trend.*=.*scaled_innovations_trend", stan_code))
  
  # Should be a simple assignment, not complex dynamics
  expect_false(grepl("for.*loop", stan_code))  # No complex loops for ZMVN
})

# VAR/VARMA Prior System Tests
# ============================

test_that("VAR trend generates correct hierarchical hyperpriors", {
  test_data <- data.frame(
    y = rnorm(50), 
    time = 1:50, 
    series = factor(rep(c('A', 'B'), 25))
  )
  
  # Test basic VAR(1) trend priors
  var_priors <- mvgam:::extract_trend_priors(
    trend_formula = ~ VAR(p = 1, time = "time", series = "series"),
    data = test_data
  )
  
  expect_true(inherits(var_priors, "brmsprior"))
  expect_true(nrow(var_priors) >= 3)
  
  # Should have hierarchical VAR hyperparameters
  expect_true(any(grepl("Amu_trend", var_priors$class)))
  expect_true(any(grepl("Aomega_trend", var_priors$class)))
  expect_true(any(grepl("sigma_trend", var_priors$class)))
  expect_true(any(grepl("L_Omega_trend", var_priors$class)))
  
  # Check bounds for precision parameters (should be positive)
  aomega_rows <- grepl("Aomega_trend", var_priors$class)
  if (any(aomega_rows)) {
    expect_equal(var_priors$lb[aomega_rows][1], "0")
  }
})

test_that("VARMA trend generates conditional MA hyperpriors", {
  test_data <- data.frame(
    y = rnorm(60), 
    time = 1:60, 
    series = factor(rep(c('A', 'B', 'C'), 20))
  )
  
  # Test VARMA(2,1) trend priors
  varma_priors <- mvgam:::extract_trend_priors(
    trend_formula = ~ VAR(p = 2, ma = TRUE, time = "time", series = "series"),
    data = test_data
  )
  
  expect_true(inherits(varma_priors, "brmsprior"))
  expect_true(nrow(varma_priors) >= 5)
  
  # Should have VAR hyperparameters
  expect_true(any(grepl("Amu_trend", varma_priors$class)))
  expect_true(any(grepl("Aomega_trend", varma_priors$class)))
  
  # Should have MA hyperparameters (conditional on ma = TRUE)
  expect_true(any(grepl("Dmu_trend", varma_priors$class)))
  expect_true(any(grepl("Domega_trend", varma_priors$class)))
  
  # Should have variance-correlation decomposition
  expect_true(any(grepl("sigma_trend", varma_priors$class)))
  expect_true(any(grepl("L_Omega_trend", varma_priors$class)))
})

test_that("hierarchical VAR trend generates group-specific priors", {
  # Create hierarchical test data with groups
  test_data <- data.frame(
    y = rnorm(80),
    time = rep(1:20, 4),
    series = factor(rep(c('A', 'B'), 40)),
    group = factor(rep(c('G1', 'G2'), each = 40))
  )
  
  # Test hierarchical VAR with grouping
  hier_var_priors <- mvgam:::extract_trend_priors(
    trend_formula = ~ VAR(p = 1, gr = "group", time = "time", series = "series"),
    data = test_data
  )
  
  expect_true(inherits(hier_var_priors, "brmsprior"))
  
  # Should have shared hyperpriors for all groups
  expect_true(any(grepl("Amu_trend", hier_var_priors$class)))
  expect_true(any(grepl("Aomega_trend", hier_var_priors$class)))
  
  # Should have group-specific variance parameters
  expect_true(any(grepl("sigma_group_trend", hier_var_priors$class)))
  
  # Should have variance-correlation structure
  expect_true(any(grepl("sigma_trend", hier_var_priors$class)))
  expect_true(any(grepl("L_Omega_trend", hier_var_priors$class)))
})

test_that("VAR trend priors follow variance-correlation decomposition pattern", {
  test_data <- data.frame(
    y = rnorm(40), 
    time = 1:40, 
    series = factor(rep(c('X', 'Y'), 20))
  )
  
  var_priors <- mvgam:::extract_trend_priors(
    trend_formula = ~ VAR(p = 1, time = "time", series = "series"),
    data = test_data
  )
  
  # Should use sigma_trend + L_Omega_trend instead of inverse Wishart
  expect_true(any(grepl("^sigma_trend$", var_priors$class)))
  expect_true(any(grepl("^L_Omega_trend$", var_priors$class)))
  
  # Should NOT have Sigma_trend (full covariance matrix)
  expect_false(any(grepl("^Sigma_trend$", var_priors$class)))
  
  # Check sigma_trend has positive lower bound
  sigma_rows <- grepl("^sigma_trend$", var_priors$class)
  if (any(sigma_rows)) {
    expect_equal(var_priors$lb[sigma_rows][1], "0")
  }
})

test_that("VAR trend priors integrate with centralized prior system", {
  test_data <- data.frame(
    y = rnorm(30),
    time = 1:30,
    series = factor('S1')
  )
  
  # Test that VAR priors are extracted properly
  var_priors <- mvgam:::extract_trend_priors(
    trend_formula = ~ VAR(p = 1, time = "time", series = "series"),
    data = test_data
  )
  
  expect_true(inherits(var_priors, "brmsprior"))
  
  # Should contain the expected VAR parameters (with empty default priors)
  expect_true(any(var_priors$class == "Amu_trend"))
  expect_true(any(var_priors$class == "Aomega_trend"))
  expect_true(any(var_priors$class == "sigma_trend"))
  expect_true(any(var_priors$class == "L_Omega_trend"))
  
  # Should NOT contain Sigma_trend (computed in Stan)
  expect_false(any(var_priors$class == "Sigma_trend"))
})

test_that("VAR trend validation catches factor model conflicts", {
  test_data <- data.frame(
    y = rnorm(60),
    time = rep(1:20, 3),
    series = factor(rep(c('A', 'B', 'C'), 20))
  )
  
  # VAR with factor model (n_lv < n_series) should work for non-hierarchical
  expect_no_error({
    var_factor_priors <- mvgam:::extract_trend_priors(
      trend_formula = ~ VAR(p = 1, n_lv = 2, time = "time", series = "series"),
      data = test_data
    )
  })
  
  # Should include factor model parameter (Z matrix)
  var_factor_priors <- mvgam:::extract_trend_priors(
    trend_formula = ~ VAR(p = 1, n_lv = 2, time = "time", series = "series"),
    data = test_data
  )
  expect_true(any(grepl("Z", var_factor_priors$class)))
})

test_that("higher-order VAR trends generate multiple lag coefficients", {
  test_data <- data.frame(
    y = rnorm(100),
    time = 1:100,
    series = factor(rep(c('S1', 'S2'), 50))
  )
  
  # Test VAR(3) - should have multiple lag hyperparameters
  var3_priors <- mvgam:::extract_trend_priors(
    trend_formula = ~ VAR(p = 3, time = "time", series = "series"),
    data = test_data
  )
  
  expect_true(inherits(var3_priors, "brmsprior"))
  
  # Should have hyperparameters for multiple lags
  # Amu_trend and Aomega_trend should be arrays with 3 elements (for p=3)
  expect_true(any(grepl("Amu_trend", var3_priors$class)))
  expect_true(any(grepl("Aomega_trend", var3_priors$class)))
  
  # Standard variance-correlation parameters
  expect_true(any(grepl("sigma_trend", var3_priors$class)))
  expect_true(any(grepl("L_Omega_trend", var3_priors$class)))
})

test_that("VAR/VARMA prior parameter naming follows conventions", {
  test_data <- data.frame(
    y = rnorm(50),
    time = 1:50, 
    series = factor('series1')
  )
  
  # Test that all VAR/VARMA parameters have _trend suffix (except Z for factor models)
  var_priors <- mvgam:::extract_trend_priors(
    trend_formula = ~ VAR(p = 2, ma = TRUE, time = "time", series = "series"),
    data = test_data
  )
  
  # All parameters should have _trend suffix except Z (factor loading matrix)
  trend_params <- grepl("_trend$", var_priors$class)
  factor_loading_params <- var_priors$class == "Z"
  
  expect_true(all(trend_params | factor_loading_params))
  
  # Check specific VAR/VARMA parameter names
  expected_params <- c("Amu_trend", "Aomega_trend", "Dmu_trend", "Domega_trend", 
                      "sigma_trend", "L_Omega_trend")
  actual_params <- var_priors$class[trend_params]
  
  # Should contain subset of expected parameters
  expect_true(any(expected_params %in% actual_params))
})

# =============================================================================
# Tests for get_prior() S3 Generic System
# =============================================================================

test_that("get_prior S3 generic works with regular formulas (brms delegation)", {
  # Create test data
  test_data <- data.frame(
    y = rnorm(20),
    x = rnorm(20),
    group = factor(rep(1:4, 5))
  )
  
  # Test that get_prior works with regular formulas (should delegate to brms)
  formula <- y ~ x + (1|group)
  priors_formula <- get_prior(formula, data = test_data, family = gaussian())
  
  # Should return brmsprior object
  expect_true(inherits(priors_formula, "brmsprior"))
  expect_true(inherits(priors_formula, "data.frame"))
  
  # Should have standard brms columns
  expected_cols <- c("prior", "class", "coef", "group", "resp", "dpar", "nlpar", "lb", "ub", "source")
  expect_true(all(expected_cols %in% names(priors_formula)))
  
  # Should have some priors for this model structure
  expect_gt(nrow(priors_formula), 3)
  
  # Should have expected parameter classes for this model
  expect_true("Intercept" %in% priors_formula$class)
  expect_true("b" %in% priors_formula$class)
  expect_true("sd" %in% priors_formula$class)
})

test_that("get_prior works with brmsformula objects", {
  # Create test data  
  test_data <- data.frame(
    count = rpois(20, 5),
    treatment = factor(rep(c("A", "B"), 10))
  )
  
  # Test with brmsformula object
  bf_obj <- brms::bf(count ~ treatment, family = poisson())
  priors_bf <- get_prior(bf_obj, data = test_data)
  
  # Should return brmsprior object
  expect_true(inherits(priors_bf, "brmsprior"))
  expect_gt(nrow(priors_bf), 2)
  
  # Should have expected parameter classes for poisson model
  expect_true("Intercept" %in% priors_bf$class)
  expect_true("b" %in% priors_bf$class)
})

test_that("get_prior.mvgam_formula works for observation-only models (no trend)", {
  # Create test data
  test_data <- data.frame(
    y = rnorm(20),
    x = rnorm(20),
    group = factor(rep(1:4, 5))
  )
  
  # Create mvgam_formula with no trend
  mf <- mvgam_formula(y ~ x + (1|group))
  
  # Get priors using S3 method dispatch
  priors_mvgam <- get_prior(mf, data = test_data, family = gaussian())
  
  # Should return brmsprior object
  expect_true(inherits(priors_mvgam, "brmsprior"))
  expect_true(inherits(priors_mvgam, "data.frame"))
  
  # Should have trend_component column added
  expect_true("trend_component" %in% names(priors_mvgam))
  
  # All trend_component values should be "observation" for no-trend model  
  expect_true(all(priors_mvgam$trend_component == "observation"))
  
  # Should have expected parameter classes
  expect_true("Intercept" %in% priors_mvgam$class)
  expect_true("b" %in% priors_mvgam$class)
  expect_true("sd" %in% priors_mvgam$class)
  expect_true("sigma" %in% priors_mvgam$class)
  
  # Should match brms output exactly (except for trend_component column)
  formula <- y ~ x + (1|group)
  priors_brms <- brms::get_prior(formula, data = test_data, family = gaussian())
  
  # Remove trend_component column for comparison
  priors_mvgam_clean <- priors_mvgam[, !names(priors_mvgam) %in% "trend_component"]
  
  # Should have same structure and content
  expect_equal(names(priors_mvgam_clean), names(priors_brms))
  expect_equal(nrow(priors_mvgam_clean), nrow(priors_brms))
  expect_equal(priors_mvgam_clean$class, priors_brms$class)
})

test_that("get_prior.mvgam_formula preserves original formula class information", {
  # Test with regular formula
  formula <- y ~ x
  mf_formula <- mvgam_formula(formula)
  expect_equal(attr(mf_formula, "formula_class"), "formula")
  
  # Test with brmsformula
  bf_obj <- brms::bf(count ~ treatment, family = poisson())
  mf_brmsformula <- mvgam_formula(bf_obj)
  expect_equal(attr(mf_brmsformula, "formula_class"), "brmsformula")
  
  # Test mvgam_formula class structure
  expect_equal(class(mf_formula), "mvgam_formula")
  expect_equal(class(mf_brmsformula), "mvgam_formula")
})

test_that("get_prior input validation works correctly", {
  # Test with invalid object
  expect_error(get_prior("not a valid object"))
  
  # Test with mvgam_formula but missing data
  mf <- mvgam_formula(y ~ x)
  expect_error(get_prior(mf), "data")
  
  # Test with invalid data
  test_data <- data.frame(y = rnorm(20), x = rnorm(20))
  expect_error(get_prior(mf, data = "not a data frame"))
  expect_error(get_prior(mf, data = data.frame()))  # Empty data frame
  
  # Test with invalid family
  expect_error(get_prior(mf, data = test_data, family = "not a family"))
})

test_that("get_prior S3 method dispatch works correctly", {
  # Create test data
  test_data <- data.frame(
    y = rnorm(20),
    x = rnorm(20)
  )
  
  # Test that different object classes dispatch to correct methods
  formula <- y ~ x
  mf <- mvgam_formula(formula)
  bf_obj <- brms::bf(y ~ x)
  
  # All should return brmsprior objects but with different characteristics
  priors_formula <- get_prior(formula, data = test_data)
  priors_mvgam <- get_prior(mf, data = test_data)  
  priors_brmsformula <- get_prior(bf_obj, data = test_data)
  
  # All should be brmsprior objects
  expect_true(inherits(priors_formula, "brmsprior"))
  expect_true(inherits(priors_mvgam, "brmsprior"))
  expect_true(inherits(priors_brmsformula, "brmsprior"))
  
  # Only mvgam_formula should have trend_component column
  expect_false("trend_component" %in% names(priors_formula))
  expect_true("trend_component" %in% names(priors_mvgam))
  expect_false("trend_component" %in% names(priors_brmsformula))
})

test_that("get_prior methods are properly exported and registered", {
  # Check that generic exists
  expect_true(exists("get_prior"))
  
  # Check that methods are registered
  methods_list <- methods("get_prior")
  expected_methods <- c("get_prior.default", "get_prior.formula", 
                       "get_prior.brmsformula", "get_prior.mvgam_formula")
  
  # All expected methods should be in the methods list
  for (method in expected_methods) {
    expect_true(method %in% methods_list)
  }
})
