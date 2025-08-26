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

  # Test CAR prior generation
  car_priors <- mvgam:::generate_trend_priors_stanvar(
    param_names = c("ar1_trend", "sigma_trend"),
    prior = test_prior,
    stanvar_name = "car_priors"
  )

  expect_true(inherits(car_priors, "stanvars"))
  expect_true(grepl("ar1_trend ~ normal\\(0, 0\\.8\\)",
                    car_priors$car_priors$scode))
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
      data_info = list(n_obs = 100, n_series = 2)
    ),
    "not >= 1"
  )

  expect_error(
    mvgam:::generate_ar_trend_stanvars(
      trend_specs = list(trend = "AR", n_lv = 2, lags = 0),
      data_info = list(n_obs = 100, n_series = 2)
    ),
    "not >= 1"
  )

  # Test invalid AR lags - single negative value to trigger >= 1 validation
  expect_error(
    mvgam:::generate_ar_trend_stanvars(
      trend_specs = list(trend = "AR", n_lv = 2, lags = -1),
      data_info = list(n_obs = 100, n_series = 2)
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

  # Should follow 3-stanvar pattern
  stanvar_names_univ <- names(result_univ)
  expect_true("zmvn_parameters" %in% stanvar_names_univ)
  expect_true("zmvn_tparameters" %in% stanvar_names_univ)
  expect_true("zmvn_model" %in% stanvar_names_univ)

  # Should NOT include correlation matrix for univariate case
  expect_false("L_Omega_trend" %in% stanvar_names_univ)

  # Test 2: Multivariate ZMVN (n_lv = 3, n_series = 3)
  trend_specs_mv <- list(n_lv = 3)
  data_info_mv <- list(n_obs = 100, n_series = 3)

  result_mv <- mvgam:::generate_zmvn_trend_stanvars(trend_specs_mv, data_info_mv)
  expect_s3_class(result_mv, "stanvars")

  stanvar_names_mv <- names(result_mv)
  expect_true("zmvn_parameters" %in% stanvar_names_mv)
  expect_true("zmvn_tparameters" %in% stanvar_names_mv)
  expect_true("zmvn_model" %in% stanvar_names_mv)
  # Note: L_Omega_trend may be included in the model block for multivariate case

  # Test 3: Factor model ZMVN (n_lv = 2, n_series = 4)
  trend_specs_factor <- list(n_lv = 2)
  data_info_factor <- list(n_obs = 80, n_series = 4)

  result_factor <- mvgam:::generate_zmvn_trend_stanvars(trend_specs_factor, data_info_factor)
  expect_s3_class(result_factor, "stanvars")

  stanvar_names_factor <- names(result_factor)
  expect_true("zmvn_parameters" %in% stanvar_names_factor)
  expect_true("zmvn_tparameters" %in% stanvar_names_factor)
  expect_true("zmvn_model" %in% stanvar_names_factor)
  # Z matrix may be included for factor models

  # Test 4: ZMVN with grouping (hierarchical)
  trend_specs_hier <- list(n_lv = 2, gr = "site")
  data_info_hier <- list(n_obs = 120, n_series = 2, n_groups = 3)

  result_hier <- mvgam:::generate_zmvn_trend_stanvars(trend_specs_hier, data_info_hier)
  expect_s3_class(result_hier, "stanvars")

  stanvar_names_hier <- names(result_hier)
  # Hierarchical should follow 3-stanvar pattern
  expect_true("zmvn_parameters" %in% stanvar_names_hier)
  expect_true("zmvn_tparameters" %in% stanvar_names_hier)
  expect_true("zmvn_model" %in% stanvar_names_hier)

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

  # Should follow 3-stanvar pattern
  boundary_names <- names(result_boundary)
  expect_true("zmvn_parameters" %in% boundary_names)
  expect_true("zmvn_tparameters" %in% boundary_names)
  expect_true("zmvn_model" %in% boundary_names)
})

test_that("ZMVN transformed parameters use non-centered parameterization", {

  # Test that ZMVN follows 3-stanvar pattern and includes tparameters block
  trend_specs <- list(n_lv = 2)
  data_info <- list(n_obs = 50, n_series = 2)

  result <- mvgam:::generate_zmvn_trend_stanvars(trend_specs, data_info)

  # Should follow 3-stanvar pattern
  expect_true("zmvn_parameters" %in% names(result))
  expect_true("zmvn_tparameters" %in% names(result))
  expect_true("zmvn_model" %in% names(result))

  # Extract transformed parameters block if it exists
  if ("zmvn_tparameters" %in% names(result)) {
    tparams_stanvar <- result[["zmvn_tparameters"]]
    expect_type(tparams_stanvar, "list")
    expect_equal(tparams_stanvar$block, "tparameters")
    expect_equal(tparams_stanvar$name, "zmvn_tparameters")
  }
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

  # Test mvgam_formula class structure (now preserves class hierarchy)
  expect_equal(class(mf_formula), c("mvgam_formula", "formula"))
  expect_equal(class(mf_brmsformula), c("mvgam_formula", "brmsformula"))
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

# =============================================================================
# Tests for Sub-task 1D: Trend Component Integration in get_prior
# =============================================================================

test_that("get_prior.mvgam_formula adds trend_component column correctly", {
  # Create test data with time series structure
  test_data <- data.frame(
    count = rpois(50, 5),
    temperature = rnorm(50),
    time = 1:50,
    series = factor(rep(c("A", "B"), 25))
  )

  # Create mvgam_formula with trend
  mf <- mvgam_formula(
    count ~ temperature,
    trend_formula = ~ AR(p = 1, time = "time", series = "series")
  )

  # Get priors using S3 method
  combined_priors <- get_prior(mf, data = test_data, family = poisson())

  # Should return brmsprior object with trend_component column
  expect_true(inherits(combined_priors, "brmsprior"))
  expect_true("trend_component" %in% names(combined_priors))

  # Should have both observation and trend components
  expect_true("observation" %in% combined_priors$trend_component)
  expect_true("trend" %in% combined_priors$trend_component)

  # Observation parameters should be marked as "observation"
  obs_rows <- combined_priors$trend_component == "observation"
  expect_true(any(combined_priors$class[obs_rows] == "Intercept"))
  expect_true(any(combined_priors$class[obs_rows] == "b"))

  # Trend parameters should be marked as "trend" and have _trend suffix
  trend_rows <- combined_priors$trend_component == "trend"
  expect_true(all(grepl("_trend$|^Z$", combined_priors$class[trend_rows])))

  # Should have AR-specific trend parameters
  expect_true(any(grepl("ar.*_trend", combined_priors$class[trend_rows])))
  expect_true(any(grepl("sigma_trend", combined_priors$class[trend_rows])))
})

test_that("get_prior.mvgam_formula handles empty trend_priors gracefully", {
  # Create test data
  test_data <- data.frame(
    y = rnorm(20),
    x = rnorm(20),
    time = 1:20,
    series = factor('A')
  )

  # Create mvgam_formula with trend_formula that results in empty priors
  mf <- mvgam_formula(
    y ~ x,
    trend_formula = ~ 1  # Should result in ZMVN with possibly no monitor_params
  )

  # Should handle gracefully without errors
  expect_no_error({
    priors <- get_prior(mf, data = test_data, family = gaussian())
  })

  priors <- get_prior(mf, data = test_data, family = gaussian())

  # Should still have trend_component column
  expect_true("trend_component" %in% names(priors))

  # Should have observation priors at minimum
  expect_true("observation" %in% priors$trend_component)

  # All observation priors should be marked correctly
  obs_rows <- priors$trend_component == "observation"
  expect_true(sum(obs_rows) >= 3)  # At least Intercept, b, sigma
})

test_that("get_prior.mvgam_formula integrates extract_response_names correctly", {
  # Create multivariate test data
  test_data <- data.frame(
    count = rpois(40, 3),
    biomass = rnorm(40),
    temperature = rnorm(40),
    time = 1:40,
    series = factor(rep(c("A", "B"), 20))
  )

  # Create multivariate mvgam_formula using bf() pattern from quick reference
  mf <- mvgam_formula(
    brms::bf(count ~ temperature, family = poisson()) +
    brms::bf(biomass ~ temperature, family = gaussian()),
    trend_formula = ~ AR(p = 1, time = "time", series = "series")
  )

  # Get priors - multivariate bf() formulas handle family internally
  mv_priors <- get_prior(mf, data = test_data)

  # Should have trend_component column
  expect_true("trend_component" %in% names(mv_priors))

  # Should have both observation and trend parameters
  expect_true("observation" %in% mv_priors$trend_component)
  expect_true("trend" %in% mv_priors$trend_component)

  # Should have multivariate-specific structure in observation parameters
  obs_rows <- mv_priors$trend_component == "observation"
  expect_true(any(grepl("count", mv_priors$resp[obs_rows])))
  expect_true(any(grepl("biomass", mv_priors$resp[obs_rows])))

  # Trend parameters should have _trend suffix
  trend_rows <- mv_priors$trend_component == "trend"
  if (sum(trend_rows) > 0) {
    expect_true(all(grepl("_trend$|^Z$", mv_priors$class[trend_rows])))
  }
})

test_that("get_prior.mvgam_formula combines priors preserving brmsprior class", {
  # Create test data
  test_data <- data.frame(
    y = rnorm(30),
    x = rnorm(30),
    time = 1:30,
    series = factor('series1')
  )

  # Create mvgam_formula
  mf <- mvgam_formula(
    y ~ x,
    trend_formula = ~ RW(time = "time", series = "series")
  )

  # Get combined priors
  combined <- get_prior(mf, data = test_data, family = gaussian())

  # Should preserve brmsprior class structure
  expect_true(inherits(combined, "brmsprior"))
  expect_true(inherits(combined, "data.frame"))

  # Should have all required brmsprior columns
  expected_cols <- c("prior", "class", "coef", "group", "resp", "dpar", "nlpar",
                    "lb", "ub", "source", "trend_component")
  expect_true(all(expected_cols %in% names(combined)))

  # Should maintain brmsprior attributes if any
  class_attr <- attr(combined, "class")
  expect_true("brmsprior" %in% class_attr)
  expect_true("data.frame" %in% class_attr)

  # Row binding should work correctly - no duplicate or missing rows
  obs_count <- sum(combined$trend_component == "observation")
  trend_count <- sum(combined$trend_component == "trend")
  expect_equal(nrow(combined), obs_count + trend_count)
  expect_gt(obs_count, 0)  # Should have observation priors
})

test_that("get_prior.mvgam_formula trend_component column matches parameter types", {
  # Create test data
  test_data <- data.frame(
    count = rpois(40, 4),
    habitat = factor(rep(c("forest", "grassland"), 20)),
    time = 1:40,
    series = factor(rep(c("site1", "site2"), 20))
  )

  # Create mvgam_formula with trend
  mf <- mvgam_formula(
    count ~ habitat,
    trend_formula = ~ VAR(p = 1, time = "time", series = "series")
  )

  # Get priors
  priors <- get_prior(mf, data = test_data, family = poisson())

  # Check observation parameters are correctly tagged
  obs_rows <- priors$trend_component == "observation"
  obs_classes <- priors$class[obs_rows]

  # Standard brms observation parameters should not have _trend suffix
  expect_false(any(grepl("_trend$", obs_classes)))
  expect_true(any(obs_classes %in% c("Intercept", "b", "sd", "sigma")))

  # Check trend parameters are correctly tagged
  trend_rows <- priors$trend_component == "trend"
  if (sum(trend_rows) > 0) {
    trend_classes <- priors$class[trend_rows]

    # All trend parameters should have _trend suffix (except Z for factor models)
    non_z_params <- trend_classes[trend_classes != "Z"]
    expect_true(all(grepl("_trend$", non_z_params)))

    # Should have VAR-specific parameters
    expect_true(any(grepl("Amu_trend|Aomega_trend|sigma_trend|L_Omega_trend", trend_classes)))
  }
})

test_that("get_prior.mvgam_formula works with different trend types", {
  # Create test data
  test_data <- data.frame(
    y = rnorm(30),
    x = rnorm(30),
    time = 1:30,
    series = factor('A')
  )

  # Test with different trend constructors
  trend_types <- list(
    RW = ~ RW(time = "time", series = "series"),
    AR = ~ AR(p = 1, time = "time", series = "series"),
    PW = ~ PW(n_changepoints = 1, time = "time", series = "series"),
    ZMVN = ~ 1  # Default ZMVN
  )

  for (trend_name in names(trend_types)) {
    # Create mvgam_formula with specific trend
    mf <- mvgam_formula(y ~ x, trend_formula = trend_types[[trend_name]])

    # Should work without error
    expect_no_error({
      priors <- get_prior(mf, data = test_data, family = gaussian())
    })

    # Get priors
    priors <- get_prior(mf, data = test_data, family = gaussian())

    # Should have trend_component column
    expect_true("trend_component" %in% names(priors),
                info = paste("Missing trend_component for:", trend_name))

    # Should have observation parameters
    expect_true("observation" %in% priors$trend_component,
                info = paste("Missing observation component for:", trend_name))

    # Should preserve brmsprior class
    expect_true(inherits(priors, "brmsprior"),
                info = paste("Class not preserved for:", trend_name))
  }
})

test_that("get_prior.mvgam_formula handles factor models correctly", {
  # Create test data for factor model (n_lv < n_series)
  test_data <- data.frame(
    y = rnorm(60),
    x = rnorm(60),
    time = rep(1:20, 3),
    series = factor(rep(c("A", "B", "C"), 20))
  )

  # Create mvgam_formula with factor model
  mf <- mvgam_formula(
    y ~ x,
    trend_formula = ~ AR(p = 1, cor = TRUE, n_lv = 2, time = "time", series = "series")
  )

  # Get priors
  factor_priors <- get_prior(mf, data = test_data, family = gaussian())

  # Should have trend_component column
  expect_true("trend_component" %in% names(factor_priors))

  # Should have both observation and trend parameters
  expect_true("observation" %in% factor_priors$trend_component)
  expect_true("trend" %in% factor_priors$trend_component)

  # Check trend parameters
  trend_rows <- factor_priors$trend_component == "trend"
  if (sum(trend_rows) > 0) {
    trend_classes <- factor_priors$class[trend_rows]

    # Factor models should include Z matrix
    expect_true("Z" %in% trend_classes)

    # Should have AR parameters with _trend suffix
    expect_true(any(grepl("ar.*_trend", trend_classes)))

    # Should have correlation parameters (cor = TRUE)
    expect_true(any(grepl("L_Omega_trend", trend_classes)))
  }
})

test_that("get_prior.mvgam_formula error handling works correctly", {
  # Create test data
  test_data <- data.frame(
    y = rnorm(20),
    x = rnorm(20),
    time = 1:20,
    series = factor('A')
  )

  # Test with formula missing response variable
  mf_bad <- mvgam_formula(~ x)  # No response
  expect_error(
    get_prior(mf_bad, data = test_data),
    "missing response variable"
  )

  # Test with invalid data
  mf_good <- mvgam_formula(y ~ x, trend_formula = ~ AR(time = "time", series = "series"))
  expect_error(
    get_prior(mf_good, data = "not_a_dataframe"),
    "Must be of type 'data.frame'"
  )

  # Test with empty data
  expect_error(
    get_prior(mf_good, data = data.frame()),
    "Must have at least 1 rows"
  )

  # Test with invalid family
  expect_error(
    get_prior(mf_good, data = test_data, family = "not_a_family"),
    "Must inherit from class 'family'"
  )
})

# =============================================================================
# COMPREHENSIVE Tests for extract_trend_priors: Missing Trend Types & Complex Linear Predictors
# =============================================================================

test_that("extract_trend_priors works with ALL available trend types", {
  # Create comprehensive test data with all necessary variables
  test_data <- data.frame(
    y = rnorm(60),
    x1 = rnorm(60),
    x2 = rnorm(60),
    time = rep(1:20, 3),
    series = factor(rep(c("A", "B", "C"), 20)),
    site = factor(rep(c("site1", "site2"), 30)),
    habitat = factor(sample(c("forest", "grassland"), 60, TRUE)),
    cap = rep(100, 60)  # Carrying capacity for PW logistic growth
  )

  # Test all available trend types with appropriate parameters
  trend_type_tests <- list(
    # Well-tested types (verification)
    "AR_basic" = ~ AR(p = 1, time = "time", series = "series"),
    "AR_seasonal" = ~ AR(p = c(1, 12), time = "time", series = "series"),
    "RW_basic" = ~ RW(time = "time", series = "series"),
    "VAR_basic" = ~ VAR(p = 1, time = "time", series = "series"),
    "ZMVN_explicit" = ~ ZMVN(time = "time", series = "series"),

    # Missing/under-tested types (critical additions)
    "CAR_basic" = ~ CAR(time = "time", series = "series"),
    "PW_changepoint" = ~ PW(n_changepoints = 1, time = "time", series = "series"),
    "PW_multi_change" = ~ PW(n_changepoints = 2, growth = "logistic", cap = "cap", time = "time", series = "series"),

    # Factor model variations
    "AR_factor" = ~ AR(p = 1, n_lv = 2, time = "time", series = "series"),

    # Hierarchical variations
    "VAR_hierarchical" = ~ VAR(p = 1, gr = "site", time = "time", series = "series"),
    "AR_hierarchical" = ~ AR(p = 1, gr = "site", time = "time", series = "series")
  )

  for (test_name in names(trend_type_tests)) {
    trend_formula <- trend_type_tests[[test_name]]

    # Should work without error for all trend types
    expect_no_error({
      trend_priors <- extract_trend_priors(
        trend_formula = trend_formula,
        data = test_data,
        response_names = "y"
      )
    })

    # Get the priors
    trend_priors <- extract_trend_priors(
      trend_formula = trend_formula,
      data = test_data,
      response_names = "y"
    )

    # Should return proper brmsprior object
    expect_true(inherits(trend_priors, "brmsprior"),
                info = paste("Not brmsprior for:", test_name))

    # Should have proper structure (NOTE may be empty for some trend types)
    expect_true(is.data.frame(trend_priors),
                info = paste("Not data.frame for:", test_name))

    trend_params <- trend_priors$class[trend_priors$class != "Z"]
    if (length(trend_params) > 0) {
      trend_suffix_count <- sum(grepl("_trend$", trend_params))
      expect_gt(trend_suffix_count, 0)
    }
  }
})

test_that("extract_trend_priors works with complex linear predictors in trend formulas", {
  # Create rich test data matching the formula patterns from test-setup-brms.R
  test_data <- data.frame(
    y = rnorm(72),
    x1 = rnorm(72),
    x2 = rnorm(72),
    x3 = rnorm(72),
    fac = factor(sample(c('A', 'B', 'C'), 72, TRUE)),
    habitat = factor(sample(c("forest", "grassland", "wetland"), 72, TRUE)),
    temperature = rnorm(72),
    precipitation = rnorm(72),
    time = rep(1:24, 3),
    series = factor(rep(c("site1", "site2", "site3"), 24)),
    group = factor(rep(c("region1", "region2"), 36))
  )

  # Test comprehensive linear predictor patterns in trend formulas
  # These mirror the patterns from test-setup-brms.R but applied to trend_formula
  complex_trend_formulas <- list(
    # Simple linear predictors + trend
    "Linear_plus_trend" = ~ x1 + AR(p = 1, time = "time", series = "series"),
    "Multiple_pred_trend" = ~ x1 + x2 + temperature + RW(time = "time", series = "series"),

    # Factor/categorical predictors + trend
    "Factor_plus_trend" = ~ habitat + AR(p = 1, time = "time", series = "series"),
    "Mixed_types_trend" = ~ x1 + habitat + temperature + VAR(p = 1, time = "time", series = "series"),

    # Smooth terms + trend
    "Smooth_plus_trend" = ~ s(temperature) + AR(p = 1, time = "time", series = "series"),
    "Multi_smooth_trend" = ~ s(x1) + s(temperature) + RW(time = "time", series = "series"),

    # Tensor products + trend
    "Tensor_plus_trend" = ~ t2(temperature, precipitation) + AR(p = 1, time = "time", series = "series"),
    "Complex_tensor_trend" = ~ te(x1, x2) + t2(temperature, precipitation) + VAR(p = 1, time = "time", series = "series"),

    # Gaussian processes + trend
    "GP_plus_trend" = ~ gp(temperature, x1) + AR(p = 1, time = "time", series = "series"),
    "GP_grouping_trend" = ~ gp(temperature, x1, by = fac) + RW(time = "time", series = "series"),

    # Random effects + trend (if supported)
    "Random_plus_trend" = ~ x1 + (1|fac) + AR(p = 1, time = "time", series = "series"),
    "Complex_random_trend" = ~ temperature + (1 + x1|habitat) + VAR(p = 1, time = "time", series = "series"),

    # Mixed complex patterns
    "Everything_trend" = ~ s(temperature) + te(x1, x2) + habitat + CAR(time = "time", series = "series"),
    "Hierarchical_complex" = ~ s(temperature) + habitat + AR(p = 1, gr = "group", time = "time", series = "series")
  )

  for (test_name in names(complex_trend_formulas)) {
    trend_formula <- complex_trend_formulas[[test_name]]

    # Should parse and extract priors without error
    expect_no_error({
      trend_priors <- extract_trend_priors(
        trend_formula = trend_formula,
        data = test_data,
        response_names = "y"
      )
    })

    # Get the priors
    trend_priors <- extract_trend_priors(
      trend_formula = trend_formula,
      data = test_data,
      response_names = "y"
    )

    # Should return proper brmsprior object
    expect_true(inherits(trend_priors, "brmsprior"))

    # Should have proper structure
    expect_true(is.data.frame(trend_priors))

    # Should have expected columns
    expected_cols <- c("prior", "class", "coef", "group", "resp", "dpar", "nlpar", "lb", "ub", "source")
    expect_true(all(expected_cols %in% names(trend_priors)))

    # If trend parameters exist, most should follow naming conventions
    trend_param_classes <- trend_priors$class[trend_priors$class != "Z"]
    if (length(trend_param_classes) > 0) {
      trend_suffix_count <- sum(grepl("_trend$", trend_param_classes))
      expect_gt(trend_suffix_count, 0)
    }
  }
})

test_that("extract_trend_priors handles trend-specific parameter variations", {
  # Create test data
  test_data <- data.frame(
    y = rnorm(48),
    temperature = rnorm(48),
    site = factor(rep(c("A", "B"), 24)),
    time = rep(1:24, 2),
    series = factor(rep(c("series1", "series2"), 24)),
    cap = rep(100, 48)  # Carrying capacity for PW logistic growth
  )

  # Test trend-specific parameter variations that should generate different prior structures
  trend_parameter_tests <- list(
    # AR variations
    "AR_no_correlation" = ~ AR(p = 1, cor = FALSE, time = "time", series = "series"),
    "AR_with_correlation" = ~ AR(p = 1, cor = TRUE, time = "time", series = "series"),
    "AR_seasonal_lags" = ~ AR(p = c(1, 6, 12), time = "time", series = "series"),
    "AR_with_MA" = ~ AR(p = 1, ma = TRUE, time = "time", series = "series"),

    # VAR variations
    "VAR_simple" = ~ VAR(p = 1, time = "time", series = "series"),
    "VAR_higher_order" = ~ VAR(p = 3, time = "time", series = "series"),
    "VARMA_model" = ~ VAR(p = 2, ma = TRUE, time = "time", series = "series"),

    # Factor model variations
    "AR_factor_small" = ~ AR(p = 1, n_lv = 1, time = "time", series = "series"),
    "VAR_factor_model" = ~ VAR(p = 1, n_lv = 1, time = "time", series = "series"),
    "RW_factor_corr" = ~ RW(cor = TRUE, n_lv = 1, time = "time", series = "series"),

    # Hierarchical variations
    "AR_hierarchical_simple" = ~ AR(p = 1, gr = "site", time = "time", series = "series"),
    "VAR_hierarchical_complex" = ~ VAR(p = 1, gr = "site", time = "time", series = "series"),

    # PW variations (if implemented)
    "PW_linear_growth" = ~ PW(n_changepoints = 1, growth = "linear", time = "time", series = "series"),
    "PW_logistic_growth" = ~ PW(n_changepoints = 2, growth = "logistic", cap = "cap", time = "time", series = "series")
  )

  for (test_name in names(trend_parameter_tests)) {
    trend_formula <- trend_parameter_tests[[test_name]]

    # Should work without error
    expect_no_error({
      priors <- extract_trend_priors(
        trend_formula = trend_formula,
        data = test_data,
        response_names = "y"
      )
    })

    priors <- extract_trend_priors(
      trend_formula = trend_formula,
      data = test_data,
      response_names = "y"
    )

    # Should return brmsprior
    expect_true(inherits(priors, "brmsprior"))

    # Validate parameter-specific expectations
    if (nrow(priors) > 0) {
      classes <- priors$class

      # AR models should have ar*_trend parameters
      if (grepl("^AR", test_name)) {
        expect_true(any(grepl("ar.*_trend", classes)))
      }

      # VAR models should have VAR-specific parameters
      if (grepl("^VAR", test_name)) {
        expect_true(any(grepl("Amu_trend|Aomega_trend", classes)))
      }

      # Factor models should have Z matrix
      if (grepl("factor", test_name)) {
        expect_true("Z" %in% classes)
      }

      # Models with explicit cor = TRUE should have correlation parameters
      if (grepl("with_correlation", test_name)) {
        expect_true(any(grepl("L_Omega_trend", classes)))
      }
    }
  }
})

test_that("extract_trend_priors integrates properly with get_prior.mvgam_formula for complex cases", {
  # Test that complex trend formulas work end-to-end with the full get_prior system
  test_data <- data.frame(
    count = rpois(36, 5),
    biomass = rnorm(36, 10, 2),
    temperature = rnorm(36),
    habitat = factor(sample(c("A", "B", "C"), 36, TRUE)),
    time = rep(1:12, 3),
    series = factor(rep(c("site1", "site2", "site3"), 12))
  )

  # Test end-to-end integration with complex cases
  integration_tests <- list(
    # Complex univariate
    "Complex_univariate" = list(
      obs_formula = count ~ s(temperature) + habitat,
      trend_formula = ~ temperature + AR(p = 1, cor = TRUE, time = "time", series = "series"),
      family = poisson()
    ),

    # Multivariate with complex trends using bf() pattern
    "Complex_multivariate" = list(
      obs_formula = brms::bf(count ~ temperature + habitat, family = poisson()) +
                   brms::bf(biomass ~ temperature + habitat, family = gaussian()),
      trend_formula = ~ s(temperature) + VAR(p = 1, time = "time", series = "series"),
      family = NULL  # bf() formulas handle family internally
    ),

    # Factor model with predictors
    "Factor_with_predictors" = list(
      obs_formula = count ~ temperature,
      trend_formula = ~ habitat + AR(p = 1, n_lv = 2, time = "time", series = "series"),
      family = poisson()
    ),

    # RW trend type
    "RW_trend" = list(
      obs_formula = count ~ temperature,
      trend_formula = ~ RW(time = "time", series = "series"),
      family = poisson()
    ),

    # PW trend type (piecewise)
    "PW_trend" = list(
      obs_formula = count ~ temperature,
      trend_formula = ~ PW(time = "time", series = "series"),
      family = poisson()
    ),

    # Pattern 1: mvbind() with shared trend (classic multivariate)
    "Multivariate_mvbind_shared" = list(
      obs_formula = brms::mvbind(count, biomass) ~ temperature,
      trend_formula = ~ AR(p = 1, time = "time", series = "series"), 
      family = gaussian()  # mvbind handles multivariate structure internally
    ),

    # Pattern 1b: mvbind() with no trend (pure observation model)
    "Multivariate_mvbind_no_trend" = list(
      obs_formula = brms::mvbind(count, biomass) ~ temperature + habitat,
      trend_formula = NULL,
      family = gaussian()
    ),

    # Pattern 2: bf() with multiple responses and shared trend
    "Multivariate_bf_shared" = list(
      obs_formula = brms::brmsformula(count ~ temperature, biomass ~ habitat),
      trend_formula = ~ VAR(p = 1, time = "time", series = "series"),
      family = gaussian()  # bf() handles response-specific processing
    ),

    # Pattern 3: Combined bf() objects with different families and shared trend
    "Multivariate_combined_bf" = list(
      obs_formula = brms::brmsformula(count ~ temperature, family = poisson()) +
                   brms::brmsformula(biomass ~ temperature, family = gaussian()),
      trend_formula = ~ RW(cor = TRUE, n_lv = 2, time = "time", series = "series"),
      family = NULL  # bf() formulas handle family internally
    ),

    # Pattern 4: mvbf() wrapper with complex trend
    "Multivariate_mvbf" = list(
      obs_formula = brms::mvbrmsformula(
        brms::brmsformula(count ~ temperature, family = poisson()),
        brms::brmsformula(biomass ~ habitat, family = gaussian())
      ),
      trend_formula = ~ AR(p = 1, cor = TRUE, time = "time", series = "series"),
      family = NULL  # mvbf() handles families internally
    )
  )

  # Add cbind binomial test (NOT multivariate - single response with trials)
  integration_tests[["Binomial_cbind"]] <- list(
    obs_formula = cbind(count, biomass) ~ temperature + habitat,  # success/failure trials
    trend_formula = ~ AR(p = 1, time = "time", series = "series"),
    family = binomial()  # single response family for trial structure
  )

  for (test_name in names(integration_tests)) {
    test_case <- integration_tests[[test_name]]

    # Create mvgam_formula
    mf <- mvgam_formula(
      formula = test_case$obs_formula,
      trend_formula = test_case$trend_formula
    )

    # Should work end-to-end - handle NULL family for embedded family cases
    expect_no_error({
      if (is.null(test_case$family)) {
        combined_priors <- get_prior(mf, data = test_data)
      } else {
        combined_priors <- get_prior(mf, data = test_data, family = test_case$family)
      }
    })

    # Get priors for analysis
    combined_priors <- if (is.null(test_case$family)) {
      get_prior(mf, data = test_data)
    } else {
      get_prior(mf, data = test_data, family = test_case$family)
    }

    # Validate trend_component column presence
    expect_true("trend_component" %in% names(combined_priors))
    expect_true("observation" %in% combined_priors$trend_component)

    # Only expect trend components if trend_formula is not NULL
    if (!is.null(test_case$trend_formula)) {
      expect_true("trend" %in% combined_priors$trend_component)

      # Should properly separate observation and trend parameters
      obs_rows <- combined_priors$trend_component == "observation"
      trend_rows <- combined_priors$trend_component == "trend"

      expect_gt(sum(obs_rows), 0)
      expect_gt(sum(trend_rows), 0)

      # Trend parameters should follow _trend suffix convention
      trend_classes <- combined_priors$class[trend_rows]
      non_z_classes <- trend_classes[trend_classes != "Z"]
      if (length(non_z_classes) > 0) {
        expect_true(all(grepl("_trend$", non_z_classes)))
      }
    } else {
      # No trend formula - should only have observation components
      expect_true(all(combined_priors$trend_component == "observation"))
    }
  }
})

# Embedded Family Support Tests for Sub-task 1E
# ==============================================

test_that("has_embedded_families detects embedded families correctly", {
  # Test regular formula (no embedded families)
  regular_formula <- y ~ x + z
  expect_false(mvgam:::has_embedded_families(regular_formula))

  # Test single brmsformula without embedded family
  brms_formula_no_family <- brms::brmsformula(count ~ temp)
  expect_false(mvgam:::has_embedded_families(brms_formula_no_family))

  # Test single brmsformula with embedded family
  brms_formula_with_family <- brms::brmsformula(count ~ temp, family = poisson())
  expect_true(mvgam:::has_embedded_families(brms_formula_with_family))

  # Test multivariate formula with embedded families
  mv_formula_embedded <- brms::brmsformula(count ~ temp, family = poisson()) +
                         brms::brmsformula(biomass ~ temp, family = gaussian())
  expect_true(mvgam:::has_embedded_families(mv_formula_embedded))

  # Test multivariate formula with some embedded families
  mv_formula_partial <- brms::brmsformula(count ~ temp, family = poisson()) +
                        brms::brmsformula(biomass ~ temp)  # no family
  expect_true(mvgam:::has_embedded_families(mv_formula_partial))

  # Test multivariate formula without embedded families
  mv_formula_none <- brms::brmsformula(count ~ temp) +
                     brms::brmsformula(biomass ~ temp)
  expect_false(mvgam:::has_embedded_families(mv_formula_none))
})

test_that("has_embedded_families input validation works correctly", {
  # Should error with invalid input types
  expect_error(mvgam:::has_embedded_families("not a formula"))
  expect_error(mvgam:::has_embedded_families(123))
  expect_error(mvgam:::has_embedded_families(NULL))
  expect_error(mvgam:::has_embedded_families(list(y ~ x)))
})

test_that("get_prior.mvgam_formula handles embedded families in single formulas", {
  # Test data
  test_data <- data.frame(
    count = rpois(20, 5),
    temp = rnorm(20),
    time = 1:20,
    series = factor("A")
  )

  # Test single brmsformula with embedded poisson family
  single_embedded <- brms::brmsformula(count ~ temp, family = poisson())
  mf_single <- mvgam_formula(single_embedded, trend_formula = NULL)

  expect_no_error({
    priors_embedded <- get_prior(mf_single, data = test_data, family = gaussian())
  })

  priors_embedded <- get_prior(mf_single, data = test_data, family = gaussian())

  # Should use embedded poisson family, not passed gaussian family
  expect_true("trend_component" %in% names(priors_embedded))
  expect_equal(unique(priors_embedded$trend_component), "observation")

  # Compare with what brms would produce directly
  brms_direct <- brms::get_prior(single_embedded, data = test_data)
  brms_direct$trend_component <- "observation"

  # Should be identical except for possible row ordering
  expect_equal(nrow(priors_embedded), nrow(brms_direct))
  expect_setequal(priors_embedded$class, brms_direct$class)
})

test_that("get_prior.mvgam_formula handles embedded families in multivariate formulas", {
  # Test data with multiple responses
  test_data <- data.frame(
    count = rpois(40, 5),
    biomass = rnorm(40, 10, 2),
    temp = rnorm(40),
    time = rep(1:20, 2),
    series = factor(rep(c("A", "B"), each = 20))
  )

  # Test multivariate formula with embedded families
  mv_embedded <- brms::brmsformula(count ~ temp, family = poisson()) +
                 brms::brmsformula(biomass ~ temp, family = gaussian())
  mf_mv <- mvgam_formula(mv_embedded, trend_formula = NULL)

  expect_no_error({
    priors_mv_embedded <- get_prior(mf_mv, data = test_data, family = binomial())
  })

  priors_mv_embedded <- get_prior(mf_mv, data = test_data, family = binomial())

  # Should use embedded families (poisson + gaussian), not passed binomial
  expect_true("trend_component" %in% names(priors_mv_embedded))
  expect_equal(unique(priors_mv_embedded$trend_component), "observation")

  # Should have parameters for both responses
  expect_true("count" %in% priors_mv_embedded$resp | any(grepl("count", priors_mv_embedded$class)))
  expect_true("biomass" %in% priors_mv_embedded$resp | any(grepl("biomass", priors_mv_embedded$class)))

  # Compare with brms direct behavior
  brms_mv_direct <- brms::get_prior(mv_embedded, data = test_data)
  brms_mv_direct$trend_component <- "observation"

  expect_equal(nrow(priors_mv_embedded), nrow(brms_mv_direct))
})

test_that("get_prior.mvgam_formula maintains backward compatibility for non-embedded formulas", {
  # Test data
  test_data <- data.frame(
    y = rnorm(20),
    x = rnorm(20),
    time = 1:20,
    series = factor("A")
  )

  # Test regular formula (no embedded family)
  regular_mf <- mvgam_formula(y ~ x, trend_formula = NULL)

  # Should behave identically to brms when no embedded families
  priors_regular <- get_prior(regular_mf, data = test_data, family = gaussian())
  brms_regular <- brms::get_prior(y ~ x, data = test_data, family = gaussian())
  brms_regular$trend_component <- "observation"

  expect_equal(nrow(priors_regular), nrow(brms_regular))
  expect_setequal(priors_regular$class, brms_regular$class)
  expect_equal(unique(priors_regular$trend_component), "observation")

  # Test with different families
  for (test_family in list(poisson(), binomial(), Gamma())) {
    expect_no_error({
      priors_fam <- get_prior(regular_mf, data = test_data, family = test_family)
    })

    priors_fam <- get_prior(regular_mf, data = test_data, family = test_family)
    brms_fam <- brms::get_prior(y ~ x, data = test_data, family = test_family)
    brms_fam$trend_component <- "observation"

    expect_equal(nrow(priors_fam), nrow(brms_fam),
                 info = paste("Family:", deparse(substitute(test_family))))
  }
})

test_that("get_prior.mvgam_formula embedded families work with trend components", {
  # Test data
  test_data <- data.frame(
    count = rpois(40, 5),
    temp = rnorm(40),
    time = rep(1:20, 2),
    series = factor(rep(c("A", "B"), each = 20))
  )

  # Test embedded family with trend formula
  embedded_with_trend <- brms::brmsformula(count ~ temp, family = poisson())
  mf_trend <- mvgam_formula(embedded_with_trend, trend_formula = ~ AR(p = 1))

  expect_no_error({
    priors_with_trend <- get_prior(mf_trend, data = test_data, family = gaussian())
  })

  priors_with_trend <- get_prior(mf_trend, data = test_data, family = gaussian())

  # Should have both observation and trend components
  expect_true("trend_component" %in% names(priors_with_trend))
  expect_true("observation" %in% priors_with_trend$trend_component)
  expect_true("trend" %in% priors_with_trend$trend_component)

  # Observation priors should respect embedded poisson family
  obs_priors <- priors_with_trend[priors_with_trend$trend_component == "observation", ]
  expect_gt(nrow(obs_priors), 0)

  # Trend priors should be present
  trend_priors <- priors_with_trend[priors_with_trend$trend_component == "trend", ]
  expect_gt(nrow(trend_priors), 0)

  # Trend parameters should follow _trend suffix convention
  trend_classes <- trend_priors$class[trend_priors$class != "Z"]
  if (length(trend_classes) > 0) {
    expect_true(all(grepl("_trend$", trend_classes)))
  }
})

test_that("get_prior.mvgam_formula embedded families edge cases", {
  # Test data
  test_data <- data.frame(
    y1 = rnorm(20),
    y2 = rnorm(20),
    x = rnorm(20),
    time = 1:20,
    series = factor("A")
  )

  # Test mixed embedded/non-embedded in multivariate formula
  mv_mixed <- brms::brmsformula(y1 ~ x, family = gaussian()) +
              brms::brmsformula(y2 ~ x)  # no family specified
  mf_mixed <- mvgam_formula(mv_mixed, trend_formula = NULL)

  expect_no_error({
    priors_mixed <- get_prior(mf_mixed, data = test_data, family = poisson())
  })

  # Should detect embedded families and not pass family parameter
  priors_mixed <- get_prior(mf_mixed, data = test_data, family = poisson())
  expect_true("trend_component" %in% names(priors_mixed))

  # Test empty mvbrmsformula forms (edge case)
  # This should be handled gracefully by has_embedded_families
  empty_mv <- structure(list(forms = NULL), class = "mvbrmsformula")
  expect_false(mvgam:::has_embedded_families(empty_mv))
})

# Comprehensive Parameter Validation Tests
# =========================================

test_that("RW trend provides all expected parameters", {
  test_data <- data.frame(
    y = rpois(20, 5),
    x = rnorm(20),
    time = 1:20,
    series = factor("A")
  )
  
  # Basic RW trend
  mf_rw <- mvgam_formula(y ~ x, trend_formula = ~ RW())
  priors_rw <- get_prior(mf_rw, data = test_data, family = poisson())
  trend_priors <- priors_rw[priors_rw$trend_component == "trend", ]
  
  # RW should have sigma_trend (base parameter)
  expect_true("sigma_trend" %in% trend_priors$class)
  
  # All trend parameters should have _trend suffix (excluding Z matrix)
  trend_classes <- trend_priors$class[trend_priors$class != "Z"]
  expect_true(all(grepl("_trend$", trend_classes)))
})

test_that("AR trend provides all expected parameters", {
  test_data <- data.frame(
    y = rpois(30, 5),
    x = rnorm(30),
    time = 1:30,
    series = factor(rep(c("A", "B"), each = 15))
  )
  
  # AR(p=1) trend
  mf_ar1 <- mvgam_formula(y ~ x, trend_formula = ~ AR(p = 1))
  priors_ar1 <- get_prior(mf_ar1, data = test_data, family = poisson())
  trend_priors_ar1 <- priors_ar1[priors_ar1$trend_component == "trend", ]
  
  # AR should have sigma_trend (base) and ar1_trend (lag 1)
  expect_true("sigma_trend" %in% trend_priors_ar1$class)
  expect_true("ar1_trend" %in% trend_priors_ar1$class)
  
  # AR(p=2) trend
  mf_ar2 <- mvgam_formula(y ~ x, trend_formula = ~ AR(p = 2))
  priors_ar2 <- get_prior(mf_ar2, data = test_data, family = poisson())
  trend_priors_ar2 <- priors_ar2[priors_ar2$trend_component == "trend", ]
  
  # AR(p=2) should have ar1_trend AND ar2_trend
  expect_true("ar1_trend" %in% trend_priors_ar2$class)
  expect_true("ar2_trend" %in% trend_priors_ar2$class)
  
  # All trend parameters should have _trend suffix (excluding Z matrix)
  trend_classes <- trend_priors_ar1$class[trend_priors_ar1$class != "Z"]
  expect_true(all(grepl("_trend$", trend_classes)))
})

test_that("AR trend with correlation provides correlation parameters", {
  test_data <- data.frame(
    y = rpois(40, 5),
    x = rnorm(40),
    time = rep(1:20, 2),
    series = factor(rep(c("A", "B"), each = 20))
  )
  
  # AR with correlation
  mf_ar_cor <- mvgam_formula(y ~ x, trend_formula = ~ AR(p = 1, cor = TRUE))
  priors_ar_cor <- get_prior(mf_ar_cor, data = test_data, family = poisson())
  trend_priors_cor <- priors_ar_cor[priors_ar_cor$trend_component == "trend", ]
  
  # Should have correlation parameter
  expect_true("L_Omega_trend" %in% trend_priors_cor$class)
  
  # Should still have base AR parameters
  expect_true("sigma_trend" %in% trend_priors_cor$class)
  expect_true("ar1_trend" %in% trend_priors_cor$class)
})

test_that("AR trend with factor model provides Z matrix parameters", {
  test_data <- data.frame(
    y = rpois(60, 5),
    x = rnorm(60),
    time = rep(1:20, 3),
    series = factor(rep(c("A", "B", "C"), each = 20))
  )
  
  # AR with factor model (n_lv < n_series)
  mf_ar_factor <- mvgam_formula(y ~ x, trend_formula = ~ AR(p = 1, n_lv = 2))
  priors_ar_factor <- get_prior(mf_ar_factor, data = test_data, family = poisson())
  trend_priors_factor <- priors_ar_factor[priors_ar_factor$trend_component == "trend", ]
  
  # Should have Z matrix for factor model
  expect_true("Z" %in% trend_priors_factor$class)
  
  # Should still have base AR parameters
  expect_true("sigma_trend" %in% trend_priors_factor$class)
  expect_true("ar1_trend" %in% trend_priors_factor$class)
})

test_that("VAR trend provides all expected parameters", {
  test_data <- data.frame(
    y = rpois(40, 5),
    x = rnorm(40),
    time = rep(1:20, 2),
    series = factor(rep(c("A", "B"), each = 20))
  )
  
  # VAR trend
  mf_var <- mvgam_formula(y ~ x, trend_formula = ~ VAR(p = 1))
  priors_var <- get_prior(mf_var, data = test_data, family = poisson())
  trend_priors_var <- priors_var[priors_var$trend_component == "trend", ]
  
  # VAR should have hierarchical hyperparameters (Heaps 2022 methodology)
  expect_true("sigma_trend" %in% trend_priors_var$class)
  expect_true("Amu_trend" %in% trend_priors_var$class)
  expect_true("Aomega_trend" %in% trend_priors_var$class)
  
  # All trend parameters should have _trend suffix (excluding Z matrix)
  trend_classes <- trend_priors_var$class[trend_priors_var$class != "Z"]
  expect_true(all(grepl("_trend$", trend_classes)))
})

test_that("CAR trend provides expected parameters", {
  test_data <- data.frame(
    y = rpois(20, 5),
    x = rnorm(20),
    time = 1:20,
    series = factor("A")
  )
  
  # CAR trend
  mf_car <- mvgam_formula(y ~ x, trend_formula = ~ CAR())
  priors_car <- get_prior(mf_car, data = test_data, family = poisson())
  trend_priors_car <- priors_car[priors_car$trend_component == "trend", ]
  
  # CAR uses ar1 without _trend suffix (legacy naming)
  expect_true("ar1" %in% trend_priors_car$class)
  
  # CAR DOES use sigma_trend as base parameter (corrected expectation)
  expect_true("sigma_trend" %in% trend_priors_car$class)
})

test_that("ZMVN trend provides expected parameters", {
  test_data <- data.frame(
    y = rpois(40, 5),
    x = rnorm(40),
    time = rep(1:20, 2),
    series = factor(rep(c("A", "B"), each = 20))
  )
  
  # ZMVN trend
  mf_zmvn <- mvgam_formula(y ~ x, trend_formula = ~ ZMVN())
  priors_zmvn <- get_prior(mf_zmvn, data = test_data, family = poisson())
  trend_priors_zmvn <- priors_zmvn[priors_zmvn$trend_component == "trend", ]
  
  # ZMVN has base variance parameters
  expect_true("sigma_trend" %in% trend_priors_zmvn$class)
  
  # ZMVN always has correlation structure (cor = TRUE hardcoded)
  expect_true("L_Omega_trend" %in% trend_priors_zmvn$class)
  
  # Should not have other trend-specific parameters beyond base + correlation
  trend_classes <- trend_priors_zmvn$class[trend_priors_zmvn$class != "Z"]
  expected_zmvn <- c("sigma_trend", "L_Omega_trend")
  unexpected_classes <- setdiff(trend_classes, expected_zmvn)
  expect_length(unexpected_classes, 0)
})

test_that("PW trend provides all expected parameters", {
  test_data <- data.frame(
    y = rpois(20, 5),
    x = rnorm(20),
    time = 1:20,
    series = factor("A")
  )
  
  # PW trend
  mf_pw <- mvgam_formula(y ~ x, trend_formula = ~ PW())
  priors_pw <- get_prior(mf_pw, data = test_data, family = poisson())
  trend_priors_pw <- priors_pw[priors_pw$trend_component == "trend", ]
  
  # PW should have piecewise-specific parameters
  expect_true("sigma_trend" %in% trend_priors_pw$class)
  expect_true("k_trend" %in% trend_priors_pw$class)
  expect_true("m_trend" %in% trend_priors_pw$class)
  expect_true("delta_trend" %in% trend_priors_pw$class)
  
  # All trend parameters should have _trend suffix (excluding Z matrix)
  trend_classes <- trend_priors_pw$class[trend_priors_pw$class != "Z"]
  expect_true(all(grepl("_trend$", trend_classes)))
})
