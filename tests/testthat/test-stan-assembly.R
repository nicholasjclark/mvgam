# Test Suite for Stan Assembly Framework (Subcomponent 1)
# ========================================================

# Setup and teardown
# ==================

# Create test data
setup_test_data <- function() {
  data.frame(
    y = rnorm(50, 0, 1),
    x = rnorm(50, 0, 1),
    time = 1:50,
    series = factor(rep(1:2, length.out = 50))
  )
}

# Create realistic trend stanvars using brms stanvar function
create_realistic_trend_stanvars <- function() {
  list(
    rw_params = brms::stanvar(
      x = NULL,
      name = "rw_params", 
      scode = "parameters { vector<lower=0>[2] sigma_rw; }"
    ),
    rw_model = brms::stanvar(
      x = NULL,
      name = "rw_model",
      scode = "model { sigma_rw ~ inv_gamma(1, 1); }"
    ),
    rw_data = brms::stanvar(
      x = 2,
      name = "n_series",
      scode = "data"
    )
  )
}

# Create invalid stanvars for testing
create_invalid_stanvars <- function() {
  list(
    invalid_no_name = list(
      scode = "parameters { real bad_param; }",
      x = NULL
    ),
    invalid_no_scode = list(
      name = "bad_stanvar",
      x = NULL
    ),
    invalid_empty_scode = list(
      name = "empty_stanvar",
      scode = "",
      x = NULL
    ),
    not_a_list = "this is not a stanvar"
  )
}

# Unit Tests: Input Validation
# ============================

test_that("assembly functions validate inputs correctly", {
  test_data <- setup_test_data()
  trend_stanvars <- create_realistic_trend_stanvars()
  
  # Test invalid formula
  expect_error(
    assemble_mvgam_stan_code("not a formula", trend_stanvars, test_data),
    "Assertion on 'obs_formula' failed"
  )
  
  expect_error(
    assemble_mvgam_stan_data("not a formula", trend_stanvars, test_data),
    "Assertion on 'obs_formula' failed"
  )
  
  # Test invalid data
  expect_error(
    assemble_mvgam_stan_code(y ~ x, trend_stanvars, "not a data frame"),
    "Assertion on 'data' failed"
  )
  
  expect_error(
    assemble_mvgam_stan_data(y ~ x, trend_stanvars, "not a data frame"),
    "Assertion on 'data' failed"
  )
  
  # Test invalid stanvars
  expect_error(
    assemble_mvgam_stan_code(y ~ x, "not a list", test_data),
    "Assertion on 'trend_stanvars' failed"
  )
  
  expect_error(
    assemble_mvgam_stan_data(y ~ x, "not a list", test_data),
    "Assertion on 'trend_stanvars' failed"
  )
  
  # Test empty data
  empty_data <- test_data[0, ]
  expect_error(
    assemble_mvgam_stan_code(y ~ x, trend_stanvars, empty_data),
    "Assertion on 'data' failed"
  )
  
  expect_error(
    assemble_mvgam_stan_data(y ~ x, trend_stanvars, empty_data),
    "Assertion on 'data' failed"
  )
})

# Unit Tests: Stan Data Consistency with brms
# ===========================================

test_that("stan data matches brms when no trend components added", {
  test_data <- setup_test_data()
  
  # Generate brms stan data directly
  brms_standata <- brms::make_standata(y ~ x, data = test_data, family = gaussian())
  
  # Generate stan data through our assembly system with empty stanvars
  empty_stanvars <- list()
  mvgam_standata <- assemble_mvgam_stan_data(
    obs_formula = y ~ x,
    trend_stanvars = empty_stanvars,
    data = test_data,
    family = gaussian()
  )
  
  # Core data should be identical
  expect_equal(mvgam_standata$N, brms_standata$N)
  expect_equal(mvgam_standata$Y, brms_standata$Y)
  expect_equal(mvgam_standata$K, brms_standata$K)
  expect_equal(mvgam_standata$X, brms_standata$X)
  
  # All core brms data components should be present
  core_components <- c("N", "Y", "K", "X")
  for (component in core_components) {
    if (component %in% names(brms_standata)) {
      expect_true(component %in% names(mvgam_standata),
                  info = paste("Missing component:", component))
      expect_equal(mvgam_standata[[component]], brms_standata[[component]],
                   info = paste("Mismatch in component:", component))
    }
  }
})

test_that("stan data preserves brms structure when adding trend components", {
  test_data <- setup_test_data()
  trend_stanvars <- create_realistic_trend_stanvars()
  
  # Generate brms stan data directly
  brms_standata <- brms::make_standata(y ~ x, data = test_data, family = gaussian())
  
  # Generate stan data through our assembly system with trend stanvars
  mvgam_standata <- assemble_mvgam_stan_data(
    obs_formula = y ~ x,
    trend_stanvars = trend_stanvars,
    data = test_data,
    family = gaussian()
  )
  
  # All original brms components should still be present and unchanged
  for (name in names(brms_standata)) {
    expect_true(name %in% names(mvgam_standata),
                info = paste("Missing original brms component:", name))
    expect_equal(mvgam_standata[[name]], brms_standata[[name]],
               info = paste("Original brms component changed:", name))
  }
  
  # Additional trend components should be added
  expect_true("n_series" %in% names(mvgam_standata))
  expect_equal(mvgam_standata$n_series, 2)
})

test_that("stan code contains required blocks", {
  test_data <- setup_test_data()
  trend_stanvars <- create_realistic_trend_stanvars()
  
  # Generate stan code
  stan_code <- assemble_mvgam_stan_code(
    obs_formula = y ~ x,
    trend_stanvars = trend_stanvars,
    data = test_data,
    family = gaussian()
  )
  
  # Should be a character string
  expect_type(stan_code, "character")
  expect_true(nchar(stan_code) > 0)
  
  # Should contain required Stan blocks
  expect_true(grepl("data\\s*\\{", stan_code))
  expect_true(grepl("parameters\\s*\\{", stan_code))
  expect_true(grepl("model\\s*\\{", stan_code))
})

# Unit Tests: Stanvar Preparation and Validation
# ==============================================

test_that("prepare_stanvars_for_brms filters valid stanvars", {
  valid_stanvars <- create_realistic_trend_stanvars()
  invalid_stanvars <- create_invalid_stanvars()
  mixed_stanvars <- c(valid_stanvars, invalid_stanvars)
  
  # Test with valid stanvars only
  result_valid <- prepare_stanvars_for_brms(valid_stanvars)
  expect_equal(length(result_valid), length(valid_stanvars))
  expect_true(all(names(result_valid) %in% names(valid_stanvars)))
  
  # Test with mixed stanvars - should filter out invalid ones
  expect_warning(
    result_mixed <- prepare_stanvars_for_brms(mixed_stanvars),
    "Skipping invalid stanvar"
  )
  expect_equal(length(result_mixed), length(valid_stanvars))
  expect_true(all(names(result_mixed) %in% names(valid_stanvars)))
})

test_that("is_valid_stanvar correctly identifies valid and invalid stanvars", {
  valid_stanvars <- create_realistic_trend_stanvars()
  invalid_stanvars <- create_invalid_stanvars()
  
  # Test valid stanvars
  for (stanvar in valid_stanvars) {
    expect_true(is_valid_stanvar(stanvar))
  }
  
  # Test invalid stanvars
  for (stanvar in invalid_stanvars) {
    expect_false(is_valid_stanvar(stanvar))
  }
  
  # Test edge cases
  expect_false(is_valid_stanvar(NULL))
  expect_false(is_valid_stanvar(list()))
  expect_false(is_valid_stanvar("not a list"))
})

# Unit Tests: Helper Functions
# ============================

test_that("helper functions work correctly", {
  # Test null coalescing operator
  expect_equal(NULL %||% "default", "default")
  expect_equal("value" %||% "default", "value")
  expect_equal(0 %||% "default", 0)
  expect_equal(FALSE %||% "default", FALSE)
  
  # Test data extraction helpers
  test_data <- setup_test_data()
  
  time_data <- extract_time_data(test_data)
  expect_true("time" %in% names(time_data))
  expect_true("n_time" %in% names(time_data))
  expect_equal(time_data$n_time, length(unique(test_data$time)))
  
  series_data <- extract_series_data(test_data)
  expect_true("series" %in% names(series_data))
  expect_true("n_series" %in% names(series_data))
  expect_equal(series_data$n_series, length(unique(test_data$series)))
  
  # Test braces balancing
  expect_true(are_braces_balanced("{ }"))
  expect_true(are_braces_balanced("{ { } }"))
  expect_true(are_braces_balanced("data { int N; } parameters { real x; }"))
  
  expect_false(are_braces_balanced("{ "))
  expect_false(are_braces_balanced(" }"))
  expect_false(are_braces_balanced("{ { }"))
})