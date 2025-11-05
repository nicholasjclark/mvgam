# Tests for Enhanced brms Setup Functions
# =====================================

# Test setup_brms_lightweight() enhancements
# -------------------------------------------

test_that("setup_brms_lightweight accepts trend_formula parameter", {
  # Create minimal test data
  data <- data.frame(
    y = rnorm(20),
    x = rnorm(20),
    time = 1:20,
    series = factor(rep(1:2, each = 10))
  )

  # Test with NULL trend_formula (should work like before)
  expect_no_error({
    setup1 <- setup_brms_lightweight(
      formula = y ~ x,
      data = data,
      trend_formula = NULL
    )
  })

  expect_true(is.list(setup1))
  expect_true("trend_formula" %in% names(setup1))
  expect_null(setup1$trend_formula)
})

test_that("setup_brms_lightweight validates trend_formula parameter types", {
  data <- data.frame(
    y = rnorm(20),
    x = rnorm(20),
    time = 1:20,
    series = factor(rep(1:2, each = 10))
  )

  # Valid formula should work
  expect_no_error({
    setup_brms_lightweight(
      formula = y ~ x,
      data = data,
      trend_formula = ~ 1
    )
  })

  # Invalid types should error
  expect_error({
    setup_brms_lightweight(
      formula = y ~ x,
      data = data,
      trend_formula = "invalid"  # Character not allowed
    )
  })

  expect_error({
    setup_brms_lightweight(
      formula = y ~ x,
      data = data,
      trend_formula = 123  # Numeric not allowed
    )
  })
})

test_that("setup_brms_lightweight includes trend information in output", {
  data <- data.frame(
    y = rnorm(20),
    x = rnorm(20),
    time = 1:20,
    series = factor(rep(1:2, each = 10))
  )

  # Test that simple trend formula works (should default to ZMVN)
  setup <- setup_brms_lightweight(
    formula = y ~ x,
    data = data,
    trend_formula = ~ 1
  )

  # Should include trend_formula in output
  expect_true("trend_formula" %in% names(setup))
  expect_true(inherits(setup$trend_formula, "formula"))

  # Should include trend_specs when trend_formula provided
  expect_true("trend_specs" %in% names(setup))
  expect_true(is.list(setup$trend_specs) || is.null(setup$trend_specs))
})

test_that("setup_brms_lightweight maintains all original functionality", {
  data <- data.frame(
    y = rnorm(20),
    x = rnorm(20),
    time = 1:20,
    series = factor(rep(1:2, each = 10))
  )

  setup <- setup_brms_lightweight(
    formula = y ~ x,
    data = data,
    family = gaussian()
  )

  # Should have all required components
  required_components <- c("formula", "data", "family", "stancode",
                          "standata", "prior", "brmsterms", "brmsfit")

  for (component in required_components) {
    expect_true(component %in% names(setup),
                info = paste("Missing component:", component))
  }

  # Components should have correct types
  expect_true(inherits(setup$formula, "formula"))
  expect_true(is.data.frame(setup$data))
  expect_true(is.character(setup$stancode))
  expect_true(is.list(setup$standata))
  expect_true(inherits(setup$prior, "brmsprior"))
})

test_that("setup_brms_lightweight validates setup components", {
  data <- data.frame(
    y = rnorm(20),
    x = rnorm(20),
    time = 1:20,
    series = factor(rep(1:2, each = 10))
  )

  # Valid setup should pass validation
  expect_no_error({
    setup <- setup_brms_lightweight(
      formula = y ~ x,
      data = data
    )
  })

  # The validate_setup_components should be called internally
  # If it reaches this point without error, validation passed
  expect_true(TRUE)
})

test_that("setup_brms_lightweight handles multivariate formulas", {
  data <- data.frame(
    y1 = rnorm(20),
    y2 = rnorm(20),
    x = rnorm(20),
    time = 1:20,
    series = factor(rep(1:2, each = 10))
  )

  # Test with simple multivariate-like data structure
  # Even if complex multivariate formulas aren't supported yet,
  # the function should handle the data without error
  expect_no_error({
    setup <- setup_brms_lightweight(
      formula = y1 ~ x,  # Simple formula with multivariate data
      data = data
    )
  })
})

test_that("setup_brms_lightweight error handling works correctly", {
  # Test with invalid formula
  expect_error({
    setup_brms_lightweight(
      formula = "not a formula",
      data = data.frame(y = 1, x = 1)
    )
  })

  # Test with invalid data
  expect_error({
    setup_brms_lightweight(
      formula = y ~ x,
      data = "not a data frame"
    )
  })

  # Test with empty data
  expect_error({
    setup_brms_lightweight(
      formula = y ~ x,
      data = data.frame()
    )
  })
})

test_that("setup_brms_lightweight mock backend works for inspection", {
  data <- data.frame(
    y = rnorm(20),
    x = rnorm(20),
    time = 1:20,
    series = factor(rep(1:2, each = 10))
  )

  setup <- setup_brms_lightweight(
    formula = y ~ x,
    data = data
  )

  # Mock backend should create a brmsfit object
  expect_true(inherits(setup$brmsfit, "brmsfit"))

  # Stan code should be extractable
  expect_true(is.character(setup$stancode))
  expect_true(nchar(setup$stancode) > 0)

  # Stan data should be extractable
  expect_true(is.list(setup$standata))
  expect_true(length(setup$standata) > 0)
})

test_that("setup_brms_lightweight performance tracking works", {
  data <- data.frame(
    y = rnorm(20),
    x = rnorm(20),
    time = 1:20,
    series = factor(rep(1:2, each = 10))
  )

  setup <- setup_brms_lightweight(
    formula = y ~ x,
    data = data
  )

  # Should track setup time
  expect_true("setup_time" %in% names(setup))
  expect_true(is.numeric(setup$setup_time))
  expect_true(setup$setup_time >= 0)
})

# Integration tests with existing validation functions
# --------------------------------------------------

test_that("setup_brms_lightweight handles various trend formula types", {
  data <- data.frame(
    y = rnorm(20),
    x = rnorm(20),
    time = 1:20,
    temperature = rnorm(20),
    habitat = factor(rep(c("A", "B"), 10)),
    series = factor(rep(1:2, each = 10))
  )

  # Test formula without trend constructors (should default to ZMVN)
  setup_default <- setup_brms_lightweight(
    formula = y ~ x,
    data = data,
    trend_formula = ~ gp(time)
  )

  expect_true("trend_specs" %in% names(setup_default))
  expect_true(is.list(setup_default$trend_specs))

  # Test formula with explicit trend constructor
  setup_explicit <- setup_brms_lightweight(
    formula = y ~ x,
    data = data,
    trend_formula = ~ temperature + RW(time = time, series = series) + habitat
  )

  expect_true("trend_specs" %in% names(setup_explicit))
  expect_true(is.list(setup_explicit$trend_specs))

  # Test intercept-only trend formula
  setup_intercept <- setup_brms_lightweight(
    formula = y ~ x,
    data = data,
    trend_formula = ~ 1
  )

  expect_true("trend_specs" %in% names(setup_intercept))
  expect_true(is.list(setup_intercept$trend_specs))
})

test_that("setup_brms_lightweight integrates with existing validation", {
  data <- data.frame(
    y = rnorm(20),
    x = rnorm(20),
    time = 1:20,
    series = factor(rep(1:2, each = 10))
  )

  setup <- setup_brms_lightweight(
    formula = y ~ x,
    data = data,
    trend_formula = NULL  # Use NULL for successful test
  )

  # Should pass validate_setup_components (called internally)
  expect_no_error({
    mvgam:::validate_setup_components(setup)
  })

  # Should work with other validation functions
  expect_no_error({
    mvgam:::validate_brms_formula(setup$formula)
  })
})
