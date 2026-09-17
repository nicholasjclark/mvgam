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

test_that("setup_brms_lightweight carries only the metadata it set", {
  # The observation-side call assigns no `trend_metadata`. A bare
  # `exists()` on that name then reaches the enclosing environments
  # instead of the call, and an object a user happens to hold under
  # that name in their session is stamped onto the setup.
  data <- data.frame(
    y = rnorm(20),
    x = rnorm(20),
    time = 1:20,
    series = factor(rep(1:2, each = 10))
  )
  assign("trend_metadata", "planted", envir = globalenv())
  withr::defer(rm("trend_metadata", envir = globalenv()))

  setup <- setup_brms_lightweight(
    formula = y ~ x,
    data = data,
    trend_formula = NULL
  )
  expect_null(setup$trend_metadata)
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

  expect_true("trend_formula" %in% names(setup))
  expect_true(inherits(setup$trend_formula, "formula"))

  # A trend formula naming no constructor defaults to ZMVN. Admitting
  # a list or a NULL took whatever the parse returned, a parse that
  # produced nothing included.
  expect_identical(setup$trend_specs$trend_specs$trend, "ZMVN")
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
                          "standata", "prior", "brmsterms", "brmsfit",
                          "setup_time")

  for (component in required_components) {
    expect_true(component %in% names(setup),
                label = paste("setup component", component))
  }

  # Components should have correct types
  expect_true(inherits(setup$formula, "formula"))
  expect_true(is.data.frame(setup$data))
  expect_true(is.character(setup$stancode))
  expect_true(is.list(setup$standata))
  expect_true(inherits(setup$prior, "brmsprior"))
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

  # A character count passed on any string at all. The mock backend
  # emits a whole Stan program, with each block opening on one line.
  expect_true(is.character(setup$stancode))
  for (blk in c("functions", STAN_BLOCKS)) {
    expect_identical(stan_block_count(setup$stancode, blk), 1L)
  }
  # Stan data carries the response and the row count brms took from
  # the frame, which counting its elements left unexamined.
  expect_true(is.list(setup$standata))
  expect_identical(setup$standata$N, nrow(data))
  expect_identical(as.numeric(setup$standata$Y), data$y)
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

  # Each formula names the trend model its parse has to reach. The
  # three cases were separated only by `is.list()`, which is true of
  # every parse and of one that fell back to the default.
  # `~ 1` is pinned in "includes trend information in output" on its
  # own fixture. These two add a formula whose terms name no
  # constructor and one that names RW.
  cases <- list(
    list(tf = ~ gp(time, k = 6), trend = "ZMVN"),
    list(
      tf = ~ temperature + RW(time = time, series = series) + habitat,
      trend = "RW"
    )
  )
  for (case in cases) {
    setup <- setup_brms_lightweight(
      formula = y ~ x, data = data, trend_formula = case$tf
    )
    expect_identical(setup$trend_specs$trend_specs$trend, case$trend)
  }

  # brms cannot evaluate a trend constructor. The parse hands it a
  # base formula with the constructor removed and the covariates
  # around it kept.
  explicit <- setup_brms_lightweight(
    formula = y ~ x, data = data,
    trend_formula =
      ~ temperature + RW(time = time, series = series) + habitat
  )
  expect_identical(
    all.vars(explicit$trend_specs$base_formula),
    c("temperature", "habitat")
  )
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
    mvgam:::validate_obs_formula_brms(setup$formula)
  })
})
