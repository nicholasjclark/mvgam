test_that("setup_brms_lightweight validates inputs correctly", {
  # Valid inputs
  data <- data.frame(y = 1:10, x = rnorm(10))
  formula <- y ~ s(x)
  
  with_mocked_bindings(
    validate_brms_formula = function(x) list(valid = TRUE),
    brm = function(...) structure(list(), class = "brmsfit"),
    extract_stancode_from_setup = function(x) "mock stan code",
    extract_standata_from_setup = function(x) list(N = 10),
    extract_prior_from_setup = function(x) data.frame(),
    extract_brmsterms_from_setup = function(x) structure(list(), class = "brmsterms"),
    validate_setup_components = function(x) TRUE,
    .package = "mvgam",
    {
      result <- mvgam:::setup_brms_lightweight(formula, data)
      
      expect_type(result, "list")
      expect_named(result, c("formula", "data", "family", "stanvars", 
                            "stancode", "standata", "prior", "brmsterms", "setup_time"))
    }
  )
  
  # Invalid formula
  expect_error(
    mvgam:::setup_brms_lightweight("not a formula", data),
    "Assertion on 'formula' failed"
  )
  
  # Invalid data
  expect_error(
    mvgam:::setup_brms_lightweight(formula, "not a data frame"),
    "Assertion on 'data' failed"
  )
  
  # Empty data
  expect_error(
    mvgam:::setup_brms_lightweight(formula, data.frame()),
    "Assertion on 'data' failed"
  )
})

test_that("setup_brms_lightweight uses mock backend correctly", {
  data <- data.frame(y = 1:10, x = rnorm(10))
  formula <- y ~ s(x)
  
  # Mock successful brm call
  mock_brm_calls <- list()
  
  with_mocked_bindings(
    validate_brms_formula = function(x) list(valid = TRUE),
    brm = function(...) {
      args <- list(...)
      mock_brm_calls <<- c(mock_brm_calls, list(args))
      structure(list(), class = "brmsfit")
    },
    extract_stancode_from_setup = function(x) "mock stan code",
    extract_standata_from_setup = function(x) list(N = 10),
    extract_prior_from_setup = function(x) data.frame(),
    extract_brmsterms_from_setup = function(x) structure(list(), class = "brmsterms"),
    validate_setup_components = function(x) TRUE,
    .package = c("mvgam", "brms"),
    {
      result <- mvgam:::setup_brms_lightweight(formula, data, family = poisson())
      
      # Check that brm was called with correct arguments
      expect_length(mock_brm_calls, 1)
      call_args <- mock_brm_calls[[1]]
      
      expect_equal(call_args$formula, formula)
      expect_equal(call_args$data, data)
      expect_equal(call_args$backend, "mock")
      expect_equal(call_args$chains, 0)
      expect_equal(call_args$iter, 0)
      expect_true(call_args$dry_run)
      expect_equal(call_args$silent, 2)
    }
  )
})

test_that("setup_brms_lightweight handles custom families and stanvars", {
  data <- data.frame(y = runif(10), x = rnorm(10))
  formula <- y ~ s(x)
  custom_family <- Beta()
  custom_stanvars <- list(code = "// custom code")
  
  with_mocked_bindings(
    validate_brms_formula = function(x) list(valid = TRUE),
    brm = function(...) {
      args <- list(...)
      expect_equal(args$family, custom_family)
      expect_equal(args$stanvars, custom_stanvars)
      structure(list(), class = "brmsfit")
    },
    extract_stancode_from_setup = function(x) "mock stan code",
    extract_standata_from_setup = function(x) list(N = 10),
    extract_prior_from_setup = function(x) data.frame(),
    extract_brmsterms_from_setup = function(x) structure(list(), class = "brmsterms"),
    validate_setup_components = function(x) TRUE,
    {
      result <- mvgam:::setup_brms_lightweight(
        formula, data, 
        family = custom_family,
        stanvars = custom_stanvars
      )
      
      expect_equal(result$family, custom_family)
      expect_equal(result$stanvars, custom_stanvars)
    }
  )
})

test_that("setup_brms_lightweight falls back on mock failure", {
  data <- data.frame(y = 1:10, x = rnorm(10))
  formula <- y ~ s(x)
  
  with_mocked_bindings(
    validate_brms_formula = function(x) list(valid = TRUE),
    brm = function(...) stop("Mock backend failed"),
    setup_brms_fallback = function(...) {
      list(
        formula = formula,
        data = data,
        family = gaussian(),
        stanvars = NULL,
        stancode = "fallback code",
        standata = list(N = 10),
        prior = data.frame(),
        brmsterms = structure(list(), class = "brmsterms"),
        setup_time = NA_real_
      )
    },
    validate_setup_components = function(x) TRUE,
    {
      # Should warn about fallback
      expect_warning(
        result <- mvgam:::setup_brms_lightweight(formula, data),
        "Mock backend setup failed"
      )
      
      expect_equal(result$stancode, "fallback code")
      expect_true(is.na(result$setup_time))
    }
  )
})

test_that("setup_brms_fallback works with direct brms functions", {
  data <- data.frame(y = 1:10, x = rnorm(10))
  formula <- y ~ s(x)
  family <- poisson()
  
  with_mocked_bindings(
    make_stancode = function(...) "fallback stan code",
    make_standata = function(...) list(N = 10, y = 1:10),
    get_prior = function(...) data.frame(prior = "normal(0,1)", class = "Intercept"),
    brmsterms = function(...) structure(list(formula = formula), class = "brmsterms"),
    {
      result <- mvgam:::setup_brms_fallback(formula, data, family, NULL)
      
      expect_equal(result$formula, formula)
      expect_equal(result$data, data)
      expect_equal(result$family, family)
      expect_equal(result$stancode, "fallback stan code")
      expect_equal(result$standata$N, 10)
      expect_true(is.data.frame(result$prior))
      expect_s3_class(result$brmsterms, "brmsterms")
    }
  )
})

test_that("setup_brms_fallback handles errors appropriately", {
  data <- data.frame(y = 1:10, x = rnorm(10))
  formula <- y ~ s(x)
  
  # Test stancode failure
  with_mocked_bindings(
    make_stancode = function(...) stop("Stan code generation failed"),
    make_standata = function(...) list(N = 10),
    {
      expect_error(
        mvgam:::setup_brms_fallback(formula, data, gaussian(), NULL),
        "Could not create brms setup components"
      )
    }
  )
  
  # Test standata failure  
  with_mocked_bindings(
    make_stancode = function(...) "valid code",
    make_standata = function(...) stop("Stan data generation failed"),
    {
      expect_error(
        mvgam:::setup_brms_fallback(formula, data, gaussian(), NULL),
        "Could not create brms setup components"
      )
    }
  )
})

test_that("extract_stancode_from_setup handles various object types", {
  # Test with stancode method
  mock_object1 <- structure(list(), class = "brmsfit")
  
  with_mocked_bindings(
    hasMethod = function(f, signature) f == "stancode" && signature == "brmsfit",
    stancode = function(x) "extracted stan code",
    {
      result <- mvgam:::extract_stancode_from_setup(mock_object1)
      expect_equal(result, "extracted stan code")
    }
  )
  
  # Test with model slot
  mock_object2 <- list(model = "model stan code")
  result <- mvgam:::extract_stancode_from_setup(mock_object2)
  expect_equal(result, "model stan code")
  
  # Test with S4 slot
  mock_object3 <- structure(list(), stancode = "s4 stan code")
  attr(mock_object3, "class") <- "S4"
  
  with_mocked_bindings(
    hasMethod = function(...) FALSE,
    {
      # This will fall through to try-error case
      result <- mvgam:::extract_stancode_from_setup(mock_object3)
      expect_null(result)
    }
  )
})

test_that("extract_standata_from_setup handles various object types", {
  # Test with standata method
  mock_object1 <- structure(list(), class = "brmsfit")
  
  with_mocked_bindings(
    hasMethod = function(f, signature) f == "standata" && signature == "brmsfit",
    standata = function(x) list(N = 100, y = 1:100),
    {
      result <- mvgam:::extract_standata_from_setup(mock_object1)
      expect_equal(result$N, 100)
      expect_equal(length(result$y), 100)
    }
  )
  
  # Test with data slot
  mock_object2 <- list(data = list(N = 50, x = rnorm(50)))
  result <- mvgam:::extract_standata_from_setup(mock_object2)
  expect_equal(result$N, 50)
  expect_equal(length(result$x), 50)
})

test_that("validate_setup_components catches missing components", {
  # Valid components
  valid_components <- list(
    formula = y ~ s(x),
    data = data.frame(y = 1:10, x = rnorm(10)),
    family = gaussian(),
    stancode = "valid stan code",
    standata = list(N = 10)
  )
  
  expect_true(mvgam:::validate_setup_components(valid_components))
  
  # Missing required component
  invalid_components <- valid_components
  invalid_components$stancode <- NULL
  
  expect_error(
    mvgam:::validate_setup_components(invalid_components),
    "Missing required setup components"
  )
  
  # Empty stancode
  invalid_components2 <- valid_components
  invalid_components2$stancode <- ""
  
  expect_error(
    mvgam:::validate_setup_components(invalid_components2),
    "Stan code extraction failed"
  )
  
  # Empty standata
  invalid_components3 <- valid_components
  invalid_components3$standata <- list()
  
  expect_error(
    mvgam:::validate_setup_components(invalid_components3),
    "Stan data extraction failed"
  )
})

test_that("setup_brms_lightweight validates brms formula before processing", {
  data <- data.frame(y = 1:10, x = rnorm(10))
  
  # Invalid formula should stop early
  with_mocked_bindings(
    validate_brms_formula = function(x) list(valid = FALSE, issues = "Invalid syntax"),
    {
      expect_error(
        mvgam:::setup_brms_lightweight(y ~ invalid_function(x), data),
        "Invalid brms formula structure"
      )
    }
  )
})

test_that("setup_brms_lightweight performance tracking works", {
  data <- data.frame(y = 1:10, x = rnorm(10))
  formula <- y ~ s(x)
  
  with_mocked_bindings(
    validate_brms_formula = function(x) list(valid = TRUE),
    brm = function(...) {
      Sys.sleep(0.001) # Minimal delay for timing
      structure(list(), class = "brmsfit")
    },
    extract_stancode_from_setup = function(x) "mock stan code",
    extract_standata_from_setup = function(x) list(N = 10),
    extract_prior_from_setup = function(x) data.frame(),
    extract_brmsterms_from_setup = function(x) structure(list(), class = "brmsterms"),
    validate_setup_components = function(x) TRUE,
    {
      result <- mvgam:::setup_brms_lightweight(formula, data)
      
      expect_true(is.numeric(result$setup_time))
      expect_true(result$setup_time >= 0)
    }
  )
})