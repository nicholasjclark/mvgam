test_that("mu expression classification handles all brms varieties correctly", {

  # Generate test data for brms patterns
  set.seed(123)
  N <- 100

  test_data <- data.frame(
    Y = rnorm(N, 6, 1),
    X_mono = sample(1:5, N, replace = TRUE),
    X_gp = runif(N, 0, 10),
    X_spline = runif(N, 0, 10),
    group_id = sample(1:10, N, replace = TRUE),
    X_fixed = rnorm(N)
  )

  # Define test cases with dynamic Stan code generation
  test_cases <- list(

    # 1. Monotonic pattern
    monotonic = list(
      name = "monotonic",
      generate_code = function() {
        brms::make_stancode(Y ~ mo(X_mono), data = test_data, family = gaussian(), parse = FALSE)
      },
      expected_mu_lines = 3,
      expected_loop_pattern = TRUE,
      key_variables = c("Intercept", "bsp", "simo_1", "Xmo_1")
    ),

    # 2. Simple GP pattern
    simple_gp = list(
      name = "simple_gp",
      generate_code = function() {
        brms::make_stancode(Y ~ gp(X_gp), data = test_data, family = gaussian(), parse = FALSE)
      },
      expected_mu_lines = 2,  # May vary with brms version
      expected_computed_variables = TRUE,
      key_variables = c("Intercept", "gp_pred", "Jgp")
    ),

    # 3. Spectral GP pattern (approximate)
    spectral_gp = list(
      name = "spectral_gp",
      generate_code = function() {
        brms::make_stancode(Y ~ gp(X_gp, c = 5/4, k = 20), data = test_data, family = gaussian(), parse = FALSE)
      },
      expected_mu_lines = 2,  # May vary
      expected_computed_variables = TRUE,
      key_variables = c("Intercept", "rgp", "gp_pred")
    ),

    # 4. Spline pattern
    spline = list(
      name = "spline",
      generate_code = function() {
        brms::make_stancode(Y ~ s(X_spline), data = test_data, family = gaussian(), parse = FALSE)
      },
      expected_mu_lines = 2,
      expected_vectorized = TRUE,
      key_variables = c("Intercept", "Xs", "bs", "Zs", "s_")
    ),

    # 5. Random effects pattern
    random_effects = list(
      name = "random_effects",
      generate_code = function() {
        brms::make_stancode(Y ~ (1|group_id), data = test_data, family = gaussian(), parse = FALSE)
      },
      expected_mu_lines = 2,
      expected_loop_pattern = TRUE,
      key_variables = c("Intercept", "r_", "J_", "Z_")
    ),

    # 6. GLM optimization pattern (fixed effects + random effects)
    glm_optimization = list(
      name = "glm_optimization",
      generate_code = function() {
        brms::make_stancode(Y ~ X_fixed + (1|group_id), data = test_data, family = gaussian(), parse = FALSE)
      },
      expected_mu_lines = 2,
      expected_glm = TRUE,
      key_variables = c("Intercept", "b", "Xc", "r_")
    ),

    # 7. Multi-GP pattern (complex indexing)
    multi_gp = list(
      name = "multi_gp",
      generate_code = function() {
        test_data$group_factor <- as.factor(sample(c("A", "B", "C"), N, replace = TRUE))
        brms::make_stancode(Y ~ gp(X_gp, by = group_factor), data = test_data, family = gaussian(), parse = FALSE)
      },
      expected_mu_lines = 2,  # May vary
      expected_indexed_pattern = TRUE,
      key_variables = c("Intercept", "gp_pred", "Igp", "Cgp", "Jgp")
    ),

    # 8. Complex spline (multiple smooths)
    complex_spline = list(
      name = "complex_spline",
      generate_code = function() {
        test_data$X_spline2 <- runif(N, 0, 10)
        test_data$X_spline3 <- runif(N, 0, 10)
        brms::make_stancode(Y ~ s(X_spline) + s(X_spline2) + s(X_spline3),
                           data = test_data, family = gaussian(), parse = FALSE)
      },
      expected_mu_lines = 2,
      expected_vectorized = TRUE,
      key_variables = c("Intercept", "Xs", "bs", "Zs_1_1", "Zs_2_1", "Zs_3_1")
    )
  )

  # Test each pattern
  for (test_case in test_cases) {

    # Generate Stan code dynamically. A generator that raises is a
    # failure, not a reason to stand down: skipping here reported a
    # broken code path as an untested one.
    stancode <- test_case$generate_code()

    # Test basic functionality - should not error
    result <- extract_mu_construction_with_classification(stancode)

    # Test return structure
    pattern <- paste("Pattern:", test_case$name)
    expect_true(is.list(result), label = pattern)
    expect_true(
      all(c("mu_construction", "supporting_declarations",
            "referenced_variables") %in% names(result)),
      label = paste(pattern, "- result fields")
    )

    # Test mu line extraction
    expect_true(length(result$mu_construction) >= 1,
                label = paste(pattern, "- at least 1 mu line"))

    # Test that classification metadata is attached
    classification <- attr(result$mu_construction, "classification")
    expect_false(is.null(classification),
                 label = paste(pattern, "- classification metadata"))

    if (!is.null(classification)) {
      # Test classification structure
      expect_equal(length(classification), length(result$mu_construction),
                   label = paste(pattern, "- classification count"))

      # Test that each classification has required fields
      for (i in seq_along(classification)) {
        cls <- classification[[i]]
        required_fields <- c("original", "normalized", "type", "execution_order",
                            "requires_loop", "variables", "structural_features")
        expect_true(all(required_fields %in% names(cls)),
                    label = paste(pattern, "- classification fields"))

        # Test execution order is valid (0-3)
        expect_true(cls$execution_order %in% 0:3,
                    label = paste(pattern, "- execution order range"))

        # Test variables structure
        expect_true(is.list(cls$variables),
                    label = paste(pattern, "- variables structure"))
        expect_true(
          all(c("functions", "indices", "parameters",
                "all_identifiers") %in% names(cls$variables)),
          label = paste(pattern, "- variables fields")
        )
      }

      # Pattern-specific tests
      if (isTRUE(test_case$expected_loop_pattern)) {
        loop_expressions <- sapply(classification, function(x) x$requires_loop)
        expect_true(any(loop_expressions),
                    label = paste(pattern, "- loop expressions"))
      }

      if (isTRUE(test_case$expected_computed_variables)) {
        computed_expressions <- sapply(classification, function(x) x$execution_order == 0)
        expect_true(any(computed_expressions),
                    label = paste(pattern, "- computed variables"))
      }

      if (isTRUE(test_case$expected_vectorized)) {
        vectorized_expressions <- sapply(classification, function(x) x$execution_order == 1)
        expect_true(any(vectorized_expressions),
                    label = paste(pattern, "- vectorized expressions"))
      }

      if (isTRUE(test_case$expected_indexed_pattern)) {
        indexed_expressions <- sapply(classification, function(x) x$execution_order == 2)
        expect_true(any(indexed_expressions),
                    label = paste(pattern, "- indexed expressions"))
      }
    }

    # Test variable extraction
    expect_true(length(result$referenced_variables) > 0,
                label = paste(pattern, "- referenced variables"))

    # Test that at least some expected variables are found (allowing for brms changes)
    found_expected_vars <- 0
    for (expected_var in test_case$key_variables) {
      # Use partial matching to handle brms naming variations
      matching_vars <- grep(expected_var, result$referenced_variables, value = TRUE, fixed = TRUE)
      if (length(matching_vars) > 0) {
        found_expected_vars <- found_expected_vars + 1
      }
    }
    expect_true(found_expected_vars >= 1,
                label = paste(pattern, "- at least 1 expected variable"))

    # Test that functions are excluded from variable references
    if (!is.null(classification)) {
      all_functions <- unique(unlist(lapply(classification, function(x) x$variables$functions)))
      overlap <- intersect(result$referenced_variables, all_functions)
      expect_equal(length(overlap), 0,
                   label = paste(pattern, "- functions among variables:",
                                 paste(overlap, collapse = ", ")))
    }

    # GLM-specific test
    if (isTRUE(test_case$expected_glm)) {
      # Should detect GLM usage in the generated code
      has_glm <- grepl("normal_id_glm_lpdf", stancode, fixed = TRUE)
      if (has_glm) {
        context <- create_analysis_context(stancode)
        expect_true(isTRUE(context$has_glm),
                    label = paste(pattern, "- GLM detected"))
      }
    }
  }
})

test_that("execution plan generation works with dynamic brms code", {

  # Generate monotonic pattern dynamically
  test_data <- data.frame(
    Y = rnorm(50, 6, 1),
    X_mono = sample(1:5, 50, replace = TRUE)
  )

  stancode <- brms::make_stancode(Y ~ mo(X_mono), data = test_data, family = gaussian(), parse = FALSE)
  result <- extract_mu_construction_with_classification(stancode)
  execution_plan <- attr(result$mu_construction, "execution_plan")

  # Should have execution plan metadata
  expect_false(is.null(execution_plan))
  expect_true(is.list(execution_plan))
  expect_true("execution_stages" %in% names(execution_plan))
  expect_true("total_stages" %in% names(execution_plan))

  # Should have proper stage organization
  stages <- execution_plan$execution_stages
  expect_true(length(stages) >= 1)  # Should have at least 1 stage

  # Stage names should be properly formatted
  stage_names <- names(stages)
  expect_true(all(grepl("^stage_\\d+$", stage_names)))
})

test_that("function discovery works with dynamic brms code", {

  # Test with GP pattern that has declared functions
  test_data <- data.frame(
    Y = rnorm(50, 0, 1),
    X = runif(50, 0, 10)
  )

  stancode <- brms::make_stancode(Y ~ gp(X), data = test_data, family = gaussian(), parse = FALSE)
  context <- create_analysis_context(stancode)

  expect_true(length(context$all_functions) > 0)       # Should always have Stan functions

  # Should include Stan reserved words
  expect_true(length(context$reserved_words) > 0)
  expect_true("sqrt" %in% context$all_functions || "rep_vector" %in% context$all_functions)
})

test_that("error handling works correctly", {

  # Test with invalid inputs
  expect_error(extract_mu_construction_with_classification(""), "All elements must have at least 1 characters")
  expect_error(extract_mu_construction_with_classification(NULL), "Must be of type 'string'")

  # Test with model block but no mu construction
  no_mu_code <- 'model { target += normal_lpdf(y | 0, 1); }'
  result <- extract_mu_construction_with_classification(no_mu_code)
  expect_equal(length(result$mu_construction), 0)
})

test_that("classification integrates properly with existing pipeline", {

  # Verifies the returned structure matches the documented interface
  test_data <- data.frame(
    Y = rnorm(30, 0, 1),
    X = rnorm(30, 0, 1)
  )

  simple_stancode <- brms::make_stancode(Y ~ X, data = test_data, family = gaussian(), parse = FALSE)

  # Should work without error in the existing pipeline
  result <- extract_mu_construction_with_classification(simple_stancode)

  # Should have the expected interface
  expect_true(is.list(result))
  expect_true(all(c("mu_construction", "supporting_declarations", "referenced_variables") %in% names(result)))
  expect_true(is.character(result$mu_construction))
  expect_true(is.character(result$supporting_declarations))
  expect_true(is.character(result$referenced_variables))
})

test_that("performance is acceptable for complex models", {

  # Generate a complex model
  test_data <- data.frame(
    Y = rnorm(100, 0, 1),
    X1 = rnorm(100, 0, 1),
    X2 = runif(100, 0, 10),
    group = sample(1:10, 100, replace = TRUE)
  )

  complex_stancode <- brms::make_stancode(
    Y ~ X1 + s(X2) + (1|group),
    data = test_data,
    family = gaussian(),
    parse = FALSE
  )

  # Should complete within reasonable time (2 seconds)
  start_time <- Sys.time()
  result <- extract_mu_construction_with_classification(complex_stancode)
  end_time <- Sys.time()

  # Two seconds is a generous ceiling for a single classification pass;
  # anything slower means the analyser has started re-scanning.
  expect_true(as.numeric(end_time - start_time) < 2.0)

  # Should still produce valid results
  expect_true(length(result$mu_construction) > 0)
  expect_true(length(result$referenced_variables) > 0)
})

# GLM detection and trend injection ----

test_that("glm_calls_present names the GLM likelihoods a program calls", {
  glm <- "model {\n  target += poisson_log_glm_lpmf(Y | X, alpha, beta);\n}"
  plain <- "model {\n  target += normal_lpdf(Y | mu, sigma);\n}"
  present <- mvgam:::glm_calls_present(glm)
  expect_identical(names(present)[present], "poisson_log_glm")
  expect_false(any(mvgam:::glm_calls_present(plain)))
})

stan_program <- function(...) paste(c(...), collapse = "\n")

test_that("the trend joins a built predictor after its last term", {
  code <- stan_program(
    "model {",
    "  if (!prior_only) {",
    "    vector[N] mu = rep_vector(0.0, N);",
    "    mu += Intercept + Xc * b;",
    "    target += normal_lpdf(Y | mu, sigma);",
    "  }",
    "}"
  )
  expect_identical(
    mvgam:::inject_trend_into_linear_predictors(code, ""),
    stan_program(
      "model {",
      "  if (!prior_only) {",
      "    vector[N] mu = rep_vector(0.0, N);",
      "    mu += Intercept + Xc * b;",
      "    for (n in 1:N) {",
      "      mu[n] += trend[obs_trend_time[n], obs_trend_series[n]];",
      "    }",
      "    target += normal_lpdf(Y | mu, sigma);",
      "  }",
      "}"
    )
  )
})

test_that("the trend joins before an inverse link inside a loop", {
  # brms applies some inverse links row by row inside the likelihood
  # loop. The trend enters on the link scale, before that loop opens.
  code <- stan_program(
    "model {",
    "  vector[N_y] mu_y = rep_vector(0.0, N_y);",
    "  mu_y += Intercept_y;",
    "  for (n in 1 : N_y) {",
    "    mu_y[n] = inv_logit(mu_y[n]);",
    "    target += beta_lpdf(Y_y[n] | mu_y[n] * phi_y, phi_y);",
    "  }",
    "}"
  )
  expect_identical(
    mvgam:::inject_trend_into_linear_predictors(code, "y"),
    stan_program(
      "model {",
      "  vector[N_y] mu_y = rep_vector(0.0, N_y);",
      "  mu_y += Intercept_y;",
      "  for (n in 1:N_y) {",
      "    mu_y[n] += trend[obs_trend_time_y[n], obs_trend_series_y[n]];",
      "  }",
      "  for (n in 1 : N_y) {",
      "    mu_y[n] = inv_logit(mu_y[n]);",
      "    target += beta_lpdf(Y_y[n] | mu_y[n] * phi_y, phi_y);",
      "  }",
      "}"
    )
  )
})

test_that("a GLM likelihood is rewritten to take the trend", {
  code <- stan_program(
    "model {",
    "  target += poisson_log_glm_lpmf(Y | Xc, Intercept, b);",
    "}"
  )
  expect_identical(
    mvgam:::inject_trend_into_linear_predictors(code, ""),
    stan_program(
      "model {",
      "  vector[N] mu = Xc * b;",
      "  for (n in 1:N) {",
      paste0("    mu[n] += Intercept + ",
             "trend[obs_trend_time[n], obs_trend_series[n]];"),
      "  }",
      "  target += poisson_log_glm_lpmf(Y | to_matrix(mu), 0.0, mu_ones);",
      "}"
    )
  )
})

test_that("an unplaceable trend is reported as an mvgam fault", {
  # A GLM family without a layout would leave the trend computed and
  # never added to the predictor.
  unknown_glm <- stan_program(
    "model {",
    "  target += mystery_glm_lpmf(Y | Xc, Intercept, b);",
    "}"
  )
  expect_error(
    mvgam:::inject_trend_into_linear_predictors(unknown_glm, ""),
    "family could not be identified"
  )
  no_predictor <- stan_program(
    "model {",
    "  target += normal_lpdf(Y | 0, sigma);",
    "}"
  )
  expect_error(
    mvgam:::inject_trend_into_linear_predictors(no_predictor, ""),
    "lacks a statement building 'mu'"
  )
})
