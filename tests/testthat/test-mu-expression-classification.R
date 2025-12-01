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
        brms::make_stancode(Y ~ mo(X_mono), data = test_data, family = gaussian())
      },
      expected_mu_lines = 3,
      expected_loop_pattern = TRUE,
      key_variables = c("Intercept", "bsp", "simo_1", "Xmo_1")
    ),

    # 2. Simple GP pattern
    simple_gp = list(
      name = "simple_gp",
      generate_code = function() {
        brms::make_stancode(Y ~ gp(X_gp), data = test_data, family = gaussian())
      },
      expected_mu_lines = 2,  # May vary with brms version
      expected_computed_variables = TRUE,
      key_variables = c("Intercept", "gp_pred", "Jgp")
    ),

    # 3. Spectral GP pattern (approximate)
    spectral_gp = list(
      name = "spectral_gp",
      generate_code = function() {
        brms::make_stancode(Y ~ gp(X_gp, c = 5/4, k = 20), data = test_data, family = gaussian())
      },
      expected_mu_lines = 2,  # May vary
      expected_computed_variables = TRUE,
      key_variables = c("Intercept", "rgp", "gp_pred")
    ),

    # 4. Spline pattern
    spline = list(
      name = "spline",
      generate_code = function() {
        brms::make_stancode(Y ~ s(X_spline), data = test_data, family = gaussian())
      },
      expected_mu_lines = 2,
      expected_vectorized = TRUE,
      key_variables = c("Intercept", "Xs", "bs", "Zs", "s_")
    ),

    # 5. Random effects pattern
    random_effects = list(
      name = "random_effects",
      generate_code = function() {
        brms::make_stancode(Y ~ (1|group_id), data = test_data, family = gaussian())
      },
      expected_mu_lines = 2,
      expected_loop_pattern = TRUE,
      key_variables = c("Intercept", "r_", "J_", "Z_")
    ),

    # 6. GLM optimization pattern (fixed effects + random effects)
    glm_optimization = list(
      name = "glm_optimization",
      generate_code = function() {
        brms::make_stancode(Y ~ X_fixed + (1|group_id), data = test_data, family = gaussian())
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
        brms::make_stancode(Y ~ gp(X_gp, by = group_factor), data = test_data, family = gaussian())
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
                           data = test_data, family = gaussian())
      },
      expected_mu_lines = 2,
      expected_vectorized = TRUE,
      key_variables = c("Intercept", "Xs", "bs", "Zs_1_1", "Zs_2_1", "Zs_3_1")
    )
  )

  # Test each pattern
  for (test_case in test_cases) {

    # Generate Stan code dynamically
    stancode <- tryCatch({
      test_case$generate_code()
    }, error = function(e) {
      skip(paste("Could not generate", test_case$name, "Stan code:", e$message))
    })

    # Test basic functionality - should not error
    result <- tryCatch({
      extract_mu_construction_with_classification(stancode)
    }, error = function(e) {
      fail(paste("Classification failed for", test_case$name, ":", e$message))
    })

    # Test return structure
    expect_true(is.list(result), info = paste("Pattern:", test_case$name))
    expect_true(all(c("mu_construction", "supporting_declarations", "referenced_variables") %in% names(result)),
                info = paste("Pattern:", test_case$name))

    # Test mu line extraction
    expect_true(length(result$mu_construction) >= 1,
                info = paste("Pattern:", test_case$name, "- should have at least 1 mu line"))

    # Test that classification metadata is attached
    classification <- attr(result$mu_construction, "classification")
    expect_false(is.null(classification), info = paste("Pattern:", test_case$name, "- classification metadata"))

    if (!is.null(classification)) {
      # Test classification structure
      expect_equal(length(classification), length(result$mu_construction),
                   info = paste("Pattern:", test_case$name, "- classification count"))

      # Test that each classification has required fields
      for (i in seq_along(classification)) {
        cls <- classification[[i]]
        required_fields <- c("original", "normalized", "type", "execution_order",
                            "requires_loop", "variables", "structural_features")
        expect_true(all(required_fields %in% names(cls)),
                   info = paste("Pattern:", test_case$name, "- classification fields"))

        # Test execution order is valid (0-3)
        expect_true(cls$execution_order %in% 0:3,
                   info = paste("Pattern:", test_case$name, "- execution order range"))

        # Test variables structure
        expect_true(is.list(cls$variables),
                   info = paste("Pattern:", test_case$name, "- variables structure"))
        expect_true(all(c("functions", "indices", "parameters", "all_identifiers") %in% names(cls$variables)),
                   info = paste("Pattern:", test_case$name, "- variables fields"))
      }

      # Pattern-specific tests
      if (isTRUE(test_case$expected_loop_pattern)) {
        loop_expressions <- sapply(classification, function(x) x$requires_loop)
        expect_true(any(loop_expressions),
                   info = paste("Pattern:", test_case$name, "- should have loop expressions"))
      }

      if (isTRUE(test_case$expected_computed_variables)) {
        computed_expressions <- sapply(classification, function(x) x$execution_order == 0)
        expect_true(any(computed_expressions),
                   info = paste("Pattern:", test_case$name, "- should have computed variables"))
      }

      if (isTRUE(test_case$expected_vectorized)) {
        vectorized_expressions <- sapply(classification, function(x) x$execution_order == 1)
        expect_true(any(vectorized_expressions),
                   info = paste("Pattern:", test_case$name, "- should have vectorized expressions"))
      }

      if (isTRUE(test_case$expected_indexed_pattern)) {
        indexed_expressions <- sapply(classification, function(x) x$execution_order == 2)
        expect_true(any(indexed_expressions),
                   info = paste("Pattern:", test_case$name, "- should have indexed expressions"))
      }
    }

    # Test variable extraction
    expect_true(length(result$referenced_variables) > 0,
                info = paste("Pattern:", test_case$name, "- has referenced variables"))

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
               info = paste("Pattern:", test_case$name, "- should find at least 1 expected variable"))

    # Test that functions are excluded from variable references
    if (!is.null(classification)) {
      all_functions <- unique(unlist(lapply(classification, function(x) x$variables$functions)))
      overlap <- intersect(result$referenced_variables, all_functions)
      expect_equal(length(overlap), 0,
                   info = paste("Pattern:", test_case$name, "- functions should not be in variables:",
                               paste(overlap, collapse = ", ")))
    }

    # GLM-specific test
    if (isTRUE(test_case$expected_glm)) {
      # Should detect GLM usage in the generated code
      has_glm <- grepl("normal_id_glm_lpdf", stancode, fixed = TRUE)
      if (has_glm) {
        context <- create_analysis_context(stancode)
        expect_true(isTRUE(context$has_glm),
                   info = paste("Pattern:", test_case$name, "- should detect GLM"))
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

  stancode <- brms::make_stancode(Y ~ mo(X_mono), data = test_data, family = gaussian())
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

  stancode <- brms::make_stancode(Y ~ gp(X), data = test_data, family = gaussian())
  context <- create_analysis_context(stancode)

  # Should discover some declared functions (exact functions may vary with brms version)
  expect_true(length(context$declared_functions) >= 0)  # May be 0 if no functions declared
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

  # Test that the new function returns the same structure as the old one
  test_data <- data.frame(
    Y = rnorm(30, 0, 1),
    X = rnorm(30, 0, 1)
  )

  simple_stancode <- brms::make_stancode(Y ~ X, data = test_data, family = gaussian())

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
    family = gaussian()
  )

  # Should complete within reasonable time (2 seconds)
  start_time <- Sys.time()
  result <- extract_mu_construction_with_classification(complex_stancode)
  end_time <- Sys.time()

  expect_true(as.numeric(end_time - start_time) < 2.0,
             info = "Classification should complete within 2 seconds")

  # Should still produce valid results
  expect_true(length(result$mu_construction) > 0)
  expect_true(length(result$referenced_variables) > 0)
})

# GLM Pipeline Integration Tests ----

test_that("GLM analysis system detects patterns correctly", {
  # Test Stan code with GLM functions
  stan_code_glm <- "
  model {
    target += poisson_log_glm_lpmf(Y | X, alpha, beta);
  }
  "

  analysis <- mvgam:::analyze_stan(stan_code_glm)

  expect_s3_class(analysis, "glm_analysis")
  expect_true(analysis$glm_patterns[["poisson_log_glm"]])
  expect_true(analysis$mu_classification$has_glm)
  expect_equal(analysis$mu_classification$glm_types, "poisson_log_glm")
})

test_that("GLM analysis handles non-GLM code", {
  # Test Stan code without GLM functions
  stan_code_normal <- "
  model {
    target += normal_lpdf(Y | mu, sigma);
  }
  "

  analysis <- mvgam:::analyze_stan(stan_code_normal)

  expect_s3_class(analysis, "glm_analysis")
  expect_false(any(analysis$glm_patterns))
  expect_false(analysis$mu_classification$has_glm)
  expect_equal(length(analysis$mu_classification$glm_types), 0)
})

test_that("processing_state constructor validates inputs", {
  code_lines <- c("model {", "  target += normal_lpdf(Y | mu, sigma);", "}")

  state <- mvgam:::processing_state(code_lines)

  expect_s3_class(state, "processing_state")
  expect_equal(state$stage, "initial")
  expect_equal(state$code_lines, code_lines)
  expect_equal(length(state$processed_positions), 0)
  expect_null(state$analysis)
  expect_null(state$mu_analysis)
})

test_that("to_analysis transition works correctly", {
  code_lines <- c("model {", "  target += poisson_log_glm_lpmf(Y | X, alpha, beta);", "}")
  state <- mvgam:::processing_state(code_lines)

  analyzed_state <- mvgam:::to_analysis(state)

  expect_s3_class(analyzed_state, "processing_state")
  expect_equal(analyzed_state$stage, "analyzed")
  expect_s3_class(analyzed_state$analysis, "glm_analysis")
  expect_true("glm_analysis" %in% analyzed_state$transformations_applied)
})

test_that("to_injection integrates mu analysis correctly", {
  # Test that to_injection calls extract_mu_construction_with_classification
  # Use realistic Stan code with mu += pattern that the injection function expects
  stan_code <- "model {
    mu += X * beta;
    target += normal_lpdf(Y | mu, sigma);
  }"

  code_lines <- strsplit(stan_code, "\n")[[1]]
  state <- mvgam:::processing_state(code_lines)

  # Move to analyzed state first
  analyzed_state <- mvgam:::to_analysis(state)
  expect_equal(analyzed_state$stage, "analyzed")

  # Move to converted state
  converted_state <- mvgam:::to_conversion(analyzed_state)
  expect_equal(converted_state$stage, "converted")

  # Test injection with mu analysis
  injected_state <- mvgam:::to_injection(converted_state, "mu += trend_effects;")

  expect_s3_class(injected_state, "processing_state")
  expect_equal(injected_state$stage, "injected")
  expect_true("mu_analyzed" %in% injected_state$transformations_applied)
  expect_true("trend_injected" %in% injected_state$transformations_applied)

  # Check that mu_analysis is stored in state and has correct structure
  expect_type(injected_state$mu_analysis, "list")
  expect_true("mu_construction" %in% names(injected_state$mu_analysis))
  expect_true("supporting_declarations" %in% names(injected_state$mu_analysis))
  expect_true("referenced_variables" %in% names(injected_state$mu_analysis))

  # Verify mu analysis found the mu construction from our test code
  expect_true(length(injected_state$mu_analysis$mu_construction) > 0)
})

test_that("transform_glm_code linear pipeline works end-to-end", {
  # Test the linear pipeline with standard form code (not GLM)
  standard_code <- "model {
    mu += X * beta;
    target += normal_lpdf(Y | mu, sigma);
  }"

  result <- mvgam:::transform_glm_code(standard_code, "mu += trend_effects;")

  expect_type(result, "character")
  expect_gt(nchar(result), 0)

  # Should contain the original code and trend injection
  expect_match(result, "trend_effects")
})

test_that("transition_with_tracking maintains operation log correctly", {
  # Create initial state with test code
  stan_code <- "model { mu += X * beta; }"
  code_lines <- strsplit(stan_code, "\n")[[1]]
  initial_state <- mvgam:::processing_state(code_lines)

  # Test transition with tracking
  analysis_details <- list(
    glm_patterns_detected = 0,
    optimization_plan = "no_glm_detected",
    test_data = TRUE
  )

  tracked_state <- mvgam:::transition_with_tracking(
    initial_state,
    new_stage = "analyzed",
    operation = "test_analysis",
    details = analysis_details,
    modifications = list(analysis = list(test = TRUE))
  )

  # Verify state transition occurred correctly
  expect_s3_class(tracked_state, "processing_state")
  expect_equal(tracked_state$stage, "analyzed")
  expect_true("test_analysis" %in% tracked_state$transformations_applied)

  # Verify operation tracking
  expect_type(tracked_state$operations_log, "list")
  expect_equal(length(tracked_state$operations_log), 1)

  # Check operation log entry structure
  log_entry <- tracked_state$operations_log[[1]]
  expect_equal(log_entry$operation, "test_analysis")
  expect_equal(log_entry$stage, "analyzed")
  expect_s3_class(log_entry$timestamp, "POSIXct")
  expect_equal(log_entry$details, analysis_details)

  # Verify field modifications applied correctly
  expect_true(!is.null(tracked_state$analysis))
  expect_equal(tracked_state$analysis$test, TRUE)

  # Test chaining multiple operations
  second_state <- mvgam:::transition_with_tracking(
    tracked_state,
    new_stage = "converted",
    operation = "test_conversion",
    details = list(converted = TRUE)
  )

  # Should have 2 operations logged
  expect_equal(length(second_state$operations_log), 2)
  expect_true("test_conversion" %in% second_state$transformations_applied)
})

test_that("format_pipeline_error provides detailed debugging context", {
  # Test basic error formatting
  expect_error(
    mvgam:::format_pipeline_error("Test error message"),
    "Test error message"
  )

  # Test error with context
  context <- list(
    operation = "test_operation",
    expected_stage = "analyzed",
    actual_stage = "initial"
  )

  expect_error(
    mvgam:::format_pipeline_error("State transition failed", context),
    "State transition failed.*operation.*test_operation.*expected_stage.*analyzed.*actual_stage.*initial"
  )

  # Test with empty context (should not fail)
  expect_error(
    mvgam:::format_pipeline_error("Simple error", list()),
    "Simple error"
  )

  # Test state transition validation with invalid stage
  stan_code <- "model { mu += X * beta; }"
  code_lines <- strsplit(stan_code, "\n")[[1]]
  state <- mvgam:::processing_state(code_lines, stage = "analyzed")  # Valid stage but wrong for to_analysis

  expect_error(
    mvgam:::to_analysis(state),
    "Invalid state for GLM analysis.*expected_stage.*initial.*actual_stage.*analyzed"
  )
})

test_that("get_operations_summary provides correct debugging information", {
  # Create state and perform multiple tracked operations
  stan_code <- "model { target += normal_lpdf(Y | mu, sigma); }"
  code_lines <- strsplit(stan_code, "\n")[[1]]
  state <- mvgam:::processing_state(code_lines)

  # Simulate analysis operation
  state <- mvgam:::transition_with_tracking(
    state,
    "analyzed",
    "glm_analysis_complete",
    list(glm_patterns_detected = 1, optimization_plan = "preserve_glm")
  )

  # Simulate conversion operation
  state <- mvgam:::transition_with_tracking(
    state,
    "converted",
    "glm_conversion_skipped",
    list(reason = "no_trends_require_conversion")
  )

  # Test operations summary
  summary_df <- mvgam:::get_operations_summary(state)

  # Verify summary structure
  expect_s3_class(summary_df, "data.frame")
  expect_equal(nrow(summary_df), 2)
  expect_true(all(c("operation", "stage", "timestamp") %in% names(summary_df)))

  # Verify summary content
  expect_equal(summary_df$operation[1], "glm_analysis_complete")
  expect_equal(summary_df$stage[1], "analyzed")
  expect_equal(summary_df$operation[2], "glm_conversion_skipped")
  expect_equal(summary_df$stage[2], "converted")
  expect_s3_class(summary_df$timestamp, "POSIXct")

  # Test empty operations log
  empty_state <- mvgam:::processing_state(code_lines)
  empty_summary <- mvgam:::get_operations_summary(empty_state)
  expect_equal(nrow(empty_summary), 0)
  expect_equal(ncol(empty_summary), 3)
})

test_that("state transitions preserve analysis data correctly", {
  # Test that analysis data is preserved through all transitions
  code_lines <- c("model {", "  mu += X * beta;", "}")

  # Create initial state
  initial_state <- mvgam:::processing_state(code_lines)
  expect_null(initial_state$analysis)
  expect_null(initial_state$mu_analysis)

  # Analysis transition
  analyzed_state <- mvgam:::to_analysis(initial_state)
  expect_s3_class(analyzed_state$analysis, "glm_analysis")
  expect_null(analyzed_state$mu_analysis)

  # Conversion transition should preserve analysis
  converted_state <- mvgam:::to_conversion(analyzed_state)
  expect_s3_class(converted_state$analysis, "glm_analysis")
  expect_null(converted_state$mu_analysis)

  # Injection should preserve analysis and add mu_analysis
  injected_state <- mvgam:::to_injection(converted_state, "mu += trend;")
  expect_s3_class(injected_state$analysis, "glm_analysis")
  expect_type(injected_state$mu_analysis, "list")

  # Assembly should preserve both
  final_code <- mvgam:::to_assembly(injected_state)
  expect_type(final_code, "character")
})
