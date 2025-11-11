# Test Random Effects Patterns in mvgam
# 
# PURPOSE: Systematically test which random effects patterns work with mvgam
# by attempting Stan code generation and compilation via stancode.mvgam_formula()
# with validate = TRUE (uses rstan::stanc() for validation)
#
# APPROACH: Test increasingly complex random effects patterns with different
# trend formulas to identify working vs problematic combinations
#
# CRITICAL: Always use devtools::load_all() at start

# Load mvgam development version
devtools::load_all()

# Required packages
library(checkmate)
library(insight)

# ============================================================================
# SETUP: Test Data Generation
# ============================================================================

#' Generate Test Data for Random Effects Testing
#'
#' Creates standardized test datasets with known structure for systematic
#' random effects pattern testing.
#'
#' @param n_time Number of time points (default: 8)
#' @param n_sites Number of sites (default: 3) 
#' @param n_plots Number of plots per site (default: 2)
#' @param n_observers Number of observers (default: 2)
#' @param add_covariates Include continuous covariates (default: TRUE)
#'
#' @return List containing univariate and multivariate datasets
generate_test_data_for_re <- function(n_time = 8, n_sites = 3, n_plots = 2, 
                                      n_observers = 2, add_covariates = TRUE) {
  
  # Univariate dataset with nested/crossed structure
  univariate <- expand.grid(
    time = 1:n_time,
    site = factor(1:n_sites),
    plot = factor(1:(n_sites * n_plots)),  # Nested: plots within sites
    observer = factor(1:n_observers),      # Crossed with sites
    KEEP.OUT.ATTRS = FALSE
  )
  
  n_obs <- nrow(univariate)
  
  # Add continuous covariates
  if (add_covariates) {
    univariate$x <- rnorm(n_obs)
    univariate$temperature <- rnorm(n_obs, mean = 15, sd = 5)
  }
  
  # Generate realistic response data
  # Base trend + site effects + plot effects + noise
  site_effects <- rep(rnorm(n_sites, 0, 0.5), each = n_time * n_plots * n_observers)
  plot_effects <- rep(rnorm(n_sites * n_plots, 0, 0.3), each = n_time * n_observers)
  base_trend <- rep(sin(2 * pi * (1:n_time) / n_time), 
                    times = n_sites * n_plots * n_observers)
  
  linear_pred <- 2 + base_trend + site_effects + plot_effects
  if (add_covariates) {
    linear_pred <- linear_pred + 0.3 * univariate$x
  }
  
  univariate$y <- rpois(n_obs, exp(linear_pred))
  
  # Multivariate dataset (2 responses)
  multivariate <- univariate
  multivariate$count <- univariate$y
  multivariate$biomass <- rnorm(n_obs, mean = linear_pred * 2, sd = 1)
  multivariate$y <- NULL  # Remove single response
  
  # Add series column for trend models
  univariate$series <- factor(rep(1:n_sites, each = n_time * n_plots * n_observers))
  multivariate$series <- univariate$series
  
  return(list(
    univariate = univariate,
    multivariate = multivariate,
    metadata = list(
      n_time = n_time,
      n_sites = n_sites, 
      n_plots = n_plots,
      n_observers = n_observers,
      total_obs = n_obs
    )
  ))
}

# ============================================================================
# TESTING FRAMEWORK: Random Effects Pattern Testing
# ============================================================================

#' Test Random Effects Pattern with mvgam
#'
#' Tests a specific random effects pattern by attempting Stan code generation
#' and compilation. Records success/failure with detailed diagnostics.
#'
#' @param formula_spec List with 'obs' and 'trend' formula specifications
#' @param test_data Data frame for testing
#' @param family Distribution family (default: poisson())
#' @param test_name Descriptive name for this test
#' @param validate_stan Use rstan::stanc() validation (default: TRUE)
#'
#' @return List with test results and diagnostics
test_re_pattern <- function(formula_spec, test_data, family = poisson(), 
                           test_name = "unknown", validate_stan = TRUE) {
  
  cat("\n", rep("=", 60), "\n", sep = "")
  cat("TESTING:", test_name, "\n")
  cat("Formula:", deparse(formula_spec$obs), "\n")
  cat("Trend  :", deparse(formula_spec$trend), "\n")
  cat(rep("-", 60), "\n", sep = "")
  
  result <- list(
    test_name = test_name,
    formula_obs = deparse(formula_spec$obs),
    formula_trend = deparse(formula_spec$trend),
    success = FALSE,
    stage_reached = "initialization",
    error_message = NULL,
    error_class = NULL,
    warnings = character(0),
    stancode_length = NA,
    compile_time = NA
  )
  
  # Start timing
  start_time <- Sys.time()
  
  tryCatch({
    # Stage 1: Create mvgam_formula
    result$stage_reached <- "formula_creation"
    cat("Stage 1: Creating mvgam_formula...\n")
    
    if (is.null(formula_spec$trend)) {
      mf <- mvgam_formula(formula_spec$obs)
    } else {
      mf <- mvgam_formula(
        formula = formula_spec$obs,
        trend_formula = formula_spec$trend
      )
    }
    
    # Stage 2: Generate Stan code
    result$stage_reached <- "stancode_generation"
    cat("Stage 2: Generating Stan code...\n")
    
    # Capture warnings during stancode generation
    warnings_captured <- NULL
    stancode_result <- withCallingHandlers({
      stancode(mf, data = test_data, family = family, validate = validate_stan)
    }, warning = function(w) {
      warnings_captured <<- c(warnings_captured, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
    
    result$warnings <- warnings_captured
    result$stancode_length <- nchar(paste(stancode_result, collapse = "\n"))
    result$compile_time <- as.numeric(Sys.time() - start_time, units = "secs")
    
    # Stage 3: Success
    result$stage_reached <- "success"
    result$success <- TRUE
    
    cat("✓ SUCCESS: Stan code generated and compiled\n")
    cat("  Code length:", result$stancode_length, "characters\n")
    cat("  Compile time:", round(result$compile_time, 3), "seconds\n")
    
    if (length(result$warnings) > 0) {
      cat("  Warnings (", length(result$warnings), "):\n")
      for (w in result$warnings) {
        cat("    -", w, "\n")
      }
    }
    
  }, error = function(e) {
    result$error_message <<- conditionMessage(e)
    result$error_class <<- class(e)
    result$compile_time <<- as.numeric(Sys.time() - start_time, units = "secs")
    
    cat("✗ FAILED at stage:", result$stage_reached, "\n")
    cat("  Error class:", paste(result$error_class, collapse = ", "), "\n")
    cat("  Error message:", result$error_message, "\n")
    cat("  Time to failure:", round(result$compile_time, 3), "seconds\n")
  })
  
  return(result)
}

#' Run Comprehensive Random Effects Test Suite
#'
#' Tests systematic combinations of random effects patterns with different
#' trend formulas to map out working vs problematic combinations.
#'
#' @param test_data_list Output from generate_test_data_for_re()
#' @param include_multivariate Test multivariate patterns (default: FALSE)
#' @param validate_stan Use Stan validation (default: TRUE)
#'
#' @return Data frame with all test results
run_re_test_suite <- function(test_data_list, include_multivariate = FALSE, 
                             validate_stan = TRUE) {
  
  cat("\n", rep("=", 80), "\n", sep = "")
  cat("COMPREHENSIVE RANDOM EFFECTS TESTING SUITE\n")
  cat("Data: ", test_data_list$metadata$total_obs, " observations, ",
      test_data_list$metadata$n_sites, " sites, ",
      test_data_list$metadata$n_plots, " plots\n")
  cat("Validation: ", ifelse(validate_stan, "rstan::stanc() ENABLED", "DISABLED"), "\n")
  cat(rep("=", 80), "\n", sep = "")
  
  test_data <- test_data_list$univariate
  
  # Define test cases systematically
  test_cases <- list(
    
    # ========================================================================
    # BASELINE: No Random Effects
    # ========================================================================
    list(
      name = "baseline_no_re_no_trend",
      obs = y ~ x,
      trend = NULL
    ),
    list(
      name = "baseline_no_re_with_trend",
      obs = y ~ x,
      trend = ~ RW()
    ),
    list(
      name = "baseline_no_re_car_trend", 
      obs = y ~ x,
      trend = ~ CAR()
    ),
    
    # ========================================================================
    # SIMPLE RANDOM INTERCEPTS
    # ========================================================================
    list(
      name = "simple_intercept_site_no_trend",
      obs = y ~ x + (1 | site),
      trend = NULL
    ),
    list(
      name = "simple_intercept_site_rw_trend",
      obs = y ~ x + (1 | site),
      trend = ~ RW()
    ),
    list(
      name = "simple_intercept_site_car_trend",
      obs = y ~ x + (1 | site),
      trend = ~ CAR()
    ),
    list(
      name = "simple_intercept_plot_rw_trend",
      obs = y ~ x + (1 | plot),
      trend = ~ RW()
    ),
    list(
      name = "simple_intercept_observer_rw_trend",
      obs = y ~ x + (1 | observer),
      trend = ~ RW()
    ),
    
    # ========================================================================
    # RANDOM SLOPES
    # ========================================================================
    list(
      name = "random_slope_site_no_trend",
      obs = y ~ x + (x | site),
      trend = NULL
    ),
    list(
      name = "random_slope_site_rw_trend",
      obs = y ~ x + (x | site),
      trend = ~ RW()
    ),
    list(
      name = "random_slope_site_car_trend",
      obs = y ~ x + (x | site),
      trend = ~ CAR()
    ),
    list(
      name = "random_slope_temperature_site",
      obs = y ~ temperature + (temperature | site),
      trend = ~ RW()
    ),
    
    # ========================================================================
    # NESTED RANDOM EFFECTS
    # ========================================================================
    list(
      name = "nested_plot_in_site_no_trend",
      obs = y ~ x + (1 | site) + (1 | plot),
      trend = NULL
    ),
    list(
      name = "nested_plot_in_site_rw_trend",
      obs = y ~ x + (1 | site) + (1 | plot),
      trend = ~ RW()
    ),
    list(
      name = "nested_plot_in_site_car_trend",
      obs = y ~ x + (1 | site) + (1 | plot),
      trend = ~ CAR()
    ),
    list(
      name = "nested_slope_plot_in_site",
      obs = y ~ x + (x | site) + (1 | plot),
      trend = ~ RW()
    ),
    
    # ========================================================================
    # CROSSED RANDOM EFFECTS  
    # ========================================================================
    list(
      name = "crossed_site_observer_no_trend",
      obs = y ~ x + (1 | site) + (1 | observer),
      trend = NULL
    ),
    list(
      name = "crossed_site_observer_rw_trend",
      obs = y ~ x + (1 | site) + (1 | observer),
      trend = ~ RW()
    ),
    list(
      name = "crossed_site_observer_car_trend",
      obs = y ~ x + (1 | site) + (1 | observer),
      trend = ~ CAR()
    ),
    list(
      name = "crossed_complex_slopes",
      obs = y ~ x + (x | site) + (x | observer),
      trend = ~ RW()
    ),
    
    # ========================================================================
    # COMPLEX COMBINATIONS
    # ========================================================================
    list(
      name = "complex_nested_crossed",
      obs = y ~ x + (1 | site) + (1 | plot) + (1 | observer),
      trend = ~ RW()
    ),
    list(
      name = "complex_mixed_slopes",
      obs = y ~ x + temperature + (x | site) + (temperature | observer),
      trend = ~ RW()
    ),
    list(
      name = "complex_all_combinations",
      obs = y ~ x + temperature + (x + temperature | site) + (1 | plot) + (1 | observer),
      trend = ~ CAR()
    ),
    
    # ========================================================================
    # TREND FORMULA RANDOM EFFECTS (if supported)
    # ========================================================================
    list(
      name = "trend_re_simple",
      obs = y ~ x,
      trend = ~ x + (1 | site)
    ),
    list(
      name = "trend_re_with_car",
      obs = y ~ x,
      trend = ~ x + (1 | site) + CAR()
    ),
    list(
      name = "both_obs_trend_re",
      obs = y ~ x + (1 | site),
      trend = ~ temperature + (1 | plot) + RW()
    )
  )
  
  # ========================================================================
  # MULTIVARIATE TESTS (if requested)
  # ========================================================================
  if (include_multivariate) {
    mv_test_data <- test_data_list$multivariate
    
    mv_cases <- list(
      list(
        name = "mv_baseline",
        obs = mvbind(count, biomass) ~ x,
        trend = ~ RW(),
        data = mv_test_data
      ),
      list(
        name = "mv_simple_re",
        obs = mvbind(count, biomass) ~ x + (1 | site),
        trend = ~ RW(),
        data = mv_test_data
      ),
      list(
        name = "mv_complex_re",
        obs = mvbind(count, biomass) ~ x + (x | site) + (1 | observer),
        trend = ~ CAR(),
        data = mv_test_data
      )
    )
    
    test_cases <- c(test_cases, mv_cases)
  }
  
  # ========================================================================
  # RUN ALL TESTS
  # ========================================================================
  
  cat("\nRunning", length(test_cases), "test cases...\n")
  
  results <- vector("list", length(test_cases))
  
  for (i in seq_along(test_cases)) {
    case <- test_cases[[i]]
    
    # Use appropriate test data
    current_data <- if ("data" %in% names(case)) case$data else test_data
    
    results[[i]] <- test_re_pattern(
      formula_spec = list(obs = case$obs, trend = case$trend),
      test_data = current_data,
      family = poisson(),
      test_name = case$name,
      validate_stan = validate_stan
    )
    
    # Brief progress indicator
    if (results[[i]]$success) {
      cat("  [", i, "/", length(test_cases), "] ✓ ", case$name, "\n")
    } else {
      cat("  [", i, "/", length(test_cases), "] ✗ ", case$name, "\n")
    }
  }
  
  # ========================================================================
  # COMPILE RESULTS
  # ========================================================================
  
  results_df <- do.call(rbind, lapply(results, function(r) {
    data.frame(
      test_name = r$test_name,
      success = r$success,
      stage_reached = r$stage_reached,
      error_class = paste(r$error_class, collapse = ","),
      stancode_length = r$stancode_length,
      compile_time = r$compile_time,
      n_warnings = length(r$warnings),
      stringsAsFactors = FALSE
    )
  }))
  
  # Summary statistics
  cat("\n", rep("=", 80), "\n", sep = "")
  cat("TEST SUITE SUMMARY\n")
  cat(rep("=", 80), "\n", sep = "")
  cat("Total tests:", nrow(results_df), "\n")
  cat("Successful:", sum(results_df$success), "\n")
  cat("Failed:    ", sum(!results_df$success), "\n")
  cat("Success rate:", round(100 * mean(results_df$success), 1), "%\n")
  
  if (any(results_df$success)) {
    successful <- results_df[results_df$success, ]
    cat("\nSuccessful compile times:\n")
    cat("  Mean:", round(mean(successful$compile_time, na.rm = TRUE), 3), "seconds\n")
    cat("  Range:", round(min(successful$compile_time, na.rm = TRUE), 3), "to",
        round(max(successful$compile_time, na.rm = TRUE), 3), "seconds\n")
  }
  
  if (any(!results_df$success)) {
    cat("\nFailure breakdown:\n")
    failure_stages <- table(results_df$stage_reached[!results_df$success])
    for (stage in names(failure_stages)) {
      cat("  ", stage, ":", failure_stages[stage], "\n")
    }
  }
  
  # Store detailed results for analysis
  attr(results_df, "detailed_results") <- results
  attr(results_df, "test_metadata") <- test_data_list$metadata
  
  return(results_df)
}

# ============================================================================
# EXECUTION: Run the Test Suite
# ============================================================================

if (interactive() || !exists(".testing_mode")) {
  cat("\n", rep("#", 80), "\n", sep = "")
  cat("# MVGAM RANDOM EFFECTS PATTERN TESTING\n")
  cat("# Generated:", Sys.time(), "\n")
  cat(rep("#", 80), "\n", sep = "")
  
  # Generate test data
  cat("\nGenerating test data...\n")
  test_data_list <- generate_test_data_for_re(
    n_time = 6,      # Smaller for faster testing
    n_sites = 3,
    n_plots = 2,
    n_observers = 2,
    add_covariates = TRUE
  )
  
  cat("Data generated:", test_data_list$metadata$total_obs, "observations\n")
  cat("Structure:")
  cat("  Sites:", test_data_list$metadata$n_sites)
  cat("  Plots per site:", test_data_list$metadata$n_plots)  
  cat("  Observers:", test_data_list$metadata$n_observers)
  cat("  Time points:", test_data_list$metadata$n_time, "\n")
  
  # Run comprehensive test suite
  cat("\nStarting comprehensive random effects testing...\n")
  
  results <- run_re_test_suite(
    test_data_list = test_data_list,
    include_multivariate = FALSE,  # Start with univariate
    validate_stan = TRUE           # Enable rstan::stanc() validation
  )
  
  # Save results for analysis
  output_file <- "tasks/random_effects_test_results.rds"
  saveRDS(results, output_file)
  cat("\nDetailed results saved to:", output_file, "\n")
  
  # Display final summary
  cat("\n", rep("#", 80), "\n", sep = "")
  cat("# TESTING COMPLETE\n")
  cat(rep("#", 80), "\n", sep = "")
  
  cat("\nFor detailed analysis, load results:\n")
  cat("  results <- readRDS('", output_file, "')\n")
  cat("  detailed <- attr(results, 'detailed_results')\n")
  cat("  View(results)  # Summary table\n")
  cat("\n")
}