# Enhanced Target Fitting Script
#
# This script attempts to compile and fit all 9 target Stan models 
# to validate the Stan code generation system
#
# Usage: source("target_fitting.R")
# Output: Comprehensive testing report for all target scenarios

# Load required packages
devtools::load_all()
library(brms)
library(cmdstanr)

# Helper function to test a single model
test_model_scenario <- function(scenario_num, description) {
  cat("\n", strrep("=", 60), "\n")
  cat("TESTING TARGET", scenario_num, ":", description, "\n")
  cat(strrep("=", 60), "\n")
  
  # Initialize result tracking
  result <- list(
    scenario = scenario_num,
    description = description,
    stan_file = paste0("tasks/current_stancode_", scenario_num, ".stan"),
    data_file = paste0("tasks/current_standata_", scenario_num, ".rds"),
    compilation_success = FALSE,
    compilation_error = NULL,
    fitting_success = FALSE,
    fitting_error = NULL,
    fitting_warnings = NULL,
    timing = list()
  )
  
  # Check if files exist
  if (!file.exists(result$stan_file)) {
    cat("✗ Stan file not found:", result$stan_file, "\n")
    result$compilation_error <- "Stan file not found"
    return(result)
  }
  
  if (!file.exists(result$data_file)) {
    cat("✗ Data file not found:", result$data_file, "\n")
    result$compilation_error <- "Data file not found"
    return(result)
  }
  
  cat("✓ Input files found\n")
  
  # Attempt compilation
  cat("→ Attempting Stan model compilation...\n")
  compilation_start <- Sys.time()
  
  tryCatch({
    mod <- cmdstan_model(result$stan_file)
    result$compilation_success <- TRUE
    result$timing$compilation <- as.numeric(difftime(Sys.time(), compilation_start, units = "secs"))
    cat("✓ Compilation successful (", round(result$timing$compilation, 2), "s)\n")
  }, error = function(e) {
    result$compilation_success <<- FALSE
    result$compilation_error <<- conditionMessage(e)
    result$timing$compilation <<- as.numeric(difftime(Sys.time(), compilation_start, units = "secs"))
    cat("✗ Compilation failed:", conditionMessage(e), "\n")
    return(result)
  })
  
  # If compilation failed, return early
  if (!result$compilation_success) {
    return(result)
  }
  
  # Load data
  cat("→ Loading Stan data...\n")
  tryCatch({
    data_list <- readRDS(result$data_file)
    cat("✓ Data loaded successfully\n")
    cat("  Data elements:", length(data_list), "\n")
    cat("  Key dimensions:", 
        if("N" %in% names(data_list)) paste("N =", data_list$N) else "N not found",
        if("n_time_trend" %in% names(data_list)) paste(", n_time_trend =", data_list$n_time_trend) else "",
        if("n_series_trend" %in% names(data_list)) paste(", n_series_trend =", data_list$n_series_trend) else "",
        "\n")
  }, error = function(e) {
    cat("✗ Error loading data:", conditionMessage(e), "\n")
    result$fitting_error <<- paste("Data loading error:", conditionMessage(e))
    return(result)
  })
  
  # Attempt fitting with short chains
  cat("→ Attempting model fitting (short chains for testing)...\n")
  fitting_start <- Sys.time()
  
  # Set a fitting timeout of 30 seconds
  fitting_timeout <- 30
  
  # Capture warnings during fitting
  warnings_list <- character(0)
  withCallingHandlers({
    tryCatch({
      fit <- mod$sample(
        data = data_list,
        seed = 123,
        iter_warmup = 50,
        iter_sampling = 50,
        chains = 1,
        refresh = 0,  # Suppress iteration output
        show_messages = FALSE,
        max_treedepth = 8,  # Limit tree depth for speed
        adapt_delta = 0.8   # Lower adapt_delta for speed
      )
      
      result$fitting_success <- TRUE
      result$timing$fitting <- as.numeric(difftime(Sys.time(), fitting_start, units = "secs"))
      
      # Get basic diagnostics
      if (fit$metadata()$stan_version_major >= 2) {
        diagnostic_summary <- fit$diagnostic_summary()
        cat("✓ Fitting successful (", round(result$timing$fitting, 2), "s)\n")
        cat("  Diagnostic summary:\n")
        if ("num_divergent" %in% names(diagnostic_summary)) {
          cat("    Divergent transitions:", sum(diagnostic_summary$num_divergent, na.rm = TRUE), "\n")
        }
        if ("num_max_treedepth" %in% names(diagnostic_summary)) {
          cat("    Max treedepth hits:", sum(diagnostic_summary$num_max_treedepth, na.rm = TRUE), "\n")
        }
      }
      
    }, error = function(e) {
      result$fitting_success <<- FALSE
      result$fitting_error <<- conditionMessage(e)
      result$timing$fitting <<- as.numeric(difftime(Sys.time(), fitting_start, units = "secs"))
      cat("✗ Fitting failed:", conditionMessage(e), "\n")
    })
  }, warning = function(w) {
    warnings_list <<- c(warnings_list, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  
  # Record warnings
  if (length(warnings_list) > 0) {
    result$fitting_warnings <- warnings_list
    cat("⚠ Warnings during fitting:\n")
    for (i in seq_along(warnings_list)) {
      cat("   ", i, ":", warnings_list[i], "\n")
    }
  }
  
  return(result)
}

# Main testing workflow
cat("MVGAM TARGET FITTING VALIDATION\n")
cat("Starting comprehensive testing of all target scenarios...\n")
cat("Test timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

# Define all test scenarios
test_scenarios <- list(
  list(1, "RW trends (basic structure)"),
  list(2, "Shared RW trends (multivariate)"),
  list(3, "VARMA trends (complex functions)"),
  list(4, "Factor AR trends (Z matrix patterns)"),
  list(5, "PW trends (Prophet functions)"),
  list(6, "CAR trends (GP + irregular time)"),
  list(7, "CAR trends (monotonic + irregular time)"),
  list(8, "Seasonal AR trends"),
  list(9, "Nonlinear with AR trends")
)

# Run tests for all scenarios
all_results <- list()
total_start <- Sys.time()

for (scenario_info in test_scenarios) {
  scenario_num <- scenario_info[[1]]
  description <- scenario_info[[2]]
  
  result <- test_model_scenario(scenario_num, description)
  all_results[[scenario_num]] <- result
}

total_time <- as.numeric(difftime(Sys.time(), total_start, units = "secs"))

# Generate comprehensive summary report
cat("\n", strrep("=", 80), "\n")
cat("COMPREHENSIVE TESTING SUMMARY\n")
cat(strrep("=", 80), "\n")
cat("Total testing time:", round(total_time, 2), "seconds\n\n")

# Summary statistics
compilation_successes <- sum(sapply(all_results, function(x) x$compilation_success))
fitting_successes <- sum(sapply(all_results, function(x) x$fitting_success))
total_scenarios <- length(all_results)

cat("OVERALL RESULTS:\n")
cat("Compilation success rate:", compilation_successes, "/", total_scenarios, 
    "(", round(100 * compilation_successes / total_scenarios, 1), "%)\n")
cat("Fitting success rate:", fitting_successes, "/", total_scenarios, 
    "(", round(100 * fitting_successes / total_scenarios, 1), "%)\n\n")

# Detailed scenario breakdown
cat("DETAILED SCENARIO BREAKDOWN:\n")
cat(sprintf("%-4s %-35s %-12s %-12s %-s\n", "ID", "Description", "Compilation", "Fitting", "Notes"))
cat(strrep("-", 80), "\n")

for (result in all_results) {
  compilation_status <- if(result$compilation_success) "✓ SUCCESS" else "✗ FAILED"
  fitting_status <- if(result$fitting_success) "✓ SUCCESS" else if(!result$compilation_success) "—" else "✗ FAILED"
  
  notes <- character(0)
  if (!is.null(result$compilation_error)) {
    notes <- c(notes, paste("Comp error:", substr(result$compilation_error, 1, 30)))
  }
  if (!is.null(result$fitting_error)) {
    notes <- c(notes, paste("Fit error:", substr(result$fitting_error, 1, 30)))
  }
  if (!is.null(result$fitting_warnings) && length(result$fitting_warnings) > 0) {
    notes <- c(notes, paste(length(result$fitting_warnings), "warnings"))
  }
  
  notes_text <- if(length(notes) > 0) paste(notes, collapse = "; ") else ""
  
  cat(sprintf("%-4d %-35s %-12s %-12s %-s\n", 
              result$scenario, 
              substr(result$description, 1, 35),
              compilation_status, 
              fitting_status,
              substr(notes_text, 1, 50)))
}

# Failed scenarios details
failed_scenarios <- all_results[sapply(all_results, function(x) !x$compilation_success || !x$fitting_success)]

if (length(failed_scenarios) > 0) {
  cat("\n", strrep("-", 80), "\n")
  cat("FAILED SCENARIOS DETAILED ERRORS:\n")
  cat(strrep("-", 80), "\n")
  
  for (result in failed_scenarios) {
    cat("\nSCENARIO", result$scenario, ":", result$description, "\n")
    if (!result$compilation_success && !is.null(result$compilation_error)) {
      cat("  Compilation Error:", result$compilation_error, "\n")
    }
    if (!result$fitting_success && !is.null(result$fitting_error)) {
      cat("  Fitting Error:", result$fitting_error, "\n")
    }
    if (!is.null(result$fitting_warnings) && length(result$fitting_warnings) > 0) {
      cat("  Fitting Warnings:\n")
      for (i in seq_along(result$fitting_warnings)) {
        cat("    ", i, ":", result$fitting_warnings[i], "\n")
      }
    }
  }
}

# Performance summary
if (compilation_successes > 0) {
  compilation_times <- sapply(all_results[sapply(all_results, function(x) x$compilation_success)], 
                             function(x) x$timing$compilation)
  cat("\n", strrep("-", 80), "\n")
  cat("PERFORMANCE SUMMARY:\n")
  cat("Average compilation time:", round(mean(compilation_times), 2), "seconds\n")
  cat("Range:", round(min(compilation_times), 2), "-", round(max(compilation_times), 2), "seconds\n")
  
  if (fitting_successes > 0) {
    fitting_times <- sapply(all_results[sapply(all_results, function(x) x$fitting_success)], 
                           function(x) x$timing$fitting)
    cat("Average fitting time:", round(mean(fitting_times), 2), "seconds\n")
    cat("Range:", round(min(fitting_times), 2), "-", round(max(fitting_times), 2), "seconds\n")
  }
}

cat("\n", strrep("=", 80), "\n")
cat("TESTING COMPLETE\n")
cat(strrep("=", 80), "\n")

# Save results for further analysis
saveRDS(all_results, "tasks/target_fitting_results.rds")
cat("Detailed results saved to: tasks/target_fitting_results.rds\n")