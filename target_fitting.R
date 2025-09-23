# Enhanced Target Fitting Script
#
# This script attempts to compile and fit all 9 target Stan models 
# to validate the Stan code generation system
#
# Usage: source("target_fitting.R")
# Output: Comprehensive testing report logged to files

# Load required packages
suppressPackageStartupMessages({
  devtools::load_all()
  library(brms)
  library(cmdstanr)
})

# Setup logging
log_file <- "tasks/target_fitting_log.txt"
if (file.exists(log_file)) file.remove(log_file)

log_msg <- function(msg, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_line <- paste0("[", timestamp, "] ", level, ": ", msg)
  cat(log_line, "\n", file = log_file, append = TRUE)
  if (level %in% c("ERROR", "SUMMARY")) {
    cat(log_line, "\n")  # Also print errors and summaries to console
  }
}

# Helper function to test a single model
test_model_scenario <- function(scenario_num, description) {
  log_msg(paste("TESTING TARGET", scenario_num, ":", description))
  
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
    log_msg(paste("Stan file not found:", result$stan_file), "ERROR")
    result$compilation_error <- "Stan file not found"
    return(result)
  }
  
  if (!file.exists(result$data_file)) {
    log_msg(paste("Data file not found:", result$data_file), "ERROR")
    result$compilation_error <- "Data file not found"
    return(result)
  }
  
  log_msg("Input files found")
  
  # Attempt compilation
  log_msg("Attempting Stan model compilation...")
  compilation_start <- Sys.time()
  
  tryCatch({
    mod <- cmdstan_model(result$stan_file, quiet = TRUE)
    result$compilation_success <- TRUE
    result$timing$compilation <- as.numeric(difftime(Sys.time(), compilation_start, units = "secs"))
    log_msg(paste("Compilation successful (", round(result$timing$compilation, 2), "s)"))
  }, error = function(e) {
    result$compilation_success <<- FALSE
    result$compilation_error <<- conditionMessage(e)
    result$timing$compilation <<- as.numeric(difftime(Sys.time(), compilation_start, units = "secs"))
    log_msg(paste("Compilation failed:", conditionMessage(e)), "ERROR")
    return(result)
  })
  
  # If compilation failed, return early
  if (!result$compilation_success) {
    return(result)
  }
  
  # Load data
  log_msg("Loading Stan data...")
  tryCatch({
    data_list <- readRDS(result$data_file)
    log_msg("Data loaded successfully")
    log_msg(paste("Data elements:", length(data_list)))
    dims_info <- c()
    if("N" %in% names(data_list)) dims_info <- c(dims_info, paste("N =", data_list$N))
    if("n_time_trend" %in% names(data_list)) dims_info <- c(dims_info, paste("n_time_trend =", data_list$n_time_trend))
    if("n_series_trend" %in% names(data_list)) dims_info <- c(dims_info, paste("n_series_trend =", data_list$n_series_trend))
    if(length(dims_info) > 0) {
      log_msg(paste("Key dimensions:", paste(dims_info, collapse = ", ")))
    }
  }, error = function(e) {
    log_msg(paste("Error loading data:", conditionMessage(e)), "ERROR")
    result$fitting_error <<- paste("Data loading error:", conditionMessage(e))
    return(result)
  })
  
  # Attempt fitting with short chains
  log_msg("Attempting model fitting (short chains for testing)...")
  fitting_start <- Sys.time()
  
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
        log_msg(paste("Fitting successful (", round(result$timing$fitting, 2), "s)"))
        
        if ("num_divergent" %in% names(diagnostic_summary)) {
          div_count <- sum(diagnostic_summary$num_divergent, na.rm = TRUE)
          log_msg(paste("Divergent transitions:", div_count))
        }
        if ("num_max_treedepth" %in% names(diagnostic_summary)) {
          tree_count <- sum(diagnostic_summary$num_max_treedepth, na.rm = TRUE)
          log_msg(paste("Max treedepth hits:", tree_count))
        }
      }
      
    }, error = function(e) {
      result$fitting_success <<- FALSE
      result$fitting_error <<- conditionMessage(e)
      result$timing$fitting <<- as.numeric(difftime(Sys.time(), fitting_start, units = "secs"))
      log_msg(paste("Fitting failed:", conditionMessage(e)), "ERROR")
    })
  }, warning = function(w) {
    warnings_list <<- c(warnings_list, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  
  # Record warnings
  if (length(warnings_list) > 0) {
    result$fitting_warnings <- warnings_list
    log_msg(paste("Warnings during fitting:", length(warnings_list), "warnings"))
    for (i in seq_along(warnings_list)) {
      log_msg(paste("Warning", i, ":", warnings_list[i]))
    }
  }
  
  return(result)
}

# Main testing workflow
log_msg("MVGAM TARGET FITTING VALIDATION", "SUMMARY")
log_msg("Starting comprehensive testing of all target scenarios...", "SUMMARY")
log_msg(paste("Test timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), "SUMMARY")

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
log_msg("COMPREHENSIVE TESTING SUMMARY", "SUMMARY")
log_msg(paste("Total testing time:", round(total_time, 2), "seconds"), "SUMMARY")

# Summary statistics
compilation_successes <- sum(sapply(all_results, function(x) x$compilation_success))
fitting_successes <- sum(sapply(all_results, function(x) x$fitting_success))
total_scenarios <- length(all_results)

log_msg("OVERALL RESULTS:", "SUMMARY")
log_msg(paste("Compilation success rate:", compilation_successes, "/", total_scenarios, 
    "(", round(100 * compilation_successes / total_scenarios, 1), "%)"), "SUMMARY")
log_msg(paste("Fitting success rate:", fitting_successes, "/", total_scenarios, 
    "(", round(100 * fitting_successes / total_scenarios, 1), "%)"), "SUMMARY")

# Detailed scenario breakdown
log_msg("DETAILED SCENARIO BREAKDOWN:", "SUMMARY")

for (result in all_results) {
  compilation_status <- if(result$compilation_success) "SUCCESS" else "FAILED"
  fitting_status <- if(result$fitting_success) "SUCCESS" else if(!result$compilation_success) "SKIPPED" else "FAILED"
  
  notes <- character(0)
  if (!is.null(result$compilation_error)) {
    notes <- c(notes, paste("Comp error:", substr(result$compilation_error, 1, 50)))
  }
  if (!is.null(result$fitting_error)) {
    notes <- c(notes, paste("Fit error:", substr(result$fitting_error, 1, 50)))
  }
  if (!is.null(result$fitting_warnings) && length(result$fitting_warnings) > 0) {
    notes <- c(notes, paste(length(result$fitting_warnings), "warnings"))
  }
  
  notes_text <- if(length(notes) > 0) paste(notes, collapse = "; ") else "OK"
  
  log_msg(paste("Scenario", result$scenario, ":", result$description, "| Compilation:", compilation_status, 
               "| Fitting:", fitting_status, "| Notes:", notes_text), "SUMMARY")
}

# Failed scenarios details
failed_scenarios <- all_results[sapply(all_results, function(x) !x$compilation_success || !x$fitting_success)]

if (length(failed_scenarios) > 0) {
  log_msg("FAILED SCENARIOS DETAILED ERRORS:", "SUMMARY")
  
  for (result in failed_scenarios) {
    log_msg(paste("SCENARIO", result$scenario, ":", result$description), "ERROR")
    if (!result$compilation_success && !is.null(result$compilation_error)) {
      log_msg(paste("  Compilation Error:", result$compilation_error), "ERROR")
    }
    if (!result$fitting_success && !is.null(result$fitting_error)) {
      log_msg(paste("  Fitting Error:", result$fitting_error), "ERROR")
    }
    if (!is.null(result$fitting_warnings) && length(result$fitting_warnings) > 0) {
      log_msg("  Fitting Warnings:", "ERROR")
      for (i in seq_along(result$fitting_warnings)) {
        log_msg(paste("    ", i, ":", result$fitting_warnings[i]), "ERROR")
      }
    }
  }
}

# Performance summary
if (compilation_successes > 0) {
  compilation_times <- sapply(all_results[sapply(all_results, function(x) x$compilation_success)], 
                             function(x) x$timing$compilation)
  log_msg("PERFORMANCE SUMMARY:", "SUMMARY")
  log_msg(paste("Average compilation time:", round(mean(compilation_times), 2), "seconds"), "SUMMARY")
  log_msg(paste("Range:", round(min(compilation_times), 2), "-", round(max(compilation_times), 2), "seconds"), "SUMMARY")
  
  if (fitting_successes > 0) {
    fitting_times <- sapply(all_results[sapply(all_results, function(x) x$fitting_success)], 
                           function(x) x$timing$fitting)
    log_msg(paste("Average fitting time:", round(mean(fitting_times), 2), "seconds"), "SUMMARY")
    log_msg(paste("Range:", round(min(fitting_times), 2), "-", round(max(fitting_times), 2), "seconds"), "SUMMARY")
  }
}

log_msg("TESTING COMPLETE", "SUMMARY")

# Save results for further analysis
saveRDS(all_results, "tasks/target_fitting_results.rds")
log_msg("Detailed results saved to: tasks/target_fitting_results.rds", "SUMMARY")