#' Stan Parameter Block Misplacement Debugging Script
#'
#' @description
#' Comprehensive debugging script to isolate the Stan parameter block misplacement
#' issue in mvgam. The error shows parameter declarations like `vector<lower=0>[1] sigma_trend;`
#' appearing in the model block instead of parameters block.
#'
#' This script systematically tests each stage of the Stan code generation pipeline
#' to identify where the block misplacement occurs.
#'
#' @author mvgam development team
#' @date 2025-08-28

# Load required libraries
library(mvgam)
library(brms)
library(checkmate)

# =============================================================================
# DEBUGGING CONFIGURATION
# =============================================================================

# Enable detailed debugging output
debug_verbose <- TRUE
debug_save_intermediate <- TRUE  # Save intermediate Stan code to files

# Create debug output directory
debug_dir <- "debug_output"
if (!dir.exists(debug_dir)) {
  dir.create(debug_dir, recursive = TRUE)
}

# Helper function for debug logging
debug_log <- function(step, message, details = NULL) {
  if (debug_verbose) {
    cat("\n", rep("=", 60), "\n", sep = "")
    cat("STEP ", step, ": ", message, "\n", sep = "")
    cat(rep("=", 60), "\n", sep = "")
    
    if (!is.null(details)) {
      cat("DETAILS:\n")
      print(details)
    }
  }
}

# Helper function to save Stan code
save_stan_code <- function(code, filename, step_description = "") {
  if (debug_save_intermediate && !is.null(code) && is.character(code)) {
    filepath <- file.path(debug_dir, paste0(step_description, "_", filename))
    writeLines(code, filepath)
    cat("Saved Stan code to:", filepath, "\n")
  }
}

# Helper function to analyze Stan code structure
analyze_stan_structure <- function(stan_code, step_name) {
  if (is.null(stan_code) || !is.character(stan_code)) {
    cat("ERROR: Stan code is NULL or not character in step:", step_name, "\n")
    return(NULL)
  }
  
  # Split into lines
  lines <- unlist(strsplit(stan_code, "\n"))
  
  # Find blocks
  blocks <- list(
    functions = which(grepl("functions\\s*\\{", lines, perl = TRUE)),
    data = which(grepl("data\\s*\\{", lines, perl = TRUE)),
    transformed_data = which(grepl("transformed data\\s*\\{", lines, perl = TRUE)),
    parameters = which(grepl("parameters\\s*\\{", lines, perl = TRUE)),
    transformed_parameters = which(grepl("transformed parameters\\s*\\{", lines, perl = TRUE)),
    model = which(grepl("model\\s*\\{", lines, perl = TRUE)),
    generated_quantities = which(grepl("generated quantities\\s*\\{", lines, perl = TRUE))
  )
  
  cat("\n--- STAN CODE STRUCTURE ANALYSIS ---\n")
  cat("Step:", step_name, "\n")
  cat("Total lines:", length(lines), "\n")
  
  for (block_name in names(blocks)) {
    block_lines <- blocks[[block_name]]
    if (length(block_lines) > 0) {
      cat(sprintf("%s block: found at line(s) %s\n", block_name, paste(block_lines, collapse = ", ")))
    }
  }
  
  # Look for suspicious patterns - parameter declarations in wrong blocks
  suspicious_patterns <- c(
    "vector<lower=0>\\[.*\\]\\s+sigma_trend",
    "vector\\[.*\\]\\s+innovations_trend",
    "matrix\\[.*\\]\\s+innovations_trend"
  )
  
  cat("\n--- SUSPICIOUS PATTERN DETECTION ---\n")
  for (pattern in suspicious_patterns) {
    matches <- grep(pattern, lines, perl = TRUE, value = FALSE)
    if (length(matches) > 0) {
      cat("FOUND SUSPICIOUS PATTERN:", pattern, "\n")
      for (line_num in matches) {
        cat(sprintf("  Line %d: %s\n", line_num, trimws(lines[line_num])))
        
        # Determine which block this line is in
        in_block <- "unknown"
        for (block_name in names(blocks)) {
          block_starts <- blocks[[block_name]]
          if (length(block_starts) > 0) {
            for (start_line in block_starts) {
              # Find end of block by counting braces
              block_end <- find_block_end(lines, start_line)
              if (line_num > start_line && line_num <= block_end) {
                in_block <- block_name
                break
              }
            }
          }
        }
        cat(sprintf("    This line appears to be in: %s block\n", in_block))
      }
    }
  }
  
  return(blocks)
}

# Helper function to find block end by counting braces
find_block_end <- function(lines, start_line) {
  brace_count <- 0
  started <- FALSE
  
  for (i in start_line:length(lines)) {
    line <- lines[i]
    # Count opening braces
    open_braces <- lengths(regmatches(line, gregexpr("\\{", line)))
    # Count closing braces  
    close_braces <- lengths(regmatches(line, gregexpr("\\}", line)))
    
    brace_count <- brace_count + open_braces - close_braces
    
    if (open_braces > 0) started <- TRUE
    if (started && brace_count <= 0) return(i)
  }
  
  return(length(lines))
}

# Helper function to examine stanvar structure
analyze_stanvars <- function(stanvars, step_name) {
  cat("\n--- STANVARS ANALYSIS ---\n")
  cat("Step:", step_name, "\n")
  
  if (is.null(stanvars)) {
    cat("Stanvars: NULL\n")
    return(NULL)
  }
  
  cat("Stanvars class:", paste(class(stanvars), collapse = ", "), "\n")
  
  if (inherits(stanvars, "stanvar")) {
    # Single stanvar
    cat("Single stanvar detected\n")
    cat("  Name:", stanvars$name %||% "UNNAMED", "\n")
    cat("  Block:", stanvars$block %||% "UNSPECIFIED", "\n")
    if (!is.null(stanvars$scode)) {
      cat("  Code snippet (first 100 chars):", substr(stanvars$scode, 1, 100), "\n")
    }
  } else if (inherits(stanvars, "stanvars")) {
    # Multiple stanvars
    cat("Multiple stanvars detected:", length(stanvars), "items\n")
    for (i in seq_along(stanvars)) {
      sv <- stanvars[[i]]
      cat(sprintf("  [%d] Name: %s, Block: %s\n", 
                  i, 
                  sv$name %||% "UNNAMED", 
                  sv$block %||% "UNSPECIFIED"))
      if (!is.null(sv$scode) && nchar(sv$scode) > 0) {
        cat(sprintf("      Code: %s\n", substr(sv$scode, 1, 80)))
      }
    }
  } else {
    cat("Unknown stanvars type:", class(stanvars), "\n")
    str(stanvars)
  }
}

# =============================================================================
# STEP 1: TEST BASIC BRMS STANCODE GENERATION (BASELINE)
# =============================================================================

test_basic_brms_stancode <- function() {
  debug_log("1", "Testing basic brms stancode generation (baseline)")
  
  # Create minimal test data
  n <- 50
  test_data <- data.frame(
    y = rnorm(n),
    x = rnorm(n),
    time = 1:n,
    series = factor(rep(1, n))
  )
  
  cat("Test data created: n =", n, "\n")
  
  # Test basic brms formula without trends
  basic_formula <- brms::bf(y ~ x)
  
  cat("Testing basic brms formula:", format(basic_formula), "\n")
  
  tryCatch({
    # Generate basic brms stancode
    basic_stancode <- brms::make_stancode(
      formula = basic_formula,
      data = test_data,
      family = gaussian(),
      prior = brms::prior("normal(0,1)", class = "Intercept"),
      backend = "rstan",
      silent = 2
    )
    
    cat("Basic brms stancode generation: SUCCESS\n")
    save_stan_code(basic_stancode, "basic_brms_stancode.stan", "step1")
    
    # Analyze the baseline structure
    analyze_stan_structure(basic_stancode, "Basic brms baseline")
    
    return(list(success = TRUE, stancode = basic_stancode, data = test_data))
    
  }, error = function(e) {
    cat("Basic brms stancode generation: FAILED\n")
    cat("Error:", e$message, "\n")
    return(list(success = FALSE, error = e$message))
  })
}

# =============================================================================
# STEP 2: CREATE MINIMAL RW TREND STANVARS
# =============================================================================

test_rw_trend_stanvars_creation <- function() {
  debug_log("2", "Creating minimal RW trend stanvars and examining block specifications")
  
  # Create test specifications matching mvgam pattern
  trend_specs <- list(
    trend = "RW",
    n_lv = 1,
    ma = FALSE,
    shared_innovations = TRUE
  )
  
  data_info <- list(
    n_obs = 50,
    n_series = 1,
    n_time = 50,
    time_var = "time",
    series_var = "series",
    unique_times = 1:50,
    unique_series = 1
  )
  
  cat("Trend specs:", utils::str(trend_specs), "\n")
  cat("Data info:", utils::str(data_info), "\n")
  
  tryCatch({
    # Load all functions first to ensure trend generators are available
    devtools::load_all(".", quiet = TRUE)
    
    # Test if the RW trend generator exists
    if (!exists("generate_rw_trend_stanvars")) {
      cat("ERROR: generate_rw_trend_stanvars function not found\n")
      return(list(success = FALSE, error = "Function not found"))
    }
    
    cat("Found generate_rw_trend_stanvars function\n")
    
    # Generate RW trend stanvars
    rw_stanvars <- generate_rw_trend_stanvars(trend_specs, data_info)
    
    cat("RW stanvars generation: SUCCESS\n")
    
    # Analyze the generated stanvars
    analyze_stanvars(rw_stanvars, "RW trend stanvars")
    
    return(list(success = TRUE, stanvars = rw_stanvars, trend_specs = trend_specs, data_info = data_info))
    
  }, error = function(e) {
    cat("RW stanvars generation: FAILED\n")
    cat("Error:", e$message, "\n")
    cat("Traceback:\n")
    traceback()
    return(list(success = FALSE, error = e$message))
  })
}

# =============================================================================
# STEP 3: TEST BRMS MAKE_STANCODE WITH RW STANVARS
# =============================================================================

test_brms_with_rw_stanvars <- function(baseline_result, rw_result) {
  debug_log("3", "Testing brms::make_stancode() with RW stanvars")
  
  if (!baseline_result$success || !rw_result$success) {
    cat("Skipping step 3: Prerequisites failed\n")
    return(list(success = FALSE, error = "Prerequisites failed"))
  }
  
  test_data <- baseline_result$data
  rw_stanvars <- rw_result$stanvars
  
  # Create basic formula for testing with stanvars
  formula <- brms::bf(y ~ x)
  
  cat("Testing brms with RW stanvars...\n")
  
  tryCatch({
    # Generate stancode with RW stanvars
    combined_stancode <- brms::make_stancode(
      formula = formula,
      data = test_data,
      family = gaussian(),
      prior = brms::prior("normal(0,1)", class = "Intercept"),
      stanvars = rw_stanvars,
      backend = "rstan",
      silent = 2
    )
    
    cat("brms + RW stanvars generation: SUCCESS\n")
    save_stan_code(combined_stancode, "brms_plus_rw_stanvars.stan", "step3")
    
    # Analyze the combined structure
    analyze_stan_structure(combined_stancode, "brms + RW stanvars")
    
    return(list(success = TRUE, stancode = combined_stancode))
    
  }, error = function(e) {
    cat("brms + RW stanvars generation: FAILED\n")
    cat("Error:", e$message, "\n")
    return(list(success = FALSE, error = e$message))
  })
}

# =============================================================================
# STEP 4: EXAMINE GENERATED STAN CODE LINE BY LINE
# =============================================================================

test_detailed_stan_code_analysis <- function(combined_result) {
  debug_log("4", "Examining generated Stan code line by line for problematic sections")
  
  if (!combined_result$success) {
    cat("Skipping step 4: Combined stancode generation failed\n")
    return(list(success = FALSE, error = "Combined stancode generation failed"))
  }
  
  stan_code <- combined_result$stancode
  lines <- unlist(strsplit(stan_code, "\n"))
  
  cat("Detailed line-by-line analysis of", length(lines), "lines\n")
  
  # Look for the specific error pattern
  error_patterns <- list(
    sigma_trend_misplaced = "vector<lower=0>\\[.*\\]\\s+sigma_trend",
    innovations_misplaced = "matrix\\[.*\\].*innovations_trend",
    parameters_in_model = "vector\\[.*\\].*_trend.*;" # Generic parameter-like declarations
  )
  
  problematic_lines <- list()
  
  for (pattern_name in names(error_patterns)) {
    pattern <- error_patterns[[pattern_name]]
    matches <- grep(pattern, lines, perl = TRUE)
    
    if (length(matches) > 0) {
      cat("\nFOUND PATTERN:", pattern_name, "\n")
      problematic_lines[[pattern_name]] <- matches
      
      for (line_num in matches) {
        cat(sprintf("  Line %d: %s\n", line_num, trimws(lines[line_num])))
        
        # Show context around problematic line
        context_start <- max(1, line_num - 3)
        context_end <- min(length(lines), line_num + 3)
        
        cat("  Context:\n")
        for (i in context_start:context_end) {
          marker <- if (i == line_num) ">>> " else "    "
          cat(sprintf("%s%d: %s\n", marker, i, trimws(lines[i])))
        }
        cat("\n")
      }
    }
  }
  
  # Look for transitions between blocks to see if parameters leak
  block_transitions <- c()
  current_block <- "unknown"
  
  for (i in seq_along(lines)) {
    line <- trimws(lines[i])
    
    if (grepl("^(functions|data|transformed data|parameters|transformed parameters|model|generated quantities)\\s*\\{", line, perl = TRUE)) {
      block_match <- regmatches(line, regexpr("^\\w+(?:\\s+\\w+)?", line, perl = TRUE))
      new_block <- gsub("\\s+", "_", block_match)
      if (new_block != current_block) {
        block_transitions <- c(block_transitions, paste0("Line ", i, ": ", current_block, " -> ", new_block))
        current_block <- new_block
      }
    }
  }
  
  cat("\nBlock transitions detected:\n")
  for (transition in block_transitions) {
    cat(" ", transition, "\n")
  }
  
  return(list(
    success = TRUE, 
    problematic_lines = problematic_lines,
    block_transitions = block_transitions,
    total_lines = length(lines)
  ))
}

# =============================================================================
# STEP 5: TEST STANVAR COMBINATION PROCESS
# =============================================================================

test_stanvar_combination <- function(rw_result) {
  debug_log("5", "Testing stanvar combination process for corruption")
  
  if (!rw_result$success) {
    cat("Skipping step 5: RW stanvars not available\n")
    return(list(success = FALSE, error = "RW stanvars not available"))
  }
  
  rw_stanvars <- rw_result$stanvars
  
  tryCatch({
    # Test 1: Examine individual stanvars before combination
    cat("=== INDIVIDUAL STANVARS BEFORE COMBINATION ===\n")
    analyze_stanvars(rw_stanvars, "Individual RW stanvars")
    
    # Test 2: Create dummy observation stanvars
    obs_stanvar <- brms::stanvar(
      name = "dummy_obs",
      scode = "vector[N] dummy_obs;",
      block = "parameters"
    )
    
    cat("\n=== DUMMY OBSERVATION STANVAR ===\n")
    analyze_stanvars(obs_stanvar, "Dummy observation stanvar")
    
    # Test 3: Combine using brms c() method
    cat("\n=== COMBINING STANVARS ===\n")
    combined_stanvars <- c(obs_stanvar, rw_stanvars)
    
    cat("Combined successfully using c() method\n")
    analyze_stanvars(combined_stanvars, "Combined stanvars")
    
    # Test 4: Generate stancode from combined stanvars directly
    test_data <- data.frame(
      y = rnorm(10),
      x = rnorm(10)
    )
    
    cat("\n=== TESTING COMBINED STANVARS IN BRMS ===\n")
    combined_test_code <- brms::make_stancode(
      y ~ x,
      data = test_data,
      stanvars = combined_stanvars,
      backend = "rstan",
      silent = 2
    )
    
    save_stan_code(combined_test_code, "combined_stanvars_test.stan", "step5")
    analyze_stan_structure(combined_test_code, "Combined stanvars test")
    
    return(list(
      success = TRUE, 
      combined_stanvars = combined_stanvars,
      combined_stancode = combined_test_code
    ))
    
  }, error = function(e) {
    cat("Stanvar combination testing: FAILED\n")
    cat("Error:", e$message, "\n")
    return(list(success = FALSE, error = e$message))
  })
}

# =============================================================================
# MAIN DEBUGGING EXECUTION
# =============================================================================

run_full_debugging_pipeline <- function() {
  cat("Starting comprehensive Stan parameter block debugging pipeline\n")
  cat("Debug output will be saved to:", normalizePath(debug_dir), "\n\n")
  
  # Step 1: Baseline brms testing
  step1_result <- test_basic_brms_stancode()
  
  # Step 2: RW stanvars creation
  step2_result <- test_rw_trend_stanvars_creation()
  
  # Step 3: brms + RW stanvars
  step3_result <- test_brms_with_rw_stanvars(step1_result, step2_result)
  
  # Step 4: Detailed analysis
  step4_result <- test_detailed_stan_code_analysis(step3_result)
  
  # Step 5: Stanvar combination testing
  step5_result <- test_stanvar_combination(step2_result)
  
  # Summary report
  cat("\n", rep("=", 80), "\n", sep = "")
  cat("DEBUGGING PIPELINE SUMMARY REPORT\n")
  cat(rep("=", 80), "\n", sep = "")
  
  results <- list(
    "1. Basic brms baseline" = step1_result$success,
    "2. RW stanvars creation" = step2_result$success,
    "3. brms + RW stanvars" = step3_result$success,
    "4. Detailed code analysis" = step4_result$success,
    "5. Stanvar combination" = step5_result$success
  )
  
  for (step_name in names(results)) {
    status <- if (results[[step_name]]) "PASS" else "FAIL"
    cat(sprintf("%-30s: %s\n", step_name, status))
  }
  
  # Error summary
  if (step4_result$success && length(step4_result$problematic_lines) > 0) {
    cat("\nPROBLEMATIC PATTERNS DETECTED:\n")
    for (pattern_name in names(step4_result$problematic_lines)) {
      lines <- step4_result$problematic_lines[[pattern_name]]
      cat(sprintf("- %s: %d occurrence(s) at line(s) %s\n", 
                  pattern_name, 
                  length(lines), 
                  paste(lines, collapse = ", ")))
    }
  }
  
  cat("\nDEBUGGING COMPLETE\n")
  cat("Check the", debug_dir, "directory for saved Stan code files\n")
  
  # Return all results for further analysis
  return(list(
    step1 = step1_result,
    step2 = step2_result,
    step3 = step3_result,
    step4 = step4_result,
    step5 = step5_result,
    summary = results
  ))
}

# =============================================================================
# EXECUTION
# =============================================================================

# Run the debugging pipeline
if (interactive() || identical(Sys.getenv("RUN_DEBUGGING"), "TRUE")) {
  debug_results <- run_full_debugging_pipeline()
  
  # Additional helper: Quick function to re-run specific steps
  rerun_step <- function(step_number) {
    switch(step_number,
           "1" = test_basic_brms_stancode(),
           "2" = test_rw_trend_stanvars_creation(),
           "3" = test_brms_with_rw_stanvars(debug_results$step1, debug_results$step2),
           "4" = test_detailed_stan_code_analysis(debug_results$step3),
           "5" = test_stanvar_combination(debug_results$step2),
           stop("Invalid step number. Use 1-5.")
    )
  }
  
  cat("\nHelper function 'rerun_step(n)' available for re-running specific steps\n")
  cat("Example: rerun_step(2) to re-run RW stanvars creation\n")
}

#' Usage Instructions:
#' 
#' 1. Source this file: source("debug_stan_parameter_blocks.R")
#' 2. The pipeline will run automatically in interactive mode
#' 3. Check the debug_output/ directory for generated Stan code files
#' 4. Use rerun_step(n) to re-run specific steps if needed
#' 
#' The script will identify:
#' - Where parameter declarations are being misplaced
#' - Which step in the pipeline introduces the bug
#' - Whether it's a stanvar creation or brms processing issue
#' - Specific line numbers and context for problematic code