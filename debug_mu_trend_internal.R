#!/usr/bin/env Rscript
# COMPREHENSIVE debug mu_trend internal processing
# Traces EXACT execution flow through the mu_trend creation logic
# Shows intermediate states, actual function calls, and execution paths

devtools::load_all()

# Simple test data
test_data <- data.frame(
  y = rpois(20, 5),
  x = rnorm(20),
  z = rnorm(20),
  time = 1:20,
  series = factor(rep("A", 20))
)

# CRITICAL: Monkey patch the extract_and_rename_stan_blocks function to add tracing
# This lets us see exactly what happens inside the function when it runs
original_extract_and_rename_stan_blocks <- NULL

add_tracing_to_stan_blocks_function <- function() {
  cat("Installing tracing into extract_and_rename_stan_blocks function...\n")
  
  # Get the current function from the namespace
  ns <- asNamespace("mvgam")
  original_extract_and_rename_stan_blocks <<- ns$extract_and_rename_stan_blocks
  
  # Create traced version
  traced_function <- function(stancode, suffix, mapping, is_multivariate, response_names, standata = NULL) {
    cat(sprintf("\n=== INSIDE extract_and_rename_stan_blocks ===\n"))
    cat(sprintf("suffix: %s\n", suffix))
    cat(sprintf("is_multivariate: %s\n", is_multivariate))
    cat(sprintf("stancode length: %d characters\n", nchar(stancode)))
    
    # Show first few lines of input stancode
    stancode_lines <- strsplit(stancode, "\n")[[1]]
    cat("First 10 lines of input stancode:\n")
    for (i in 1:min(10, length(stancode_lines))) {
      cat(sprintf("  %d: %s\n", i, trimws(stancode_lines[i])))
    }
    
    # TRACE THE EXACT mu_trend_exists CHECK (lines 4930-4934)
    cat(sprintf("\n--- TRACING mu_trend_exists CHECK ---\n"))
    mu_trend_pattern <- paste0("vector\\[.*?\\]\\s+mu", gsub("_", "\\_", suffix, fixed=TRUE))
    cat(sprintf("mu_trend detection pattern: %s\n", mu_trend_pattern))
    
    mu_trend_exists <- any(grepl(mu_trend_pattern, stancode_lines, perl = TRUE))
    cat(sprintf("mu_trend_exists result: %s\n", mu_trend_exists))
    
    if (mu_trend_exists) {
      # Find and show the matching lines
      matching_lines <- grep(mu_trend_pattern, stancode_lines, perl = TRUE, value = TRUE)
      cat("Matching mu_trend lines found:\n")
      for (line in matching_lines) {
        cat(sprintf("  %s\n", trimws(line)))
      }
      cat("→ BRANCH: Will use rename logic (skipping mu_trend creation)\n")
    } else {
      cat("→ BRANCH: Will create new mu_trend\n")
      
      # TRACE THE COEFFICIENT DETECTION (lines 4939-4940)
      cat(sprintf("\n--- TRACING COEFFICIENT DETECTION ---\n"))
      has_b_coef <- grepl(paste0("vector\\[.*\\]\\s+b", suffix), stancode)
      has_x_matrix <- grepl(paste0("matrix\\[.*\\]\\s+X", suffix), stancode)
      has_coefficients <- has_b_coef && has_x_matrix
      
      cat(sprintf("b%s coefficient pattern search: %s\n", suffix, has_b_coef))
      cat(sprintf("X%s matrix pattern search: %s\n", suffix, has_x_matrix))
      cat(sprintf("has_coefficients result: %s\n", has_coefficients))
      
      # Show actual patterns found
      b_patterns <- grep(paste0("vector\\[.*\\]\\s+b", suffix), stancode_lines, value = TRUE)
      x_patterns <- grep(paste0("matrix\\[.*\\]\\s+X", suffix), stancode_lines, value = TRUE)
      
      if (length(b_patterns) > 0) {
        cat(sprintf("Found b%s patterns:\n", suffix))
        for (pattern in b_patterns) {
          cat(sprintf("  %s\n", trimws(pattern)))
        }
      }
      
      if (length(x_patterns) > 0) {
        cat(sprintf("Found X%s patterns:\n", suffix))
        for (pattern in x_patterns) {
          cat(sprintf("  %s\n", trimws(pattern)))
        }
      }
      
      # TRACE THE CONDITIONAL LOGIC PATH
      cat(sprintf("\n--- TRACING CONDITIONAL LOGIC PATH ---\n"))
      
      time_param <- paste0("N", suffix)
      
      if (has_coefficients) {
        cat(sprintf("→ ENTERING has_coefficients=TRUE branch\n"))
        
        # TRACE THE VALIDATION CHECKS (lines 4947-4955)
        cat(sprintf("--- TRACING VALIDATION CHECKS ---\n"))
        has_xc <- grepl(paste0("Xc", suffix), stancode) || 
                  grepl(paste0("matrix\\[.*\\]\\s+X", suffix), stancode)
        has_intercept <- grepl(paste0("real\\s+Intercept", suffix), stancode)
        
        cat(sprintf("Xc%s validation: %s\n", suffix, has_xc))
        cat(sprintf("Intercept%s validation: %s\n", suffix, has_intercept))
        
        if (!has_xc) {
          cat("*** VALIDATION FAILURE: !has_xc ***\n")
          cat("This will trigger insight::format_error() and likely cause function to fail\n")
          return(original_extract_and_rename_stan_blocks(stancode, suffix, mapping, is_multivariate, response_names, standata))
        } else {
          cat("✓ Validation passed, generating coefficients pattern\n")
        }
        
        # Generate the mu_trend_code (lines 4959-4962)
        mu_trend_code <- paste0(
          "vector[", time_param, "] mu", suffix, " = rep_vector(0.0, ", time_param, ");\n",
          "  mu", suffix, " += Intercept", suffix, " + Xc", suffix, " * b", suffix, ";"
        )
        
        cat(sprintf("Generated mu_trend_code:\n%s\n", mu_trend_code))
        
      } else {
        cat(sprintf("→ ENTERING has_coefficients=FALSE branch\n"))
        
        # TRACE INTERCEPT DETECTION (line 4966)
        intercept_present <- grepl("real.*Intercept[^_]", stancode)
        cat(sprintf("intercept_present (no suffix): %s\n", intercept_present))
        
        if (intercept_present) {
          cat(sprintf("→ ENTERING intercept_present=TRUE sub-branch\n"))
          mu_trend_code <- paste0(
            "vector[", time_param, "] mu", suffix, 
            " = rep_vector(Intercept", suffix, ", ", time_param, ");"
          )
        } else {
          cat(sprintf("→ ENTERING no_intercept sub-branch\n"))
          mu_trend_code <- paste0(
            "vector[", time_param, "] mu", suffix, 
            " = rep_vector(0.0, ", time_param, ");"
          )
        }
        
        cat(sprintf("Generated mu_trend_code:\n%s\n", mu_trend_code))
      }
      
      cat(sprintf("--- CREATING STANVAR ---\n"))
      cat(sprintf("About to create stanvar with mu_trend_code in 'tparameters' block\n"))
    }
    
    # Call the original function and trace its result
    cat(sprintf("\n--- CALLING ORIGINAL FUNCTION ---\n"))
    result <- original_extract_and_rename_stan_blocks(stancode, suffix, mapping, is_multivariate, response_names, standata)
    
    cat(sprintf("Function returned successfully\n"))
    cat(sprintf("Result has stanvars: %s\n", !is.null(result$stanvars)))
    cat(sprintf("Result has mapping: %s\n", !is.null(result$mapping)))
    
    if (!is.null(result$stanvars)) {
      stanvar_names <- names(result$stanvars)
      cat(sprintf("Stanvar names: %s\n", paste(stanvar_names, collapse=", ")))
      
      # Check for mu_trend creation stanvar
      mu_creation_stanvar <- result$stanvars[["trend_model_mu_creation"]]
      if (!is.null(mu_creation_stanvar)) {
        cat(sprintf("mu_trend creation stanvar exists with code:\n%s\n", mu_creation_stanvar$scode))
      } else {
        cat("No mu_trend creation stanvar found\n")
      }
    }
    
    cat(sprintf("=== END extract_and_rename_stan_blocks ===\n\n"))
    
    return(result)
  }
  
  # Replace the function in the namespace
  environment(traced_function) <- ns
  assignInNamespace("extract_and_rename_stan_blocks", traced_function, ns)
  
  cat("✓ Tracing installed\n")
}

remove_tracing_from_stan_blocks_function <- function() {
  if (!is.null(original_extract_and_rename_stan_blocks)) {
    ns <- asNamespace("mvgam")
    assignInNamespace("extract_and_rename_stan_blocks", original_extract_and_rename_stan_blocks, ns)
    cat("✓ Tracing removed\n")
  }
}

trace_mu_trend_processing <- function(formula, trend_formula, data, family = poisson()) {
  cat("=== COMPREHENSIVE MU_TREND INTERNAL PROCESSING ===\n")
  cat(sprintf("Formula: %s\n", deparse(formula)))
  cat(sprintf("Trend Formula: %s\n", deparse(trend_formula)))
  cat("\n")
  
  tryCatch({
    # STEP 1: Install tracing and generate stancode
    cat("STEP 1: Installing internal tracing and generating stancode...\n")
    add_tracing_to_stan_blocks_function()
    
    # Create mvgam_formula and generate stancode (this will trigger our tracing)
    mf <- mvgam_formula(formula, trend_formula = trend_formula)
    
    # This call will trigger our traced extract_and_rename_stan_blocks function
    final_stancode <- stancode(mf, data = data, family = family, validate = FALSE)
    
    cat("\nSTEP 2: Final stancode generated, checking result...\n")
    
    # Analyze final results to confirm what was actually generated
    final_lines <- strsplit(final_stancode, "\n")[[1]]
    mu_trend_init <- grep("mu.*_trend.*=.*rep_vector", final_lines, value = TRUE)
    mu_trend_add <- grep("mu.*_trend.*\\+=", final_lines, value = TRUE)
    
    cat("FINAL RESULT CONFIRMATION:\n")
    cat("mu_trend initialization in final code:\n")
    if (length(mu_trend_init) > 0) {
      for (line in mu_trend_init) {
        cat(sprintf("  %s\n", trimws(line)))
      }
    } else {
      cat("  NO mu_trend initialization found in final code\n")
    }
    
    cat("mu_trend additive components in final code:\n")
    if (length(mu_trend_add) > 0) {
      for (line in mu_trend_add) {
        cat(sprintf("  %s\n", trimws(line)))
      }
    } else {
      cat("  NO mu_trend additive components found in final code\n")
    }
    
    # Remove tracing
    remove_tracing_from_stan_blocks_function()
    
  }, error = function(e) {
    cat(sprintf("ERROR during comprehensive tracing: %s\n", e$message))
    remove_tracing_from_stan_blocks_function()  # Clean up even on error
  })
  
  cat(paste0("\n", paste(rep("=", 80), collapse=""), "\n\n"))
}

# Test the key scenarios with comprehensive tracing
cat("Testing key mu_trend scenarios with COMPREHENSIVE INTERNAL TRACING...\n")
cat("This will show the EXACT execution flow inside extract_and_rename_stan_blocks\n\n")

# Scenario 1: Pure intercept-only (should work correctly)
cat("SCENARIO 1: Intercept-only CAR trend\n")
trace_mu_trend_processing(y ~ x, ~ CAR(), test_data)

# Scenario 2: No-intercept with predictors (missing additive components bug)
cat("SCENARIO 2: No-intercept CAR with predictors\n")
trace_mu_trend_processing(y ~ x, ~ -1 + z + CAR(), test_data)

# Scenario 3: With intercept and predictors (wrong branch bug)
cat("SCENARIO 3: CAR with intercept and predictors\n") 
trace_mu_trend_processing(y ~ x, ~ z + CAR(), test_data)

cat("========================================\n")
cat("COMPREHENSIVE INTERNAL TRACING COMPLETE\n")
cat("========================================\n")
cat("This shows:\n")
cat("1. The EXACT intermediate Stan code when detection runs\n")
cat("2. The EXACT execution path through conditional logic\n") 
cat("3. The EXACT validation checks and their results\n")
cat("4. The EXACT mu_trend_code generation\n")
cat("5. The EXACT stanvar creation process\n")
cat("6. Any errors or exceptions during execution\n")
cat("\nThis is sufficient to identify the precise fixes needed.\n")