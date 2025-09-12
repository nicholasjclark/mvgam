#!/usr/bin/env Rscript
# TARGETED mu_trend Extraction Debugging Script
# Purpose: Precisely trace the mu_trend extraction issues in the mvgam pipeline

library(brms)
library(glue)

# Load all mvgam functions
devtools::load_all()

cat("===========================================\n")
cat("TARGETED MU_TREND EXTRACTION DEBUG\n") 
cat("===========================================\n\n")

# Global trace storage - use global assignment
mu_trend_trace <<- list(
  calls = list(),
  extractions = list(),
  creations = list(),
  errors = list()
)

# Trace helper with precise namespace targeting
trace_call <- function(func_name, details = NULL) {
  info <- list(
    timestamp = Sys.time(),
    function_name = func_name,
    details = details,
    call_depth = length(sys.calls())
  )
  if (exists("mu_trend_trace", envir = .GlobalEnv)) {
    .GlobalEnv$mu_trend_trace$calls <- append(.GlobalEnv$mu_trend_trace$calls, list(info))
  }
  cat(glue("🔍 [{Sys.time()}] {func_name}"), 
      if (!is.null(details)) glue(" - {details}"), "\n")
}

# CRITICAL: Properly patch the namespace functions
cat("📌 Patching mvgam namespace functions...\n")

# 1. Patch extract_and_rename_trend_parameters (the main entry point)
original_extract_and_rename_trend_parameters <- mvgam:::extract_and_rename_trend_parameters

new_extract_and_rename_trend_parameters <- function(trend_setup, dimensions, suffix = "_trend") {
  trace_call("extract_and_rename_trend_parameters", glue("suffix='{suffix}'"))
  
  cat("\n🎯 === MU_TREND EXTRACTION ENTRY POINT ===\n")
  
  # ENHANCED INPUT VALIDATION
  cat("\n📦 INPUT TYPE CHECKING:\n")
  cat(glue("  - trend_setup class: {paste(class(trend_setup), collapse=', ')}\n"))
  cat(glue("  - trend_setup names: {paste(names(trend_setup), collapse=', ')}\n"))
  
  # Check stancode specifically
  if (!is.null(trend_setup$stancode)) {
    cat(glue("  - stancode type: {paste(class(trend_setup$stancode), collapse=', ')}\n"))
    cat(glue("  - stancode length: {length(trend_setup$stancode)}\n"))
    if (!is.character(trend_setup$stancode)) {
      cat("  🚨 ERROR: stancode is NOT character type!\n")
      cat(glue("  - stancode content preview: {capture.output(str(trend_setup$stancode))[1]}\n"))
    }
  }
  
  # Check dimensions
  cat(glue("  - dimensions class: {paste(class(dimensions), collapse=', ')}\n"))
  if (!is.null(dimensions) && all(c("n_time", "n_series") %in% names(dimensions))) {
    cat(glue("  - Dimensions: n_time={dimensions$n_time}, n_series={dimensions$n_series}\n"))
  }
  
  cat(glue("  - suffix class: {paste(class(suffix), collapse=', ')}, value: '{suffix}'\n"))
  
  # Trace the stancode input if it's valid
  if (!is.null(trend_setup$stancode) && is.character(trend_setup$stancode)) {
    lines <- strsplit(trend_setup$stancode, "\n")[[1]]
    cat(glue("  - Input stancode: {length(lines)} lines\n"))
    
    # Look for existing mu patterns
    mu_lines <- grep("\\bmu\\b", lines, value = FALSE)
    if (length(mu_lines) > 0) {
      cat("  📍 Existing 'mu' references in input:\n")
      for (line_idx in head(mu_lines, 5)) {
        cat(glue("    Line {line_idx}: {lines[line_idx]}\n"))
      }
    }
    
    # Look for GP patterns
    gp_lines <- grep("gp_pred_|Xgp_|sdgp_|lscale_", lines, value = FALSE)
    if (length(gp_lines) > 0) {
      cat("  📍 GP patterns found:\n")
      for (line_idx in head(gp_lines, 3)) {
        cat(glue("    Line {line_idx}: {lines[line_idx]}\n"))
      }
    }
  }
  
  # Store extraction details
  if (exists("mu_trend_trace", envir = .GlobalEnv)) {
    .GlobalEnv$mu_trend_trace$extractions <- append(.GlobalEnv$mu_trend_trace$extractions, list(list(
      timestamp = Sys.time(),
      suffix = suffix,
      input_code_length = nchar(trend_setup$stancode %||% ""),
      dimensions = dimensions
    )))
  }
  
  tryCatch({
    cat("\n🔄 Calling original extract_and_rename_trend_parameters...\n")
    
    # Add debugging wrapper to capture exact error location
    withCallingHandlers({
      result <- original_extract_and_rename_trend_parameters(trend_setup, dimensions, suffix)
    }, error = function(e) {
      cat("\n🚨 ERROR LOCATION DETAILS:\n")
      cat(glue("  - Error in: {deparse(e$call)}\n"))
      cat(glue("  - Error message: {e$message}\n"))
      
      # Try to identify which argument caused the issue
      if (grepl("non-character argument", e$message)) {
        cat("\n  🔍 Checking for non-character arguments:\n")
        if (!is.character(suffix)) {
          cat(glue("    - suffix is not character: {class(suffix)}\n"))
        }
        if (exists("stancode", where = trend_setup) && !is.character(trend_setup$stancode)) {
          cat(glue("    - stancode is not character: {class(trend_setup$stancode)}\n"))
        }
      }
      
      # Enhanced debugging for variable mapping errors
      if (grepl("Variable mapping missing required variables", e$message)) {
        cat("\n  🔍 VARIABLE MAPPING ANALYSIS:\n")
        cat("    - This error occurs in the enhanced mu_trend system\n")
        cat("    - The system detected complex patterns but can't find required variables\n")
        cat("    - Need to check what variables are being requested vs what's available\n")
      }
    })
    
    cat("\n✅ extract_and_rename_trend_parameters completed successfully\n")
    if (!is.null(result) && inherits(result, "stanvars")) {
      cat(glue("  - Returned {length(result)} stanvars\n"))
      
      # Look for mu_trend stanvars specifically
      for (i in seq_along(result)) {
        sv <- result[[i]]
        if (!is.null(sv$scode) && grepl("mu.*trend", sv$scode)) {
          cat(glue("  📋 mu_trend stanvar found in [{i}]:\n"))
          cat(glue("    Block: {sv$block}\n"))
          cat(glue("    Code: {substr(sv$scode, 1, 100)}...\n"))
        }
      }
    }
    
    return(result)
    
  }, error = function(e) {
    cat(glue("\n❌ extract_and_rename_trend_parameters FAILED: {e$message}\n"))
    if (exists("mu_trend_trace", envir = .GlobalEnv)) {
      .GlobalEnv$mu_trend_trace$errors <- append(.GlobalEnv$mu_trend_trace$errors, list(list(
        function_name = "extract_and_rename_trend_parameters",
        error = e$message,
        timestamp = Sys.time()
      )))
    }
    stop(e)
  })
}

# Replace in namespace
assignInNamespace("extract_and_rename_trend_parameters", 
                  new_extract_and_rename_trend_parameters, 
                  ns = "mvgam")

# 2. Patch extract_and_rename_stan_blocks (the critical mu_trend creation logic)
original_extract_and_rename_stan_blocks <- mvgam:::extract_and_rename_stan_blocks

new_extract_and_rename_stan_blocks <- function(stancode, suffix, mapping, is_multivariate, response_names, standata = NULL) {
  trace_call("extract_and_rename_stan_blocks", glue("suffix='{suffix}', lines={length(strsplit(stancode, '\\n')[[1]])}"))
  
  cat("\n🚨 === CRITICAL MU_TREND CREATION LOGIC ===\n")
  cat(glue("  - Processing suffix: '{suffix}'\n"))
  cat(glue("  - Input code length: {nchar(stancode)} characters\n"))
  
  # Show initial mapping state
  cat("\n🗺️ INITIAL MAPPING STATE:\n")
  if (length(mapping$original_to_renamed) > 0) {
    cat(glue("  - Variables already mapped: {length(mapping$original_to_renamed)}\n"))
    cat(glue("  - Mapped variables: {paste(names(mapping$original_to_renamed), collapse=', ')}\n"))
  } else {
    cat("  - No variables mapped yet\n")
  }
  
  lines <- strsplit(stancode, "\n")[[1]]
  
  # REPRODUCE THE EXACT DETECTION LOGIC (lines 5126-5133 in original)
  mu_trend_pattern <- paste0("vector\\[.*?\\]\\s+mu", gsub("_", "\\_", suffix, fixed=TRUE))
  matches <- grepl(mu_trend_pattern, lines, perl = TRUE)
  mu_trend_exists <- any(matches)
  
  cat("\n🔍 MU_TREND DETECTION ANALYSIS:\n")
  cat(glue("  - Detection pattern: '{mu_trend_pattern}'\n"))
  cat(glue("  - mu_trend_exists: {mu_trend_exists}\n"))
  
  if (mu_trend_exists) {
    matching_lines <- which(matches)
    cat("  ✅ Found existing mu_trend:\n")
    for (line_idx in matching_lines) {
      cat(glue("    Line {line_idx}: {lines[line_idx]}\n"))
    }
  } else {
    cat("  🚨 No mu_trend found - will create simplified version\n")
    
    # REPRODUCE COEFFICIENT DETECTION (lines 5138-5139)
    has_coefficients <- grepl("vector\\[.*\\]\\s+b[^_]", stancode) && 
                        grepl("matrix\\[.*\\]\\s+X[^_]", stancode)
    
    cat(glue("  - has_coefficients: {has_coefficients}\n"))
    
    # Check for complex patterns that are missed
    gp_patterns <- grep("gp_pred_|Xgp_|sdgp_|lscale_|zgp_", lines)
    spline_patterns <- grep("Xs_\\d+_\\d+|s_\\d+_\\d+", lines)
    re_patterns <- grep("Z_\\d+_\\d+|r_\\d+_\\d+", lines)
    
    cat(glue("  - GP patterns found: {length(gp_patterns)} lines\n"))
    cat(glue("  - Spline patterns found: {length(spline_patterns)} lines\n"))
    cat(glue("  - Random effects patterns found: {length(re_patterns)} lines\n"))
    
    total_complex_patterns <- length(gp_patterns) + length(spline_patterns) + length(re_patterns)
    
    if (total_complex_patterns > 0) {
      cat("  📍 Complex patterns that should be integrated:\n")
      
      if (length(gp_patterns) > 0) {
        cat("    🔵 GP patterns:\n")
        for (line_idx in head(gp_patterns, 2)) {
          cat(glue("      Line {line_idx}: {lines[line_idx]}\n"))
        }
      }
      
      if (length(spline_patterns) > 0) {
        cat("    🟢 Spline patterns:\n")
        for (line_idx in head(spline_patterns, 2)) {
          cat(glue("      Line {line_idx}: {lines[line_idx]}\n"))
        }
      }
      
      if (length(re_patterns) > 0) {
        cat("    🟡 Random effects patterns:\n")
        for (line_idx in head(re_patterns, 2)) {
          cat(glue("      Line {line_idx}: {lines[line_idx]}\n"))
        }
      }
    }
    
    # Store creation details
    if (exists("mu_trend_trace", envir = .GlobalEnv)) {
      .GlobalEnv$mu_trend_trace$creations <- append(.GlobalEnv$mu_trend_trace$creations, list(list(
        timestamp = Sys.time(),
        suffix = suffix,
        mu_trend_exists = mu_trend_exists,
        has_coefficients = has_coefficients,
        gp_patterns = length(gp_patterns),
        spline_patterns = length(spline_patterns),
        re_patterns = length(re_patterns),
        total_complex_patterns = total_complex_patterns,
        will_create_simplified = TRUE
      )))
    }
  }
  
  tryCatch({
    # Show mapping state before calling original function
    cat("\n🔄 CALLING ORIGINAL FUNCTION...\n")
    
    # Call original function with enhanced error handling
    result <- withCallingHandlers({
      original_extract_and_rename_stan_blocks(stancode, suffix, mapping, is_multivariate, response_names, standata)
    }, error = function(e) {
      if (grepl("Variable mapping missing required variables", e$message)) {
        cat("\n🚨 VARIABLE MAPPING ERROR INTERCEPTED:\n")
        
        # Try to extract the enhanced mu_trend logic to see what variables it's looking for
        tryCatch({
          # This mimics the enhanced logic in extract_and_rename_stan_blocks
          mu_construction_result <- mvgam:::extract_mu_construction_from_model_block(stancode)
          if (length(mu_construction_result$mu_construction) > 0) {
            required_vars <- mu_construction_result$referenced_variables
            available_vars <- names(mapping$original_to_renamed)
            missing_vars <- setdiff(required_vars, available_vars)
            
            cat(glue("  - Required variables: {paste(required_vars, collapse=', ')}\n"))
            cat(glue("  - Available variables: {paste(available_vars, collapse=', ')}\n"))
            cat(glue("  - Missing variables: {paste(missing_vars, collapse=', ')}\n"))
            
            # Show mu construction patterns
            cat("\n  📋 MU CONSTRUCTION PATTERNS FOUND:\n")
            for (i in seq_along(mu_construction_result$mu_construction)) {
              cat(glue("    {i}. {mu_construction_result$mu_construction[i]}\n"))
            }
          }
        }, error = function(e2) {
          cat(glue("  - Could not analyze mu construction: {e2$message}\n"))
        })
      }
    })
    
    cat("\n✅ extract_and_rename_stan_blocks completed\n")
    if (!is.null(result$stanvars) && length(result$stanvars) > 0) {
      for (i in seq_along(result$stanvars)) {
        sv <- result$stanvars[[i]]
        if (!is.null(sv$scode) && grepl("mu.*=", sv$scode)) {
          cat(glue("  📋 mu creation stanvar [{i}]:\n"))
          cat(glue("    {sv$scode}\n"))
        }
      }
    }
    
    return(result)
    
  }, error = function(e) {
    cat(glue("\n❌ extract_and_rename_stan_blocks FAILED: {e$message}\n"))
    if (exists("mu_trend_trace", envir = .GlobalEnv)) {
      .GlobalEnv$mu_trend_trace$errors <- append(.GlobalEnv$mu_trend_trace$errors, list(list(
        function_name = "extract_and_rename_stan_blocks", 
        error = e$message,
        timestamp = Sys.time()
      )))
    }
    stop(e)
  })
}

# Replace in namespace
assignInNamespace("extract_and_rename_stan_blocks", 
                  new_extract_and_rename_stan_blocks, 
                  ns = "mvgam")

cat("✅ Namespace functions patched successfully\n\n")

# Test data setup
set.seed(123)
n_time <- 20
n_series <- 2
data <- expand.grid(
  time = 1:n_time,
  series = factor(1:n_series)
) 
data$x <- rnorm(nrow(data))
data$y <- rpois(nrow(data), lambda = exp(1 + 0.1 * data$x + rnorm(nrow(data), 0, 0.1)))

# TEST 1: Simple case (should work)
cat("🧪 TEST 1: Simple trend_formula = ~ -1\n")
cat("Expected: Basic mu_trend creation, no complex patterns\n\n")

tryCatch({
  # Clear trace
  if (exists("mu_trend_trace", envir = .GlobalEnv)) {
    .GlobalEnv$mu_trend_trace$calls <- list()
    .GlobalEnv$mu_trend_trace$extractions <- list()
    .GlobalEnv$mu_trend_trace$creations <- list()
  }
  
  code1 <- stancode(
    mvgam_formula(y ~ x, trend_formula = ~ -1),
    data = data,
    family = poisson(),
    validate = FALSE
  )
  
  cat("\n✅ TEST 1 SUCCESS\n")
  cat(glue("  - Code length: {nchar(code1)} chars\n"))
  cat(glue("  - Function calls: {length(.GlobalEnv$mu_trend_trace$calls)}\n"))
  cat(glue("  - Extractions: {length(.GlobalEnv$mu_trend_trace$extractions)}\n"))
  cat(glue("  - Creations: {length(.GlobalEnv$mu_trend_trace$creations)}\n"))
  
  writeLines(code1, "debug_simple_traced.stan")
  
}, error = function(e) {
  cat(glue("\n❌ TEST 1 FAILED: {e$message}\n"))
})

# TEST 2: Random effects case (should reveal the problem)
cat("\n\n🧪 TEST 2: Random effects trend_formula = ~ (1 | series)\n")
cat("Expected: Random effects patterns detected, complex mu_trend creation needed\n\n")

tryCatch({
  # Clear trace
  if (exists("mu_trend_trace", envir = .GlobalEnv)) {
    .GlobalEnv$mu_trend_trace$calls <- list()
    .GlobalEnv$mu_trend_trace$extractions <- list()
    .GlobalEnv$mu_trend_trace$creations <- list()
  }
  
  code2 <- stancode(
    mvgam_formula(y ~ 1, trend_formula = ~ (1 | series)),
    data = data,
    family = poisson(),
    validate = FALSE
  )
  
  cat("\n✅ TEST 2 SUCCESS\n")
  cat(glue("  - Code length: {nchar(code2)} chars\n"))
  cat(glue("  - Function calls: {length(.GlobalEnv$mu_trend_trace$calls)}\n"))
  cat(glue("  - Extractions: {length(.GlobalEnv$mu_trend_trace$extractions)}\n"))
  cat(glue("  - Creations: {length(.GlobalEnv$mu_trend_trace$creations)}\n"))
  
  writeLines(code2, "debug_re_traced.stan")
  
  # Analyze what was created vs what should have been
  if (exists("mu_trend_trace", envir = .GlobalEnv) && length(.GlobalEnv$mu_trend_trace$creations) > 0) {
    creation <- .GlobalEnv$mu_trend_trace$creations[[length(.GlobalEnv$mu_trend_trace$creations)]]
    cat("\n🔍 CREATION ANALYSIS:\n")
    cat(glue("  - Random effects patterns found: {creation$re_patterns}\n"))
    cat(glue("  - Total complex patterns: {creation$total_complex_patterns}\n"))
    cat(glue("  - Created simplified: {creation$will_create_simplified}\n"))
    
    if (creation$total_complex_patterns > 0 && creation$will_create_simplified) {
      cat("\n🚨 PROBLEM IDENTIFIED:\n")
      cat("  - Complex patterns exist but simplified mu_trend was created\n")
      cat("  - This is the root cause of the issue!\n")
    }
  }
  
}, error = function(e) {
  cat(glue("\n❌ TEST 2 FAILED: {e$message}\n"))
})

# TEST 3: GP case (the original problematic pattern)
cat("\n\n🧪 TEST 3: GP trend_formula = ~ gp(x)\n")
cat("Expected: GP patterns detected, complex mu_trend creation needed\n\n")

tryCatch({
  # Clear trace
  if (exists("mu_trend_trace", envir = .GlobalEnv)) {
    .GlobalEnv$mu_trend_trace$calls <- list()
    .GlobalEnv$mu_trend_trace$extractions <- list()
    .GlobalEnv$mu_trend_trace$creations <- list()
  }
  
  code3 <- stancode(
    mvgam_formula(y ~ 1, trend_formula = ~ gp(x)),
    data = data,
    family = poisson(),
    validate = FALSE
  )
  
  cat("\n✅ TEST 3 SUCCESS\n")
  cat(glue("  - Code length: {nchar(code3)} chars\n"))
  
  writeLines(code3, "debug_gp_traced.stan")
  
  # Analyze results
  if (exists("mu_trend_trace", envir = .GlobalEnv) && length(.GlobalEnv$mu_trend_trace$creations) > 0) {
    creation <- .GlobalEnv$mu_trend_trace$creations[[length(.GlobalEnv$mu_trend_trace$creations)]]
    cat("🔍 GP ANALYSIS:\n")
    cat(glue("  - GP patterns: {creation$gp_patterns}, Total complex: {creation$total_complex_patterns}\n"))
  }
  
}, error = function(e) {
  cat(glue("\n❌ TEST 3 FAILED: {e$message}\n"))
})

# TEST 4: Mixed complex patterns
cat("\n\n🧪 TEST 4: Mixed trend_formula = ~ (1 | series) + gp(x)\n")
cat("Expected: Multiple complex patterns detected\n\n")

tryCatch({
  # Clear trace
  if (exists("mu_trend_trace", envir = .GlobalEnv)) {
    .GlobalEnv$mu_trend_trace$calls <- list()
    .GlobalEnv$mu_trend_trace$extractions <- list()
    .GlobalEnv$mu_trend_trace$creations <- list()
  }
  
  code4 <- stancode(
    mvgam_formula(y ~ 1, trend_formula = ~ (1 | series) + gp(x)),
    data = data,
    family = poisson(),
    validate = FALSE
  )
  
  cat("\n✅ TEST 4 SUCCESS\n")
  cat(glue("  - Code length: {nchar(code4)} chars\n"))
  
  writeLines(code4, "debug_mixed_traced.stan")
  
  # Analyze results
  if (exists("mu_trend_trace", envir = .GlobalEnv) && length(.GlobalEnv$mu_trend_trace$creations) > 0) {
    creation <- .GlobalEnv$mu_trend_trace$creations[[length(.GlobalEnv$mu_trend_trace$creations)]]
    cat("🔍 MIXED PATTERN ANALYSIS:\n")
    cat(glue("  - GP patterns: {creation$gp_patterns}\n"))
    cat(glue("  - RE patterns: {creation$re_patterns}\n"))
    cat(glue("  - Total complex: {creation$total_complex_patterns}\n"))
    
    if (creation$total_complex_patterns > 1) {
      cat("🚨 MULTIPLE COMPLEX PATTERNS DETECTED - HIGHEST RISK CASE!\n")
    }
  }
  
}, error = function(e) {
  cat(glue("\n❌ TEST 4 FAILED: {e$message}\n"))
})

# TEST 5: Spline patterns (if supported)
cat("\n\n🧪 TEST 5: Spline trend_formula = ~ s(x)\n")
cat("Expected: Spline patterns detected\n\n")

tryCatch({
  # Clear trace
  if (exists("mu_trend_trace", envir = .GlobalEnv)) {
    .GlobalEnv$mu_trend_trace$calls <- list()
    .GlobalEnv$mu_trend_trace$extractions <- list()
    .GlobalEnv$mu_trend_trace$creations <- list()
  }
  
  code5 <- stancode(
    mvgam_formula(y ~ 1, trend_formula = ~ s(x)),
    data = data,
    family = poisson(),
    validate = FALSE
  )
  
  cat("\n✅ TEST 5 SUCCESS\n")
  cat(glue("  - Code length: {nchar(code5)} chars\n"))
  
  writeLines(code5, "debug_spline_traced.stan")
  
  # Analyze results
  if (exists("mu_trend_trace", envir = .GlobalEnv) && length(.GlobalEnv$mu_trend_trace$creations) > 0) {
    creation <- .GlobalEnv$mu_trend_trace$creations[[length(.GlobalEnv$mu_trend_trace$creations)]]
    cat("🔍 SPLINE ANALYSIS:\n")
    cat(glue("  - Spline patterns: {creation$spline_patterns}, Total complex: {creation$total_complex_patterns}\n"))
  }
  
}, error = function(e) {
  cat(glue("\n❌ TEST 5 FAILED: {e$message}\n"))
})

# Final analysis
cat("\n\n📊 === COMPREHENSIVE TRACE ANALYSIS ===\n")

if (exists("mu_trend_trace", envir = .GlobalEnv) && length(.GlobalEnv$mu_trend_trace$calls) > 0) {
  cat("🔍 Function call summary:\n")
  call_counts <- table(sapply(.GlobalEnv$mu_trend_trace$calls, function(x) x$function_name))
  for (func in names(call_counts)) {
    cat(glue("  - {func}: {call_counts[[func]]} calls\n"))
  }
}

if (exists("mu_trend_trace", envir = .GlobalEnv) && length(.GlobalEnv$mu_trend_trace$creations) > 0) {
  cat("\n🏗️ mu_trend creation summary:\n")
  for (i in seq_along(.GlobalEnv$mu_trend_trace$creations)) {
    creation <- .GlobalEnv$mu_trend_trace$creations[[i]]
    cat(glue("  [{i}] Suffix: {creation$suffix}, Complex patterns: {creation$total_complex_patterns} "))
    cat(glue("(GP:{creation$gp_patterns} + RE:{creation$re_patterns} + Spline:{creation$spline_patterns}), "))
    cat(glue("Simplified: {creation$will_create_simplified}\n"))
    
    if (creation$total_complex_patterns > 0 && creation$will_create_simplified) {
      cat(glue("      🚨 ISSUE: {creation$total_complex_patterns} complex patterns ignored!\n"))
    }
  }
}

if (exists("mu_trend_trace", envir = .GlobalEnv) && length(.GlobalEnv$mu_trend_trace$errors) > 0) {
  cat("\n❌ Errors encountered:\n")
  for (error in .GlobalEnv$mu_trend_trace$errors) {
    cat(glue("  - {error$function_name}: {error$error}\n"))
  }
}

# Restore original functions
cat("\n🔧 Restoring original functions...\n")
assignInNamespace("extract_and_rename_trend_parameters", 
                  original_extract_and_rename_trend_parameters, 
                  ns = "mvgam")
assignInNamespace("extract_and_rename_stan_blocks", 
                  original_extract_and_rename_stan_blocks, 
                  ns = "mvgam")

cat("\n🎯 COMPREHENSIVE MU_TREND DEBUGGING COMPLETE!\n")
cat("Stan files generated:\n")
cat("  - debug_simple_traced.stan: Baseline simple case\n")
cat("  - debug_re_traced.stan: Random effects case\n") 
cat("  - debug_gp_traced.stan: GP case\n")
cat("  - debug_mixed_traced.stan: Mixed patterns case\n")
cat("  - debug_spline_traced.stan: Spline case\n")
cat("\nThis comprehensive test suite reveals all mu_trend extraction patterns!\n")