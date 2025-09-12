#!/usr/bin/env Rscript
# Comprehensive Debugging Script: brms Stan Code Extraction Analysis
# Purpose: Trace exactly what brms generates BEFORE mvgam extraction/renaming
# to understand the systemic mu_trend creation issue

library(brms)
library(glue)

# Load all mvgam functions (including internal ones)
devtools::load_all()

cat("===========================================\n")
cat("BRMS STAN CODE EXTRACTION DEBUG SESSION\n") 
cat("===========================================\n\n")

# Helper function for structured output
debug_section <- function(title, content_func) {
  cat(glue("\n📋 === {title} ===\n"))
  cat(paste(rep("=", nchar(title) + 8), collapse = ""), "\n\n")
  content_func()
  cat("\n")
}

# Monkey patch key functions to trace internal state
original_generate_combined_stancode <- generate_combined_stancode
original_extract_and_rename_stan_blocks <- extract_and_rename_stan_blocks

# Global debugging state
debug_state <- list(
  brms_trend_stancode = NULL,
  brms_trend_standata = NULL,
  extraction_calls = list(),
  stanvar_generations = list()
)

# Monkey patch extract_and_rename_stan_blocks
extract_and_rename_stan_blocks <- function(stancode, suffix, mapping, is_multivariate, response_names, standata = NULL) {
  cat(glue("\n🔍 EXTRACT_AND_RENAME_STAN_BLOCKS CALLED\n"))
  cat(glue("  Suffix: {suffix}\n"))
  cat(glue("  Is_multivariate: {is_multivariate}\n"))
  cat(glue("  Response_names: {paste(response_names, collapse = ', ')}\n\n"))
  
  # Store the original brms stancode for analysis
  if (grepl("_trend$", suffix)) {
    cat("📄 ORIGINAL BRMS TREND STANCODE (Before extraction):\n")
    cat(paste(rep("=", 60), collapse = ""), "\n")
    cat(stancode)
    cat("\n", paste(rep("=", 60), collapse = ""), "\n\n")
    debug_state$brms_trend_stancode <<- stancode
    
    # Analyze original brms structure
    debug_section("BRMS TREND CODE ANALYSIS", function() {
      lines <- strsplit(stancode, "\n")[[1]]
      
      cat("🔍 FUNCTIONS BLOCK:\n")
      func_start <- which(grepl("^functions\\s*\\{", lines))
      func_end <- if(length(func_start) > 0) which(grepl("^\\}\\s*$", lines))[1] else 0
      if (length(func_start) > 0 && func_end > func_start) {
        cat("  Lines", func_start, "-", func_end, ":\n")
        for (i in func_start:func_end) {
          cat(glue("    {i}: {lines[i]}\n"))
        }
      } else {
        cat("  No functions block found\n")
      }
      
      cat("\n🔍 DATA BLOCK:\n") 
      data_start <- which(grepl("^data\\s*\\{", lines))
      if (length(data_start) > 0) {
        data_end <- which(grepl("^\\}\\s*$", lines))
        data_end <- data_end[data_end > data_start][1]
        cat("  Lines", data_start, "-", data_end, ":\n")
        for (i in data_start:min(data_start + 20, data_end)) {
          cat(glue("    {i}: {lines[i]}\n"))
        }
        if (data_end > data_start + 20) cat("    ... (truncated)\n")
      }
      
      cat("\n🔍 MODEL BLOCK MU COMPUTATION:\n")
      model_start <- which(grepl("^model\\s*\\{", lines))
      if (length(model_start) > 0) {
        model_lines <- lines[model_start:length(lines)]
        mu_lines <- grep("mu\\s*[=+]|mu\\[", model_lines)
        if (length(mu_lines) > 0) {
          cat("  Found mu computation lines:\n")
          for (line_idx in mu_lines[1:min(10, length(mu_lines))]) {
            actual_line <- model_start + line_idx - 1
            cat(glue("    {actual_line}: {lines[actual_line]}\n"))
          }
          if (length(mu_lines) > 10) cat("    ... (showing first 10)\n")
        } else {
          cat("  No mu computation found in model block\n")
        }
      }
      
      cat("\n🔍 GP-RELATED PATTERNS:\n")
      gp_lines <- grep("gp_|Xgp_|sdgp_|lscale_|zgp_", lines)
      if (length(gp_lines) > 0) {
        cat("  Found GP-related lines:\n")
        for (line_idx in gp_lines) {
          cat(glue("    {line_idx}: {lines[line_idx]}\n"))
        }
      } else {
        cat("  No GP patterns found\n")
      }
    })
  }
  
  # Store extraction call for analysis
  call_info <- list(
    timestamp = Sys.time(),
    suffix = suffix,
    original_code_lines = length(strsplit(stancode, "\n")[[1]]),
    is_multivariate = is_multivariate
  )
  debug_state$extraction_calls <<- append(debug_state$extraction_calls, list(call_info))
  
  # Call original function
  result <- original_extract_and_rename_stan_blocks(stancode, suffix, mapping, is_multivariate, response_names, standata)
  
  # Analyze what was created
  if (grepl("_trend$", suffix)) {
    debug_section("EXTRACTION RESULTS ANALYSIS", function() {
      cat("🔍 CREATED STANVARS:\n")
      if (!is.null(result$stanvars) && length(result$stanvars) > 0) {
        for (i in seq_along(result$stanvars)) {
          sv <- result$stanvars[[i]]
          cat(glue("  [{i}] Name: {sv$name %||% 'unnamed'}\n"))
          cat(glue("      Block: {sv$block}\n"))
          cat(glue("      Code preview: {substr(sv$scode, 1, 100)}...\n\n"))
        }
      } else {
        cat("  No stanvars created\n")
      }
      
      cat("🔍 PARAMETER MAPPING:\n")
      if (!is.null(result$mapping)) {
        cat("  Original to Renamed:\n")
        for (name in names(result$mapping$original_to_renamed)) {
          cat(glue("    {name} -> {result$mapping$original_to_renamed[[name]]}\n"))
        }
      }
    })
  }
  
  return(result)
}

# Additional debugging for trend generation could be added here if needed

# Test cases: Compare simple vs complex trend_formula
debug_section("TEST CASE SETUP", function() {
  cat("Setting up test data for complex trend_formula debugging...\n")
})

# Create test data
set.seed(123)
n_time <- 20
n_series <- 2
data <- expand.grid(
  time = 1:n_time,
  series = factor(1:n_series)
) 
data$x <- rnorm(nrow(data))
data$y <- rpois(nrow(data), lambda = exp(1 + 0.1 * data$x + rnorm(nrow(data), 0, 0.1)))

# TEST 1: Simple trend_formula (should work)
debug_section("TEST 1: SIMPLE TREND FORMULA", function() {
  cat("Testing: trend_formula = ~ -1 (simple case)\n\n")
  
  tryCatch({
    code1 <- stancode(
      mvgam_formula(y ~ x, trend_formula = ~ -1),
      data = data,
      family = poisson(),
      validate = FALSE  # Skip stanc validation to see raw code
    )
    cat("✅ Simple trend_formula generated (no validation)\n")
    cat("Generated code length:", nchar(code1), "characters\n")
    
    # Save to file for inspection
    writeLines(code1, "debug_simple_trend.stan")
    cat("📄 Code saved to debug_simple_trend.stan\n")
  }, error = function(e) {
    cat("❌ Simple trend_formula failed:", e$message, "\n")
    cat("Full error:", str(e), "\n")
  })
})

# TEST 2: Complex trend_formula with GP (like CAR case)
debug_section("TEST 2: COMPLEX TREND FORMULA (GP)", function() {
  cat("Testing: trend_formula = ~ gp(x) (complex case)\n\n")
  
  tryCatch({
    code2 <- stancode(
      mvgam_formula(y ~ 1, trend_formula = ~ gp(x)),
      data = data,
      family = poisson(),
      validate = FALSE  # Skip stanc validation to see raw code
    )
    cat("✅ Complex trend_formula with GP generated (no validation)\n")
    cat("Generated code length:", nchar(code2), "characters\n")
    
    # Save to file for inspection
    writeLines(code2, "debug_gp_trend.stan")
    cat("📄 Code saved to debug_gp_trend.stan\n")
    
    # Show first 100 lines of generated code for immediate inspection
    lines <- strsplit(code2, "\n")[[1]]
    cat("\n🔍 FIRST 30 LINES OF GP TREND CODE:\n")
    for (i in 1:min(30, length(lines))) {
      cat(sprintf("%3d: %s\n", i, lines[i]))
    }
    if (length(lines) > 30) cat("... (truncated, see full file)\n")
    
  }, error = function(e) {
    cat("❌ Complex trend_formula with GP failed:", e$message, "\n")
    cat("Full error:", str(e), "\n")
  })
})

# TEST 3: CAR trend (the known problematic case)  
debug_section("TEST 3: CAR TREND WITH GP", function() {
  cat("Testing: CAR() with gp(x) in trend_formula\n\n")
  
  tryCatch({
    code3 <- stancode(
      mvgam_formula(y ~ 1, trend_formula = ~ gp(x) + CAR()),
      data = data,
      family = poisson(),
      validate = FALSE  # Skip stanc validation to see raw code
    )
    cat("✅ CAR with GP generated (no validation)\n")
    cat("Generated code length:", nchar(code3), "characters\n")
    
    # Save to file for inspection
    writeLines(code3, "debug_car_gp_trend.stan")
    cat("📄 Code saved to debug_car_gp_trend.stan\n")
    
    # Focus on data block to see variable ordering issue
    lines <- strsplit(code3, "\n")[[1]]
    data_start <- which(grepl("^data\\s*\\{", lines))
    data_end <- which(grepl("^\\}\\s*$", lines))
    data_end <- data_end[data_end > data_start][1]
    
    if (length(data_start) > 0 && !is.na(data_end)) {
      cat("\n🔍 DATA BLOCK STRUCTURE (variable ordering):\n")
      for (i in data_start:data_end) {
        marker <- ""
        if (grepl("N_series_trend", lines[i])) marker <- " ⚠️ "
        if (grepl("times_trend.*N_series_trend", lines[i])) marker <- " ❌ USES BEFORE DECLARE"
        cat(sprintf("%3d: %s%s\n", i, lines[i], marker))
      }
    }
    
  }, error = function(e) {
    cat("❌ CAR with GP failed:", e$message, "\n")
    cat("Error details:", conditionMessage(e), "\n")
    cat("Full error structure:", str(e), "\n")
  })
})

# Analyze the debugging state
debug_section("DEBUGGING STATE ANALYSIS", function() {
  cat("🔍 EXTRACTION CALLS SUMMARY:\n")
  for (i in seq_along(debug_state$extraction_calls)) {
    call <- debug_state$extraction_calls[[i]]
    cat(glue("  [{i}] {call$timestamp}: suffix='{call$suffix}', {call$original_code_lines} lines\n"))
  }
  
  if (!is.null(debug_state$brms_trend_stancode)) {
    cat("\n🔍 KEY INSIGHTS FROM BRMS TREND CODE:\n")
    lines <- strsplit(debug_state$brms_trend_stancode, "\n")[[1]]
    
    # Find where mu is computed in model block
    model_start <- which(grepl("^model\\s*\\{", lines))
    if (length(model_start) > 0) {
      mu_in_model <- grep("mu\\s*[+=]|gp_pred.*=", lines[model_start:length(lines)])
      if (length(mu_in_model) > 0) {
        cat("  ⚠️  PROBLEM: mu computation found in model block (should be in tparameters)\n")
        cat("  Lines that need to be moved:\n")
        for (line_idx in mu_in_model[1:min(5, length(mu_in_model))]) {
          actual_line <- model_start + line_idx - 1
          cat(glue("    {actual_line}: {lines[actual_line]}\n"))
        }
      } else {
        cat("  ✅ No problematic mu computation found in model block\n")
      }
    }
  }
})

# Restore original functions
generate_combined_stancode <- original_generate_combined_stancode
extract_and_rename_stan_blocks <- original_extract_and_rename_stan_blocks

debug_section("DEBUG SESSION COMPLETE", function() {
  cat("🎯 Key findings will help identify where brms->mvgam extraction fails\n")
  cat("📋 Next steps:\n")
  cat("   1. Examine the captured brms trend stancode for complex predictors\n") 
  cat("   2. Identify why extract_and_rename_stan_blocks fails to move mu computation\n")
  cat("   3. Fix the extraction logic to handle GP/complex predictors properly\n")
})