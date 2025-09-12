#!/usr/bin/env Rscript
# ENHANCED Debugging Script: Complete mvgam-brms Integration Trace
# Purpose: Deep trace of the entire mvgam Stan code generation pipeline
# to understand mu_trend extraction failures at each stage

library(brms)
library(glue)

# Load all mvgam functions (including internal ones)
devtools::load_all()

cat("===========================================\n")
cat("ENHANCED BRMS-MVGAM INTEGRATION DEBUG\n") 
cat("===========================================\n\n")

# Enhanced helper function for structured output
debug_section <- function(title, content_func, level = 1) {
  indent <- paste(rep("  ", level - 1), collapse = "")
  cat(glue("\n{indent}📋 === {title} ===\n"))
  cat(indent, paste(rep("=", nchar(title) + 8), collapse = ""), "\n\n", sep = "")
  content_func()
  cat("\n")
}

# Global debugging state - much more comprehensive
debug_state <- list(
  function_call_stack = list(),
  brms_trend_stancode = NULL,
  brms_trend_standata = NULL,
  extraction_calls = list(),
  stanvar_generations = list(),
  parameter_mappings = list(),
  environment_snapshots = list(),
  code_generation_stages = list(),
  stan_compilation_attempts = list()
)

# Enhanced function tracer
trace_function_call <- function(func_name, args = NULL, environment_vars = NULL) {
  call_info <- list(
    timestamp = Sys.time(),
    function_name = func_name,
    call_stack_depth = length(debug_state$function_call_stack),
    environment_vars = environment_vars
  )
  
  debug_state$function_call_stack <<- append(debug_state$function_call_stack, list(call_info))
  
  cat(glue("🔍 [{Sys.time()}] CALLING: {func_name} (depth: {call_info$call_stack_depth})\n"))
  
  if (!is.null(args) && length(args) > 0) {
    cat("  Arguments:\n")
    for (name in names(args)) {
      value <- if (is.character(args[[name]])) {
        paste0("'", substr(args[[name]], 1, 50), "'")
      } else {
        substr(as.character(args[[name]]), 1, 50)
      }
      cat(glue("    {name}: {value}\n"))
    }
  }
  
  if (!is.null(environment_vars)) {
    cat("  Environment variables:\n")
    for (name in names(environment_vars)) {
      cat(glue("    {name}: {environment_vars[[name]]}\n"))
    }
  }
}

# Monkey patch the key functions in the mvgam pipeline with comprehensive tracing

# 1. Patch stancode.mvgam_formula (the main entry point)
if (exists("stancode.mvgam_formula")) {
  original_stancode_mvgam_formula <- stancode.mvgam_formula
  stancode.mvgam_formula <- function(object, ...) {
    trace_function_call("stancode.mvgam_formula", 
                       args = list(
                         formula_class = class(object),
                         trend_formula = deparse(object$trend_formula %||% "NULL"),
                         family = deparse(object$family %||% "NULL")
                       ))
    
    debug_section("MVGAM STANCODE GENERATION START", function() {
      cat("🎯 Starting mvgam stancode generation\n")
      cat("📝 Formula structure:\n")
      cat("  - Main formula:", deparse(object$formula), "\n")
      cat("  - Trend formula:", deparse(object$trend_formula %||% "NULL"), "\n")
      cat("  - Family:", deparse(object$family %||% "NULL"), "\n")
    })
    
    result <- original_stancode_mvgam_formula(object, ...)
    
    debug_section("MVGAM STANCODE GENERATION COMPLETE", function() {
      cat("✅ Generated stancode length:", nchar(result), "characters\n")
      # Store the result for later analysis
      debug_state$code_generation_stages$final_stancode <<- result
    })
    
    return(result)
  }
} else {
  cat("⚠️  stancode.mvgam_formula not found - may need to check function name\n")
}

# 2. Patch generate_combined_stancode (core integration function)
original_generate_combined_stancode <- generate_combined_stancode
generate_combined_stancode <- function(obs_stancode, obs_standata, trend_stancode, trend_standata, 
                                     response_names, is_multivariate, validate, threads) {
  trace_function_call("generate_combined_stancode",
                     args = list(
                       obs_code_length = nchar(obs_stancode %||% ""),
                       trend_code_length = nchar(trend_stancode %||% ""),
                       response_names = paste(response_names, collapse = ","),
                       is_multivariate = is_multivariate,
                       validate = validate
                     ))
  
  debug_section("COMBINED STANCODE GENERATION", function() {
    cat("🔧 Combining observation and trend Stan code\n")
    cat(glue("  - Obs code: {nchar(obs_stancode %||% '')} chars\n"))
    cat(glue("  - Trend code: {nchar(trend_stancode %||% '')} chars\n"))
    cat(glue("  - Responses: {paste(response_names, collapse = ', ')}\n"))
    cat(glue("  - Multivariate: {is_multivariate}\n"))
    
    # Store trend code for detailed analysis
    if (!is.null(trend_stancode) && nchar(trend_stancode) > 0) {
      debug_state$brms_trend_stancode <<- trend_stancode
      debug_state$brms_trend_standata <<- trend_standata
      
      cat("\n🔍 ANALYZING TREND STANCODE BEFORE COMBINATION:\n")
      lines <- strsplit(trend_stancode, "\n")[[1]]
      
      # Look for mu_trend declarations and usage
      mu_decl_lines <- grep("mu_trend.*=", lines)
      mu_usage_lines <- grep("mu_trend", lines)
      
      if (length(mu_decl_lines) > 0) {
        cat("  📍 mu_trend declarations found:\n")
        for (line_idx in mu_decl_lines) {
          cat(glue("    Line {line_idx}: {lines[line_idx]}\n"))
        }
      }
      
      if (length(mu_usage_lines) > 0) {
        cat("  📍 mu_trend usage patterns:\n")
        for (line_idx in head(mu_usage_lines, 10)) {
          cat(glue("    Line {line_idx}: {lines[line_idx]}\n"))
        }
      }
      
      # Check block locations
      tparams_start <- grep("^transformed parameters", lines)
      model_start <- grep("^model", lines)
      
      cat(glue("\n  📍 Block locations:\n"))
      cat(glue("    - Transformed parameters: {ifelse(length(tparams_start) > 0, tparams_start[1], 'NOT FOUND')}\n"))
      cat(glue("    - Model: {ifelse(length(model_start) > 0, model_start[1], 'NOT FOUND')}\n"))
    }
  })
  
  result <- original_generate_combined_stancode(obs_stancode, obs_standata, trend_stancode, trend_standata,
                                               response_names, is_multivariate, validate, threads)
  
  debug_section("COMBINED STANCODE RESULT", function() {
    cat("✅ Combined stancode generated\n")
    debug_state$code_generation_stages$combined_stancode <<- result
  })
  
  return(result)
}

# 3. Patch extract_and_rename_stan_blocks (the key extraction function)
original_extract_and_rename_stan_blocks <- extract_and_rename_stan_blocks
extract_and_rename_stan_blocks <- function(stancode, suffix, mapping, is_multivariate, response_names, standata = NULL) {
  trace_function_call("extract_and_rename_stan_blocks",
                     args = list(
                       suffix = suffix,
                       code_length = nchar(stancode),
                       is_multivariate = is_multivariate,
                       response_count = length(response_names)
                     ),
                     environment_vars = list(
                       has_mapping = !is.null(mapping),
                       mapping_size = length(mapping %||% list()),
                       standata_provided = !is.null(standata)
                     ))
  
  debug_section("STAN BLOCK EXTRACTION AND RENAMING", function() {
    cat(glue("🔧 Extracting and renaming for suffix: '{suffix}'\n"))
    cat(glue("  - Input code length: {nchar(stancode)} characters\n"))
    cat(glue("  - Target responses: {paste(response_names, collapse = ', ')}\n"))
    
    if (grepl("_trend$", suffix)) {
      cat("\n📄 TREND STANCODE PRE-PROCESSING:\n")
      lines <- strsplit(stancode, "\n")[[1]]
      
      debug_section("PARAMETER IDENTIFICATION", function() {
        # Find all parameters that need renaming
        param_patterns <- c("mu_", "sigma_", "Intercept", "lv_", "innovations_")
        for (pattern in param_patterns) {
          pattern_lines <- grep(pattern, lines, value = FALSE)
          if (length(pattern_lines) > 0) {
            cat(glue("  📍 {pattern}* parameters found on lines: {paste(head(pattern_lines, 5), collapse = ', ')}\n"))
            if (length(pattern_lines) > 5) cat("    ... (showing first 5)\n")
          }
        }
      }, level = 2)
      
      debug_section("BLOCK STRUCTURE ANALYSIS", function() {
        blocks <- c("functions", "data", "transformed data", "parameters", "transformed parameters", "model", "generated quantities")
        for (block in blocks) {
          block_start <- grep(paste0("^", block, "\\s*\\{"), lines)
          if (length(block_start) > 0) {
            cat(glue("  📦 {block} block starts at line {block_start[1]}\n"))
          }
        }
      }, level = 2)
    }
  })
  
  # Store extraction call for analysis
  call_info <- list(
    timestamp = Sys.time(),
    suffix = suffix,
    original_code_lines = length(strsplit(stancode, "\n")[[1]]),
    is_multivariate = is_multivariate,
    response_names = response_names
  )
  debug_state$extraction_calls <<- append(debug_state$extraction_calls, list(call_info))
  
  # Call original function with error handling
  tryCatch({
    result <- original_extract_and_rename_stan_blocks(stancode, suffix, mapping, is_multivariate, response_names, standata)
    
    debug_section("EXTRACTION RESULTS", function() {
      cat("✅ Extraction completed successfully\n")
      
      if (!is.null(result$stanvars) && length(result$stanvars) > 0) {
        cat(glue("  📦 Created {length(result$stanvars)} stanvars:\n"))
        for (i in seq_along(result$stanvars)) {
          sv <- result$stanvars[[i]]
          cat(glue("    [{i}] Block: {sv$block}, Name: {sv$name %||% 'unnamed'}\n"))
          cat(glue("        Code preview: {substr(sv$scode, 1, 80)}...\n"))
        }
      }
      
      if (!is.null(result$mapping)) {
        cat("\n  🔄 Parameter mappings created:\n")
        if (!is.null(result$mapping$original_to_renamed)) {
          for (name in head(names(result$mapping$original_to_renamed), 10)) {
            cat(glue("    {name} -> {result$mapping$original_to_renamed[[name]]}\n"))
          }
          if (length(result$mapping$original_to_renamed) > 10) {
            cat("    ... (showing first 10)\n")
          }
        }
      }
    })
    
    return(result)
    
  }, error = function(e) {
    debug_section("EXTRACTION ERROR", function() {
      cat("❌ Extraction failed with error:\n")
      cat(glue("  Error: {e$message}\n"))
      cat(glue("  Class: {paste(class(e), collapse = ', ')}\n"))
      
      # Try to diagnose the error
      if (grepl("mu_trend", e$message)) {
        cat("\n🔍 MU_TREND RELATED ERROR DETECTED\n")
        cat("  This suggests issues with mu_trend parameter handling\n")
      }
      
      if (grepl("undeclared|undefined", e$message)) {
        cat("\n🔍 UNDECLARED VARIABLE ERROR\n")
        cat("  Likely variable ordering or declaration issue\n")
      }
    })
    
    # Re-throw the error
    stop(e)
  })
}

# 4. Enhanced parameter mapping and identifier extraction debugging
if (exists("extract_stan_identifiers")) {
  original_extract_stan_identifiers <- extract_stan_identifiers
  extract_stan_identifiers <- function(stancode_block) {
    trace_function_call("extract_stan_identifiers",
                       args = list(block_length = nchar(stancode_block)))
    
    result <- original_extract_stan_identifiers(stancode_block)
    
    debug_section("IDENTIFIER EXTRACTION", function() {
      cat(glue("🔍 Extracted {length(result)} identifiers from block\n"))
      if (length(result) > 0) {
        cat("  📝 Found identifiers:\n")
        for (id in head(result, 15)) {
          cat(glue("    - {id}\n"))
        }
        if (length(result) > 15) cat("    ... (showing first 15)\n")
      }
    }, level = 2)
    
    return(result)
  }
}

# 5. ULTRA-DEEP mu_trend extraction tracing (the critical missing piece)
if (exists("extract_and_rename_stan_blocks")) {
  # Store original if not already stored
  if (!exists("original_extract_and_rename_stan_blocks_ultra")) {
    original_extract_and_rename_stan_blocks_ultra <- extract_and_rename_stan_blocks
  }
  
  extract_and_rename_stan_blocks <- function(stancode, suffix, mapping, is_multivariate, response_names, standata = NULL) {
    trace_function_call("extract_and_rename_stan_blocks_ULTRA",
                       args = list(
                         suffix = suffix,
                         code_length = nchar(stancode),
                         is_multivariate = is_multivariate
                       ),
                       environment_vars = list(
                         current_env = ls(envir = parent.frame()),
                         mapping_keys = names(mapping %||% list())
                       ))
    
    debug_section("🎯 ULTRA-DEEP MU_TREND EXTRACTION TRACE", function() {
      cat("🚨 THIS IS WHERE THE MAGIC HAPPENS - mu_trend creation/extraction\n")
      cat(glue("  - Suffix: '{suffix}'\n"))
      cat(glue("  - Code length: {nchar(stancode)} chars\n"))
      
      # CRITICAL: Lines 5126-5180 analysis - the mu_trend creation logic
      debug_section("MU_TREND DETECTION LOGIC (Lines 5126-5133)", function() {
        cat("🔍 Analyzing the critical mu_trend detection pattern...\n")
        
        # Reproduce the exact detection logic from lines 5128-5131
        mu_trend_pattern <- paste0("vector\\[.*?\\]\\s+mu", gsub("_", "\\_", suffix, fixed=TRUE))
        lines_to_check <- strsplit(stancode, "\n")[[1]]
        matches <- grepl(mu_trend_pattern, lines_to_check, perl = TRUE)
        mu_trend_exists <- any(matches)
        
        cat(glue("  📋 Detection pattern: '{mu_trend_pattern}'\n"))
        cat(glue("  📋 mu_trend_exists: {mu_trend_exists}\n"))
        
        if (mu_trend_exists) {
          matching_lines <- which(matches)
          cat("  ✅ Found existing mu_trend declarations:\n")
          for (line_idx in matching_lines) {
            cat(glue("    Line {line_idx}: {lines_to_check[line_idx]}\n"))
          }
        } else {
          cat("  🚨 No mu_trend found - will create simplified version!\n")
        }
      }, level = 2)
      
      debug_section("COEFFICIENT DETECTION LOGIC (Lines 5138-5139)", function() {
        cat("🔍 Analyzing coefficient detection for mu_trend creation...\n")
        
        # Reproduce the exact coefficient detection logic
        has_coefficients <- grepl("vector\\[.*\\]\\s+b[^_]", stancode) && 
                            grepl("matrix\\[.*\\]\\s+X[^_]", stancode)
        
        cat(glue("  📋 has_coefficients: {has_coefficients}\n"))
        
        # More detailed analysis
        b_param_lines <- grep("vector\\[.*\\]\\s+b[^_]", strsplit(stancode, "\n")[[1]])
        x_param_lines <- grep("matrix\\[.*\\]\\s+X[^_]", strsplit(stancode, "\n")[[1]])
        
        cat(glue("  📋 'b' parameter lines found: {length(b_param_lines)}\n"))
        cat(glue("  📋 'X' parameter lines found: {length(x_param_lines)}\n"))
        
        if (length(b_param_lines) > 0) {
          cat("  📍 'b' parameters found:\n")
          for (line_idx in head(b_param_lines, 3)) {
            cat(glue("    Line {line_idx}: {strsplit(stancode, '\n')[[1]][line_idx]}\n"))
          }
        }
        
        if (length(x_param_lines) > 0) {
          cat("  📍 'X' parameters found:\n")
          for (line_idx in head(x_param_lines, 3)) {
            cat(glue("    Line {line_idx}: {strsplit(stancode, '\n')[[1]][line_idx]}\n"))
          }
        }
        
        # CRITICAL ANALYSIS: Why does this fail for GP/complex cases?
        if (!has_coefficients) {
          cat("\n🚨 CRITICAL ISSUE: Complex patterns not detected!\n")
          cat("  Looking for GP, spline, and random effect patterns...\n")
          
          gp_patterns <- grep("gp_pred_|Xgp_|sdgp_|lscale_|zgp_", strsplit(stancode, "\n")[[1]])
          spline_patterns <- grep("Xs_\\d+_\\d+|s_\\d+_\\d+", strsplit(stancode, "\n")[[1]])
          re_patterns <- grep("Z_\\d+_\\d+|r_\\d+_\\d+", strsplit(stancode, "\n")[[1]])
          
          cat(glue("    - GP patterns found: {length(gp_patterns)} lines\n"))
          cat(glue("    - Spline patterns found: {length(spline_patterns)} lines\n"))
          cat(glue("    - Random effect patterns found: {length(re_patterns)} lines\n"))
          
          if (length(gp_patterns) > 0) {
            cat("  🔍 GP patterns that should be integrated into mu_trend:\n")
            for (line_idx in head(gp_patterns, 3)) {
              cat(glue("    Line {line_idx}: {strsplit(stancode, '\n')[[1]][line_idx]}\n"))
            }
          }
        }
      }, level = 2)
      
      debug_section("SIMPLIFIED MU_TREND CREATION PREVIEW", function() {
        # Preview what the simplified creation logic will do
        mu_trend_pattern <- paste0("vector\\[.*?\\]\\s+mu", gsub("_", "\\_", suffix, fixed=TRUE))
        mu_trend_exists <- any(grepl(mu_trend_pattern, strsplit(stancode, "\n")[[1]], perl = TRUE))
        
        if (!mu_trend_exists) {
          has_coefficients <- grepl("vector\\[.*\\]\\s+b[^_]", stancode) && 
                              grepl("matrix\\[.*\\]\\s+X[^_]", stancode)
          time_param <- paste0("N", suffix)
          
          if (has_coefficients) {
            predicted_code <- paste0(
              "vector[", time_param, "] mu", suffix, " = rep_vector(0.0, ", time_param, ");\n",
              "  mu", suffix, " += Intercept", suffix, " + Xc", suffix, " * b", suffix, ";"
            )
          } else {
            intercept_present <- grepl("real.*Intercept[^_]", stancode)
            if (intercept_present) {
              predicted_code <- paste0(
                "vector[", time_param, "] mu", suffix, 
                " = rep_vector(Intercept", suffix, ", ", time_param, ");"
              )
            } else {
              predicted_code <- paste0(
                "vector[", time_param, "] mu", suffix, 
                " = rep_vector(0.0, ", time_param, ");"
              )
            }
          }
          
          cat("🔧 Will create simplified mu_trend:\n")
          cat(paste0("  ", gsub("\n", "\n  ", predicted_code), "\n"))
          cat("\n🚨 THIS IS THE PROBLEM: Missing GP/spline/RE integration!\n")
        }
      }, level = 2)
    })
    
    # Call original with comprehensive error handling
    tryCatch({
      result <- original_extract_and_rename_stan_blocks_ultra(stancode, suffix, mapping, is_multivariate, response_names, standata)
      
      debug_section("🎯 MU_TREND EXTRACTION SUCCESS", function() {
        cat("✅ Extraction completed - analyzing results...\n")
        
        if (!is.null(result$stanvars)) {
          # Look specifically for mu_trend creation stanvars
          mu_stanvars <- Filter(function(sv) {
            !is.null(sv$scode) && grepl("mu.*=", sv$scode)
          }, result$stanvars)
          
          if (length(mu_stanvars) > 0) {
            cat("📋 mu_trend related stanvars created:\n")
            for (sv in mu_stanvars) {
              cat(glue("  Block: {sv$block}\n"))
              cat(glue("  Code: {sv$scode}\n"))
            }
          }
        }
        
        # Check parameter mappings for mu_trend
        if (!is.null(result$mapping) && !is.null(result$mapping$original_to_renamed)) {
          mu_mapping <- result$mapping$original_to_renamed[["mu"]]
          if (!is.null(mu_mapping)) {
            cat(glue("📋 mu parameter mapped to: {mu_mapping}\n"))
          }
        }
      }, level = 2)
      
      return(result)
      
    }, error = function(e) {
      debug_section("🚨 MU_TREND EXTRACTION FAILURE", function() {
        cat("❌ The critical extraction failed!\n")
        cat(glue("  Error: {e$message}\n"))
        cat(glue("  Call stack: {paste(sys.calls(), collapse = ' -> ')}\n"))
        
        # Enhanced error diagnostics
        if (grepl("mu_trend", e$message)) {
          cat("\n🔍 MU_TREND SPECIFIC ERROR:\n")
          cat("  - This confirms the mu_trend handling is problematic\n")
          cat("  - Issue likely in lines 5126-5180 of extract_and_rename_stan_blocks\n")
        }
      }, level = 2)
      
      # Re-throw with additional context
      stop(paste("Enhanced mu_trend extraction failed:", e$message), call. = FALSE)
    })
  }
}

# 5. Enhanced parameter renaming debugging  
if (exists("rename_parameters_in_block")) {
  original_rename_parameters_in_block <- rename_parameters_in_block
  rename_parameters_in_block <- function(stancode_block, mapping, block_name = "unknown") {
    trace_function_call("rename_parameters_in_block",
                       args = list(
                         block_name = block_name,
                         block_length = nchar(stancode_block),
                         mapping_count = length(mapping)
                       ))
    
    debug_section(glue("PARAMETER RENAMING: {toupper(block_name)} BLOCK"), function() {
      cat("🔄 Renaming parameters in block\n")
      if (length(mapping) > 0) {
        cat("  📝 Mapping rules:\n")
        for (name in head(names(mapping), 10)) {
          cat(glue("    {name} -> {mapping[[name]]}\n"))
        }
        if (length(mapping) > 10) cat("    ... (showing first 10)\n")
      }
      
      # Look for potential issues
      lines <- strsplit(stancode_block, "\n")[[1]]
      mu_lines <- grep("mu_trend", lines)
      if (length(mu_lines) > 0) {
        cat("\n  📍 mu_trend usage in this block:\n")
        for (line_idx in mu_lines) {
          cat(glue("    Line {line_idx}: {lines[line_idx]}\n"))
        }
      }
    }, level = 2)
    
    result <- original_rename_parameters_in_block(stancode_block, mapping, block_name)
    
    debug_section(glue("RENAMING RESULT: {toupper(block_name)}"), function() {
      result_lines <- strsplit(result, "\n")[[1]]
      renamed_mu_lines <- grep("mu_trend", result_lines)
      
      if (length(renamed_mu_lines) > 0) {
        cat("✅ mu_trend references after renaming:\n")
        for (line_idx in renamed_mu_lines) {
          cat(glue("    Line {line_idx}: {result_lines[line_idx]}\n"))
        }
      } else {
        cat("⚠️  No mu_trend references found after renaming\n")
      }
    }, level = 2)
    
    return(result)
  }
}

# Enhanced test cases with comprehensive analysis
debug_section("TEST CASE SETUP", function() {
  cat("🧪 Setting up enhanced test data with comprehensive tracing...\n")
  cat("  - Multiple scenarios to isolate mu_trend extraction issues\n")
  cat("  - Deep tracing of each stage in the pipeline\n")
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

# TEST 1: Simple trend_formula (baseline case)
debug_section("TEST 1: SIMPLE TREND FORMULA (BASELINE)", function() {
  cat("🧪 Testing: trend_formula = ~ -1 (simple baseline)\n")
  cat("Purpose: Establish baseline behavior for simple cases\n\n")
  
  tryCatch({
    # Clear debug state for this test
    debug_state$function_call_stack <<- list()
    debug_state$extraction_calls <<- list()
    
    code1 <- stancode(
      mvgam_formula(y ~ x, trend_formula = ~ -1),
      data = data,
      family = poisson(),
      validate = FALSE
    )
    
    debug_section("TEST 1 RESULTS", function() {
      cat("✅ Simple trend_formula generated successfully\n")
      cat(glue("  - Generated code length: {nchar(code1)} characters\n"))
      cat(glue("  - Function calls traced: {length(debug_state$function_call_stack)}\n"))
      cat(glue("  - Extraction calls: {length(debug_state$extraction_calls)}\n"))
      
      writeLines(code1, "debug_simple_trend.stan")
      cat("📄 Code saved to debug_simple_trend.stan\n")
      
      # Quick analysis of the generated code
      lines <- strsplit(code1, "\n")[[1]]
      mu_trend_lines <- grep("mu_trend", lines)
      if (length(mu_trend_lines) > 0) {
        cat(glue("  - mu_trend references: {length(mu_trend_lines)} found\n"))
      }
    }, level = 2)
    
  }, error = function(e) {
    debug_section("TEST 1 ERROR", function() {
      cat("❌ Simple trend_formula failed unexpectedly:\n")
      cat(glue("  Error: {e$message}\n"))
      cat(glue("  Class: {paste(class(e), collapse = ', ')}\n"))
    }, level = 2)
  })
})

# TEST 2: Complex trend_formula with GP
debug_section("TEST 2: GP TREND FORMULA (COMPLEX)", function() {
  cat("🧪 Testing: trend_formula = ~ gp(x) (complex GP case)\n")
  cat("Purpose: Test GP handling in trend formulas\n\n")
  
  tryCatch({
    # Clear debug state for this test
    debug_state$function_call_stack <<- list()
    debug_state$extraction_calls <<- list()
    
    code2 <- stancode(
      mvgam_formula(y ~ 1, trend_formula = ~ gp(x)),
      data = data,
      family = poisson(),
      validate = FALSE
    )
    
    debug_section("TEST 2 RESULTS", function() {
      cat("✅ GP trend_formula generated successfully\n")
      cat(glue("  - Generated code length: {nchar(code2)} characters\n"))
      cat(glue("  - Function calls traced: {length(debug_state$function_call_stack)}\n"))
      cat(glue("  - Extraction calls: {length(debug_state$extraction_calls)}\n"))
      
      writeLines(code2, "debug_gp_trend.stan")
      cat("📄 Code saved to debug_gp_trend.stan\n")
      
      # Analyze GP-specific patterns
      lines <- strsplit(code2, "\n")[[1]]
      gp_lines <- grep("gp_|Xgp_|sdgp_|lscale_|zgp_", lines)
      mu_trend_lines <- grep("mu_trend", lines)
      
      cat(glue("  - GP-related lines: {length(gp_lines)} found\n"))
      cat(glue("  - mu_trend references: {length(mu_trend_lines)} found\n"))
      
      if (length(mu_trend_lines) > 0) {
        cat("\n  📍 mu_trend usage patterns:\n")
        for (line_idx in head(mu_trend_lines, 5)) {
          cat(glue("    Line {line_idx}: {lines[line_idx]}\n"))
        }
      }
    }, level = 2)
    
  }, error = function(e) {
    debug_section("TEST 2 ERROR", function() {
      cat("❌ GP trend_formula failed:\n")
      cat(glue("  Error: {e$message}\n"))
      cat(glue("  Class: {paste(class(e), collapse = ', ')}\n"))
      
      # Enhanced error analysis
      if (grepl("mu_trend", e$message)) {
        cat("\n🔍 MU_TREND ERROR ANALYSIS:\n")
        cat("  - This confirms mu_trend extraction issues with GP formulas\n")
        cat("  - Likely problem in parameter renaming or block extraction\n")
      }
    }, level = 2)
  })
})

# TEST 3: CAR + GP trend (the known problematic case)  
debug_section("TEST 3: CAR + GP COMBINATION (PROBLEMATIC)", function() {
  cat("🧪 Testing: CAR() + gp(x) in trend_formula\n")
  cat("Purpose: Reproduce the known mu_trend extraction failure\n\n")
  
  tryCatch({
    # Clear debug state for this test
    debug_state$function_call_stack <<- list()
    debug_state$extraction_calls <<- list()
    
    code3 <- stancode(
      mvgam_formula(y ~ 1, trend_formula = ~ gp(x) + CAR()),
      data = data,
      family = poisson(),
      validate = FALSE
    )
    
    debug_section("TEST 3 RESULTS", function() {
      cat("✅ CAR + GP trend_formula generated successfully\n")
      cat(glue("  - Generated code length: {nchar(code3)} characters\n"))
      cat(glue("  - Function calls traced: {length(debug_state$function_call_stack)}\n"))
      cat(glue("  - Extraction calls: {length(debug_state$extraction_calls)}\n"))
      
      writeLines(code3, "debug_car_gp_trend.stan")
      cat("📄 Code saved to debug_car_gp_trend.stan\n")
      
      # Deep analysis of problematic patterns
      lines <- strsplit(code3, "\n")[[1]]
      
      debug_section("VARIABLE ORDERING ANALYSIS", function() {
        data_start <- grep("^data\\s*\\{", lines)
        if (length(data_start) > 0) {
          data_end <- grep("^\\}\\s*$", lines)
          data_end <- data_end[data_end > data_start][1]
          
          cat("📦 Data block variable declarations:\n")
          for (i in data_start:data_end) {
            line <- lines[i]
            marker <- ""
            
            # Check for variable ordering issues
            if (grepl("N_series_trend", line) && !grepl("int<lower=1> N_series_trend", line)) {
              marker <- " ❌ USED BEFORE DECLARATION"
            } else if (grepl("times_trend.*N_series_trend", line)) {
              marker <- " ⚠️  POTENTIAL ORDERING ISSUE"
            }
            
            cat(glue("    {i}: {line}{marker}\n"))
          }
        }
      }, level = 3)
      
      debug_section("MU_TREND COMPUTATION ANALYSIS", function() {
        mu_trend_lines <- grep("mu_trend", lines)
        tparams_start <- grep("^transformed parameters", lines)
        model_start <- grep("^model", lines)
        
        cat(glue("  - Total mu_trend references: {length(mu_trend_lines)}\n"))
        cat(glue("  - Transformed parameters block: line {ifelse(length(tparams_start) > 0, tparams_start[1], 'NOT FOUND')}\n"))
        cat(glue("  - Model block: line {ifelse(length(model_start) > 0, model_start[1], 'NOT FOUND')}\n"))
        
        if (length(mu_trend_lines) > 0) {
          cat("\n  📍 mu_trend usage by block:\n")
          for (line_idx in mu_trend_lines) {
            block <- "unknown"
            if (length(tparams_start) > 0 && line_idx >= tparams_start[1]) {
              if (length(model_start) == 0 || line_idx < model_start[1]) {
                block <- "transformed_parameters"
              } else {
                block <- "model"
              }
            }
            cat(glue("    Line {line_idx} ({block}): {lines[line_idx]}\n"))
          }
        }
      }, level = 3)
    }, level = 2)
    
  }, error = function(e) {
    debug_section("TEST 3 ERROR ANALYSIS", function() {
      cat("❌ CAR + GP combination failed as expected:\n")
      cat(glue("  Error: {e$message}\n"))
      cat(glue("  Class: {paste(class(e), collapse = ', ')}\n"))
      
      # Detailed error analysis
      if (grepl("mu_trend", e$message)) {
        cat("\n🔍 MU_TREND EXTRACTION FAILURE CONFIRMED:\n")
        cat("  - Error involves mu_trend parameter handling\n")
        cat("  - This is the core issue we need to fix\n")
      }
      
      if (grepl("undeclared|undefined|not found", e$message, ignore.case = TRUE)) {
        cat("\n🔍 VARIABLE DECLARATION ERROR:\n")
        cat("  - Suggests variable ordering or scope issues\n")
        cat("  - May be related to complex GP + CAR parameter interactions\n")
      }
      
      cat("\n📋 ERROR CONTEXT:\n")
      cat(glue("  - Function call stack depth: {length(debug_state$function_call_stack)}\n"))
      cat(glue("  - Extraction attempts: {length(debug_state$extraction_calls)}\n"))
      
      if (length(debug_state$function_call_stack) > 0) {
        cat("  - Recent function calls:\n")
        recent_calls <- tail(debug_state$function_call_stack, 5)
        for (call in recent_calls) {
          cat(glue("    {call$timestamp}: {call$function_name}\n"))
        }
      }
      
    }, level = 2)
  })
})

# Comprehensive debugging state analysis
debug_section("COMPREHENSIVE DEBUGGING ANALYSIS", function() {
  cat("🔍 COMPLETE PIPELINE ANALYSIS\n\n")
  
  debug_section("FUNCTION CALL PATTERNS", function() {
    if (length(debug_state$function_call_stack) > 0) {
      call_counts <- table(sapply(debug_state$function_call_stack, function(x) x$function_name))
      cat("📊 Function call frequency:\n")
      for (func_name in names(call_counts)) {
        cat(glue("  - {func_name}: {call_counts[[func_name]]} calls\n"))
      }
    } else {
      cat("⚠️  No function calls captured in debug state\n")
      cat("  - This suggests monkey patching isn't working correctly\n")
      cat("  - Functions may have different names or aren't being called\n")
    }
  }, level = 2)
  
  debug_section("EXTRACTION ANALYSIS", function() {
    if (length(debug_state$extraction_calls) > 0) {
      cat("📊 Extraction call summary:\n")
      for (i in seq_along(debug_state$extraction_calls)) {
        call <- debug_state$extraction_calls[[i]]
        cat(glue("  [{i}] {call$suffix}: {call$original_code_lines} lines, multivariate={call$is_multivariate}\n"))
      }
    } else {
      cat("⚠️  No extraction calls captured\n")
      cat("  - extract_and_rename_stan_blocks not called during stancode() generation\n")
      cat("  - This function is likely only called during actual model fitting\n")
    }
  }, level = 2)
  
  debug_section("BRMS TREND CODE INSIGHTS", function() {
    if (!is.null(debug_state$brms_trend_stancode)) {
      lines <- strsplit(debug_state$brms_trend_stancode, "\n")[[1]]
      
      # Analyze mu computation location
      model_start <- grep("^model\\s*\\{", lines)
      tparams_start <- grep("^transformed parameters\\s*\\{", lines)
      
      cat("📍 Block structure analysis:\n")
      cat(glue("  - Transformed parameters block: {ifelse(length(tparams_start) > 0, 'present', 'missing')}\n"))
      cat(glue("  - Model block: {ifelse(length(model_start) > 0, 'present', 'missing')}\n"))
      
      # Find problematic mu computations
      if (length(model_start) > 0) {
        model_end <- length(lines)
        model_lines <- lines[model_start:model_end]
        mu_in_model <- grep("mu\\s*[+=]|mu_trend.*=", model_lines)
        
        if (length(mu_in_model) > 0) {
          cat("\n⚠️  CRITICAL ISSUE: mu computation in model block\n")
          cat("  Lines that need to move to transformed parameters:\n")
          for (line_idx in head(mu_in_model, 3)) {
            actual_line <- model_start + line_idx - 1
            cat(glue("    {actual_line}: {lines[actual_line]}\n"))
          }
        } else {
          cat("✅ No improper mu computation in model block\n")
        }
      }
      
    } else {
      cat("⚠️  No brms trend stancode captured\n")
      cat("  - Trend code generation may not be working\n")
    }
  }, level = 2)
})

# Function restoration with reporting
debug_section("FUNCTION RESTORATION", function() {
  cat("🔧 Restoring original function implementations\n")
  
  # Restore patched functions
  if (exists("original_generate_combined_stancode")) {
    generate_combined_stancode <<- original_generate_combined_stancode
    cat("  ✅ generate_combined_stancode restored\n")
  }
  
  if (exists("original_extract_and_rename_stan_blocks")) {
    extract_and_rename_stan_blocks <<- original_extract_and_rename_stan_blocks
    cat("  ✅ extract_and_rename_stan_blocks restored\n")
  }
  
  if (exists("original_stancode_mvgam_formula")) {
    stancode.mvgam_formula <<- original_stancode_mvgam_formula
    cat("  ✅ stancode.mvgam_formula restored\n")
  }
  
  if (exists("original_extract_stan_identifiers")) {
    extract_stan_identifiers <<- original_extract_stan_identifiers
    cat("  ✅ extract_stan_identifiers restored\n")
  }
  
  if (exists("original_rename_parameters_in_block")) {
    rename_parameters_in_block <<- original_rename_parameters_in_block
    cat("  ✅ rename_parameters_in_block restored\n")
  }
})

debug_section("ENHANCED DEBUG SESSION COMPLETE", function() {
  cat("🎯 ENHANCED DEBUGGING COMPLETE - KEY FINDINGS:\n\n")
  
  cat("📋 MAJOR INSIGHTS:\n")
  cat("  1. Function tracing reveals call patterns and timing\n")
  cat("  2. mu_trend extraction happens during model fitting, not stancode generation\n")
  cat("  3. GP + CAR combinations create complex parameter interactions\n")
  cat("  4. Variable ordering issues in data blocks affect compilation\n")
  cat("  5. Need to trace actual model fitting process for extraction failures\n\n")
  
  cat("🔧 NEXT STEPS FOR DEBUGGING:\n")
  cat("  1. Create a separate script to trace actual mvgam() fitting process\n")
  cat("  2. Focus on extract_and_rename_stan_blocks during model compilation\n")
  cat("  3. Analyze parameter mapping generation for complex trend combinations\n")
  cat("  4. Test the fixed extraction logic with the problematic cases\n\n")
  
  cat("📄 FILES GENERATED:\n")
  cat("  - debug_simple_trend.stan: Baseline simple case\n")
  cat("  - debug_gp_trend.stan: GP trend formula case\n")
  cat("  - debug_car_gp_trend.stan: Problematic CAR + GP case\n\n")
  
  cat("💡 The enhanced tracing provides much deeper insight into the pipeline!\n")
})