# Debug script to trace stanvar pipeline and identify parameter duplication issue
devtools::load_all()

cat("=== STANVAR PIPELINE DEBUG SCRIPT ===\n")

# Setup test data
data <- data.frame(
  y = rpois(50, 10),
  x = rnorm(50),
  time = 1:50,
  series = gl(1, 50, labels = 'series_1')
)

mf_with_trend <- mvgam_formula(y ~ x, trend_formula = ~ 1)

cat("\n=== STEP 1: BASE BRMS MODEL (NO TRENDS) ===\n")
# Test what pure brms generates without any trend stanvars
base_brms_code <- brms::make_stancode(
  y ~ x, 
  data = data, 
  family = poisson()
)
writeLines(base_brms_code, 'step1_pure_brms.stan')
cat("Pure brms model written to step1_pure_brms.stan\n")

# Check for any trend parameters in pure brms
base_lines <- strsplit(base_brms_code, '\n')[[1]]
trend_lines <- which(grepl('trend', base_lines))
if(length(trend_lines) > 0) {
  cat("WARNING: Pure brms model contains 'trend' references:\n")
  for(i in trend_lines) {
    cat("  Line", i, ":", base_lines[i], "\n")
  }
} else {
  cat("✓ Pure brms model contains no trend references\n")
}

cat("\n=== STEP 2: SETUP BRMS LIGHTWEIGHT ===\n")
# Follow test pattern - extract formula component from mvgam_formula
obs_formula <- mf_with_trend$formula  # Extract regular formula
trend_formula <- mf_with_trend$trend_formula  # Extract trend formula

tryCatch({
  obs_setup <- setup_brms_lightweight(obs_formula, data = data, family = poisson())
  cat("✓ setup_brms_lightweight succeeded\n")
  cat("obs_setup contains:", names(obs_setup), "\n")
}, error = function(e) {
  cat("✗ setup_brms_lightweight failed:", conditionMessage(e), "\n")
  obs_setup <- NULL
})

if(!is.null(obs_setup)) {
  cat("\n=== STEP 3: PARSE TREND SPECS ===\n")
  # Follow test pattern - parse multivariate trends to get trend_specs
  tryCatch({
    mv_spec <- parse_multivariate_trends(obs_formula, trend_formula)
    cat("✓ parse_multivariate_trends succeeded\n")
    cat("mv_spec contains:", names(mv_spec), "\n")
    cat("Has trends:", mv_spec$has_trends, "\n")
  }, error = function(e) {
    cat("✗ parse_multivariate_trends failed:", conditionMessage(e), "\n")
    mv_spec <- NULL
  })

  cat("\n=== STEP 4: TIME SERIES VALIDATION AND DIMENSIONS ===\n")
  # Follow test pattern - validate time series and extract dimensions
  if(!is.null(mv_spec) && mv_spec$has_trends) {
    tryCatch({
      validated_result <- validate_time_series_for_trends(data, mv_spec$trend_specs)
      cat("✓ validate_time_series_for_trends succeeded\n")
      
      # Add dimensions to trend_specs (function returns dimensions separately)
      mv_spec$trend_specs$dimensions <- validated_result$dimensions
      
      # Check if dimensions were added
      if(!is.null(mv_spec$trend_specs$dimensions)) {
        cat("✓ Dimensions added to trend_specs\n")
        cat("n_time:", mv_spec$trend_specs$dimensions$n_time, "\n")
        cat("n_series:", mv_spec$trend_specs$dimensions$n_series, "\n")
        cat("n_obs:", mv_spec$trend_specs$dimensions$n_obs, "\n")
      } else {
        cat("⚠ No dimensions found in updated trend_specs\n")
      }
      
    }, error = function(e) {
      cat("✗ validate_time_series_for_trends failed:", conditionMessage(e), "\n")
      validated_result <- NULL
    })
  }

  cat("\n=== STEP 5: TREND STANVARS GENERATION ===\n")
  if(!is.null(mv_spec) && mv_spec$has_trends && !is.null(validated_result)) {
    tryCatch({
      trend_stanvars <- extract_trend_stanvars_from_setup(obs_setup, mv_spec$trend_specs)
      cat("✓ extract_trend_stanvars_from_setup succeeded\n")
      cat("Generated", length(trend_stanvars), "trend stanvars\n")
    
    # Inspect each trend stanvar in detail
    cat("\n=== DETAILED STANVAR INSPECTION ===\n")
    parameters_stanvars <- list()
    tparams_stanvars <- list()
    model_stanvars <- list()
    data_stanvars <- list()
    mapping_stanvars <- list()
    
    for(i in seq_along(trend_stanvars)) {
      sv <- trend_stanvars[[i]]
      
      # Debug what type of object this is
      cat("\n--- Stanvar", i, "---\n")
      cat("Class:", paste(class(sv), collapse = ", "), "\n")
      cat("Is stanvar?", inherits(sv, 'stanvar'), "\n")
      
      if(inherits(sv, 'stanvar')) {
        cat("Name:", ifelse(is.null(sv$name), "NULL", sv$name), "\n")
        cat("Block:", sv$block, "\n")
        cat("Position:", ifelse(is.null(sv$position), "NULL", sv$position), "\n")
        
        # Show first 200 chars of code
        scode_preview <- ifelse(is.null(sv$scode), "NULL", substr(sv$scode, 1, 200))
        cat("Code preview:", scode_preview, "\n")
        
        # Categorize by block
        if(sv$block == "parameters") {
          parameters_stanvars[[length(parameters_stanvars) + 1]] <- sv
          cat(">>> PARAMETERS BLOCK <<<\n")
        } else if(sv$block == "tparameters") {
          tparams_stanvars[[length(tparams_stanvars) + 1]] <- sv
          cat(">>> TRANSFORMED PARAMETERS BLOCK <<<\n")
        } else if(sv$block == "model") {
          model_stanvars[[length(model_stanvars) + 1]] <- sv
          cat(">>> MODEL BLOCK <<<\n")
        } else if(sv$block == "data") {
          data_stanvars[[length(data_stanvars) + 1]] <- sv
          cat(">>> DATA BLOCK <<<\n")
        }
        
        # Check for mapping arrays
        if(!is.null(sv$name) && grepl("obs_trend_(time|series)", sv$name)) {
          mapping_stanvars[[length(mapping_stanvars) + 1]] <- sv
          cat(">>> MAPPING ARRAY <<<\n")
        }
      }
    }
    
    # Summary
    cat("\n=== STANVAR SUMMARY ===\n")
    cat("Parameters stanvars:", length(parameters_stanvars), "\n")
    cat("Transformed parameters stanvars:", length(tparams_stanvars), "\n") 
    cat("Model stanvars:", length(model_stanvars), "\n")
    cat("Data stanvars:", length(data_stanvars), "\n")
    cat("Mapping stanvars:", length(mapping_stanvars), "\n")
    
    # Check for mapping arrays specifically  
    if(length(mapping_stanvars) == 0) {
      cat("⚠ WARNING: No obs_trend_time/obs_trend_series mapping arrays found!\n")
      cat("This explains why injection fails.\n")
    }
    
    }, error = function(e) {
      cat("✗ extract_trend_stanvars_from_setup failed:", conditionMessage(e), "\n")
      trend_stanvars <- NULL
    })
  } else {
    cat("No trends to generate stanvars for\n")
    trend_stanvars <- NULL
  }
}

if(!is.null(obs_setup) && !is.null(trend_stanvars)) {
  cat("\n=== STEP 6: BASE STANCODE WITH TREND STANVARS ===\n")
  tryCatch({
    base_with_trends <- generate_base_stancode_with_stanvars(obs_setup, trend_stanvars)
    writeLines(base_with_trends, 'step4_base_with_trends.stan')
    cat("✓ Base stancode with trends written to step4_base_with_trends.stan\n")
    
    # Analyze where trend parameters appear
    base_trend_lines <- strsplit(base_with_trends, '\n')[[1]]
    
    # Safely find Stan blocks (handle multiple blocks)
    param_block_lines <- which(grepl('parameters\\s*\\{', base_trend_lines))
    model_block_lines <- which(grepl('model\\s*\\{', base_trend_lines))
    tparams_block_lines <- which(grepl('transformed\\s+parameters\\s*\\{', base_trend_lines))
    
    cat("Parameters blocks at lines:", param_block_lines, "\n")
    cat("Model blocks at lines:", model_block_lines, "\n") 
    cat("Transformed parameters blocks at lines:", tparams_block_lines, "\n")
    
    # Check for duplicated blocks (corruption indicator)
    if(length(param_block_lines) > 1) {
      cat("⚠ WARNING: Multiple parameters blocks detected - Stan code corruption!\n")
    }
    if(length(model_block_lines) > 1) {
      cat("⚠ WARNING: Multiple model blocks detected - Stan code corruption!\n") 
    }
    if(length(tparams_block_lines) > 1) {
      cat("⚠ WARNING: Multiple transformed parameters blocks detected - Stan code corruption!\n")
    }
    
    # Find trend parameter references
    sigma_trend_lines <- which(grepl('sigma_trend', base_trend_lines))
    innovations_lines <- which(grepl('innovations_trend', base_trend_lines))
    
    cat("\nsigma_trend appears at lines:", sigma_trend_lines, "\n")
    cat("innovations_trend appears at lines:", innovations_lines, "\n")
    
    # Show problematic lines
    cat("\n=== PROBLEMATIC PARAMETER PLACEMENTS ===\n")
    for(line_num in sigma_trend_lines) {
      line_content <- base_trend_lines[line_num]
      cat("Line", line_num, ":", line_content, "\n")
      
      # Determine which block this line is in
      param_block_before <- param_block_lines[param_block_lines < line_num]
      model_block_before <- model_block_lines[model_block_lines < line_num]
      
      if(length(param_block_before) > 0 && 
         (length(model_block_before) == 0 || max(param_block_before) > max(model_block_before))) {
        cat("  ✓ Correctly in parameters block\n")
      } else if(length(model_block_before) > 0) {
        cat("  ✗ ERROR: In model block!\n")
      } else {
        cat("  ? In unknown block\n")
      }
    }
    
  }, error = function(e) {
    cat("✗ generate_base_stancode_with_stanvars failed:", conditionMessage(e), "\n")
    base_with_trends <- NULL
  })
}

if(!is.null(obs_setup) && !is.null(trend_stanvars) && !is.null(base_with_trends)) {
  cat("\n=== STEP 7: INJECTION PROCESS ===\n")
  tryCatch({
    # This is where the error occurs in the failing tests
    injected_code <- inject_trend_into_linear_predictor(base_with_trends, trend_stanvars)
    writeLines(injected_code, 'step5_after_injection.stan')
    cat("✓ Injected code written to step5_after_injection.stan\n")
    
  }, error = function(e) {
    cat("✗ inject_trend_into_linear_predictor failed:", conditionMessage(e), "\n")
    
    # Capture the error details
    error_msg <- conditionMessage(e)
    cat("Error details:\n")
    cat(error_msg, "\n")
    
    # If it's a Stan syntax error, it means injection corrupted the code
    if(grepl("Syntax error", error_msg)) {
      cat("\n>>> STAN SYNTAX ERROR DETECTED <<<\n")
      cat("This indicates injection process corrupted the Stan code\n")
      
      # Extract line information from error
      if(grepl("line [0-9]+", error_msg)) {
        line_match <- regexpr("line [0-9]+", error_msg)
        line_info <- regmatches(error_msg, line_match)
        cat("Error at:", line_info, "\n")
      }
    }
  })
}

cat("\n=== SUMMARY ===\n")
cat("Files generated:\n")
cat("  step1_pure_brms.stan - Pure brms model (baseline)\n")
if(file.exists('step4_base_with_trends.stan')) {
  cat("  step4_base_with_trends.stan - Base model with trend stanvars\n")
}
if(file.exists('step5_after_injection.stan')) {
  cat("  step5_after_injection.stan - After trend injection\n")
}

cat("\nNext steps:\n")
cat("1. Compare step1 vs step4 to see what trend stanvars add\n")
cat("2. Compare step4 vs step5 to see what injection does\n")
cat("3. Look for parameter duplication or misplacement\n")