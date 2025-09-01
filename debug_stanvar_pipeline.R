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

# Create mvgam_formula with separate observation and trend formulas
mf_with_trend <- mvgam_formula(y ~ x, trend_formula = ~ 1)

cat("\n=== STEP 1: BASE OBSERVATION MODEL (POISSON) ===\n")
# Test what pure brms generates for observation model
obs_brms_code <- brms::make_stancode(
  y ~ x, 
  data = data, 
  family = poisson()
)
writeLines(obs_brms_code, 'step1_pure_brms_obs.stan')
cat("Pure brms observation model (Poisson) written to step1_pure_brms_obs.stan\n")

# Check for any trend parameters in pure observation model
obs_lines <- strsplit(obs_brms_code, '\n')[[1]]
trend_lines <- which(grepl('trend', obs_lines))
if(length(trend_lines) > 0) {
  cat("WARNING: Pure observation model contains 'trend' references:\n")
  for(i in trend_lines) {
    cat("  Line", i, ":", obs_lines[i], "\n")
  }
} else {
  cat("✓ Pure observation model contains no trend references\n")
}

cat("\n=== STEP 1B: BASE TREND MODEL (GAUSSIAN) ===\n")
# Generate what the trend model should look like (Gaussian family)
# The trend formula should be evaluated separately
trend_brms_code <- brms::make_stancode(
  y ~ 1,  # Trend formula as response (simplified for debugging)
  data = data, 
  family = gaussian()
)
writeLines(trend_brms_code, 'step1_pure_brms_trend.stan')
cat("Pure brms trend model (Gaussian) written to step1_pure_brms_trend.stan\n")

cat("\n=== STEP 2: SETUP BRMS LIGHTWEIGHT FOR BOTH MODELS ===\n")
# Extract formulas from mvgam_formula
obs_formula <- mf_with_trend$formula  # Extract observation formula
trend_formula <- mf_with_trend$trend_formula  # Extract trend formula

# Setup observation model (Poisson)
tryCatch({
  obs_setup <- setup_brms_lightweight(obs_formula, data = data, family = poisson())
  cat("✓ Observation setup_brms_lightweight succeeded (Poisson)\n")
  cat("obs_setup contains:", names(obs_setup), "\n")
}, error = function(e) {
  cat("✗ Observation setup_brms_lightweight failed:", conditionMessage(e), "\n")
  obs_setup <- NULL
})

# Setup trend model (Gaussian) - this is what extract_and_rename should work with
tryCatch({
  # For trend model, we need to create a dummy response for the trend formula
  # Since trends are always Gaussian state-space models
  trend_data <- data
  trend_data$y <- rnorm(nrow(data))  # Dummy Gaussian response for trend model
  
  trend_setup <- setup_brms_lightweight(y ~ 1, data = trend_data, family = gaussian())
  cat("✓ Trend setup_brms_lightweight succeeded (Gaussian)\n")
  cat("trend_setup contains:", names(trend_setup), "\n")
}, error = function(e) {
  cat("✗ Trend setup_brms_lightweight failed:", conditionMessage(e), "\n")
  trend_setup <- NULL
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
      validated_result <- validate_time_series_for_trends(data, mv_spec$trend_specs, response_vars = "y")
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
      # DETAILED INVESTIGATION: Let's manually trace what each component produces
      cat("\n=== TRACING STANVAR COMPONENT GENERATION ===\n")
      
      # Step 1: Check brms_stanvars from extract_and_rename_trend_parameters
      cat("\n1. Testing extract_and_rename_trend_parameters...\n")
      brms_stanvars <- tryCatch({
        extract_and_rename_trend_parameters(
          trend_setup = trend_setup,  # Use trend_setup, not obs_setup!
          dimensions = mv_spec$trend_specs$dimensions,
          suffix = "_trend"
        )
      }, error = function(e) {
        cat("   ✗ extract_and_rename_trend_parameters failed:", conditionMessage(e), "\n")
        NULL
      })
      
      if(!is.null(brms_stanvars)) {
        cat("   ✓ extract_and_rename_trend_parameters succeeded\n")
        cat("   Class:", paste(class(brms_stanvars), collapse = ", "), "\n")
        cat("   Length:", length(brms_stanvars), "\n")
      }
      
      # Step 2: Check mapping_stanvars (manual brms::stanvar calls)
      cat("\n2. Testing mapping stanvar creation...\n")
      dimensions <- mv_spec$trend_specs$dimensions
      mapping_stanvars <- if (!is.null(dimensions$mappings)) {
        mapping <- dimensions$mappings[[1]]  # Single response case
        if (!is.null(mapping)) {
          obs_time_stanvar <- brms::stanvar(
            x = mapping$obs_trend_time,
            name = "obs_trend_time",
            scode = "array[N] int obs_trend_time;",
            block = "data"
          )
          obs_series_stanvar <- brms::stanvar(
            x = mapping$obs_trend_series,
            name = "obs_trend_series", 
            scode = "array[N] int obs_trend_series;",
            block = "data"
          )
          c(obs_time_stanvar, obs_series_stanvar)
        } else {
          NULL
        }
      } else {
        cat("   ⚠ No dimensions$mappings found\n")
        NULL
      }
      
      if(!is.null(mapping_stanvars)) {
        cat("   ✓ Manual mapping stanvars created\n") 
        cat("   Class:", paste(class(mapping_stanvars), collapse = ", "), "\n")
        cat("   Length:", length(mapping_stanvars), "\n")
      } else {
        cat("   ✗ No mapping stanvars created\n")
      }
      
      # Step 3: Check generate_trend_specific_stanvars
      cat("\n3. Testing generate_trend_specific_stanvars...\n")
      data_info <- list(
        n_obs = dimensions$n_obs,
        n_series = dimensions$n_series, 
        n_lv = mv_spec$trend_specs$n_lv,
        n_time = dimensions$n_time,
        n_groups = mv_spec$trend_specs$n_groups,
        n_subgroups = mv_spec$trend_specs$n_subgroups,
        time_var = dimensions$time_var,
        series_var = dimensions$series_var,
        unique_times = dimensions$unique_times,
        unique_series = dimensions$unique_series
      )
      
      trend_specific_stanvars <- tryCatch({
        generate_trend_specific_stanvars(mv_spec$trend_specs, data_info, "")
      }, error = function(e) {
        cat("   ✗ generate_trend_specific_stanvars failed:", conditionMessage(e), "\n")
        NULL
      })
      
      if(!is.null(trend_specific_stanvars)) {
        cat("   ✓ generate_trend_specific_stanvars succeeded\n")
        cat("   Class:", paste(class(trend_specific_stanvars), collapse = ", "), "\n")
        cat("   Length:", length(trend_specific_stanvars), "\n")
      }
      
      # Step 4: Test combine_stanvars manually
      cat("\n4. Testing combine_stanvars...\n")
      combined_result <- tryCatch({
        combine_stanvars(brms_stanvars, mapping_stanvars, trend_specific_stanvars)
      }, error = function(e) {
        cat("   ✗ combine_stanvars failed:", conditionMessage(e), "\n")
        NULL
      })
      
      if(!is.null(combined_result)) {
        cat("   ✓ combine_stanvars succeeded\n")
        cat("   Class:", paste(class(combined_result), collapse = ", "), "\n")
        cat("   Length:", length(combined_result), "\n")
      }
      
      # Step 5: Now run the full function to compare
      cat("\n5. Running full extract_trend_stanvars_from_setup...\n")
      trend_stanvars <- extract_trend_stanvars_from_setup(trend_setup, mv_spec$trend_specs)
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
        
        # Show full stanvar code content
        if(!is.null(sv$scode)) {
          cat("FULL STAN CODE:\n")
          cat("================\n")
          cat(sv$scode, "\n")
          cat("================\n")
          
          # CRITICAL: Check for problematic patterns in this specific stanvar
          problematic_in_stanvar <- c()
          if(grepl("vector<.*>.*sigma_trend", sv$scode)) {
            problematic_in_stanvar <- c(problematic_in_stanvar, "sigma_trend declaration")
          }
          if(grepl("matrix.*innovations_trend", sv$scode)) {
            problematic_in_stanvar <- c(problematic_in_stanvar, "innovations_trend declaration")
          }
          if(grepl("parameters\\s*\\{", sv$scode)) {
            problematic_in_stanvar <- c(problematic_in_stanvar, "parameters block header")
          }
          if(grepl("model\\s*\\{", sv$scode)) {
            problematic_in_stanvar <- c(problematic_in_stanvar, "model block header")
          }
          if(grepl("transformed\\s+parameters\\s*\\{", sv$scode)) {
            problematic_in_stanvar <- c(problematic_in_stanvar, "transformed parameters block header")
          }
          
          if(length(problematic_in_stanvar) > 0) {
            cat("🚨 CRITICAL PATTERNS IN THIS STANVAR:", paste(problematic_in_stanvar, collapse = ", "), "\n")
            cat("🚨 BLOCK TARGET:", sv$block, "\n")
            cat("🚨 BLOCK MISMATCH:", ifelse(
              (grepl("vector<.*>.*sigma_trend|matrix.*innovations_trend", sv$scode) && sv$block != "parameters") ||
              (grepl("parameters\\s*\\{|model\\s*\\{|transformed\\s+parameters\\s*\\{", sv$scode)),
              "YES - CONTENT/BLOCK MISMATCH", "NO - CONTENT/BLOCK MATCH"
            ), "\n")
          }
        } else {
          cat("STAN CODE: NULL\n")
        }
        
        # Show data content if it exists - Enhanced with sdata analysis
        data_source <- if(!is.null(sv$x)) "x field" else if(!is.null(sv$sdata)) "sdata field" else "none"
        cat("Data source:", data_source, "\n")
        
        if(!is.null(sv$x)) {
          if(is.numeric(sv$x) && length(sv$x) <= 10) {
            cat("Data (x):", paste(sv$x, collapse = ", "), "\n")
          } else if(is.matrix(sv$x)) {
            cat("Data (x): Matrix", paste(dim(sv$x), collapse = "x"), "\n")
          } else if(is.array(sv$x)) {
            cat("Data (x): Array", paste(dim(sv$x), collapse = "x"), "\n")
          } else {
            cat("Data (x): Length", length(sv$x), "class", paste(class(sv$x), collapse = ","), "\n")
          }
        } else if(!is.null(sv$sdata)) {
          if(is.numeric(sv$sdata) && length(sv$sdata) <= 10) {
            cat("Data (sdata):", paste(sv$sdata, collapse = ", "), "\n")
          } else if(is.matrix(sv$sdata)) {
            cat("Data (sdata): Matrix", paste(dim(sv$sdata), collapse = "x"), "\n")
          } else if(is.array(sv$sdata)) {
            cat("Data (sdata): Array", paste(dim(sv$sdata), collapse = "x"), "\n")
          } else {
            cat("Data (sdata): Length", length(sv$sdata), "class", paste(class(sv$sdata), collapse = ","), "\n")
          }
        } else {
          cat("Data: NULL\n")
        }
        
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
        
        # Check for problematic patterns in Stan code
        if(!is.null(sv$scode)) {
          problematic_patterns <- c()
          if(grepl("parameters\\s*\\{", sv$scode)) problematic_patterns <- c(problematic_patterns, "parameters block header")
          if(grepl("model\\s*\\{", sv$scode)) problematic_patterns <- c(problematic_patterns, "model block header")
          if(grepl("transformed\\s+parameters\\s*\\{", sv$scode)) problematic_patterns <- c(problematic_patterns, "tparameters block header")
          if(grepl("data\\s*\\{", sv$scode)) problematic_patterns <- c(problematic_patterns, "data block header")
          if(grepl("vector<.*>.*sigma_trend", sv$scode)) problematic_patterns <- c(problematic_patterns, "sigma_trend declaration")
          if(grepl("matrix.*innovations_trend", sv$scode)) problematic_patterns <- c(problematic_patterns, "innovations_trend declaration")
          
          if(length(problematic_patterns) > 0) {
            cat("⚠ PROBLEMATIC PATTERNS:", paste(problematic_patterns, collapse = ", "), "\n")
          }
        }
      } else {
        # Handle non-stanvar objects
        cat("NON-STANVAR OBJECT - Length:", length(sv), "\n")
        if(is.list(sv) && length(sv) > 0) {
          cat("List contents:\n")
          for(j in 1:min(3, length(sv))) {
            cat("  [", j, "]:", names(sv)[j], "=", class(sv[[j]]), "\n")
          }
        }
      }
    }
    
    # Summary with enhanced detail
    cat("\n=== ENHANCED STANVAR SUMMARY ===\n")
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
    
    # Detailed block content analysis
    cat("\n=== BLOCK CONTENT ANALYSIS ===\n")
    if(length(parameters_stanvars) > 0) {
      cat("PARAMETERS BLOCK STANVARS:\n")
      for(i in seq_along(parameters_stanvars)) {
        sv <- parameters_stanvars[[i]]
        cat("  [", i, "] Name:", sv$name %||% "NULL", "Code length:", nchar(sv$scode %||% ""), "\n")
      }
    }
    
    if(length(model_stanvars) > 0) {
      cat("MODEL BLOCK STANVARS:\n")
      for(i in seq_along(model_stanvars)) {
        sv <- model_stanvars[[i]]
        cat("  [", i, "] Name:", sv$name %||% "NULL", "Code length:", nchar(sv$scode %||% ""), "\n")
      }
    }
    
    if(length(data_stanvars) > 0) {
      cat("DATA BLOCK STANVARS:\n")
      for(i in seq_along(data_stanvars)) {
        sv <- data_stanvars[[i]]
        cat("  [", i, "] Name:", sv$name %||% "NULL", "Has data:", !is.null(sv$x), "\n")
      }
    }
    
    # CRITICAL INVESTIGATION: Test flocker-style combination
    cat("\n=== FLOCKER-STYLE COMBINATION TEST ===\n")
    cat("Testing + operator vs c() operator for combining stanvars...\n")
    
    # Test creating simple stanvars and combining them
    tryCatch({
      test_sv1 <- brms::stanvar(scode = "// Test stanvar 1", block = "model", name = "test1")
      test_sv2 <- brms::stanvar(scode = "// Test stanvar 2", block = "model", name = "test2")
      
      # Test + operator (flocker style)
      plus_result <- test_sv1 + test_sv2
      cat("+ operator result class:", paste(class(plus_result), collapse = ", "), "\n")
      cat("+ operator result length:", length(plus_result), "\n")
      
      # Test c() operator (our current approach)  
      c_result <- c(test_sv1, test_sv2)
      cat("c() operator result class:", paste(class(c_result), collapse = ", "), "\n")
      cat("c() operator result length:", length(c_result), "\n")
      
      # Check internal structure differences
      if(inherits(plus_result, 'stanvar') && inherits(c_result, 'stanvar')) {
        cat("Both methods produce stanvar objects, but structures may differ\n")
        
        # Check if + operator avoids duplicate Stan code blocks
        plus_generated <- brms::make_stancode(y ~ x, data = data.frame(y = 1:5, x = 1:5), stanvars = plus_result)
        c_generated <- brms::make_stancode(y ~ x, data = data.frame(y = 1:5, x = 1:5), stanvars = c_result)
        
        plus_model_blocks <- length(grep("^\\s*model\\s*\\{", strsplit(plus_generated, "\n")[[1]]))
        c_model_blocks <- length(grep("^\\s*model\\s*\\{", strsplit(c_generated, "\n")[[1]]))
        
        cat("+ operator generates", plus_model_blocks, "model blocks\n")
        cat("c() operator generates", c_model_blocks, "model blocks\n")
        
        if(plus_model_blocks < c_model_blocks) {
          cat("🎯 + OPERATOR PREVENTS DUPLICATE BLOCKS!\n")
        }
        
      } else {
        cat("Methods produce different class structures!\n")
        cat("+ method is.stanvar:", inherits(plus_result, 'stanvar'), "\n")
        cat("c() method is.stanvar:", inherits(c_result, 'stanvar'), "\n")
      }
      
    }, error = function(e) {
      cat("Error testing combination methods:", conditionMessage(e), "\n")
    })
    
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
  cat("Note: This should combine observation model (Poisson) with trend stanvars (from Gaussian)\n")
  tryCatch({
    base_with_trends <- generate_base_stancode_with_stanvars(obs_setup, trend_stanvars)
    writeLines(base_with_trends, 'step4_base_with_trends.stan')
    cat("✓ Base stancode with trends written to step4_base_with_trends.stan\n")
    
    # Enhanced analysis of generated Stan code corruption
    cat("\n=== DETAILED STAN CODE ANALYSIS ===\n")
    base_trend_lines <- strsplit(base_with_trends, '\n')[[1]]
    
    # Find all Stan blocks with precise regex
    param_block_lines <- which(grepl('^\\s*parameters\\s*\\{', base_trend_lines))
    model_block_lines <- which(grepl('^\\s*model\\s*\\{', base_trend_lines))
    tparams_block_lines <- which(grepl('^\\s*transformed\\s+parameters\\s*\\{', base_trend_lines))
    data_block_lines <- which(grepl('^\\s*data\\s*\\{', base_trend_lines))
    
    cat("BLOCK STRUCTURE ANALYSIS:\n")
    cat("Parameters blocks at lines:", param_block_lines, "\n")
    cat("Model blocks at lines:", model_block_lines, "\n") 
    cat("Transformed parameters blocks at lines:", tparams_block_lines, "\n")
    cat("Data blocks at lines:", data_block_lines, "\n")
    
    # Check for duplicated blocks (corruption indicator)
    corruption_detected <- FALSE
    if(length(param_block_lines) > 1) {
      cat("⚠ WARNING: Multiple parameters blocks detected at lines:", paste(param_block_lines, collapse = ", "), "\n")
      corruption_detected <- TRUE
    }
    if(length(model_block_lines) > 1) {
      cat("⚠ WARNING: Multiple model blocks detected at lines:", paste(model_block_lines, collapse = ", "), "\n")
      corruption_detected <- TRUE
    }
    if(length(tparams_block_lines) > 1) {
      cat("⚠ WARNING: Multiple transformed parameters blocks detected at lines:", paste(tparams_block_lines, collapse = ", "), "\n")
      corruption_detected <- TRUE
    }
    
    # Find specific problematic patterns
    cat("\nPROBLEMATIC PATTERN ANALYSIS:\n")
    sigma_trend_lines <- which(grepl('sigma_trend', base_trend_lines))
    innovations_lines <- which(grepl('innovations_trend', base_trend_lines))
    lprior_lines <- which(grepl('lprior', base_trend_lines))
    
    cat("sigma_trend appears at lines:", sigma_trend_lines, "\n")
    cat("innovations_trend appears at lines:", innovations_lines, "\n")
    cat("lprior appears at lines:", lprior_lines, "\n")
    
    # Analyze block context for each problematic line
    cat("\n=== LINE-BY-LINE BLOCK CONTEXT ANALYSIS ===\n")
    problematic_lines <- unique(c(sigma_trend_lines, innovations_lines))
    
    for(line_num in problematic_lines) {
      line_content <- trimws(base_trend_lines[line_num])
      cat("Line", line_num, ":", line_content, "\n")
      
      # Determine current block context
      current_block <- "unknown"
      last_param_block <- max(c(0, param_block_lines[param_block_lines < line_num]))
      last_model_block <- max(c(0, model_block_lines[model_block_lines < line_num]))
      last_tparams_block <- max(c(0, tparams_block_lines[tparams_block_lines < line_num]))
      last_data_block <- max(c(0, data_block_lines[data_block_lines < line_num]))
      
      latest_block_line <- max(c(last_param_block, last_model_block, last_tparams_block, last_data_block))
      
      if(latest_block_line == last_param_block && last_param_block > 0) {
        current_block <- "parameters"
      } else if(latest_block_line == last_model_block && last_model_block > 0) {
        current_block <- "model"
      } else if(latest_block_line == last_tparams_block && last_tparams_block > 0) {
        current_block <- "transformed parameters"
      } else if(latest_block_line == last_data_block && last_data_block > 0) {
        current_block <- "data"
      }
      
      cat("  Current block context:", current_block, "(last block header at line", latest_block_line, ")\n")
      
      # Check if this is appropriate placement
      is_declaration <- grepl("vector<.*>|matrix<.*>|array<.*>|real |int ", line_content)
      is_assignment <- grepl("=.*[^=<>]", line_content) && !grepl("vector<.*>=|matrix<.*>=", line_content)
      
      if(is_declaration && current_block != "parameters") {
        cat("  ❌ ERROR: Parameter declaration in", current_block, "block!\n")
        corruption_detected <- TRUE
      } else if(is_assignment && current_block == "parameters") {
        cat("  ❌ ERROR: Assignment/computation in parameters block!\n")
        corruption_detected <- TRUE
      } else if(is_declaration && current_block == "parameters") {
        cat("  ✅ OK: Parameter declaration in correct block\n")
      } else if(is_assignment && current_block %in% c("model", "transformed parameters")) {
        cat("  ✅ OK: Assignment/computation in correct block\n")
      } else {
        cat("  ⚠ UNCLEAR: Need manual inspection\n")
      }
    }
    
    # Show context around corruption
    if(corruption_detected) {
      cat("\n=== CORRUPTION CONTEXT PREVIEW ===\n")
      for(block_line in c(param_block_lines, model_block_lines, tparams_block_lines)) {
        if(block_line > 0) {
          start_line <- max(1, block_line - 2)
          end_line <- min(length(base_trend_lines), block_line + 10)
          cat("Context around line", block_line, ":\n")
          for(i in start_line:end_line) {
            marker <- if(i == block_line) " >>> " else "     "
            cat(marker, i, ":", base_trend_lines[i], "\n")
          }
          cat("\n")
        }
      }
    }
    
  }, error = function(e) {
    cat("✗ generate_base_stancode_with_stanvars failed:", conditionMessage(e), "\n")
    base_with_trends <- NULL
  })
}

# ISOLATED TESTING: Test ONLY trend-specific stanvars (no extract/rename)
if(!is.null(obs_setup) && !is.null(mv_spec) && mv_spec$has_trends) {
  cat("\n=== STEP 6.5: ISOLATED ZMVN STANVARS TEST ===\n")
  cat("Testing ONLY trend-specific stanvars (no extract/rename components)...\n")
  
  tryCatch({
    # Get the same data info used in the full pipeline
    dimensions <- mv_spec$trend_specs$dimensions
    data_info <- list(
      n_obs = dimensions$n_obs,
      n_series = dimensions$n_series, 
      n_lv = mv_spec$trend_specs$n_lv,
      n_time = dimensions$n_time,
      n_groups = mv_spec$trend_specs$n_groups,
      n_subgroups = mv_spec$trend_specs$n_subgroups,
      time_var = dimensions$time_var,
      series_var = dimensions$series_var,
      unique_times = dimensions$unique_times,
      unique_series = dimensions$unique_series
    )
    
    # Generate ONLY the trend-specific stanvars (no extract/rename)
    isolated_trend_stanvars <- generate_trend_specific_stanvars(mv_spec$trend_specs, data_info, "")
    
    cat("Generated", length(isolated_trend_stanvars), "isolated trend stanvars\n")
    cat("Class:", paste(class(isolated_trend_stanvars), collapse = ", "), "\n")
    
    # Test isolated stanvars with brms
    isolated_code <- generate_base_stancode_with_stanvars(obs_setup, isolated_trend_stanvars)
    writeLines(isolated_code, 'step6_5_isolated_trends.stan')
    cat("✓ Isolated trend stancode written to step6_5_isolated_trends.stan\n")
    
    # Analyze isolated code for corruption
    cat("\n=== ISOLATED CODE ANALYSIS ===\n")
    isolated_lines <- strsplit(isolated_code, '\n')[[1]]
    
    # Check for duplicate blocks
    param_blocks <- which(grepl('^\\s*parameters\\s*\\{', isolated_lines))
    model_blocks <- which(grepl('^\\s*model\\s*\\{', isolated_lines))
    tparam_blocks <- which(grepl('^\\s*transformed\\s+parameters\\s*\\{', isolated_lines))
    
    cat("ISOLATED CODE BLOCK STRUCTURE:\n")
    cat("Parameters blocks:", length(param_blocks), "at lines:", param_blocks, "\n")
    cat("Model blocks:", length(model_blocks), "at lines:", model_blocks, "\n")
    cat("Transformed parameters blocks:", length(tparam_blocks), "at lines:", tparam_blocks, "\n")
    
    # Check for problematic patterns
    sigma_lines <- which(grepl('sigma_trend', isolated_lines))
    innov_lines <- which(grepl('innovations_trend', isolated_lines))
    
    if(length(sigma_lines) > 0) {
      cat("sigma_trend in isolated code at lines:", sigma_lines, "\n")
      for(line_num in sigma_lines[1:min(3, length(sigma_lines))]) {
        cat("  Line", line_num, ":", trimws(isolated_lines[line_num]), "\n")
      }
    }
    
    if(length(innov_lines) > 0) {
      cat("innovations_trend in isolated code at lines:", innov_lines, "\n")
      for(line_num in innov_lines[1:min(3, length(innov_lines))]) {
        cat("  Line", line_num, ":", trimws(isolated_lines[line_num]), "\n")
      }
    }
    
    # Check corruption
    isolated_corruption <- length(param_blocks) > 1 || length(model_blocks) > 1 || length(tparam_blocks) > 1
    if(isolated_corruption) {
      cat("❌ ISOLATED STANVARS CAUSE CORRUPTION - Problem is in trend generators\n")
    } else {
      cat("✅ ISOLATED STANVARS ARE CLEAN - Problem is in extract/rename components\n")
    }
    
    # Detailed inspection of isolated stanvars
    cat("\n=== ISOLATED STANVAR INSPECTION ===\n")
    for(i in seq_along(isolated_trend_stanvars)) {
      sv <- isolated_trend_stanvars[[i]]
      cat("\n--- Isolated Stanvar", i, "---\n")
      
      if(inherits(sv, 'stanvar')) {
        cat("Name:", sv$name %||% "NULL", "\n")
        cat("Block:", sv$block, "\n")
        
        if(!is.null(sv$scode)) {
          # Check for problematic patterns in isolated stanvar
          has_sigma <- grepl("vector<.*>.*sigma_trend", sv$scode)
          has_innov <- grepl("matrix.*innovations_trend", sv$scode)
          has_block_header <- grepl("parameters\\s*\\{|model\\s*\\{|transformed\\s+parameters\\s*\\{", sv$scode)
          
          if(has_sigma || has_innov || has_block_header) {
            cat("🚨 ISOLATED STANVAR HAS PROBLEMATIC PATTERNS:\n")
            if(has_sigma) cat("  - Contains sigma_trend declaration\n")
            if(has_innov) cat("  - Contains innovations_trend declaration\n")  
            if(has_block_header) cat("  - Contains block headers\n")
            cat("FULL CODE:\n================\n", sv$scode, "\n================\n")
          } else {
            cat("✅ Clean isolated stanvar\n")
          }
        }
      } else {
        cat("Non-stanvar object:", paste(class(sv), collapse = ", "), "\n")
      }
    }
    
  }, error = function(e) {
    cat("✗ Isolated trend stanvars test failed:", conditionMessage(e), "\n")
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