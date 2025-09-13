#!/usr/bin/env Rscript
# ==============================================================================
# COMPREHENSIVE MU_TREND DEBUGGING SCRIPT
# ==============================================================================
# 
# This script provides complete internal tracing of the Enhanced mu_trend 
# Construction System across multiple trend_formula complexities.
#
# USAGE: Rscript debug_mu_trend_comprehensive.R
# ==============================================================================

library(devtools)
library(brms)
load_all()  # Load all mvgam functions for development

cat("🔍 COMPREHENSIVE MU_TREND INTERNAL DEBUGGING SCRIPT\n")
cat("====================================================================\n\n")

# ==============================================================================
# MONKEY PATCHING SETUP - Trace All Key Functions
# ==============================================================================

# Store original functions with correct references (updated for new mu analysis system)
original_extract_mu_construction <- get("extract_mu_construction_with_classification", envir = asNamespace("mvgam"))
original_reconstruct_mu_trend <- get("reconstruct_mu_trend_with_renamed_vars", envir = asNamespace("mvgam"))
original_extract_and_rename <- get("extract_and_rename_stan_blocks", envir = asNamespace("mvgam"))

# Also hook into GLM bypass functions that skip Enhanced system
original_inject_glm_predictor <- get("inject_trend_into_glm_predictor", envir = asNamespace("mvgam"))
original_inject_linear_predictor <- get("inject_trend_into_linear_predictor", envir = asNamespace("mvgam"))

# Hook into variable declaration functions to trace resolution issues
original_find_variable_declarations <- get("find_variable_declarations", envir = asNamespace("mvgam"))
original_extract_computed_variables <- get("extract_computed_variables", envir = asNamespace("mvgam"))

# Global debugging state
debug_state <- list(
  current_scenario = "",
  current_step = "",
  results = list()
)

# Enhanced logging function
debug_log <- function(title, content, level = 1) {
  indent <- paste(rep("  ", level - 1), collapse = "")
  cat(sprintf("%s🔍 %s:\n", indent, title))
  
  if (is.character(content) && length(content) > 0) {
    if (length(content) == 1) {
      cat(sprintf("%s   %s\n", indent, content))
    } else {
      for (i in seq_along(content)) {
        cat(sprintf("%s   [%d] %s\n", indent, i, content[i]))
      }
    }
  } else if (is.list(content)) {
    for (name in names(content)) {
      cat(sprintf("%s   %s: %s\n", indent, name, 
                  if (is.character(content[[name]])) paste(content[[name]], collapse = "; ") 
                  else toString(content[[name]])))
    }
  } else {
    cat(sprintf("%s   %s\n", indent, toString(content)))
  }
  cat("\n")
}

# Monkey patch extract_mu_construction_with_classification (new mu analysis system)
assignInNamespace("extract_mu_construction_with_classification", function(stancode) {
  debug_log("EXTRACT_MU_CONSTRUCTION INPUT", list(
    scenario = debug_state$current_scenario,
    stancode_length = nchar(stancode),
    stancode_preview = substr(stancode, 1, 200)
  ), 2)
  
  result <- original_extract_mu_construction(stancode)
  
  debug_log("EXTRACT_MU_CONSTRUCTION OUTPUT", list(
    mu_construction_count = length(result$mu_construction),
    mu_construction = if (length(result$mu_construction) > 0) result$mu_construction else "NONE FOUND",
    supporting_declarations_count = length(result$supporting_declarations),
    supporting_declarations = if (length(result$supporting_declarations) > 0) result$supporting_declarations else "NONE FOUND",
    referenced_variables_count = length(result$referenced_variables),
    referenced_variables = if (length(result$referenced_variables) > 0) result$referenced_variables else "NONE FOUND"
  ), 2)
  
  # Store in global state
  debug_state$results[[debug_state$current_scenario]][["extract_mu_construction"]] <- result
  
  return(result)
}, "mvgam")

# Monkey patch reconstruct_mu_trend_with_renamed_vars
assignInNamespace("reconstruct_mu_trend_with_renamed_vars", function(mu_construction, supporting_declarations, variable_mapping, time_param = "N_trend") {
  debug_log("RECONSTRUCT_MU_TREND INPUT", list(
    scenario = debug_state$current_scenario,
    mu_construction_count = length(mu_construction),
    mu_construction = if (length(mu_construction) > 0) mu_construction else "EMPTY",
    supporting_declarations_count = length(supporting_declarations),
    supporting_declarations = if (length(supporting_declarations) > 0) supporting_declarations else "EMPTY",
    variable_mapping_count = length(variable_mapping),
    variable_mapping = if (length(variable_mapping) > 0) paste(names(variable_mapping), "->", unlist(variable_mapping), collapse = "; ") else "EMPTY",
    time_param = time_param
  ), 2)
  
  # DETAILED STEP-BY-STEP DEBUGGING
  debug_log("STEP-BY-STEP RECONSTRUCTION ANALYSIS", list(
    step = "1_INPUT_ANALYSIS",
    original_mu_expressions = mu_construction,
    has_mu_initialization = any(grepl("vector\\[.*\\]\\s+mu\\s*=\\s*rep_vector", mu_construction)),
    has_mu_assignments = any(grepl("mu\\s*\\+=", mu_construction)),
    has_variable_declarations = any(grepl("vector\\[.*\\]\\s+[a-zA-Z_][a-zA-Z0-9_]*\\s*=", mu_construction))
  ), 3)
  
  result <- original_reconstruct_mu_trend(mu_construction, supporting_declarations, variable_mapping, time_param)
  
  debug_log("STEP-BY-STEP RECONSTRUCTION ANALYSIS", list(
    step = "2_OUTPUT_ANALYSIS",
    transformed_expressions = result,
    has_mu_trend_initialization = any(grepl("vector\\[.*\\]\\s+mu_trend\\s*=\\s*rep_vector", result)),
    mu_trend_init_count = length(grep("vector\\[.*\\]\\s+mu_trend\\s*=\\s*rep_vector", result)),
    has_mu_trend_assignments = any(grepl("mu_trend\\s*\\+=", result)),
    duplicate_initialization_detected = length(grep("vector\\[.*\\]\\s+mu_trend\\s*=\\s*rep_vector", result)) > 1
  ), 3)
  
  # Analyze each line individually
  if (length(result) > 0) {
    for (i in seq_along(result)) {
      line <- result[i]
      debug_log("RECONSTRUCTION LINE ANALYSIS", list(
        line_number = i,
        content = line,
        is_mu_trend_init = grepl("vector\\[.*\\]\\s+mu_trend\\s*=\\s*rep_vector", line),
        is_mu_trend_assignment = grepl("mu_trend\\s*\\+=", line),
        is_supporting_declaration = grepl("^\\s*(int|real|vector|matrix|array)", line) && !grepl("mu_trend", line),
        line_type = if (grepl("vector\\[.*\\]\\s+mu_trend\\s*=\\s*rep_vector", line)) "INITIALIZATION" 
                   else if (grepl("mu_trend\\s*\\+=", line)) "ASSIGNMENT"
                   else if (grepl("^\\s*(int|real|vector|matrix|array)", line)) "SUPPORTING_DECL"
                   else "OTHER"
      ), 4)
    }
  }
  
  debug_log("RECONSTRUCT_MU_TREND OUTPUT", list(
    result_length = length(result),
    result = if (length(result) > 0) result else "EMPTY RESULT"
  ), 2)
  
  # Store in global state
  debug_state$results[[debug_state$current_scenario]][["reconstruct_mu_trend"]] <- result
  
  return(result)
}, "mvgam")

# Monkey patch extract_and_rename_stan_blocks (CORRECT SIGNATURE)
assignInNamespace("extract_and_rename_stan_blocks", function(stancode, suffix, mapping, is_multivariate, response_names, standata = NULL) {
  debug_log("EXTRACT_AND_RENAME INPUT", list(
    scenario = debug_state$current_scenario,
    suffix = suffix,
    is_multivariate = is_multivariate,
    response_names = paste(response_names, collapse = ", "),
    stancode_preview = substr(stancode, 1, 300),
    mapping_summary = sprintf("original_to_renamed: %d items, renamed_to_original: %d items", 
                             length(mapping$original_to_renamed), length(mapping$renamed_to_original))
  ), 2)
  
  result <- original_extract_and_rename(stancode, suffix, mapping, is_multivariate, response_names, standata)
  
  # Check stanvar creation decision  
  has_mu_construction <- !is.null(debug_state$results[[debug_state$current_scenario]][["extract_mu_construction"]]) && 
                         length(debug_state$results[[debug_state$current_scenario]][["extract_mu_construction"]]$mu_construction) > 0
  
  debug_log("STANVAR CREATION DECISION", list(
    has_mu_construction = has_mu_construction,
    logic = if (has_mu_construction) "Enhanced system should handle mu_trend" else "Fallback system should create stanvar"
  ), 2)
  
  debug_log("EXTRACT_AND_RENAME OUTPUT", list(
    stanvars_count = if (!is.null(result$stanvars)) length(result$stanvars) else 0,
    stanvar_names = if (!is.null(result$stanvars)) names(result$stanvars) else "NONE",
    mapping_updated = sprintf("original_to_renamed: %d items, renamed_to_original: %d items", 
                             length(result$mapping$original_to_renamed), length(result$mapping$renamed_to_original))
  ), 2)
  
  # Store in global state
  debug_state$results[[debug_state$current_scenario]][["extract_and_rename"]] <- result
  
  return(result)
}, "mvgam")

# Monkey patch extract_computed_variables (CORRECT PARAMETER NAME)
assignInNamespace("extract_computed_variables", function(stan_code) {
  result <- original_extract_computed_variables(stan_code)
  
  debug_log("EXTRACT_COMPUTED_VARIABLES", list(
    scenario = debug_state$current_scenario,
    found_variables = if (length(result) > 0) result else "NONE FOUND"
  ), 2)
  
  return(result)
}, "mvgam")

# Monkey patch find_variable_declarations for variable resolution tracing
assignInNamespace("find_variable_declarations", function(stancode, referenced_vars) {
  debug_log("FIND_VARIABLE_DECLARATIONS INPUT", list(
    scenario = debug_state$current_scenario,
    stancode_length = nchar(stancode),
    referenced_vars_count = length(referenced_vars),
    referenced_vars = if (length(referenced_vars) > 0) paste(referenced_vars, collapse = ", ") else "EMPTY"
  ), 2)
  
  result <- original_find_variable_declarations(stancode, referenced_vars)
  
  debug_log("FIND_VARIABLE_DECLARATIONS OUTPUT", list(
    found_declarations_count = length(result),
    found_declarations = if (length(result) > 0) result else "NONE FOUND",
    missing_vars = if (length(referenced_vars) > 0) {
      # Try to extract declared variables from found declarations
      declared_vars <- sapply(result, function(x) {
        # Extract variable name from declaration line  
        matches <- regmatches(x, regexpr("\\b[a-zA-Z_][a-zA-Z0-9_]*", x, perl = TRUE))
        if (length(matches) > 0) matches[1] else ""
      })
      paste(setdiff(referenced_vars, declared_vars), collapse = ", ")
    } else "NONE"
  ), 2)
  
  # Store in global state
  debug_state$results[[debug_state$current_scenario]][["find_variable_declarations"]] <- list(
    input_vars = referenced_vars,
    found_declarations = result
  )
  
  return(result)
}, "mvgam")

# CRITICAL: Monitor GLM bypass functions that skip Enhanced system
assignInNamespace("inject_trend_into_glm_predictor", function(...) {
  debug_log("GLM BYPASS DETECTED", list(
    scenario = debug_state$current_scenario,
    message = "GLM path bypassing Enhanced mu_trend Construction System",
    args_count = length(list(...))
  ), 1)
  
  debug_state$results[[debug_state$current_scenario]][["glm_bypass_used"]] <- TRUE
  
  result <- original_inject_glm_predictor(...)
  
  debug_log("GLM BYPASS RESULT", list(
    scenario = debug_state$current_scenario,
    result_length = if (is.character(result)) nchar(result) else "NOT CHARACTER"
  ), 1)
  
  return(result)
}, "mvgam")

assignInNamespace("inject_trend_into_linear_predictor", function(...) {
  debug_log("LINEAR PREDICTOR INJECTION", list(
    scenario = debug_state$current_scenario,
    message = "Checking if Enhanced system or GLM bypass will be used"
  ), 1)
  
  result <- original_inject_linear_predictor(...)
  
  debug_log("LINEAR PREDICTOR RESULT", list(
    scenario = debug_state$current_scenario,
    result_type = class(result),
    glm_bypass_occurred = !is.null(debug_state$results[[debug_state$current_scenario]][["glm_bypass_used"]])
  ), 1)
  
  return(result)
}, "mvgam")

# ==============================================================================
# TEST SCENARIOS - Different Trend Formula Complexities
# ==============================================================================

test_scenarios <- list(
  "RW_simple" = list(
    description = "RW Simple (from target_generation.R)",
    formula = bf(y ~ x, family = poisson()),
    trend_formula = ~ 1,
    data_type = "univariate"
  ),
  
  "VARMA_complex" = list(
    description = "VARMA Complex (from target_generation.R)",
    formula = bf(mvbind(count, biomass) ~ s(x)) + set_rescor(FALSE),
    trend_formula = ~ presence + VAR(p = 2, ma = TRUE),
    data_type = "multivariate"
  ),
  
  "GP_CAR_complex" = list(
    description = "GP + CAR Complex (from target_generation.R)", 
    formula = bf(y ~ (1 | series), family = poisson()),
    trend_formula = ~ gp(x) + CAR(),
    data_type = "univariate"
  ),
  
  "Spline_multivariate" = list(
    description = "Spline Multivariate (from target_generation.R)",
    formula = bf(mvbind(count, biomass) ~ s(x)) + set_rescor(FALSE),
    trend_formula = ~ RW(cor = TRUE),
    data_type = "multivariate"
  ),
  
  "Monotonic_CAR" = list(
    description = "Monotonic CAR (Target 7 - failing)",
    formula = bf(y ~ (1 | series), family = poisson()),
    trend_formula = ~ mo(income) + CAR(),
    data_type = "univariate"
  )
)

# ==============================================================================
# RUN COMPREHENSIVE TESTS
# ==============================================================================

# Setup test data exactly like target_generation.R
setup_stan_test_data <- function() {
  set.seed(42)
  n_time <- 24
  n_series <- 3

  # Simple univariate dataset
  univariate <- data.frame(
    time = 1:n_time,
    series = factor(rep("series1", n_time)),
    y = rpois(n_time, lambda = 5),
    x = rnorm(n_time),
    temperature = rnorm(n_time, mean = 15, sd = 3)
  )
  
  # Add ordered factor for monotonic effects (Target 7)
  income_options <- c("below_20", "20_to_40", "40_to_100", "greater_100")
  income <- factor(sample(income_options, n_time, TRUE),
                   levels = income_options, ordered = TRUE)
  univariate$income <- income

  # Multivariate dataset with balanced design  
  multivariate <- data.frame(
    time = rep(1:n_time, n_series),
    series = factor(rep(paste0("series", 1:n_series), each = n_time)),
    count = rpois(n_time * n_series, lambda = 5),
    biomass = rgamma(n_time * n_series, shape = 2, rate = 0.5),
    presence = rbinom(n_time * n_series, size = 1, prob = 0.7),
    x = rnorm(n_time * n_series),
    temperature = rnorm(n_time * n_series, mean = 15, sd = 3)
  )
  
  # Add ordered factor for monotonic effects  
  multivariate$income <- factor(sample(income_options, n_time * n_series, TRUE),
                               levels = income_options, ordered = TRUE)

  return(list(univariate = univariate, multivariate = multivariate))
}

test_data <- setup_stan_test_data()

cat("📊 TESTING DATA STRUCTURE:\n")
debug_log("Test Data Summary", list(
  univariate_rows = nrow(test_data$univariate),
  multivariate_rows = nrow(test_data$multivariate),
  univariate_columns = paste(names(test_data$univariate), collapse = ", "),
  multivariate_columns = paste(names(test_data$multivariate), collapse = ", "),
  y_range = paste(range(test_data$univariate$y), collapse = " to "),
  time_range_uni = paste(range(test_data$univariate$time), collapse = " to "),
  time_range_multi = paste(range(test_data$multivariate$time), collapse = " to ")
))

cat("🧪 RUNNING COMPREHENSIVE TESTS ACROSS TREND FORMULA COMPLEXITIES\n")
cat("====================================================================\n\n")

for (scenario_name in names(test_scenarios)) {
  scenario <- test_scenarios[[scenario_name]]
  debug_state$current_scenario <- scenario_name
  debug_state$results[[scenario_name]] <- list()
  
  cat(sprintf("🔬 SCENARIO: %s\n", scenario$description))
  cat(sprintf("   Main Formula: %s\n", deparse(scenario$formula)))
  cat(sprintf("   Trend Formula: %s\n", deparse(scenario$trend_formula)))
  cat(sprintf("   Data Type: %s\n\n", scenario$data_type))
  
  tryCatch({
    # Use the actual mvgam pipeline to generate Stan code
    debug_log("Running full mvgam pipeline", scenario_name)
    
    # Select appropriate data
    data_to_use <- if (scenario$data_type == "univariate") test_data$univariate else test_data$multivariate
    
    # Create mvgam_formula object
    mvgam_formula_obj <- mvgam_formula(
      formula = scenario$formula,
      trend_formula = scenario$trend_formula
    )
    
    debug_log("MVGAM FORMULA CREATED", list(
      formula = deparse(scenario$formula),
      trend_formula = deparse(scenario$trend_formula),
      data_rows = nrow(data_to_use)
    ))
    
    # Generate Stan code using the actual pipeline
    generated_stancode <- stancode(
      mvgam_formula_obj,
      data = data_to_use,
      validate = FALSE
    )
    
    debug_log("STANCODE GENERATED", list(
      stancode_length = nchar(generated_stancode),
      contains_mu_trend = grepl("mu_trend", generated_stancode),
      mu_trend_count = length(gregexpr("mu_trend", generated_stancode)[[1]]),
      contains_gp_pred = grepl("gp_pred.*_trend", generated_stancode),
      contains_vector_mu_trend = grepl("vector\\[.*\\]\\s*mu_trend", generated_stancode),
      stancode_preview = substr(generated_stancode, 1, 1000)
    ))
    
    # Extract specific mu_trend patterns for analysis
    mu_trend_lines <- grep("mu_trend", strsplit(generated_stancode, "\n")[[1]], value = TRUE)
    
    debug_log("MU_TREND PATTERN ANALYSIS", list(
      total_mu_trend_lines = length(mu_trend_lines),
      mu_trend_declarations = length(grep("vector\\[.*\\]\\s*mu_trend\\s*=", mu_trend_lines)),
      mu_trend_assignments = length(grep("mu_trend\\s*\\+=", mu_trend_lines)),
      mu_trend_references = length(grep("mu_trend\\[", mu_trend_lines)),
      sample_lines = if (length(mu_trend_lines) > 0) head(mu_trend_lines, 3) else "NONE"
    ))
    
  }, error = function(e) {
    debug_log("ERROR IN SCENARIO", list(
      scenario = scenario_name,
      error = as.character(e),
      traceback = paste(traceback(), collapse = "\n")
    ))
  })
  
  cat("────────────────────────────────────────────────────────────────────\n\n")
}

# ==============================================================================
# COMPREHENSIVE ANALYSIS AND RECOMMENDATIONS
# ==============================================================================

cat("📈 COMPREHENSIVE ANALYSIS ACROSS ALL SCENARIOS\n")
cat("====================================================================\n\n")

for (scenario_name in names(debug_state$results)) {
  result_data <- debug_state$results[[scenario_name]]
  
  cat(sprintf("📋 SCENARIO ANALYSIS: %s\n", scenario_name))
  
  # Analyze the generated stancode patterns
  debug_log("Generated Stan Code Analysis", list(
    scenario_completed = !is.null(result_data) && length(result_data) > 0,
    monkey_patches_triggered = any(sapply(c("extract_mu_construction", "reconstruct_mu_trend", "extract_and_rename"), 
                                         function(x) !is.null(result_data[[x]])))
  ))
  
  # Analyze mu_construction extraction if it happened
  if (!is.null(result_data$extract_mu_construction)) {
    mu_const <- result_data$extract_mu_construction
    has_mu_construction <- length(mu_const$mu_construction) > 0
    
    debug_log("Mu Construction Extraction", list(
      found_mu_construction = has_mu_construction,
      construction_lines = if (has_mu_construction) length(mu_const$mu_construction) else 0,
      supporting_declarations = length(mu_const$supporting_declarations),
      referenced_variables = paste(mu_const$referenced_variables, collapse = ", ")
    ))
  } else {
    debug_log("Mu Construction Extraction", "NOT TRIGGERED - Enhanced system bypassed")
  }
  
  # Analyze reconstruction if it happened
  if (!is.null(result_data$reconstruct_mu_trend)) {
    reconstruct_result <- result_data$reconstruct_mu_trend
    
    debug_log("Reconstruction Process", list(
      output_lines = length(reconstruct_result),
      includes_initialization = any(grepl("vector\\[.*\\]\\s+mu_trend\\s*=\\s*rep_vector", reconstruct_result)),
      includes_assignments = any(grepl("mu_trend\\s*\\+=", reconstruct_result)),
      sample_output = if (length(reconstruct_result) > 0) head(reconstruct_result, 2) else "EMPTY"
    ))
  } else {
    debug_log("Reconstruction Process", "NOT TRIGGERED")
  }
  
  # Analyze stanvar creation if it happened
  if (!is.null(result_data$extract_and_rename)) {
    extract_result <- result_data$extract_and_rename
    
    debug_log("Stanvar Creation Process", list(
      total_stanvars = if (!is.null(extract_result$stanvars)) length(extract_result$stanvars) else 0,
      has_mu_creation_stanvar = if (!is.null(extract_result$stanvars)) "trend_model_mu_creation" %in% names(extract_result$stanvars) else FALSE,
      stanvar_names = if (!is.null(extract_result$stanvars)) names(extract_result$stanvars) else "NONE"
    ))
  } else {
    debug_log("Stanvar Creation Process", "NOT TRIGGERED")
  }
  
  # CRITICAL: Check GLM bypass usage
  glm_bypass_used <- !is.null(result_data$glm_bypass_used) && result_data$glm_bypass_used
  debug_log("GLM BYPASS ANALYSIS", list(
    glm_bypass_used = glm_bypass_used,
    impact = if (glm_bypass_used) "Enhanced mu_trend Construction System BYPASSED - explains missing mu_trend" else "Enhanced system should be active"
  ))
  
  cat("\n")
}

cat("🎯 KEY FINDINGS AND RECOMMENDATIONS\n")
cat("====================================================================\n")

# Analyze patterns across all scenarios
all_scenarios <- names(debug_state$results)
scenarios_with_mu_construction <- character(0)
scenarios_with_stanvar <- character(0)
scenarios_with_reconstruction <- character(0)
scenarios_with_glm_bypass <- character(0)

for (scenario in all_scenarios) {
  result <- debug_state$results[[scenario]]
  
  # Check if mu construction was found
  if (!is.null(result$extract_mu_construction) && 
      length(result$extract_mu_construction$mu_construction) > 0) {
    scenarios_with_mu_construction <- c(scenarios_with_mu_construction, scenario)
  }
  
  # Check if reconstruction produced output
  if (!is.null(result$reconstruct_mu_trend) && length(result$reconstruct_mu_trend) > 0) {
    scenarios_with_reconstruction <- c(scenarios_with_reconstruction, scenario)
  }
  
  # Check if stanvar was created
  if (!is.null(result$extract_and_rename$stanvars) && 
      "trend_model_mu_creation" %in% names(result$extract_and_rename$stanvars)) {
    scenarios_with_stanvar <- c(scenarios_with_stanvar, scenario)
  }
  
  # CRITICAL: Check if GLM bypass was used
  if (!is.null(result$glm_bypass_used) && result$glm_bypass_used) {
    scenarios_with_glm_bypass <- c(scenarios_with_glm_bypass, scenario)
  }
}

debug_log("CROSS-SCENARIO ANALYSIS", list(
  total_scenarios = length(all_scenarios),
  scenarios_with_mu_construction = if (length(scenarios_with_mu_construction) > 0) paste(scenarios_with_mu_construction, collapse = ", ") else "NONE",
  scenarios_with_reconstruction = if (length(scenarios_with_reconstruction) > 0) paste(scenarios_with_reconstruction, collapse = ", ") else "NONE",
  scenarios_with_stanvar = if (length(scenarios_with_stanvar) > 0) paste(scenarios_with_stanvar, collapse = ", ") else "NONE", 
  scenarios_with_glm_bypass = if (length(scenarios_with_glm_bypass) > 0) paste(scenarios_with_glm_bypass, collapse = ", ") else "NONE",
  enhanced_vs_glm_conflict = length(intersect(scenarios_with_mu_construction, scenarios_with_glm_bypass)) > 0
))

cat("\n🔧 CRITICAL FINDINGS:\n")
if (length(scenarios_with_glm_bypass) > 0) {
  cat("   ⚠️  GLM BYPASS DETECTED in scenarios:", paste(scenarios_with_glm_bypass, collapse = ", "), "\n")
  cat("   ⚠️  This explains why mu_trend declarations are missing!\n")
  cat("   ⚠️  GLM bypass skips Enhanced mu_trend Construction System entirely\n")
} else {
  cat("   ✅ No GLM bypass detected - Enhanced system should be working\n")
}

cat("\n🔧 DEBUGGING RECOMMENDATIONS:\n")
cat("   1. GLM bypass scenarios need mu_trend integration in GLM path\n")
cat("   2. Enhanced system scenarios should use reconstruction properly\n") 
cat("   3. No scenario should have both Enhanced system AND GLM bypass\n")
cat("   4. All scenarios should result in proper mu_trend declaration via ONE path\n")
cat("   5. Fix GLM bypass to include mu_trend or disable GLM optimization\n\n")

# Restore original functions
assignInNamespace("extract_mu_construction_with_classification", original_extract_mu_construction, "mvgam")
assignInNamespace("reconstruct_mu_trend_with_renamed_vars", original_reconstruct_mu_trend, "mvgam")
assignInNamespace("extract_and_rename_stan_blocks", original_extract_and_rename, "mvgam") 
assignInNamespace("extract_computed_variables", original_extract_computed_variables, "mvgam")
assignInNamespace("find_variable_declarations", original_find_variable_declarations, "mvgam")
assignInNamespace("inject_trend_into_glm_predictor", original_inject_glm_predictor, "mvgam")
assignInNamespace("inject_trend_into_linear_predictor", original_inject_linear_predictor, "mvgam")

cat("✅ COMPREHENSIVE DEBUGGING COMPLETE\n")
cat("   Original functions restored\n")
cat("   Results available in debug_state$results\n")
cat("====================================================================\n")