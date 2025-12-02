# Comprehensive tests for extract_linpred_from_prep across all model fixtures
# Testing linear predictor extraction for all 13 model types
# ENHANCED: Deep investigation of Stan code, standata, and prediction pipeline

# CRITICAL: Always load functions first
devtools::load_all()

# Load necessary packages
library(testthat)
library(posterior)
library(insight)

cat("=== COMPREHENSIVE PREDICTION SYSTEM INVESTIGATION ===\n")
cat("Deep testing of prediction pipeline across all model fixtures\n\n")

# Define test fixture paths
fixture_dir <- "tasks/fixtures"
fixture_files <- c(
  "fit1.rds",   # Simple univariate 
  "fit2.rds",   # Multivariate mvbind
  "fit3.rds",   # Smooths + VAR
  "fit4.rds",   # Multivariate bf + bf
  "fit5.rds",   # Complex model
  "fit6.rds",   # Random effects
  "fit7.rds",   # Additional model
  "fit8.rds",   # GP model
  "fit9.rds",   # Nonlinear model
  "fit10.rds",  # Another model
  "fit11.rds",  # Another model
  "fit12.rds",  # Offset model
  "fit13.rds"   # New model
)

# Test results storage
test_results <- list()
n_passed <- 0
n_total <- 0

# ==============================================================================
# PART 1: STAN CODE EXTRACTION AND MODEL INVENTORY
# ==============================================================================

cat("=== PART 1: STAN CODE EXTRACTION ===\n")
cat("Extracting and saving Stan code from all models...\n\n")

# Extract Stan code from all models
stan_extraction_results <- list()

for (i in seq_along(fixture_files)) {
  fit_file <- fixture_files[i]
  model_num <- i
  fit_path <- file.path(fixture_dir, fit_file)
  
  cat(sprintf("--- Model %d (%s) ---\n", model_num, fit_file))
  
  if (!file.exists(fit_path)) {
    cat("  SKIPPED: File does not exist\n\n")
    stan_extraction_results[[model_num]] <- list(status = "missing")
    next
  }
  
  tryCatch({
    # Load model
    fit <- readRDS(fit_path)
    
    # Extract Stan code (stored in mvgam object)
    stan_code <- fit$stancode
    stan_file <- sprintf("tasks/current_stancode_%d.stan", model_num)
    writeLines(stan_code, stan_file)
    
    # Extract model metadata
    metadata <- list(
      model_num = model_num,
      file = fit_file,
      formula = deparse(fit$formula),
      trend_formula = if(is.null(fit$trend_formula)) "NULL" else deparse(fit$trend_formula),
      family = if(is.function(fit$family)) fit$family$family else "unknown",
      is_multivariate = brms::is.mvbrmsformula(fit$formula),
      n_variables = length(variables(fit)),
      n_obs_params = length(extract_obs_parameters(fit)),
      n_trend_params = length(extract_trend_parameters(fit)),
      stan_file = stan_file,
      data_vars = names(fit$data),
      data_nrow = nrow(fit$data)
    )
    
    stan_extraction_results[[model_num]] <- c(list(status = "success"), metadata)
    
    cat(sprintf("  ✓ Stan code saved to %s\n", stan_file))
    cat(sprintf("  ✓ Family: %s\n", paste(metadata$family, collapse = ", ")))
    cat(sprintf("  ✓ Multivariate: %s\n", metadata$is_multivariate))
    cat(sprintf("  ✓ Variables: %d total, %d obs params, %d trend params\n", 
               metadata$n_variables, metadata$n_obs_params, metadata$n_trend_params))
    
  }, error = function(e) {
    cat(sprintf("  ✗ ERROR: %s\n", e$message))
    stan_extraction_results[[model_num]] <- list(status = "error", error = e$message)
  })
  
  cat("\n")
}

# ==============================================================================
# PART 2: ENHANCED PREDICTION PIPELINE TESTING
# ==============================================================================

# Function to create newdata from model data
create_test_newdata <- function(model_data, n_rows = 3) {
  # Use first n_rows of training data as newdata
  newdata <- model_data[1:min(n_rows, nrow(model_data)), ]
  return(newdata)
}

# Function to test single model with deep investigation
test_single_model <- function(fit_path, model_name, model_num) {
  cat(sprintf("--- Deep Testing %s ---\n", model_name))
  
  if (!file.exists(fit_path)) {
    cat("SKIPPED: File does not exist\n\n")
    return(list(status = "skipped", error = "File not found"))
  }
  
  tryCatch({
    # Load model
    model <- readRDS(fit_path)
    cat("Model loaded successfully\n")
    
    # Extract detailed model info
    is_mv <- brms::is.mvbrmsformula(model$formula)
    cat(sprintf("Multivariate: %s\n", is_mv))
    
    # DEEP INVESTIGATION: Parameter analysis
    cat("\n  PARAMETER ANALYSIS:\n")
    all_vars <- variables(model)
    obs_params <- extract_obs_parameters(model)
    trend_params <- extract_trend_parameters(model)
    
    # Categorize parameters by type
    param_categories <- list(
      intercepts = obs_params[grepl("^(b_)?Intercept", obs_params)],
      fixed_effects = obs_params[grepl("^b_", obs_params) & !grepl("Intercept", obs_params)],
      smooth_std = obs_params[grepl("^zs_", obs_params)],
      smooth_sds = obs_params[grepl("^sds_", obs_params)], 
      smooth_raw = obs_params[grepl("^s_", obs_params)],
      random_effects = obs_params[grepl("^r_", obs_params)],
      random_sds = obs_params[grepl("^sd_", obs_params)],
      random_z = obs_params[grepl("^z_", obs_params)],
      gp_params = obs_params[grepl("^(sdgp|lscale|zgp)_", obs_params)],
      monotonic = obs_params[grepl("^(simo|bsp)_", obs_params)],
      other_obs = obs_params[!grepl("^(b_|Intercept|zs_|sds_|s_|r_|sd_|z_|sdgp|lscale|zgp|simo|bsp)_?", obs_params)]
    )
    
    for (cat_name in names(param_categories)) {
      if (length(param_categories[[cat_name]]) > 0) {
        cat(sprintf("    %s: %d params\n", cat_name, length(param_categories[[cat_name]])))
        if (length(param_categories[[cat_name]]) <= 5) {
          cat(sprintf("      %s\n", paste(param_categories[[cat_name]], collapse = ", ")))
        }
      }
    }
    
    if (length(trend_params) > 0) {
      cat(sprintf("    trend_params: %d params\n", length(trend_params)))
    }
    
    # Create newdata from training data with enhanced analysis
    newdata <- create_test_newdata(model$data, n_rows = 3)
    cat(sprintf("\n  NEWDATA ANALYSIS:\n"))
    cat(sprintf("    Rows: %d, Cols: %d\n", nrow(newdata), ncol(newdata)))
    cat(sprintf("    Variables: %s\n", paste(names(newdata), collapse = ", ")))
    
    # Check for required formula variables
    obs_vars <- all.vars(model$formula)
    trend_vars <- if (!is.null(model$trend_formula)) all.vars(model$trend_formula) else character(0)
    all_required_vars <- unique(c(obs_vars, trend_vars))
    
    # Remove response variables
    response_vars <- insight::find_response(model$formula, combine = FALSE)
    if (is.character(response_vars)) response_vars <- list(response_vars)
    all_response_vars <- unlist(response_vars)
    predictor_vars <- setdiff(all_required_vars, all_response_vars)
    
    missing_vars <- setdiff(predictor_vars, names(newdata))
    if (length(missing_vars) > 0) {
      cat(sprintf("    WARNING: Missing variables: %s\n", paste(missing_vars, collapse = ", ")))
    } else {
      cat("    ✓ All required variables present\n")
    }
    
    # Extract observation parameters
    obs_params <- extract_obs_parameters(model)
    cat(sprintf("Observation parameters: %d found\n", length(obs_params)))
    
    # Create mock stanfit with detailed analysis
    cat("\n  MOCK STANFIT CREATION:\n")
    full_draws <- posterior::as_draws_matrix(model$fit)
    obs_draws <- full_draws[, obs_params, drop = FALSE]
    cat(sprintf("    Full draws: %d draws × %d variables\n", nrow(full_draws), ncol(full_draws)))
    cat(sprintf("    Obs subset: %d draws × %d obs params\n", nrow(obs_draws), ncol(obs_draws)))
    
    mock_fit <- create_mock_stanfit(obs_draws)
    cat("    ✓ Mock stanfit created\n")
    
    # Generate prep object using our S3 method
    # CRITICAL: Use obs_model brmsfit, not the full mvgam object
    if (!"obs_model" %in% names(model) || is.null(model$obs_model)) {
      stop("Model missing obs_model component")
    }
    
    prep <- prepare_predictions.mock_stanfit(
      object = mock_fit,
      brmsfit = model$obs_model,  # Use obs_model brmsfit, not full mvgam
      newdata = newdata
    )
    cat("\n  PREP OBJECT ANALYSIS:\n")
    cat(sprintf("    Class: %s\n", paste(class(prep), collapse = ", ")))
    cat(sprintf("    Components: %s\n", paste(names(prep), collapse = ", ")))
    
    # Analyze standata structure
    if ("sdata" %in% names(prep)) {
      sdata_keys <- names(prep$sdata)
      cat(sprintf("    Standata: %d components\n", length(sdata_keys)))
      
      # Categorize standata components
      design_matrices <- sdata_keys[grepl("^(X|Xc)($|_)", sdata_keys)]
      smooth_matrices <- sdata_keys[grepl("^Zs_", sdata_keys)]
      re_matrices <- sdata_keys[grepl("^(Z_|J_)", sdata_keys)]
      gp_matrices <- sdata_keys[grepl("^(Xgp|Mgp)_", sdata_keys)] 
      response_data <- sdata_keys[grepl("^(Y|N)($|_)", sdata_keys)]
      offset_data <- sdata_keys[grepl("offset", sdata_keys)]
      other_data <- setdiff(sdata_keys, c(design_matrices, smooth_matrices, re_matrices, gp_matrices, response_data, offset_data))
      
      if (length(design_matrices) > 0) cat(sprintf("      Design matrices: %s\n", paste(design_matrices, collapse = ", ")))
      if (length(smooth_matrices) > 0) cat(sprintf("      Smooth matrices: %s\n", paste(smooth_matrices, collapse = ", ")))
      if (length(re_matrices) > 0) cat(sprintf("      RE matrices: %s\n", paste(re_matrices, collapse = ", ")))
      if (length(gp_matrices) > 0) cat(sprintf("      GP matrices: %s\n", paste(gp_matrices, collapse = ", ")))
      if (length(response_data) > 0) cat(sprintf("      Response data: %s\n", paste(response_data, collapse = ", ")))
      if (length(offset_data) > 0) cat(sprintf("      Offset data: %s\n", paste(offset_data, collapse = ", ")))
      if (length(other_data) > 0) cat(sprintf("      Other data: %s\n", paste(other_data, collapse = ", ")))
    }
    
    # Analyze draws structure  
    if ("draws" %in% names(prep)) {
      draws_mat <- prep$draws
      cat(sprintf("    Draws: %d draws × %d parameters\n", nrow(draws_mat), ncol(draws_mat)))
    }
    
    cat("    ✓ Prep object generated\n")
    
    # Extract linear predictor with error handling
    cat("\n  LINEAR PREDICTOR EXTRACTION:\n")
    linpred <- extract_linpred_from_prep(prep)
    cat("    ✓ Linear predictor extracted successfully\n")
    
    # VALIDATION 1: Check dimensions
    n_draws <- nrow(obs_draws)
    n_obs <- nrow(newdata)
    
    if (is_mv) {
      # Multivariate should return named list
      if (!is.list(linpred)) {
        stop("Multivariate model should return list")
      }
      
      response_names <- names(linpred)
      cat(sprintf("Responses: %s\n", paste(response_names, collapse = ", ")))
      
      for (resp in response_names) {
        resp_matrix <- linpred[[resp]]
        
        # Check matrix class
        if (!is.matrix(resp_matrix)) {
          stop(sprintf("Response %s: not a matrix", resp))
        }
        
        # Check dimensions
        if (nrow(resp_matrix) != n_draws) {
          stop(sprintf("Response %s: wrong ndraws (%d vs %d)", 
                      resp, nrow(resp_matrix), n_draws))
        }
        
        # For multivariate, n_obs is response-specific
        resp_n_obs <- prep$sdata[[paste0("N_", resp)]]
        if (ncol(resp_matrix) != resp_n_obs) {
          stop(sprintf("Response %s: wrong nobs (%d vs %d)", 
                      resp, ncol(resp_matrix), resp_n_obs))
        }
        
        cat(sprintf("Response %s: [%d × %d] ✓\n", 
                   resp, nrow(resp_matrix), ncol(resp_matrix)))
      }
    } else {
      # Univariate should return matrix
      if (!is.matrix(linpred)) {
        stop("Univariate model should return matrix")
      }
      
      # Check dimensions
      if (nrow(linpred) != n_draws) {
        stop(sprintf("Wrong ndraws (%d vs %d)", nrow(linpred), n_draws))
      }
      if (ncol(linpred) != n_obs) {
        stop(sprintf("Wrong nobs (%d vs %d)", ncol(linpred), n_obs))
      }
      
      cat(sprintf("Univariate: [%d × %d] ✓\n", nrow(linpred), ncol(linpred)))
    }
    
    # VALIDATION 2: Check for NA values
    if (is_mv) {
      for (resp in names(linpred)) {
        if (any(is.na(linpred[[resp]]))) {
          stop(sprintf("Response %s contains NA values", resp))
        }
      }
    } else {
      if (any(is.na(linpred))) {
        stop("Linear predictor contains NA values")
      }
    }
    cat("No NA values ✓\n")
    
    # VALIDATION 3: Check for finite values
    if (is_mv) {
      for (resp in names(linpred)) {
        if (!all(is.finite(linpred[[resp]]))) {
          stop(sprintf("Response %s contains non-finite values", resp))
        }
      }
    } else {
      if (!all(is.finite(linpred))) {
        stop("Linear predictor contains non-finite values")
      }
    }
    cat("All values finite ✓\n")
    
    # VALIDATION 4: Check matrix class for all components
    if (is_mv) {
      for (resp in names(linpred)) {
        if (!"matrix" %in% class(linpred[[resp]])) {
          stop(sprintf("Response %s: not proper matrix class", resp))
        }
      }
      cat("All response matrices ✓\n")
    } else {
      if (!"matrix" %in% class(linpred)) {
        stop("Not proper matrix class")
      }
      cat("Matrix class ✓\n")
    }
    
    cat(sprintf("\n✅ SUCCESS: %s passed all validations\n\n", model_name))
    
    return(list(
      status = "success", 
      linpred = linpred,
      metadata = list(
        model_num = model_num,
        is_multivariate = is_mv,
        param_categories = param_categories,
        newdata_vars = names(newdata),
        prep_sdata_keys = if("sdata" %in% names(prep)) names(prep$sdata) else NULL,
        prep_draws_ncol = if("draws" %in% names(prep)) ncol(prep$draws) else NULL
      )
    ))
    
  }, error = function(e) {
    cat(sprintf("\n❌ ERROR: %s\n", e$message))
    cat(sprintf("FAILED: %s\n\n", model_name))
    
    # Enhanced error analysis
    error_type <- "unknown"
    if (grepl("random effects", e$message, ignore.case = TRUE)) {
      error_type <- "random_effects"
    } else if (grepl("nonlinear|dpars", e$message, ignore.case = TRUE)) {
      error_type <- "nonlinear" 
    } else if (grepl("smooth", e$message, ignore.case = TRUE)) {
      error_type <- "smooth_terms"
    } else if (grepl("missing.*parameter", e$message, ignore.case = TRUE)) {
      error_type <- "missing_parameters"
    }
    
    return(list(
      status = "error", 
      error = e$message,
      error_type = error_type,
      model_num = model_num
    ))
  })
}

cat("\n=== PART 2: PREDICTION PIPELINE TESTING ===\n")
cat("Testing extract_linpred_from_prep() with deep investigation...\n\n")

# Run tests for all fixtures
for (i in seq_along(fixture_files)) {
  fit_file <- fixture_files[i]
  model_name <- paste0("Model ", i, " (", fit_file, ")")
  fit_path <- file.path(fixture_dir, fit_file)
  
  result <- test_single_model(fit_path, model_name, i)
  test_results[[model_name]] <- result
  
  n_total <- n_total + 1
  if (result$status == "success") {
    n_passed <- n_passed + 1
  }
}

# ==============================================================================
# PART 3: COMPREHENSIVE ANALYSIS AND FAILURE INVESTIGATION
# ==============================================================================

cat("=== PART 3: FAILURE ANALYSIS ===\n")

# Analyze failure patterns
failed_results <- test_results[sapply(test_results, function(x) x$status == "error")]
if (length(failed_results) > 0) {
  cat("\n  FAILURE PATTERN ANALYSIS:\n")
  
  error_types <- sapply(failed_results, function(x) x$error_type)
  error_type_counts <- table(error_types)
  
  for (error_type in names(error_type_counts)) {
    cat(sprintf("    %s: %d models\n", error_type, error_type_counts[error_type]))
  }
  
  # Deep dive into specific error patterns
  for (error_type in unique(error_types)) {
    cat(sprintf("\n    %s INVESTIGATION:\n", toupper(error_type)))
    
    type_failures <- failed_results[error_types == error_type]
    
    for (failure_name in names(type_failures)) {
      failure <- type_failures[[failure_name]]
      model_num <- failure$model_num
      
      cat(sprintf("      Model %d: %s\n", model_num, failure$error))
      
      # Additional investigation based on error type
      if (error_type == "random_effects") {
        cat("        → Checking RE parameter patterns...\n")
        # Could add specific RE pattern analysis here
      } else if (error_type == "nonlinear") {
        cat("        → Checking nonlinear formula structure...\n")
        # Could add specific NL analysis here  
      }
    }
  }
} else {
  cat("  ✅ No failures to analyze!\n")
}

# Summary
cat("\n=== SUMMARY ===\n")
cat(sprintf("Total tests: %d\n", n_total))
cat(sprintf("Passed: %d\n", n_passed))
cat(sprintf("Failed: %d\n", n_total - n_passed))
cat(sprintf("Success rate: %.1f%%\n", 100 * n_passed / n_total))

# Stan extraction summary  
success_extractions <- sum(sapply(stan_extraction_results, function(x) {
  if (is.null(x) || !is.list(x) || !"status" %in% names(x)) return(FALSE)
  return(x$status == "success")
}))
cat(sprintf("Stan code extractions: %d/%d successful\n", success_extractions, length(stan_extraction_results)))

# Detailed results
cat("\n=== DETAILED RESULTS ===\n")
for (name in names(test_results)) {
  result <- test_results[[name]]
  status_symbol <- switch(result$status,
    "success" = "✓",
    "error" = "✗", 
    "skipped" = "⊝"
  )
  cat(sprintf("%s %s\n", status_symbol, name))
  if (result$status == "error") {
    cat(sprintf("    Error: %s\n", result$error))
  }
}

cat("\n=== TESTING COMPLETE ===\n")

# Save comprehensive results
test_summary <- list(
  n_total = n_total,
  n_passed = n_passed,
  success_rate = n_passed / n_total,
  results = test_results,
  stan_extractions = stan_extraction_results,
  failure_analysis = if(length(failed_results) > 0) list(
    error_types = table(sapply(failed_results, function(x) x$error_type)),
    failed_models = sapply(failed_results, function(x) x$model_num)
  ) else NULL,
  timestamp = Sys.time()
)

saveRDS(test_summary, "tasks/linpred_test_results.rds")
cat("\n=== INVESTIGATION COMPLETE ===\n")
cat("Comprehensive results saved to tasks/linpred_test_results.rds\n")
cat("Stan code files saved as tasks/current_stancode_*.stan\n")