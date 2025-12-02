# Comprehensive tests for extract_linpred_from_prep across all model fixtures
# Testing linear predictor extraction for all 13 model types

# CRITICAL: Always load functions first
devtools::load_all()

# Load necessary packages
library(testthat)

cat("=== COMPREHENSIVE LINEAR PREDICTOR TESTING ===\n")
cat("Testing extract_linpred_from_prep() across all model fixtures\n\n")

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

# Function to create newdata from model data
create_test_newdata <- function(model_data, n_rows = 3) {
  # Use first n_rows of training data as newdata
  newdata <- model_data[1:min(n_rows, nrow(model_data)), ]
  return(newdata)
}

# Function to test single model
test_single_model <- function(fit_path, model_name) {
  cat(sprintf("--- Testing %s ---\n", model_name))
  
  if (!file.exists(fit_path)) {
    cat("SKIPPED: File does not exist\n\n")
    return(list(status = "skipped", error = "File not found"))
  }
  
  tryCatch({
    # Load model
    model <- readRDS(fit_path)
    cat("Model loaded successfully\n")
    
    # Extract basic info
    is_mv <- brms::is.mvbrmsformula(model$formula)
    cat(sprintf("Multivariate: %s\n", is_mv))
    
    # Create newdata from training data
    newdata <- create_test_newdata(model$data, n_rows = 3)
    cat(sprintf("Newdata created: %d rows\n", nrow(newdata)))
    
    # Extract observation parameters
    obs_params <- extract_obs_parameters(model)
    cat(sprintf("Observation parameters: %d found\n", length(obs_params)))
    
    # Create mock stanfit with observation parameters
    full_draws <- posterior::as_draws_matrix(model$fit)
    obs_draws <- full_draws[, obs_params, drop = FALSE]
    mock_fit <- create_mock_stanfit(obs_draws)
    cat("Mock stanfit created\n")
    
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
    cat("Prep object generated\n")
    
    # Extract linear predictor
    linpred <- extract_linpred_from_prep(prep)
    cat("Linear predictor extracted\n")
    
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
    
    cat(sprintf("SUCCESS: %s passed all validations\n\n", model_name))
    return(list(status = "success", linpred = linpred))
    
  }, error = function(e) {
    cat(sprintf("ERROR: %s\n", e$message))
    cat(sprintf("FAILED: %s\n\n", model_name))
    return(list(status = "error", error = e$message))
  })
}

# Run tests for all fixtures
for (i in seq_along(fixture_files)) {
  fit_file <- fixture_files[i]
  model_name <- paste0("Model ", i, " (", fit_file, ")")
  fit_path <- file.path(fixture_dir, fit_file)
  
  result <- test_single_model(fit_path, model_name)
  test_results[[model_name]] <- result
  
  n_total <- n_total + 1
  if (result$status == "success") {
    n_passed <- n_passed + 1
  }
}

# Summary
cat("=== SUMMARY ===\n")
cat(sprintf("Total tests: %d\n", n_total))
cat(sprintf("Passed: %d\n", n_passed))
cat(sprintf("Failed: %d\n", n_total - n_passed))
cat(sprintf("Success rate: %.1f%%\n", 100 * n_passed / n_total))

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

# Save results for later analysis
test_summary <- list(
  n_total = n_total,
  n_passed = n_passed,
  success_rate = n_passed / n_total,
  results = test_results,
  timestamp = Sys.time()
)

saveRDS(test_summary, "tasks/linpred_test_results.rds")
cat("Results saved to tasks/linpred_test_results.rds\n")