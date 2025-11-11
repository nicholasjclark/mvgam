# Check Stan code from fitted models to verify trend intercepts
# This script loads saved models and analyzes their Stan code for intercept handling

devtools::load_all()

# Helper function to check intercept status in Stan code
check_model_intercept <- function(model, model_name) {
  cat("=== Checking", model_name, "===\n")
  
  # Extract Stan code from the fitted model (correct slot name)
  stancode <- model$stancode
  
  # Check for trend intercept parameter
  has_intercept_param <- grepl("real Intercept_trend", stancode)
  
  # Extract trend formula if available
  if (!is.null(model$trend_formula)) {
    trend_formula_str <- deparse(model$trend_formula)
  } else {
    trend_formula_str <- "NULL (no trend)"
  }
  
  # Check for specific patterns in Stan code
  has_mu_trend_intercept <- grepl("mu_trend \\+= Intercept_trend", stancode)
  has_rep_vector_intercept <- grepl("rep_vector\\(Intercept_trend", stancode)
  has_rep_vector_zero <- grepl("rep_vector\\(0\\.0, N_trend\\)", stancode)
  
  # Extract mu_trend construction lines
  mu_trend_lines <- grep("mu_trend", strsplit(stancode, "\n")[[1]], value = TRUE)
  mu_trend_lines <- trimws(mu_trend_lines)
  mu_trend_lines <- mu_trend_lines[mu_trend_lines != ""]
  
  cat("Trend formula:", trend_formula_str, "\n")
  cat("Has Intercept_trend parameter:", has_intercept_param, "\n")
  cat("Has mu_trend += Intercept_trend:", has_mu_trend_intercept, "\n")
  cat("Has rep_vector(Intercept_trend):", has_rep_vector_intercept, "\n")
  cat("Has rep_vector(0.0, N_trend):", has_rep_vector_zero, "\n")
  
  if (length(mu_trend_lines) > 0) {
    cat("mu_trend construction:\n")
    for (line in mu_trend_lines[1:min(3, length(mu_trend_lines))]) {
      cat("  ", line, "\n")
    }
  }
  
  # Determine expected behavior based on trend formula
  expected_intercept <- FALSE
  if (!is.null(model$trend_formula)) {
    # Parse trend formula to determine expected intercept
    formula_str <- deparse(model$trend_formula)
    # Look for explicit intercept patterns
    if (grepl("~ 1$", formula_str) || grepl("\\+ 1", formula_str)) {
      expected_intercept <- TRUE
    }
    # For trend_y ~ 0, should not have intercept
    if (grepl("~ 0$", formula_str)) {
      expected_intercept <- FALSE
    }
    # For more complex formulas, check if there are non-trend terms with intercept
    if (grepl("trend_y ~", formula_str) && !grepl("~ 0", formula_str) && !grepl("- 1", formula_str)) {
      # If it's not explicitly no-intercept, assume it should have intercept
      if (!grepl("~ 0", formula_str)) {
        expected_intercept <- TRUE
      }
    }
  }
  
  cat("Expected intercept:", expected_intercept, "\n")
  
  # Check if actual matches expected
  if (has_intercept_param == expected_intercept) {
    cat("✓ PASS: Intercept handling correct\n")
  } else {
    cat("✗ FAIL: Expected intercept =", expected_intercept, ", got =", has_intercept_param, "\n")
  }
  
  cat("\n")
  
  return(list(
    model = model_name,
    trend_formula = trend_formula_str,
    expected_intercept = expected_intercept,
    actual_intercept = has_intercept_param,
    correct = (has_intercept_param == expected_intercept),
    mu_trend_lines = mu_trend_lines
  ))
}

# List of expected model files
model_files <- list(
  "fit1" = "tasks/fixtures/fit1.rds",
  "fit2" = "tasks/fixtures/fit2.rds", 
  "fit3" = "tasks/fixtures/fit3.rds",
  "fit4" = "tasks/fixtures/fit4.rds",
  "fit5" = "tasks/fixtures/fit5.rds",
  "fit6" = "tasks/fixtures/fit6.rds",
  "fit7" = "tasks/fixtures/fit7.rds",
  "fit8" = "tasks/fixtures/fit8.rds",
  "fit9" = "tasks/fixtures/fit9.rds",
  "fit10" = "tasks/fixtures/fit10.rds",
  "fit11" = "tasks/fixtures/fit11.rds"
)

# Initialize results collection
results <- list()
missing_files <- character(0)

cat("Checking fitted models for trend intercept correctness...\n")
cat("============================================================\n\n")

# Check each model
for (model_name in names(model_files)) {
  model_path <- model_files[[model_name]]
  
  if (file.exists(model_path)) {
    tryCatch({
      model <- readRDS(model_path)
      result <- check_model_intercept(model, model_name)
      results[[model_name]] <- result
    }, error = function(e) {
      cat("ERROR loading", model_name, ":", e$message, "\n\n")
    })
  } else {
    cat("Missing file:", model_path, "\n")
    missing_files <- c(missing_files, model_name)
  }
}

# Summary report
cat("=== SUMMARY REPORT ===\n")
cat("Total models checked:", length(results), "\n")
cat("Missing model files:", length(missing_files), "\n")

if (length(missing_files) > 0) {
  cat("Missing files:", paste(missing_files, collapse = ", "), "\n")
}

# Count successes and failures
if (length(results) > 0) {
  correct_models <- sum(sapply(results, function(x) x$correct))
  total_models <- length(results)
  
  cat("Correct intercept handling:", correct_models, "/", total_models, "\n")
  
  # List any failures
  failed_models <- names(results)[!sapply(results, function(x) x$correct)]
  if (length(failed_models) > 0) {
    cat("\nFAILED MODELS:\n")
    for (failed in failed_models) {
      result <- results[[failed]]
      cat("  -", failed, ": expected =", result$expected_intercept, 
          ", got =", result$actual_intercept, "\n")
      cat("    Formula:", result$trend_formula, "\n")
    }
  } else {
    cat("✓ ALL MODELS PASS intercept validation!\n")
  }
}

# Save results for further analysis
saveRDS(results, "tasks/intercept_check_results.rds")
cat("\nResults saved to tasks/intercept_check_results.rds\n")

cat("Intercept check complete.\n")