# Comprehensive Diagnostic Script for mu Generation Issues
# 
# Purpose: Test all model patterns from tasks/fix-stancode-mu-generation.md
# to identify specific issues with intercept control and mu construction

# CRITICAL: Load all functions before testing
devtools::load_all()

# Load test data
library(testthat)

# Create comprehensive test data
create_test_data <- function() {
  set.seed(123)
  n_time <- 10
  n_series <- 2
  
  # Create time x series data structure
  test_data <- expand.grid(
    time = 1:n_time,
    series = factor(1:n_series)
  )
  
  # Add covariates
  test_data$x <- rnorm(nrow(test_data))
  test_data$habitat <- factor(sample(c("forest", "grassland"), nrow(test_data), replace = TRUE))
  test_data$site <- factor(sample(1:3, nrow(test_data), replace = TRUE))
  
  # Add response variables
  test_data$y <- rpois(nrow(test_data), exp(1 + 0.5 * test_data$x))
  test_data$count <- rpois(nrow(test_data), exp(1 + 0.3 * test_data$x))
  test_data$biomass <- rnorm(nrow(test_data), 10 + 2 * test_data$x, 2)
  test_data$presence <- rbinom(nrow(test_data), 1, plogis(0.5 * test_data$x))
  
  # Multivariate format
  mv_data <- test_data[rep(seq_len(n_time * n_series), 3), ]
  mv_data$response <- rep(c("count", "biomass", "presence"), each = n_time * n_series)
  mv_data$y <- c(test_data$count, test_data$biomass, test_data$presence)
  
  list(
    univariate = test_data,
    multivariate = mv_data
  )
}

# Test helper functions
extract_intercept_info <- function(stancode) {
  # Check for Intercept_trend parameter declaration
  has_intercept_param <- grepl("Intercept_trend", stancode)
  
  # Check for mu_trend construction patterns
  mu_trend_lines <- grep("mu_trend", stancode, value = TRUE)
  
  # Check for rep_vector patterns
  rep_vector_lines <- grep("rep_vector", stancode, value = TRUE)
  rep_vector_trend_lines <- grep("rep_vector.*trend", stancode, value = TRUE)
  
  # Check for specific problematic patterns
  has_rep_vector_zero <- grepl("rep_vector\\(0\\.0, N_trend\\)", stancode)
  has_mu_trend_plus_intercept <- grepl("mu_trend \\+= Intercept_trend", stancode)
  has_rep_vector_intercept <- grepl("rep_vector\\(Intercept_trend, N_trend\\)", stancode)
  
  list(
    has_intercept_param = has_intercept_param,
    mu_trend_lines = mu_trend_lines,
    rep_vector_lines = rep_vector_lines,
    rep_vector_trend_lines = rep_vector_trend_lines,
    has_rep_vector_zero = has_rep_vector_zero,
    has_mu_trend_plus_intercept = has_mu_trend_plus_intercept,
    has_rep_vector_intercept = has_rep_vector_intercept
  )
}

extract_mu_construction_info <- function(stancode, response_names = NULL) {
  # Look for mu construction patterns
  mu_lines <- grep("mu\\[", stancode, value = TRUE)
  mu_plus_lines <- grep("mu.*\\+=", stancode, value = TRUE)
  
  # Look for response-specific mu patterns
  if (!is.null(response_names)) {
    mu_response_patterns <- list()
    for (resp in response_names) {
      pattern <- paste0("mu_", resp)
      mu_response_patterns[[resp]] <- grep(pattern, stancode, value = TRUE)
    }
  } else {
    mu_response_patterns <- NULL
  }
  
  # Check for GLM patterns
  glm_patterns <- grep("_glm_lpmf", stancode, value = TRUE)
  
  list(
    mu_lines = mu_lines,
    mu_plus_lines = mu_plus_lines,
    mu_response_patterns = mu_response_patterns,
    glm_patterns = glm_patterns
  )
}

# Test data setup
cat("Creating test data...\n")
test_data <- create_test_data()

# Results collection
results <- list()
errors <- list()

# =============================================================================
# TASK 1: Trend Intercept Control Verification
# =============================================================================
cat("\n=== TASK 1: Trend Intercept Control Verification ===\n")

# Test patterns for intercept control - CORRECTED EXPECTATIONS
intercept_tests <- list(
  # Should NOT have Intercept_trend (RW() should not include intercept by default)
  "default_rw" = list(
    formula = y ~ x,
    trend_formula = ~ RW(),
    should_have_intercept = FALSE
  ),
  
  # Should NOT have Intercept_trend  
  "minus_one_rw" = list(
    formula = y ~ x, 
    trend_formula = ~ -1 + RW(),
    should_have_intercept = FALSE
  ),
  
  # Should NOT have Intercept_trend
  "zero_rw" = list(
    formula = y ~ x,
    trend_formula = ~ 0 + RW(), 
    should_have_intercept = FALSE
  ),
  
  # Should NOT have Intercept_trend (AR() should not include intercept by default)
  "default_ar" = list(
    formula = y ~ x,
    trend_formula = ~ AR(),
    should_have_intercept = FALSE
  ),
  
  # Should NOT have Intercept_trend
  "minus_one_ar" = list(
    formula = y ~ x,
    trend_formula = ~ -1 + AR(),
    should_have_intercept = FALSE
  ),
  
  # Test the new pattern that SHOULD have intercept
  "explicit_intercept_rw" = list(
    formula = y ~ x,
    trend_formula = ~ 1 + RW(),
    should_have_intercept = TRUE
  )
)

for (test_name in names(intercept_tests)) {
  cat("Testing:", test_name, "\n")
  test_spec <- intercept_tests[[test_name]]
  
  tryCatch({
    # Generate Stan code
    stancode <- make_stancode(
      mvgam_formula(test_spec$formula, trend_formula = test_spec$trend_formula),
      data = test_data$univariate,
      family = poisson(),
      validate = FALSE  # Skip validation for debugging
    )
    
    # Extract intercept information
    intercept_info <- extract_intercept_info(stancode)
    
    # Store results
    results[[test_name]] <- list(
      test_spec = test_spec,
      intercept_info = intercept_info,
      stancode_length = nchar(stancode)
    )
    
    # Check expectation
    actual_has_intercept <- intercept_info$has_intercept_param
    expected_has_intercept <- test_spec$should_have_intercept
    
    if (actual_has_intercept == expected_has_intercept) {
      cat("  ✓ PASS: Intercept handling correct\n")
    } else {
      cat("  ✗ FAIL: Expected intercept =", expected_has_intercept, 
          ", but got =", actual_has_intercept, "\n")
      cat("    Problematic patterns found:\n")
      if (intercept_info$has_rep_vector_zero) {
        cat("      ✗ Found: rep_vector(0.0, N_trend)\n")
      }
      if (intercept_info$has_mu_trend_plus_intercept) {
        cat("      ✗ Found: mu_trend += Intercept_trend\n")
      }
      if (intercept_info$has_rep_vector_intercept) {
        cat("      ✓ Found: rep_vector(Intercept_trend, N_trend)\n")
      }
      cat("    mu_trend lines:\n")
      for (line in intercept_info$mu_trend_lines[1:min(3, length(intercept_info$mu_trend_lines))]) {
        cat("      ", line, "\n")
      }
    }
    
  }, error = function(e) {
    cat("  ERROR:", e$message, "\n")
    errors[[test_name]] <- e$message
  })
}

# =============================================================================
# TASK 2: Observation Model mu Verification  
# =============================================================================
cat("\n=== TASK 2: Observation Model mu Verification ===\n")

observation_tests <- list(
  # Basic fixed effects
  "fixed_effects" = list(
    formula = y ~ x,
    trend_formula = ~ RW()
  ),
  
  # Random effects  
  "random_effects" = list(
    formula = y ~ x + (1 | series),
    trend_formula = ~ RW()
  ),
  
  # Smooth effects
  "smooth_effects" = list(
    formula = y ~ s(x),
    trend_formula = ~ RW()  
  ),
  
  # GP effects (if implemented)
  "gp_effects" = list(
    formula = y ~ gp(x, k = 5),
    trend_formula = ~ RW()
  ),
  
  # Intercept only
  "intercept_only" = list(
    formula = y ~ 1,
    trend_formula = ~ RW()
  ),
  
  # No intercept observation
  "no_intercept_obs" = list(
    formula = y ~ -1 + x,
    trend_formula = ~ RW()
  )
)

for (test_name in names(observation_tests)) {
  cat("Testing:", test_name, "\n")
  test_spec <- observation_tests[[test_name]]
  
  tryCatch({
    # Skip gp test if function doesn't exist
    if (test_name == "gp_effects" && !exists("gp")) {
      cat("  SKIP: gp() function not available\n")
      next
    }
    
    # Generate Stan code
    stancode <- make_stancode(
      mvgam_formula(test_spec$formula, trend_formula = test_spec$trend_formula),
      data = test_data$univariate,
      family = poisson(),
      validate = FALSE
    )
    
    # Extract mu construction information
    mu_info <- extract_mu_construction_info(stancode)
    
    # Store results  
    results[[paste0("obs_", test_name)]] <- list(
      test_spec = test_spec,
      mu_info = mu_info,
      stancode_length = nchar(stancode)
    )
    
    cat("  ✓ Generated Stan code (", nchar(stancode), " chars)\n")
    cat("    mu patterns found:", length(mu_info$mu_plus_lines), "\n")
    if (length(mu_info$glm_patterns) > 0) {
      cat("    GLM optimization detected\n")
    }
    
  }, error = function(e) {
    cat("  ERROR:", e$message, "\n") 
    errors[[paste0("obs_", test_name)]] <- e$message
  })
}

# =============================================================================
# TASK 3: Multivariate Model mu Verification
# =============================================================================
cat("\n=== TASK 3: Multivariate Model mu Verification ===\n")

multivariate_tests <- list(
  # mvbind pattern
  "mvbind_shared" = list(
    formula = mvbind(count, biomass) ~ x,
    trend_formula = ~ RW(),
    expected_responses = c("count", "biomass")
  ),
  
  # mvbind with smooth
  "mvbind_smooth" = list(
    formula = mvbind(count, biomass) ~ s(x),
    trend_formula = ~ AR(),
    expected_responses = c("count", "biomass") 
  ),
  
  # bf() pattern with multiple responses
  "bf_multiple" = list(
    formula = bf(count ~ x) + bf(biomass ~ x),
    trend_formula = ~ RW(),
    expected_responses = c("count", "biomass")
  ),
  
  # Three response pattern
  "three_responses" = list(
    formula = mvbind(count, biomass, presence) ~ x,
    trend_formula = ~ RW(),
    expected_responses = c("count", "biomass", "presence")
  )
)

for (test_name in names(multivariate_tests)) {
  cat("Testing:", test_name, "\n")  
  test_spec <- multivariate_tests[[test_name]]
  
  tryCatch({
    # Generate Stan code
    stancode <- make_stancode(
      mvgam_formula(test_spec$formula, trend_formula = test_spec$trend_formula),
      data = test_data$univariate,
      family = c(poisson(), gaussian(), bernoulli())[1:length(test_spec$expected_responses)],
      validate = FALSE
    )
    
    # Extract mu construction with response names
    mu_info <- extract_mu_construction_info(stancode, test_spec$expected_responses)
    
    # Store results
    results[[paste0("mv_", test_name)]] <- list(
      test_spec = test_spec,
      mu_info = mu_info,
      stancode_length = nchar(stancode)
    )
    
    cat("  ✓ Generated Stan code (", nchar(stancode), " chars)\n")
    
    # Check response-specific patterns
    for (resp in test_spec$expected_responses) {
      resp_patterns <- mu_info$mu_response_patterns[[resp]]
      if (length(resp_patterns) > 0) {
        cat("    ✓ Found mu_", resp, " patterns:", length(resp_patterns), "\n")
      } else {
        cat("    ✗ Missing mu_", resp, " patterns\n")
      }
    }
    
  }, error = function(e) {
    cat("  ERROR:", e$message, "\n")
    errors[[paste0("mv_", test_name)]] <- e$message
  })
}

# =============================================================================
# TASK 4: Edge Case Verification
# =============================================================================
cat("\n=== TASK 4: Edge Case Verification ===\n")

edge_tests <- list(
  # No intercept both places
  "no_intercept_both" = list(
    formula = y ~ -1 + x,
    trend_formula = ~ -1 + RW()
  ),
  
  # Trend only model
  "trend_only" = list(
    formula = y ~ 1,
    trend_formula = ~ s(habitat) + AR()
  ),
  
  # Intercept only observation with trend intercept
  "intercept_obs_intercept_trend" = list(
    formula = y ~ 1,
    trend_formula = ~ 1 + RW()
  )
)

for (test_name in names(edge_tests)) {
  cat("Testing:", test_name, "\n")
  test_spec <- edge_tests[[test_name]]
  
  tryCatch({
    # Generate Stan code
    stancode <- make_stancode(
      mvgam_formula(test_spec$formula, trend_formula = test_spec$trend_formula),
      data = test_data$univariate,
      family = poisson(),
      validate = FALSE
    )
    
    # Extract information
    intercept_info <- extract_intercept_info(stancode)
    mu_info <- extract_mu_construction_info(stancode)
    
    # Store results
    results[[paste0("edge_", test_name)]] <- list(
      test_spec = test_spec,
      intercept_info = intercept_info,
      mu_info = mu_info,
      stancode_length = nchar(stancode)
    )
    
    cat("  ✓ Generated Stan code (", nchar(stancode), " chars)\n")
    
  }, error = function(e) {
    cat("  ERROR:", e$message, "\n")
    errors[[paste0("edge_", test_name)]] <- e$message
  })
}

# =============================================================================
# SUMMARY REPORT
# =============================================================================
cat("\n=== DIAGNOSTIC SUMMARY ===\n")

# Count successes and failures  
total_tests <- length(intercept_tests) + length(observation_tests) + length(multivariate_tests) + length(edge_tests)
successful_tests <- length(results)
failed_tests <- length(errors)

cat("Total tests attempted:", total_tests, "\n")
cat("Successful tests:", successful_tests, "\n") 
cat("Failed tests:", failed_tests, "\n")

if (failed_tests > 0) {
  cat("\nERRORS ENCOUNTERED:\n")
  for (test_name in names(errors)) {
    cat("  -", test_name, ":", errors[[test_name]], "\n")
  }
}

# Check intercept control issues
cat("\nINTERCEPT CONTROL ANALYSIS:\n")
intercept_issues <- list()
for (test_name in names(intercept_tests)) {
  if (test_name %in% names(results)) {
    result <- results[[test_name]]
    expected <- result$test_spec$should_have_intercept
    actual <- result$intercept_info$has_intercept_param
    
    if (expected != actual) {
      intercept_issues[[test_name]] <- list(
        expected = expected,
        actual = actual,
        formula = deparse(result$test_spec$trend_formula)
      )
    }
  }
}

if (length(intercept_issues) > 0) {
  cat("  INTERCEPT CONTROL FAILURES:\n")
  for (issue_name in names(intercept_issues)) {
    issue <- intercept_issues[[issue_name]]
    cat("    -", issue_name, ":", issue$formula, 
        "expected =", issue$expected, ", got =", issue$actual, "\n")
  }
} else {
  cat("  ✓ No intercept control issues detected\n")
}

# Check multivariate mu patterns
cat("\nMULTIVARIATE MU PATTERN ANALYSIS:\n")
mv_issues <- list()
for (test_name in grep("^mv_", names(results), value = TRUE)) {
  result <- results[[test_name]]
  expected_responses <- result$test_spec$expected_responses
  
  for (resp in expected_responses) {
    resp_patterns <- result$mu_info$mu_response_patterns[[resp]]
    if (length(resp_patterns) == 0) {
      mv_issues[[paste0(test_name, "_", resp)]] <- paste0("Missing mu_", resp, " patterns")
    }
  }
}

if (length(mv_issues) > 0) {
  cat("  MULTIVARIATE ISSUES:\n")
  for (issue_name in names(mv_issues)) {
    cat("    -", issue_name, ":", mv_issues[[issue_name]], "\n")
  }
} else {
  cat("  ✓ No multivariate mu pattern issues detected\n")
}

# Save results for further analysis
cat("\nSaving results to tasks/mu_generation_diagnosis_results.rds\n")
saveRDS(list(
  results = results,
  errors = errors,
  intercept_issues = intercept_issues,
  mv_issues = mv_issues,
  test_data = test_data
), "tasks/mu_generation_diagnosis_results.rds")

cat("Diagnostic complete. Check results object for detailed analysis.\n")