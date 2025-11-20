# GLM Trend Injection Debug Script
# Tests our new system without testthat to avoid segmentation faults

library(devtools)
devtools::load_all()
library(brms)

# Helper function for pattern matching (exact copy from working test file)
stan_pattern <- function(pattern, x, ignore.case = FALSE, ...) {
  # Function to remove ALL whitespace for robust matching
  remove_whitespace <- function(text) {
    gsub("\\s+", "", text)
  }

  # Remove all whitespace from both pattern and input
  x_no_space <- remove_whitespace(x)

  # Check if pattern contains regex escapes (\\[ \\] \\( \\) etc.)
  # If so, remove whitespace but don't double-escape
  # If not, remove whitespace and escape for literal matching
  if (grepl("\\\\[\\[\\]()\\{\\}^$\\*\\+\\?\\.|]", pattern)) {
    # Pattern is already regex-escaped, just remove whitespace
    pattern_final <- remove_whitespace(pattern)
  } else {
    # Pattern is literal Stan code, remove whitespace then escape metacharacters
    pattern_no_space <- remove_whitespace(pattern)
    pattern_final <- gsub("([\\[\\]()\\{\\}^$\\*\\+\\?\\.|\\\\])", "\\\\\\1", pattern_no_space)
  }

  # Apply grepl with processed pattern and whitespace-free input
  grepl(pattern_final, x_no_space, ignore.case = ignore.case, ...)
}

# Test data setup
cat("Setting up test data...\n")
set.seed(123)
n_time <- 50
data_simple <- data.frame(
  y = rpois(n_time, exp(rnorm(n_time, 0, 0.5))),
  x = rnorm(n_time),
  time = 1:n_time,
  series = 1
)

cat("Data created with", nrow(data_simple), "observations\n\n")

# Test 1: Simple GLM with RW trend (basic case that should work)
cat("=== TEST 1: Simple GLM with RW trend ===\n")
tryCatch({
  mf_simple <- mvgam_formula(y ~ x, trend_formula = ~ RW())
  code_simple <- stancode(mf_simple, data = data_simple, family = poisson(), validate = FALSE)
  
  cat("✓ Stan code generated successfully\n")
  
  # Look at what was actually generated
  cat("\n--- GENERATED STAN CODE SAMPLE ---\n")
  model_block_start <- regexpr("model \\{", code_simple)
  if (model_block_start > 0) {
    # Show ~200 characters from model block
    sample_start <- model_block_start
    sample_end <- min(nchar(code_simple), model_block_start + 500)
    code_sample <- substr(code_simple, sample_start, sample_end)
    cat(code_sample)
    cat("\n--- END SAMPLE ---\n")
  }
  
  # Check for expected patterns with simpler tests
  has_mu_vector <- grepl("vector\\[N\\] mu_", code_simple)
  has_glm_call <- grepl("poisson_log_glm_lpmf", code_simple)
  has_to_matrix <- grepl("to_matrix", code_simple)
  has_trend_injection <- grepl("trend\\[", code_simple)
  
  cat("Has mu vector declaration:", has_mu_vector, "\n")
  cat("Has GLM call:", has_glm_call, "\n") 
  cat("Has to_matrix format:", has_to_matrix, "\n")
  cat("Has trend injection:", has_trend_injection, "\n")
  
  if (has_mu_vector && has_to_matrix) {
    cat("✓ TEST 1 PASSED: Basic GLM trend injection working\n")
  } else {
    cat("✗ TEST 1 FAILED: Missing expected patterns\n")
  }
  
}, error = function(e) {
  cat("✗ TEST 1 ERROR:", e$message, "\n")
})

cat("\n")

# Test 2: GLM without trends (should preserve GLM optimization)
cat("=== TEST 2: GLM without trends ===\n")
tryCatch({
  mf_no_trend <- mvgam_formula(y ~ x)
  code_no_trend <- stancode(mf_no_trend, data = data_simple, family = poisson(), validate = FALSE)
  
  cat("✓ Stan code generated successfully\n")
  
  # Should have original GLM call, not to_matrix format
  has_original_glm <- stan_pattern("poisson_log_glm_lpmf(Y | Xc, Intercept, b)", code_no_trend)
  has_to_matrix <- stan_pattern("to_matrix", code_no_trend)
  
  cat("Has original GLM call:", has_original_glm, "\n")
  cat("Has to_matrix (should be FALSE):", has_to_matrix, "\n")
  
  if (has_original_glm && !has_to_matrix) {
    cat("✓ TEST 2 PASSED: GLM optimization preserved\n")  
  } else {
    cat("✗ TEST 2 FAILED: GLM optimization not preserved\n")
  }
  
}, error = function(e) {
  cat("✗ TEST 2 ERROR:", e$message, "\n")
})

cat("\n")

# Test 3: Multivariate data (more complex case)
cat("=== TEST 3: Multivariate GLM with trends ===\n")
tryCatch({
  # Create multivariate data
  n_time <- 30
  data_mv <- data.frame(
    count = rpois(n_time, 5),
    biomass = rnorm(n_time, 10, 2),
    x = rnorm(n_time),
    presence = rbinom(n_time, 1, 0.5),
    time = 1:n_time,
    series = 1
  )
  
  mf_mv <- mvgam_formula(
    bf(mvbind(count, biomass) ~ x) + set_rescor(FALSE),
    trend_formula = ~ presence + RW()
  )
  
  code_mv <- stancode(mf_mv, data = data_mv, validate = FALSE)
  
  cat("✓ Multivariate Stan code generated successfully\n")
  
  # Check for response-specific patterns
  has_count_mu <- stan_pattern("vector[N] mu_count", code_mv)
  has_biomass_mu <- stan_pattern("vector[N] mu_biomass", code_mv)
  has_count_glm <- stan_pattern("to_matrix(mu_count)", code_mv)
  has_biomass_glm <- stan_pattern("to_matrix(mu_biomass)", code_mv)
  
  cat("Has count mu vector:", has_count_mu, "\n")
  cat("Has biomass mu vector:", has_biomass_mu, "\n")
  cat("Has count GLM transformation:", has_count_glm, "\n")
  cat("Has biomass GLM transformation:", has_biomass_glm, "\n")
  
  if (has_count_mu && has_biomass_mu) {
    cat("✓ TEST 3 PASSED: Multivariate GLM trend injection working\n")
  } else {
    cat("✗ TEST 3 FAILED: Multivariate processing incomplete\n")
  }
  
}, error = function(e) {
  cat("✗ TEST 3 ERROR:", e$message, "\n")
})

cat("\n")

# Test 4: Check what specific patterns are missing in complex models
cat("=== TEST 4: Pattern analysis for complex VAR model ===\n")
tryCatch({
  # This is the type that was failing in the test suite
  n_time <- 20
  data_var <- data.frame(
    count = rpois(n_time, 3),
    biomass = rnorm(n_time, 5, 1),
    x = rnorm(n_time),
    presence = rbinom(n_time, 1, 0.6),
    time = 1:n_time,
    series = 1
  )
  
  mf_var <- mvgam_formula(
    bf(mvbind(count, biomass) ~ s(x)) + set_rescor(FALSE),
    trend_formula = ~ presence + VAR(p = 2, ma = TRUE)
  )
  
  code_var <- stancode(mf_var, data = data_var, validate = FALSE)
  
  cat("✓ VAR model Stan code generated successfully\n")
  
  # Check for patterns that tests expect
  patterns_to_check <- list(
    "sqrtm function" = "matrix sqrtm\\(matrix A\\)",
    "AtoP function" = "matrix AtoP\\(matrix P_real\\)", 
    "rev_mapping function" = "array\\[,\\] matrix\\[,\\] rev_mapping\\(",
    "Heaps methodology" = "Following Heaps 2022 methodology",
    "trend dimensions" = "int<lower=1> N_trend",
    "trend design matrix" = "matrix\\[N_trend, K_trend\\] X_trend"
  )
  
  cat("Checking expected patterns:\n")
  for (name in names(patterns_to_check)) {
    pattern <- patterns_to_check[[name]]
    found <- stan_pattern(pattern, code_var)
    cat(sprintf("  %-20s: %s\n", name, ifelse(found, "✓ FOUND", "✗ MISSING")))
  }
  
}, error = function(e) {
  cat("✗ TEST 4 ERROR:", e$message, "\n")
})

cat("\n=== SUMMARY ===\n")
cat("Debug script completed. Check results above for:\n")
cat("1. Basic GLM trend injection functionality\n") 
cat("2. GLM optimization preservation\n")
cat("3. Multivariate model support\n")
cat("4. Complex model pattern matching\n")