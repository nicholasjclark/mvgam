#!/usr/bin/env Rscript
# Debug mu_trend initialization across trend types and formula patterns
# Usage: Rscript debug_mu_trend.R

devtools::load_all()
library(dplyr)

# Create test data
set.seed(123)
test_data <- data.frame(
  y = rpois(100, 5),
  x = rnorm(100),
  z = rnorm(100),
  time = rep(1:50, 2),
  series = factor(rep(c("A", "B"), each = 50))
)

# Test cases: various trend_formula patterns
test_cases <- list(
  # Intercept-only cases (should use Intercept_trend)
  list(name = "CAR_intercept_only",
       formula = y ~ x,
       trend_formula = ~ CAR(),
       expected_mu = "rep_vector(Intercept_trend, N_trend)"),

  list(name = "AR_intercept_only",
       formula = y ~ x,
       trend_formula = ~ AR(),
       expected_mu = "rep_vector(Intercept_trend, N_trend)"),

  list(name = "RW_intercept_only",
       formula = y ~ x,
       trend_formula = ~ RW(),
       expected_mu = "rep_vector(Intercept_trend, N_trend)"),

  # With linear predictors (should use 0.0 + linear predictor)
  list(name = "CAR_with_predictor",
       formula = y ~ x,
       trend_formula = ~ z + CAR(),
       expected_mu = "rep_vector(0.0, N_trend).*Intercept_trend.*Xc_trend.*b_trend"),

  list(name = "AR_with_predictor",
       formula = y ~ x,
       trend_formula = ~ z + AR(),
       expected_mu = "rep_vector(0.0, N_trend).*Intercept_trend.*Xc_trend.*b_trend"),

  # With spline terms (should use 0.0 + spline predictors) - CRITICAL DIFFERENT PATTERN!
  list(name = "CAR_with_spline",
       formula = y ~ x,
       trend_formula = ~ s(z) + CAR(),
       expected_mu = "rep_vector(0.0, N_trend).*Intercept_trend.*(Xs_trend.*bs_trend|Zs_trend.*s_trend)"),

  list(name = "AR_with_spline",
       formula = y ~ x,
       trend_formula = ~ s(z) + AR(),
       expected_mu = "rep_vector(0.0, N_trend).*Intercept_trend.*(Xs_trend.*bs_trend|Zs_trend.*s_trend)"),

  list(name = "VAR_with_spline",
       formula = y ~ x,
       trend_formula = ~ s(z) + VAR(),
       expected_mu = "rep_vector(0.0, N_trend).*Intercept_trend.*(Xs_trend.*bs_trend|Zs_trend.*s_trend)"),

  # No intercept (should use 0.0 only)
  list(name = "CAR_no_intercept",
       formula = y ~ x,
       trend_formula = ~ -1 + z + CAR(),
       expected_mu = "rep_vector(0.0, N_trend)"),

  list(name = "AR_no_intercept",
       formula = y ~ x,
       trend_formula = ~ -1 + z + AR(),
       expected_mu = "rep_vector(0.0, N_trend)"),

  list(name = "CAR_no_intercept_spline",
       formula = y ~ x,
       trend_formula = ~ -1 + s(z) + CAR(),
       expected_mu = "rep_vector(0.0, N_trend)"),

  # Complex cases
  list(name = "CAR_interaction",
       formula = y ~ x,
       trend_formula = ~ z * x + CAR(),
       expected_mu = "rep_vector(0.0, N_trend).*Intercept_trend.*Xc_trend.*b_trend"),

  list(name = "VAR_multivariate_spline",
       formula = y ~ x,
       trend_formula = ~ s(z) + VAR(p = 2),
       expected_mu = "rep_vector(0.0, N_trend).*Intercept_trend.*(Xs_trend.*bs_trend|Zs_trend.*s_trend)")
)

# Debugging functions
extract_mu_trend_line <- function(stancode) {
  lines <- strsplit(stancode, "\n")[[1]]
  mu_trend_lines <- grep("mu.*_trend.*=.*rep_vector", lines, value = TRUE)
  if (length(mu_trend_lines) == 0) {
    return("NOT_FOUND")
  }
  # Clean up whitespace and return first match
  trimws(mu_trend_lines[1])
}

check_coefficient_detection <- function(stancode, suffix = "_trend") {
  has_b_coef <- grepl(paste0("vector\\[.*\\]\\s+b", suffix), stancode)
  has_x_matrix <- grepl(paste0("matrix\\[.*\\]\\s+X", suffix), stancode)
  has_coefficients <- has_b_coef && has_x_matrix
  intercept_present <- grepl(paste0("real\\s+Intercept", suffix), stancode)

  list(
    has_b_coef = has_b_coef,
    has_x_matrix = has_x_matrix,
    has_coefficients = has_coefficients,
    intercept_present = intercept_present
  )
}

# Function to safely generate stancode
safe_stancode <- function(formula, trend_formula, data, family = poisson()) {
  tryCatch({
    mf <- mvgam_formula(formula, trend_formula = trend_formula)
    stancode(mf, data = data, family = family, validate = FALSE)
  }, error = function(e) {
    paste("ERROR:", e$message)
  })
}

# Run debugging analysis
cat("=== MU_TREND INITIALIZATION DEBUGGING ===\n\n")

results <- list()

for (i in seq_along(test_cases)) {
  test_case <- test_cases[[i]]
  cat(sprintf("Test %d: %s\n", i, test_case$name))
  cat(sprintf("Formula: %s\n", deparse(test_case$formula)))
  cat(sprintf("Trend Formula: %s\n", deparse(test_case$trend_formula)))

  # Generate stancode
  stancode_result <- safe_stancode(
    test_case$formula,
    test_case$trend_formula,
    test_data
  )

  if (grepl("^ERROR:", stancode_result)) {
    cat(sprintf("FAILED: %s\n\n", stancode_result))
    results[[test_case$name]] <- list(status = "FAILED", error = stancode_result)
    next
  }

  # Extract mu_trend line
  mu_trend_line <- extract_mu_trend_line(stancode_result)

  # Check detection logic
  detection <- check_coefficient_detection(stancode_result)

  # Determine expected behavior based on detection
  if (detection$has_coefficients) {
    expected_pattern <- "rep_vector\\(0\\.0.*mu.*\\+=.*Intercept.*\\+.*Xc.*\\*.*b"
    logic_path <- "has_coefficients=TRUE"
  } else if (detection$intercept_present) {
    expected_pattern <- "rep_vector\\(Intercept_trend, N_trend\\)"
    logic_path <- "intercept_only"
  } else {
    expected_pattern <- "rep_vector\\(0\\.0, N_trend\\)"
    logic_path <- "no_intercept"
  }

  # Check if result matches expectation
  matches_expected <- grepl(expected_pattern, mu_trend_line)

  # Print results
  cat(sprintf("Detection Logic:\n"))
  cat(sprintf("  has_b_coef: %s\n", detection$has_b_coef))
  cat(sprintf("  has_x_matrix: %s\n", detection$has_x_matrix))
  cat(sprintf("  has_coefficients: %s\n", detection$has_coefficients))
  cat(sprintf("  intercept_present: %s\n", detection$intercept_present))
  cat(sprintf("  logic_path: %s\n", logic_path))

  cat(sprintf("Generated mu_trend: %s\n", mu_trend_line))
  cat(sprintf("Expected pattern: %s\n", expected_pattern))
  cat(sprintf("RESULT: %s\n", if (matches_expected) "PASS" else "FAIL"))

  if (!matches_expected) {
    cat("*** MISMATCH DETECTED ***\n")
    # Show relevant Stan code sections
    cat("Relevant Stan parameters:\n")
    param_lines <- grep("(Intercept_trend|b_trend|X_trend)",
                       strsplit(stancode_result, "\n")[[1]], value = TRUE)
    for (line in param_lines[1:min(5, length(param_lines))]) {
      cat(sprintf("  %s\n", trimws(line)))
    }
  }

  cat(paste(c("\n", rep("=", 50), "\n\n"), collapse = ""))

  # Store results
  results[[test_case$name]] <- list(
    status = if (matches_expected) "PASS" else "FAIL",
    mu_trend_line = mu_trend_line,
    detection = detection,
    logic_path = logic_path,
    expected_pattern = expected_pattern
  )
}

# Summary
cat("=== SUMMARY ===\n")
passed <- sum(sapply(results, function(x) x$status == "PASS"))
failed <- sum(sapply(results, function(x) x$status == "FAIL"))
errored <- sum(sapply(results, function(x) x$status == "FAILED"))

cat(sprintf("Total tests: %d\n", length(results)))
cat(sprintf("Passed: %d\n", passed))
cat(sprintf("Failed: %d\n", failed))
cat(sprintf("Errored: %d\n", errored))

if (failed > 0) {
  cat("\nFAILED TESTS:\n")
  failed_names <- names(results)[sapply(results, function(x) x$status == "FAIL")]
  for (name in failed_names) {
    result <- results[[name]]
    cat(sprintf("- %s: %s (expected: %s)\n",
               name, result$mu_trend_line, result$expected_pattern))
  }
}

# Save detailed results for further analysis
saveRDS(results, "mu_trend_debug_results.rds")
cat("\nDetailed results saved to: mu_trend_debug_results.rds\n")
