# ZMVN Single vs Multi-Series Investigation
# =========================================
# Testing if ZMVN correctly adapts correlation behavior based on number of series

devtools::load_all()

cat("=== ZMVN SINGLE VS MULTI-SERIES INVESTIGATION ===\n\n")

# Single series data
test_data_single <- data.frame(
  y = rpois(20, 5),
  x = rnorm(20),
  time = 1:20,
  series = factor("A")  # Only one series
)

# Multiple series data  
test_data_multi <- data.frame(
  y = rpois(40, 5),
  x = rnorm(40),
  time = rep(1:20, 2),
  series = factor(rep(c("A", "B"), each = 20))  # Two series
)

cat("1. ZMVN Constructor Behavior\n")
cat("---------------------------\n")
zmvn_obj <- ZMVN()
cat("ZMVN constructor cor parameter:", zmvn_obj$cor, "\n")
cat("ZMVN constructor monitor_params:", paste(zmvn_obj$monitor_params, collapse = ", "), "\n")
cat("Note: Constructor always sets cor=TRUE regardless of data context\n\n")

cat("2. Single Series Context\n")
cat("------------------------\n")
mf_single <- mvgam_formula(y ~ x, trend_formula = ~ ZMVN())
priors_single <- get_prior(mf_single, data = test_data_single, family = poisson())
trend_priors_single <- priors_single[priors_single$trend_component == "trend", ]
params_single <- sort(unique(trend_priors_single$class))

cat("Single series (n=1) parameters:", paste(params_single, collapse = ", "), "\n")
cat("Has L_Omega_trend:", "L_Omega_trend" %in% params_single, "\n")
cat("Expected behavior: Should NOT have L_Omega_trend (no correlations needed)\n\n")

cat("3. Multiple Series Context\n")
cat("--------------------------\n")  
mf_multi <- mvgam_formula(y ~ x, trend_formula = ~ ZMVN())
priors_multi <- get_prior(mf_multi, data = test_data_multi, family = poisson())
trend_priors_multi <- priors_multi[priors_multi$trend_component == "trend", ]
params_multi <- sort(unique(trend_priors_multi$class))

cat("Multiple series (n=2) parameters:", paste(params_multi, collapse = ", "), "\n")
cat("Has L_Omega_trend:", "L_Omega_trend" %in% params_multi, "\n")
cat("Expected behavior: Should have L_Omega_trend (correlations between series)\n\n")

cat("4. Analysis\n")
cat("-----------\n")
single_has_corr <- "L_Omega_trend" %in% params_single
multi_has_corr <- "L_Omega_trend" %in% params_multi

if (single_has_corr && multi_has_corr) {
  cat("🚨 ISSUE: Both single and multi-series have correlations\n")
  cat("   The system is NOT adapting to single series context\n")
  cat("   Single series should use standard normal (no L_Omega_trend)\n")
} else if (!single_has_corr && multi_has_corr) {
  cat("✅ CORRECT: System adapts correlation behavior based on series count\n")
  cat("   Single series: standard normal\n") 
  cat("   Multi series: with correlations\n")
} else if (single_has_corr && !multi_has_corr) {
  cat("🚨 UNEXPECTED: Single has correlations but multi doesn't\n")
} else {
  cat("🚨 ISSUE: Neither context has correlations (unexpected for ZMVN)\n")
}

cat("\n5. Root Cause Analysis\n")
cat("----------------------\n")
cat("If ZMVN always generates L_Omega_trend regardless of series count:\n")
cat("- The issue is likely in the prior generation process\n") 
cat("- It's not checking the actual number of series in the data\n")
cat("- The hardcoded cor=TRUE in ZMVN constructor may be overriding data context\n\n")

cat("RECOMMENDATION:\n")
cat("- ZMVN trend generator should check n_series from data dimensions\n")
cat("- If n_series == 1: skip correlation parameters\n")  
cat("- If n_series > 1: include correlation parameters\n")
cat("- This logic should be in the trend stanvar generation, not constructor\n")