# GP Prediction Validation Script
# Tests that mvgam GP predictions exactly match brms::posterior_linpred()
#
# CRITICAL: Always start with devtools::load_all()

devtools::load_all()

library(brms)
library(posterior)

cat("\n")
cat(strrep("=", 78), "\n")
cat("GP PREDICTION VALIDATION\n")
cat(strrep("=", 78), "\n")

# Load test model with GP terms
cat("\n[1/6] Loading fit8 (has GP in observation and trend formulas)...\n")
fit8 <- readRDS("tasks/fixtures/fit8.rds")
cat("    ✓ Model loaded\n")
cat("    - Observation formula:", deparse(formula(fit8$obs_model)[[3]]), "\n")
cat("    - Trend formula:",
    if (!is.null(fit8$trend_model)) {
      deparse(formula(fit8$trend_model)[[3]])
    } else {
      "NULL"
    }, "\n")

# Create newdata for predictions (use subset of training data)
cat("\n[2/6] Creating newdata...\n")
training_data <- fit8$obs_model$data
newdata <- training_data[1:6, ]  # Use first 6 observations
cat("    ✓ Using", nrow(newdata), "observations for validation\n")

# Extract observation parameters
cat("\n[3/6] Extracting observation parameters...\n")
obs_params <- extract_obs_parameters(fit8)
cat("    ✓ Extracted", length(obs_params), "observation parameters\n")

# Create observation prep object using our S3 method
cat("\n[4/6] Creating prep object for observation model...\n")
obs_draws <- posterior::subset_draws(
  posterior::as_draws(fit8$fit),
  variable = obs_params
)
# Convert to draws_matrix for create_mock_stanfit
obs_draws <- posterior::as_draws_matrix(obs_draws)
mock_obs <- create_mock_stanfit(obs_draws)

prep_obs <- prepare_predictions(
  mock_obs,
  brmsfit = fit8$obs_model,
  newdata = newdata
)
cat("    ✓ Prep object created\n")

# Check if GP terms are detected
cat("\n    Checking for GP terms in prep object...\n")
gp_info <- mvgam:::detect_gp_terms(prep_obs)
if (!is.null(gp_info)) {
  cat("    ✓ Detected", gp_info$n_terms, "GP term(s):\n")
  for (suffix in gp_info$suffixes) {
    cat("      - Suffix:", suffix, "\n")
  }
} else {
  cat("    ⚠ No GP terms detected (unexpected for fit8)\n")
}

# Compute linear predictor using our implementation
cat("\n[5/6] Computing linear predictor with mvgam...\n")
mvgam_linpred <- mvgam:::extract_linpred_from_prep(prep_obs)
cat("    ✓ Computed predictions\n")
cat("    - Dimensions:", nrow(mvgam_linpred), "draws ×",
    ncol(mvgam_linpred), "observations\n")

# Get reference predictions from brms
cat("\n[6/6] Computing reference predictions with brms...\n")
brms_linpred <- posterior_linpred(
  fit8$obs_model,
  newdata = newdata,
  summary = FALSE
)
cat("    ✓ brms predictions computed\n")
cat("    - Dimensions:", nrow(brms_linpred), "draws ×",
    ncol(brms_linpred), "observations\n")

# Compare predictions
cat("\n")
cat(strrep("=", 78), "\n")
cat("VALIDATION RESULTS\n")
cat(strrep("=", 78), "\n\n")

# Check dimensions match
dims_match <- all(dim(mvgam_linpred) == dim(brms_linpred))
cat("[Dimension Check]\n")
cat("  mvgam:", nrow(mvgam_linpred), "×", ncol(mvgam_linpred), "\n")
cat("  brms: ", nrow(brms_linpred), "×", ncol(brms_linpred), "\n")
cat("  Match:", if (dims_match) "✓ YES" else "✗ NO", "\n\n")

if (!dims_match) {
  stop("Dimension mismatch - cannot compare predictions")
}

# Calculate differences
abs_diff <- abs(mvgam_linpred - brms_linpred)
max_abs_diff <- max(abs_diff)
mean_abs_diff <- mean(abs_diff)
rel_diff <- abs_diff / (abs(brms_linpred) + 1e-10)
max_rel_diff <- max(rel_diff)

cat("[Numerical Comparison]\n")
cat("  Max absolute difference:", sprintf("%.2e", max_abs_diff), "\n")
cat("  Mean absolute difference:", sprintf("%.2e", mean_abs_diff), "\n")
cat("  Max relative difference:", sprintf("%.2e", max_rel_diff), "\n\n")

# Test precision thresholds
threshold_1e6 <- max_abs_diff < 1e-6
threshold_1e8 <- max_abs_diff < 1e-8
threshold_1e10 <- max_abs_diff < 1e-10

cat("[Precision Tests]\n")
cat("  < 1e-6:  ", if (threshold_1e6) "✓ PASS" else "✗ FAIL", "\n")
cat("  < 1e-8:  ", if (threshold_1e8) "✓ PASS" else "✗ FAIL", "\n")
cat("  < 1e-10: ", if (threshold_1e10) "✓ PASS" else "✗ FAIL", "\n\n")

# Summary statistics
cat("[Summary Statistics]\n")
cat("  mvgam predictions:\n")
cat("    Range: [", sprintf("%.4f", min(mvgam_linpred)), ", ",
    sprintf("%.4f", max(mvgam_linpred)), "]\n", sep = "")
cat("    Mean:  ", sprintf("%.4f", mean(mvgam_linpred)), "\n")
cat("    SD:    ", sprintf("%.4f", sd(as.vector(mvgam_linpred))), "\n\n")

cat("  brms predictions:\n")
cat("    Range: [", sprintf("%.4f", min(brms_linpred)), ", ",
    sprintf("%.4f", max(brms_linpred)), "]\n", sep = "")
cat("    Mean:  ", sprintf("%.4f", mean(brms_linpred)), "\n")
cat("    SD:    ", sprintf("%.4f", sd(as.vector(brms_linpred))), "\n\n")

# Overall result
cat(strrep("=", 78), "\n")
if (threshold_1e6) {
  cat("✓ VALIDATION PASSED: GP predictions match brms within tolerance\n")
  cat(strrep("=", 78), "\n\n")
} else {
  cat("✗ VALIDATION FAILED: Differences exceed 1e-6 threshold\n")
  cat(strrep("=", 78), "\n\n")

  # Show worst cases
  cat("\n[Worst Mismatches]\n")
  worst_idx <- order(abs_diff, decreasing = TRUE)[1:min(5, length(abs_diff))]
  for (idx in worst_idx) {
    draw <- ((idx - 1) %/% ncol(mvgam_linpred)) + 1
    obs <- ((idx - 1) %% ncol(mvgam_linpred)) + 1
    cat(sprintf("  Draw %d, Obs %d: mvgam=%.6f, brms=%.6f, diff=%.2e\n",
                draw, obs,
                mvgam_linpred[draw, obs],
                brms_linpred[draw, obs],
                abs_diff[idx]))
  }
  cat("\n")

  stop("GP prediction validation failed")
}

# Success
cat("All GP prediction tests passed!\n\n")
