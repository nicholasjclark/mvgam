# GP Prediction Validation Script (Simple brms model)
# Tests that mvgam GP prediction infrastructure matches brms
#
# CRITICAL: Always start with devtools::load_all()

devtools::load_all()

library(brms)
library(posterior)

cat("\n")
cat(strrep("=", 78), "\n")
cat("GP PREDICTION VALIDATION (Simple brms Model)\n")
cat(strrep("=", 78), "\n")

# Create simple dataset
cat("\n[1/5] Creating test dataset...\n")
set.seed(123)
n <- 20
test_data <- data.frame(
  y = rnorm(n, mean = 5, sd = 2),
  x = seq(0, 10, length.out = n)
)
cat("    ✓ Created", n, "observations\n")

# Fit simple brms model with GP
cat("\n[2/5] Fitting brms model with GP (this may take a moment)...\n")
suppressMessages(
  fit_brms <- brm(
    y ~ gp(x, k = 5),
    data = test_data,
    family = gaussian(),
    backend = "cmdstanr",
    chains = 2,
    iter = 500,
    warmup = 250,
    refresh = 0,
    silent = 2
  )
)
cat("    ✓ Model fitted\n")

# Create newdata
cat("\n[3/5] Creating newdata for predictions...\n")
newdata <- test_data[1:6, ]
cat("    ✓ Using", nrow(newdata), "observations\n")

# Get brms reference predictions
cat("\n[4/5] Computing brms predictions...\n")
brms_pred <- posterior_linpred(fit_brms, newdata = newdata, summary = FALSE)
cat("    ✓ brms predictions: ", nrow(brms_pred), "draws ×",
    ncol(brms_pred), "obs\n")

# Get mvgam predictions using our infrastructure
cat("\n[5/5] Computing mvgam predictions...\n")

# Use brms prepare_predictions directly (it should work)
prep <- brms::prepare_predictions(fit_brms, newdata = newdata)
cat("    ✓ Prep object created\n")

# Check for GP terms
gp_info <- mvgam:::detect_gp_terms(prep)
if (!is.null(gp_info)) {
  cat("    ✓ Detected", gp_info$n_terms, "GP term(s):",
      paste(gp_info$suffixes, collapse = ", "), "\n")
} else {
  stop("No GP terms detected - this is unexpected")
}

# Compute using our function
mvgam_pred <- mvgam:::extract_linpred_from_prep(prep)
cat("    ✓ mvgam predictions:", nrow(mvgam_pred), "draws ×",
    ncol(mvgam_pred), "obs\n")

# Compare predictions
cat("\n")
cat(strrep("=", 78), "\n")
cat("VALIDATION RESULTS\n")
cat(strrep("=", 78), "\n\n")

# Check dimensions
dims_match <- all(dim(mvgam_pred) == dim(brms_pred))
cat("[Dimension Check]\n")
cat("  mvgam:", nrow(mvgam_pred), "×", ncol(mvgam_pred), "\n")
cat("  brms: ", nrow(brms_pred), "×", ncol(brms_pred), "\n")
cat("  Match:", if (dims_match) "✓ YES" else "✗ NO", "\n\n")

if (!dims_match) {
  stop("Dimension mismatch")
}

# Calculate differences
abs_diff <- abs(mvgam_pred - brms_pred)
max_abs_diff <- max(abs_diff)
mean_abs_diff <- mean(abs_diff)

cat("[Numerical Comparison]\n")
cat("  Max absolute difference: ", sprintf("%.2e", max_abs_diff), "\n")
cat("  Mean absolute difference:", sprintf("%.2e", mean_abs_diff), "\n\n")

# Test thresholds
pass_1e6 <- max_abs_diff < 1e-6
pass_1e8 <- max_abs_diff < 1e-8
pass_1e10 <- max_abs_diff < 1e-10

cat("[Precision Tests]\n")
cat("  < 1e-6:  ", if (pass_1e6) "✓ PASS" else "✗ FAIL", "\n")
cat("  < 1e-8:  ", if (pass_1e8) "✓ PASS" else "✗ FAIL", "\n")
cat("  < 1e-10: ", if (pass_1e10) "✓ PASS" else "✗ FAIL", "\n\n")

# Overall result
cat(strrep("=", 78), "\n")
if (pass_1e6) {
  cat("✓ VALIDATION PASSED: GP predictions match brms within tolerance\n")
  cat(strrep("=", 78), "\n\n")
  cat("GP prediction implementation is correct!\n\n")
} else {
  cat("✗ VALIDATION FAILED\n")
  cat(strrep("=", 78), "\n\n")

  # Show worst cases
  cat("\n[Worst Mismatches]\n")
  worst_idx <- order(abs_diff, decreasing = TRUE)[1:min(5, length(abs_diff))]
  for (idx in worst_idx) {
    draw <- ((idx - 1) %/% ncol(mvgam_pred)) + 1
    obs <- ((idx - 1) %% ncol(mvgam_pred)) + 1
    cat(sprintf("  Draw %d, Obs %d: diff=%.2e\n", draw, obs, abs_diff[idx]))
  }

  stop("Validation failed")
}
