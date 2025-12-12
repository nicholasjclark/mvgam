# Debug script for ordinal posterior_epred issue
# Error: Don't know how to transform an object of class 'NULL' to any supported
# draws format.

devtools::load_all()
library(brms)

# Load the ordinal model from cache
mvgam_ord <- readRDS("tasks/fixtures/val_mvgam_cumulative_fx.rds")

# Inspect model structure
cat("=== Model Structure Inspection ===\n\n")

cat("1. Model class:", paste(class(mvgam_ord), collapse = ", "), "\n\n")

cat("2. Top-level names:\n")
print(names(mvgam_ord))

cat("\n3. model_output check:\n")
cat("   Is NULL:", is.null(mvgam_ord$model_output), "\n")
cat("   Class:", paste(class(mvgam_ord$model_output), collapse = ", "), "\n")

cat("\n4. Family check:\n")
cat("   Family name:", mvgam_ord$family$family, "\n")
cat("   Is ordinal:", is_ordinal_family(mvgam_ord$family), "\n")

# Check where draws might be stored
cat("\n5. Alternative draw storage locations:\n")

if (!is.null(mvgam_ord$fit)) {
  cat("   $fit exists, class:", paste(class(mvgam_ord$fit), collapse = ", "), "\n")
}

if (!is.null(mvgam_ord$backend)) {
  cat("   Backend:", mvgam_ord$backend, "\n")
}

# Check combined_fit
if (!is.null(mvgam_ord$combined_fit)) {
  cat("\n6. combined_fit structure:\n")
  cat("   Class:", paste(class(mvgam_ord$combined_fit), collapse = ", "), "\n")
}

# Try extracting draws from model_output
cat("\n7. Trying posterior::as_draws_matrix on model_output:\n")
tryCatch({
  draws <- posterior::as_draws_matrix(mvgam_ord$model_output)
  cat("   Success! Dims:", paste(dim(draws), collapse = " x "), "\n")
  cat("   First 10 columns:", paste(head(colnames(draws), 10), collapse = ", "),
      "\n")

  # Look for threshold columns
  thres_cols <- grep("^b_Intercept\\[", colnames(draws), value = TRUE)
  cat("   Threshold columns:", paste(thres_cols, collapse = ", "), "\n")
}, error = function(e) {
  cat("   Error:", conditionMessage(e), "\n")
})

# Test posterior_linpred
cat("\n8. Testing posterior_linpred:\n")
tryCatch({
  linpred <- posterior_linpred(mvgam_ord, ndraws = 10)
  cat("   Success! Dims:", paste(dim(linpred), collapse = " x "), "\n")
}, error = function(e) {
  cat("   Error:", conditionMessage(e), "\n")
})

# Test individual extraction functions
cat("\n9. Testing extract_ordinal_thresholds:\n")
tryCatch({
  # Debug the extraction step by step
  draws_mat <- posterior::as_draws_matrix(mvgam_ord)
  cat("   Draws extracted, dims:", paste(dim(draws_mat), collapse = " x "), "\n")

  all_cols <- colnames(draws_mat)
  cat("   Total columns:", length(all_cols), "\n")
  cat("   First 20 columns:\n")
  print(head(all_cols, 20))

  # Look for threshold pattern
  thres_pattern <- "^b_Intercept\\["
  thres_cols <- grep(thres_pattern, all_cols, value = TRUE)
  cat("   Threshold columns found:", length(thres_cols), "\n")
  if (length(thres_cols) > 0) {
    cat("   Threshold columns:", paste(thres_cols, collapse = ", "), "\n")
  }

  # Also try without the bracket pattern
  intercept_cols <- grep("Intercept", all_cols, value = TRUE)
  cat("   All Intercept-related columns:", paste(intercept_cols, collapse = ", "), "\n")

  thres <- extract_ordinal_thresholds(mvgam_ord, ndraws = 10)
  cat("   Success! Dims:", paste(dim(thres), collapse = " x "), "\n")
}, error = function(e) {
  cat("   Error:", conditionMessage(e), "\n")
})

cat("\n10. Testing extract_ordinal_disc:\n")
tryCatch({
  disc <- extract_ordinal_disc(mvgam_ord, ndraws = 10, nobs = 30)
  cat("   Success! Dims:", paste(dim(disc), collapse = " x "), "\n")
}, error = function(e) {
  cat("   Error:", conditionMessage(e), "\n")
})

cat("\n11. Testing posterior_epred:\n")
tryCatch({
  epred <- posterior_epred(mvgam_ord, ndraws = 10)
  cat("   Success! Dims:", paste(dim(epred), collapse = " x "), "\n")
}, error = function(e) {
  cat("   Error:", conditionMessage(e), "\n")
  cat("   Traceback:\n")
  traceback()
})
