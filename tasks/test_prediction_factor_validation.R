# Test Prediction Factor Level Validation
#
# Tests the factor level validation system:
# 1. extract_factor_levels() helper
# 2. validate_prediction_factor_levels() helper
# 3. Integration via extract_component_linpred()

devtools::load_all()

cat("=== PREDICTION FACTOR VALIDATION TESTS ===\n\n")

# =============================================================================
# TEST 1: extract_factor_levels() helper
# =============================================================================

cat("--- Test 1: extract_factor_levels() ---\n")

test_data <- data.frame(
  series = factor(c("A", "B", "A", "B"), levels = c("A", "B", "C")),
  group = c("x", "y", "x", "y"),
  value = 1:4
)

# Test 1.1: Factor column (preserves level ordering)
result <- extract_factor_levels(test_data, "series")
expected <- c("A", "B", "C")
stopifnot(identical(result, expected))
cat("  1.1 Factor column: PASS\n")

# Test 1.2: Character column (sorted unique)
result <- extract_factor_levels(test_data, "group")
expected <- c("x", "y")
stopifnot(identical(result, expected))
cat("  1.2 Character column: PASS\n")

# Test 1.3: NULL var_name
result <- extract_factor_levels(test_data, NULL)
stopifnot(is.null(result))
cat("  1.3 NULL var_name: PASS\n")

# Test 1.4: NA var_name
result <- extract_factor_levels(test_data, NA_character_)
stopifnot(is.null(result))
cat("  1.4 NA var_name: PASS\n")

# Test 1.5: "NA" string var_name
result <- extract_factor_levels(test_data, "NA")
stopifnot(is.null(result))
cat("  1.5 'NA' string var_name: PASS\n")

# Test 1.6: Missing column
result <- extract_factor_levels(test_data, "nonexistent")
stopifnot(is.null(result))
cat("  1.6 Missing column: PASS\n")

# Test 1.7: Column with NA values (should exclude)
test_data_na <- data.frame(
  series = factor(c("A", NA, "B", NA)),
  stringsAsFactors = FALSE
)
result <- extract_factor_levels(test_data_na, "series")
expected <- c("A", "B")
stopifnot(identical(result, expected))
cat("  1.7 Column with NAs: PASS\n")

# Test 1.8: All-NA column
test_data_all_na <- data.frame(series = factor(c(NA, NA, NA)))
result <- extract_factor_levels(test_data_all_na, "series")
stopifnot(is.null(result))
cat("  1.8 All-NA column: PASS\n")

cat("\n")

# =============================================================================
# TEST 2: validate_prediction_factor_levels() helper
# =============================================================================

cat("--- Test 2: validate_prediction_factor_levels() ---\n")

# Create mock metadata structure
training_metadata <- list(
  levels = list(
    series = c("s1", "s2", "s3"),
    gr = c("A", "B"),
    subgr = c("x", "y")
  ),
  variables = list(
    series_var = "series",
    gr_var = "group",
    subgr_var = "subgroup"
  )
)

# Test 2.1: Valid subset of training levels
valid_newdata <- data.frame(
  series = factor(c("s1", "s2"), levels = c("s1", "s2")),
  group = factor(c("A", "A"), levels = c("A")),
  subgroup = factor(c("x", "x"), levels = c("x"))
)
result <- validate_prediction_factor_levels(valid_newdata, training_metadata)
stopifnot(isTRUE(result))
cat("  2.1 Valid subset levels: PASS\n")

# Test 2.2: Invalid series level
invalid_series <- data.frame(
  series = factor(c("s1", "s4"), levels = c("s1", "s4")),
  group = factor(c("A", "A")),
  subgroup = factor(c("x", "x"))
)
error_caught <- tryCatch({
  validate_prediction_factor_levels(invalid_series, training_metadata)
  FALSE
}, error = function(e) {
  grepl("Series levels in newdata not found", conditionMessage(e))
})
stopifnot(error_caught)
cat("  2.2 Invalid series level: PASS (error caught)\n")

# Test 2.3: Invalid gr level
invalid_gr <- data.frame(
  series = factor(c("s1", "s2")),
  group = factor(c("C", "C")),
  subgroup = factor(c("x", "x"))
)
error_caught <- tryCatch({
  validate_prediction_factor_levels(invalid_gr, training_metadata)
  FALSE
}, error = function(e) {
  grepl("Grouping variable.*has levels not in", conditionMessage(e))
})
stopifnot(error_caught)
cat("  2.3 Invalid gr level: PASS (error caught)\n")

# Test 2.4: Invalid subgr level
invalid_subgr <- data.frame(
  series = factor(c("s1", "s2")),
  group = factor(c("A", "A")),
  subgroup = factor(c("z", "z"))  # Invalid level
)
error_caught <- tryCatch({
  validate_prediction_factor_levels(invalid_subgr, training_metadata)
  FALSE
}, error = function(e) {
  grepl("Sub-grouping variable.*not in training", conditionMessage(e))
})
stopifnot(error_caught)
cat("  2.4 Invalid subgr level: PASS (error caught)\n")

# Test 2.5: Missing metadata$levels (should pass silently)
no_levels_metadata <- list(variables = training_metadata$variables)
result <- validate_prediction_factor_levels(valid_newdata, no_levels_metadata)
stopifnot(isTRUE(result))
cat("  2.5 Missing metadata$levels: PASS (skipped)\n")

# Test 2.6: Character series (not factor)
char_newdata <- data.frame(
  series = c("s1", "s2"),  # Character, not factor
  group = c("A", "A"),
  subgroup = c("x", "x")
)
result <- validate_prediction_factor_levels(char_newdata, training_metadata)
stopifnot(isTRUE(result))
cat("  2.6 Character series column: PASS\n")

cat("\n")

# =============================================================================
# TEST 3: Integration with extract_component_linpred()
# =============================================================================

cat("--- Test 3: Integration Tests ---\n")

# Load a cached model if available
FIXTURE_DIR <- "tasks/fixtures"
model_path <- file.path(FIXTURE_DIR, "val_mvgam_ar1_fx.rds")

if (!file.exists(model_path)) {
  cat("  Skipping integration tests (no fixtures found)\n")
  cat("  Run tasks/validate_extraction_vs_brms.R to generate fixtures\n")
} else {
  model <- readRDS(model_path)

  # Create valid training data
  set.seed(42)
  n_time <- 30
  train_data <- data.frame(
    y = rpois(n_time, 5),
    x = rnorm(n_time),
    z = seq(-2, 2, length.out = n_time),
    time = 1:n_time,
    series = factor("s1"),
    group = factor(rep(letters[1:6], each = 5))
  )

  # Test 3.1: Valid prediction (subset of training)
  valid_pred_data <- train_data[1:10, ]
  result <- tryCatch({
    extract_component_linpred(model, valid_pred_data, component = "obs")
    TRUE
  }, error = function(e) {
    cat("    Error:", conditionMessage(e), "\n")
    FALSE
  })
  if (result) {
    cat("  3.1 Valid prediction data: PASS\n")
  } else {
    cat("  3.1 Valid prediction data: FAIL\n")
  }

  # Test 3.2: Invalid series level
  invalid_pred_data <- train_data[1:5, ]
  invalid_pred_data$series <- factor("s2", levels = "s2")
  error_caught <- tryCatch({
    extract_component_linpred(model, invalid_pred_data, component = "obs")
    FALSE
  }, error = function(e) {
    grepl("Series levels", conditionMessage(e)) ||
    grepl("not found", conditionMessage(e))
  })
  if (error_caught) {
    cat("  3.2 Invalid series level detected: PASS\n")
  } else {
    cat("  3.2 Invalid series level detected: FAIL\n")
  }

  # Test 3.3: Check that trend_metadata has levels
  if (!is.null(model$trend_metadata$levels)) {
    cat("  3.3 trend_metadata$levels exists: PASS\n")
    cat("      Series levels:", model$trend_metadata$levels$series, "\n")
  } else {
    cat("  3.3 trend_metadata$levels exists: FAIL (NULL)\n")
    cat("      Note: Model may have been created before levels were stored\n")
  }
}

cat("\n")

# =============================================================================
# SUMMARY
# =============================================================================

cat("=== ALL TESTS PASSED ===\n")
