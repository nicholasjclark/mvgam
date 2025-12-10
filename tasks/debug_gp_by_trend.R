# Debug script for GP with by-variable in trend formula
# Reproduces Test 10T failure from validate_extraction_vs_brms.R

devtools::load_all()

set.seed(42)
n_time <- 30

# Generate test data (same as validation script)
test_data <- data.frame(
  y = rpois(n_time, 10),
  z = seq(-2, 2, length.out = n_time),
  w = seq(-1, 1, length.out = n_time),
  time = 1:n_time,
  series = factor("s1"),
  cat = factor(rep(c("A", "B"), each = 15))
)

cat("=== Debug: GP with by-variable in trend formula ===\n\n")
cat("Test data structure:\n")
print(str(test_data))

# Step 1: Try observation formula first (this works)
cat("\n--- Step 1: Test GP+by in observation formula (should work) ---\n")
tryCatch({
  code_obs <- stancode(
    y ~ 1 + gp(z, k = 5) + gp(w, by = cat, k = 5),
    data = test_data,
    family = poisson()
  )
  cat("SUCCESS: Observation formula stancode generated\n")
  cat("Length:", nchar(code_obs), "characters\n")
}, error = function(e) {
  cat("ERROR:", conditionMessage(e), "\n")
})

# Step 2: Try trend formula (this fails)
cat("\n--- Step 2: Test GP+by in trend formula (fails) ---\n")
tryCatch({
  code_trend <- stancode(
    y ~ 1,
    trend_formula = ~ gp(z, k = 5) + gp(w, by = cat, k = 5) + AR(p = 1),
    data = test_data,
    family = poisson()
  )
  cat("SUCCESS: Trend formula stancode generated\n")
  cat("Length:", nchar(code_trend), "characters\n")
}, error = function(e) {
  cat("ERROR:", conditionMessage(e), "\n")
})

# Step 3: Try simpler trend formula with just one GP+by
cat("\n--- Step 3: Test single GP+by in trend formula ---\n")
tryCatch({
  code_simple <- stancode(
    y ~ 1,
    trend_formula = ~ gp(w, by = cat, k = 5) + AR(p = 1),
    data = test_data,
    family = poisson()
  )
  cat("SUCCESS: Single GP+by trend formula stancode generated\n")
}, error = function(e) {
  cat("ERROR:", conditionMessage(e), "\n")
})

# Step 4: Try GP without by in trend (should work)
cat("\n--- Step 4: Test GP without by in trend formula ---\n")
tryCatch({
  code_no_by <- stancode(
    y ~ 1,
    trend_formula = ~ gp(z, k = 5) + AR(p = 1),
    data = test_data,
    family = poisson()
  )
  cat("SUCCESS: GP without by in trend formula works\n")
}, error = function(e) {
  cat("ERROR:", conditionMessage(e), "\n")
})

# Step 5: Try mvgam() to trigger full code path and see debug output
cat("\n--- Step 5: mvgam() with GP+by in trend formula ---\n")
tryCatch({
  fit <- mvgam(
    y ~ 1,
    trend_formula = ~ gp(z, k = 5) + gp(w, by = cat, k = 5) + AR(p = 1),
    data = test_data,
    family = poisson(),
    chains = 1,
    iter = 100,
    backend = "cmdstanr",
    silent = 2
  )
  cat("SUCCESS: mvgam model fitted\n")
}, error = function(e) {
  cat("ERROR:", conditionMessage(e), "\n")
})

cat("\n=== Debug complete ===\n")
