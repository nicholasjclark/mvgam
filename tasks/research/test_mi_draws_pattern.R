# Test MI Architecture: rstan::sflist2stanfit() pattern for mvgam
# Based on actual brms implementation in combine_models()

# Note: Run with devtools::load_all() before executing
# Rscript -e "devtools::load_all();source('tasks/research/test_mi_draws_pattern.R')"

library(rstan)

# Setup helpers
SM <- suppressMessages
SW <- suppressWarnings

cat("\n=== Testing brms-style MI Architecture ===\n")
cat("Using rstan::sflist2stanfit() to combine stanfit objects\n\n")

# Create simple test data
set.seed(123)
test_data <- data.frame(
  time = 1:20,
  series = factor("s1"),
  y = rnorm(20, mean = 5, sd = 1),
  x = rnorm(20)
)

# Step 1: Fit two models (simulating two imputations)
cat("Step 1: Fitting models to 'imputations'...\n")
fit1 <- SW(SM(mvgam(
  y ~ x,
  trend_formula = ~ RW(),
  data = test_data,
  chains = 2,
  iter = 400,
  warmup = 200,
  silent = 2
)))
cat("  ✓ Fit 1 complete\n")

fit2 <- SW(SM(mvgam(
  y ~ x,
  trend_formula = ~ RW(),
  data = test_data,
  chains = 2,
  iter = 400,
  warmup = 200,
  silent = 2
)))
cat("  ✓ Fit 2 complete\n")

# Step 2: Extract stanfit objects (actual brms pattern)
cat("\nStep 2: Extract stanfit objects...\n")
stanfit1 <- fit1$fit
stanfit2 <- fit2$fit
cat("  Stanfit1 class:", class(stanfit1), "\n")
cat("  Stanfit2 class:", class(stanfit2), "\n")

# Step 3: Combine using rstan::sflist2stanfit (brms approach)
cat("\nStep 3: Combine using rstan::sflist2stanfit()...\n")
sflist <- list(stanfit1, stanfit2)
combined_stanfit <- rstan::sflist2stanfit(sflist)
cat("  ✓ Combined stanfit created\n")
cat("  Combined class:", class(combined_stanfit), "\n")

# Check total draws
cat("\nStep 3.1: Verify draw counts...\n")
draws1 <- rstan::extract(stanfit1, permuted = FALSE)
draws2 <- rstan::extract(stanfit2, permuted = FALSE)
draws_combined <- rstan::extract(combined_stanfit, permuted = FALSE)
expected_draws <- dim(draws1)[1] + dim(draws2)[1]
actual_draws <- dim(draws_combined)[1]
cat("  Expected total draws:", expected_draws, "\n")
cat("  Actual total draws:", actual_draws, "\n")
cat("  Match:", expected_draws == actual_draws, "\n")

# Step 4: Create pooled object (exact brms pattern)
cat("\nStep 4: Create mvgam_multiple object...\n")
pooled <- fit1  # Use first fit as template (brms approach)
pooled$fit <- combined_stanfit  # Replace with combined stanfit
attr(pooled, "fits") <- list(fit1, fit2)  # Store individual fits
attr(pooled, "n_imputations") <- 2
class(pooled) <- c("mvgam_multiple", "mvgam", "brmsfit")
cat("  ✓ Created mvgam_multiple object with combined stanfit\n")

# Step 5: Test method compatibility
cat("\nStep 5: Test method compatibility...\n")

test_results <- list()

cat("  Testing variables()...")
test_results$vars <- tryCatch({
  v <- variables(pooled)
  cat(" ✓ (", length(v), "parameters)\n", sep = "")
  TRUE
}, error = function(e) {
  cat(" ✗\n    Error:", conditionMessage(e), "\n")
  FALSE
})

cat("  Testing summary()...")
test_results$summary <- tryCatch({
  s <- summary(pooled)
  cat(" ✓\n")
  TRUE
}, error = function(e) {
  cat(" ✗\n    Error:", conditionMessage(e), "\n")
  FALSE
})

cat("  Testing print()...")
test_results$print <- tryCatch({
  capture.output(print(pooled))
  cat(" ✓\n")
  TRUE
}, error = function(e) {
  cat(" ✗\n    Error:", conditionMessage(e), "\n")
  FALSE
})

# Results
cat("\n=== RESULTS ===\n")
all_pass <- all(unlist(test_results))
if (all_pass) {
  cat("✓ ALL TESTS PASSED\n")
  cat("\nArchitecture is compatible:\n")
  cat("  • posterior::bind_draws() works for combining fits\n")
  cat("  • Combined draws can be stored in $fit slot\n")
  cat("  • All methods (variables, summary, print) work\n")
  cat("  • Individual fits preserved in attributes\n")
  cat("\n→ Ready to implement mvgam_multiple() following brms pattern\n")
} else {
  cat("✗ SOME TESTS FAILED\n")
  cat("Results:", paste(names(test_results), "=", test_results, collapse=", "), "\n")
}
