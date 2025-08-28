#!/usr/bin/env Rscript

# Debug the failing stancode by capturing it before validation

library(devtools)
load_all()

# Monkey patch the validate_stan_code function to capture the failing code
original_validate <- mvgam:::validate_stan_code
assignInNamespace("validate_stan_code", function(stan_code, ...) {
  cat("=== FAILING STAN CODE ===\n")
  cat(stan_code)
  cat("\n=== END OF FAILING CODE ===\n")
  
  # Also save to file for detailed inspection
  writeLines(stan_code, "failing_stancode.stan")
  cat("Saved failing code to failing_stancode.stan\n")
  
  # Now call the original function to get the actual error
  original_validate(stan_code, ...)
}, "mvgam")

# Create test data and try to generate stancode
data <- data.frame(
  time = 1:24,
  series = factor(rep("series1", 24)),
  y = rpois(24, lambda = 5),
  x = rnorm(24)
)

mf <- mvgam_formula(y ~ x, trend_formula = ~ RW())

tryCatch({
  code <- stancode(mf, data = data, family = poisson())
}, error = function(e) {
  cat("Captured error as expected:", e$message, "\n")
})