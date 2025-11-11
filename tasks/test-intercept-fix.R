# Test script to verify the intercept fix works correctly
devtools::load_all()

# Create test data
set.seed(123)
n_time <- 10
n_series <- 2

test_data <- expand.grid(
  time = 1:n_time,
  series = factor(1:n_series)
)
test_data$x <- rnorm(nrow(test_data))
test_data$y <- rpois(nrow(test_data), exp(1 + 0.5 * test_data$x))

# Test cases
test_formulas <- list(
  "RW() only" = ~ RW(),
  "RW() with -1" = ~ -1 + RW(),
  "RW() + x - 1" = ~ RW() + x - 1,
  "RW() + gp(x) - 1" = ~ RW() + gp(x, k = 5) - 1,
  "x + RW() - 1" = ~ x + RW() - 1,
  "1 + RW()" = ~ 1 + RW(),
  "AR() only" = ~ AR(),
  "AR() with -1" = ~ -1 + AR()
)

cat("Testing intercept handling after fix:\n")
cat("=====================================\n\n")

for (name in names(test_formulas)) {
  cat("Test:", name, "\n")
  cat("Formula:", deparse(test_formulas[[name]]), "\n")
  
  tryCatch({
    stancode <- make_stancode(
      mvgam_formula(y ~ x, trend_formula = test_formulas[[name]]),
      data = test_data,
      family = poisson(),
      validate = FALSE
    )
    
    # Check for Intercept_trend parameter
    has_intercept <- grepl("real Intercept_trend", stancode)
    
    # Expected behavior: only formulas with explicit '1 +' should have intercept
    expected <- name == "1 + RW()"
    
    if (has_intercept == expected) {
      cat("  ✓ PASS: Intercept handling correct (has_intercept =", has_intercept, ")\n")
    } else {
      cat("  ✗ FAIL: Expected intercept =", expected, ", got =", has_intercept, "\n")
    }
    
  }, error = function(e) {
    cat("  ERROR:", e$message, "\n")
  })
  
  cat("\n")
}

cat("Test complete.\n")