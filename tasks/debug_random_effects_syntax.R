# Debug Random Effects Syntax Issues
# Test different RE syntax to isolate the compilation problem

devtools::load_all()

cat("=== TESTING DIFFERENT RANDOM EFFECTS SYNTAX ===\n")

# Create simple test dataset
set.seed(12345)
data_test <- data.frame(
  time = rep(1:10, 3),
  site = factor(rep(c("A", "B", "C"), each = 10)),
  series = factor("series1"),
  x = rnorm(30),
  y = rpois(30, 3)
)

cat("Test data created with", nrow(data_test), "rows\n")

# Test different random effects patterns
test_cases <- list(
  "No RE" = list(
    formula = y ~ x,
    trend_formula = ~ RW()
  ),
  "Random intercepts only" = list(
    formula = y ~ x + (1 | site),
    trend_formula = ~ RW()
  ),
  "Uncorrelated RE" = list(
    formula = y ~ x + (x || site),  # Uncorrelated slopes + intercepts
    trend_formula = ~ RW()
  ),
  "Correlated RE" = list(
    formula = y ~ x + (x | site),   # Correlated slopes + intercepts
    trend_formula = ~ RW()
  )
)

for (test_name in names(test_cases)) {
  cat("\n--- Testing:", test_name, "---\n")
  
  test_case <- test_cases[[test_name]]
  
  tryCatch({
    # First try make_stancode
    stan_code <- make_stancode(
      formula = test_case$formula,
      trend_formula = test_case$trend_formula,
      data = data_test,
      family = poisson()
    )
    
    cat("✓ Stan code generation successful\n")
    
    # Check for complex correlation structures in generated code
    if (grepl("Cor_", stan_code) || grepl("L_Omega", stan_code)) {
      cat("  Contains correlation structures (Cor_, L_Omega)\n")
    }
    
    if (grepl("cholesky", stan_code, ignore.case = TRUE)) {
      cat("  Contains Cholesky decomposition\n")
    }
    
    # Try actual fitting with very few iterations
    cat("  Attempting quick fit...\n")
    
    fit <- mvgam(
      formula = test_case$formula,
      trend_formula = test_case$trend_formula,
      data = data_test,
      family = poisson(),
      chains = 1,
      iter = 100,  # Very short for testing
      silent = 2
    )
    
    cat("✓ Model fitting successful\n")
    
  }, error = function(e) {
    cat("✗ Error:", e$message, "\n")
    
    # If it's a Stan compilation error, show the line
    if (grepl("Semantic error", e$message)) {
      cat("  This is a Stan compilation error\n")
      if (grepl("line [0-9]+", e$message)) {
        line_match <- regmatches(e$message, regexpr("line [0-9]+", e$message))
        cat("  Problem at:", line_match, "\n")
      }
    }
  })
}

cat("\n=== SUMMARY ===\n")
cat("This will help identify which random effects syntax causes the compilation error.\n")
cat("If correlated RE (x | site) fails but uncorrelated (x || site) works,\n")
cat("then the issue is with mvgam's handling of correlation matrices in Stan.\n")