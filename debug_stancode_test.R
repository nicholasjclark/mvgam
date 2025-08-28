# Debug script to capture full Stan code generation details
library(testthat)

# Load the package
devtools::load_all()

# Create test data
set.seed(42)
n_time <- 24
data <- data.frame(
  time = 1:n_time,
  series = factor(rep("series1", n_time)),
  y = rpois(n_time, lambda = 5),
  x = rnorm(n_time),
  temperature = rnorm(n_time, mean = 15, sd = 3)
)

cat("=== DEBUGGING STAN CODE GENERATION ===\n\n")

# Simple observation-only model
cat("1. Creating mvgam_formula...\n")
mf_obs_only <- mvgam_formula(y ~ x)
cat("Formula created successfully\n\n")

cat("2. Generating Stan code...\n")
tryCatch({
  code_obs_only <- stancode(mf_obs_only, data = data, family = poisson())
  
  cat("Stan code generated successfully!\n")
  cat("Class:", class(code_obs_only), "\n")
  cat("Length:", nchar(code_obs_only), "characters\n\n")
  
  # Show the actual generated code
  cat("=== GENERATED STAN CODE ===\n")
  cat(code_obs_only)
  cat("\n=== END STAN CODE ===\n\n")
  
}, error = function(e) {
  cat("ERROR during stancode generation:\n")
  cat("Error message:", e$message, "\n")
  cat("Error class:", class(e), "\n")
  
  # Try to get more details about where the error occurred
  if (exists("traceback")) {
    cat("\nTraceback:\n")
    print(traceback())
  }
})

# Now try with trends to see the difference
cat("3. Trying with trend model...\n")
tryCatch({
  mf_with_trend <- mvgam_formula(y ~ x, trend_formula = ~ RW())
  cat("Trend formula created successfully\n")
  
  code_with_trend <- stancode(mf_with_trend, data = data, family = poisson())
  
  cat("Trend Stan code generated successfully!\n")
  cat("Class:", class(code_with_trend), "\n")
  cat("Length:", nchar(code_with_trend), "characters\n\n")
  
  # Show the actual generated code
  cat("=== GENERATED STAN CODE WITH TRENDS ===\n")
  cat(code_with_trend)
  cat("\n=== END STAN CODE WITH TRENDS ===\n\n")
  
}, error = function(e) {
  cat("ERROR during trend stancode generation:\n")
  cat("Error message:", e$message, "\n")
  cat("Error class:", class(e), "\n")
  
  # Try to capture more debugging info
  cat("\nDetailed error info:\n")
  cat("Call stack:\n")
  calls <- sys.calls()
  for (i in seq_along(calls)) {
    cat(paste0("  ", i, ": ", deparse(calls[[i]][1]), "\n"))
  }
})

cat("=== END DEBUGGING ===\n")