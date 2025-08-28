# Debug script to trace stanvar processing step by step
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

cat("=== DEBUGGING STANVAR PROCESSING ===\n\n")

# Create trend formula
mf_with_trend <- mvgam_formula(y ~ x, trend_formula = ~ RW())

# Let's trace the stancode.mvgam_formula function step by step
cat("1. Calling standata first to see what happens...\n")
tryCatch({
  standata_result <- standata(mf_with_trend, data = data, family = poisson())
  cat("Standata successful, keys:", paste(names(standata_result), collapse = ", "), "\n")
}, error = function(e) {
  cat("Error in standata:", e$message, "\n")
})

cat("\n2. Now let's trace the stancode function...\n")

# Let's manually trace what happens in stancode.mvgam_formula
# First, let's see what brms generates as a base
cat("2a. Getting brms base code...\n")
tryCatch({
  # This should be what brms produces first
  base_brms_formula <- brms::brmsformula(mf_with_trend$formula)
  cat("Base brms formula created\n")
  
  base_code <- brms::stancode(base_brms_formula, data = data, family = poisson())
  cat("Base brms code generated, length:", nchar(base_code), "\n")
  
  cat("=== BASE BRMS CODE ===\n")
  cat(base_code)
  cat("\n=== END BASE BRMS CODE ===\n\n")
  
}, error = function(e) {
  cat("Error in base brms generation:", e$message, "\n")
})

cat("2b. Now trying full mvgam stancode...\n")
tryCatch({
  # Debug the internal steps by looking at the error location
  debug(stancode.mvgam_formula)
  code_with_trend <- stancode(mf_with_trend, data = data, family = poisson())
  undebug(stancode.mvgam_formula)
  
}, error = function(e) {
  cat("Error in mvgam stancode:", e$message, "\n")
  undebug(stancode.mvgam_formula)
  
  # Get more details
  cat("\nError details:\n")
  print(e)
})

cat("\n=== END DEBUGGING ===\n")