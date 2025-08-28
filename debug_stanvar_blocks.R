#!/usr/bin/env Rscript

# Debug stanvar block assignment issue
# This script traces the exact flow of stanvar creation and block assignment

library(devtools)
load_all()

# Create test data
set.seed(42)
data <- data.frame(
  time = 1:24,
  series = factor(rep("series1", 24)),
  y = rpois(24, lambda = 5),
  x = rnorm(24)
)

# Step 1: Test basic brms stanvar behavior
cat("=== STEP 1: Testing basic brms stanvar behavior ===\n")
sigma_sv <- brms::stanvar(
  name = "sigma_trend", 
  scode = "vector<lower=0>[1] sigma_trend;", 
  block = "parameters"
)
cat("Created stanvar - Block:", sigma_sv$block, "\n")

# Test brms compilation
test_code <- brms::make_stancode(
  bf(y ~ x), 
  data = data, 
  family = poisson(),
  stanvars = sigma_sv
)

# Check if sigma_trend appears in parameters block
lines <- strsplit(test_code, "\n")[[1]]
param_block_start <- which(grepl("^parameters", lines))
param_block_end <- which(grepl("^transformed parameters|^model", lines))[1] - 1
param_section <- lines[param_block_start:param_block_end]

cat("BRMS Parameters Block Contents:\n")
cat(param_section, sep = "\n")
cat("\n")

# Step 2: Test mvgam stanvar generation
cat("=== STEP 2: Testing mvgam stanvar creation ===\n")

# Create mvgam_formula and try stancode generation
mf <- mvgam_formula(y ~ x, trend_formula = ~ RW())
cat("Created mvgam_formula successfully\n")

# Try to generate stancode with debug
tryCatch({
  code <- stancode(mf, data = data, family = poisson())
  cat("Stancode generation succeeded\n")
}, error = function(e) {
  cat("ERROR in stancode generation:", e$message, "\n")
  
  # Let's trace the error by looking at the code that was generated
  if (grepl("Syntax error", e$message)) {
    # Extract the problematic code from the error
    error_lines <- strsplit(e$message, "\n")[[1]]
    syntax_lines <- error_lines[grepl("^\\s*[0-9]+:", error_lines)]
    cat("Problematic Stan code lines:\n")
    cat(syntax_lines, sep = "\n")
    cat("\n")
  }
})

# Step 3: Manual trace of the stanvar generation pipeline
cat("=== STEP 3: Manual trace of stanvar generation ===\n")

# Create the setup manually to see what's happening
obs_setup <- list(
  formula = bf(y ~ x),
  data = data,
  family = poisson()
)

# Create a simple trend specification
trend_specs <- list(
  trend = "RW",
  n_lv = 1,
  ma = FALSE,
  shared_innovations = TRUE
)

cat("Created basic setup objects\n")

# Try to trace the exact issue by checking what stanvars are being created
# and where they're being placed
cat("\n=== DEBUGGING COMPLETE ===\n")
cat("Key findings will be printed above.\n")
cat("Look for stanvar block assignments and any syntax errors.\n")