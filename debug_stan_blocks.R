#!/usr/bin/env Rscript
# Debug script to understand Stan block structure and brace matching issues

library(mvgam)

# Set up test data identical to test suite
setup_stan_test_data <- function() {
  set.seed(42)
  n_time <- 24
  univariate <- data.frame(
    time = 1:n_time,
    series = factor(rep("series1", n_time)),
    y = rpois(n_time, lambda = 5),
    x = rnorm(n_time),
    temperature = rnorm(n_time, mean = 15, sd = 3)
  )
  return(list(univariate = univariate))
}

# Get test data
data <- setup_stan_test_data()$univariate

# Try to generate base Stan code first
cat("Attempting to generate base Stan code with brms...\n")
try({
  base_code <- brms::stancode(y ~ x, data = data, family = poisson())
  cat("Base Stan code generated successfully. Length:", nchar(base_code), "characters\n")
  
  # Show first 35 lines to see structure including model block
  code_lines <- strsplit(base_code, "\n")[[1]]
  cat("First 35 lines of base Stan code:\n")
  for(i in 1:min(35, length(code_lines))) {
    cat(sprintf("%2d: %s\n", i, code_lines[i]))
  }
  
  # Look for model block specifically and show brace structure
  model_start <- grep("^\\s*model\\s*\\{", code_lines)
  if(length(model_start) > 0) {
    cat("\nFound model block starting at line:", model_start, "\n")
    cat("Model block start line:", code_lines[model_start], "\n")
    
    # Show lines around model block to understand structure
    start_range <- max(1, model_start - 2)
    end_range <- min(length(code_lines), model_start + 10)
    cat("Context around model block:\n")
    for(i in start_range:end_range) {
      cat(sprintf("%2d: %s\n", i, code_lines[i]))
    }
  } else {
    cat("\nNo model block found in base code!\n")
  }
  
}, silent = FALSE)

cat("\n", paste(rep("=", 50), collapse=""), "\n")

# Now try with mvgam_formula trend
cat("Attempting to generate Stan code with trend...\n")
mf_with_trend <- mvgam_formula(y ~ x, trend_formula = ~ RW())

try({
  code_with_trend <- stancode(mf_with_trend, data = data, family = poisson())
  cat("Stan code with trend generated successfully!\n")
}, silent = FALSE)