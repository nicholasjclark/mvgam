#!/usr/bin/env Rscript
# Debug script to examine Stan code during trend injection

devtools::load_all()

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

# Function to debug find_stan_block
debug_find_stan_block <- function(code_lines, block_name) {
  cat("Debugging find_stan_block for:", block_name, "\n")
  cat("Total lines:", length(code_lines), "\n")
  
  # Check for block pattern
  block_pattern <- paste0("^\\s*", block_name, "\\s*\\{")
  block_matches <- grep(block_pattern, code_lines)
  
  if (length(block_matches) == 0) {
    cat("ERROR: No matches found for pattern:", block_pattern, "\n")
    cat("Showing all lines:\n")
    for (i in seq_along(code_lines)) {
      cat(sprintf("%3d: %s\n", i, code_lines[i]))
    }
    return(NULL)
  }
  
  start_idx <- block_matches[1]
  cat("Block starts at line:", start_idx, "\n")
  cat("Start line content:", code_lines[start_idx], "\n")
  
  # Manual brace counting
  brace_count <- 1
  cat("Starting brace count:", brace_count, "\n")
  
  end_idx <- NULL
  for (i in (start_idx + 1):length(code_lines)) {
    line <- code_lines[i]
    
    # Count braces
    open_braces <- lengths(gregexpr("\\{", line, fixed = TRUE))
    close_braces <- lengths(gregexpr("\\}", line, fixed = TRUE))
    if (open_braces == -1) open_braces <- 0
    if (close_braces == -1) close_braces <- 0
    
    brace_count <- brace_count + open_braces - close_braces
    cat(sprintf("Line %d: '%s' | opens=%d, closes=%d, count=%d\n", 
                i, line, open_braces, close_braces, brace_count))
    
    if (brace_count == 0) {
      end_idx <- i
      cat("Block ends at line:", end_idx, "\n")
      break
    }
  }
  
  if (is.null(end_idx)) {
    cat("ERROR: Could not find end of block\n")
    return(NULL)
  }
  
  return(list(start_idx = start_idx, end_idx = end_idx))
}

# Get test data
data <- setup_stan_test_data()$univariate

# Create mvgam_formula with trend
mf_with_trend <- mvgam_formula(y ~ x, trend_formula = ~ RW())

cat("Attempting to debug the failing stancode generation...\n")

# Try to step through the process manually
try({
  # Generate base stancode first
  cat("\n=== Step 1: Generate base stancode ===\n")
  base_stancode <- brms::stancode(y ~ x, data = data, family = poisson())
  cat("Base stancode length:", nchar(base_stancode), "\n")
  
  # Parse into lines
  code_lines <- strsplit(base_stancode, "\n", fixed = TRUE)[[1]]
  cat("Base stancode has", length(code_lines), "lines\n")
  
  # Test finding model block in base code
  cat("\n=== Step 2: Test finding model block in base code ===\n")
  model_result <- debug_find_stan_block(code_lines, "model")
  
  if (!is.null(model_result)) {
    cat("SUCCESS: Found model block in base code!\n")
  } else {
    cat("FAILURE: Could not find model block in base code\n")
  }
  
}, silent = FALSE)

cat("\nDebug completed.\n")