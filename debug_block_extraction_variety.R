# Debug script to test Stan block extraction across different trend formula complexities

library(brms)
devtools::load_all()

cat("=== TESTING BLOCK EXTRACTION ACROSS TREND FORMULAS ===\n\n")

# Test data
data_simple <- data.frame(y = rnorm(50), time = 1:50, series = gl(1, 50))
data_complex <- data.frame(y = rnorm(100), x1 = rnorm(100), x2 = factor(rep(c("A", "B"), 50)), 
                          time = rep(1:50, 2), series = gl(2, 50))

# Different trend formula complexities
test_cases <- list(
  list(name = "Intercept only", formula = ~ 1, data = data_simple),
  list(name = "Single continuous", formula = ~ x1, data = data_complex),
  list(name = "Factor", formula = ~ x2, data = data_complex),
  list(name = "Multiple predictors", formula = ~ x1 + x2, data = data_complex),
  list(name = "Interaction", formula = ~ x1 * x2, data = data_complex)
)

for (i in seq_along(test_cases)) {
  test_case <- test_cases[[i]]
  cat("=== TEST CASE", i, ":", test_case$name, "===\n")
  
  tryCatch({
    # Create trend setup
    trend_setup <- setup_brms_lightweight(test_case$formula, data = test_case$data, family = gaussian())
    
    # Show the full Stan code structure
    lines <- strsplit(trend_setup$stancode, "\n")[[1]]
    block_starts <- which(grepl("^\\s*(data|transformed data|parameters|transformed parameters|model|generated quantities)\\s*\\{", lines))
    
    cat("Stan code structure:\n")
    for (j in seq_along(block_starts)) {
      line_num <- block_starts[j]
      block_line <- lines[line_num]
      cat("  Line", line_num, ":", trimws(block_line), "\n")
    }
    
    # Test manual data block extraction
    cat("\n--- Manual data block extraction ---\n")
    in_data_block <- FALSE
    data_lines <- c()
    data_start <- NULL
    data_end <- NULL
    
    for (j in seq_along(lines)) {
      line <- lines[j]
      
      if (grepl("^\\s*data\\s*\\{", line)) {
        data_start <- j
        in_data_block <- TRUE
        next
      }
      
      if (in_data_block && grepl("^\\s*\\}\\s*$", line)) {
        data_end <- j
        break
      }
      
      if (in_data_block) {
        data_lines <- c(data_lines, line)
      }
    }
    
    cat("Data block boundaries: lines", data_start, "to", data_end, "\n")
    cat("Extracted data declarations (", length(data_lines), "lines):\n")
    for (line in data_lines) {
      clean_line <- trimws(line)
      if (clean_line != "" && grepl(";\\s*$", clean_line)) {
        cat("  ", clean_line, "\n")
      }
    }
    
    # Test function-based extraction
    cat("\n--- Function-based extraction ---\n")
    func_result <- extract_stan_block_content(trend_setup$stancode, "data")
    func_lines <- strsplit(func_result, "\n")[[1]]
    
    # Count how many actual data declarations vs other content
    data_decl_count <- 0
    other_content_count <- 0
    
    for (line in func_lines) {
      clean_line <- trimws(line)
      if (clean_line != "") {
        if (grepl("^\\s*(int|real|vector|matrix|array).*<.*>.*\\w+.*;", clean_line) || 
            grepl("^\\s*(int|real|vector|matrix|array)\\s+\\w+.*;", clean_line)) {
          data_decl_count <- data_decl_count + 1
        } else if (!grepl("^\\s*//", clean_line) && !grepl("^\\s*\\}\\s*$", clean_line)) {
          other_content_count <- other_content_count + 1
        }
      }
    }
    
    cat("Function extracted", length(func_lines), "total lines\n")
    cat("  Data declarations:", data_decl_count, "\n")
    cat("  Other content:", other_content_count, "\n")
    
    if (other_content_count > 0) {
      cat("  ⚠ WARNING: Function extraction includes non-data content!\n")
      cat("  Non-data content preview:\n")
      shown <- 0
      for (line in func_lines) {
        clean_line <- trimws(line)
        if (clean_line != "" && !grepl("^\\s*(int|real|vector|matrix|array)", clean_line) && 
            !grepl("^\\s*//", clean_line) && !grepl("^\\s*\\}\\s*$", clean_line)) {
          cat("    ", clean_line, "\n")
          shown <- shown + 1
          if (shown >= 3) {
            cat("    ... (and more)\n")
            break
          }
        }
      }
    } else {
      cat("  ✓ Function extraction looks clean\n")
    }
    
  }, error = function(e) {
    cat("ERROR:", conditionMessage(e), "\n")
  })
  
  cat("\n", rep("-", 50), "\n\n")
}

cat("=== SUMMARY ===\n")
cat("Check if block extraction issues are consistent across all trend formula types\n")
cat("Identify which formulas produce clean vs corrupted extraction\n")