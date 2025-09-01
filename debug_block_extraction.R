# Debug script to test Stan block extraction issues

library(brms)
devtools::load_all()

cat("=== TESTING STAN BLOCK EXTRACTION ===\n\n")

# Create simple test case
data <- data.frame(y = rnorm(50), x = rnorm(50), time = 1:50, series = gl(1, 50))
trend_setup <- setup_brms_lightweight(y ~ x, data = data, family = gaussian())

cat("=== FULL TREND STAN CODE ===\n")
cat(trend_setup$stancode)
cat("\n=== END FULL CODE ===\n\n")

cat("=== TESTING extract_stan_block_content ===\n")
data_block_result <- extract_stan_block_content(trend_setup$stancode, "data")
cat("Data block extraction result:\n")
cat("Length:", nchar(data_block_result), "characters\n")
cat("Content:\n")
cat(data_block_result)
cat("\n=== END DATA BLOCK ===\n\n")

# Test the underlying extract_stan_block function
cat("=== TESTING extract_stan_block (underlying) ===\n")
raw_data_block <- extract_stan_block(trend_setup$stancode, "data")
cat("Raw data block result:\n")
cat("Content:\n")
cat(raw_data_block)
cat("\n=== END RAW DATA BLOCK ===\n\n")

# Test manual parsing to see what SHOULD be extracted
cat("=== MANUAL DATA BLOCK PARSING ===\n")
lines <- strsplit(trend_setup$stancode, "\n")[[1]]
in_data_block <- FALSE
data_lines <- c()

for (i in seq_along(lines)) {
  line <- lines[i]
  
  if (grepl("^\\s*data\\s*\\{", line)) {
    cat("Found data block start at line", i, ":", line, "\n")
    in_data_block <- TRUE
    next  # Skip the opening brace line
  }
  
  if (in_data_block) {
    # Check for end of block
    if (grepl("^\\s*\\}\\s*$", line)) {
      cat("Found data block end at line", i, ":", line, "\n")
      break
    }
    data_lines <- c(data_lines, line)
  }
}

cat("Manually extracted data lines (", length(data_lines), "lines):\n")
for (line in data_lines) {
  if (trimws(line) != "") {
    cat("  ", line, "\n")
  }
}

cat("\n=== TESTING DIFFERENT BLOCKS ===\n")
blocks_to_test <- c("data", "parameters", "transformed parameters", "model")
for (block_name in blocks_to_test) {
  cat("\n--- Testing", block_name, "block ---\n")
  result <- tryCatch({
    extract_stan_block_content(trend_setup$stancode, block_name)
  }, error = function(e) {
    paste("ERROR:", conditionMessage(e))
  })
  
  cat("Length:", nchar(result), "characters\n")
  if (nchar(result) < 200) {
    cat("Content:\n")
    cat(result)
  } else {
    cat("Content (first 200 chars):\n")
    cat(substr(result, 1, 200), "...\n")
  }
  cat("\n")
}

cat("\n=== CONCLUSION ===\n")
cat("Check if extract_stan_block_content is extracting the right boundaries\n")
cat("Compare manual parsing vs function results\n")