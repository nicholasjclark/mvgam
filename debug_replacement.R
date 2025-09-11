# Debug script to test pattern replacement
library(mvgam)

# Test the replacement function directly
test_text <- "// centered version of X_trend without an intercept"
trend_patterns <- c(
  "obs_trend_time",
  "obs_trend_series", 
  "times_trend",
  "trend(?!_)",  # The computed trend matrix should be response-specific (negative lookahead to avoid matching *_trend)
  "mu_ones" # GLM compatibility stanvar should be response-specific
)
response_suffix <- "_count"

cat("Original text:\n", test_text, "\n\n")

# Test each pattern individually
for (pattern in trend_patterns) {
  cat("Testing pattern:", pattern, "\n")
  
  # Check if pattern already contains regex special characters (lookahead/lookbehind)
  if (grepl("\\?[=!]|\\(\\?", pattern)) {
    # Pattern already contains lookahead/lookbehind, use as-is with word boundary only at start
    regex_pattern <- paste0("\\b", pattern)
    # For negative lookahead patterns, extract the base pattern for replacement
    base_pattern <- gsub("\\(\\?[^)]*\\)", "", pattern)
    replacement <- paste0(base_pattern, response_suffix)
  } else {
    # Standard pattern, add word boundaries on both sides
    regex_pattern <- paste0("\\b", pattern, "\\b")
    replacement <- paste0(pattern, response_suffix)
  }
  
  cat("  Regex pattern:", regex_pattern, "\n")
  cat("  Replacement:", replacement, "\n")
  
  # Test if pattern matches
  matches <- grepl(regex_pattern, test_text, perl = TRUE)
  cat("  Matches:", matches, "\n")
  
  if (matches) {
    result <- gsub(regex_pattern, replacement, test_text, perl = TRUE)
    cat("  Result:", result, "\n")
  }
  
  cat("\n")
}