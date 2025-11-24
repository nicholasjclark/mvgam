#!/usr/bin/env Rscript

# Load mvgam package
devtools::load_all()

# Run tests and capture output
test_results <- testthat::test_file(
  'tests/testthat/test-stancode-standata.R',
  stop_on_failure = FALSE
)

# Print summary
cat("TEST SUMMARY\n")
cat("============\n")
cat("Total tests:", length(test_results), "\n")
passed <- sum(sapply(test_results, function(x) length(x$results) > 0 && all(sapply(x$results, function(r) r$passed))))
failed <- sum(sapply(test_results, function(x) length(x$results) > 0 && any(sapply(x$results, function(r) !r$passed))))
cat("Passed:", passed, "\n")
cat("Failed:", failed, "\n")

# Print failure details
cat("\nFAILED TESTS\n")
cat("============\n")
for (i in seq_along(test_results)) {
  test <- test_results[[i]]
  if (length(test$results) > 0) {
    for (j in seq_along(test$results)) {
      result <- test$results[[j]]
      if (!result$passed) {
        cat("\nTest:", test$test, "\n")
        cat("File:", test$file, ":", test$line, "\n")
        cat("Error:", result$message, "\n")
        if (!is.null(result$trace)) {
          cat("Trace:\n")
          cat(paste(result$trace, collapse = "\n"), "\n")
        }
        cat("---\n")
      }
    }
  }
}