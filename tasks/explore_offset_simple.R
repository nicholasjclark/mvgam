# Simplified offset exploration: Check standata output
devtools::load_all()

cat("=== SIMPLE OFFSET EXPLORATION ===\n\n")

# Load fit12
fit12 <- readRDS("tasks/fixtures/fit12.rds")
cat("Loaded fit12\n")
cat("Formula:", deparse(fit12$formula), "\n\n")

# Get original data
original_data <- fit12$model_data
newdata <- original_data[1:10, ]

cat("Creating newdata with", nrow(newdata), "rows\n")
cat("Columns:", paste(names(newdata), collapse = ", "), "\n\n")

# Call standata directly (what our S3 method does)
cat("=== Calling brms::standata ===\n")
sdata <- brms::standata(fit12, newdata = newdata, internal = TRUE)

cat("\nstandata names (first 30):\n")
print(head(names(sdata), 30))

cat("\n\nSearching for 'offset' in standata names:\n")
offset_fields <- grep("offset", names(sdata), value = TRUE, 
                      ignore.case = TRUE)
if (length(offset_fields) > 0) {
  cat("✓ Found offset-related fields:\n")
  for (field in offset_fields) {
    val <- sdata[[field]]
    cat(sprintf("  %s: class=%s, length=%d\n", 
                field, class(val)[1], length(val)))
    if (length(val) <= 10) {
      cat("    Values:", paste(val, collapse = ", "), "\n")
    } else {
      cat("    First 5:", paste(head(val, 5), collapse = ", "), "\n")
    }
  }
} else {
  cat("✗ No offset fields found\n")
}

cat("\n\n=== CONCLUSION ===\n")
cat("Offset storage in standata:\n")
if (length(offset_fields) > 0) {
  cat("  Location: sdata$", offset_fields[1], "\n", sep = "")
  cat("  Recommendation: Check prep$sdata for offset\n")
} else {
  cat("  No offset found in standata\n")
  cat("  May need to extract via brms::get_ac()\n")
}
