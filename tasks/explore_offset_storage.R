# Exploration: Where does brms store offsets in prep objects?
# Purpose: Verify offset storage location before implementing support
# Created: 2025-01-13

devtools::load_all()

cat("=== OFFSET STORAGE EXPLORATION ===\n\n")

# Load fit12 which has an offset term
cat("Loading fit12 (model with offset)...\n")
fit12_mvgam <- readRDS("tasks/fixtures/fit12.rds")

cat("Formula:\n")
print(fit12_mvgam$formula)

# Create newdata with offset
cat("\n\nCreating newdata with offset...\n")
original_data <- fit12_mvgam$model_data
newdata <- original_data[1:10, ]
cat("  newdata has", nrow(newdata), "rows\n")
cat("  Columns:", paste(names(newdata), collapse = ", "), "\n")

# Extract the underlying brmsfit object (mvgam is a brmsfit subclass)
# This avoids mvgam's S3 method dispatch issues
fit12 <- fit12_mvgam
class(fit12) <- "brmsfit"

# Test 1: Use brms::prepare_predictions directly on brmsfit
cat("\n=== TEST 1: Full brms prepare_predictions ===\n")
prep_full <- brms::prepare_predictions(fit12, newdata = newdata)

cat("prep structure:\n")
cat("  Names:", paste(names(prep_full), collapse = ", "), "\n")
cat("  Class:", paste(class(prep_full), collapse = ", "), "\n")

# Check for ac component
if ("ac" %in% names(prep_full)) {
  cat("\nprep$ac exists!\n")
  cat("  ac names:", paste(names(prep_full$ac), collapse = ", "), "\n")
  
  if ("offset" %in% names(prep_full$ac)) {
    cat("\n✓ prep$ac$offset FOUND\n")
    cat("  Length:", length(prep_full$ac$offset), "\n")
    cat("  Values:", paste(head(prep_full$ac$offset, 5), collapse = ", "), "\n")
  } else {
    cat("\n✗ prep$ac$offset NOT found\n")
  }
} else {
  cat("\nprep$ac does NOT exist\n")
}

# Check for sdata$offset
if ("sdata" %in% names(prep_full)) {
  cat("\nprep$sdata exists\n")
  if ("offset" %in% names(prep_full$sdata)) {
    cat("✓ prep$sdata$offset FOUND\n")
    cat("  Value:", prep_full$sdata$offset, "\n")
  } else {
    cat("✗ prep$sdata$offset NOT found\n")
  }
} else {
  cat("prep$sdata does NOT exist\n")
}

# Test 2: Use brms::standata to see what's in sdata
cat("\n\n=== TEST 2: brms::standata output ===\n")
sdata <- brms::standata(fit12, newdata = newdata, internal = TRUE)

cat("sdata names (first 20):\n")
cat("  ", paste(head(names(sdata), 20), collapse = ", "), "\n")

if ("offset" %in% names(sdata)) {
  cat("\n✓ sdata$offset FOUND\n")
  cat("  Length:", length(sdata$offset), "\n")
  cat("  Values:", paste(head(sdata$offset, 5), collapse = ", "), "\n")
} else {
  cat("\n✗ sdata$offset NOT found\n")
}

# Search for offset pattern in all sdata names
offset_matches <- grep("offset", names(sdata), value = TRUE, 
                       ignore.case = TRUE)
if (length(offset_matches) > 0) {
  cat("\nOffset-related fields in sdata:\n")
  for (field in offset_matches) {
    cat("  -", field, "\n")
  }
}

# Test 3: Check dpars structure
cat("\n\n=== TEST 3: dpars structure ===\n")
if ("dpars" %in% names(prep_full)) {
  cat("prep$dpars exists\n")
  cat("  dpars names:", paste(names(prep_full$dpars), collapse = ", "), "\n")
  
  if ("mu" %in% names(prep_full$dpars)) {
    cat("\nprep$dpars$mu exists\n")
    cat("  Dimensions:", paste(dim(prep_full$dpars$mu), 
                               collapse = " × "), "\n")
  }
} else {
  cat("prep$dpars does NOT exist\n")
}

# Test 4: Try calling brms:::get_ac
cat("\n\n=== TEST 4: brms:::get_ac function ===\n")
tryCatch({
  ac <- brms:::get_ac(fit12, newdata = newdata)
  cat("✓ brms:::get_ac succeeded\n")
  cat("  ac class:", paste(class(ac), collapse = ", "), "\n")
  cat("  ac names:", paste(names(ac), collapse = ", "), "\n")
  
  if ("offset" %in% names(ac)) {
    cat("\n✓ ac$offset FOUND via get_ac\n")
    cat("  Length:", length(ac$offset), "\n")
    cat("  Values:", paste(head(ac$offset, 5), collapse = ", "), "\n")
  }
}, error = function(e) {
  cat("✗ brms:::get_ac failed:\n")
  cat("  Error:", conditionMessage(e), "\n")
})

# Summary
cat("\n\n=== SUMMARY ===\n")
cat("Based on exploration with fit12:\n")
cat("1. Offset storage location: [to be filled by script output]\n")
cat("2. Recommended extraction method: [to be determined]\n")
cat("3. prep$ac availability: [to be confirmed]\n")
cat("4. prep$sdata$offset availability: [to be confirmed]\n")

cat("\n=== END EXPLORATION ===\n")
