# Analysis of t2() smooth structure in mvgam models
# Purpose: Understand how t2() smooths are represented and why extraction 
# expects 1 sds parameter but t2() generates multiple

# Load all functions first
devtools::load_all()

# Load the cached mvgam model with t2() smooth
cat("Loading mvgam model with t2() smooth...\n")
model <- readRDS("tasks/fixtures/val_mvgam_ar1_t2.rds")

# Print basic model information
cat("\n=== MODEL OVERVIEW ===\n")
cat("Model class:", class(model), "\n")
cat("Formula:", deparse(formula(model)), "\n")

# Extract and print Stan code to see t2() representation
cat("\n=== STAN CODE STRUCTURE ===\n")
stan_code <- stancode(model)
cat("Stan code length:", nchar(stan_code), "characters\n")

# Look for smooth-related sections in Stan code
cat("\nSearching for smooth-related sections in Stan code:\n")
stan_lines <- strsplit(stan_code, "\n")[[1]]

# Find lines mentioning smooths, sds, or Zs
smooth_lines <- grep("(smooth|sds|Zs|rho)", stan_lines, ignore.case = TRUE)
if (length(smooth_lines) > 0) {
  cat("Found", length(smooth_lines), "lines mentioning smooths/sds/Zs:\n")
  for (i in smooth_lines[1:min(10, length(smooth_lines))]) {
    cat(sprintf("Line %d: %s\n", i, trimws(stan_lines[i])))
  }
  if (length(smooth_lines) > 10) {
    cat("... and", length(smooth_lines) - 10, "more lines\n")
  }
} else {
  cat("No lines found mentioning smooths/sds/Zs\n")
}

# Extract parameter names and examine sds parameters
cat("\n=== SDS PARAMETERS ANALYSIS ===\n")
param_names <- model$fit@sim$fnames_oi
sds_params <- param_names[grepl("^sds_", param_names)]

cat("Total parameters:", length(param_names), "\n")
cat("sds_ parameters found:", length(sds_params), "\n")

if (length(sds_params) > 0) {
  cat("sds_ parameter names:\n")
  for (i in seq_along(sds_params)) {
    cat(sprintf("  %d. %s\n", i, sds_params[i]))
  }
} else {
  cat("No sds_ parameters found\n")
}

# Extract and examine zs parameters  
cat("\n=== ZS PARAMETERS ANALYSIS ===\n")
zs_params <- param_names[grepl("^zs_", param_names)]

cat("zs_ parameters found:", length(zs_params), "\n")
if (length(zs_params) > 0) {
  cat("zs_ parameter names:\n")
  for (i in seq_along(zs_params[1:min(20, length(zs_params))])) {
    cat(sprintf("  %d. %s\n", i, zs_params[i]))
  }
  if (length(zs_params) > 20) {
    cat("  ... and", length(zs_params) - 20, "more zs_ parameters\n")
  }
} else {
  cat("No zs_ parameters found\n")
}

# Examine standata for Zs matrices
cat("\n=== STANDATA ZS MATRICES ===\n")
stan_data <- model$model_data
zs_matrices <- names(stan_data)[grepl("^Zs", names(stan_data))]

cat("Zs matrices found in standata:", length(zs_matrices), "\n")
if (length(zs_matrices) > 0) {
  for (zs_name in zs_matrices) {
    zs_matrix <- stan_data[[zs_name]]
    if (is.matrix(zs_matrix) || is.array(zs_matrix)) {
      cat(sprintf("  %s: dimensions %s\n", 
                  zs_name, 
                  paste(dim(zs_matrix), collapse=" x ")))
    } else {
      cat(sprintf("  %s: length %d\n", zs_name, length(zs_matrix)))
    }
  }
} else {
  cat("No Zs matrices found in standata\n")
}

# Look for other smooth-related data structures
cat("\n=== OTHER SMOOTH STRUCTURES ===\n")
smooth_related <- names(stan_data)[grepl("(smooth|rho|sigma)", names(stan_data), 
                                         ignore.case = TRUE)]
if (length(smooth_related) > 0) {
  cat("Other smooth-related structures:\n")
  for (name in smooth_related) {
    obj <- stan_data[[name]]
    if (is.matrix(obj) || is.array(obj)) {
      cat(sprintf("  %s: dimensions %s\n", name, 
                  paste(dim(obj), collapse=" x ")))
    } else {
      cat(sprintf("  %s: length %d (class: %s)\n", 
                  name, length(obj), class(obj)[1]))
    }
  }
} else {
  cat("No other smooth-related structures found\n")
}

# Summary analysis for t2() smooth sds parameters
cat("\n=== T2() SMOOTH SDS SUMMARY ===\n")
cat("Expected sds parameters for t2() smooth: 1 (based on extraction logic)\n")
cat("Actual sds parameters found:", length(sds_params), "\n")

if (length(sds_params) > 1) {
  cat("\nIMPORTANT: t2() smooth generates MULTIPLE sds parameters!\n")
  cat("This explains why the extraction code fails when it expects only 1.\n")
  cat("t2() smooths are tensor product smooths that can have multiple\n")
  cat("smoothing parameters for different dimensions/terms.\n")
  
  # Try to understand the structure
  cat("\nAnalyzing sds parameter structure:\n")
  sds_stems <- gsub("\\[.*\\]", "", sds_params)
  unique_stems <- unique(sds_stems)
  cat("Unique sds parameter stems:", length(unique_stems), "\n")
  for (stem in unique_stems) {
    count <- sum(sds_stems == stem)
    cat(sprintf("  %s: %d parameter(s)\n", stem, count))
  }
} else if (length(sds_params) == 1) {
  cat("Only 1 sds parameter found - extraction should work correctly\n")
} else {
  cat("No sds parameters found - unexpected for a model with smooths\n")
}

cat("\nAnalysis complete.\n")