# Test nonlinear formula support with fit9
# ALWAYS start with devtools::load_all()
devtools::load_all()

# Load fit9 (nonlinear model)
cat("Loading fit9 fixture...\n")
fit9 <- readRDS("tasks/fixtures/fit9.rds")

# Verify fit9 has nonlinear formula
cat("Checking if fit9 has nonlinear formula...\n")
has_nl <- has_nlpars(fit9$formula)
cat("  has_nlpars(fit9$formula):", has_nl, "\n")

if (!has_nl) {
  stop("ERROR: fit9 should have nonlinear formula but has_nlpars() returned FALSE")
}

# Print formula structure
cat("\nFormula structure:\n")
print(str(fit9$formula, max.level = 2))

# Extract observation parameters
cat("\nExtracting observation parameters...\n")
obs_params <- extract_obs_parameters(fit9)
cat("  Found", length(obs_params), "observation parameters\n")
cat("  First few:", head(obs_params, 10), "\n")

# Create parameter subset
cat("\nCreating parameter subset...\n")
draws <- posterior::as_draws(fit9$fit)
obs_draws <- posterior::subset_draws(draws, variable = obs_params)
cat("  Subset has", posterior::nvariables(obs_draws), "parameters\n")

# Convert to draws_matrix for mock stanfit
obs_draws_matrix <- posterior::as_draws_matrix(obs_draws)
cat("  Converted to draws_matrix\n")

# Create mock stanfit
cat("\nCreating mock stanfit...\n")
mock_fit <- create_mock_stanfit(obs_draws_matrix)
cat("  Mock stanfit class:", class(mock_fit), "\n")

# Create newdata (same structure as training data)
cat("\nCreating newdata...\n")
original_data <- fit9$model_data
newdata <- original_data[1:min(20, nrow(original_data)), ]
cat("  newdata has", nrow(newdata), "rows\n")

# Generate prep object using S3 method
cat("\nGenerating prep object with prepare_predictions.mock_stanfit()...\n")
prep <- tryCatch({
  prepare_predictions(mock_fit, brmsfit = fit9, newdata = newdata)
}, error = function(e) {
  cat("ERROR in prepare_predictions:\n")
  cat("  ", conditionMessage(e), "\n")
  return(NULL)
})

if (is.null(prep)) {
  stop("Failed to create prep object")
}

cat("  prep class:", class(prep), "\n")
cat("  prep$nobs:", prep$nobs, "\n")

# Check for dpars$mu
if ("dpars" %in% names(prep)) {
  cat("  prep$dpars exists\n")
  if ("mu" %in% names(prep$dpars)) {
    cat("  prep$dpars$mu dimensions:", 
        paste(dim(prep$dpars$mu), collapse = " × "), "\n")
  } else {
    cat("  WARNING: prep$dpars$mu NOT found\n")
  }
} else {
  cat("  WARNING: prep$dpars NOT found\n")
}

# Test extract_linpred_from_prep
cat("\nTesting extract_linpred_from_prep()...\n")
linpred <- tryCatch({
  extract_linpred_from_prep(prep)
}, error = function(e) {
  cat("ERROR in extract_linpred_from_prep:\n")
  cat("  ", conditionMessage(e), "\n")
  return(NULL)
})

if (is.null(linpred)) {
  stop("Failed to extract linear predictor")
}

# Validate results
cat("  Success! Linear predictor extracted\n")
cat("  Dimensions:", paste(dim(linpred), collapse = " × "), "\n")
cat("  Class:", class(linpred), "\n")
cat("  Range:", round(range(linpred), 3), "\n")
cat("  Any NA?:", any(is.na(linpred)), "\n")
cat("  All finite?:", all(is.finite(linpred)), "\n")

cat("\n=== TEST PASSED ===\n")
