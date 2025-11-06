# Exploration script for brms::prepare_predictions()
# Task 2.3: Understand the structure and usage of brms prepare_predictions
# for both observation and trend models across multiple model types

# CRITICAL: Load all functions using devtools::load_all()
# This ensures we're testing the current development version
devtools::load_all()

# Load additional required packages
library(posterior)
library(brms)

# ==============================================================================
# Task 2.3.1: Load models and explore basic parameter extraction
# ==============================================================================

cat("Loading fitted models from fixtures...\n")

# Load multiple model types for comprehensive exploration
fit1 <- readRDS("tasks/fixtures/fit1.rds")  # Basic RW (univariate)
fit2 <- readRDS("tasks/fixtures/fit2.rds")  # Multivariate shared RW
fit3 <- readRDS("tasks/fixtures/fit3.rds")  # VARMA with smooths

cat("Models loaded successfully\n\n")

# ==============================================================================
# Explore fit1: Basic RW model (univariate)
# ==============================================================================

cat(strrep("=", 78), "\n")
cat("EXPLORING FIT1: Basic RW Model (univariate)\n")
cat(strrep("=", 78), "\n\n")

# Extract observation parameters
cat("Extracting observation parameters from fit1...\n")
obs_params_1 <- extract_obs_parameters(fit1)
cat("Observation parameters:\n")
print(obs_params_1)
cat("\n")

# Extract trend parameters
cat("Extracting trend parameters from fit1...\n")
trend_params_1 <- extract_trend_parameters(fit1)
cat("Trend parameters:\n")
print(trend_params_1)
cat("\n")

# Create parameter subset for observation model
cat("Creating observation draws subset...\n")
full_draws_1 <- posterior::as_draws_matrix(fit1$fit)
cat("Full draws dimensions:", dim(full_draws_1), "\n")

obs_draws_1 <- posterior::subset_draws(full_draws_1, variable = obs_params_1)
cat("Observation draws dimensions:", dim(obs_draws_1), "\n")
cat("Observation draws class:", class(obs_draws_1), "\n")
cat("Observation draws structure:\n")
str(obs_draws_1, max.level = 1)
cat("\n")

# Check if subsetting preserved all required classes
required_classes <- c("draws_matrix", "draws", "matrix", "array")
has_all_classes <- all(required_classes %in% class(obs_draws_1))
cat("Has all required classes:", has_all_classes, "\n")
if (!has_all_classes) {
  missing <- setdiff(required_classes, class(obs_draws_1))
  cat("Missing classes:", paste(missing, collapse = ", "), "\n")
}
cat("\n")

# Create mock stanfit
cat("Creating mock stanfit for observation model...\n")
mock_obs_1 <- create_mock_stanfit(obs_draws_1)
cat("Mock stanfit class:", class(mock_obs_1), "\n")
cat("Mock stanfit created successfully\n\n")

# ==============================================================================
# Explore fit2: Multivariate shared RW model
# ==============================================================================

cat(strrep("=", 78), "\n")
cat("EXPLORING FIT2: Multivariate Shared RW Model\n")
cat(strrep("=", 78), "\n\n")

cat("Response names:", fit2$response_names, "\n\n")

# Extract observation parameters
cat("Extracting observation parameters from fit2...\n")
obs_params_2 <- extract_obs_parameters(fit2)
cat("Observation parameters (first 20):\n")
print(head(obs_params_2, 20))
cat("Total observation parameters:", length(obs_params_2), "\n\n")

# Extract trend parameters
cat("Extracting trend parameters from fit2...\n")
trend_params_2 <- extract_trend_parameters(fit2)
cat("Trend parameters:\n")
print(trend_params_2)
cat("\n")

# Create parameter subsets
full_draws_2 <- posterior::as_draws_matrix(fit2$fit)
cat("Full draws dimensions:", dim(full_draws_2), "\n")

obs_draws_2 <- posterior::subset_draws(full_draws_2, variable = obs_params_2)
cat("Observation draws dimensions:", dim(obs_draws_2), "\n\n")

# ==============================================================================
# Explore fit3: VARMA with smooths
# ==============================================================================

cat(strrep("=", 78), "\n")
cat("EXPLORING FIT3: VARMA Model with Smooths\n")
cat(strrep("=", 78), "\n\n")

cat("Response names:", fit3$response_names, "\n\n")

# Extract observation parameters
cat("Extracting observation parameters from fit3...\n")
obs_params_3 <- extract_obs_parameters(fit3)
cat("Observation parameters (first 20):\n")
print(head(obs_params_3, 20))
cat("Total observation parameters:", length(obs_params_3), "\n\n")

# Extract trend parameters
cat("Extracting trend parameters from fit3...\n")
trend_params_3 <- extract_trend_parameters(fit3)
cat("Trend parameters (first 20):\n")
print(head(trend_params_3, 20))
cat("Total trend parameters:", length(trend_params_3), "\n\n")

# Create parameter subsets
full_draws_3 <- posterior::as_draws_matrix(fit3$fit)
cat("Full draws dimensions:", dim(full_draws_3), "\n")

obs_draws_3 <- posterior::subset_draws(full_draws_3, variable = obs_params_3)
cat("Observation draws dimensions:", dim(obs_draws_3), "\n")

trend_draws_3 <- posterior::subset_draws(full_draws_3, variable = trend_params_3)
cat("Trend draws dimensions:", dim(trend_draws_3), "\n\n")

cat(strrep("=", 78), "\n")
cat("TASK 2.3.3: Test prepare_predictions.mock_stanfit() Implementation\n")
cat(strrep("=", 78), "\n\n")

# Create simple newdata for predictions
cat("Creating newdata for predictions...\n")
newdata <- data.frame(
  time = 25:27,
  series = factor(rep("series1", 3)),
  x = c(0.5, -0.3, 0.8)
)
cat("newdata structure:\n")
print(newdata)
cat("\n")

# ==============================================================================
# Test 1: fit1 (Basic RW model)
# ==============================================================================

cat(strrep("-", 78), "\n")
cat("TEST 1: fit1 - Basic RW Model (univariate)\n")
cat(strrep("-", 78), "\n\n")

if (!is.null(fit1$obs_model)) {
  cat("Testing prepare_predictions.mock_stanfit() with fit1...\n\n")

  # Call our new S3 method
  cat("Calling prepare_predictions(mock_obs_1, brmsfit = fit1$obs_model, ",
      "newdata = newdata)...\n")

  tryCatch({
    prep1 <- prepare_predictions(
      mock_obs_1,
      brmsfit = fit1$obs_model,
      newdata = newdata
    )

    cat("SUCCESS: prepare_predictions.mock_stanfit() dispatched correctly!\n\n")

    # Document the prep structure
    cat("prep object class:\n")
    print(class(prep1))
    cat("\n")

    cat("prep object top-level names:\n")
    print(names(prep1))
    cat("\n")

    cat("prep structure (max.level = 2):\n")
    str(prep1, max.level = 2)
    cat("\n")

    # Verify key components exist
    cat("Key component verification:\n")
    cat("- Has X (fixed effects):", !is.null(prep1$X), "\n")
    cat("- Has formula:", !is.null(prep1$formula), "\n")
    cat("- Has family:", !is.null(prep1$family), "\n")
    cat("- Has draws:", !is.null(prep1$draws), "\n")
    cat("- Has standata:", !is.null(prep1$standata), "\n")
    cat("\n")

    # Check design matrix dimensions
    if (!is.null(prep1$X)) {
      cat("Design matrix X dimensions:", dim(prep1$X), "\n")
      cat("Design matrix X column names:", colnames(prep1$X), "\n")
    }
    cat("\n")

  }, error = function(e) {
    cat("ERROR: prepare_predictions.mock_stanfit() failed for fit1:\n")
    cat(conditionMessage(e), "\n")
    cat("Traceback:\n")
    print(traceback())
    cat("\n")
  })

} else {
  cat("WARNING: fit1$obs_model is NULL - cannot test\n\n")
}

# ==============================================================================
# Test 2: fit2 (Multivariate shared RW)
# ==============================================================================

cat(strrep("-", 78), "\n")
cat("TEST 2: fit2 - Multivariate Shared RW Model\n")
cat(strrep("-", 78), "\n\n")

if (!is.null(fit2$obs_model)) {
  cat("Testing prepare_predictions.mock_stanfit() with fit2...\n\n")

  # Create mock for fit2
  mock_obs_2 <- create_mock_stanfit(obs_draws_2)

  # Create appropriate newdata for multivariate model
  newdata_mv <- data.frame(
    time = rep(25:27, each = 2),
    series = factor(rep(c("series1", "series2"), 3)),
    x = rnorm(6)
  )

  cat("newdata for multivariate model:\n")
  print(newdata_mv)
  cat("\n")

  tryCatch({
    prep2 <- prepare_predictions(
      mock_obs_2,
      brmsfit = fit2$obs_model,
      newdata = newdata_mv
    )

    cat("SUCCESS: prepare_predictions.mock_stanfit() worked for multivariate!\n\n")

    cat("prep2 structure (max.level = 2):\n")
    str(prep2, max.level = 2)
    cat("\n")

    # Check for multivariate-specific features
    cat("Multivariate-specific checks:\n")
    cat("- Response names:", fit2$response_names, "\n")
    if (!is.null(prep2$X)) {
      cat("- Design matrix X dimensions:", dim(prep2$X), "\n")
    }
    cat("\n")

  }, error = function(e) {
    cat("ERROR: prepare_predictions.mock_stanfit() failed for fit2:\n")
    cat(conditionMessage(e), "\n\n")
  })

} else {
  cat("WARNING: fit2$obs_model is NULL - cannot test\n\n")
}

# ==============================================================================
# Test 3: fit3 (VARMA with smooths)
# ==============================================================================

cat(strrep("-", 78), "\n")
cat("TEST 3: fit3 - VARMA Model with Smooths\n")
cat(strrep("-", 78), "\n\n")

if (!is.null(fit3$obs_model)) {
  cat("Testing prepare_predictions.mock_stanfit() with fit3...\n\n")

  # Create mock for fit3
  mock_obs_3 <- create_mock_stanfit(obs_draws_3)

  tryCatch({
    prep3 <- prepare_predictions(
      mock_obs_3,
      brmsfit = fit3$obs_model,
      newdata = newdata_mv
    )

    cat("SUCCESS: prepare_predictions.mock_stanfit() worked with smooths!\n\n")

    cat("prep3 structure (max.level = 2):\n")
    str(prep3, max.level = 2)
    cat("\n")

    # Check for smooth-specific features
    cat("Smooth-specific checks:\n")
    cat("- Has Xs (smooth basis):", !is.null(prep3$Xs), "\n")
    if (!is.null(prep3$Xs)) {
      cat("- Xs is a list:", is.list(prep3$Xs), "\n")
      cat("- Number of smooth terms:", length(prep3$Xs), "\n")
    }
    cat("\n")

  }, error = function(e) {
    cat("ERROR: prepare_predictions.mock_stanfit() failed for fit3:\n")
    cat(conditionMessage(e), "\n\n")
  })

} else {
  cat("WARNING: fit3$obs_model is NULL - cannot test\n\n")
}

cat("\n")
cat(strrep("=", 78), "\n")
cat("TASK 2.3.3 COMPLETE - prep structure documented\n")
cat(strrep("=", 78), "\n")
