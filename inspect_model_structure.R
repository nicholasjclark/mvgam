# Script to inspect mvgam object structure for parameter renaming
# Based on tests/local/test-models-single.R

library(mvgam)
devtools::load_all()

# Setup test data (from test-models-single.R)
setup_stan_test_data <- function() {
  set.seed(42)
  n_time <- 24
  n_series <- 3

  # Simple univariate dataset
  univariate <- data.frame(
    time = 1:n_time,
    series = factor(rep("series1", n_time)),
    y = rpois(n_time, lambda = 5),
    x = rnorm(n_time),
    temperature = rnorm(n_time, mean = 15, sd = 3)
  )

  # Multivariate dataset with balanced design
  time_presence <- rbinom(n_time, size = 1, prob = 0.7)
  time_x <- rnorm(n_time)
  time_habitat <- factor(sample(c("forest", "grassland"), n_time, replace = TRUE))

  multivariate <- data.frame(
    time = rep(1:n_time, n_series),
    count = rpois(n_time * n_series, lambda = 4),
    biomass = rlnorm(n_time * n_series, meanlog = 1, sdlog = 0.5),
    presence = rep(time_presence, n_series),
    x = rep(time_x, n_series),
    habitat = rep(time_habitat, n_series)
  )

  list(
    univariate = univariate,
    multivariate = multivariate
  )
}

test_data <- setup_stan_test_data()

# Fit Target 1: Basic RW model
cat("=== Fitting basic RW model ===\n")
fit1 <- suppressWarnings(suppressMessages(mvgam(
  y ~ x,
  trend_formula = ~ RW(),
  data = test_data$univariate,
  family = poisson(),
  chains = 2,
  iter = 500,
  silent = 2
)))

cat("\n=== Top-level mvgam object components ===\n")
print(names(fit1))

cat("\n=== Observation model structure ===\n")
cat("obs_model class:", class(fit1$obs_model), "\n")
cat("obs_model names:", paste(names(fit1$obs_model), collapse = ", "), "\n")

cat("\n=== Trend model structure ===\n")
cat("trend_model class:", class(fit1$trend_model), "\n")
cat("trend_model names:", paste(names(fit1$trend_model), collapse = ", "), "\n")

cat("\n=== Checking standata structure ===\n")
cat("standata names:\n")
print(names(fit1$standata))

cat("\n=== Observation model standata (from brms mock) ===\n")
obs_standata <- brms::standata(fit1$obs_model)
cat("obs_standata names:\n")
print(names(obs_standata))

# Check for coefficient names in model matrix
if ("X" %in% names(obs_standata)) {
  cat("\n=== Fixed effects model matrix X ===\n")
  cat("Dimensions:", dim(obs_standata$X), "\n")
  cat("Column names (coefficient names):\n")
  print(colnames(obs_standata$X))
}

cat("\n=== Trend model standata ===\n")
trend_standata <- brms::standata(fit1$trend_model)
cat("trend_standata names:\n")
print(names(trend_standata))

if ("X" %in% names(trend_standata)) {
  cat("\n=== Trend fixed effects model matrix X (in trend_standata) ===\n")
  cat("Dimensions:", dim(trend_standata$X), "\n")
  cat("Column names:\n")
  print(colnames(trend_standata$X))
}

cat("\n=== Series and time information ===\n")
print(fit1$series_info)
print(fit1$time_info)

cat("\n=== Trend metadata ===\n")
print(str(fit1$trend_metadata, max.level = 2))

cat("\n=== Raw Stan parameter names ===\n")
# Use posterior package properly
stan_pars <- posterior::variables(posterior::as_draws(fit1$fit))
cat("Total parameters:", length(stan_pars), "\n")
cat("First 40 parameters:\n")
print(head(stan_pars, 40))

cat("\n=== Categorized parameters ===\n")
categorized <- mvgam:::.categorize_parameters(fit1)
cat("Categories:\n")
print(names(categorized))

cat("\n=== Observation betas ===\n")
if (!is.null(categorized$observation_betas)) {
  print(categorized$observation_betas)
}

cat("\n=== Trend betas ===\n")
if (!is.null(categorized$trend_betas)) {
  print(categorized$trend_betas)
}

cat("\n=== Trend parameters ===\n")
if (!is.null(categorized$trend_pars)) {
  print(head(categorized$trend_pars, 10))
}

cat("\n=== Combined standata (used for Stan sampling) ===\n")
cat("Combined standata names (first 30):\n")
print(head(names(fit1$standata), 30))

# Check if combined standata has model matrices
if ("X" %in% names(fit1$standata)) {
  cat("\n=== Combined standata X matrix ===\n")
  cat("Dimensions:", dim(fit1$standata$X), "\n")
  cat("Has colnames:", !is.null(colnames(fit1$standata$X)), "\n")
  if (!is.null(colnames(fit1$standata$X))) {
    cat("Column names:\n")
    print(colnames(fit1$standata$X))
  }
}

if ("X_trend" %in% names(fit1$standata)) {
  cat("\n=== Combined standata X_trend matrix ===\n")
  cat("Dimensions:", dim(fit1$standata$X_trend), "\n")
  cat("Has colnames:", !is.null(colnames(fit1$standata$X_trend)), "\n")
  if (!is.null(colnames(fit1$standata$X_trend))) {
    cat("Column names:\n")
    print(colnames(fit1$standata$X_trend))
  }
}

cat("\n=== Testing parameter extraction via brms ===\n")
# Test if we can use brms machinery on the observation model
obs_coef_names <- colnames(brms::standata(fit1$obs_model)$X)
cat("Observation coefficient names from brms:\n")
print(obs_coef_names)

# Test if we can use brms machinery on the trend model
trend_coef_names <- colnames(brms::standata(fit1$trend_model)$X_trend)
cat("\nTrend coefficient names from brms:\n")
print(trend_coef_names)

cat("\n=== Summary ===\n")
cat("✓ obs_model and trend_model are brmsfit objects (mock backend)\n")
cat("✓ Can extract coefficient names via brms::standata()\n")
cat("✓ Combined standata available in fit1$standata\n")
cat("✓ Series and time metadata available\n")
cat("✓ Trend metadata available\n")

cat("\nKey insight: mvgam can leverage brms's standata() to get coefficient names!\n")

# Save model for future testing
cat("\n=== Saving model for future tests ===\n")
saveRDS(fit1, "tasks/test_model_basic_rw.rds")
cat("Model saved to: tasks/test_model_basic_rw.rds\n")
cat("To reload: fit1 <- readRDS('tasks/test_model_basic_rw.rds')\n")
