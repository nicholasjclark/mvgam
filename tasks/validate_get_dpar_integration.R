# Validate get_dpar() Integration for mvgam
#
# This script tests how brms's get_dpar() works and explores how mvgam
# can leverage it for distributional parameter extraction.
#
# Uses cached models from tasks/fixtures - NO NEW FITTING

devtools::load_all()
library(brms)
library(posterior)

FIXTURE_DIR <- "tasks/fixtures"

# =============================================================================
# PART 1: Understand brms prep$dpars structure
# =============================================================================

cat("\n========================================\n")
cat("PART 1: brms prep$dpars structure\n")
cat("========================================\n")

# Load cached brms model (Poisson with fixed effects)
brms_fit <- readRDS(file.path(FIXTURE_DIR, "val_brms_ar1_fx.rds"))
cat("Loaded brms model:", class(brms_fit)[1], "\n")
cat("Family:", brms_fit$family$family, "\n")

# Get the original data
test_data <- brms_fit$data
cat("Data rows:", nrow(test_data), "\n")

# Create prep object using brms's prepare_predictions
cat("\nCreating brms prep object...\n")
prep <- brms::prepare_predictions(brms_fit, newdata = test_data)

# Examine prep structure
cat("\n--- prep object class ---\n")
print(class(prep))

cat("\n--- prep$dpars names ---\n")
print(names(prep$dpars))

cat("\n--- prep$dpars$mu structure ---\n")
if (!is.null(prep$dpars$mu)) {
  cat("Class:", paste(class(prep$dpars$mu), collapse = ", "), "\n")
  # bprepl is a lazy evaluation object - check its structure
  if (inherits(prep$dpars$mu, "bprepl")) {
    cat("Type: bprepl (lazy evaluation object)\n")
    cat("Components:", names(prep$dpars$mu), "\n")
  } else {
    cat("Dim:", dim(prep$dpars$mu), "\n")
    cat("Range:", range(prep$dpars$mu), "\n")
  }
}

cat("\n--- prep$family structure ---\n")
cat("Family name:", prep$family$family, "\n")
cat("Link:", prep$family$link, "\n")
cat("dpars field names:", names(prep$family$dpars), "\n")

# Test get_dpar() directly
cat("\n--- Testing get_dpar() ---\n")
# Check the actual function signature
cat("get_dpar signature:\n")
print(args(brms:::get_dpar))

tryCatch({
  mu_vals <- brms:::get_dpar(prep, "mu", i = 1:5)
  cat("mu[1:5] class:", paste(class(mu_vals), collapse = ", "), "\n")
  cat("mu[1:5] dims:", dim(mu_vals), "\n")
  cat("mu[1:5] range:", range(mu_vals), "\n")
}, error = function(e) {
  cat("ERROR:", conditionMessage(e), "\n")
})

# =============================================================================
# PART 2: Check Gaussian model for sigma parameter
# =============================================================================

cat("\n========================================\n")
cat("PART 2: Gaussian model sigma extraction\n")
cat("========================================\n")

# Load fit1 which should be Gaussian
fit1 <- readRDS(file.path(FIXTURE_DIR, "fit1.rds"))
cat("Loaded fit1, family:", fit1$family$family, "\n")

# Check if this is an mvgam object with obs_model
if (inherits(fit1, "mvgam") && !is.null(fit1$obs_model)) {
  brms_obs <- fit1$obs_model
  cat("obs_model family:", brms_obs$family$family, "\n")

  # Create prep from obs_model
  prep_obs <- brms::prepare_predictions(brms_obs, newdata = fit1$data)

  cat("\n--- Gaussian prep$dpars ---\n")
  cat("dpars names:", names(prep_obs$dpars), "\n")

  # Test get_dpar for sigma
  cat("\nTesting get_dpar(sigma)...\n")
  tryCatch({
    sigma_vals <- brms:::get_dpar(prep_obs, "sigma", i = 1:5)
    cat("SUCCESS! sigma class:", paste(class(sigma_vals), collapse = ", "), "\n")
    cat("sigma dims:", dim(sigma_vals), "\n")
    cat("sigma range:", range(sigma_vals), "\n")
  }, error = function(e) {
    cat("ERROR:", conditionMessage(e), "\n")
  })
}

# =============================================================================
# PART 3: Check posterior draws for distributional params
# =============================================================================

cat("\n========================================\n")
cat("PART 3: Posterior draws structure\n")
cat("========================================\n")

# Load an mvgam fit
mvgam_fit <- readRDS(file.path(FIXTURE_DIR, "val_mvgam_ar1_fx.rds"))
cat("Loaded mvgam model\n")
cat("Family:", mvgam_fit$family$family, "\n")

# Get all parameter names
draws <- as_draws_matrix(mvgam_fit$fit)
all_params <- colnames(draws)

cat("\n--- Distributional parameter names in draws ---\n")
dpar_pattern <- "^(sigma|shape|nu|phi|zi|hu)(_|\\[|$)"
dpar_params <- grep(dpar_pattern, all_params, value = TRUE)
cat("Found dpars:", dpar_params, "\n")

# Check obs_parameters
obs_params <- extract_obs_parameters(mvgam_fit)
cat("\n--- Obs parameters (first 20) ---\n")
print(head(obs_params, 20))

# Filter for dpar-like params in obs_params
obs_dpars <- grep(dpar_pattern, obs_params, value = TRUE)
cat("\n--- Obs dpars ---\n")
print(obs_dpars)

# =============================================================================
# PART 4: Test mvgam prep object with manual dpar injection
# =============================================================================

cat("\n========================================\n")
cat("PART 4: mvgam prep with dpar injection\n")
cat("========================================\n")

# Create mvgam prep object
obs_params <- extract_obs_parameters(mvgam_fit)
full_draws <- posterior::as_draws_matrix(mvgam_fit$fit)
obs_draws <- full_draws[, obs_params, drop = FALSE]
mock_fit <- create_mock_stanfit(obs_draws)

mvgam_prep <- prepare_predictions.mock_stanfit(
  object = mock_fit,
  brmsfit = mvgam_fit$obs_model,
  newdata = mvgam_fit$data
)

cat("\n--- mvgam prep structure ---\n")
cat("Class:", class(mvgam_prep), "\n")
cat("dpars names:", names(mvgam_prep$dpars), "\n")
cat("ndraws:", mvgam_prep$ndraws %||% nrow(mvgam_prep$draws), "\n")

# Check family for sigma info
cat("\n--- Family dpar info ---\n")
cat("Family:", mvgam_prep$family$family, "\n")
if (!is.null(mvgam_prep$family$dpars)) {
  print(names(mvgam_prep$family$dpars))
}

# Try get_dpar before injection
cat("\nTrying get_dpar(mu) on mvgam prep...\n")
tryCatch({
  mu_test <- brms:::get_dpar(mvgam_prep, "mu", i = 1:5)
  cat("SUCCESS! mu class:", paste(class(mu_test), collapse = ", "), "\n")
  cat("mu dims:", dim(mu_test), "\n")
}, error = function(e) {
  cat("ERROR:", conditionMessage(e), "\n")
})

# =============================================================================
# PART 5: Explore how brms populates dpars
# =============================================================================

cat("\n========================================\n")
cat("PART 5: brms dpar population mechanism\n")
cat("========================================\n")

# Compare brms prep with mvgam prep
cat("\n--- brms prep (from brmsfit) ---\n")
brms_prep <- brms::prepare_predictions(brms_fit, newdata = brms_fit$data)
cat("dpars:", names(brms_prep$dpars), "\n")
cat("dpars$mu class:", paste(class(brms_prep$dpars$mu), collapse = ", "), "\n")
if (!inherits(brms_prep$dpars$mu, "bprepl")) {
  cat("dpars$mu dim:", dim(brms_prep$dpars$mu), "\n")
} else {
  cat("dpars$mu type: bprepl (lazy)\n")
}

# Check what's in draws vs dpars
cat("\n--- Draws structure ---\n")
cat("draws class:", paste(class(brms_prep$draws), collapse = ", "), "\n")
if (!is.null(brms_prep$draws)) {
  if (!is.null(colnames(brms_prep$draws))) {
    cat("draws columns (first 20):", head(colnames(brms_prep$draws), 20), "\n")
  } else if (!is.null(names(brms_prep$draws))) {
    cat("draws names (first 20):", head(names(brms_prep$draws), 20), "\n")
  }
}

cat("\n--- sdata fields (first 20) ---\n")
print(head(names(brms_prep$sdata), 20))

# =============================================================================
# PART 6: Solution - understand the dpar computation flow
# =============================================================================

cat("\n========================================\n")
cat("PART 6: dpar computation flow\n")
cat("========================================\n")

# The key insight: brms computes dpars LAZILY in get_dpar()
# prep$dpars is populated during prepare_predictions() for distributional models
# For non-distributional models, dpars are computed on-the-fly

cat("
FINDINGS:
1. prep$dpars$mu is computed during prepare_predictions()
2. For Poisson, there's only 'mu' dpar (no sigma/shape)
3. For Gaussian, 'sigma' would be a dpar
4. get_dpar() handles both pre-computed dpars and scalar defaults

The solution for mvgam posterior_predict:
- Use the same prep object from posterior_epred
- For families needing sigma/shape, extract from stanfit
- Either inject into prep$dpars or call get_dpar() which handles defaults
")

# Check if mu is already in prep for mvgam
cat("\n--- mvgam_prep$dpars content ---\n")
if (!is.null(mvgam_prep$dpars)) {
  for (nm in names(mvgam_prep$dpars)) {
    cat(nm, ":", class(mvgam_prep$dpars[[nm]]),
        "dim:", dim(mvgam_prep$dpars[[nm]]), "\n")
  }
} else {
  cat("dpars is NULL\n")
}

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n========================================\n")
cat("SUMMARY\n")
cat("========================================\n")

cat("
Key findings from cached model analysis:

1. brms prep$dpars contains linear predictors for distributional params
   - For Poisson: only 'mu' (no sigma needed)
   - For Gaussian: 'mu' and 'sigma' (if modeled)
   - For NegBinom: 'mu' and 'shape'

2. get_dpar() handles:
   - Pre-computed dpars in prep$dpars
   - Scalar defaults from family (e.g., constant sigma)
   - Inverse link transformation when ilink=TRUE

3. mvgam's prepare_predictions.mock_stanfit():
   - Creates prep object with sdata and draws
   - Does NOT populate prep$dpars (except for nonlinear models)
   - mu is computed via extract_linpred_from_prep()

4. For posterior_predict, we need:
   - mu (already have via posterior_epred)
   - sigma/shape/phi/nu/zi/hu (need to extract from stanfit)

SOLUTION OPTIONS:
A. Modify prepare_predictions.mock_stanfit() to populate dpars
B. Extract dpars separately in posterior_predict.mvgam()
C. Create a shared helper that returns prep with dpars populated

Option B is simplest - extract dpars from stanfit and pass to sample_from_family()
")
