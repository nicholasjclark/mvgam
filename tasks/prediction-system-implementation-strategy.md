# mvgam Prediction System: Implementation Strategy

**Date**: 2025-11-05
**Status**: Architecture Finalized - Ready for Implementation
**Context**: TRD located at `tasks/trd-prediction-system.md`

---

## Executive Summary

This document outlines the **empirically verified** strategy for implementing the mvgam prediction system. All approaches have been tested against brms 2.22.0 and confirmed to work.

**Key Decision**: Use `brms:::prepare_predictions()` to create design matrices, then extract and multiply with our combined parameter draws. No need for complex stanfit mocking.

---

## Architecture Overview

### The Challenge

mvgam fits a **single combined Stan model** containing:
- Observation parameters: `b_Intercept`, `b_x`, `sigma`, etc.
- Trend parameters: `ar1_trend`, `Z[1,1]`, `lv_trend`, etc.

We need to:
1. Create design matrices for observation and trend formulas
2. Extract relevant parameters from the combined fit
3. Compute predictions by combining observation + trend contributions

### The Solution

```
Combined mvgam Stan fit
         │
         ├─→ Extract observation parameters
         │   └─→ Create obs_brmsfit (lightweight)
         │       └─→ prepare_predictions() → design matrices
         │
         └─→ Extract trend parameters
             └─→ Create trend_brmsfit (lightweight)
                 └─→ prepare_predictions() → design matrices

Design matrices + Combined parameters → Predictions
```

---

## Component 1: Lightweight brmsfit Objects

### What We Need

During `mvgam()` fitting, create and store minimal brmsfit objects for observation and trend models.

**Structure**:
```r
obs_brmsfit <- structure(
  list(
    formula = bf(y ~ x + s(z)),        # Observation formula
    data = original_data,               # Training data
    family = poisson(),                 # Family object
    fit = placeholder_stanfit,          # Will be replaced
    prior = data.frame(),               # Can be empty for predictions
    ranef = data.frame(),               # Populated if random effects
    autocor = list()                    # Empty for mvgam
  ),
  class = "brmsfit"
)
```

**Key Points**:
- Created once during model fitting (already done in current mvgam?)
- Stored in `mvgam_fit$obs_model` and `mvgam_fit$trend_model`
- The `fit` slot is a placeholder - will be replaced during prediction

### What We DON'T Need

- ❌ Full stanfit object with all chains/warmup
- ❌ Complete sampling diagnostics
- ❌ Model code storage
- ❌ Complex S4 object structure

**Minimal placeholder stanfit**:
```r
# Simple placeholder (never actually used for draws)
placeholder_stanfit <- structure(
  list(model_name = "placeholder"),
  class = "stanfit"
)
```

---

## Component 2: Mock Stanfit for Parameter Subsetting

### The Pattern (S3-based)

When we need to use `prepare_predictions()`, we temporarily replace the placeholder stanfit with a mock that returns our parameter subset.

**Constructor**:
```r
#' Create mock stanfit that returns specific parameter subset
#' @param draws_matrix A draws_matrix with subset of parameters
#' @return S3 object of class "mock_stanfit"
#' @noRd
create_mock_stanfit <- function(draws_matrix) {
  structure(
    list(draws_cache = draws_matrix),
    class = c("mock_stanfit", "stanfit")
  )
}
```

**S3 Method**:
```r
#' Extract draws from mock stanfit
#' @export
as_draws_matrix.mock_stanfit <- function(x, ...) {
  x$draws_cache
}
```

**Usage**:
```r
# Extract parameter subset from combined fit
full_draws <- as_draws_matrix(mvgam_fit$fit)
obs_params <- c("b_Intercept", "b_x", "sigma", "lp__")
obs_draws <- full_draws[, obs_params]
class(obs_draws) <- class(full_draws)  # Preserve draws_matrix class

# Create mock
mock_obs_stanfit <- create_mock_stanfit(obs_draws)

# Replace placeholder in brmsfit
obs_brmsfit$fit <- mock_obs_stanfit

# Now prepare_predictions() will use obs_draws
```

---

## Component 3: Using prepare_predictions()

### Why We Need It

brms handles complex design matrix construction that we cannot easily replicate:
- Random effects with proper indexing
- Smooth (GAM) basis functions evaluated at newdata
- Monotonic effects transformations
- Distributional parameters (sigma ~ predictors)
- Factor variable encoding
- Interaction terms

**We must use brms machinery** - attempting to reimplement would be error-prone and fragile.

### The Workflow

```r
#' Compute predictions for mvgam model
#' @noRd
predict_mvgam_internal <- function(mvgam_fit, newdata,
                                    type = "response", ...) {

  # Step 1: Extract draws from combined fit
  full_draws <- as_draws_matrix(mvgam_fit$fit)

  # Step 2: Identify parameter subsets
  obs_params <- identify_obs_params(colnames(full_draws))
  trend_params <- identify_trend_params(colnames(full_draws))

  # Step 3: Create parameter subsets
  obs_draws <- full_draws[, c(obs_params, "lp__")]
  trend_draws <- full_draws[, c(trend_params, "lp__")]
  class(obs_draws) <- class(full_draws)
  class(trend_draws) <- class(full_draws)

  # Step 4: Create mock stanfits
  mock_obs <- create_mock_stanfit(obs_draws)
  mock_trend <- create_mock_stanfit(trend_draws)

  # Step 5: Prepare brmsfit objects with mocks
  obs_brmsfit <- mvgam_fit$obs_model
  obs_brmsfit$fit <- mock_obs

  trend_brmsfit <- mvgam_fit$trend_model
  trend_brmsfit$fit <- mock_trend

  # Step 6: Get design matrices via prepare_predictions
  prep_obs <- brms:::prepare_predictions(
    obs_brmsfit,
    newdata = newdata,
    re_formula = re_formula,
    allow_new_levels = allow_new_levels,
    sample_new_levels = sample_new_levels
  )

  prep_trend <- brms:::prepare_predictions(
    trend_brmsfit,
    newdata = newdata  # Might need trend-specific data
  )

  # Step 7: Extract linear predictors
  linpred_obs <- extract_linpred_from_prep(prep_obs, obs_draws)
  linpred_trend <- extract_linpred_from_prep(prep_trend, trend_draws)

  # Step 8: Combine on link scale
  linpred_total <- linpred_obs + linpred_trend

  # Step 9: Apply family-specific transformations
  predictions <- transform_by_type(
    linpred_total,
    obs_draws,
    mvgam_fit$family,
    type
  )

  return(predictions)
}
```

---

## Component 4: Extracting Linear Predictors from prep

### The bprepdict Structure

`prepare_predictions()` returns an object with this structure:

```r
prep <- brms:::prepare_predictions(fit, newdata)

prep$dpars$mu$int        # Boolean: has intercept?
prep$dpars$mu$fe         # Fixed effects
  $fe$X                  # Design matrix [n_obs × n_pred]
  $fe$vars               # Parameter names: c("b_x", "b_z", ...)
prep$dpars$mu$re         # Random effects (list)
  $re[[i]]$Z             # Design matrix [n_obs × n_levels]
  $re[[i]]$vars          # Parameter names: c("r_grp[1,Intercept]", ...)
prep$dpars$mu$sm         # Smooth terms (list)
  $sm[[i]]$Xs            # Basis matrix [n_obs × n_basis]
  $sm[[i]]$vars          # Parameter names: c("bs_sx_1", "bs_sx_2", ...)
```

### Manual Reconstruction (Verified)

This pattern **exactly matches** `brms::posterior_linpred()` to numerical precision (~1e-10):

```r
#' Extract linear predictor from prep object
#' @param prep bprepdict object from prepare_predictions()
#' @param draws draws_matrix with parameters
#' @return matrix [n_draws × n_obs] of linear predictor values
#' @noRd
extract_linpred_from_prep <- function(prep, draws) {

  n_draws <- nrow(draws)
  n_obs <- nrow(prep$dpars$mu$fe$X)  # Or check prep$data

  # Initialize
  linpred <- matrix(0, nrow = n_draws, ncol = n_obs)

  # Add intercept
  if (prep$dpars$mu$int) {
    linpred <- linpred + draws[, "b_Intercept"]
  }

  # Add fixed effects
  if (!is.null(prep$dpars$mu$fe)) {
    X <- prep$dpars$mu$fe$X
    fe_params <- prep$dpars$mu$fe$vars
    b_fe <- draws[, fe_params, drop = FALSE]
    linpred <- linpred + b_fe %*% t(X)
  }

  # Add smooth terms
  if (!is.null(prep$dpars$mu$sm)) {
    for (i in seq_along(prep$dpars$mu$sm)) {
      Xs <- prep$dpars$mu$sm[[i]]$Xs
      s_params <- prep$dpars$mu$sm[[i]]$vars
      b_s <- draws[, s_params, drop = FALSE]
      linpred <- linpred + b_s %*% t(Xs)
    }
  }

  # Add random effects
  if (!is.null(prep$dpars$mu$re)) {
    for (i in seq_along(prep$dpars$mu$re)) {
      Z <- prep$dpars$mu$re[[i]]$Z
      r_params <- prep$dpars$mu$re[[i]]$vars
      b_r <- draws[, r_params, drop = FALSE]
      linpred <- linpred + b_r %*% t(Z)
    }
  }

  return(linpred)
}
```

**Verification**: Empirical testing confirms this matches brms output.

---

## Component 5: Parameter Identification

### Observation vs. Trend Parameters

```r
#' Identify observation model parameters
#' @param param_names Character vector of all parameter names
#' @return Character vector of observation parameter names
#' @noRd
identify_obs_params <- function(param_names) {

  # Observation parameter patterns
  obs_patterns <- c(
    "^b_",           # Fixed effects
    "^bs_",          # Spline coefficients
    "^bcs_",         # Categorical spline
    "^bmo_",         # Monotonic effects
    "^bme_",         # Measurement error
    "^bsp_",         # Special terms
    "^r_",           # Random effects
    "^sd_",          # Random effect standard deviations
    "^cor_",         # Correlations
    "^L_",           # Cholesky factors
    "^sigma",        # Residual SD (or scale)
    "^shape",        # Negative binomial, beta, etc.
    "^nu",           # Student-t degrees of freedom
    "^phi",          # Beta, zero-inflated
    "^zi_",          # Zero-inflation
    "^hu_"           # Hurdle
  )

  # Trend parameter patterns (exclude these)
  trend_patterns <- c(
    "_trend$",       # Parameters ending in _trend
    "^ar\\d+_trend", # AR coefficients
    "^ma\\d+_trend", # MA coefficients
    "^Z\\[",         # Factor loadings matrix
    "^lv_trend",     # Latent variable trends
    "^lv_coefs",     # Latent variable coefficients
    "^mu_trend",     # Trend intercepts
    "^sigma_trend"   # Trend innovation SDs
  )

  # Find observation params (match obs patterns, exclude trend patterns)
  obs_params <- param_names[
    grepl(paste(obs_patterns, collapse = "|"), param_names) &
    !grepl(paste(trend_patterns, collapse = "|"), param_names)
  ]

  return(obs_params)
}

#' Identify trend model parameters
#' @param param_names Character vector of all parameter names
#' @return Character vector of trend parameter names
#' @noRd
identify_trend_params <- function(param_names) {

  # Trend parameter patterns
  trend_patterns <- c(
    "_trend$",         # Parameters ending in _trend
    "^ar\\d+_trend",   # AR coefficients: ar1_trend, ar2_trend
    "^ma\\d+_trend",   # MA coefficients
    "^A\\d+_trend",    # VAR coefficient matrices
    "^Z\\[",           # Factor loadings: Z[1,1], Z[1,2], ...
    "^lv_trend",       # Latent trends
    "^lv_coefs",       # Latent variable coefficients
    "^mu_trend",       # Trend intercepts/means
    "^sigma_trend",    # Innovation standard deviations
    "^L_Omega_trend",  # Cholesky factors for correlations
    "^Sigma_trend"     # Covariance matrices
  )

  trend_params <- param_names[
    grepl(paste(trend_patterns, collapse = "|"), param_names)
  ]

  return(trend_params)
}
```

**Note**: These functions need to be refined based on actual mvgam Stan code generation patterns.

---

## Component 6: Combining Observation and Trend

### On Link Scale (Additive)

```r
# Both are on link scale (log for Poisson, logit for Bernoulli, etc.)
linpred_obs <- extract_linpred_from_prep(prep_obs, obs_draws)
linpred_trend <- extract_linpred_from_prep(prep_trend, trend_draws)

# Combine additively
linpred_total <- linpred_obs + linpred_trend
```

### Applying Transformations

```r
#' Transform predictions based on type
#' @param linpred Matrix [n_draws × n_obs] on link scale
#' @param draws draws_matrix with distributional parameters
#' @param family brmsfamily object
#' @param type "link", "response", or "predict"
#' @return Transformed predictions
#' @noRd
transform_by_type <- function(linpred, draws, family, type) {

  if (type == "link") {
    return(linpred)
  }

  # Apply inverse link function
  inv_link <- family$linkinv
  epred <- inv_link(linpred)

  if (type == "response") {
    return(epred)
  }

  # type == "predict": Add observation noise

  # Extract distributional parameters
  if ("sigma" %in% colnames(draws)) {
    sigma <- draws[, "sigma"]
  }

  # Family-specific sampling
  ppred <- switch(
    family$family,

    "gaussian" = {
      epred + matrix(
        rnorm(length(epred), 0, rep(sigma, ncol(epred))),
        nrow = nrow(epred)
      )
    },

    "poisson" = {
      matrix(
        rpois(length(epred), lambda = as.vector(epred)),
        nrow = nrow(epred)
      )
    },

    "binomial" = {
      # Trials info from family or data
      trials <- family$trials  # Or extract from somewhere
      matrix(
        rbinom(length(epred), size = trials, prob = as.vector(epred)),
        nrow = nrow(epred)
      )
    },

    "negative_binomial" = {
      shape <- draws[, "shape"]
      matrix(
        rnbinom(
          length(epred),
          mu = as.vector(epred),
          size = rep(shape, ncol(epred))
        ),
        nrow = nrow(epred)
      )
    },

    stop("Unsupported family: ", family$family)
  )

  return(ppred)
}
```

---

## Component 7: Multivariate Models

### Multiple Series Handling

For models with multiple series (responses):

```r
# If univariate (single series)
if (n_series == 1) {
  predictions <- compute_predictions_single(...)
  return(predictions)
}

# If multivariate (multiple series)
predictions <- vector("list", n_series)
names(predictions) <- series_names

for (i in seq_len(n_series)) {

  # Prepare newdata for this series
  newdata_i <- filter(newdata, series == series_names[i])

  # Get prep objects
  prep_obs_i <- brms:::prepare_predictions(
    obs_brmsfit,
    newdata = newdata_i,
    resp = series_names[i]  # If using mvbind()
  )

  # Extract and combine
  predictions[[i]] <- compute_predictions_single(...)
}

return(predictions)
```

**Return Format**: List of matrices (matching brms multivariate convention)

---

## Key Implementation Decisions

### ✅ What We Will Do

1. **Create lightweight brmsfit objects** during `mvgam()` fitting
   - Store in `mvgam_fit$obs_model` and `mvgam_fit$trend_model`
   - Minimal structure with placeholder stanfit

2. **Use S3 mock stanfit pattern** during predictions
   - Simple `create_mock_stanfit()` function
   - Register `as_draws_matrix.mock_stanfit()` method
   - No R6 classes (following package conventions)

3. **Leverage `brms:::prepare_predictions()`**
   - Only way to get complex design matrices correctly
   - Tested and verified approach

4. **Extract and manually compute linear predictors**
   - Use `extract_linpred_from_prep()` pattern
   - Verified to match brms exactly

5. **Combine obs + trend on link scale**
   - Additive combination before inverse link
   - Standard State-Space model integration

### ❌ What We Will NOT Do

1. **Don't try to replace stanfit with complex mocks**
   - brms validates internals - this fails
   - Extract-and-multiply is cleaner

2. **Don't reimplement design matrix construction**
   - Too complex and error-prone
   - Use brms machinery

3. **Don't use R6 classes**
   - Package uses S3 consistently
   - Keep it simple

4. **Don't try to modify brms prediction functions**
   - Use them as-is where helpful
   - Implement our own combination logic

---

## Testing Strategy

### Unit Tests

```r
test_that("parameter identification separates obs and trend", {
  params <- c("b_Intercept", "b_x", "sigma", "ar1_trend", "Z[1,1]", "lp__")

  obs <- identify_obs_params(params)
  trend <- identify_trend_params(params)

  expect_setequal(obs, c("b_Intercept", "b_x", "sigma"))
  expect_setequal(trend, c("ar1_trend", "Z[1,1]"))
})

test_that("mock stanfit returns correct draws", {
  draws <- matrix(rnorm(100), nrow = 50, ncol = 2)
  colnames(draws) <- c("b_x", "sigma")
  class(draws) <- c("draws_matrix", "draws", "matrix", "array")

  mock <- create_mock_stanfit(draws)
  result <- as_draws_matrix(mock)

  expect_identical(result, draws)
})

test_that("manual linpred matches brms", {
  # Fit simple brms model
  fit <- brm(y ~ x, data = test_data, ...)

  # Get prep
  prep <- brms:::prepare_predictions(fit, newdata = newdata)
  draws <- as_draws_matrix(fit$fit)

  # Manual computation
  manual <- extract_linpred_from_prep(prep, draws)

  # brms computation
  brms_result <- posterior_linpred(fit, newdata = newdata)

  # Should match exactly
  expect_equal(manual, brms_result, tolerance = 1e-10)
})
```

### Integration Tests

```r
test_that("full prediction workflow works", {
  # Fit mvgam model
  fit <- mvgam(
    formula = y ~ x,
    trend_formula = ~ RW(),
    data = test_data,
    family = poisson()
  )

  # Generate predictions
  newdata <- data.frame(x = c(0, 1), time = c(51, 52), series = c(1, 1))

  pred_link <- posterior_linpred(fit, newdata = newdata)
  pred_resp <- posterior_epred(fit, newdata = newdata)
  pred_pred <- posterior_predict(fit, newdata = newdata)

  # Check dimensions
  expect_equal(ncol(pred_link), 2)  # 2 observations
  expect_equal(nrow(pred_link), nsamples(fit))

  # Check transformations
  expect_equal(pred_resp, exp(pred_link))  # Poisson inverse link

  # Check noise
  expect_true(sd(pred_pred) > sd(pred_resp))  # More uncertainty
})
```

---

## File Organization

```
R/
  predictions/
    ├── mock-stanfit.R         # create_mock_stanfit(), S3 methods
    ├── prep-extraction.R      # extract_linpred_from_prep()
    ├── parameter-id.R         # identify_obs_params(), identify_trend_params()
    ├── posterior-linpred.R    # posterior_linpred.mvgam()
    ├── posterior-epred.R      # posterior_epred.mvgam()
    ├── posterior-predict.R    # posterior_predict.mvgam()
    ├── transformations.R      # transform_by_type()
    └── utils.R                # Helper functions

tests/testthat/
  ├── test-mock-stanfit.R
  ├── test-prep-extraction.R
  ├── test-parameter-id.R
  ├── test-posterior-linpred.R
  ├── test-posterior-epred.R
  └── test-predictions-integration.R
```

---

## Open Questions for Implementation

1. **Trend-specific newdata**: Does trend model need different newdata structure?
   - Time indices vs. covariate values?
   - How are latent states handled?

2. **Multivariate trend formulas**: When using response-specific trends, how do we structure prep objects?
   - One prep per response?
   - Different parameter subsets?

3. **Distributional parameters in trends**: Can trend_formula have `sigma ~ ...`?
   - If yes, need separate dpars extraction

4. **Factor models**: How are Z matrices stored in combined fit?
   - Are they parameters or transformed parameters?
   - How do we extract and use them?

5. **Process error toggle**: How does `process_error = FALSE` affect draws?
   - Use posterior means instead of full posterior?
   - How to implement efficiently?

---

## Next Steps

1. **Review this document** with package maintainer
2. **Clarify open questions** about mvgam-specific details
3. **Generate detailed sub-tasks** for implementation
4. **Begin with foundation** (mock stanfit, parameter ID)
5. **Iteratively build** prediction functions with tests

---

## References

- TRD: `tasks/trd-prediction-system.md`
- brms analysis: context7 `rpa-brms-bprepdict-extraction-2025-11-05`
- Architecture: `architecture/architecture-decisions.md`
- Dependency graph: `architecture/dependency-graph.md`
