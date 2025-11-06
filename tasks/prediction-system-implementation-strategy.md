# mvgam Prediction System: Implementation Strategy

**Date**: 2025-11-05
**Status**: Architecture Finalized - Ready for Implementation
**Context**: TRD located at `tasks/trd-prediction-system.md`

---

## Executive Summary

This document outlines the **empirically verified** strategy for implementing the mvgam prediction system. All approaches have been tested against brms 2.22.0 and confirmed to work.

**Key Decision**: Use `brms::prepare_predictions()` to create design matrices, then extract and multiply with our combined parameter draws. No need for complex stanfit mocking.

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

## Component 2: Mock Stanfit and S3 Dispatch Strategy

### The S3 Dispatch Pattern

Instead of trying to make mock_stanfit objects that perfectly mimic real S4 stanfit objects, we use S3 method dispatch on the mock_stanfit class itself.

**Key Architecture Decision**: Create `prepare_predictions.mock_stanfit()` method that:
- Dispatches when the first argument is a mock_stanfit object
- Accepts the brmsfit object (obs_model or trend_model) as an additional argument
- Extracts design matrices and metadata from the brmsfit
- Uses the cached draws from the mock_stanfit for predictions
- Returns the same prep structure as brms prepare_predictions

**Benefits of S3 Dispatch Approach**:
- Full control over prediction workflow without fighting brms internals
- No need to implement complex S4 slot access patterns
- Clean separation between parameter subsetting (mock_stanfit) and metadata (brmsfit)
- Easier to maintain and debug
- Natural extension point for mvgam-specific prediction logic

**Core Components**:
1. `create_mock_stanfit(draws_matrix)` - Creates S3 object holding parameter subset
2. `prepare_predictions.mock_stanfit(object, brmsfit, newdata, ...)` - Custom S3 method
3. Helper functions to extract design matrices using brmsfit metadata

---

## Component 3: Design Matrix Extraction

### Why We Need brms Machinery

brms handles complex design matrix construction that we cannot easily replicate:
- Random effects with proper indexing
- Smooth (GAM) basis functions evaluated at newdata
- Monotonic effects transformations
- Distributional parameters (sigma ~ predictors)
- Factor variable encoding
- GP predictions at new input locations

### Strategy

Our `prepare_predictions.mock_stanfit()` method will leverage brmsfit metadata to build design matrices without requiring a real stanfit object with S4 slots
- Interaction terms

**We must use brms machinery** - attempting to reimplement would be error-prone and fragile.

### The Workflow

High-level prediction workflow using S3 dispatch:

1. **Extract parameter subsets** from combined mvgam fit (observation vs trend parameters)
2. **Create mock_stanfit objects** holding the parameter draws subsets
3. **Call prepare_predictions.mock_stanfit()** method with mock object + brmsfit metadata + newdata
4. **Extract design matrices** from returned prep object
5. **Compute linear predictors** by combining design matrices with parameter draws
6. **Combine observation and trend contributions** additively on link scale
7. **Apply transformations** based on prediction type (link, expectation, or outcome scale)

---

## Component 4: Extracting Linear Predictors from prep

### The prep Object Structure

The prepare_predictions method returns a prep object containing:
- **Distributional parameters** (dpars) for mu, sigma, etc.
- **Design matrices** for fixed effects, random effects, smooths, GPs
- **Parameter mappings** linking design matrix columns to parameter names
- **Metadata** for intercepts, offsets, and special terms

### Linear Predictor Computation

The implementation must:
1. Access design matrices from appropriate prep structure locations
2. Match design matrix columns to parameter names from draws
3. Perform matrix multiplication to compute linear predictor contributions
4. Sum all contributions (intercept + fixed effects + smooths + random effects + etc.)
5. Return matrix with dimensions ndraws × nobs

Structure details to be documented in `tasks/prep_exploration_notes.md` after exploration phase.

---

## Component 5: Parameter Identification

### Observation vs. Trend Parameters

Parameter categorization already implemented in `R/index-mvgam.R`:
- `categorize_mvgam_parameters()` - Comprehensive categorization into observation and trend components
- `extract_obs_parameters()` - Extracts observation parameter names
- `extract_trend_parameters()` - Extracts trend parameter names

Uses systematic naming conventions:
- Observation parameters: Standard brms naming (b_, sd_, r_, sigma, etc.)
- Trend parameters: `_trend` suffix for all trend model parameters
- Factor loadings: Z, Z_raw (bridge parameters between observation and trend)

---

## Component 6: Combining Observation and Trend

### On Link Scale (Additive)

Linear predictors from observation and trend models are combined additively on the link scale (log for Poisson, logit for Bernoulli, etc.):

- Observation linpred represents fixed effects, smooths, random effects from observation formula
- Trend linpred represents State-Space dynamics from trend formula
- Combined linpred = observation + trend (both on same link scale)

### Applying Transformations

Three prediction scales must be supported:

1. **Link scale** (`posterior_linpred`): Return combined linpred as-is
2. **Expectation scale** (`posterior_epred`): Apply inverse link function to get E[Y|...]
3. **Outcome scale** (`posterior_predict`): Sample from observation model using expected values + distributional parameters

Family-specific inverse link functions and sampling methods provided by brms family objects.

---

## Component 7: Multivariate Models

### Multiple Response Handling

Multivariate models with `mvbind()` or multiple `bf()` formulas require special handling:

- **Shared trends**: Single trend applies to all responses - predict trend once, apply to each response
- **Response-specific trends**: Each response has its own trend model - predict separately per response
- **Response-specific parameters**: Extract parameters per response (sigma_count, sigma_biomass, etc.)
- **Design matrices**: May be response-specific for observation model components

Return format should match brms multivariate conventions (list of matrices or combined matrix depending on context).

---

## Key Implementation Decisions

### ✅ Key Decisions

1. **S3 Dispatch Pattern**: Implement `prepare_predictions.mock_stanfit()` to dispatch on mock object
   - Mock stanfit holds parameter draws subset
   - brmsfit provides metadata (formula, family, basis functions)
   - Full control over design matrix extraction without S4 complexity

2. **Lightweight brmsfit Storage**: Store obs_model and trend_model in mvgam object during fitting
   - Minimal brmsfit structure with placeholder fit slot
   - Reused during prediction with mock stanfits

3. **Design Matrix Extraction**: Leverage brms machinery for complex features
   - Smooths, random effects, GPs, monotonic effects
   - Verified accuracy with brms outputs

4. **Parameter Categorization**: Use existing `extract_obs_parameters()` and `extract_trend_parameters()`
   - Systematic `_trend` suffix convention
   - Handles factor loadings as bridge parameters

5. **Additive Combination**: Combine observation and trend on link scale before transformations

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
  prep <- brms::prepare_predictions(fit, newdata = newdata)
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
