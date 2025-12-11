# Development Tasks: Core Prediction Functions (3.0)

## Overview

This task list covers implementation of the user-facing prediction functions that build on the completed prediction infrastructure (see `tasks/completed/dev-tasks-prediction-infrastructure.md`).

**Foundation Already Complete:**
- `extract_component_linpred(mvgam_fit, newdata, component, resp, ndraws, re_formula, allow_new_levels, sample_new_levels)` - unified linear predictor extraction
- `extract_linpred_from_prep()` - handles all brms formula features (smooths, REs, GPs, monotonics, nonlinear, offsets)
- `prepare_predictions.mock_stanfit()` - S3 method for design matrix extraction
- 100% success rate on validation tests (17/17 in `validate_extraction_vs_brms.R`)

**Target Functions:**
1. `posterior_linpred.mvgam()` - link scale predictions
2. `posterior_epred.mvgam()` - expected values (response scale)
3. `posterior_predict.mvgam()` - posterior predictive samples
4. `predict.mvgam()` - convenience wrapper with summaries
5. `fitted.mvgam()` - in-sample fitted values

---

## Relevant Files

### R Package Files
- `R/posterior_linpred.R` - `posterior_linpred.mvgam()` S3 method and helpers
- `R/predictions.R` - Core prediction infrastructure (extend with S3 methods)
- `R/index-mvgam.R` - Parameter extraction helpers
- `R/mock-stanfit.R` - Mock stanfit and `prepare_predictions.mock_stanfit()`

### Test Files
- `tests/testthat/test-predict.R` - Existing prediction validation tests
- `tests/testthat/test-predictions-core.R` - New tests for S3 methods (TO CREATE)

### Validation Files
- `tasks/validate_extraction_vs_brms.R` - Numerical validation framework (extend)
- `tasks/validate_prediction_functions.R` - Full prediction function validation (TO CREATE)

### Reference Files (brms Source Analysis)
- `tasks/brms_posterior_predict_internals.R` - **CRITICAL REFERENCE**: Complete brms `posterior_predict` source code showing:
  - Family-specific dispatch pattern: `pp_fun <- paste0("posterior_predict_", object$family$fun)`
  - dpars extraction: `get_dpar(prep, "mu", i = i)`, `get_dpar(prep, "sigma", i = i)`
  - Truncation handling via `rcontinuous()` and `rdiscrete()` helpers
  - Multivariate handling in `posterior_predict.mvbrmsprep()`
  - All family sampling implementations (gaussian, poisson, binomial, negbinomial, gamma, etc.)

### Context Files
- `tasks/completed/dev-tasks-prediction-infrastructure.md` - Completed foundation tasks
- `tasks/trd-prediction-system.md` - Full requirements document
- `tasks/prediction-system-implementation-strategy.md` - Architecture decisions

---

## Agent Usage Protocol

**MANDATORY for all tasks:**

1. **pathfinder agent**: Use BEFORE editing any file to:
   - Find exact function locations and line numbers
   - Map dependencies that might need updates
   - Assess edit safety

2. **r-package-analyzer agent**: Use to study brms internals when needed (supplement `brms_posterior_predict_internals.R`):
   - Task 2.1: Study `posterior_epred.brmsfit()` for epred-specific patterns
   - Verify inverse link handling matches our implementation

3. **code-reviewer agent**: Use AFTER completing each numbered task to:
   - Verify fail-fast validation patterns
   - Check for code duplication
   - Ensure tidyverse style compliance
   - Validate test coverage

---

## Tasks

### 1.0 Core Combination Logic and `posterior_linpred.mvgam()` ✓

Build the foundation for combining observation and trend linear predictors, then expose via brms-compatible S3 method.

**Completed**: Commit 71db40be

- [x] **1.1 Create `get_combined_linpred()` helper function**
  - Created in new file `R/posterior_linpred.R`
  - Extracts obs linpred via `extract_component_linpred(..., component = "obs")`
  - Checks if trend model exists and has formula
  - If trend exists: extracts trend linpred and combines additively
  - Delegates validation to `extract_component_linpred()`

- [x] **1.2 Implement `process_error` logic in combination**
  - When `process_error = FALSE`:
    - Computes column means of trend linpred matrix
    - Broadcasts to match obs linpred dimensions
    - Fixes trend at posterior mean to reduce uncertainty
  - Parameter validation via `checkmate::assert_logical()`

- [x] **1.3 Implement `posterior_linpred.mvgam()` S3 method**
  - Created in `R/posterior_linpred.R`
  - Imports `posterior_linpred` generic from brms
  - Handles `newdata = NULL` → uses training data from `object$data`
  - Delegates all validation to `extract_component_linpred()`
  - Code reviewed and approved

- [x] **1.4 Add validation tests for `posterior_linpred.mvgam()`**
  - Extended `tasks/validate_extraction_vs_brms.R` with `run_linpred_validation()` helper
  - Tests added (all passing):
    - Dimensions match brms [ndraws × nobs]
    - `ndraws` subsetting returns correct number of draws
    - `process_error = FALSE` reduces variance vs TRUE
    - Obs-formula models: 5 tests (intercept-only, fixed, RE, smooth, GP)
    - Trend-formula models: 4 tests (fixed, RE, smooth, GP)

- [x] **1.5 Code review for Task 1.0**
  - Code reviewer approved implementation
  - Proper delegation to `extract_component_linpred()` (no duplication)
  - Fail-fast validation patterns implemented

---

### 2.0 Implement `posterior_epred.mvgam()`

Expected values on response scale by applying inverse link function.

- [ ] **2.1 Create `apply_family_linkinv()` helper function**
  - Use **pathfinder agent** to find insertion point
  - Reference `brms_posterior_predict_internals.R` for family object structure
  - Extract inverse link function from family object: `family$linkinv`
  - Handle edge cases: custom families, multivariate families
  - Apply element-wise to linpred matrix
  - Add `@noRd` roxygen documentation

- [ ] **2.2 Implement `posterior_epred.mvgam()` S3 method**
  - Function signature:
    ```r
    posterior_epred.mvgam <- function(object, newdata = NULL,
                                      process_error = TRUE,
                                      ndraws = NULL, draw_ids = NULL,
                                      re_formula = NULL,
                                      allow_new_levels = FALSE,
                                      sample_new_levels = "uncertainty",
                                      resp = NULL, ...)
    ```
  - Implementation: Call `posterior_linpred(..., transform = FALSE)` then apply inverse link
  - Handle distributional parameters if needed (sigma for gaussian, etc.)
  - Add roxygen2 documentation with `@export`

- [ ] **2.3 Add validation tests for `posterior_epred.mvgam()`**
  - Extend `tasks/validate_prediction_functions.R` with epred tests
  - Test: Poisson models return exp(linpred)
  - Test: Bernoulli models return plogis(linpred)
  - Test: Gaussian models return identity(linpred)
  - Test: Values are on correct scale (counts >= 0, probabilities in [0,1])

- [ ] **2.4 Code review for Task 2.0**
  - Use **code-reviewer agent** on all changes
  - Verify inverse link implementation matches brms behavior

---

### 3.0 Implement `posterior_predict.mvgam()`

Posterior predictive samples with observation-level noise.

- [ ] **3.1 Create family-specific sampling infrastructure**
  - Reference `brms_posterior_predict_internals.R` lines 312-968 for all family implementations
  - Create `sample_from_family()` helper that dispatches based on `family$family`:
    - `"gaussian"`: `rnorm(n, mean = epred, sd = sigma)` (line 312-322)
    - `"poisson"`: `rpois(n, lambda = epred)` (line 512-520)
    - `"binomial"`: `rbinom(n, size = trials, prob = epred)` (line 486-494)
    - `"negbinomial"`: `rnbinom(n, mu = epred, size = shape)` (line 522-533)
    - `"bernoulli"`: `rbinom(n, size = 1, prob = epred)` (line 507-510)
    - `"Gamma"`: `rgamma(n, shape = shape, scale = epred/shape)` (line 590-599)
    - Additional families as needed
  - Use **pathfinder agent** to find insertion point

- [ ] **3.2 Handle distributional parameters extraction**
  - For families requiring extra parameters (sigma, phi, shape):
    - Reference brms patterns: `get_dpar(prep, "sigma", i = i)`
    - Extract from combined fit using appropriate patterns
    - Match dimensions to epred matrix
  - Implement `extract_distributional_params()` helper
  - Test with Gaussian family (requires sigma)
  - Test with negative binomial (requires phi/shape)

- [ ] **3.3 Implement `posterior_predict.mvgam()` S3 method**
  - Function signature:
    ```r
    posterior_predict.mvgam <- function(object, newdata = NULL,
                                        process_error = TRUE,
                                        ndraws = NULL, draw_ids = NULL,
                                        re_formula = NULL,
                                        allow_new_levels = FALSE,
                                        sample_new_levels = "uncertainty",
                                        resp = NULL, ...)
    ```
  - Implementation:
    1. Call `posterior_epred()` to get expected values
    2. Extract distributional parameters
    3. Sample from family using `sample_from_family()`
  - Ensure reproducibility with `set.seed()` documentation
  - Add roxygen2 documentation with `@export`

- [ ] **3.4 Handle truncation (optional, lower priority)**
  - Reference `rcontinuous()` (line 978-1003) and `rdiscrete()` (line 1015-1035)
  - Implement truncation handling if model has `trunc()` terms
  - Can be deferred if no immediate need

- [ ] **3.5 Add validation tests for `posterior_predict.mvgam()`**
  - Test: Poisson predictions are non-negative integers
  - Test: Bernoulli predictions are 0 or 1
  - Test: Gaussian predictions have correct mean and sd
  - Test: Predictions have MORE variance than epred (includes obs noise)
  - Test: Distribution of predictions matches family

- [ ] **3.6 Code review for Task 3.0**
  - Use **code-reviewer agent** on all changes
  - Verify family sampling matches brms behavior

---

### 4.0 Implement Convenience Wrappers

User-friendly interfaces with automatic summarization.

- [ ] **4.1 Implement `predict.mvgam()` S3 method**
  - Reference `brms_posterior_predict_internals.R` lines 239-255 for `predict.brmsfit()`
  - Function signature:
    ```r
    predict.mvgam <- function(object, newdata = NULL,
                              type = c("response", "link", "prediction"),
                              process_error = TRUE,
                              ndraws = NULL,
                              summary = TRUE,
                              robust = FALSE,
                              probs = c(0.025, 0.975),
                              ...)
    ```
  - Dispatch to appropriate posterior_* function based on `type`
  - When `summary = TRUE`: return data.frame with Estimate, Est.Error, Q2.5, Q97.5
  - When `summary = FALSE`: return raw matrix
  - Add roxygen2 documentation with `@export`

- [ ] **4.2 Implement `fitted.mvgam()` S3 method**
  - Function signature:
    ```r
    fitted.mvgam <- function(object,
                             scale = c("response", "linear"),
                             process_error = TRUE,
                             summary = TRUE,
                             ...)
    ```
  - Equivalent to `predict(object, newdata = NULL, ...)`
  - Uses training data stored in object
  - Add roxygen2 documentation with `@export`

- [ ] **4.3 Create summary computation helper**
  - Create `summarize_predictions()` internal function
  - Reference brms `posterior_summary()` for output format
  - Compute: Estimate (mean or median), Est.Error (sd or mad), quantiles
  - Support `robust = TRUE` for median/mad instead of mean/sd
  - Return tidy data.frame with observation index

- [ ] **4.4 Add tests for convenience wrappers**
  - Test: `predict()` returns data.frame when summary=TRUE
  - Test: `predict()` returns matrix when summary=FALSE
  - Test: `fitted()` matches `predict(newdata=NULL)`
  - Test: quantile columns match requested probs
  - Test: `robust = TRUE` uses median instead of mean

- [ ] **4.5 Code review for Task 4.0**
  - Use **code-reviewer agent** on all changes

---

### 5.0 Multivariate Model Support

Ensure all prediction functions work correctly with multivariate responses.

- [ ] **5.1 Test multivariate predictions with existing validation models**
  - Use fit2 (mvbind) and fit4 (bf+bf) from fixtures
  - Verify `posterior_linpred()` returns correct structure for each response
  - Test `resp` argument filtering
  - Reference `posterior_predict.mvbrmsprep()` (lines 105-116) for multivariate handling

- [ ] **5.2 Handle shared vs response-specific trends**
  - Shared trends: same trend contribution for all responses
  - Response-specific: different trend per response (list trend_formula)
  - Verify combination logic handles both cases

- [ ] **5.3 Add multivariate validation tests**
  - Extend validation framework with multivariate test cases
  - Compare against brms multivariate predictions where applicable
  - Test: each response returns correct dimensions
  - Test: `resp = NULL` returns all responses (3D array for posterior_predict)

- [ ] **5.4 Code review for Task 5.0**
  - Use **code-reviewer agent** on multivariate handling

---

### 6.0 Integration Testing and Documentation

Final validation and documentation.

- [ ] **6.1 Create comprehensive integration test file**
  - Create `tests/testthat/test-predictions-core.R`
  - Move validation tests from tasks/ to testthat
  - Cover: all prediction functions, all model types, edge cases

- [ ] **6.2 Run full validation suite**
  - Execute `tasks/validate_prediction_functions.R`
  - Verify all tests pass with high correlation (>= 0.99) against brms
  - Document any discrepancies

- [ ] **6.3 Update NAMESPACE exports**
  - Run `devtools::document()` to update NAMESPACE
  - Verify all S3 methods properly exported:
    - `posterior_linpred.mvgam`
    - `posterior_epred.mvgam`
    - `posterior_predict.mvgam`
    - `predict.mvgam`
    - `fitted.mvgam`
  - Check for any namespace conflicts

- [ ] **6.4 Run full package test suite**
  - Execute `devtools::test()`
  - Verify zero errors and zero warnings
  - Fix any failures before completion

- [ ] **6.5 Final code review**
  - Use **code-reviewer agent** on complete prediction system
  - Review: consistency, documentation completeness, test coverage

---

## Success Criteria

1. All prediction functions return correct dimensions [ndraws × nobs]
2. `process_error` toggle demonstrably affects uncertainty
3. Pure brms models (no trend) predictions match brms exactly
4. Models with trends: obs + trend combination validated
5. Multivariate models return correct per-response predictions
6. All validation tests pass (target: 100% success rate, cor >= 0.99)
7. Full test suite passes with zero errors/warnings
8. All functions have complete roxygen2 documentation

---

## Notes

- **DO NOT** skip code-reviewer agent usage - catch issues early
- **DO NOT** implement features not specified in TRD
- **DO** reference `brms_posterior_predict_internals.R` for family-specific implementations
- **DO** use pathfinder before any edits to understand dependencies
- **DO** validate against brms at every step
- **DO** keep sub-tasks completable in ≤15 minutes
