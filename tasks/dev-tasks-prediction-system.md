# Development Tasks: Prediction System

## Relevant Files

### R Package Files
- `R/index-mvgam.R` - Contains `variables.mvgam()` and `categorize_mvgam_parameters()`
- `R/mock-stanfit.R` - Mock stanfit infrastructure for parameter subsetting
- `R/predictions.R` - Core prediction infrastructure (NEW)
- `R/as.data.frame.mvgam.R` - Posterior extraction methods that use aliases
- `R/mcmc_plot.mvgam.R` - MCMC plotting that uses categorized parameters
- `R/tidier_methods.R` - Tidy methods that use categorized parameters
- `R/pairs.mvgam.R` - Pairs plotting that uses categorized parameters
- `R/residual_cor.R` - Residual correlation analysis using categorized parameters

### Test Files
- `tests/testthat/test-index-mvgam.R` - Tests for parameter indexing and categorization
- `tests/testthat/test-predictions-integration.R` - Integration tests for prediction workflow (NEW)
- `tests/testthat/test-as.data.frame.mvgam.R` - Tests for posterior extraction with aliases
- `tests/testthat/test-mcmc_plot.mvgam.R` - Tests for plotting with renamed parameters

### Context Files
- `/architecture/architecture-decisions.md` - Lazy categorization architecture
- `/tasks/trd-prediction-system.md` - Prediction system requirements
- `/tasks/prediction-system-implementation-strategy.md` - Implementation strategy and brms integration approach

### Task Files
- `tasks/fit_and_save_models.R` - Script to generate test model fixtures
- `tasks/test_parameter_extraction.R` - Comprehensive tests for parameter extraction helpers across 6+ models
- `tasks/explore_prepare_predictions.R` - Exploration of brms prepare_predictions() (PENDING)
- `tasks/test_integration_local.R` - Local integration testing before final tests (PENDING)
- `tasks/prep_exploration_notes.md` - Documentation of prep object structure findings (PENDING)
- `tasks/fixtures/*.rds` - Fitted model fixtures for development (gitignored, 9 models)

### Notes
- Test model patterns available in `tests/local/test-models-single.R`
- Development fixtures stored in `tasks/fixtures/` (not version controlled)
- Use `devtools::load_all()` before testing
- No test errors or warnings allowed
- Integration-test-first approach: explore → build → test locally → final tests

---

## Tasks

- [ ] 1.0 Parameter Renaming Infrastructure for User-Facing Methods (Skip for now, need to think about how to implement this efficiently)

---

- [ ] **2.0 Foundation: Parameter Extraction and Prediction Infrastructure**

  - [ ] **2.1 Setup: Test Model Fixtures** (Critical Path)
    - [x] 2.1.1 Create `tasks/fit_and_save_models.R` script that sources `tests/local/setup_tests_local.R`, fits all 9 models from `tests/local/test-models-single.R` (fit1 through fit9), and saves each as `tasks/fixtures/fit{1-9}.rds`. Include timing information and model summary prints for verification.
    - [x] 2.1.2 Run `fit_and_save_models.R` to generate all fixture files in `tasks/fixtures/`. Verify each .rds loads correctly with `readRDS()` and test basic methods (`print()`, `variables()`, `summary()`) work. Document total fitting time and model sizes.
    - [x] 2.1.3 Create `.gitignore` entry for `tasks/fixtures/*.rds` to exclude fitted models from version control (they're large and user-specific). Add `tasks/fixtures/README.md` explaining how to regenerate fixtures using the fit script.

  - [x] **2.2 Parameter Categorization Enhancement**
    - [x] 2.2.1 Rename `.categorize_parameters()` to `categorize_mvgam_parameters()` in `R/index-mvgam.R`. Update all internal calls in the same file. Verify `devtools::load_all()` succeeds with no errors.
    - [x] 2.2.2 Search for all uses of `.categorize_parameters()` across the codebase using `Grep`. Update each call to use new name `categorize_mvgam_parameters()`. Run `devtools::load_all()` after each file update to catch errors early. (None found)
    - [x] 2.2.3 Add DRY helper function `extract_obs_parameters(mvgam_fit)` in `R/index-mvgam.R` that calls internal `extract_parameters_by_type(mvgam_fit, type = "observation")` and returns combined character vector of all observation parameter names. Include roxygen `@noRd` documentation.
    - [x] 2.2.4 Add DRY helper function `extract_trend_parameters(mvgam_fit)` in `R/index-mvgam.R` that calls internal `extract_parameters_by_type(mvgam_fit, type = "trend")` and returns combined character vector of all trend parameter names (excluding computed trend states, including bridge parameters Z and Z_raw). Include roxygen `@noRd` documentation.
    - [x] 2.2.5 Write comprehensive tests in `tasks/test_parameter_extraction.R` using pre-saved fixtures. Test 6 models, verify no duplicates, no overlap between obs/trend, complete parameter coverage, proper exclusion of computed states, and handling of bridge parameters (Z, Z_raw without _trend suffix).
    - [x] 2.2.6 Fix categorization logic to handle all brms parameter patterns: multivariate intercepts, basis splines (bs_), standardized smooths (zs_), GP parameters (sdgp_, lscale_, zgp_), standardized RE (z_), factor loadings (Z, Z_raw), and monotonic effects (bsp_, simo_).

  - [ ] **2.3 brms prepare_predictions() Exploration**
    - [ ] 2.3.1 Create `tasks/explore_prepare_predictions.R` script. Load `tasks/fixtures/fit1.rds`, extract observation parameters using `extract_obs_parameters()`, create parameter subset with `posterior::as_draws_matrix()`, and create mock stanfit using `create_mock_stanfit()`. Document what the draws subset looks like.
    - [ ] 2.3.2 In exploration script, replace `fit1$obs_model$fit` with mock stanfit, create simple newdata with required variables, and call `brms::prepare_predictions(fit1$obs_model, newdata = newdata)`. Print the entire structure of the returned object using `str(prep, max.level = 3)`. Explore what's available.
    - [ ] 2.3.3 Investigate the `prep` object structure. What are the top-level components? What's nested under `prep$dpars`? Are there design matrices? If so, where and what do they look like? Document findings as comments in the exploration script without assuming we know the answer.
    - [ ] 2.3.4 Experiment with extracting components from `prep` and matching them to parameter names from the draws. Try manual matrix operations with any matrices found. Document what works and what doesn't. Save any successful patterns as comments.
    - [ ] 2.3.5 Repeat exploration for trend model: extract trend parameters, create mock for `fit1$trend_model$fit`, call `brms::prepare_predictions()`, and explore the returned structure. Note similarities and differences from observation model prep.
    - [ ] 2.3.6 Test exploration workflow with fit2 (multivariate) and fit3 (smooths + VAR) fixtures from `tasks/fixtures/`. Document how the `prep` object structure changes for different model types. What's different for multivariate models? How do smooths appear?
    - [ ] 2.3.7 Create `tasks/prep_exploration_notes.md` summarizing findings: What does `brms::prepare_predictions()` return? How are design matrices structured? How do we map parameters to matrix components? What patterns work for computing predictions? This becomes our reference for implementation.

  - [ ] **2.4 Core Prediction Infrastructure (Foundation Functions)**
    - [ ] 2.4.1 Create `R/predictions.R` file with helper function `prepare_obs_predictions(mvgam_fit, newdata, re_formula = NULL, allow_new_levels = FALSE, sample_new_levels = "uncertainty")`. Extract obs params, create mock stanfit, replace in `obs_model$fit`, call `brms::prepare_predictions()`, and return prep object. Include roxygen `@noRd` docs. Implementation based on exploration findings.
    - [ ] 2.4.2 Add helper function `prepare_trend_predictions(mvgam_fit, newdata)` in `R/predictions.R`. Extract trend params, create mock stanfit, replace in `trend_model$fit`, call `brms::prepare_predictions()`, and return prep object. Handle NULL trend_model case (pure brms models). Implementation based on exploration findings.
    - [ ] 2.4.3 Add `extract_linpred_from_prep(prep, draws, dpar = "mu")` function in `R/predictions.R`. Implement linear predictor computation based on what we learned in exploration. Return matrix with appropriate dimensions. Document the approach used.
    - [ ] 2.4.4 Add input validation function `validate_newdata_for_predictions(mvgam_fit, newdata)` in `R/predictions.R`. Check that newdata is data.frame, check for required variables from both formulas (use `all.vars()`), check for time/series variables if trend model present. Return validated newdata or stop with informative error via `insight::format_error()`.

  - [ ] **2.5 Local Integration Testing (in tasks/)**
    - [ ] 2.5.1 Create `tasks/test_integration_local.R` script. Load `tasks/fixtures/fit1.rds`, create newdata, and test the complete workflow: `prepare_obs_predictions()` → `prepare_trend_predictions()` → `extract_linpred_from_prep()` for both. Document any issues encountered.
    - [ ] 2.5.2 In local integration script, combine obs and trend linear predictors additively. Apply inverse link function and verify predictions are reasonable (positive values for Poisson, proper range for probabilities). Print summary statistics of predictions.
    - [ ] 2.5.3 Test workflow with fit2 (multivariate) in local script. Test with fit3 (smooths + VAR). Document any model-specific adjustments needed. Update helper functions in `R/predictions.R` if issues found.
    - [ ] 2.5.4 Test edge cases in local script: newdata with different number of rows, missing series levels, predictions at future time points. Document what works and what needs additional validation.

  - [ ] **2.6 Final Integration Tests (in tests/testthat/)**
    - [ ] 2.6.1 Create `tests/testthat/test-predictions-integration.R` file. Write test "integration: basic prediction workflow" that fits a simple inline model (y ~ x, trend_formula = ~ RW()), creates newdata, and calls `prepare_obs_predictions()` and `prepare_trend_predictions()` without errors.
    - [ ] 2.6.2 In integration test, extract linear predictors from both obs and trend prep objects using `extract_linpred_from_prep()`. Verify dimensions are correct. Verify no NA values in output. Verify combining them produces sensible predictions.
    - [ ] 2.6.3 Add integration test for multivariate model. Fit inline multivariate model with shared trend, test prediction workflow. Verify response-specific handling works correctly.
    - [ ] 2.6.4 Add integration test for edge case: pure brms model (no trend_formula). Fit inline model without trends, verify `prepare_trend_predictions()` handles gracefully, and predictions work with only observation component.

  - [ ] **2.7 Documentation and Validation**
    - [ ] 2.7.1 Add roxygen2 documentation to `categorize_mvgam_parameters()` explaining return structure and usage. Update `@examples` to show how to extract specific parameter groups. Run `devtools::document()` to generate .Rd file.
    - [ ] 2.7.2 Add comprehensive input validation to all new functions in `R/predictions.R` using `checkmate::assert_*()`. Validate mvgam_fit is class "mvgam", newdata is data.frame, prep objects have expected structure based on exploration findings.
    - [ ] 2.7.3 Run full test suite with `devtools::test()`. Verify all tests pass with zero warnings. Fix any failures or warnings before proceeding. Document any unexpected behavior in test comments.

---

## High-Level Parent Tasks (For Future Development)

- **3.0 Core Prediction Functions**
  - Implement `posterior_linpred.mvgam()` (link scale predictions)
  - Implement `posterior_epred.mvgam()` (expectation scale predictions)
  - Implement `posterior_predict.mvgam()` (posterior predictive samples)
  - Handle process_error toggle for trend uncertainty
  - Support multivariate models (shared and response-specific trends)

- **4.0 Convenience Wrappers and Specialized Functions**
  - Implement `predict.mvgam()` wrapper with summary statistics
  - Implement `fitted.mvgam()` for in-sample fitted values
  - Implement `posterior_smooths.mvgam()` for smooth term extraction
  - Implement `posterior_average.mvgam()` for categorical models

- **5.0 marginaleffects Integration**
  - Implement required S3 methods (`get_predict.mvgam()`, etc.)
  - Validate integration with core marginaleffects functions
  - Test series-specific marginal effects
  - Ensure process_error passes through correctly

- **6.0 conditional_effects Enhancement**
  - Extend `conditional_effects.mvgam()` for State-Space models
  - Support series-specific conditional effects
  - Maintain brms delegation pattern and compatibility
  - Integrate with existing plotting methods

- **7.0 Testing, Documentation, and Validation**
  - Complete unit tests for all prediction functions
  - Write integration tests for end-to-end workflows
  - Create comprehensive roxygen2 documentation
