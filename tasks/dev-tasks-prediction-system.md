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
- `tasks/explore_prepare_predictions.R` - Exploration of prepare_predictions.mock_stanfit() across fit1, fit2, fit3
- `tasks/prep_structure_output.txt` - Full exploration output documenting prep object structure (gitignored)
- `tasks/test_integration_local.R` - Local integration testing before final tests (PENDING)
- `tasks/fixtures/*.rds` - Fitted model fixtures for development (gitignored, 9 models)

### Notes
- **CRITICAL**: ALWAYS use `devtools::load_all()` at the start of ALL exploration and test scripts (NOT `library(mvgam)`)
- **Architecture Decision**: Use S3 dispatch pattern with `prepare_predictions.mock_stanfit()` method
  - Mock stanfit holds parameter draws subset
  - S3 method receives mock object + brmsfit metadata + newdata
  - Avoids need to implement S4 stanfit slots
  - Provides full control over prediction workflow
- **brms Pattern Discovery** (via r-package-analyzer agent, 2025-01-06):
  - brms ALWAYS adds dummy response variables before calling standata() or prepare_predictions()
  - Pattern: `validate_newdata()` → `add_dummy_responses()` in `R/predict.R` (lines ~1850-2010)
  - Dummy values required for formula evaluation and data structure consistency
  - Values never used in prediction computations (can be 0 for all families)
  - Our implementation follows this exact pattern for univariate and multivariate models
- **prep Object Structure** (from exploration, 2025-01-06):
  - Univariate: Single design matrices (X, Xc, Zs, etc.)
  - Multivariate: Response-specific naming (`X_count`, `X_biomass`, `Y_count`, `Y_biomass`)
  - Smooths: Basis matrices `Zs_<response>_<term>_<smooth>` plus `Xs_<response>` random effects
  - Full output documented in `tasks/prep_structure_output.txt`
- Test model patterns available in `tests/local/test-models-single.R`
- Development fixtures stored in `tasks/fixtures/` (not version controlled)
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

  - [x] **2.3 prepare_predictions.mock_stanfit() Implementation**
    - [x] 2.3.1 Create `tasks/explore_prepare_predictions.R` script. **START WITH `devtools::load_all()`** (NOT library(mvgam)). Load multiple models from `tasks/fixtures/` (fit1, fit2, fit3), extract observation and trend parameters using `extract_obs_parameters()` and `extract_trend_parameters()`, create parameter subsets with `posterior::subset_draws()`, and create mock stanfits using `create_mock_stanfit()`. Discovered that calling `brms::prepare_predictions()` on brmsfit with mock_stanfit fails because brms tries to access S4 slots. **Solution: Create S3 method `prepare_predictions.mock_stanfit()` that dispatches on the mock object**.
    - [x] 2.3.2 Implement `prepare_predictions.mock_stanfit(object, brmsfit, newdata, ...)` in `R/mock-stanfit.R`. **IMPLEMENTATION COMPLETE**: Uses `brms::standata(brmsfit, newdata, internal=TRUE)`. Adds dummy response variables to newdata before calling standata (following brms internal pattern in `validate_newdata()` → `add_dummy_responses()`). Handles both univariate and multivariate formulas. Extracts draws from mock using `object$draws_cache`. Returns minimal prep with sdata, draws, formula, family, newdata, nobs. Method exported via roxygen `@export` and NAMESPACE updated via `devtools::document()`.
    - [x] 2.3.3 Test S3 method with exploration script. **VERIFIED**: Method dispatches correctly for all three model types. Returns proper brmsprep objects with class `c("brmsprep", "mvgam_prep")`. Full exploration output saved to `tasks/prep_structure_output.txt`.
    - [x] 2.3.4 Test with fit2 (multivariate) and fit3 (smooths + VAR). **COMPLETE**: All model types tested successfully. **Key findings**: (1) Multivariate uses response-specific naming (`X_count`, `X_biomass`, `Y_count`, `Y_biomass`), (2) Smooth terms appear as `Zs_<response>_<term>_<smooth>` basis matrices and `Xs_<response>` random effect designs, (3) prep$sdata contains all design matrices needed for linear predictor computation, (4) Univariate uses single X/Y, multivariate splits by response.
    - [ ] 2.3.5 Implement helper function `extract_linpred_from_prep(prep, resp = NULL)` in `R/predictions.R`. **Implementation Strategy**: Pure R, fully vectorized (no loops over draws). Follow brms pattern: matrix ops via BLAS for all draws simultaneously. Break into reviewable chunks.
      - [x] 2.3.5.1 **Core fixed effects for univariate models**: Create `R/predictions.R` with `extract_linpred_from_prep()`. Implement intercept extraction and broadcasting: `matrix(b_Intercept, nrow=ndraws, ncol=nobs)`. Implement fixed effects: `b %*% t(X)` where X excludes intercept column. Handle edge cases: no intercept, intercept-only models. Return `[ndraws × nobs]` matrix. **COMPLETE - CODE REVIEWER APPROVED**.
      - [x] 2.3.5.2 **Add input validation and error handling**: Add `checkmate::assert_class(prep, "brmsprep")`. Check dimension mismatches between X and b parameters. Add informative errors via `insight::format_error()`. Add roxygen2 `@noRd` documentation. **COMPLETE - CODE REVIEWER APPROVED**.
      - [x] 2.3.5.3 **Multivariate formula support**: Detect multivariate: `brms::is.mvbrmsformula(prep$formula)`. Extract design matrices by response: `X_count`, `X_biomass` from prep$sdata. Extract parameters by response: `b_count_*`, `b_biomass_*` patterns. Loop over responses (NOT draws) - each response fully vectorized via `b_resp %*% t(X_resp)`. Return list of matrices OR single matrix if `resp` specified. **COMPLETE**: Implemented and tested with fit2 (mvbind) and fit4 (bf + bf). Also fixed critical bug in `get_safe_dummy_value()` - changed from `lb > 0` to `lb >= 0` because Gamma family has ybounds=[0, Inf] but requires values strictly > 0 for validation. Used brms's three-tier logic: integer families (type=="int") get 1L, positive continuous (lb >= 0) get 1, unconstrained get 0.
      - [ ] 2.3.5.4 **Special terms support** (smooths, random effects, GPs): Extend `extract_linpred_from_prep()` to handle all brms special terms beyond fixed effects. Break into exploratory and implementation phases.
        - [x] 2.3.5.4.1 **Exploration**: Create `tasks/explore_special_terms.R` using `devtools::load_all()`. Load fit3 (smooths + VAR), fit6 (random effects), and fit8 (GPs). Generate prep objects and examine `prep$sdata` structure. Document naming conventions and available matrices. Add verification planning with TODOs for implementation phases. **Output saved to `tasks/special_terms_exploration_output.txt`.**
        - [ ] 2.3.5.4.2 **Smooth terms implementation**: Based on exploration findings (see `special_terms_exploration_output.txt` VERIFICATION 1), implement smooth term support. **Key details from exploration**: Smooth basis matrices are `Zs_<response>_<term>_<smooth>` (6×8 dimensions for fit3), parameters are `zs_<response>_<term>_<smooth>` (standardized form with 8 coefficients per smooth). **Implementation requirements**: (1) Extract all Zs_* matrices from prep$sdata, (2) Match to corresponding zs_* parameters in prep$draws, (3) Test computation formula: `zs %*% t(Zs)` to produce [ndraws × nobs] contributions, (4) Sum contributions from multiple smooths per response, (5) Verify dimensions: Zs should be [nobs × nbasis] and zs should be [ndraws × nbasis]. Add to both `extract_linpred_univariate()` and `extract_linpred_multivariate()`.
        - [ ] 2.3.5.4.3 **Random effects implementation**: Add random effects support. Identify RE design matrices (Z_* patterns) and grouping indices (J_* patterns). Extract RE parameters (r_* patterns). Implement vectorized computation accounting for grouping structure. Handle varying intercepts and varying slopes. Support nested and crossed random effects as brms does.
        - [ ] 2.3.5.4.4 **GP effects implementation** (if needed): Check if any test models use GP terms. If present, identify GP matrices in prep$sdata, extract GP parameters, and implement vectorized computation. If not present in current test suite, document as future work.
        - [ ] 2.3.5.4.5 **brms Integration Analysis & Testing**: **FIRST**, use r-package-analyzer agent to investigate brms source code (see CRITICAL QUESTIONS in `special_terms_exploration_output.txt`). Examine `R/posterior_linpred.R` to understand: (1) Exact computation formulas for smooth/RE/GP contributions, (2) How terms are combined (intercept + fixed + smooths + RE + GP), (3) Any brms-specific optimizations. **THEN** create `tasks/test_special_terms.R` comparing our implementation to `brms::posterior_linpred()` output. Verify: (1) Smooth contributions match brms, (2) RE contributions match brms, (3) GP contributions match brms, (4) Combined linear predictor matches brms exactly. Document any discrepancies and required fixes.
        - [ ] 2.3.5.4.6 **Code review and integration**: Submit implementation to code-reviewer. Address any issues. Update `tasks/test_extract_linpred_2351.R` to include special terms tests. Ensure all tests pass before proceeding to next task.

  - [ ] **2.4 Core Prediction Infrastructure (Foundation Functions)**
    - [ ] 2.4.1 Create `R/predictions.R` file with helper function `prepare_obs_predictions(mvgam_fit, newdata, re_formula = NULL, allow_new_levels = FALSE, sample_new_levels = "uncertainty")`. Extract obs params with `extract_obs_parameters()`, create parameter subset with `posterior::subset_draws()`, create mock stanfit, call `prepare_predictions(mock_stanfit, brmsfit = mvgam_fit$obs_model, newdata = newdata, ...)` using our S3 method. Return prep object. Include roxygen `@noRd` docs.
    - [ ] 2.4.2 Add helper function `prepare_trend_predictions(mvgam_fit, newdata)` in `R/predictions.R`. Extract trend params, create mock stanfit, call `prepare_predictions(mock_stanfit, brmsfit = mvgam_fit$trend_model, newdata = newdata)`. Handle NULL trend_model case (pure brms models). Return prep object.
    - [ ] 2.4.3 Add `extract_linpred_from_prep(prep, dpar = "mu")` function in `R/predictions.R`. Implement linear predictor computation using design matrices from prep object. Return matrix with dimensions ndraws × nobs. Based on exploration findings.
    - [ ] 2.4.4 Add input validation function `validate_newdata_for_predictions(mvgam_fit, newdata)` in `R/predictions.R`. Check that newdata is data.frame, check for required variables from both formulas (use `all.vars()`), check for time/series variables if trend model present. Return validated newdata or stop with informative error via `insight::format_error()`.

  - [ ] **2.5 Local Integration Testing (in tasks/)**
    - [ ] 2.5.1 Create `tasks/test_integration_local.R` script. **START WITH `devtools::load_all()`**. Load `tasks/fixtures/fit1.rds`, create newdata, and test the complete workflow: `prepare_obs_predictions()` → `prepare_trend_predictions()` → `extract_linpred_from_prep()` for both. Document any issues encountered.
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
