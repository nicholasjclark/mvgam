# TRD-stancode-generation-update Development Tasks

## Overview
This document tracks implementation progress for the stancode generation update feature, implementing comprehensive prior system integration and Stan code inspection capabilities for mvgam trend models.

## Task Status
- ✅ **Completed**: All foundation and core prior systems (sections 1.0-2.6)
- 🔄 **In Progress**: Parameter standardization (section 2.7) 
- ⏳ **Blocked**: All subsequent tasks depend on parameter standardization completion

---

## COMPLETED SECTIONS

### 1.0 Build Foundation Components and Infrastructure ✅
- [x] 1.1 Create prior helper functions using brmsprior class in `R/priors.R` - COMPLETED
- [x] 1.2 Build validation utility functions in `R/validations.R` - COMPLETED
- [x] 1.3 Enhance setup_brms_lightweight() in `R/brms_integration.R` - COMPLETED
- [x] 1.4 Stan assembly helper functions - COMPLETED
- [x] 1.5 Write comprehensive unit tests - COMPLETED

### 2.0 Implement Core Prior and Stan Generation Systems ✅
- [x] 2.1 Create prior extraction system in `R/priors.R` - COMPLETED
- [x] 2.2 Build prior combination logic in `R/priors.R` - COMPLETED
- [x] 2.3 Stan data generation - COMPLETED
- [x] 2.4 Stan code generation pipeline - COMPLETED
- [x] 2.5 Integrate prior generation into trend dispatcher system - COMPLETED
- [x] 2.6 Implement prior flow to trend stanvar generators - COMPLETED: All 18 sub-tasks completed including theta1_trend addition to common_trend_priors, centralized prior system implementation, and comprehensive test coverage (121 tests passing at 100% success rate)

---

## NEXT TASK: Parameter Standardization and Non-Centered Parameterization

**Context**: The current system uses inconsistent parameter names across trend generators (raw_innovations vs innovations_trend, LV vs lv_trend, L_Omega vs L_Omega_trend). We need to standardize on a three-layer architecture:

1. **`innovations_trend`** (parameters block) - raw std_normal innovations  
2. **`scaled_innovations_trend`** (transformed parameters) - after applying sigma_trend scaling and correlations
3. **`lv_trend`** (transformed parameters) - final latent states using trend-specific dynamics

**Structural Pattern**: Each trend generator must follow the 3-stanvar pattern for clarity:
- **`{trend}_parameters_stanvar`** (block = "parameters") - Declares all trend-specific parameters
- **`{trend}_tparameters_stanvar`** (block = "tparameters") - Computes lv_trend dynamics
- **`{trend}_model_stanvar`** (block = "model") - Only priors, no computations

**Why this task is next**: All current tests pass with existing names, providing a stable foundation. Parameter standardization must happen before implementing trend registry prior specifications (section 3.0) to avoid name conflicts and ensure consistency.

**Testing strategy**: Each phase is independently testable with 15-minute completion targets to enable fail-fast validation.

### 2.7 Parameter Standardization and Non-Centered Parameterization 🔄
- [x] 2.7.1 Phase 1.1: Update shared innovation system parameter names (raw_innovations → innovations_trend, L_Omega → L_Omega_trend) - COMPLETED
- [x] 2.7.2 Phase 1.2: Update shared innovation system to generate scaled_innovations_trend in transformed parameters (apply sigma_trend and correlations) - COMPLETED
- [x] 2.7.3 Phase 1.3: Test shared innovation system in isolation with simple test case - COMPLETED: Updated tests resolved 7 failures, parameter standardization working
- [x] 2.7.4 Phase 2.1: Update RW generator to use scaled_innovations_trend → lv_trend pattern (simplest case) - COMPLETED: Established ma_innovations_trend naming convention for MA transformations, implemented 3-stanvar pattern (rw_parameters, rw_tparameters, rw_model), added validation fixes for helper function and dimension checking
- [x] 2.7.5 Phase 2.2: Refactor AR generator - implement 3-stanvar pattern (ar_parameters, ar_tparameters, ar_model) and use standardized parameter names - COMPLETED: Implemented clean 3-stanvar pattern with proper validation, supports multiple AR lags (including discontinuous lags), MA combinations, factor models, uses standardized parameter names (ar{lag}_trend, lv_trend, ma_innovations_trend, scaled_innovations_trend), added comprehensive tests covering unit testing, integration testing, edge cases, and system consistency
- [x] 2.7.6 Phase 2.3: Refactor CAR generator - implement 3-stanvar pattern and standardized parameter names - COMPLETED: Implemented clean 3-stanvar pattern (car_parameters, car_tparameters, car_model), updated to use standardized parameter names (ar1_trend, lv_trend, scaled_innovations_trend), removed mu_trend from dynamics for consistency with RW/AR generators, added proper validation, maintains CAR-specific features (time_dis, continuous-time AR evolution)  
- [x] 2.7.7 Phase 2.4: Refactor ZMVN generator - convert to non-centered parameterization with 3-stanvar pattern - COMPLETED: Implemented clean 3-stanvar pattern (zmvn_parameters, zmvn_tparameters, zmvn_model), uses shared innovation system for non-centered parameterization with direct transformation lv_trend = scaled_innovations_trend, works for both n_lv=1 and n_lv>1 cases, properly handles hierarchical and factor model cases, added comprehensive tests covering univariate/multivariate, correlations, factor models, hierarchical structures, edge cases, validation, standardized naming (11/11 tests passing)
- [ ] 2.7.8 Phase 2.5: Refactor VAR generator - implement 3-stanvar pattern (VAR exempt from shared innovations)
- [ ] 2.7.9 Phase 2.6: Update PW generator if needed (may not use shared innovations)
- [ ] 2.7.10 Phase 3.1: Update all tests to expect new parameter names (innovations_trend, lv_trend, etc.)
- [ ] 2.7.11 Phase 3.2: Update common_trend_priors to remove LV and use consistent naming
- [ ] 2.7.12 Phase 3.3: Run full test suite and verify all trend types work with new standardization

---

## PENDING SECTIONS (After Parameter Standardization)

### 3.0 Complete Prior Specification System ⏳
- [ ] 3.1 Add prior_spec to RW trend registration - reference common_trend_priors.sigma_trend in .onLoad()
- [ ] 3.2 Add prior_spec to AR trend registration - use build_ar_prior_spec() for dynamic lag handling in .onLoad()
- [ ] 3.3 Add prior_spec to VAR trend registration - include only sigma_trend (process variances), A_trend matrix handled by stationarity constraints
- [ ] 3.4 Add prior_spec to CAR trend registration - reference common_trend_priors.sigma_trend and ar1_trend
- [ ] 3.5 Add prior_spec to ZMVN trend registration - minimal spec, mostly uses defaults
- [ ] 3.6 Add prior_spec to PW trend registration - include k_trend, m_trend, delta_trend specs
- [ ] 3.7 Update trend dispatcher in stan_assembly.R - pass prior argument through dispatch chain to all generators
- [ ] 3.8 Update tests in test-trend_system.R - add prior = NULL to all generate_*_trend_stanvars() calls
- [ ] 3.9 Update tests in test-stan_assembly.R - add prior = NULL to generate_combined_stancode() calls
- [ ] 3.10 Update tests in test-setup-brms.R - verify prior parameter flows through setup_brms_lightweight()
- [ ] 3.11 Create test for prior flow through generate_combined_stancode() - verify priors reach generators
- [ ] 3.12 Create test for AR lag-specific priors - test ar1_trend, ar12_trend, ar24_trend prior application
- [ ] 3.13 Create test for common_trend_priors inheritance - verify sigma_trend shared across RW, AR, CAR
- [ ] 3.14 Create test for default prior fallback - verify defaults used when no user prior specified
- [ ] 3.15 Write core system tests in `tests/testthat/test-priors.R` - test prior extraction, combination logic, and Stan generation with various trend types and multivariate scenarios

### 4.0 Implement Fixed Z Matrix Support ⏳
- [ ] 4.1 Implement fixed Z matrix support for dimension-reduced factor models - enhance prior system to detect fixed Z matrix specifications, convert Z from stochastic parameter to data object in Stan model, enable shared state models and user-specified factor structures with proper validation and Stan code generation updates

### 5.0 Create User-Facing Inspection Functions ⏳
- [ ] 5.1 Implement get_prior() function in `R/priors.R` - accept same parameters as mvgam(), use setup_brms_lightweight() and prior extraction system, return brmsprior object with proper validation
- [ ] 5.2 Implement make_stancode() function in `R/priors.R` - accept same parameters plus prior argument, use existing generate_combined_stancode() pipeline, return character string with complete Stan model code
- [ ] 5.3 Implement make_standata() function in `R/priors.R` - accept same parameters as make_stancode(), use existing combine_stan_data() system, return named list compatible with Stan
- [ ] 5.4 Create set_prior() and prior() extensions in `R/priors.R` - enhance brms functions to handle trend-specific parameters with proper class validation and parameter bounds
- [ ] 5.5 Implement prior_summary() function in `R/priors.R` - work with both fitted mvgam objects and model specifications, show actual vs planned priors with all/non-default filtering
- [ ] 5.6 Add roxygen2 documentation in `R/priors.R` - comprehensive @param, @return, @examples following TRD specifications with cross-references and proper @export tags
- [ ] 5.7 Write user interface tests in `tests/testthat/test-make_stan.R` - test all user-facing functions with realistic workflows, edge cases, and error conditions matching TRD examples

### 6.0 Comprehensive Testing and Integration Validation ⏳
- [ ] 6.1 Create brms equivalence tests in `tests/testthat/test-brms-equivalence.R` - verify that when trend_formula = NULL, generated Stan code exactly matches brm() output for various model types
- [ ] 6.2 Build complete workflow tests in `tests/testthat/test-complete-workflows.R` - test entire get_prior() -> modify -> make_stan*() -> mvgam() -> prior_summary() workflows with validation
- [ ] 6.3 Test multivariate scenarios in `tests/testthat/test-multivariate-inspection.R` - validate inspection functions work correctly with multivariate models, different trends per response, and factor models
- [ ] 6.4 Validate statistical correctness in `tests/testthat/test-statistical-validation.R` - ensure modified priors in inspection functions correctly apply in fitted models using small test datasets
- [ ] 6.5 Test edge cases and error handling in `tests/testthat/complete-workflows.R` - invalid formulas, incompatible trends, malformed priors, with verification of helpful error messages
- [ ] 6.6 Update package documentation in `README.md` and `vignettes/` - add inspection workflow examples, update package overview to highlight new capabilities following TRD requirements

---

## Notes
- Each sub-task should be completable within 15 minutes for fail-fast validation
- Use `code-reviewer` agent for all R code changes (non-negotiable)
- All tests must pass before moving to the next section
- Parameter standardization (2.7) is the critical path blocking all subsequent work