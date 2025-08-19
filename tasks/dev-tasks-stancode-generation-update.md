## Relevant Files

### R Package Files
- `R/priors.R` - Prior extraction, combination, and user-facing functions (get_prior, set_prior, prior_summary)
- `R/stan_assembly.R` - Stan code generation, assembly, and data combination (generate_combined_stancode, combine_stan_data)
- `R/brms_integration.R` - Enhanced brms setup and integration functions (setup_brms_lightweight with trend_formula support)
- `R/validations.R` - Comprehensive validation utilities (validate_stan_code_structure, validate_combined_stancode, etc.)
- `man/*.Rd` - Auto-generated documentation (via roxygen2)

### Test Files
- `tests/testthat/test-priors.R` - Tests for mvgamprior class and validation utilities
- `tests/testthat/test-setup-brms.R` - Tests for prior extraction, combination, and Stan generation
- `tests/testthat/test-make_stan.R` - Tests for user-facing inspection functions
- `tests/testthat/test-brms-equivalence.R` - Tests for brms compatibility when trend_formula = NULL
- `tests/testthat/test-complete-workflows.R` - End-to-end workflow tests
- `tests/testthat/test-multivariate.R` - Multivariate model inspection tests
- `tests/testthat/test-statistical-validation.R` - Statistical correctness validation

### Context Files
- `/active/architecture-decisions.md` - Provides implementation context for dual-object architecture
- `/active/code_improvements.md` - Critical R code style and efficiency guidelines
- `/active/quick-reference.md` - Quick reference for development patterns

### Package Structure Files
- `NAMESPACE` - Export declarations (auto-generated via roxygen2)
- `DESCRIPTION` - Package dependencies if new packages needed
- `README.md` - Usage examples if user-facing functions added

### Notes
- All tests are located in `tests/testthat/` directory, separate from main R files
- Use `Rscript -e "devtools::load_all();testthat::test_file(path/to/test)"` to run specific tests during development
- Use `devtools::test()` or `testthat::test_dir("tests/testthat")` to run all tests
- Use `devtools::check()` to run full package validation
- Reference files in `/active` directory for implementation context
- Follow existing package conventions for function naming and structure

## Tasks

- [x] 1.0 Build Foundation Components and Infrastructure
  - [x] 1.1 Create prior helper functions using brmsprior class in `R/priors.R` - COMPLETED: implemented helper functions for working with brmsprior objects, trend-specific metadata via attributes, _trend suffix convention for trend parameters
  - [x] 1.2 Build validation utility functions in `R/validations.R` - COMPLETED: validate_brms_formula(), validate_trend_formula_brms(), and validate_setup_components() already exist with comprehensive validation
  - [x] 1.3 Enhance setup_brms_lightweight() in `R/brms_integration.R` - COMPLETED: added trend_formula parameter, improved error handling, enhanced for inspection functions
  - [x] 1.4 Stan assembly helper functions - COMPLETED: generate_combined_stancode(), validate_stan_code_structure(), and validate_combined_stancode() already exist in stan_assembly.R and validations.R
  - [x] 1.5 Write comprehensive unit tests - COMPLETED: created `tests/testthat/test-priors.R` (78 tests) and `tests/testthat/test-setup-brms.R` (46 tests) with comprehensive coverage including trend formula parsing behavior, order independence, and integration testing
  
- [ ] 2.0 Implement Core Prior and Stan Generation Systems
  - [x] 2.1 Create prior extraction system in `R/priors.R` - COMPLETED: implemented extract_observation_priors() using brms::get_prior() and extract_trend_priors() using trend registry with parameter naming conventions, added comprehensive input validation, all 78 tests pass
  - [x] 2.2 Build prior combination logic in `R/priors.R` - COMPLETED: implemented combine_obs_trend_priors() to merge observation and trend priors into single brmsprior object with proper validation and trend_component attributes
  - [x] 2.3 Stan data generation - COMPLETED: combine_stan_data() and generate_base_brms_standata() already exist in stan_assembly.R with comprehensive data combination
  - [x] 2.4 Stan code generation pipeline - COMPLETED: generate_combined_stancode() already exists in stan_assembly.R with comprehensive observation+trend integration
  - [x] 2.5 Integrate prior generation into trend dispatcher system - COMPLETED: replaced manual prior generators with convention-based dispatch using monitor_params metadata, automatic prior generation for all trend types, seamless integration with existing trend system, all 77 tests pass
  - [ ] 2.6 Implement prior flow to trend stanvar generators
    - [x] 2.6.1 Create common_trend_priors object - define shared prior specs (sigma_trend, LV, etc.) that multiple trends use - COMPLETED: created common_trend_priors object with sigma_trend, LV, ar1_trend, LV_raw, Z specifications including defaults, bounds, descriptions, and dimensions
    - [x] 2.6.2 Add prior_spec field to trend registry - modify register_trend_type() to accept optional prior_spec list parameter - COMPLETED: updated register_trend_type() function with prior_spec parameter, added validation, enhanced documentation, addressed code reviewer feedback
    - [x] 2.6.3 Create get_trend_prior_spec() function - merge trend-specific priors with common_trend_priors based on parameter names - COMPLETED: implemented function to retrieve complete prior specs, resolves common_trend_priors references, includes proper validation and documentation
    - [x] 2.6.4 Create build_ar_prior_spec() helper - dynamically generate ar{lag}_trend specs for non-continuous lags like c(1,12,24) - COMPLETED: implemented dynamic AR prior generator with support for arbitrary lag structures, custom AR priors, optional sigma/common parameters, comprehensive validation and examples
    - [ ] 2.6.5 Create map_prior_to_stan_string() function - convert brmsprior row to Stan distribution string - NEXT: This function should accept a single brmsprior row and return a Stan distribution string suitable for Stan model code. It should handle empty/missing priors by using a fallback default, validate the input, and return clean Stan syntax like "normal(0, 1)" or "exponential(2)". Add this function to R/priors.R after the build_ar_prior_spec() function.
    - [ ] 2.6.6 Create extract_prior_string() helper - find matching prior in brmsprior object by class/coef with suffix handling
    - [ ] 2.6.7 Update generate_combined_stancode() signature - add prior = NULL parameter, update roxygen2 docs
    - [ ] 2.6.8 Update generate_rw_trend_stanvars() signature - add prior = NULL parameter, update function docs
    - [ ] 2.6.9 Update generate_ar_trend_stanvars() signature - add prior = NULL parameter, update function docs
    - [ ] 2.6.10 Update generate_var_trend_stanvars() signature - add prior = NULL parameter, update function docs
    - [ ] 2.6.11 Update generate_car_trend_stanvars() signature - add prior = NULL parameter, update function docs
    - [ ] 2.6.12 Update generate_zmvn_trend_stanvars() signature - add prior = NULL parameter, update function docs
    - [ ] 2.6.13 Update generate_pw_trend_stanvars() signature - add prior = NULL parameter, update function docs
    - [ ] 2.6.14 Update generate_gp_injection_stanvars() signature - add prior = NULL parameter if exists
    - [ ] 2.6.15 Create map_trend_priors() function - extract relevant priors from brmsprior, handle ar{lag}_trend patterns
    - [ ] 2.6.16 Implement prior usage in generate_rw_trend_stanvars() - use mapped priors or COMMON defaults for sigma_trend
    - [ ] 2.6.17 Implement AR lag detection in generate_ar_trend_stanvars() - extract ar_lags, map each ar{lag}_trend prior
    - [ ] 2.6.18 Implement prior usage in generate_ar_trend_stanvars() - apply priors to each ar{lag}_trend coefficient
    - [ ] 2.6.19 Add prior_spec to RW trend registration - reference common_trend_priors.sigma_trend in .onLoad()
    - [ ] 2.6.20 Add prior_spec to AR trend registration - use build_ar_prior_spec() for dynamic lag handling in .onLoad()
    - [ ] 2.6.21 Add prior_spec to VAR trend registration - include only sigma_trend (process variances), A_trend matrix handled by stationarity constraints
    - [ ] 2.6.22 Add prior_spec to CAR trend registration - reference common_trend_priors.sigma_trend and ar1_trend
    - [ ] 2.6.23 Add prior_spec to ZMVN trend registration - minimal spec, mostly uses defaults
    - [ ] 2.6.24 Add prior_spec to PW trend registration - include k_trend, m_trend, delta_trend specs
    - [ ] 2.6.25 Update trend dispatcher in stan_assembly.R - pass prior argument through dispatch chain to all generators
    - [ ] 2.6.26 Update tests in test-trend_system.R - add prior = NULL to all generate_*_trend_stanvars() calls
    - [ ] 2.6.27 Update tests in test-stan_assembly.R - add prior = NULL to generate_combined_stancode() calls
    - [ ] 2.6.28 Update tests in test-setup-brms.R - verify prior parameter flows through setup_brms_lightweight()
    - [ ] 2.6.29 Create test for prior flow through generate_combined_stancode() - verify priors reach generators
    - [ ] 2.6.30 Create test for AR lag-specific priors - test ar1_trend, ar12_trend, ar24_trend prior application
    - [ ] 2.6.31 Create test for common_trend_priors inheritance - verify sigma_trend shared across RW, AR, CAR
    - [ ] 2.6.32 Create test for default prior fallback - verify defaults used when no user prior specified
  - [ ] 2.7 Write core system tests in `tests/testthat/test-priors.R` - test prior extraction, combination logic, and Stan generation with various trend types and multivariate scenarios
  - [ ] 2.8 Implement fixed Z matrix support for dimension-reduced factor models - enhance prior system to detect fixed Z matrix specifications, convert Z from stochastic parameter to data object in Stan model, enable shared state models and user-specified factor structures with proper validation and Stan code generation updates
  
- [ ] 3.0 Create User-Facing Inspection Functions
  - [ ] 3.1 Implement get_prior() function in `R/priors.R` - accept same parameters as mvgam(), use setup_brms_lightweight() and prior extraction system, return brmsprior object with proper validation
  - [ ] 3.2 Implement make_stancode() function in `R/priors.R` - accept same parameters plus prior argument, use existing generate_combined_stancode() pipeline, return character string with complete Stan model code
  - [ ] 3.3 Implement make_standata() function in `R/priors.R` - accept same parameters as make_stancode(), use existing combine_stan_data() system, return named list compatible with Stan
  - [ ] 3.4 Create set_prior() and prior() extensions in `R/priors.R` - enhance brms functions to handle trend-specific parameters with proper class validation and parameter bounds
  - [ ] 3.5 Implement prior_summary() function in `R/priors.R` - work with both fitted mvgam objects and model specifications, show actual vs planned priors with all/non-default filtering
  - [ ] 3.6 Add roxygen2 documentation in `R/priors.R` - comprehensive @param, @return, @examples following TRD specifications with cross-references and proper @export tags
  - [ ] 3.7 Write user interface tests in `tests/testthat/test-make_stan.R` - test all user-facing functions with realistic workflows, edge cases, and error conditions matching TRD examples
  
- [ ] 4.0 Comprehensive Testing and Integration Validation
  - [ ] 4.1 Create brms equivalence tests in `tests/testthat/test-brms-equivalence.R` - verify that when trend_formula = NULL, generated Stan code exactly matches brm() output for various model types
  - [ ] 4.2 Build complete workflow tests in `tests/testthat/test-complete-workflows.R` - test entire get_prior() -> modify -> make_stan*() -> mvgam() -> prior_summary() workflows with validation
  - [ ] 4.3 Test multivariate scenarios in `tests/testthat/test-multivariate-inspection.R` - validate inspection functions work correctly with multivariate models, different trends per response, and factor models
  - [ ] 4.4 Validate statistical correctness in `tests/testthat/test-statistical-validation.R` - ensure modified priors in inspection functions correctly apply in fitted models using small test datasets
  - [ ] 4.5 Test edge cases and error handling in `tests/testthat/complete-workflows.R` - invalid formulas, incompatible trends, malformed priors, with verification of helpful error messages
  - [ ] 4.6 Update package documentation in `README.md` and `vignettes/` - add inspection workflow examples, update package overview to highlight new capabilities following TRD requirements
