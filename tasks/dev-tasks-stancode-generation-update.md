## Relevant Files

### R Package Files
- `R/priors.R` - Prior extraction, combination, and user-facing functions (get_prior, set_prior, prior_summary)
- `R/stancode.R` - Stan code generation, assembly, and make_stancode implementation
- `R/standata.R` - Stan data generation and make_standata implementation
- `R/brms_integration.R` - Enhanced brms setup and integration functions
- `R/validations.R` - Enhanced validation utilities for inspection functions
- `man/*.Rd` - Auto-generated documentation (via roxygen2)

### Test Files
- `tests/testthat/test-foundation-components.R` - Tests for mvgamprior class and validation utilities
- `tests/testthat/test-core-systems.R` - Tests for prior extraction, combination, and Stan generation
- `tests/testthat/test-inspection-functions.R` - Tests for user-facing inspection functions
- `tests/testthat/test-brms-equivalence.R` - Tests for brms compatibility when trend_formula = NULL
- `tests/testthat/test-complete-workflows.R` - End-to-end workflow tests
- `tests/testthat/test-multivariate-inspection.R` - Multivariate model inspection tests
- `tests/testthat/test-statistical-validation.R` - Statistical correctness validation
- `tests/testthat/test-edge-cases.R` - Edge case and error handling tests
- `tests/testthat/test-performance.R` - Performance validation tests

### Context Files
- `/active/architecture-decisions.md` - Provides implementation context for dual-object architecture
- `/active/code_improvements.md` - May contain relevant enhancement guidelines
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

- [ ] 1.0 Build Foundation Components and Infrastructure
  - [ ] 1.1 Create mvgamprior class with S3 methods in `R/priors.R` - define data.frame subclass with columns: prior, class, coef, group, resp, dpar, nlpar, lb, ub, source, trend_component. Include print.mvgamprior() and summary.mvgamprior() methods following brms patterns
  - [ ] 1.2 Build validation utility functions in `R/validations.R` - create validate_brms_formula(), validate_trend_formula_brms(), and validate_setup_components() with checkmate assertions and insight error formatting
  - [ ] 1.3 Enhance setup_brms_lightweight() in `R/brms_integration.R` - add trend_formula parameter, improve error handling, and ensure mock backend setup works correctly for inspection functions
  - [ ] 1.4 Create Stan assembly helper functions in `R/stancode.R` - implement combine_observation_trend_stancode(), validate_stancode_structure(), and format_stan_compilation_errors() with proper error messaging
  - [ ] 1.5 Write unit tests in `tests/testthat/test-foundation-components.R` - test mvgamprior class creation, validation functions, and Stan helper utilities with valid/invalid inputs
- [ ] 2.0 Implement Core Prior and Stan Generation Systems
  - [ ] 2.1 Create prior extraction system in `R/priors.R` - implement extract_observation_priors() using brms::get_prior() and extract_trend_priors() using trend registry with parameter naming conventions
  - [ ] 2.2 Build prior combination logic in `R/priors.R` - create combine_obs_trend_priors() to merge observation and trend priors into single mvgamprior object with trend_component field population
  - [ ] 2.3 Implement Stan data generation in `R/standata.R` - create generate_combined_standata() that combines brms::standata() output with trend-specific data using existing trend system
  - [ ] 2.4 Create Stan code generation pipeline in `R/stancode.R` - implement generate_combined_stancode() that integrates observation model stancode with trend stanvars using enhanced assembly system
  - [ ] 2.5 Build trend-specific prior generators in `R/priors.R` - create get_ar_priors(), get_rw_priors(), get_var_priors() etc. following convention-based dispatch with _trend suffix naming
  - [ ] 2.6 Write core system tests in `tests/testthat/test-core-systems.R` - test prior extraction, combination logic, and Stan generation with various trend types and multivariate scenarios
- [ ] 3.0 Create User-Facing Inspection Functions
  - [ ] 3.1 Implement get_prior() function in `R/priors.R` - accept same parameters as mvgam(), use setup_brms_lightweight() and prior extraction system, return mvgamprior object with proper validation
  - [ ] 3.2 Implement make_stancode() function in `R/stancode.R` - accept same parameters plus prior argument, use Stan code generation pipeline, return character string with complete Stan model code
  - [ ] 3.3 Implement make_standata() function in `R/standata.R` - accept same parameters as make_stancode(), use Stan data generation system, return named list compatible with Stan
  - [ ] 3.4 Create set_prior() and prior() extensions in `R/priors.R` - enhance brms functions to handle trend-specific parameters with proper class validation and parameter bounds
  - [ ] 3.5 Implement prior_summary() function in `R/priors.R` - work with both fitted mvgam objects and model specifications, show actual vs planned priors with all/non-default filtering
  - [ ] 3.6 Add roxygen2 documentation in `R/priors.R`, `R/stancode.R`, `R/standata.R` - comprehensive @param, @return, @examples following TRD specifications with cross-references and proper @export tags
  - [ ] 3.7 Write user interface tests in `tests/testthat/test-inspection-functions.R` - test all user-facing functions with realistic workflows, edge cases, and error conditions matching TRD examples
- [ ] 4.0 Comprehensive Testing and Integration Validation
  - [ ] 4.1 Create brms equivalence tests in `tests/testthat/test-brms-equivalence.R` - verify that when trend_formula = NULL, generated Stan code exactly matches brm() output for various model types
  - [ ] 4.2 Build complete workflow tests in `tests/testthat/test-complete-workflows.R` - test entire get_prior() -> modify -> make_stan*() -> mvgam() -> prior_summary() workflows with validation
  - [ ] 4.3 Test multivariate scenarios in `tests/testthat/test-multivariate-inspection.R` - validate inspection functions work correctly with multivariate models, different trends per response, and factor models
  - [ ] 4.4 Validate statistical correctness in `tests/testthat/test-statistical-validation.R` - ensure modified priors in inspection functions correctly apply in fitted models using small test datasets
  - [ ] 4.5 Test edge cases and error handling in `tests/testthat/test-edge-cases.R` - invalid formulas, incompatible trends, malformed priors, with verification of helpful error messages
  - [ ] 4.6 Performance validation in `tests/testthat/test-performance.R` - ensure inspection functions complete in <1 second for typical models and memory usage stays within reasonable bounds
  - [ ] 4.7 Update package documentation in `README.md` and `vignettes/` - add inspection workflow examples, update package overview to highlight new capabilities following TRD requirements