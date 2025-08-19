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
  - [ ] 2.6 Write core system tests in `tests/testthat/test-priors.R` - test prior extraction, combination logic, and Stan generation with various trend types and multivariate scenarios
  - [ ] 2.7 Implement fixed Z matrix support for dimension-reduced factor models - enhance prior system to detect fixed Z matrix specifications, convert Z from stochastic parameter to data object in Stan model, enable shared state models and user-specified factor structures with proper validation and Stan code generation updates
  
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
