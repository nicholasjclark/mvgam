# Development Tasks: Model Fitting and mvgam Object Creation

## Relevant Files

### R Package Files to Modify
- `R/mvgam_core.R` - Add fitting integration and object creation to existing mvgam() function
- `R/brms_integration.R` - Add backend abstraction functions (follows existing brms integration pattern)
- `R/print.mvgam.R` - Add S3 methods for fitted objects (extend existing print methods)
- `R/validations.R` - Add validation for fitted objects (extend existing validation patterns)

### New R Package Files (Minimal)
- `R/fit_mvgam.R` - Stan fitting functions only (cannot integrate into existing files due to size)

### Test Files to Extend
- `tests/testthat/test-mvgam_core.R` - Extend existing mvgam() tests with fitting functionality
- `tests/testthat/test-brms_integration.R` - Extend with backend testing
- `tests/testthat/test-validations.R` - Extend with fitted object validation
- `tests/local/` - New comprehensive model fitting tests (like brms pattern)

### Context Files
- `/tasks/trd-model-fitting-object-creation.md` - Complete requirements specification
- `/architecture/dependency-graph.md` - Package dependency context
- `/architecture/architecture-decisions.md` - Design patterns and constraints

### Package Structure Files
- `NAMESPACE` - Export declarations (auto-generated via roxygen2)
- `.Rbuildignore` - Exclude local test directory from CRAN builds
- `DESCRIPTION` - Add cmdstanr to Suggests if needed

### Notes
- Integrate with existing files wherever possible to maintain package coherence
- Use existing test infrastructure and extend rather than create new test files
- Focus on seamless integration with existing functions without modification
- Local test directory follows brms pattern for comprehensive model testing
- All new functionality must work with existing mvgam infrastructure unchanged

## Tasks

- [ ] 1.0 Research and Design Foundation
  - [X] 1.1 Use r-package-analyzer to study brms backend patterns in brms/R/backends.R and understand cmdstanr/rstan abstraction
  - [X] 1.2 Use r-package-analyzer to study brms object structure in brmsfit construction for prediction/residuals/plotting compatibility
  - [X] 1.3 Use r-package-analyzer to study marginaleffects package interaction patterns with brmsfit objects for ecosystem compatibility
  - [X] 1.4 Use r-package-analyzer to study insight package model information extraction from brms objects for post-fitting analysis
  - [X] 1.5 Use r-package-analyzer to study brms update-enabling metadata storage patterns in brmsfit objects for future extensibility
  - [X] 1.6 Analyze existing `R/mvgam_core.R` structure to identify integration points for fitting functionality
  - [X] 1.7 Analyze existing `R/brms_integration.R` patterns to determine where backend functions should be added

- [ ] 2.0 Backend Implementation
  - [X] 2.1 Use r-package-analyzer to examine complete brms backends.R functionality ensuring no features are missed in mvgam implementation
  - [X] 2.2 Create `R/backends.R` with complete brms backend functions adapted for mvgam with proper attribution to Paul-Christian Bürkner
  - [X] 2.3 Add comprehensive roxygen2 documentation to `R/backends.R` with appropriate brms attribution and mvgam-specific modifications
  - [X] 2.4 Submit `R/backends.R` to code-reviewer identifying critical integration issues requiring resolution before functional backend system
  - [x] 2.5 Check `architecture/dependency-graph.md` to understand existing mvgam dependencies and integration patterns with brms ecosystem
  - [x] 2.6 Research brms exported functions to determine which utility functions (`as_one_character`, `stop2`, `do_call`, etc.) are available vs need implementation
  - [x] 2.7 Implement missing utility functions required by `R/backends.R` either by importing from brms or creating mvgam-specific versions
  - [x] 2.8 Add mvgam object support to all backend functions replacing `is.brmsfit(x)` checks with `is.mvgam(x)` (simplified approach)
  - [x] 2.9 Replace brms-specific error handling patterns (`stop2`) with mvgam standards using `insight::format_error()` throughout backends.R
  - [x] 2.10 Implement working `fit_mvgam_model()` function in `R/mvgam_core.R` integrating with `parse_model()`, `compile_model()`, and `fit_model()` from backends.R
  - [ ] 2.11 Add backend parameter handling to existing `mvgam()` function enabling backend selection with `control` argument following brms patterns
  - [ ] 2.12 Create comprehensive test file `tests/testthat/test-backends.R` validating all backend functions with rstan, cmdstanr, and mock backends
  - [ ] 2.13 Test complete integration workflow: mvgam() → generate_combined_stancode() → fit_mvgam_model() → backends.R functions
  - [ ] 2.14 Validate backend switching functionality ensuring seamless rstan/cmdstanr interoperability with existing mvgam infrastructure
  - [ ] 2.15 Test backend functions with existing `generate_combined_stancode()` and `combine_stan_data()` outputs ensuring perfect compatibility

- [ ] 3.0 Object Creation and S3 Methods
  - [ ] 3.1 Use r-package-analyzer to study brmsfit object construction patterns focusing on essential components and S3 class hierarchy requirements
  - [ ] 3.2 Use r-package-analyzer to study insight package integration methods (get_data, find_formula, get_parameters, model_info) for ecosystem compatibility
  - [ ] 3.3 Use r-package-analyzer to study brms attribute storage patterns for data validation (terms, na.action, contrasts, xlev) and extension metadata
  - [ ] 3.4 Use r-package-analyzer to study brms data preservation patterns for validate_newdata() compatibility and marginaleffects integration requirements
  - [ ] 3.5 Design mvgam object structure specification implementing `c("mvgamfit", "brmsfit")` class hierarchy with essential brmsfit component preservation
  - [ ] 3.6 Implement data storage structure in `create_mvgam_object()` preserving original data with insight-compatible attributes (terms, na.action, contrasts, xlev)
  - [ ] 3.7 Implement formula component storage in `create_mvgam_object()` enabling insight formula extraction with trend formula preservation for variable identification
  - [ ] 3.8 Implement parameter organization structure in `create_mvgam_object()` supporting insight parameter extraction with proper trend parameter separation using existing `_trend` naming
  - [ ] 3.9 Implement metadata storage structure in `create_mvgam_object()` supporting model_info() requirements: family, link, multivariate flags, backend information
  - [ ] 3.10 Implement update-enabling metadata storage in `create_mvgam_object()` storing original function call, data reference name, compilation artifacts, and version information
  - [ ] 3.11 Add `create_mvgam_object()` core function to `R/mvgam_core.R` implementing the designed object structure with proper class hierarchy and component organization
  - [ ] 3.12 Create `R/insight_effects.R` with file header and organization structure for insight and marginaleffects integration methods
  - [ ] 3.13 Add `get_data.mvgamfit()` method to `R/insight_effects.R` implementing insight data extraction interface returning original data with preserved attributes
  - [ ] 3.14 Add `model_info.mvgamfit()` method to `R/insight_effects.R` implementing insight model information extraction with multivariate and Bayesian flags
  - [ ] 3.15 Add `find_variables.mvgamfit()` method to `R/insight_effects.R` implementing insight variable extraction supporting component-based access and trend variable identification
  - [ ] 3.16 Add `get_parameters.mvgamfit()` method to `R/insight_effects.R` implementing insight parameter extraction returning tidy data.frame with component organization
  - [ ] 3.17 Add `get_predict.mvgamfit()` method to `R/insight_effects.R` implementing marginaleffects prediction interface returning compatible matrices for marginal effects calculation
  - [ ] 3.18 Add basic validation in `create_mvgam_object()` ensuring required components present and class hierarchy properly established for ecosystem compatibility
  - [ ] 3.19 Add roxygen2 documentation for `create_mvgam_object()` explaining object structure design and ecosystem integration approach
  - [ ] 3.20 Extend existing `tests/testthat/test-mvgam_core.R` with object structure validation testing class hierarchy and essential component presence
  - [ ] 3.21 Create `tests/testthat/test-insight-integration.R` testing insight method implementations with standard insight package validation patterns

- [ ] 4.0 Main Interface Integration
  - [ ] 4.1 Add `run_model` parameter to existing `mvgam()` function in `R/mvgam_core.R` with default TRUE for backward compatibility
  - [ ] 4.2 Add `backend` parameter to existing `mvgam()` function with auto-detection default copying brms pattern
  - [ ] 4.3 Add `silent` parameter to existing `mvgam()` function for Stan error control following brms implementation
  - [ ] 4.4 Integrate `fit_mvgam()` call into existing `mvgam()` workflow after `generate_combined_stancode()` and `combine_stan_data()`
  - [ ] 4.5 Implement conditional logic: when `run_model = FALSE`, return setup object containing stancode/standata/metadata
  - [ ] 4.6 Integrate `create_mvgam_object()` call into existing `mvgam()` workflow after successful fitting
  - [ ] 4.7 Implement original function call capture in `mvgam()` using `match.call()` for update-enabling metadata storage following brms patterns
  - [ ] 4.8 Implement data reference name preservation in `mvgam()` storing `substitute(data)` for update flexibility rather than data object storage
  - [ ] 4.9 Implement version tracking integration in `mvgam()` capturing R, mvgam, backend, and dependency versions for reproducibility validation
  - [ ] 4.10 Ensure all existing `mvgam()` parameters preserved and existing workflow completely unchanged when `run_model = TRUE`
  - [ ] 4.11 Implement Stan error propagation with `silent` argument control copying brms error handling patterns
  - [ ] 4.12 Update roxygen2 documentation for `mvgam()` explaining new parameters while preserving existing documentation
  - [ ] 4.13 Verify modified `mvgam()` function maintains exact same behavior as before when `run_model = TRUE` and new parameters unused

- [ ] 5.0 Testing Infrastructure
  - [ ] 5.1 Create `tests/local/` directory structure copying brms local test organization for comprehensive model testing
  - [ ] 5.2 Add `tests/local/*` to `.Rbuildignore` to exclude comprehensive model tests from CRAN package builds
  - [ ] 5.3 Create `tests/local/test-comprehensive-fitting.R` with full model tests using existing portal_data and trend types
  - [ ] 5.4 Extend existing `tests/testthat/helper-mvgam.R` with fitting test utilities and validation functions
  - [ ] 5.5 Extend existing `tests/testthat/test-mvgam_core.R` with `run_model = FALSE` parameter testing
  - [ ] 5.6 Add backend-specific testing to existing `tests/testthat/test-brms_integration.R` for cmdstanr/rstan/mock
  - [ ] 5.7 Extend existing `tests/testthat/test-validations.R` with fitted object validation testing
  - [ ] 5.8 Add ecosystem compatibility tests verifying mvgam objects work with existing package methods
  - [ ] 5.9 Create error handling tests in existing test files verifying Stan errors propagate with silent control
  - [ ] 5.10 Document testing approach extending existing `tests/README.md` with local vs CRAN test separation explanation

- [ ] 6.0 Integration Validation and Final Testing
  - [ ] 6.1 Run complete existing test suite verifying zero regressions in current mvgam functionality
  - [ ] 6.2 Test seamless integration with existing `mvgam_formula()` constructor ensuring formula processing unchanged
  - [ ] 6.3 Test perfect integration with existing `get_prior()` system ensuring prior workflow completely preserved
  - [ ] 6.4 Test integration with all existing validation functions ensuring every existing check continues working unchanged
  - [ ] 6.5 Verify existing `generate_combined_stancode()` output works perfectly with new `fit_mvgam()` function
  - [ ] 6.6 Verify existing `combine_stan_data()` output integrates seamlessly with new fitting infrastructure without modification
  - [ ] 6.7 Test mvgam object ecosystem compatibility with all existing S3 methods (plot.mvgam, summary.mvgam, print.mvgam, etc.)
  - [ ] 6.8 Validate Stan error propagation using intentionally broken stancode ensuring silent argument works correctly
  - [ ] 6.9 Test complete `run_model = FALSE` workflow ensuring users access stancode/standata/priors from single function call
  - [ ] 6.10 Run comprehensive model fitting tests in `tests/local/` covering all existing trend types, data structures, and integration patterns
