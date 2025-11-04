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
