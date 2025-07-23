# Implementation: Summary Refactor

## Increments
Each increment should be shippable and add value.

### Increment 1: Create Modular Extractor Functions
- [ ] Task: Create `extract_model_spec()` function
  - Files: `R/summary.mvgam.R` (add function)
  - Validates: Can extract model specifications into structured list
  - Complete when: Function returns consistent list with formulas, family, trend model, dimensions

- [ ] Task: Create `extract_sampling_info()` function  
  - Files: `R/summary.mvgam.R` (add function)
  - Validates: Can extract sampling metadata
  - Complete when: Function returns chains, iterations, algorithm details

- [ ] Task: Create `extract_diagnostics()` function
  - Files: `R/summary.mvgam.R` (add function) 
  - Validates: Can extract MCMC diagnostics
  - Complete when: Function returns structured diagnostic information

- [ ] Task: Create `extract_parameters()` function
  - Files: `R/summary.mvgam.R` (add function)
  - Validates: Can extract all parameter estimates with existing logic
  - Complete when: Function returns all coefficient and family parameter summaries

### Increment 2: Create Summary Object and Print Method
- [ ] Task: Modify `summary.mvgam()` to use extractors and return structured object
  - Files: `R/summary.mvgam.R` (modify existing function)
  - Validates: Returns `mvgam_summary` object instead of printing
  - Complete when: Function returns structured object with all four components

- [ ] Task: Create `print.mvgam_summary()` method
  - Files: `R/summary.mvgam.R` (add method)
  - Validates: Produces identical output to current function
  - Complete when: Print method exactly reproduces current console output

- [ ] Task: Add S3 method export to NAMESPACE
  - Files: `NAMESPACE` (add export)
  - Validates: Method is properly exported
  - Complete when: `print.mvgam_summary` appears in NAMESPACE

### Increment 3: Enable Reuse in Other Functions
- [ ] Task: Refactor `print.mvgam()` to use extractor functions
  - Files: `R/print.mvgam.R` (modify to reuse extractors)
  - Validates: Eliminates code duplication between print and summary
  - Complete when: `print.mvgam()` uses shared extractor functions

- [ ] Task: Update documentation and tests
  - Files: `R/summary.mvgam.R` (roxygen comments), `tests/testthat/test-summary.R`
  - Validates: Documentation reflects new object structure
  - Complete when: Help files and tests cover new functionality
  - Testing note: Use internal mvgam model objects (e.g. `mvgam:::mvgam_example1`) to avoid building new models for testing

## Status
- Created: 2025-01-23
- Current Increment: 1
- Overall Progress: Not Started