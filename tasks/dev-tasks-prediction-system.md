# Development Tasks: Prediction System

## Relevant Files

### R Package Files
- `R/index-mvgam.R` - Contains `variables.mvgam()` and `.categorize_parameters()`
- `R/as.data.frame.mvgam.R` - Posterior extraction methods that use aliases
- `R/mcmc_plot.mvgam.R` - MCMC plotting that uses categorized parameters
- `R/tidier_methods.R` - Tidy methods that use categorized parameters
- `R/pairs.mvgam.R` - Pairs plotting that uses categorized parameters
- `R/residual_cor.R` - Residual correlation analysis using categorized parameters

### Test Files
- `tests/testthat/test-index-mvgam.R` - Tests for parameter indexing and categorization
- `tests/testthat/test-as.data.frame.mvgam.R` - Tests for posterior extraction with aliases
- `tests/testthat/test-mcmc_plot.mvgam.R` - Tests for plotting with renamed parameters

### Context Files
- `/architecture/architecture-decisions.md` - Lazy categorization architecture
- `/tasks/trd-prediction-system.md` - Prediction system requirements (depends on this work)

### Notes
- Test models available in `tests/local/` for validation
- Use `devtools::load_all()` before testing
- No test errors or warnings allowed

---

## Tasks

- [ ] 1.0 Parameter Renaming Infrastructure for User-Facing Methods
  - [ ] 1.1 Fix `variables.mvgam()` to return structured categorization output instead of character vector
  - [ ] 1.2 Update all calling code that expects character vector from `variables()` to handle structured output
  - [ ] 1.3 Research brms parameter naming patterns for coefficients (intercepts, fixed effects, interactions) using the r package analyzer
  - [ ] 1.4 Research brms parameter naming patterns for random effects (varying intercepts, varying slopes, correlations) using the r package analyzer
  - [ ] 1.5 Implement internal helper to extract coefficient names from brms mock model objects for observation formula
  - [ ] 1.6 Implement internal helper to extract coefficient names from brms mock model objects for trend formula
  - [ ] 1.7 Implement internal helper to extract random effect names from brms model structure
  - [ ] 1.8 Implement internal helper to map Stan parameter names to brms-style names for observation coefficients
  - [ ] 1.9 Implement internal helper to map Stan parameter names to brms-style names for trend coefficients
  - [ ] 1.10 Implement internal helper to map Stan parameter names to brms-style names for random effects
  - [ ] 1.11 Update `.categorize_parameters()` to populate alias column for observation_betas using coefficient mapping
  - [ ] 1.12 Update `.categorize_parameters()` to populate alias column for trend_betas using coefficient mapping
  - [ ] 1.13 Update `.categorize_parameters()` to populate alias column for observation_re_params using random effects mapping
  - [ ] 1.14 Update `.categorize_parameters()` to populate alias column for trend_re_params using random effects mapping
  - [ ] 1.15 Validate parameter aliasing works correctly with `as.data.frame.mvgam()` using test models
  - [ ] 1.16 Validate parameter aliasing works correctly with `mcmc_plot.mvgam()` using test models
  - [ ] 1.17 Validate parameter aliasing works correctly with `tidy.mvgam()` using test models
  - [ ] 1.18 Test parameter extraction across test models in `tests/local/` for completeness
  - [ ] 1.19 Ensure all posterior package methods work with renamed parameters
  - [ ] 1.20 Update roxygen2 documentation for `variables.mvgam()` to reflect new return structure

---

## High-Level Parent Tasks (For Future Development)

- **2.0 Foundation: Parameter Extraction and Prediction Infrastructure**
  - Build lazy parameter categorization system for obs/trend separation
  - Create integration layer for brms `prepare_predictions()` with mock objects
  - Implement core validation for newdata structure and requirements
  - Leverage test models in `tests/local/` for validation

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
  - Write vignettes: "Prediction and Forecasting" and "marginaleffects Integration"
  - Ensure R CMD check passes with no errors/warnings
