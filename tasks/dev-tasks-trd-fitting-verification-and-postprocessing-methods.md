# Development Tasks: Model Fitting Verification and Post-Processing Methods

## Relevant Files

### R Package Files
- `R/mvgam_core.R` - Core fitting functions (`mvgam_single_dataset` → `mvgam_single`, `mvgam_multiple`)
- `R/print.mvgam.R` - Print method (needs replacing)
- `R/summary.mvgam.R` - Summary methods (new file to create)
- `R/diagnostics.mvgam.R` - Diagnostic methods (new file to create)
- `R/variables.mvgam.R` - Parameter discovery (new file to create)
- `R/index-mvgam.R` - Contains existing `variables.mvgam()` (line 174 in dependency-graph.md)

### Test Files
- `tests/local/test-models-single.R` - Single dataset tests (new file to create)
- `tests/local/test-models-multiple.R` - Multiple imputation tests (new file to create)
- `tests/local/setup_tests_local.R` - Test setup helpers (existing)

### Reference Files
- `target_generation.R` - Test model generation patterns
- `tests/local/tests-models1.R` - Existing tests (to be replaced)

### Package Structure Files
- `NAMESPACE` - Export declarations (auto-generated via roxygen2)
- `DESCRIPTION` - Package dependencies (may need `posterior` package added to Imports)

### Notes
- All tests located in `tests/local/` directory (not run on CRAN)
- Use `devtools::load_all()` before testing
- Follow brms conventions: use brms exported functionality where possible
- Backend compatibility: uses `brms::read_csv_as_stanfit` to convert cmdstanr to rstan stanfit
- Test data generation: reference `target_generation.R` for diverse test cases
- Model patterns: reference `target_generation.R` for diverse test cases

---

## Tasks

- [ ] **1.0 Function Verification, Renaming, and Basic Tests**
  - [ ] 1.1 Read `R/mvgam_core.R` to locate `mvgam_single_dataset()` function definition and understand its current implementation
  - [ ] 1.2 Rename function from `mvgam_single_dataset()` to `mvgam_single()` in `R/mvgam_core.R` using Edit tool
  - [ ] 1.3 Use Grep to find all calls to `mvgam_single_dataset()` throughout the codebase to identify locations needing updates
  - [ ] 1.4 Update all internal function calls from `mvgam_single_dataset()` to `mvgam_single()` using Edit tool (may be in `mvgam()` function)
  - [ ] 1.5 Update roxygen2 documentation for renamed function, ensuring `@param` and `@return` sections are accurate
  - [ ] 1.6 Read `create_mvgam_from_combined_fit()` function in `R/mvgam_core.R` to understand mvgam object structure creation
  - [ ] 1.7 Verify that `create_mvgam_from_combined_fit()` returns proper object structure with all required components listed in TRD FR2 (obs_fit, trend_fit, combined_fit, formula, etc.)
  - [ ] 1.8 Read `mvgam_multiple()` function in `R/mvgam_core.R` to understand multiple imputation workflow and verify it calls appropriate helper functions
  - [ ] 1.9 Create `tests/local/test-models-single.R` file with basic structure including `source("setup_tests_local.R")` header
  - [ ] 1.10 Write test helper function `create_simple_gaussian_ar_model()` in test file using `sim_mvgam(family = gaussian(), T = 60, trend_model = 'AR1')` pattern from `tests-models1.R`
  - [ ] 1.11 Write first test "mvgam_single fits gaussian AR model correctly" that creates model and checks object has class `c("mvgam", "brmsfit")`
  - [ ] 1.12 Add assertions to test checking for required object components: `model_output`, `formula`, `trend_formula`, `family` (using `expect_true(!is.null(...))`)
  - [ ] 1.13 Run test using `Rscript -e "devtools::load_all();testthat::test_file('tests/local/test-models-single.R')"` and verify it passes
  - [ ] 1.14 Add test "mvgam_single handles poisson family correctly" using `sim_mvgam(family = poisson())` and verify model fits
  - [ ] 1.15 Run updated tests and ensure no errors or warnings appear

- [ ] **2.0 Basic Print Method with Tests**
  - [ ] 2.1 Read existing `R/print.mvgam.R` to understand current implementation and helper functions that may be reusable
  - [ ] 2.2 Read brms print method documentation to understand expected output format and sections (family, link, formula, data, draws, parameters)
  - [ ] 2.3 Create helper function `extract_family_name.mvgam()` in `R/print.mvgam.R` to extract family name from mvgam object
  - [ ] 2.4 Create helper function `extract_family_link.mvgam()` in `R/print.mvgam.R` to extract link function from mvgam object
  - [ ] 2.5 Create helper function `extract_data_name.mvgam()` in `R/print.mvgam.R` to extract data object name from mvgam object
  - [ ] 2.6 Create helper function `format_sampling_info()` in `R/print.mvgam.R` to format MCMC sampling details (chains, iter, warmup, thin)
  - [ ] 2.7 Create helper function `extract_parameter_summaries()` in `R/print.mvgam.R` to get mean and sd for parameters without CIs or diagnostics
  - [ ] 2.8 Create helper function `format_parameter_table()` in `R/print.mvgam.R` to format parameter estimates as printable table (Estimate, Est.Error columns only)
  - [ ] 2.9 Replace `print.mvgam()` function body to print family/link using new helper functions
  - [ ] 2.10 Add formula printing section to `print.mvgam()` for both observation and trend formulas
  - [ ] 2.11 Add parameter table printing section for population-level effects using `format_parameter_table()`
  - [ ] 2.12 Add parameter table printing section for trend parameters (if trend_formula present) using `format_parameter_table()`
  - [ ] 2.13 Add parameter table printing section for family-specific parameters (sigma, shape, etc.) using `format_parameter_table()`
  - [ ] 2.14 Add roxygen2 documentation to `print.mvgam()` following TRD Section 10 documentation template with `@param`, `@return`, `@details`, `@seealso`, `@examples`
  - [ ] 2.15 Write test "print.mvgam produces correct output structure" in `test-models-single.R` that captures output and checks for key sections (use `capture.output()` and `grepl()`)
  - [ ] 2.16 Verify test checks output contains "Family:", "Formula:", "Population-Level Effects:" and does NOT contain "CI" or "Rhat"
  - [ ] 2.17 Run tests with `Rscript -e "devtools::load_all();testthat::test_file('tests/local/test-models-single.R')"` and fix any failures

- [ ] **3.0 Comprehensive Summary Method with Tests**
  - [ ] 3.1 Create new file `R/summary.mvgam.R` with roxygen2 header and checkmate imports
  - [ ] 3.2 Read brms summary method to understand mvgam_summary structure and expected columns (Estimate, Est.Error, l-CI, u-CI, Rhat, Bulk_ESS, Tail_ESS)
  - [ ] 3.3 Create S3 class definition comment for mvgam_summary structure following TRD Section 3.5 specification
  - [ ] 3.4 Create helper function `extract_observation_summaries()` in `R/summary.mvgam.R` to extract observation model parameters from summarise_draws output
  - [ ] 3.5 Create helper function `extract_trend_summaries()` in `R/summary.mvgam.R` to extract trend model parameters from summarise_draws output
  - [ ] 3.6 Create helper function `compute_summary_stats()` in `R/summary.mvgam.R` using `posterior::summarise_draws()` with mean, sd, quantile, and convergence measures
  - [ ] 3.7 Implement `summary.mvgam()` function signature with parameters: object, priors = FALSE, prob = 0.95, robust = FALSE, mc_se = FALSE
  - [ ] 3.8 Add input validation to `summary.mvgam()` using checkmate assertions for all parameters
  - [ ] 3.9 Add code to extract posterior draws using `posterior::as_draws_df()` from object$model_output
  - [ ] 3.10 Add code to compute summaries via `posterior::summarise_draws()` with appropriate quantiles based on prob argument
  - [ ] 3.11 Add code to organize summaries by parameter category (observation vs trend) using helper functions
  - [ ] 3.12 Create mvgam_summary list structure with all components from TRD Section 3.5 and assign class "mvgam_summary"
  - [ ] 3.13 Create `print.mvgam_summary()` function that prints model specification and parameter tables with diagnostics
  - [ ] 3.14 Add roxygen2 documentation to `summary.mvgam()` following TRD Section 10 template with detailed `@details` explaining columns
  - [ ] 3.15 Write test "summary.mvgam includes MCMC diagnostics" in `test-models-single.R` that checks for Rhat, Bulk_ESS, Tail_ESS columns in output
  - [ ] 3.16 Write test "summary.mvgam respects prob argument" that verifies 90% and 99% intervals are computed correctly
  - [ ] 3.17 Run tests and verify summary output includes credible intervals and diagnostics

- [ ] **4.0 Convergence Warnings and Diagnostic Integration**
  - [ ] 4.1 Create new file `R/diagnostics.mvgam.R` with roxygen2 header
  - [ ] 4.2 Create `rhat.mvgam()` function that delegates to `rhat(object$model_output, pars = pars, ...)` following TRD Section 3.7
  - [ ] 4.3 Create `neff_ratio.mvgam()` function that delegates to `neff_ratio(object$model_output, pars = pars, ...)` following TRD Section 3.7
  - [ ] 4.4 Create `nuts_params.mvgam()` function that delegates to `nuts_params(object$model_output, pars = pars, ...)` for NUTS diagnostics
  - [ ] 4.5 Create `log_posterior.mvgam()` function that delegates to `log_posterior(object$model_output, ...)` for diagnostic plotting
  - [ ] 4.6 Add roxygen2 documentation to all diagnostic functions with `@param`, `@return`, and `@seealso` tags
  - [ ] 4.7 Create helper function `check_convergence()` in `R/diagnostics.mvgam.R` that checks Rhat > 1.05 and issues warnings with remediation suggestions
  - [ ] 4.8 Add ESS checking to `check_convergence()` that warns if any neff_ratio < 0.001 (equivalent to ESS < 400 for typical setup)
  - [ ] 4.9 Add divergent transition checking to `check_convergence()` that extracts nuts_params and counts divergences if NUTS algorithm used
  - [ ] 4.10 Add treedepth saturation checking to `check_convergence()` that warns if transitions hit max_treedepth limit
  - [ ] 4.11 Call `check_convergence()` at end of `summary.mvgam()` before returning summary object
  - [ ] 4.12 Write test "rhat.mvgam returns valid convergence diagnostics" in `test-models-single.R` that checks rhat values are numeric and > 0
  - [ ] 4.13 Write test "convergence warnings trigger appropriately" that fits poorly converged model (iter = 50) and expects warning matching "not converged|Rhat"
  - [ ] 4.14 Run tests and verify diagnostic functions work correctly
  - [ ] 4.15 Update `print.mvgam_summary()` to include diagnostic footer explaining Rhat and ESS following TRD Section 3.5 output format

- [ ] **5.0 Multiple Imputation Summary with Tests**
  - [ ] 5.1 Create `tests/local/test-models-multiple.R` file with `source("setup_tests_local.R")` header
  - [ ] 5.2 Create test data for MI using `sim_mvgam(prop_missing = 0.2)` then `mice::mice(data, m = 5, printFlag = FALSE)` pattern
  - [ ] 5.3 Write test helper function that creates list of 5 imputed datasets using `lapply(1:5, function(i) mice::complete(imp, i))`
  - [ ] 5.4 Write test "mvgam_multiple pools MI results correctly" that fits model with `combine = TRUE` and checks for mvgam_pooled class
  - [ ] 5.5 Add assertions checking pooled object has `pooled_estimates` component with observation and trend sub-lists
  - [ ] 5.6 Add assertions checking `attr(object, "individual_fits")` exists and has length equal to n_imputations
  - [ ] 5.7 Run initial MI test to verify mvgam_multiple is working before implementing summary methods
  - [ ] 5.8 Create helper function `extract_mi_diagnostics()` in `R/summary.mvgam.R` to extract Rubin's rules diagnostics from pooled object
  - [ ] 5.9 Create `summary.mvgam_pooled()` function in `R/summary.mvgam.R` that calls `NextMethod()` to get base summary then adds MI diagnostics
  - [ ] 5.10 Add MI-specific diagnostics to summary structure: n_imputations, avg_relative_increase, avg_fmi, avg_df
  - [ ] 5.11 Set class of pooled summary to `c("mvgam_pooled_summary", "mvgam_summary")`
  - [ ] 5.12 Create `print.mvgam_pooled_summary()` function that calls `NextMethod()` then adds MI diagnostics section
  - [ ] 5.13 Add roxygen2 documentation for `summary.mvgam_pooled()` following TRD Section 3.6 specification
  - [ ] 5.14 Write test "summary.mvgam_pooled shows MI diagnostics" in `test-models-multiple.R` that checks output contains "Multiple Imputation Diagnostics"
  - [ ] 5.15 Run MI tests and verify pooled summaries display correctly

- [ ] **6.0 Parameter Discovery and Additional Tests**
  - [ ] 6.1 Check if `R/variables.mvgam.R` file exists; if not, check `R/index-mvgam.R` for existing `variables.mvgam()` function
  - [ ] 6.2 Read existing `variables.mvgam()` implementation to understand current functionality
  - [ ] 6.3 Implement simple Phase 1 version of `variables.mvgam()` that delegates to `variables(x$model_output, ...)` following TRD Section 3.3
  - [ ] 6.4 Add roxygen2 documentation to `variables.mvgam()` following TRD Section 10 template explaining parameter naming conventions
  - [ ] 6.5 Write test "variables.mvgam returns all parameter names" in `test-models-single.R` that checks return type is character vector
  - [ ] 6.6 Write test "variables.mvgam includes trend parameters" that checks output contains parameters with "_trend" suffix
  - [ ] 6.7 Create test for multivariate model in `test-models-single.R` using `mvbind(count, biomass) ~ temp` pattern from `target_generation.R`
  - [ ] 6.8 Add assertions to multivariate test checking `response_names` attribute and parameter organization
  - [ ] 6.9 Create test for factor model using `AR(p = 1, n_lv = 2, cor = TRUE)` pattern from `target_generation.R` Target 4
  - [ ] 6.10 Add assertions to factor model test verifying Z matrix parameters appear in variables output
  - [ ] 6.11 Create test for CAR model using pattern from `target_generation.R` Target 6 with irregular time intervals
  - [ ] 6.12 Create test for PW model using pattern from `target_generation.R` Target 5 with changepoints
  - [ ] 6.13 Run all tests in `test-models-single.R` and verify comprehensive coverage of trend types
  - [ ] 6.14 Add test for VAR model with `VAR(p = 2, ma = TRUE)` pattern from `target_generation.R` Target 3
  - [ ] 6.15 Ensure all tests check both print and summary methods work correctly for each model type

- [ ] **7.0 Final Documentation and Integration Testing**
  - [ ] 7.1 Run `devtools::document()` to generate all roxygen2 documentation and update NAMESPACE
  - [ ] 7.2 Check `DESCRIPTION` file to verify `posterior` package is listed in Imports section; add if missing
  - [ ] 7.3 Read all generated `.Rd` files in `man/` directory to verify documentation renders correctly
  - [ ] 7.4 Run `devtools::check()` and review any documentation warnings or errors
  - [ ] 7.5 Test all `@examples` sections manually to ensure they run without errors
  - [ ] 7.6 Run complete test suite: `Rscript -e "devtools::load_all();testthat::test_file('tests/local/test-models-single.R')"`
  - [ ] 7.7 Run MI test suite: `Rscript -e "devtools::load_all();testthat::test_file('tests/local/test-models-multiple.R')"`
  - [ ] 7.8 Create integration test that fits model, calls print, summary, variables, rhat, and neff_ratio in sequence
  - [ ] 7.9 Verify integration test output matches brms conventions and expected format from TRD
  - [ ] 7.10 Test backend compatibility by fitting same model with both cmdstanr and rstan backends, verify identical summary output
  - [ ] 7.11 Add test checking `brms::read_csv_as_stanfit()` conversion is working correctly for cmdstanr models
  - [ ] 7.12 Review all helper functions to ensure they have `@noRd` tags and clear internal documentation
  - [ ] 7.13 Run final `devtools::check()` to verify package passes all checks with no errors, warnings, or notes
  - [ ] 7.14 Test multivariate print/summary output manually to verify response-specific parameters display correctly
  - [ ] 7.15 Document any remaining open issues or future enhancements in task completion notes

---

## Implementation Notes

### Key Design Decisions
1. **Replace not extend**: Both `print.mvgam()` and `summary.mvgam()` need complete replacement
2. **Test as you go**: Add tests immediately after implementing each method
3. **brms conventions**: Use brms exported functionality wherever possible (e.g., `brms::read_csv_as_stanfit`)
4. **Equal priority**: Treat MI support as equal priority, not deferred

### Testing Strategy
- Build off `target_generation.R` patterns for diverse test models
- Use `sim_mvgam()` for generating test data
- Two main test files: `test-models-single.R` and `test-models-multiple.R`
- Run tests after each major implementation step
- No `try()`, `tryCatch()`, or `skip()` within tests

### Helper Function Organization
- Formatting helpers in same file as method that uses them
- Extraction helpers in file corresponding to component
- All helpers marked `@noRd` with clear comments

### Common Pitfalls to Avoid
- Don't use `info` argument in testthat expectations (doesn't exist)
- Don't create wrapper functions to fix failing tests
- Don't insert placeholder functions with `try()` to address errors
- Always use `checkmate::assert_*()` for input validation
- Always use `insight::format_error()` for user-facing error messages
