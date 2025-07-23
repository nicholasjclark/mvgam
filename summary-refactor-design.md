# Design: Summary Refactor

## Approach
Transform the monolithic `summary.mvgam()` function into a modular system that creates a structured `mvgam_summary` S3 object with four logical components, using helper functions that can be reused across the package.

## Components

### `mvgam_summary` Object Structure
- Purpose: Container for all summary information with logical organization
- Interface: S3 object with class `c("mvgam_summary", "list")`
- Dependencies: None (pure data structure)

```r
structure(
  list(
    model_spec = list(...),      # Model specifications
    parameters = list(...),      # Parameter estimates  
    diagnostics = list(...),     # MCMC diagnostics
    sampling_info = list(...)    # Sampling details
  ),
  class = c("mvgam_summary", "list")
)
```

### Model Specification Extractor (`extract_model_spec()`)
- Purpose: Extract formulas, family, trend model, dimensions from mvgam object
- Interface: `extract_model_spec(object) -> list`
- Dependencies: Existing mvgam object structure

### Parameter Estimates Extractor (`extract_parameters()`)
- Purpose: Collect all parameter summaries (coefficients, family params, trend params)
- Interface: `extract_parameters(object, include_betas, smooth_test, digits, variational) -> list`
- Dependencies: `mcmc_summary()`, existing parameter extraction logic

### Diagnostics Extractor (`extract_diagnostics()`)
- Purpose: Gather MCMC diagnostics and convergence information
- Interface: `extract_diagnostics(object) -> list`
- Dependencies: `check_all_diagnostics()`, existing diagnostic functions

### Sampling Info Extractor (`extract_sampling_info()`)
- Purpose: Extract chains, iterations, algorithm details
- Interface: `extract_sampling_info(object) -> list`
- Dependencies: Existing sampling metadata from object

### Print Method (`print.mvgam_summary()`)
- Purpose: Reproduce current console output exactly using modular components
- Interface: Standard S3 print method
- Dependencies: All extractor functions, existing formatting logic

## Data Flow
1. `summary.mvgam()` calls four extractor functions
2. Extractor functions process mvgam object and return structured lists
3. Results combined into `mvgam_summary` object
4. `print.mvgam_summary()` uses same extractors to format output

## Error Handling
Use same error handling as current function - fail fast for malformed mvgam objects, graceful degradation for missing components.

## What We're Not Doing
- Not changing any printed output formatting
- Not adding new summary statistics
- Not modifying function parameters or signatures
- Not optimizing performance (maintainability first)

## Status
- Created: 2025-01-23
- Status: Draft
- Next Step: Implementation planning