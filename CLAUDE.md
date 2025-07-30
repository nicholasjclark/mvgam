# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

mvgam is an R package for fitting Multivariate Dynamic Generalized Additive Models. The package enables Bayesian analysis of multivariate data using flexible State-Space models by enhancing the brms package. It can handle various data types (counts, proportions, continuous values) with complex temporal or spatial dynamics, missing data, and seasonality, building custom Stan models that provide robust Bayesian inference.

## Development Commands

### Testing
- Use `Rscript -e "devtools::load_all();testthat::test_file(path/to/test)"` to run specific tests during development

### Building and Documentation
- `Rscript -e "devtools::document()"` - Generate documentation from roxygen2 comments
- `Rscript -e "pkgdown::build_site()"` - Build package website
- `R CMD INSTALL --preclean --no-multiarch` - Install package locally

### Validating and Optimising Stan code
- Use the `Ref` MCP server if searching for documentation on the Stan modelling language
- Use the `context7` MCP server if searching for up-to-date Stan code examples

## Architecture

### Package Structure
- Uses standard R package structure with `DESCRIPTION`, `NAMESPACE`, `src/` and `man/` directories
- R source code organized in `R/` directory
- Exported Rcpp source code organized in `src/` directory
- Vignettes in `vignettes/` directory demonstrate key features
- Tests in `tests/testthat/`

### Key Design Patterns

**S3 Type System**: Uses S3 for structured objects
- Maintains compatibility with R's statistical modeling ecosystem
- Supports method inheritance and specialization

**Layered Architecture Pattern**: Uses clear separation of concerns across multiple layers:
- Interface Layer: User-facing functions (`mvgam()`, `forecast()`, `predict()`, `plot()`) provide clean APIs
- Model Specification Layer: Formula processing with special trend model constructors (`RW()`, `VAR()`, `CAR()`), family definitions
- Code Generation Layer: Translates R specifications into brms code to build initial Stan models and data, then extends these with custom trend and factor additions
- Computational Backend Layer: Interfaces with Stan for MCMC sampling
- Post-processing Layer: S3 methods for analysis, diagnostics, and visualization

**Modular Component System**: Modular design where different components can be mixed and matched:
- Trend Modules: Independent implementations of different temporal dynamics (Random Walk, AR, VAR, Gaussian Process, CAR) that can include brms-generated predictor effects
- Family Modules: Separate observation model implementations for different distributions, maintaining full brms flexibility
- Backend Modules: Pluggable computational backends (Stan via rstan or cmdstanr)
- Visualization Modules: Modular plotting system with specialized functions for different aspects

**Bayesian Workflow Integration Pattern**: Designed around the complete Bayesian modeling workflow:
- Model Building: Formula specification, prior setup, trend model selection
- Fitting: MCMC sampling with convergence monitoring
- Checking: Posterior predictive checks, residual analysis, diagnostic plots
- Inference: Parameter summarization, uncertainty quantification
- Prediction: Forecasting with proper uncertainty propagation
- Evaluation: Cross-validation, scoring rules, model comparison

## Key Files

### Core Model Functions
- `R/mvgam.R` - Main model fitting function that:
  - Validates and processes brms-compatible formulas for observation and trend processes
  - Sets up Stan model code generation
  - Runs MCMC sampling and returns fitted model objects

- Trend model constructors in `R/mvgam_trend_types.R` (`RW()`, `AR()`, `VAR()`, `CAR()`):
  - Define temporal dynamics specifications and point to forecasting cpp functions
  - Provide user support to easily add new trend types with higher dispatch
  
- Trend Stan code injection generators in `R/trend_injection_generators.R`:
  - Dispatch functions that generate Stan code for specified trend types
  
- Full Stan code generation in `R/stan_assembly.R` and `R/stan_code_generation.R`:
  - Two-stage Stan code assembly system with validation

### Prediction & Forecasting
- `R/forecast.mvgam.R` - Generates in-sample and out-of-sample forecasts:
  - Respects temporal dynamics for proper time series forecasting
  - Supports multiple prediction types (response, trend, link)
  - Returns structured forecast objects with uncertainty quantification

- `R/predict.mvgam.R` - General prediction treating trends as random effects

- `src/trend_funs.cpp` - Rcpp source code for trend forecasting
  
### Visualization Suite
- `R/plot.mvgam.R` - Main plotting method with multiple types:
  - Series plots, residual diagnostics, smooth functions, forecasts
  - Calls specialized functions: `plot_mvgam_forecasts()`, `plot_mvgam_series()`, `plot_mvgam_trend()`

### Model Analysis
- `R/summary.mvgam.R` - Parameter estimates and convergence diagnostics
- `R/ppc.mvgam.R` - Posterior predictive checks using bayesplot
- `R/residuals.mvgam.R` - Dunn-Smyth residuals for model checking
- `R/loo.mvgam.R` - Approximate leave-one-out cross-validation
  
### Testing and Quality
- `tests/testthat/` - Test suite
- `vignettes/` - Documentation and examples
- `.github/workflows/` - CI/CD with R CMD check, pkgdown building and valgrind check

## Development Notes

### Testing Strategy
- Separate test files for major components
- Prioritize internal mvgam objects (i.e. `mvgam:::mvgam_example1`) for testing

### Code Organization
- Provider files should follow consistent naming pattern
- Utility functions should be grouped by purpose (`utils-*.R`)
- Standalone imports should minimize external dependencies

### Documentation
- Roxygen2 comments for all exported functions
- Vignettes demonstrate in-depth use cases
- pkgdown site provides comprehensive documentation
- Examples demonstrate simpler use cases

### Code Style and Formatting
- Apply tidyverse styling (https://style.tidyverse.org/) for all R and roxygen code
- **Line Length**: Maximum 80 characters per line for readability
- **Roxygen Documentation**: Use 2-space indentation for continuation lines
- **Comment Style**: Use sentence case, avoid ALL CAPS in comments
- **Function Names**: Use snake_case following tidyverse conventions
- **Consistency**: Follow existing package conventions where established

### Validation and Error Handling
- All mvgam functions must follow these validation patterns:

#### Required Packages for Validation
- `checkmate`: Parameter validation and type checking
- `insight`: Error and warning message formatting  
- `rlang`: Session-wide warnings and advanced error handling

#### Validation Patterns
1. **Input Validation**: Use `checkmate::assert_*()` for all function parameters
2. **Error Messages**: Use `insight::format_error()` for user-friendly error formatting
3. **Warnings**: Use `insight::format_warning()` for informative warnings
4. **Session Warnings**: Use `rlang::warn(..., .frequency = "once")` for one-time warnings

#### Message Formatting Standards
- Use `{.field parameter_name}` for parameter highlighting
- Include suggested solutions in error messages
- Provide context about why constraints exist
- Use consistent terminology across the package

### Export Guidelines
- Only export functions that users directly need (trend constructors, methods)
- Keep validation and utility functions internal (`@noRd`)
- Use clear, simple descriptions without excessive technical references

## Git Workflow

### Branch Management
- Use descriptive feature branch names: `feature/brms-integration`
- Create branches from master before starting major work
- Keep feature branches focused on specific functionality
- Push branches to remote for backup and collaboration
- Use `gh` client for all Github interactions

### Commit Message Standards
Follow this pattern for all commits:
```
Brief description of changes (50 chars max)

- Detailed explanation of what was changed
- Why the change was necessary  
- Any important implementation notes
```

### .Rbuildignore Management
- Update `.Rbuildignore` immediately when creating new spec files
- Automatically add specification documents to `.Rbuildignore`:
  - `*-requirements.md`, `*-design.md`, `*-implementation.md`
  - `*-plan.md` files
  - Any temporary development documentation files

### Pre-commit Workflow
1. Check git status to understand current changes
2. Stage appropriate files with clear understanding of what's being committed
3. Write descriptive commit messages explaining the changes
4. Push to remote branch for backup
