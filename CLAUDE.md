# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

mvgam is an R package for fitting Multivariate Dynamic Generalized Additive Models.The package enables Bayesian forecasting and analysis of multivariate time series data using flexible GAM frameworks. It can handle various data types (counts, proportions, continuous values) with complex temporal dynamics, missing data, and seasonality, building custom Stan models that provide robust Bayesian inference.

## Development Commands

### Testing
- `R CMD check` - Full package check (used in CI)
- `testthat::test_check("mvgam")` - Run all tests via testthat
- `devtools::test()` - Run tests interactively during development

### Building and Documentation
- `devtools::document()` - Generate documentation from roxygen2 comments
- `pkgdown::build_site()` - Build package website
- `devtools::build()` - Build package tarball
- `devtools::install()` - Install package locally for development

### Package Structure
- Uses standard R package structure with DESCRIPTION, NAMESPACE, and man/ directories
- Source code organized in `R/` directory with provider-specific files
- Vignettes in vignettes/ directory demonstrate key features
- Tests in `tests/testthat/`

## Architecture

### Key Design Patterns

**S3 Type System**: Uses S3 for structured objects
- Maintains compatibility with R's statistical modeling ecosystem
- Supports method inheritance and specialization

**Layered Architecture Pattern**: Uses clear separation of concerns across multiple layers:
- Interface Layer: User-facing functions (mvgam(), forecast(), plot()) provide clean APIs
- Model Specification Layer: Formula processing, trend model constructors (RW(), VAR(), GP()), family definitions
- Code Generation Layer: Translates R specifications into Stan/JAGS model code
- Computational Backend Layer: Interfaces with Stan/JAGS for MCMC sampling
- Post-processing Layer: Methods for analysis, diagnostics, and visualization

**Modular Component System**: Modular design where different components can be mixed and matched:
- Trend Modules: Independent implementations of different temporal dynamics (Random Walk, AR, VAR, Gaussian Process, CAR)
- Family Modules: Separate observation model implementations for different distributions
- Backend Modules: Pluggable computational backends (Stan via rstan/cmdstanr, JAGS)
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
  - Validates and processes GAM formulas for observation and trend processes
  - Sets up Stan/JAGS model code generation
  - Runs MCMC sampling and returns fitted model objects

- Trend model constructors in `R/mvgam_trend_types.R` (`RW()`, `AR()`, `VAR()`, `GP()`, `CAR()`):
  - Define temporal dynamics specifications
  - Configure stationarity constraints and correlation structures

### Prediction & Forecasting
- `R/forecast.mvgam.R` - Generates in-sample and out-of-sample forecasts:
  - Respects temporal dynamics for proper time series forecasting
  - Supports multiple prediction types (response, trend, link)
  - Returns structured forecast objects with uncertainty quantification

- `R/predict.mvgam.R` - General prediction treating trends as random effects
  
### Visualization Suite
- `R/plot.mvgam.R` - Main plotting method with multiple types:
  - Series plots, residual diagnostics, smooth functions, forecasts
  - Calls specialized functions: `plot_mvgam_forecasts()`, `plot_mvgam_series()`, `plot_mvgam_trend()`

### Model Analysis
- `R/summary.mvgam.R` - Parameter estimates and convergence diagnostics
- `R/ppc.mvgam.R` - Posterior predictive checks using bayesplot
- `R/residuals.mvgam.R` - Dunn-Smyth residuals for model checking
- `R/loo.mvgam.R` - Approximate leave-one-out cross-validation

### Family Support
- Extensive distribution families in `R/families.R`:
  - Standard: gaussian, poisson, binomial, Gamma
  - Extended: negative binomial, beta, Student-t, Tweedie
  - Special: N-mixture models for imperfect detection
  
### Testing and Quality
- `tests/testthat/` - Test suite
- `vignettes/` - Documentation and examples
- `.github/workflows/` - CI/CD with R CMD check, pkgdown building and valgrind check

## Development Notes

### Testing Strategy
- Separate test files for each major component
- Prioritize internal mvgam objects (i.e. `mvgam:::mvgam_example1`) for testing

### File Management
- Specification documents (`*-requirements.md`, `*-design.md`, `*-implementation.md`) should be automatically added to `.Rbuildignore`
- Any temporary development files should be excluded from package builds
- When creating new specification files, always update `.Rbuildignore` to prevent inclusion in built package

### Code Organization
- Provider files should follow consistent naming pattern
- Utility functions should be grouped by purpose (`utils-*.R`)
- Standalone imports should minimize external dependencies

### Documentation
- Roxygen2 comments for all exported functions
- tidyverse styling (https://style.tidyverse.org/) for all R and roxygen code
- Vignettes demonstrate in-depth use cases
- pkgdown site provides comprehensive documentation
- Examples demonstrate simpler use cases
