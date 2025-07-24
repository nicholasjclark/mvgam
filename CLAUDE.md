# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

mvgam is an R package for fitting Multivariate Dynamic Generalized Additive Models.The package enables Bayesian analysis of multivariate data using flexible GAM frameworks. It can handle various data types (counts, proportions, continuous values) with complex temporal or spatial dynamics, missing data, and seasonality, building custom Stan models that provide robust Bayesian inference.

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
- Use `gh` client for all Github interactions

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
- Apply tidyverse styling (https://style.tidyverse.org/) for all R and roxygen code
- Vignettes demonstrate in-depth use cases
- pkgdown site provides comprehensive documentation
- Examples demonstrate simpler use cases

## Package-Specific Development Standards

### Code Style and Formatting
- **Line Length**: Maximum 80 characters per line for readability
- **Roxygen Documentation**: Use 2-space indentation for continuation lines
- **Comment Style**: Use sentence case, avoid ALL CAPS in comments
- **Function Names**: Use snake_case following tidyverse conventions
- **Consistency**: Follow existing package conventions where established

### Validation and Error Handling
All mvgam functions must follow these validation patterns:

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

#### Dynamic Factor Model Constraints
Special validation for identifiability constraints:
- `n_lv > 0` cannot be combined with `gr != 'NA'` or `subgr != 'series'`
- `n_lv > 0` cannot be combined with `ma = TRUE`
- Only certain trend types support `n_lv > 0` (checked via `characteristics$supports_factors`)
- Variance parameters fixed for dynamic factor models (one-time warning)

### Export Guidelines
- Only export functions that users directly need (trend constructors, methods)
- Keep validation and utility functions internal (`@noRd`)
- Export extension points for users (e.g., `custom_trend()`)
- Use clear, simple descriptions without excessive technical references

## Project Management Integration

### TodoWrite Usage for Multi-Phase Projects
Use TodoWrite tool proactively for:

#### When to Use TodoWrite
- Complex multi-step tasks requiring 3+ distinct steps
- Major refactoring projects with multiple phases
- When user provides multiple tasks or requirements
- Before starting work on complex implementations
- After completing tasks to track progress

#### TodoWrite Best Practices
- Create specific, actionable todo items
- Break complex tasks into manageable steps
- Use clear, descriptive task names
- Update status in real-time during work
- Mark tasks complete immediately after finishing
- Only have ONE task in_progress at any time

#### Task States and Management
- `pending`: Task not yet started
- `in_progress`: Currently working on (limit to ONE task at a time)
- `completed`: Task finished successfully

## Advanced Git Workflow

### Branch Management
- Use descriptive feature branch names: `feature/brms-integration`
- Create branches from master before starting major work
- Keep feature branches focused on specific functionality
- Push branches to remote for backup and collaboration

### Commit Message Standards
Follow this pattern for all commits:
```
Brief description of changes (50 chars max)

- Detailed explanation of what was changed
- Why the change was necessary  
- Any important implementation notes

🤖 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com>
```

### .Rbuildignore Management
- Automatically add specification documents to `.Rbuildignore`:
  - `*-requirements.md`, `*-design.md`, `*-implementation.md`
  - `*-plan.md` files
  - Any temporary development documentation
- Update `.Rbuildignore` immediately when creating new spec files

### Pre-commit Workflow
1. Check git status to understand current changes
2. Stage appropriate files with clear understanding of what's being committed
3. Write descriptive commit messages explaining the changes
4. Push to remote branch for backup

## R Package Refactoring Best Practices

### Managing Complex Architectural Migrations
When performing major refactoring like the brms integration:

#### Backwards Compatibility Strategy
- Maintain parallel implementations during transition
- Use feature flags to enable new vs. old implementations
- Deprecate old functionality with clear migration path
- Provide detailed upgrade documentation

#### Dual Architecture Management
- Create clear interfaces between old and new systems
- Use wrapper functions to maintain existing user interfaces
- Implement systematic testing to ensure equivalent functionality
- Plan staged rollout with alpha/beta testing phases

#### Systematic Feature Implementation
- Follow established implementation timeline (e.g., 16-week plan)
- Implement foundational components before dependent features
- Validate each component before proceeding to next phase
- Maintain comprehensive testing throughout migration

#### Risk Mitigation
- Create rollback plans for each phase
- Maintain working implementations at each milestone
- Use extensive testing to catch regressions early
- Document all architectural decisions and constraints

### Complex Project Context Management
For extended development projects:

#### Documentation Standards
- Maintain comprehensive project plans (e.g., `mvgam-brms-refactoring-plan.md`)
- Document architectural decisions and constraints
- Keep implementation notes and design rationale
- Update progress and status regularly

#### Code Organization During Refactoring
- Group related functionality in logical file structures
- Use consistent naming conventions for new components
- Maintain clear separation between old and new implementations
- Document integration points and dependencies
