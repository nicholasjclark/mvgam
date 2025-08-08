# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

mvgam is an R package for fitting, plotting and interpreting Bayesian Multivariate State-Space Models.

## Development Commands

### Testing
- YOU MUST load all functions prior to testing. Use `Rscript -e "devtools::load_all();testthat::test_file(path/to/test)"` to run specific tests during development

### Building and Documentation
- `Rscript -e "devtools::document()"` - Generate roxygen2 documentation
- `Rscript -e "pkgdown::build_site()"` - Build package website
- `R CMD INSTALL --preclean --no-multiarch` - Install package locally

### Documentation Memory
- Use `Ref` and `context7` MCP servers to find relevant, up-to-date documentation when working with 3rd party libraries, as needed

## Architecture

### Package Structure
- Standard R package structure with `DESCRIPTION`, `NAMESPACE`, `src/` and `man/` directories
- R source code in `R/` directory
- Rcpp source code in `src/` directory
- Vignettes in `vignettes/` directory demonstrate key features
- Tests in `tests/testthat/`

### Key Design Patterns

**S3 Type System**: Uses S3 for structured objects to support method inheritance and specialization

**Layered Architecture Pattern**:
- Interface Layer: User-facing functions provide clean APIs
- Model Specification Layer: Formula processing with special trend model constructors
- Code Generation Layer: Translates R specifications into brms code to build initial Stan models and data, then extends these with custom trend and factor additions
- Computational Backend Layer: Interfaces with Stan for sampling
- Post-processing Layer: S3 methods for analysis, diagnostics, and visualization

**Bayesian Workflow Integration**:
- Model Building: Formula specification, prior setup, trend model selection, observation family
- Fitting: MCMC sampling with convergence monitoring
- Checking: Posterior predictive checks, residual analysis, diagnostic plots
- Inference: Parameter summarization, uncertainty quantification
- Prediction: Forecasting with proper uncertainty propagation
- Evaluation: Cross-validation, scoring rules, model comparison
  
## Continuous Integration
- `.github/workflows/` - CI/CD with R CMD check, pkgdown building and valgrind memory check

## Development Notes

### Testing Strategy
- Please prioritize test-driven development
- **Test File Organization**: Always add new tests to existing test files rather than creating separate files, unless absolutely necessary for clarity
- When proposing new tests, first check existing test file names and add to the most appropriate existing file
- Prioritize internal objects (i.e. `mvgam:::mvgam_example1`) for testing, where appropriate
- DO NOT write replacement functions just so tests can pass. Please update existing functions appropriately in light of any test warnings or failures
- DO NOT use `try()` or `tryCatch()` and `skip()` within tests
- DO NOT insert placeholder functions or use `try()` / `tryCatch()` within functions to mask errors or warnings

### Code Organization
- Provider files should follow consistent naming patterns
- Utility functions should be grouped by purpose (`utils-*.R`)

### Documentation
- Roxygen2 comments for all functions
- Vignettes demonstrate in-depth use cases
- pkgdown site provides comprehensive documentation
- README provides a high level overview of package goals and major functions
- roxygen2 examples demonstrate simpler use cases

### Code Style and Formatting
- Apply tidyverse styling (https://style.tidyverse.org/) for all R and roxygen code
- **Line Length**: Maximum 80 characters per line for readability
- **Roxygen Documentation**: Use 2-space indentation for continuation lines
- **Comment Style**: Use sentence case, avoid ALL CAPS in comments
- **Function Names**: Use snake_case

### Validation and Error Handling
- All R functions must follow these validation patterns:

#### Validation Patterns
1. **Input Validation**: Use `checkmate::assert_*()` for all function parameters
2. **Error Messages**: Use `insight::format_error()` for user-friendly error formatting
3. **Warnings**: Use `insight::format_warning()` for informative warnings
4. **Session Warnings**: Use `rlang::warn(..., .frequency = "once")` for one-time warnings

#### Message Formatting Standards
- Use `{.field parameter_name}` for parameter highlighting
- Include suggested solutions in error messages
- Provide context about why constraints exist
- Use consistent terminology

### Export Guidelines
- Only export functions that users directly need
- Keep validation and utility functions internal (`@noRd`)
- Use clear, simple descriptions without excessive technical references

### .Rbuildignore Management
- Update `.Rbuildignore` immediately when creating new spec files:
  - `*-requirements.md`, `*-design.md`, `*-implementation.md`
  - `*-plan.md` files
  - Any temporary development documentation files

## Git Workflow

### Branch Management
- Use descriptive feature branch names: `feature/brms-integration`
- Create branches from master before starting major work
- Keep feature branches focused on specific functionality
- Push branches to remote for backup and collaboration
- Use `gh` client for all Github interactions

### Commit Message Standards
- Follow this pattern for all commits:
```
Brief description of changes (50 chars max)

- Detailed explanation of what was changed
- Why the change was necessary  
- Any important implementation notes
```
- Please do not mention Claude as a co-author or include links to Claude Code in messages

### Pre-commit Workflow
1. Check git status to understand current changes
2. Stage appropriate files with clear understanding of what's being committed
3. Write commit messages explaining changes
