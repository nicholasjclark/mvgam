## Package Overview

mvgam is an R package for fitting, plotting and interpreting Bayesian Multivariate State-Space Models.

## Development Commands

### Testing
- YOU MUST load all functions (`devtools::load_all()`) prior to testing or debugging. Use `Rscript -e "devtools::load_all();testthat::test_file(path/to/test)"` to run specific tests
- You CANNOT use filter calls in `testthat::test_file()` for selective execution (use the `desc` argument for this instead)
- No test errors or warnings are allowed. This is NON-NEGOTIABLE

### Building and Documentation
- `Rscript -e "devtools::document()"` - Generate roxygen2 documentation
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
- Model Building: Formula specification, priors, trend model selection, observation family
- Fitting: MCMC sampling with convergence monitoring
- Checking: Posterior predictive checks, residual analysis, diagnostic plots
- Inference: Parameter summaries, uncertainty quantification
- Prediction: Forecasting with proper uncertainty propagation
- Evaluation: Cross-validation, scoring rules, model comparison
  
## Continuous Integration
- `.github/workflows/` - CI/CD with R CMD check, pkgdown building and valgrind memory check

## Development Notes

### Testing Strategy
- After updating any logic, check whether existing unit tests need to be updated. If so, update them
- When proposing new tests, first check existing test file names and add to the most appropriate existing file
- DO NOT write wrapper functions to fix failing tests
- DO NOT use `try()`, `tryCatch()` or `skip()` within tests
- DO NOT insert placeholder functions or use `try()` / `tryCatch()` within functions to address test errors or warnings

### testthat Expectations Guidelines
- testthat expectation functions (expect_true, expect_equal, etc.) do NOT have an `info` argument
- Use descriptive variable names and clear assertions
- NEVER use `info = "..."` in any expect_* function calls - add comments above the expectation instead

### Documentation
- Use roxygen2 tags for all functions
- Vignettes demonstrate in-depth use cases
- pkgdown site provides comprehensive documentation
- README provides a high level overview of package goals and major functions
- roxygen2 examples demonstrate simpler use cases

### Code Style and Formatting
- Apply tidyverse styling for all R and roxygen code
- **Line Length**: Maximum 80 characters per line
- **Roxygen Documentation**: Use 2-space indentation for continuation lines
- When commenting complex logic, add above line `# Reason` comment explaining the why, not just the what
- **Comment Style**: Use sentence case, avoid ALL CAPS in comments
- **Comment Quality**: Keep comments professional and interpretable in isolation. **NEVER use meaningless descriptive words like "comprehensive", "unified", "enhanced", "robust", "simplified", "complete", "advanced", "improved", "optimized", "efficient", "powerful", "flexible", or "intelligent" in comments, function descriptions, or variable names**. These words provide no useful information. Describe WHAT the code does and WHY, not how good it supposedly is
- **Function Names**: Use snake_case

### Validation and Error Handling
- All R functions must follow these validation patterns:

#### Validation Patterns
1. **Input Validation**: Use `checkmate::assert_*()` for all function parameters
2. **Error Messages**: Use `insight::format_error()` for user-friendly error formatting
3. **Warnings**: Use `insight::format_warning()` for informative warnings
4. **Session Warnings**: Use `if (!identical(Sys.getenv("TESTTHAT"), "true")) rlang::warn(..., .frequency = "once")` for one-time warnings

#### Message Formatting Standards
- Use `{.field parameter_name}` for parameter highlighting
- Include suggested solutions in error messages
- Provide context about why constraints exist

### Export Guidelines
- Only export functions that users directly need
- Keep validation and utility functions internal (`@noRd`)

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
- Do not mention Claude as a co-author or include links to Claude Code in messages

### Pre-commit Workflow
1. Check git status to understand current changes
2. Stage appropriate files with clear understanding of what's being committed
3. Write commit messages explaining changes

### AI Behavior Rules
- **Never assume missing context. Ask questions if uncertain**
- **Never hallucinate libraries or functions** – only use known, verified R packages
