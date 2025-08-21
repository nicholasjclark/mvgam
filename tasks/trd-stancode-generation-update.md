# Task Requirements Document: Stan Code Generation System Update

## 1. Task Overview

**Purpose:** Modernize mvgam's Stan code generation system to ensure correctness, efficiency, standardization, modularity, and user control over priors, while providing brms-equivalent inspection capabilities.

**Scope:** Complete refactoring of Stan code generation workflow covering:
- Stan code generation and assembly
- Stan data preparation and combination
- Prior specification and inspection system
- User-facing inspection functions (make_stancode, make_standata, get_prior, etc.)
- Temporal dynamics integration validation
- Modular architecture for future trend type development

**User Type:** R users with intermediate to advanced statistical modeling experience, but requiring no Stan knowledge.

## 2. User Journey

### Current State (Before Implementation)
Users cannot inspect generated Stan code, modify priors, or validate model specifications before fitting. The Stan generation system has modularity and correctness issues.

### Target State (After Implementation)
1. **Model Specification:** User specifies formula, trend_formula, data, and family
2. **Prior Inspection:** User calls `get_prior()` to see all available priors with defaults
3. **Prior Modification:** User modifies priors using `set_prior()`, `prior()`, or direct data frame manipulation
4. **Code/Data Inspection:** User optionally calls `make_stancode()` and `make_standata()` to preview generated code
5. **Model Fitting:** User calls `mvgam()` with custom priors, system uses improved Stan generation internally
6. **Post-Fit Inspection:** User calls `prior_summary()` to verify priors used in fitted model

### Decision Points
- **No trend model:** User omits trend_formula or sets to NULL → pure brms equivalent model
- **Default trend:** User sets trend_formula = ~ -1 or ~ 1 → ZMVN multivariate Gaussian latent states
- **Custom trend:** User specifies explicit trend constructors in trend_formula
- **Prior modification:** User can use brms-style `set_prior()` interface or direct data frame manipulation

## 3. Function Specifications

### 3.1 `get_prior()`
**Purpose:** Inspect all available priors for a specified model before fitting
**Parameters:**
- `formula` (required): Main observation model formula
- `trend_formula` (optional): Trend model specification, default NULL
- `data` (required): Data frame containing model variables
- `family` (optional): Response distribution family, default gaussian()
- `...` (optional): Additional arguments passed to brms setup

**Return Value:** 
- `mvgamprior` data frame with columns: `prior`, `class`, `coef`, `group`, `resp`, `dpar`, `nlpar`, `lb`, `ub`, `source`, `trend_component`
- `trend_component` column distinguishes observation vs trend parameters
- Compatible with brms `brmsprior` structure for cross-package consistency

**Side Effects:** None (read-only inspection)

### 3.2 `make_stancode()`
**Purpose:** Generate complete Stan model code before fitting
**Parameters:**
- `formula` (required): Main observation model formula
- `trend_formula` (optional): Trend model specification, default NULL
- `data` (required): Data frame containing model variables  
- `family` (optional): Response distribution family, default gaussian()
- `prior` (optional): Prior specifications from `get_prior()` or `set_prior()`
- `...` (optional): Additional arguments passed to mvgam setup

**Return Value:** 
- Character string containing complete Stan model code
- Includes both observation and trend components when trend_formula specified
- Code follows brms style conventions and Stan best practices

**Side Effects:** None (read-only generation)

### 3.3 `make_standata()`
**Purpose:** Generate complete Stan data list before fitting
**Parameters:** Same as `make_stancode()`

**Return Value:**
- Named list containing all data for Stan model
- Includes both observation and trend data components
- Structure matches what would be passed to Stan during fitting

**Side Effects:** None (read-only generation)

### 3.4 `set_prior()` and `prior()`
**Purpose:** Specify custom priors using brms interface patterns
**Parameters:** Follow brms `set_prior()` specification exactly, with extensions for trend-specific parameters

**Return Value:** Prior specification objects compatible with mvgam functions

**Side Effects:** None (specification only)

### 3.5 `prior_summary()`
**Purpose:** Inspect priors used in fitted or specified models
**Parameters:**
- `object` (required): Fitted mvgam object or model specification
- `all` (optional): Show all priors vs non-default only, default TRUE
- `...` (optional): Additional arguments

**Return Value:** Data frame showing priors actually used or planned to be used

**Side Effects:** None (read-only inspection)

### 3.6 `get_inits()`
**Purpose:** Generate or inspect initialization values for Stan parameters
**Parameters:**
- Model specification or fitted object
- Initialization strategy options

**Return Value:** List of initialization values for Stan parameters

**Side Effects:** None (generation only)

## 4. Functional Requirements

1. **The system must validate all formula and trend specifications early** to provide fast-fail feedback before Stan compilation
2. **The system must generate Stan code that exactly matches brms equivalents** when no trend_formula is specified
3. **The system must correctly integrate trend linear predictors into temporal dynamics models** without parameter naming conflicts
4. **The system must support all brms prior specification patterns** including `set_prior()`, `prior()`, and direct data frame manipulation
5. **The system must provide modular architecture** enabling easy addition of new trend types via convention-based dispatch
6. **The system must handle multivariate responses** with response-specific parameter naming (mu_count, mu_biomass, etc.)
7. **The system must respect the autocorrelation separation principle** preventing conflicting temporal models
8. **The system must optimize for sampling efficiency and memory usage** rather than compilation time
9. **The system must use standardized parameter naming conventions** with _trend suffixes to avoid brms conflicts
10. **The system must support factor models** when n_lv < n_series for compatible trend types
11. **The system must support hierarchical correlations** with group and subgroup structures
12. **The system must provide enhanced error messages** following CLAUDE.md formatting standards
13. **The system must work with both fitted and unfitted model specifications** for all inspection functions
14. **The system must maintain prior specification consistency** between inspection and fitting phases
15. **The system must auto-register trend types** using convention-based function discovery

## 5. Data Flow & Dependencies

### Primary Data Flow
```
User Input → mvgam() → parse_multivariate_trends() → setup_brms_lightweight() 
→ validate_time_series_for_trends() → extract_time_series_dimensions()
→ generate_combined_stancode() → extract_trend_stanvars_from_setup() 
→ fit_mvgam_model() → create_mvgam_from_combined_fit()
```

### Inspection Functions Data Flow
```
User Input → get_prior()/make_stan*() → setup_brms_lightweight() 
→ validate_time_series_for_trends() → generate_combined_stancode() 
→ return inspection results (no fitting)
```

### Dependencies
- **Internal:** brms (setup and code generation), existing mvgam validation system
- **External:** Stan ecosystem (rstan/cmdstanr), checkmate, insight
- **Architecture:** Dual-object brmsfit-like structure, stanvars combination system

### Critical Integration Points
- **brms Setup:** Use backend = "mock" for lightweight brms setup during inspection
- **Stanvars System:** All trend generators must return proper "stanvars" class objects
- **Parameter Monitoring:** Automatic discovery of trend parameters for Stan monitoring
- **Time Series Dimensions:** Single source of truth for n_time, n_series, n_obs calculations

## 6. User Interface Requirements

### Function Signatures
All inspection functions must accept the same core arguments as `mvgam()` for consistency:
```r
get_prior(formula, trend_formula = NULL, data, family = gaussian(), ...)
make_stancode(formula, trend_formula = NULL, data, family = gaussian(), prior = NULL, ...)
make_standata(formula, trend_formula = NULL, data, family = gaussian(), prior = NULL, ...)
```

### Parameter Naming Conventions
- Use brms-compatible argument names where possible
- `trend_formula` distinguishes from brms `formula` argument
- `prior` argument accepts output from `get_prior()` or `set_prior()`

### Default Behaviors
- `trend_formula = NULL` → pure brms model (no trend components)
- Missing `prior` argument → use context-aware defaults
- All functions work before model fitting

### Progress Indicators
- No progress indicators needed for inspection functions (should be fast)
- Validation errors should provide immediate feedback with suggested solutions

## 7. Error Handling & Validation

### Input Validation Requirements
1. **Formula Validation:** Check for valid R formula syntax and variable existence in data
2. **Trend Specification Validation:** Validate trend constructor parameters and compatibility
3. **Data Structure Validation:** Ensure required time/series variables exist and are properly formatted
4. **Prior Specification Validation:** Check for valid distribution names, parameter bounds, and compatibility

### Error Message Standards
Follow CLAUDE.md formatting with `insight::format_error()`:
- Use `{.field parameter_name}` for parameter highlighting
- Include suggested solutions in error messages
- Provide context about why constraints exist
- Example: "The {.field trend_formula} contains incompatible autocorrelation terms. State-Space trends cannot be combined with brms autocorr() terms. Consider using mvgam trend constructors instead."

### Validation Sequence
1. **Early Validation:** Check formulas, data structure, and basic compatibility during function calls
2. **Stan-Specific Validation:** Validate generated code compiles without errors
3. **Runtime Validation:** Let existing mvgam/brms systems handle data-specific validation

### Graceful Degradation
- If Stan code generation fails, provide helpful error message with model specification details
- If prior specification is invalid, suggest correct syntax with examples
- If data doesn't match structure requirements, point to data preparation guidance

## 8. Examples & Usage Patterns

### Basic Usage (No Trends)
```r
# Should generate identical Stan code to brms
get_prior(y ~ x + (1|group), data = dat, family = poisson())
stancode <- make_stancode(y ~ x + (1|group), data = dat, family = poisson())
# stancode should match: brm(y ~ x + (1|group), data = dat, family = poisson(), 
#                           backend = "mock")$stancode
```

### Realistic Workflow (With Trends)
```r
# 1. Inspect available priors
priors <- get_prior(count ~ treatment + s(time), 
                   trend_formula = ~ AR(p = 1, cor = TRUE),
                   data = ecology_data, 
                   family = poisson())

# 2. Modify trend-specific priors
priors$prior[priors$class == "ar1_trend"] <- "normal(0, 0.5)"
priors$prior[priors$class == "sigma_trend"] <- "exponential(2)"

# 3. Inspect generated code
stancode <- make_stancode(count ~ treatment + s(time), 
                         trend_formula = ~ AR(p = 1, cor = TRUE),
                         data = ecology_data, 
                         family = poisson(),
                         prior = priors)

# 4. Check data structure
standata <- make_standata(count ~ treatment + s(time), 
                         trend_formula = ~ AR(p = 1, cor = TRUE),
                         data = ecology_data, 
                         family = poisson(),
                         prior = priors)

# 5. Fit model with custom priors
model <- mvgam(count ~ treatment + s(time), 
               trend_formula = ~ AR(p = 1, cor = TRUE),
               data = ecology_data, 
               family = poisson(),
               prior = priors)

# 6. Verify priors used
prior_summary(model)
```

### Edge Cases
```r
# Multivariate model with different trends per response
priors <- get_prior(mvbind(count, biomass) ~ treatment, 
                   trend_formula = list(count = ~ AR(p = 1),
                                       biomass = ~ RW()),
                   data = ecology_data, 
                   family = c(poisson(), gaussian()))

# Factor model with hierarchical correlations
get_prior(count ~ treatment, 
          trend_formula = ~ AR(p = 1, cor = TRUE, gr = site, n_lv = 2),
          data = ecology_data, 
          family = poisson())
```

### Integration Examples
```r
# Using brms set_prior() interface
custom_priors <- set_prior("normal(0, 0.1)", class = "ar1_trend") +
                set_prior("exponential(1)", class = "sigma_trend")

model <- mvgam(count ~ treatment, 
               trend_formula = ~ AR(p = 1),
               data = ecology_data,
               prior = custom_priors)
```

## 9. Testing Requirements

### Unit Tests (Per Function)
- `get_prior()`: Test with various formula combinations, multivariate models, edge cases
- `make_stancode()`: Verify generated code compiles, matches brms when appropriate
- `make_standata()`: Check data structure correctness, validate required elements
- Prior specification functions: Test brms compatibility and custom trend parameters

### Integration Tests (Workflow Tests)
- Complete workflow from `get_prior()` → modify → `make_stan*()` → `mvgam()` → `prior_summary()`
- Cross-validation: results from inspection functions match fitted model behavior
- Multivariate model workflows with different trends per response

### Stan Code Validation Tests
- Generated Stan code compiles without errors for all trend types
- Generated code produces equivalent results to current system for existing models
- When no trends specified, generated code exactly matches brms equivalent

### Performance Considerations
- Inspection functions should complete in <1 second for typical models
- Memory usage should not exceed current system requirements
- Focus on sampling efficiency in generated Stan code rather than compilation speed

### Regression Tests
- Ensure updated system produces statistically equivalent results to current mvgam
- Verify brms ecosystem methods continue working with mvgam objects
- Test all existing trend types work with new system

## 10. Documentation Requirements

### roxygen2 Documentation
Each function requires:
- `@title` and `@description` explaining purpose and usage
- `@param` documentation for all parameters with type specifications
- `@return` describing exact return value structure
- `@examples` showing basic and advanced usage patterns
- `@seealso` cross-references to related functions
- `@export` tags for user-facing functions

### Function-Specific Documentation
```r
#' Inspect Available Priors for mvgam Models
#'
#' Returns a data frame of all prior specifications that can be applied 
#' to parameters in an mvgam model. Similar to \code{\link[brms]{get_prior}}
#' but extended for State-Space trend components.
#'
#' @param formula An object of class \code{formula} describing the observation model
#' @param trend_formula An optional formula describing trend dynamics. Default NULL
#'   results in pure brms equivalent model. See \code{\link{trend_formulae}} for syntax.
#' @param data A data frame containing the variables in the model
#' @param family A description of the response distribution and link function
#' @param ... Additional arguments passed to model setup
#'
#' @return A \code{mvgamprior} data frame with columns:
#'   \itemize{
#'     \item \code{prior}: Prior specification (empty for defaults)
#'     \item \code{class}: Parameter class (e.g., "b", "ar1_trend", "sigma_trend")
#'     \item \code{coef}: Specific coefficient name (if applicable)
#'     \item \code{resp}: Response variable name (for multivariate models)
#'     \item \code{source}: Source of prior ("default" or "user")
#'     \item \code{trend_component}: "observation" or "trend" to distinguish components
#'   }
#'
#' @examples
#' # Basic usage - no trends (equivalent to brms)
#' get_prior(y ~ x, data = dat, family = gaussian())
#'
#' # With trend specification
#' get_prior(count ~ treatment, 
#'           trend_formula = ~ AR(p = 1), 
#'           data = ecology_data, 
#'           family = poisson())
#'
#' @seealso \code{\link{make_stancode}}, \code{\link{set_prior}}, \code{\link{mvgam}}
#' @export
```

### README Examples
Update package README with workflow examples showing new inspection capabilities

### Vignette Updates
- Add section to existing vignettes demonstrating prior specification workflow
- Create focused vignette on "Advanced Prior Specification" if needed

### Internal Code Comments
- Complex Stan generation logic requires inline comments explaining approach
- Parameter transformation steps need documentation
- Integration points with brms require clear documentation

## 11. Implementation Notes for Developers

### Code Organization
```
R/
├── priors.R              # get_prior(), prior_summary(), set_prior() extensions
├── stancode.R            # make_stancode() implementation  
├── standata.R            # make_standata() implementation
├── stan_assembly.R       # Enhanced stanvar combination and injection logic
├── validations.R         # Enhanced validation with Stan-specific checks
└── trend_system.R        # Enhanced registry and dispatch system
```

### R Idioms to Use
- S3 method dispatch for `print.mvgamprior`, `summary.mvgamprior` methods
- Consistent use of `checkmate::assert_*()` for input validation
- `insight::format_error()` for user-friendly error messages
- Standard R formula processing with `terms()`, `all.vars()`, etc.

### R Idioms to Avoid
- Avoid `try()` or `tryCatch()` to hide errors - let validation catch issues early
- Don't use global state or package-level variables for Stan code generation
- Avoid hardcoded parameter names - use convention-based discovery

### Performance Considerations
- Cache brms setup results within function calls to avoid repeated computation
- Use lazy evaluation for expensive operations in inspection functions
- Pre-compile regex patterns used in Stan code manipulation
- Minimize data copying during stanvar combination

### Debugging Approaches
- Provide `verbose` arguments for inspection functions to show intermediate steps
- Include Stan code in error messages when compilation fails
- Log trend dispatch function calls for troubleshooting registry issues
- Preserve intermediate objects for debugging (optional return via `debug = TRUE`)

### Integration with Architecture Decisions
- Follow Single-Fit Dual-Object Architecture: generate code for combined model
- Use Convention-Based Dispatch: `"AR" → generate_ar_trend_stanvars()`
- Respect Autocorrelation Separation Principle in validation
- Apply Trend Parameter Naming Convention consistently
- Use Enhanced Registry System for automatic trend type discovery

## 12. Non-Goals (Explicit Boundaries)

### What This Task Will NOT Include
- **Custom Stan Code Modification:** Users cannot directly edit generated Stan code
- **New Trend Type Development:** Focus on system modernization, not new trend types
- **Alternative Stan Backends:** Stick with rstan/cmdstanr, no custom Stan interfaces
- **Backward Compatibility:** Breaking changes are acceptable per architecture decisions
- **Performance Optimization Beyond Sampling:** Focus on Stan model efficiency, not R code speed
- **GUI or Web Interfaces:** Command-line R interface only
- **Integration with Other Packages:** Focus on brms ecosystem, not external packages

### Features Deliberately Excluded
- **Stan Code Export to Files:** Generated code available as strings only
- **Alternative Prior Specification Languages:** R interface only, no Stan prior syntax
- **Cross-Package Prior Transfer:** mvgam-specific extensions not transferable to brms
- **Real-Time Model Updating:** Static model specification and fitting workflow

### Integration Limits
- **Stan Version Dependencies:** Target current stable Stan versions, not bleeding edge
- **brms Version Coupling:** Work with current brms stable version, not development
- **Complex Missing Data Handling:** Rely on existing brms missing data capabilities
- **Computational Backend Choice:** Use existing mvgam Stan integration approach

## 13. Success Criteria

### Correctness Verification
1. **brms Equivalence:** When `trend_formula = NULL`, generated Stan code exactly matches `brm()` output
2. **Statistical Equivalence:** Models fit with new system produce equivalent results to current mvgam
3. **Prior Propagation:** Custom priors specified in inspection functions correctly apply in fitted models
4. **Stan Compilation:** All generated Stan code compiles without errors across trend types

### User Experience Validation
1. **Workflow Completion:** Users can complete inspect → modify → fit workflow without errors
2. **Error Message Quality:** Invalid specifications produce helpful error messages with suggested fixes
3. **Documentation Clarity:** All functions have clear examples and cross-references
4. **Performance Targets:** Inspection functions complete in <1 second for typical models

### Technical Implementation Success
1. **Modular Architecture:** New trend types can be added following convention-based patterns
2. **Code Quality:** Stan code follows brms style conventions and best practices
3. **Integration Preservation:** All existing brms ecosystem methods work with mvgam objects
4. **Memory Efficiency:** Generated Stan models use memory efficiently during sampling

### System Integration Success
1. **Registry Function:** Auto-discovery of trend types works correctly
2. **Validation System:** Enhanced validation catches specification errors early
3. **Stan Assembly:** Complex stanvar combination produces correct combined models
4. **Parameter Monitoring:** Automatic parameter discovery includes all trend parameters

## 14. Open Questions

### Technical Decisions Still Needed
1. **Prior Data Frame Extension:** Should `mvgamprior` inherit from `brmsprior` or be independent class?
2. **Multivariate Prior Specification:** How should users specify different priors for different responses?
3. **Stan Block Organization:** Should trend components go in specific Stan blocks or be flexible?
4. **Parameter Transformation:** How should bounded parameters be handled in prior specification?

### User Experience Questions
1. **Function Naming:** Should we use `mvgam_get_prior()` or `get_prior()` for disambiguation?
2. **Error Detail Level:** How much Stan-specific detail should appear in error messages?
3. **Default Prior Strategy:** Should defaults be conservative (wide) or informative (narrow)?
4. **Inspection Output Format:** Should `make_stancode()` return formatted Stan code or raw strings?

### Implementation Uncertainties
1. **brms Integration Depth:** How tightly should we integrate with brms internal functions?
2. **Stan Code Caching:** Should generated code be cached to improve performance?
3. **Validation Timing:** Should all validation happen upfront or be distributed throughout pipeline?
4. **Registry Extensibility:** How should external packages register custom trend types?

### Future Compatibility
1. **Stan Evolution:** How should system adapt to future Stan language changes?
2. **brms Updates:** What happens when brms changes internal Stan generation patterns?
3. **Ecosystem Growth:** How should system accommodate future mvgam features?
4. **User Extension:** Should advanced users be able to customize Stan generation?
