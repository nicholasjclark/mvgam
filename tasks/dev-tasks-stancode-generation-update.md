# TRD-stancode-generation-update Development Tasks

## Prior to Work
Read context files `active/architecture-decisions.md` and `active/quick-reference.md`

## Overview
Implementation tasks for stancode generation update feature with comprehensive prior system integration and Stan code inspection capabilities.

## Current Status
- ✅ **Foundation Systems**: Core prior and Stan generation systems (sections 1.0-2.6)
- ✅ **Parameter Standardization**: All trend generators use 3-stanvar pattern with standardized naming (section 2.7) 
- ✅ **VAR/VARMA Implementation**: Complete hierarchical grouping support with factor model constraints
- ✅ **Parameter Extraction**: Comprehensive parameter renaming system with 97.4% test success rate (Steps 4-6)
- 🎯 **Current Priority**: Stan code integration and validation testing (Steps 7-10)
- ✅ **get_prior() Implementation**: Clean S3 generic system with perfect brms delegation completed
- ❗ **Missing TRD Core Functions**: `make_stancode()`, `make_standata()`, `prior_summary()`, `get_inits()`, `set_prior()`

---

## COMPLETED MAJOR SECTIONS

### 1.0-2.6 Foundation and Core Systems ✅
**Key Files**: `R/priors.R`, `R/validations.R`, `R/brms_integration.R`, `R/stan_assembly.R`
**Achievement**: Complete prior extraction/combination system, Stan data/code generation pipeline, centralized prior system with 121 tests passing

### 2.7 Parameter Standardization and Non-Centered Parameterization ✅  
**Key Changes**: All trend generators (RW, AR, CAR, ZMVN, VAR, PW) standardized to 3-stanvar pattern
**Architecture**: `innovations_trend` → `scaled_innovations_trend` → `lv_trend` pipeline
**Critical Fix**: Eliminated duplicate stanvar names bug, single source of truth for dimensions

### 2.7.8 VAR/VARMA Implementation ✅
**Key Achievement**: Complete Heaps 2022 stationary VARMA(p,q) with hierarchical grouping support
**Features**: Group-specific coefficients, shared hyperpriors, block-structured matrices, factor model constraints

### 2.7.11.4-6 Parameter Extraction System ✅ *(2025-08-21)*
**Key Functions**: `extract_and_rename_trend_parameters()`, `create_times_trend_matrix()`
**Achievement**: 97.4% test success rate, complete namespace separation with `_trend` suffix system
**Architecture**: Stanvars collection compatibility, 432 Stan reserved words filtering, brms injection ready

---

## 🚀 NEXT TASK: Stan Code Integration (Steps 7-10)

**START HERE**: Parameter extraction system completed. Now need integration testing and Stan code combination.

### Remaining Minor Issue
- **Multivariate AR Parameters**: Pattern matching `grepl("ar1.*_trend$")` needs debugging (2 failures)

### Priority Tasks

- [x] **Step 7 - Test Parameter Availability** ✅ (Completed 2025-08-23): 
  - Successfully verified mu extraction from trend model stancode
  - Implemented CRITICAL fix: Created mu_trend for GLM-optimized models (no explicit mu vector)
  - Added comprehensive tests verifying exact equivalence with GLM linear predictor
  - Ensured bidirectional parameter mapping for all formula types
  - All 515 tests passing including exact structure preservation tests

- [x] **Step 8 - Test Data Structure Correctness** ✅ (Completed 2025-08-23): 
  - Validated times_trend matrix [n_time, n_series] dimensions and order preservation
  - Implemented comprehensive metadata system in extract_time_series_dimensions() with data ordering mappings
  - Added per-series time information for forecasting compatibility
  - Expanded tests in test-setup-brms.R with 5 new comprehensive test cases
  - All 611 tests passing after fixing 5 real bugs in validation and test construction


### Step 9 - brms-Equivalent Inspection Functions

**Implementation Strategy**: Use `mvgam_formula()` constructor + S3 method dispatch to extend brms functions cleanly without masking

- [ ] **mvgam_formula() constructor + get_prior.mvgam_formula()** (120 min total - 8 sub-tasks of 15 min each): 
  - Create `mvgam_formula()` constructor for consistent interface across inspection functions
  - Add `get_prior.mvgam_formula()` S3 method for clean brms integration
  - Return `brmsprior` data frame with `trend_component` column distinguishing observation vs trend parameters
  - Support both observation and trend model priors in unified interface
  
  **Sub-tasks (each 15 minutes)**:
  - [x] **Sub-task 1A**: Implement `mvgam_formula()` constructor function ✅ *(2025-08-25)*
    - ✅ Create function signature: `mvgam_formula(formula, trend_formula = NULL)` (simplified minimal approach)
    - ✅ Validate inputs: formula (formula/brmsformula), trend_formula (formula or NULL)  
    - ✅ Create list structure: `list(formula = formula, trend_formula = trend_formula)`
    - ✅ Set S3 class: `c("mvgam_formula", base_class)` with proper inheritance handling
    - ✅ Added to R/priors.R with comprehensive roxygen2 documentation
    
  - [x] **Sub-task 1B**: Add comprehensive validation to `mvgam_formula()` ✅ *(2025-08-25)*
    - ✅ Use checkmate for all parameter validation (formula, trend_formula)
    - ✅ Handle different formula types: formula, brmsformula, mvbrmsformula
    - ✅ Validate autocorrelation separation principle when trend_formula present
    - ✅ Store validated components in mvgam_formula object
    - ✅ Add helpful error messages using `insight::format_error()`
    
  - [x] **Sub-task 1C**: Implement clean S3 generic `get_prior()` system ✅ *(2025-08-26)*
    - ✅ Created proper S3 generic: `get_prior <- function(object, ...) UseMethod("get_prior")`
    - ✅ Implemented delegation methods: `get_prior.default()`, `get_prior.formula()`, `get_prior.brmsformula()`
    - ✅ Main implementation: `get_prior.mvgam_formula(object, data, family = gaussian(), ...)`
    - ✅ Perfect brms delegation when `trend_formula = NULL` with `trend_component = "observation"` column
    - ✅ Clean class structure: `class(mvgam_formula) <- "mvgam_formula"` with `attr(obj, "formula_class")` metadata
    - ✅ Comprehensive test coverage: 8 new tests added, all passing (273/286 total tests pass = 95.5%)
    
  - [ ] **Sub-task 1D**: Add trend component integration to get_prior method
    - Call `extract_response_names(formula)` to get response variable names  
    - Call `extract_trend_priors(trend_formula, data, response_names)` for trend priors
    - Add `trend_component = "trend"` column to trend-generated priors
    - Combine using `rbind(obs_priors, trend_priors)` preserving brmsprior class
    - Handle empty trend_priors case gracefully
    
  - [ ] **Sub-task 1E**: Test brms compatibility (exact equivalence)  
    - Create test file `tests/testthat/test-mvgam-formula.R`
    - Test: `mvgam_formula(y ~ x, trend_formula = NULL)` → `get_prior()` identical to brms
    - Test with different family types: gaussian(), poisson(), binomial() 
    - Test with different formula types: formula, brmsformula objects
    - Verify S3 dispatch works correctly and doesn't mask brms methods
    
  - [ ] **Sub-task 1F**: Test trend functionality and integration
    - Test trend_component column addition for both observation and trend parameters
    - Test with different trend types: RW(), AR(p=1), PW() constructors
    - Test multivariate formulas: mvbind(y1, y2) ~ x with shared trends  
    - Verify trend parameter naming follows _trend suffix convention
    - Test integration with `extract_response_names()` and `extract_trend_priors()`
    
  - [ ] **Sub-task 1G**: Add comprehensive documentation and examples
    - Complete roxygen2 for both `mvgam_formula()` and `get_prior.mvgam_formula()` 
    - Add realistic workflow examples: construct → inspect → modify priors
    - Document S3 class structure and inheritance from base formula classes
    - Add @family prior-functions tag and @seealso references
    - Show integration with make_stancode(), make_standata() patterns
    
  - [ ] **Sub-task 1H**: Integration testing and ecosystem validation
    - Test returned brmsprior objects work with `brms::set_prior()`
    - Test error handling: malformed inputs, incompatible autocorr + trends
    - Create examples showing complete workflow: construct → inspect → modify → fit
    - Verify mvgam_formula objects can be reused across inspection functions  
    - Test that formula inheritance preserves brms compatibility

- [ ] **make_stancode.mvgam_formula()** (60 min): Generate complete Stan model code before fitting
  - Add S3 method: `make_stancode.mvgam_formula(object, prior = NULL, ...)`
  - Extract formula and trend_formula from mvgam_formula object
  - Character string containing complete Stan model code with both observation and trend components
  - Follow brms style conventions and Stan best practices  
  - Handle custom prior specifications from `get_prior()` or `set_prior()`
  - Integrate with existing `generate_combined_stancode_and_data()` function

- [ ] **make_standata.mvgam_formula()** (60 min): Generate complete Stan data list before fitting
  - Add S3 method: `make_standata.mvgam_formula(object, prior = NULL, ...)`
  - Extract formula and trend_formula from mvgam_formula object
  - Named list containing all data for Stan model (observation + trend data components)
  - Structure matches what would be passed to Stan during fitting
  - Support custom prior data requirements
  - Integrate with existing Stan data preparation pipeline

- [ ] **prior_summary()** (30 min): Inspect priors used in fitted or specified models
  - Data frame showing priors actually used or planned to be used
  - Support both fitted mvgam objects and model specifications
  - Show all priors vs non-default only option

- [ ] **get_inits()** (45 min): Generate or inspect initialization values for Stan parameters
  - List of initialization values for Stan parameters
  - Support multiple initialization strategy options
  - Handle both observation and trend parameter initialization

### Prior Specification Functions  
- [ ] **set_prior()** and **prior()** (30 min): Specify custom priors using brms interface patterns
  - Follow brms `set_prior()` specification exactly with extensions for trend-specific parameters
  - Return prior specification objects compatible with mvgam functions
  - Support trend parameter targeting with `_trend` suffix conventions

- [ ] **Step 10 - Multivariate Formula Integration** (60 min): Resolve setup_brms_lightweight handling of multivariate observation models with response-specific trend formulas

- [ ] **Step 11 - Systematic Validation** (45 min): Test for correct standata and stancode across multiple configurations: univariate trends (RW, AR, PW), multivariate shared trends, response-specific trends, mixed family models, by expanding tests in `test-setup-brms.R`

- [ ] **Step 12 - Full Integration Test** (45 min): Complete test suite with Stan compilation focus, prediction compatibility validation, multivariate workflow testing


## PENDING FUTURE SECTIONS

### 3.0 Complete Prior Specification System ⏳
- [ ] 3.1 Add prior_spec to RW trend registration  
- [ ] 3.2 Add prior_spec to AR trend registration with dynamic lag handling
- [ ] 3.3 Add prior_spec to remaining trend types (CAR, ZMVN, VAR, PW)

### 4.0 Enhanced Multivariate Support ⏳  
- [ ] 4.1 Response-specific trend injection patterns
- [ ] 4.2 Mixed family multivariate model testing
- [ ] 4.3 Prediction infrastructure for multivariate models

### 5.0 Documentation and Integration ⏳
- [ ] 5.1 Update package documentation with new parameter patterns
- [ ] 5.2 Integration testing with full mvgam workflow
- [ ] 5.3 Performance benchmarking and optimization
