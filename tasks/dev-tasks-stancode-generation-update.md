# TRD-stancode-generation-update Development Tasks

## Overview
Implementation tasks for stancode generation update feature with comprehensive prior system integration and Stan code inspection capabilities.

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
**Architecture**: Stanvars collection compatibility, 432 Stan reserved words filtering, brms injection ready

---

## 🚀 NEXT TASK: Stan Code Integration (Steps 7-10)

### Priority Tasks

- [x] **Step 7 - Test Parameter Availability** ✅ (Completed 2025-08-23): 
  - Successfully verified mu extraction from trend model stancode
  - Implemented CRITICAL fix: Created mu_trend for GLM-optimized models (no explicit mu vector)
  - Added comprehensive tests verifying exact equivalence with GLM linear predictor
  - Ensured bidirectional parameter mapping for all formula types

- [x] **Step 8 - Test Data Structure Correctness** ✅ (Completed 2025-08-23): 
  - Validated times_trend matrix [n_time, n_series] dimensions and order preservation
  - Implemented comprehensive metadata system in extract_time_series_dimensions() with data ordering mappings
  - Added per-series time information for forecasting compatibility
  - Expanded tests in test-setup-brms.R with 5 new comprehensive test cases


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
    
  - [x] **Sub-task 1D**: Add trend component integration to get_prior method ✅ *(2025-08-26)*
    - ✅ Call `extract_response_names(formula)` to get response variable names  
    - ✅ Call `extract_trend_priors(trend_formula, data, response_names)` for trend priors
    - ✅ Add `trend_component = "trend"` column to trend-generated priors
    - ✅ Combine using `rbind(obs_priors, trend_priors)` preserving brmsprior class
    - ✅ Handle empty trend_priors case gracefully
    
  - [x] **Sub-task 1E**: Add embedded family support for get_prior method ✅ *(2025-08-26)*
    - ✅ **RESOLVED**: Implemented `has_embedded_families()` detection function in R/priors.R
    - ✅ **RESOLVED**: Made family parameter validation conditional based on formula type 
    - ✅ **RESOLVED**: Handles `bf()` formulas with embedded families correctly
    - ✅ **RESOLVED**: Maintains perfect brms delegation when `trend_formula = NULL`
  - [x] **Sub-task 1F**: Test brms compatibility (exact equivalence) ✅ *(2025-08-26)*
    - Enhanced existing test file with comprehensive brms compatibility tests
    - Verified perfect equivalence when `trend_formula = NULL`
    
  - [x] **Sub-task 1G**: Test trend functionality and integration ✅ *(2025-08-26)*
    - Added comprehensive tests for all multivariate patterns and trend types
    - Fixed critical bug in extract_mvbind_responses() function
    - Fixed AR(p=2) parameter generation bug - now correctly generates ar1_trend AND ar2_trend
    - Updated CAR and ZMVN test expectations to match actual behavior
    
  - [x] **Sub-task 1H**: Critical parameter generation fixes ✅ *(2025-08-26)*
    - ✅ **RESOLVED**: Fix ZMVN single-series correlation parameter issue
      - Fixed ZMVN incorrectly adding L_Omega_trend for single series
      - Added data-context-aware filtering in generate_trend_priors_from_monitor_params()
      - Added dimensions calculation in parse_trend_formula() using existing extract_time_series_dimensions()
      
   - [X] **Sub-task 1I**: Complete validation that all priors from trend_formula are correctly returned
    - Use the test-runner agent to run all tests in test-priors.R and test-mvgam_formula.R
    - Validate logic in `generate_trend_priors()` to ensure ALL brms-generated priors are returned with the correct "_trend" suffix 
    
   - [X] **Sub-task 1I**: Comprehensive brms addition-terms validation (45 min)
    - Survey all brms addition-terms that should NOT be supported in trend_formula
    - Add validation functions to reject: weights(), cens(), trunc(), mi(), trials(), rate(), vreal(), vint()
    - Extend existing `validate_single_trend_formula()` with comprehensive checks
    - Add comprehensive tests covering all forbidden addition-terms with clear error messages to test-priors.R and test-mvgam_formula.R
    - Use the test-runner agent to run complete test files in test-priors.R and test-mvgam_formula.R
    - Validate that trend_formula only accepts: fixed effects, random effects, smooths, gp(), tensor products
    - Document allowed vs forbidden terms in trend_formula validation architecture
    - Document S3 class structure and inheritance from base formula classes
    
  - [x] **Sub-task 1J**: Integration testing and ecosystem validation ✅ *(2025-08-27)*
    - ✅ Test returned brmsprior objects work with `brms::set_prior()` and `brms::prior()` for both trend and observation parameters
    - ✅ Verify mvgam_formula objects can be reused across inspection functions  
    - ✅ Test that formula inheritance preserves brms compatibility
    - ✅ Enhanced test coverage with proper S3 method registration and documentation

### **Phase 1: Fix brmsprior Compatibility Issues (45 min)**

  - [X] **Sub-task 1K**: Replace trend_component column with attributes-based metadata (15 min)
    - Remove `trend_component` column from `get_prior.mvgam_formula()` return
    - Add `attr(priors, "trend_components")` with vector indicating each row's component type
    - Update `combine_obs_trend_priors()` to use attributes instead of columns
    - Verify brms functions work with clean brmsprior structure
    
  - [X] **Sub-task 1L**: Add custom print/summary methods for enhanced brmsprior objects (15 min)  
    - Create `print.brmsprior` method that shows trend component info when mvgam attributes exist
    - Add `summary.brmsprior` enhancement for mvgam-specific metadata
    - Ensure standard brms objects print unchanged (compatibility check)
    - Override `c.brmsprior` to preserve attributes during combination
    
  - [X] **Sub-task 1M**: Update integration tests for attribute-based approach (15 min)
    - Fix all related tests to use attributes instead of columns  
    - Test brms ecosystem integration with clean brmsprior objects
    - Verify `brms::set_prior()` and `brms::prior()` work correctly
    - Test attribute preservation through prior combination operations

### **Phase 2: Enhanced Prior Interface (60 min)**

  - [X] **Sub-task 1N**: Create mvgam::set_prior() wrapper function (15 min) ✅ *(2025-08-27)*
    - ✅ Route observation parameters to `brms::set_prior()` automatically  
    - ✅ Handle trend parameters (`_trend` suffix) through mvgam system
    - ✅ Maintain full brms syntax compatibility
    - ✅ Return enhanced brmsprior with proper attributes
    - ✅ Added comprehensive documentation with examples and architectural justification
    - ✅ Fixed attribute consistency for trend_components (vector for both parameter types)
    - ✅ Added proper @importFrom declarations and NAMESPACE export
    - ✅ **COMPREHENSIVE REVISION**: Added full support for all brms input patterns:
      - ✅ Character strings, brmsprior objects, and list inputs
      - ✅ Vectorized operations with parameter consistency validation
      - ✅ Bounds constraint validation (lb < ub)
      - ✅ Comprehensive input type validation and error handling
      - ✅ Mixed parameter handling with helpful user warnings
      - ✅ Complete compatibility with all brms::set_prior() usage patterns
    
  - [X] **Sub-task 1O**: Create mvgam::prior() wrapper function with + operator support (15 min)
    - Use r-package-analyzer to understand how brms::prior() works internally
    - Full brms syntax compatibility: `mvgam::prior(normal(0, 1), class = ar1_trend)`
    - Support `+` operator for combining observation and trend priors
    - Handle both brms-style and mvgam trend parameter specifications
    - Automatic detection and routing based on parameter class names
    
  - [ ] **Sub-task 1P**: Add prior validation and parameter detection system (15 min)
    - Validate trend parameter class names against registered trend types
    - Clear error messages for invalid parameter specifications  
    - Parameter existence checking (e.g., `ar2_trend` only valid when `p >= 2`)
    - Integration with existing trend registry system
    
  - [ ] **Sub-task 1Q**: Update mvgam() main function to accept mixed prior specifications (15 min)
    - Accept enhanced brmsprior objects with trend and observation components
    - Route trend priors to Stan generation system properly
    - Route observation priors to brms system unchanged
    - Backwards compatibility with current prior specifications

### **Phase 3: Advanced Features (45 min)**

  - [ ] **Sub-task 1R**: Smart prior merging system - users modify subset while keeping defaults (15 min)
    - Allow users to specify only changed priors, keep defaults for rest
    - Intelligent merging of user specifications with `get_prior()` defaults
    - Preserve user customizations while filling in missing specifications
    - Handle both observation and trend parameter defaults appropriately
    
  - [ ] **Sub-task 1S**: Enhanced inspection functions: prior_summary.mvgam(), retrospective prior inspection (15 min)  
    - `prior_summary.mvgam()` method for fitted models showing actual priors used
    - Retrospective inspection of trend vs observation prior sources
    - Integration with existing mvgam object structure
    - Clear display of default vs user-specified priors
    
  - [ ] **Sub-task 1T**: Comprehensive documentation with realistic workflow examples (15 min)
    - Complete roxygen2 documentation for all wrapper functions
    - Realistic workflow examples in documentation
    - Integration examples with `make_stancode`/`make_standata`  
    - Clear explanation of observation vs trend parameter handling

### **Phase 4: Robust Integration (30 min)**

  - [ ] **Sub-task 1U**: Cross-function compatibility testing with make_stancode/standata and multivariate models (15 min)
    - Test enhanced priors work with `make_stancode.mvgam_formula()`
    - Test enhanced priors work with `make_standata.mvgam_formula()`
    - Multivariate model compatibility with response-specific trend priors
    - Performance testing with complex prior specifications
    
  - [ ] **Sub-task 1V**: Comprehensive error handling, edge case management, clear user messaging (15 min)
    - Graceful handling of invalid prior combinations
    - Clear error messages for common user mistakes
    - Edge case handling (empty priors, conflicting specifications, etc.)  
    - Documentation of limitations and constraints

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
  - Add realistic workflow examples to roxygyen2 documents: construct → inspect → modify priors
  - Add @seealso references
  - Show integration with make_stancode(), make_standata() patterns

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
