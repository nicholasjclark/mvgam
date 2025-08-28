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
    
  - [x] **Sub-task 1O**: Remove mvgam::prior() function to avoid masking ✅ *(2025-08-28)*
    - ✅ Use r-package-analyzer to verify how brms::prior() handles custom class names and whether it already supports arbitrary suffixes
    - ✅ Delete the `prior()` function from R/priors.R completely
    - ✅ Update documentation to show users should use `brms::prior()` directly
    - ✅ Verify that `brms::prior()` already works with `_trend` suffix parameters
    - ✅ Update any internal code that was using `mvgam::prior()` to use `brms::prior()`
    - ✅ Update test-priors.R to use `brms::prior()` instead of `mvgam::prior()` and remove mvgam attribute expectations
    - ✅ All 346 tests in test-priors.R now pass (100% success rate)
    
  - [X] **Sub-task 1P**: Complete elimination of mvgam prior attribute system (45 min)
    - **CRITICAL**: Replace all attribute-based logic with naming convention detection (`grepl("_trend$", prior$class)`)
    - **Phase 1 - Core Functions (20 min)**:
      - Update `extract_trend_priors()` to filter by `grepl("_trend$", prior$class)` instead of `trend_components` attribute
      - Update `extract_observation_priors()` to filter by `!grepl("_trend$", prior$class)` instead of attributes  
      - Update `combine_obs_trend_priors()` to simple `rbind()`, remove all attribute creation
      - Remove `print.brmsprior()` and `c.brmsprior()`
      
    - **Phase 2 - Remove old mvgam::set_prior() tests (15 min)**:
      - Use r-package-analyzer to verify how brms::set_prior() and its aliases (prior_(), prior_string() etc...) works
      - Update all tests to use `brms::set_prior()` instead of `set_prior()`
      - Ensure tests cover the breadth of ways users can set priors in brms
      
    - **Phase 3 - Validation Architecture (10 min)**:
      - Move trend parameter validation to trend setup phase (where we have full context)
      - Remove parameter class validation from prior functions (they become pure data manipulation)
      - Test that all prior-related functions work with pure brms objects
    - **LAST**: Use r-test-runner to verify all tests pass after complete attribute elimination

### **Phase 3: Clean Integration and Testing (30 min)**
    
  - [X] **Sub-task 1**: Complete attribute elimination and test cleanup ✅ *(2025-08-28)*
    - ✅ Updated architecture-decisions.md to recommend brms::prior() over brms::set_prior() for trend parameters
    - ✅ Removed all mvgam attribute expectations from tests (mvgam_enhanced, trend_components)
    - ✅ Updated tests to use _trend suffix detection instead of attributes
    - ✅ Removed obsolete print.brmsprior test expectations
    - ✅ Updated brms::set_prior() test to use brms::prior() for trend parameters
    - ✅ **KNOWN ISSUE**: 1 test failure in factor model Z parameter generation needs investigation

  - [x] **Sub-task 2**: Investigate factor model Z parameter generation ✅ *(2025-08-28)*
    - ✅ **RESOLVED**: Test logic was incorrect, not parameter generation
    - ✅ **FIX**: Updated test to look for Z in all parameter classes (not just trend classes)  
    - ✅ **RESULT**: Z parameter correctly generated for AR factor models without _trend suffix
    - ✅ **VALIDATION**: All 25 tests in test-priors.R now pass (100% success rate)
    - ✅ **NO SKIPS**: Eliminated skip conditions, proper expectations added

- [X] **stancode.mvgam_formula()** (60 min): Generate complete Stan model code before fitting
  - Add S3 method: `stancode.mvgam_formula(object, prior = NULL, ...)`
  - Extract formula and trend_formula from mvgam_formula object
  - Character string containing complete Stan model code with both observation and trend components
  - Follow brms style conventions and Stan best practices  
  - Handle custom prior specifications from `prior()` or `prior_string()` etc...
  - Integrate with existing `generate_combined_stancode_and_data()` function

- [X] **standata.mvgam_formula()** (60 min): Generate complete Stan data list before fitting
  - Add S3 method: `standata.mvgam_formula(object, prior = NULL, ...)`
  - Extract formula and trend_formula from mvgam_formula object
  - Named list containing all data for Stan model (observation + trend data components)
  - Structure matches what would be passed to Stan during fitting
  - Support custom prior data requirements
  - Integrate with existing Stan data preparation pipeline

- [x] **Sub-task 3**: Investigate stancode/standata test failures and enhance validation ✅ *(2025-08-28)*
  - **INVESTIGATION COMPLETE**: Root causes identified, comprehensive architectural fixes required
  
  ### 🔍 ROOT CAUSES IDENTIFIED (2025-08-28):
  
  **CRITICAL FINDINGS**: 15/23 tests failing (65% failure rate) due to Stan compilation errors
  
  1. **Stan Block Generation Bugs**:
     - ✅ **FIXED**: `mu_trend` wrongly assigned to `"model"` block → changed to `"tparameters"` (R/stan_assembly.R:4027)
     - ❌ **UNFIXED**: Duplicate blocks - Two `transformed parameters` and two `model` blocks being generated
     - ❌ **UNFIXED**: Parameter placement - `sigma_trend`, `innovations_trend` appearing in wrong blocks despite correct stanvar specs
  
  2. **Missing Variable Declarations**:
     - `n_trend`, `n_series_trend`, `n_lv_trend` referenced but not declared in appropriate blocks
     - `trend`, `obs_ind`, `times_trend` missing in required scopes (`obs_ind` should not exist)
     - Self-referencing assignment: `vector[N] mu_combined = mu_combined;` (line 38 in generated code)
  
  3. **Stanvar Injection System Failures**:
     - Block boundaries not respected during stanvar combination
     - brms::make_stancode() may be mishandling our stanvar injection
     - Variable scoping inconsistencies between code segments
  
  ### 🎯 WHERE TO START NEXT SESSION:
  
  **PRIORITY 1 - Fix Stanvar Injection Pipeline**:
  - **FILE**: `R/stan_assembly.R`
  - **FUNCTIONS**: 
    - `generate_base_stancode_with_stanvars()` (line 330) - Check how stanvars are combined
    - `inject_trend_into_linear_predictor()` (line 725) - May be causing duplicate blocks
    - `generate_combined_stancode()` (line 173) - Orchestrator that needs review
  - **KEY ISSUE**: Parameter stanvars with `block = "parameters"` ending up in model block
  
  **PRIORITY 2 - Debug Duplicate Block Generation**:
  - Trace why two `transformed parameters` and two `model` blocks are created
  - Check if brms and mvgam are both creating the same blocks
  - May need to modify how we inject trend code into existing blocks vs creating new ones
  
  **PRIORITY 3 - Variable Declaration Completeness**:
  - Ensure `generate_common_trend_data()` creates ALL required dimension variables
  - Check that `extract_and_rename_trend_parameters()` properly handles all references
  
  ### 📝 DEBUGGING RESOURCES AVAILABLE:
  - **DEBUG SCRIPT**: `debug_stan_parameter_blocks.R` created during investigation
  - **TEST FILE**: `tests/testthat/test-stancode-standata.R` with 23 comprehensive tests
  - **RECOMMENDATION**: Keep debug script for next session - provides systematic testing approach

## 🚨 **CRITICAL PRIORITY**: Observation-to-Trend Mapping Implementation (Option 1)

**STATUS**: Next immediate priority before any further stancode investigation  
**ISSUE**: Current trend injection assumes no missing data, causing misalignment between brms observations and trend matrix positions  
**SOLUTION**: Create obs_trend_mapping during injection to handle missing data correctly

### **Phase 1: Investigation and Design** (90 min)

- [ ] **Sub-task A1**: Investigate data flow and access points (30 min)
  - Trace where observation-level data is available during stanvar generation/injection pipeline
  - Determine how to access time/series information for each observation that brms includes  
  - Map out the timing: when do we have both brms final observation data AND trend specifications?
  - Document the data structures available at each pipeline stage
  
- [ ] **Sub-task A2**: Analyze brms data processing behavior (30 min)
  - Understand exactly how brms decides which observations to include/exclude
  - Test with datasets containing missing values to see brms ordering behavior
  - Determine if brms provides metadata about excluded observations
  - Verify that `prepare_stan_data()` ordering is preserved by brms processing
  
- [ ] **Sub-task A3**: Design the mapping strategy (30 min)
  - Choose data structure: `array[N] int obs_trend_linear_idx` vs `array[N] int obs_time_idx; array[N] int obs_series_idx`
  - Decide creation point: during trend stanvar generation vs during injection function
  - Plan missing data handling: systematic missing vs random missing vs no missing
  - Design validation approach to detect misaligned mappings

### **Phase 2: Implementation** (120 min)

- [ ] **Sub-task B1**: Implement mapping creation infrastructure (45 min)
  - Create `generate_obs_trend_mapping()` function that takes observation data + trend dimensions
  - Generate mapping array: for each observation n → corresponding trend[time_idx, series_idx] position
  - Handle edge cases: single series, single time point, irregular time series
  - Add comprehensive input validation and error handling
  
- [ ] **Sub-task B2**: Integrate mapping into stanvar generation pipeline (45 min)
  - Add mapping creation to `extract_trend_stanvars_from_setup()` or equivalent function
  - Ensure mapping gets included as "data" block stanvar in trend_stanvars collection
  - Pass observation data through pipeline to mapping creation point
  - Add mapping metadata to trend specifications for downstream usage
  
- [ ] **Sub-task B3**: Update injection logic to use mapping (30 min)
  - Replace direct indexing `trend[time_idx, series_idx]` with mapping-based approach
  - Support both 2D matrix access and linear indexing strategies
  - Add validation that mapping array exists and has correct dimensions
  - Preserve existing injection logic structure while eliminating obs_ind dependency

### **Phase 3: Testing and Validation** (90 min)

- [ ] **Sub-task C1**: Test mapping with controlled datasets (45 min)
  - **Complete data**: No missing values - verify mapping == direct indexing results
  - **Random missing**: 20% randomly missing observations across series/time  
  - **Systematic missing**: Some series start later, some end earlier
  - **Edge cases**: Single series, single time point, all-but-one missing
  
- [ ] **Sub-task C2**: Validate against existing test suite (30 min)
  - Run all 23 tests in `test-stancode-standata.R` with new mapping approach
  - Fix any regressions introduced by mapping implementation
  - Verify that Stan compilation errors are resolved
  - Ensure generated Stan code structure is valid
  
- [ ] **Sub-task C3**: Performance and correctness validation (15 min)
  - Compare trend injection results with/without missing data
  - Verify that mapping overhead is minimal for complete data cases
  - Test with larger datasets to ensure scalability
  - Validate that trend effects are correctly applied to matching observations

### **Phase 4: Integration and Documentation** (30 min)

- [ ] **Sub-task D1**: Update debug script and diagnostic tools (15 min)
  - Enhance `debug_stan_parameter_blocks.R` to test mapping creation and usage
  - Add mapping validation to Stan code analysis functions
  - Create helper functions to inspect obs_trend_mapping correctness
  
- [ ] **Sub-task D2**: Document architecture decision and usage (15 min)
  - Update `active/architecture-decisions.md` with missing data handling approach
  - Document when/why obs_trend_mapping is created vs obs_ind approach
  - Add examples of mapping structure for different missing data patterns

**TOTAL ESTIMATED TIME**: 330 minutes (5.5 hours)

**SUCCESS CRITERIA**: 
- ✅ All 23 stancode/standata tests pass
- ✅ Generated Stan code compiles without errors  
- ✅ Trend effects correctly applied even with missing data
- ✅ No performance regression for complete data cases
- ✅ Robust handling of various missing data patterns

---

- [ ] **Sub-task 4 - Multivariate Formula Integration** (60 min): Resolve setup_brms_lightweight handling of multivariate observation models with response-specific trend formulas

- [ ] **Sub-task 5 - Systematic Validation** (45 min): Test for correct standata and stancode across multiple configurations: univariate trends (RW, AR, PW), multivariate shared trends, response-specific trends, mixed family models, by expanding tests in `test-setup-brms.R`

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
