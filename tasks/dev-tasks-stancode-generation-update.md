# TRD-stancode-generation-update Development Tasks

## Overview
Implementation tasks for stancode generation update feature with comprehensive prior system integration and Stan code inspection capabilities.

## Current Status
- ✅ **Foundation Systems**: Core prior and Stan generation systems (sections 1.0-2.6)
- ✅ **Parameter Standardization**: All trend generators use 3-stanvar pattern with standardized naming (section 2.7) 
- ✅ **VAR/VARMA Implementation**: Complete hierarchical grouping support with factor model constraints
- ✅ **Parameter Extraction**: Comprehensive parameter renaming system with 97.4% test success rate (Steps 4-6)
- 🎯 **Current Priority**: Stan code integration and validation testing (Steps 7-10)
- ❗ **Missing TRD Core Functions**: `get_prior()`, `make_stancode()`, `make_standata()`, `prior_summary()`, `get_inits()`, `set_prior()`

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

- [ ] **Step 7 - Test Parameter Availability** (30 min): Verify mu correctly extracted from trend model stancode (either needs to be extracted directly, in the case of complex linear predictors, or created separately in the case of simple linear predictors; see models for the tests on line 524 of `test-setup-brms.R` to investigate). Verify times_trend availability in final standata after injection, test shared innovation system access to renamed parameters by expanding tests in `test-setup-brms.R`

- [ ] **Step 8 - Test Data Structure Correctness** (30 min): Validate times_trend matrix [n_time, n_series] dimensions AND order, ensure ordering is stored as a slot in the final mvgam object for simple mapping of fitted values, predictions and residuals after model fitting. Validate parameter mapping preservation for prediction compatibility by expanding tests in `test-setup-brms.R`

3- [ ] **Step 9 - Systematic Validation** (45 min): Test for correct standata and stancode across multiple configurations: univariate trends (RW, AR, PW), multivariate shared trends, response-specific trends, mixed family models, by expanding tests in `test-setup-brms.R`

- [ ] **Step 10 - Full Integration Test** (45 min): Complete test suite with Stan compilation focus, prediction compatibility validation, multivariate workflow testing

- [ ] **Multivariate Formula Integration** (60 min): Resolve setup_brms_lightweight handling of multivariate observation models with response-specific trend formulas

---

## 🎯 CORE USER-FACING FUNCTIONS (TRD Requirements)

**CRITICAL**: The following inspection functions are core TRD objectives and must be implemented:

### brms-Equivalent Inspection Functions
- [ ] **get_prior()** (45 min): Inspect all available priors before fitting
  - Return `mvgamprior` data frame with `trend_component` column distinguishing observation vs trend parameters
  - Compatible with brms `brmsprior` structure for cross-package consistency
  - Support both observation and trend model priors in unified interface

- [ ] **make_stancode()** (60 min): Generate complete Stan model code before fitting
  - Character string containing complete Stan model code with both observation and trend components
  - Follow brms style conventions and Stan best practices
  - Handle custom prior specifications from `get_prior()` or `set_prior()`

- [ ] **make_standata()** (60 min): Generate complete Stan data list before fitting
  - Named list containing all data for Stan model (observation + trend data components)
  - Structure matches what would be passed to Stan during fitting
  - Support custom prior data requirements

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

---

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
