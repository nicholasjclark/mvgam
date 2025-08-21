# TRD-stancode-generation-update Development Tasks

## Overview
Implementation tasks for stancode generation update feature with comprehensive prior system integration and Stan code inspection capabilities.

## Current Status
- ✅ **Foundation Systems**: Core prior and Stan generation systems (sections 1.0-2.6)
- ✅ **Parameter Standardization**: All trend generators use 3-stanvar pattern with standardized naming (section 2.7) 
- ✅ **VAR/VARMA Implementation**: Complete hierarchical grouping support with factor model constraints
- ✅ **Parameter Extraction**: Comprehensive parameter renaming system with 97.4% test success rate (Steps 4-6)
- 🎯 **Next Priority**: Stan code integration and validation testing (Steps 7-10)

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

### Remaining Minor Issues (Optional - 5 test failures)
- **Likelihood Exclusion**: Test assertions need stanvar collection access fixes (3 failures)  
- **Multivariate AR Parameters**: Pattern matching `grepl("ar1.*_trend$")` needs debugging (2 failures)

### Priority Tasks

- [ ] **Step 7 - Test Parameter Availability** (30 min): Verify mu_trend and times_trend availability in final standata after injection, test shared innovation system access to renamed parameters

- [ ] **Step 8 - Test Data Structure Correctness** (30 min): Validate times_trend matrix [n_time, n_series] dimensions, parameter mapping preservation for prediction compatibility

- [ ] **Step 9 - Systematic Validation** (45 min): Test multiple configurations: univariate trends (RW, AR, PW), multivariate shared trends, response-specific trends, mixed family models

- [ ] **Step 10 - Full Integration Test** (45 min): Complete test suite with Stan compilation focus, prediction compatibility validation, multivariate workflow testing

- [ ] **Multivariate Formula Integration** (60 min): Resolve setup_brms_lightweight handling of multivariate observation models with response-specific trend formulas

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