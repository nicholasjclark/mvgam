# TRD-stancode-generation-update Development Tasks

## Overview
**CRITICAL PRIORITY**: Fix stancode generation failures that cause 15/23 tests to fail with Stan compilation errors. Root cause: missing data handling system needs obs_ind mapping that brms doesn't provide.

## 📋 COMPLETED FOUNDATION WORK ✅

**Summary**: Complete prior system, parameter extraction, trend generators, and inspection functions (Steps 1-9) with 346+ tests passing across `R/priors.R`, `R/validations.R`, `R/brms_integration.R`, `R/stan_assembly.R`. Key achievements: standardized 3-stanvar pattern, mvgam_formula() interface, centralized prior resolution, and parameter extraction system.

---

## 🚨 **CURRENT CRITICAL ISSUE**: Duplicate Parameter Declarations

**STATUS**: 15/23 tests failing in `tests/testthat/test-stancode-standata.R` with Stan compilation errors  
**ROOT CAUSE**: Duplicate `sigma_trend` parameter declarations (scalar vs vector versions) causing Stan syntax errors

### 🔍 ERROR ANALYSIS *(2025-09-01)*

**Current Status**: Stan compilation failures due to duplicate parameter declarations

#### **Primary Issue: Duplicate sigma_trend Parameters**
- **Problem**: Stan code contains both `real<lower=0> sigma_trend;` and `vector<lower=0>[1] sigma_trend;` declarations
- **Impact**: Stan compilation fails with "Identifier 'sigma_trend' is already in use" error
- **Affected Tests**: 13/15 test failures show this exact pattern
- **Root Cause**: Different pipeline components (shared innovations vs trend-specific stanvars) create conflicting parameter versions

#### **Secondary Issues**
- **Series Validation Error**: `series_var` NULL/empty causing zero-length argument errors in multivariate models (2 tests)
- **Input Validation Missing**: Character string inputs not properly rejected (1 test)

---

## 🚀 **NEXT IMMEDIATE TASKS**: Fix Parameter Duplication

**GOAL**: Eliminate duplicate parameter declarations to restore Stan code compilation

**KEY FILES TO MODIFY**:
- `R/stan_assembly.R`: Functions creating trend-specific stanvars and shared innovation stanvars
- `R/validations.R`: Series validation functions for multivariate models
- Input validation in stancode generation functions

**A1**: Identify which component creates scalar `sigma_trend` vs vector `sigma_trend[1]`
  - Search `R/stan_assembly.R` for all functions that create `sigma_trend` parameters
  - Based on previous context: shared innovation system creates scalar, trend-specific stanvars create vector
  - Examine `generate_shared_innovations_stanvar()` vs `generate_trend_specific_stanvars()`

**A2**: Analyze parameter coordination between pipeline components  
  - Check how different stanvar sources are combined in `extract_trend_stanvars_from_setup()`
  - Identify where parameter conflicts should be detected and resolved
  - Document current parameter generation flow

### **Task 2: Fix Duplicate Parameter Issue (45 min)**

**B1**: Eliminate scalar `sigma_trend` from shared innovation system (30 min)
  - Based on step6_5 showing only vector version works correctly
  - Modify shared innovation stanvar generation to use vector form consistently
  - OR implement parameter deduplication logic during stanvar combination

**B2**: Test parameter fix with simple case (15 min)
  - Create minimal test case to verify single `sigma_trend` declaration
  - Ensure Stan code compiles successfully

### **Task 3: Fix Secondary Issues (45 min)**

**C1**: Fix series_var validation error (20 min)
  - Add NULL checks in `validate_series_time()` function in `R/validations.R:1277`
  - Handle multivariate models where `series_var` may be empty/NULL

**C2**: Fix input validation test (15 min)  
  - Add proper input type checking to reject character strings in stancode functions
  - Ensure expected error is thrown for invalid inputs

**C3**: Final validation (10 min)
  - Run targeted test subset to verify fixes work

**SUCCESS CRITERIA**: 
- ✅ Generated Stan code compiles without errors  
- ✅ Trend effects correctly applied even with missing data
- ✅ No performance regression for complete data cases
- ✅ Robust handling of various missing data patterns

---

## 🔄 **FUTURE WORK AFTER STANCODE FIXES**

### **Multivariate Formula Integration** (60 min)
- Resolve setup_brms_lightweight handling of multivariate observation models with response-specific trend formulas

### **Systematic Validation** (45 min)  
- Test for correct standata and stancode across multiple configurations: univariate trends (RW, AR, PW), multivariate shared trends, response-specific trends, mixed family models

### **Prior Specification System Completion**
- Add prior_spec to remaining trend types (CAR, ZMVN, VAR, PW)
- Enhanced multivariate support with response-specific trend injection patterns
- Documentation updates and performance optimization

### **Package Integration**
- Full mvgam workflow integration testing
- Performance benchmarking and optimization
- Update package documentation with new parameter patterns
