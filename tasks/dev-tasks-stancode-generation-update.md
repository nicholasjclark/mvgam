# TRD-stancode-generation-update Development Tasks

## Overview
**CRITICAL PRIORITY**: Fix stancode generation failures that cause 15/23 tests to fail with Stan compilation errors. Root cause: missing data handling system needs obs_ind mapping that brms doesn't provide.

## 📋 COMPLETED FOUNDATION WORK ✅

**Summary**: Complete prior system, parameter extraction, trend generators, and inspection functions (Steps 1-9) with 346+ tests passing across `R/priors.R`, `R/validations.R`, `R/brms_integration.R`, `R/stan_assembly.R`. Key achievements: standardized 3-stanvar pattern, mvgam_formula() interface, centralized prior resolution, and parameter extraction system.

---

## 🚨 **CURRENT CRITICAL ISSUE**: Mixed Progress on Stan Code Generation

**STATUS**: **13/86 tests failing** (major improvement from 15/23) in enhanced `tests/testthat/test-stancode-standata.R`  
**ROOT CAUSE**: GLM-optimized path working, but multiple remaining issues in both standard and multivariate paths

### 🔍 ERROR ANALYSIS *(2025-09-02 Updated)*

**Current Status**: **MAJOR PROGRESS** - GLM-optimized path working correctly, but remaining issues in standard and multivariate paths

#### **✅ RESOLVED: GLM-Compatible Trend Injection**
- **Solution Implemented**: GLM detection in Stage 5 + automatic `mu_ones` stanvar injection
- **Performance Preserved**: Matrix multiplication `vector[N] mu = Xc * b` + efficient trend addition
- **GLM Functions Working**: All brms GLM types (poisson_log_glm, normal_id_glm, etc.) with correct lpdf/lpmf usage
- **Type Safety**: `to_matrix(mu)` conversion + `mu_ones` stanvar for GLM compatibility

#### **🚨 REMAINING CRITICAL ISSUES** *(13/86 test failures)*

1. **Duplicate Parameter Declarations**
   - **Error**: `Identifier 'sigma' is already in use` - sigma declared in both obs and trend models
   - **Root Cause**: `filter_block_content()` not removing all duplicate declarations
   - **Impact**: Stan compilation failure in some family combinations

2. **Missing mu Variable in Standard Path** 
   - **Error**: `Identifier 'mu' not in scope` when adding trend effects
   - **Root Cause**: Standard (non-GLM) path doesn't create `mu` variable, only GLM path does
   - **Impact**: Models without GLM optimization fail compilation

3. **Duplicated Stanvar Names in Multivariate Models**
   - **Error**: `Duplicated names in 'stanvars' are not allowed`
   - **Root Cause**: Multiple responses create conflicting stanvar names (e.g., `obs_trend_time`)
   - **Impact**: All multivariate models fail

4. **Multiple Block Structure Issues**
   - **Error**: Multiple data/parameters blocks detected instead of one
   - **Root Cause**: Block insertion creating new blocks instead of merging with existing
   - **Impact**: Invalid Stan syntax structure

---

## 🚀 **NEXT IMMEDIATE TASKS**: Fix Remaining Critical Issues

**GOAL**: Resolve duplicate declarations, missing variables, and multivariate stanvar conflicts

**KEY FILES TO MODIFY**:
- `R/stan_assembly.R`: Block filtering, variable creation, multivariate stanvar naming

### **Task 1: Fix Duplicate Parameter Declarations (Priority 1)**

**D1**: Enhance `filter_block_content()` to remove ALL duplicate declarations ⚠️ *CRITICAL*
  - ✅ Fixed `lprior` declarations in previous work
  - ❌ **CRITICAL**: Fix duplicate `sigma` declarations causing compilation failure  
  - Add `sigma` parameter declarations to exclusion filter
  - Test with gaussian() and other families that declare sigma in both models
  - Ensure filtering works across all parameter block content

### **Task 2: Fix Missing mu Variable in Standard Path (Priority 1)**

**D2**: Create `mu` variable in standard (non-GLM) trend injection ⚠️ *CRITICAL*
  - **Issue**: GLM path creates `mu = Xc * b`, but standard path assumes `mu` exists
  - **Solution**: Standard path must also create `mu` vector before adding trend effects
  - Implement consistent `mu` creation across both injection approaches
  - Ensure both paths use efficient computation patterns

### **Task 3: Fix Duplicated Stanvar Names in Multivariate Models (Priority 2)** - *ARCHITECTURAL REDESIGN NEEDED*

**D3**: ⚠️ **REQUIRES ARCHITECTURAL CHANGES** - Complex shared correlation model support
  - **Root Issue**: Shared correlation models (`RW(cor = TRUE)`) generate duplicate shared parameters
  - **Current Problem**: Each response generates complete stanvar set independently, causing duplicates:
    - Duplicated shared: `sigma_trend`, `L_Omega_trend`, `Sigma_trend`, `innovations_trend`, `Z`, `rw_tparameters`
    - Correctly suffixed: `obs_trend_time_count/_biomass`, `K_trend_count/_biomass`
  - **Architectural Challenge**: Need to distinguish shared vs response-specific components:
    - **Shared Components**: Correlation matrices, innovation parameters, factor loadings
    - **Response-Specific**: Observation mappings, computed trend matrices, GLM compatibility
  - **Required Solution**: 
    - Detect shared correlation models in multivariate processing
    - Generate shared components only once (no suffix)
    - Generate response-specific mappings with suffixes
    - Implement shared/specific component separation logic
  - **Current Workaround**: Simple suffix patterns insufficient for shared correlation architecture
  - **Estimated Effort**: 8-12 hours for full architectural redesign

### **Task 4: Fix Multiple Block Structure Issues (Priority 3)**

**D4**: Ensure single block creation instead of duplicates
  - **Issue**: Test detection of 2 data blocks, 2+ parameters blocks instead of 1 each
  - **Root Cause**: Block insertion creating new blocks instead of merging
  - Fix block detection and merging logic in `insert_into_stan_block()`
  - Ensure consistent block structure validation

### **Task 5: VAR Trend Constructor Bug (Priority 3)**  

**D5**: Fix VAR constructor argument parsing 
  - **Error**: `unused argument (cor = TRUE)` in VAR trend constructor
  - **Issue**: Interface mismatch in VAR() function call
  - Update VAR trend constructor to handle correlation argument correctly

### **Task 6: Input Validation Enhancement (Priority 4)**

**D6**: Strengthen input validation for edge cases
  - **Issues**: Several validation tests failing with unexpected behavior
  - Add robust error handling for malformed inputs
  - Improve error messages for user-facing validation functions

**SUCCESS CRITERIA**: 
- ✅ **GLM-optimized path working correctly** (ACHIEVED)
- 🔄 **Generated Stan code compiles without errors for all test cases** (Major progress: 17/86 → 2/86 failing)  
- ✅ **Standard (non-GLM) path creates proper mu variable** (FIXED: trend injection in model block)
- ✅ **No duplicate parameter declarations** (FIXED: enhanced parameter filtering) 
- ⚠️ **Multivariate models handle unique stanvar names** (Needs architectural redesign for shared correlations)
- ❌ **Single block structure maintained** (multiple blocks detected)
- ✅ **Trend effects correctly applied with missing data** (architecture working)
- ✅ **GLM performance optimization preserved** (ACHIEVED)

**MAJOR IMPROVEMENTS ACHIEVED**:
- Fixed duplicate sigma parameter declarations in parameters block
- Fixed missing mu variable by injecting trend effects in model block after mu += lines
- Comprehensive trend injection placement validation added to tests
- 13/17 critical compilation errors resolved (76% improvement)

**TARGET**: **0/86 test failures** - Complete Stan code generation system

---

## 📈 **PROGRESS SUMMARY** 

**✅ COMPLETED MAJOR FEATURES:**
- GLM-compatible trend injection with automatic detection
- Efficient matrix multiplication + trend addition  
- Type safety with `to_matrix(mu)` conversion and `mu_ones` stanvar
- All brms GLM function support (poisson_log_glm, normal_id_glm, etc.)
- Correct lpdf/lpmf usage for continuous vs discrete distributions
- Architecture documentation fully updated

**🚨 PRIORITY AREAS REMAINING:**
1. **P1**: Duplicate sigma parameter declarations (immediate compilation failures)
2. **P1**: Missing mu variable in standard path (50% of models fail)  
3. **P2**: Multivariate stanvar name conflicts (all multivariate models fail)
4. **P3**: Block structure validation and VAR constructor bugs

**ESTIMATED COMPLETION**: **6-8 hours** for remaining issues

---

## 🔄 **FUTURE WORK AFTER CRITICAL FIXES**

### **Enhanced Testing & Validation** 
- Expand test coverage for edge cases and family combinations
- Performance benchmarking against brms-only models
- Systematic validation across all supported trend types

### **Advanced Features**
- Prior specification system completion for remaining trend types
- Enhanced multivariate support with complex trend sharing patterns  
- Integration testing with full mvgam workflow
