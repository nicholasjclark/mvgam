# TRD-stancode-generation-update Development Tasks

## Overview
**CRITICAL PRIORITY**: Fix stancode generation failures that cause 15/23 tests to fail with Stan compilation errors. Root cause: missing data handling system needs obs_ind mapping that brms doesn't provide.

## 📋 COMPLETED FOUNDATION WORK ✅

**Summary**: Complete prior system, parameter extraction, trend generators, and inspection functions (Steps 1-9) with 346+ tests passing across `R/priors.R`, `R/validations.R`, `R/brms_integration.R`, `R/stan_assembly.R`. Key achievements: standardized 3-stanvar pattern, mvgam_formula() interface, centralized prior resolution, and parameter extraction system.

---

## 🚨 **CURRENT CRITICAL ISSUE**: Stan Block Structure Problems

**STATUS**: 1/5 tests failing in enhanced `tests/testthat/test-stancode-standata.R` with Stan compilation errors  
**ROOT CAUSE**: Block insertion utilities creating duplicate blocks instead of using existing brms blocks

### 🔍 ERROR ANALYSIS *(2025-09-01 Updated)*

**Current Status**: Stan block structure violations after DRY utility function refactoring

#### **Primary Issue: Multiple Block Creation**
- **Problem**: Stan code contains duplicate blocks - 2 data blocks, 3 parameters blocks, 2 transformed parameters blocks instead of 1 each
- **Impact**: Stan compilation fails with block ordering violations ("model {" or "generated quantities {" expected after transformed parameters block)
- **Root Cause**: `find_stan_block()` not detecting existing brms blocks, causing `insert_into_stan_block()` to create new blocks instead of inserting into existing ones

#### **Secondary Issues**
- **Block Ordering**: Transformed parameters block appearing AFTER model block (line 85) violates Stan syntax requirements
- **Pattern Matching**: Block detection regex patterns may not match brms-generated block formats

---

## 🚀 **NEXT IMMEDIATE TASKS**: Fix Stan Block Structure Issues

**GOAL**: Eliminate duplicate block creation to restore proper Stan syntax and compilation

**KEY FILES TO MODIFY**:
- `R/stan_assembly.R`: Block detection and insertion utility functions (`find_stan_block`, `insert_into_stan_block`)
- Enhanced test framework with `validate=FALSE` for debugging

### **Task 1: Debug Block Detection (30 min)**

**C0a-fix3a**: Debug why `find_stan_block()` isn't detecting existing brms blocks ✅ *MAJOR PROGRESS*
  - ✅ Read `architecture/stan-data-flow-pipeline.md`
  - ✅ Run tests in `test-stancode-standata.R` and inspect generated Stan code vs expectations
  - ✅ Inspect `tasks/target_stancode_1.stan` to see what the first test SHOULD create
  - ✅ **FIXED: Block detection now correctly finds transformed parameters block (34 lines vs 1 line)**
  - ✅ **FIXED: Injection placement now at END of block (after trend computation)**
  - ✅ **ROOT CAUSE RESOLVED: gregexpr() brace counting bug with fixed=TRUE**
  - ✅ Compare block detection regex patterns against actual brms-generated Stan code structure
  - ✅ Test `find_stan_block()` with sample brms output to verify pattern matching
  - ✅ Ensure patterns handle brms block formatting (spacing, comments, etc.)
  
  **REMAINING ISSUES TO COMPLETE C0a-fix3a:**
  - ❌ **Missing mu variable**: `mu[n] +=` references undeclared variable 
  - ❌ **Missing mu_trend variable**: Referenced in trend computation but not declared in stanvars
  - ❌ **Duplicate lprior declarations**: Both obs and trend models declare `real lprior = 0;`
  - ❌ **Duplicate sigma prior**: Trend model sigma prior should be filtered out
  - Add `real lprior = 0;` and `lprior += student_t_lpdf(sigma` to list of exclusions that should not be retained from the trend brms model
  - Fix `insert_into_stan_block()` to use existing blocks instead of creating duplicates
  - Modify insertion logic to properly add content to existing blocks
  - Ensure content is inserted in correct location within existing block structure
  - Verify braces are properly matched after insertion
  - Ensure `architecture/stan-data-flow-pipeline.md` is accurate and up to date
  **STATUS**: Block structure issues RESOLVED. Variable coordination issues remain.

### **Task 2: Fix Block Ordering (20 min)**

**C0a-fix3b**: Fix Stan block ordering (transformed parameters must come before model)
  - Read `architecture/stan-data-flow-pipeline.md`
  - Ensure block insertion respects Stan's required ordering: data → parameters → transformed parameters → model → generated quantities  
  - Fix insertion logic that places transformed parameters after model block (line 85 issue)
  - Ensure `architecture/stan-data-flow-pipeline.md` is accurate and up to date

### **Task 3: Complete DRY Refactoring (45 min)**

**C0b**: Complete refactoring of `inject_trend_into_linear_predictor()` to use shared utilities
  - Read `architecture/stan-data-flow-pipeline.md`
  - Remove remaining duplicate logic after utility functions are working properly
  - Ensure univariate and multivariate trend injection uses shared block manipulation code
  - Ensure `architecture/stan-data-flow-pipeline.md` is accurate and up to date

**C0c**: Add GLM support as specialization of shared utilities  
  - Read `architecture/stan-data-flow-pipeline.md`
  - Implement GLM-compatible trend injection using the corrected block utilities
  - Handle brms GLM optimization patterns (6 GLM function types from brms source)
  - Ensure `architecture/stan-data-flow-pipeline.md` is accurate and up to date

**C0d**: Remove excess messages during Stan code generation, improve readability  
  - Remove all intermediate messages during Stan code generation
  - Ensure consistent spacing, indentation and alignment of all generated Stan code
  
### **Task 4: Final Validation (15 min)**

**C4**: Final validation of all fixes
  - Run complete test suite with enhanced block structure validation
  - Verify no duplicate blocks, correct ordering, proper compilation

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
