# TRD-stancode-generation-update Development Tasks

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

## 🚀 **NEXT IMMEDIATE TASKS**: Fix Test Failures Using TDD Approach

**CURRENT STATUS**: 82 test failures, 308 passes - systematic fixes needed based on test failure analysis

## 📋 **CRITICAL TDD DEVELOPMENT PROTOCOL** ⚠️

**ALL developers and agents MUST follow these TDD principles for every task:**

### **1. Test-First Development**
- **BEFORE coding**: Run failing tests to understand exact expectations
- **Command**: `Rscript -e "devtools::load_all(); testthat::test_file('tests/testthat/test-stancode-standata.R')"`
- **Focus**: Target specific failing test patterns, not general implementation

### **2. Gold Standard Reference** 🏆
- **ALWAYS check**: `tasks/target_stancode_*.stan` files for uncertainty resolution
- **These files are VALIDATED**: All 6 target files pass `rstan::stanc()` syntax checks
- **Reference mapping**:
  - `target_stancode_1.stan` → RW trends (basic structure)
  - `target_stancode_2.stan` → Shared RW trends (multivariate)
  - `target_stancode_3.stan` → VARMA trends (complex functions)
  - `target_stancode_4.stan` → Factor AR trends (Z matrix patterns)
  - `target_stancode_5.stan` → PW trends (Prophet functions)
  - `target_stancode_6.stan` → CAR trends (GP + irregular time)

### **3. Validation Cycle**
- **After each fix**: Re-run specific failing tests
- **Success metric**: Test expectations should pass
- **Stan validation**: Generated code must compile with `rstan::stanc()`

### **4. When Uncertain**
- **DON'T guess** - Check target Stan files for exact patterns
- **DON'T assume** - Verify against test expectations
- **DO reference** - Use validated target files as implementation guide

**KEY FILES TO MODIFY**:
- `R/stan_assembly.R`: Trend stanvar generation, parameter extraction
- `R/brms_integration.R`: Formula parsing for multivariate models  
- `R/trend_system.R`: Individual trend type implementations
- `R/validations.R`: Validation and data preparation functions

### **Task 1: Fix Core RW Trend Generation Issues (Priority 1)** ⚠️ *15-min tasks*

**D1.1**: ✅ **COMPLETED**: Fix trend formula parsing to exclude design matrix when no predictors
  - **Solution**: Modified `extract_univariate_standata()` to only include data objects with stancode declarations
  - **Root Cause**: brms generates K, Kc, X, Xc in standata but NOT in stancode for intercept-only models
  - **Fix Applied**: Added declaration checking in `R/stan_assembly.R:5275` with code reviewer approval
  - **Validation**: Tests 81-85 now pass - design matrix variables properly excluded from intercept-only models
  - **Enhanced Coverage**: Added comprehensive design matrix checks to VARMA test for positive validation

**D1.2**: Fix times_trend array structure in generated stanvars  
  - ✅ **COMPLETED**: Updated test expectations from matrix to array
  - **Gold Standard**: `tasks/target_stancode_1.stan:18` shows `array[n_trend, n_series_trend] int times_trend`
  - **TDD Validation**: Pattern matching should work correctly

**D1.3**: Fix innovations_trend matrix parameter declaration
  - **TDD Approach**: Tests expect `matrix[n_trend, n_lv_trend] innovations_trend;`
  - **Gold Standard**: See `tasks/target_stancode_1.stan:37` for exact parameter declaration
  - **Current**: May be declaring as different type or missing entirely
  - **Fix**: Ensure proper matrix parameter in RW trend generator
  - **File**: `R/trend_generators/rw_trend.R` or equivalent
  - **TDD Validation**: Run failing tests - parameter block should contain correct declaration

### **Task 2: Fix AR Trend Specific Issues (Priority 1)** ⚠️ *15-min tasks*

**D2.1**: Fix AR lag parameter generation for seasonal models
  - **TDD Approach**: Tests expect `ar1_trend` and `ar12_trend` for AR(p=c(1,12))
  - **Gold Standard**: Compare generated code against expected AR patterns in tests
  - **Current**: May be generating incorrect parameter names or missing parameters
  - **Fix**: Update AR trend generator to handle vector p values correctly
  - **File**: `R/trend_generators/ar_trend.R`
  - **TDD Validation**: `expect_true(grepl("ar1_trend", code))` and `ar12_trend` should pass

**D2.2**: Fix AR initialization patterns for seasonal models
  - **TDD Approach**: Tests expect "// Initialize first 12 time points" and `for (i in 1:12)`
  - **Gold Standard**: When uncertain about AR patterns, reference similar structures in target Stan files
  - **Current**: Missing proper initialization comments and loops
  - **Fix**: Add initialization logic for max(p) time points in AR generator
  - **File**: AR trend generator transformed parameters block
  - **TDD Validation**: Run tests - initialization comments and loops should be present

**D2.3**: Fix AR dynamics starting point calculation
  - **TDD Approach**: Tests expect `for (i in 13:n_trend)` for AR(p=c(1,12))
  - **Gold Standard**: Check target files for similar dynamic loop patterns
  - **Current**: Starting dynamics at wrong time point
  - **Fix**: AR dynamics should start at max(p) + 1
  - **File**: AR trend generator implementation
  - **TDD Validation**: Run specific test - dynamics loop should start at correct time point

### **Task 3: Fix Multivariate Formula Parsing Issues (Priority 2)** ⚠️ *15-min tasks*

**D3.1**: Fix mvbrmsformula parsing in multivariate trends
  - **TDD Approach**: Error `Assertion on 'formula' failed: Must be a formula, not mvbrmsformula/bform`
  - **Gold Standard**: Check `tasks/target_stancode_3.stan` for multivariate VARMA structure
  - **Tests failing**: All multivariate tests with `bf(mvbind(...)) + set_rescor(FALSE)`
  - **Fix**: Update `parse_multivariate_trends()` to handle brms formula objects
  - **File**: `R/brms_integration.R` - modify assertion and processing logic
  - **TDD Validation**: Multivariate tests should not error during parsing

**D3.2**: Extract observation formula from mvbrmsformula objects
  - **TDD Approach**: Need to extract base formula from brms formula wrappers
  - **Gold Standard**: Reference multivariate target files for expected structure
  - **Fix**: Add extraction logic for mvbrmsformula → base formula
  - **File**: `R/brms_integration.R` 
  - **TDD Validation**: Can process `bf(mvbind(count, biomass) ~ x)` correctly

**D3.3**: Handle set_rescor(FALSE) in observation models
  - **TDD Approach**: Tests use `+ set_rescor(FALSE)` which needs parsing
  - **Gold Standard**: When uncertain about brms integration, check target Stan files for expected patterns
  - **Fix**: Process brms formula modifiers properly
  - **File**: `R/brms_integration.R`
  - **TDD Validation**: Run multivariate tests - formulas with rescor settings should work

### **Task 4: Fix Factor Model Structure Issues (Priority 3)** ⚠️ *15-min tasks*

**D4.1**: Fix ZMVN factor model trend data generation
  - **TDD Approach**: Tests expect `matrix[n_trend, K_trend] X_trend` for trend covariates
  - **Gold Standard**: Check `tasks/target_stancode_4.stan` for factor AR model structure
  - **Current**: Missing trend design matrix variables in ZMVN models
  - **Fix**: Ensure trend formula processing includes design matrix for factor models
  - **File**: `R/trend_generators/zmvn_trend.R` or equivalent
  - **TDD Validation**: Factor model tests should find trend design matrix

**D4.2**: Fix Z factor loading matrix construction
  - **TDD Approach**: Tests expect `vector[n_series_trend * n_lv_trend] Z_raw` and constrainted construction
  - **Gold Standard**: See `tasks/target_stancode_4.stan:91` for exact Z_raw and constraint patterns
  - **Current**: Missing factor loading parameter generation
  - **Fix**: Add Z_raw parameter and constraint logic in factor model generation
  - **File**: Factor model stanvar generator
  - **TDD Validation**: Factor loading construction should match test patterns

**D4.3**: Fix Stan compilation error "Identifier 'LV_raw' not in scope"
  - **TDD Approach**: Stan code references undefined parameter `LV_raw`
  - **Gold Standard**: Check all target Stan files - none should reference undefined parameters
  - **Fix**: Correct parameter name or add missing parameter declaration
  - **File**: Factor model Stan code generation
  - **TDD Validation**: Stan syntax validation should pass for factor models

### **Task 5: Fix ARMA Trend Implementation (Priority 4)** ⚠️ *15-min tasks*

**D5.1**: Fix ARMA parameter generation for complex lag patterns
  - **TDD Approach**: Tests expect `ar2_trend`, `ar4_trend`, `theta1_trend` for AR(p=c(2,4), ma=TRUE)
  - **Gold Standard**: When uncertain about ARMA patterns, reference similar MA/AR structures in target Stan files
  - **Current**: May be generating ar1_trend instead of correct lag parameters
  - **Fix**: ARMA trend generator should handle non-consecutive AR lags correctly
  - **File**: `R/trend_generators/arma_trend.R`
  - **TDD Validation**: Run ARMA tests - correct AR lag parameters should be generated

**D5.2**: Fix MA transformation integration with AR dynamics  
  - **TDD Approach**: Tests expect `ma_innovations_trend` matrix and MA transformation loop
  - **Gold Standard**: Check target Stan files for MA transformation patterns and integration
  - **Current**: Missing MA processing in ARMA models
  - **Fix**: Add MA innovation transformation before AR dynamics
  - **File**: ARMA transformed parameters block generation
  - **TDD Validation**: Run ARMA tests - MA transformation patterns should be present

**SUCCESS CRITERIA (Test-Driven Development)**: 
- 🎯 **PRIMARY TARGET**: Reduce from 82 to 0 test failures
- ✅ **Fixed Test Infrastructure**: Syntax errors and regex patterns corrected  
- 🔄 **RW Trend Tests Pass**: Core trend generation (lines 55-134)
- 🔄 **AR Seasonal Tests Pass**: Complex lag patterns (lines 137-212) 
- 🔄 **ARMA Model Tests Pass**: MA integration (lines 214-299)
- 🔄 **Factor Model Tests Pass**: Z matrix construction (lines 301-398)  
- 🔄 **Multivariate Tests Pass**: Formula parsing (lines 400+)
- 🔄 **Stan Syntax Validation**: All generated code compiles with rstan::stanc()

**IMPLEMENTATION APPROACH**:
1. **Follow TDD Strictly**: Each 15-minute task targets specific test failures
2. **Verify Against Target Stan Files**: Use tasks/target_stancode_*.stan as reference
3. **Incremental Validation**: Run tests after each task to verify progress
4. **Pattern Consistency**: Ensure universal patterns work across all trend types

**CURRENT STATUS UPDATE**:
- ✅ **Test Framework Working**: 308 tests passing, clear failure patterns identified  
- ✅ **Target Stan Files Validated**: All 6 target files pass rstan::stanc() syntax checks
- 📊 **Failure Analysis Complete**: Systematic mapping of 82 failures to implementation gaps
- 🎯 **Task Prioritization**: Each task addresses 5-15 related test failures

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
