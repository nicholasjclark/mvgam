# TRD-stancode-generation-update Development Tasks

## 📋 COMPLETED FOUNDATION WORK ✅

**Summary**: Complete prior system, parameter extraction, trend generators, and inspection functions (Steps 1-9) with 346+ tests passing across `R/priors.R`, `R/validations.R`, `R/brms_integration.R`, `R/stan_assembly.R`. Key achievements: standardized 3-stanvar pattern, mvgam_formula() interface, centralized prior resolution, and parameter extraction system.

---

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

**D1.2**: ✅ **COMPLETED**:  Fix times_trend array structure in generated stanvars  
  - Updated test expectations from matrix to array
  - **Gold Standard**: `tasks/target_stancode_1.stan:18` shows `array[n_trend, n_series_trend] int times_trend`
  - **TDD Validation**: Pattern matching should work correctly

**D1.3**: ✅ **COMPLETED**:  FFix innovations_trend matrix parameter declaration
  - **TDD Approach**: Tests expect `matrix[n_trend, n_lv_trend] innovations_trend;`
  - **Gold Standard**: See `tasks/target_stancode_1.stan:37` for exact parameter declaration
  - **Current**: May be declaring as different type or missing entirely
  - **Fix**: Ensure proper matrix parameter in RW trend generator
  - **File**: `R/trend_generators/rw_trend.R` or equivalent
  - **TDD Validation**: Run failing tests - parameter block should contain correct declaration

### **Task 2: Fix AR Trend Specific Issues (Priority 1)** ⚠️ *15-min tasks*

**D2.1**: ✅ **COMPLETED**:  Fix AR lag parameter generation for seasonal models
  - **TDD Approach**: Tests expect `ar1_trend` and `ar12_trend` for AR(p=c(1,12))
  - **Gold Standard**: Compare generated code against expected AR patterns in tests
  - **Current**: May be generating incorrect parameter names or missing parameters
  - **Fix**: Update AR trend generator to handle vector p values correctly
  - **Files**: `R/trend_system.R`, `R/stan_assembly.R`
  - **TDD Validation**: `expect_true(grepl("ar1_trend", code))` and `ar12_trend` should pass

**D2.2**: ✅ **COMPLETED**: Fix AR initialization patterns for seasonal models
  - **TDD Approach**: Tests expect "// Initialize first 12 time points" and `for (i in 1:12)`
  - **Gold Standard**: When uncertain about AR patterns, reference similar structures in target Stan files
  - **Fix Applied**: AR trend generator already correctly implemented initialization patterns 
  - **File**: `R/stan_assembly.R:2795-2798` - initialization code working correctly
  - **TDD Validation**: Tests show initialization comments and loops are present and working

**D2.3**: ✅ **COMPLETED**: Fix AR dynamics starting point calculation
  - **TDD Approach**: Tests expect `for (i in 13:n_trend)` for AR(p=c(1,12))
  - **Gold Standard**: Check target files for similar dynamic loop patterns
  - **Fix Applied**: AR dynamics correctly start at max(p) + 1 (line 2801: `for (i in {max_lag + 1}:n_trend)`)
  - **File**: `R/stan_assembly.R:2800-2805` - dynamics calculation working correctly
  - **TDD Validation**: Generated Stan code shows correct dynamics loop starting at time point 13

### **Task 3: Fix Multivariate Formula Parsing Issues (Priority 2)** ✅ **COMPLETED**

**D3.1**: ✅ **COMPLETED**: Fix mvbrmsformula parsing in multivariate trends
  - **TDD Approach**: Error `Assertion on 'formula' failed: Must be a formula, not mvbrmsformula/bform`
  - **Gold Standard**: Check `tasks/target_stancode_3.stan` for multivariate VARMA structure
  - **Tests failing**: All multivariate tests with `bf(mvbind(...)) + set_rescor(FALSE)`
  - **Fix Applied**: Updated `parse_multivariate_trends()` to accept mvbrmsformula objects directly
  - **File**: `R/brms_integration.R:145-155` - simplified validation logic
  - **TDD Validation**: Multivariate tests no longer error during parsing

**D3.2**: ✅ **COMPLETED**: Extract observation formula from mvbrmsformula objects
  - **TDD Approach**: Need to extract base formula from brms formula wrappers
  - **Gold Standard**: Reference multivariate target files for expected structure
  - **Fix Applied**: Removed incorrect extraction logic; helper functions handle mvbrmsformula directly
  - **File**: `R/brms_integration.R` 
  - **TDD Validation**: Can process `bf(mvbind(count, biomass) ~ x)` correctly

**D3.3**: ✅ **COMPLETED**: Handle set_rescor(FALSE) in observation models
  - **TDD Approach**: Tests use `+ set_rescor(FALSE)` which needs parsing
  - **Gold Standard**: When uncertain about brms integration, check target Stan files for expected patterns
  - **Fix Applied**: Updated `setup_brms_lightweight()` to accept brms formula objects
  - **File**: `R/brms_integration.R:25-32`
  - **TDD Validation**: Formulas with rescor settings now work correctly

### **Task 4: Fix Factor Model Structure Issues (Priority 3)** ⚠️ *15-min tasks*

**D4.1**: Fix ZMVN factor model trend data generation
  - **TDD Approach**: Tests expect `matrix[n_trend, K_trend] X_trend` for trend covariates
  - **Gold Standard**: Check `tasks/target_stancode_4.stan` for factor AR model structure
  - **Current**: Missing trend design matrix variables in ZMVN models
  - **Fix**: Ensure trend formula processing includes design matrix for factor models
  - **File**: `R/trend_generators/zmvn_trend.R` or equivalent
  - **TDD Validation**: Factor model tests should find trend design matrix

**D4.2**: ✅ **COMPLETED**: Fix Z factor loading matrix construction
  - **TDD Approach**: Tests expect `vector[n_series_trend * n_lv_trend] Z_raw` and constrainted construction
  - **Gold Standard**: See `tasks/target_stancode_4.stan:91` for exact Z_raw and constraint patterns
  - **Fix Applied**: Completely rewrote `generate_factor_model()` in `R/stan_assembly.R:2207-2245`
  - **Key Changes**: Added Z_raw parameter declaration, identifiability constraint construction, removed outdated LV_raw references
  - **Architecture Cleanup**: Fixed prior targets to use Z_raw instead of constructed Z matrix
  - **TDD Validation**: ✅ Z_raw found, ✅ LV_raw correctly removed, ✅ Z matrix construction found

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
- 🎉 **MAJOR PROGRESS ACHIEVED**: Reduced from ~201 to 135 test failures (66+ tests fixed!)
- ✅ **Fixed Test Infrastructure**: Syntax errors and regex patterns corrected  
- ✅ **D1.1-D1.3 COMPLETED**: RW trend generation working correctly
- ✅ **D2.1-D2.3 COMPLETED**: AR seasonal patterns working (initialization, dynamics, parameter naming)
- ✅ **D3.1-D3.3 COMPLETED**: Multivariate formula parsing fixed  
- ✅ **D4.2 COMPLETED**: Factor model Z_raw construction implemented, LV_raw removed
- ✅ **Architecture Cleanup**: Removed outdated parameter references and improved Stan generation
- 🔄 **Remaining Work**: 135 failures still to address (mainly ARMA models and remaining factor patterns)
- 🔄 **Stan Syntax Validation**: Most generated code compiles with rstan::stanc()

**IMPLEMENTATION APPROACH**:
1. **Follow TDD Strictly**: Each 15-minute task targets specific test failures
2. **Verify Against Target Stan Files**: Use tasks/target_stancode_*.stan as reference
3. **Incremental Validation**: Run tests after each task to verify progress
4. **Pattern Consistency**: Ensure universal patterns work across all trend types

**CURRENT STATUS UPDATE**:
- 🎉 **Major Progress**: 328 tests passing, 135 failures remaining (was ~201)
- ✅ **Target Stan Files Validated**: All 6 target files pass rstan::stanc() syntax checks
- ✅ **Key Architecture Issues Resolved**: Factor model Z_raw generation, AR trend patterns, formula parsing
- 🎯 **Next Priority**: ARMA model implementation and remaining factor model edge cases
- 📈 **Success Rate**: ~71% test pass rate achieved (328 pass / 463 total)

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
