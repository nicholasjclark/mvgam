# TRD-stancode-generation-update Development Tasks

## Overview
**CRITICAL PRIORITY**: Fix stancode generation failures that cause 15/23 tests to fail with Stan compilation errors. Root cause: missing data handling system needs obs_ind mapping that brms doesn't provide.

## 📋 COMPLETED FOUNDATION WORK ✅

**Summary**: Complete prior system, parameter extraction, trend generators, and inspection functions (Steps 1-9) with 346+ tests passing across `R/priors.R`, `R/validations.R`, `R/brms_integration.R`, `R/stan_assembly.R`. Key achievements: standardized 3-stanvar pattern, mvgam_formula() interface, centralized prior resolution, and parameter extraction system.

---

## 🚨 **CURRENT CRITICAL ISSUE**: Stancode Generation Failures

**STATUS**: 15/23 tests failing in `tests/testthat/test-stancode-standata.R` with Stan compilation errors  
**ROOT CAUSE**: Missing data alignment between brms observations and trend matrix positions

### 🔍 ERROR ANALYSIS *(2025-08-28)*

**Investigation Complete**: Root cause identified through systematic debugging with `debug_stan_parameter_blocks.R`

#### **Primary Issue: Missing obs_ind Variable**
- **Problem**: `inject_trend_into_linear_predictor()` (R/stan_assembly.R:725) references `obs_ind` that doesn't exist
- **Cause**: brms handles missing data by **exclusion**, not indexing - never creates `obs_ind` array
- **Impact**: Trend injection attempts `mu += trend[obs_ind]` with undefined variable
- **Solution**: Create observation-to-trend mapping during stanvar generation

#### **Secondary Issues**
- **Block placement**: Parameters correctly assigned to stanvar blocks, but text manipulation corrupts final Stan code
- **Missing variables**: `n_trend`, `n_series_trend`, `n_lv_trend` referenced but not declared
- **Text manipulation bugs**: `inject_trend_into_linear_predictor()` creates duplicate blocks and corrupt assignments

---

## 🚀 **NEXT IMMEDIATE TASK**: Implement obs_ind Mapping System

**GOAL**: Create `obs_trend_mapping` during stanvar generation to properly align brms observations with trend matrix positions, eliminating dependency on non-existent `obs_ind`

**KEY FILES TO MODIFY**:
- `R/stan_assembly.R`: Functions `extract_trend_stanvars_from_setup()`, `inject_trend_into_linear_predictor()`  
- `tests/testthat/test-stancode-standata.R`: Target test file with 15/23 failing tests
- `debug_stan_parameter_blocks.R`: Debug script for systematic testing
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
  - Replace `obs_ind` references with `obs_trend_mapping` in `inject_trend_into_linear_predictor()`
  - Support both 2D matrix access `trend[time_idx, series_idx]` and linear indexing strategies
  - Add validation that mapping array exists and has correct dimensions
  - Preserve existing injection logic structure while eliminating undefined variable dependency

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

## 📚 **SUPPORTING CONTEXT & RESOURCES**

### **Debug and Testing Tools**
- **Primary debug script**: `debug_stan_parameter_blocks.R` - systematic Stan code analysis
- **Target test file**: `tests/testthat/test-stancode-standata.R` - 15/23 failing tests to fix
- **Key functions to modify**: `extract_trend_stanvars_from_setup()`, `inject_trend_into_linear_predictor()` in `R/stan_assembly.R`

### **Data Ordering Architecture**
- **mvgam enforces ordering**: `prepare_stan_data()` (R/stan_assembly.R:434) orders data as `series_first, time_within_series`  
- **brms preserves ordering**: Confirmed that brms respects the ordering passed to it
- **Trend matrix structure**: `matrix[n_trend, n_series_trend] trend` where `trend[i,s]` = value for time i, series s
- **times_trend mapping**: `matrix[n_trend, n_series_trend] times_trend` maps positions to time indices

### **Secondary Issues to Address After obs_ind Fix**
- **Duplicate blocks**: Two `transformed parameters` and two `model` blocks being generated by text manipulation
- **Missing variables**: `n_trend`, `n_series_trend`, `n_lv_trend` referenced but not declared in appropriate blocks  
- **Text manipulation bugs**: `inject_trend_into_linear_predictor()` creates self-referencing assignments

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
