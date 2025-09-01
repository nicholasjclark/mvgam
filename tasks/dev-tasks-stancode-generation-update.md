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
  
- [x] **Sub-task A1**: Analyze brms data processing behavior (30 min) ✅ COMPLETED
  - **FINDINGS**: brms automatically excludes NA responses, preserves input ordering for non-missing obs
  - **KEY INSIGHT**: brms does NOT create `obs_ind` array - we must create our own mapping
  - **IMPLICATION**: Need to track `which(!is.na(y))` and map each obs to trend[time, series] position
  - **TEST CONFIRMED**: Created test scripts verifying brms behavior with missing data patterns
  
- [X] **Sub-task A2**: Design the mapping strategy (30 min)
  - Read `architecture/stan-data-flow-pipeline.md`
  - Choose data structure: `array[N] int obs_trend_linear_idx` vs `array[N] int obs_time_idx; array[N] int obs_series_idx`
  - Decide creation point: during trend stanvar generation vs during injection function
  - Plan missing data handling: systematic missing vs random missing vs no missing
  - Design validation approach to detect misaligned mappings
  - Update `architecture/stan-data-flow-pipeline.md` accordingly

### **Phase 2: Implementation** (120 min)

- [X] **Sub-task B1**: Implement mapping creation infrastructure (45 min)
  - Read `architecture/stan-data-flow-pipeline.md`
  - Create `generate_obs_trend_mapping()` function that takes observation data + trend dimensions
  - Generate mapping array: for each observation n → corresponding trend[time_idx, series_idx] position
  - Handle edge cases: single series, single time point, irregular time series
  - Add comprehensive input validation and error handling
  - Update `architecture/stan-data-flow-pipeline.md` accordingly
  
- [X] **Sub-task B2**: Integrate mapping into stanvar generation pipeline (45 min)
  - Read `architecture/stan-data-flow-pipeline.md`
  - Add mapping creation to `extract_trend_stanvars_from_setup()` or equivalent function
  - Ensure mapping gets included as "data" block stanvar in trend_stanvars collection
  - Pass observation data through pipeline to mapping creation point
  - Add mapping metadata to trend specifications for downstream usage
  - Update `architecture/stan-data-flow-pipeline.md` accordingly
  
- [X] **Sub-task B3**: Update injection logic to use mapping (30 min)
  - Read `architecture/stan-data-flow-pipeline.md`
  - Replace `obs_ind` references with `obs_trend_mapping` in `inject_trend_into_linear_predictor()`
  - Support both 2D matrix access `trend[time_idx, series_idx]` and linear indexing strategies
  - Add validation that mapping array exists and has correct dimensions
  - Preserve existing injection logic structure while eliminating undefined variable dependency
  - Update `architecture/stan-data-flow-pipeline.md` accordingly

### **Phase 3: Architecture Refactoring and Testing** (90 min)

- [x] **Sub-task C1**: Refactor mapping generation into extract_time_series_dimensions() (45 min) ✅ COMPLETED
  - Modified `extract_time_series_dimensions()` to accept `response_vars` parameter (required, not optional)
  - Generate mappings within dimension extraction: call `generate_obs_trend_mapping()` for each response
  - Return mappings in dimensions result structure alongside existing fields
  - Updated function signature and documentation for new centralized approach

- [x] **Sub-task C2**: Update calling code to use cleaner mapping architecture (30 min) ✅ COMPLETED  
  - Updated `validate_time_series_for_trends()` to pass response variables to dimension extraction
  - Modified `extract_trend_stanvars_from_setup()` to extract mappings from dimensions.mappings instead of generating separately
  - Removed complex parameter threading: `obs_data` parameter no longer needed
  - Updated multivariate and univariate calling code to simpler structure without extra parameters

- [ ] **Sub-task C3.1**: Investigate stanvar class inheritance issues (20 min)
  - Use debug script to examine why `extract_trend_stanvars_from_setup()` returns objects with `class: list` instead of proper `stanvar` objects
  - Trace through stanvar creation pipeline to identify where class inheritance is broken
  - Check if issue is in trend generator functions or in the collection/combination process
  - Document specific functions that need stanvar class fixes

- [ ] **Sub-task C3.2**: Fix Stan code block corruption (30 min)
  - Investigate why invalid stanvars cause brms to generate duplicated model/parameters blocks
  - Test brms stanvar processing with properly classed stanvar objects vs invalid ones
  - Fix stanvar creation to ensure proper class inheritance and block assignment
  - Verify fix prevents parameter declarations appearing in model block

- [ ] **Sub-task C3.3**: Validate complete stanvar pipeline (15 min)
  - Run enhanced debug script to verify all stanvars have proper `stanvar` class
  - Confirm parameters appear only in parameters block, not model block
  - Ensure mapping arrays (`obs_trend_time`, `obs_trend_series`) are present for injection
  - Test that fixed stanvars resolve both injection failures and Stan compilation errors
  - Run all tests in `test-stancode-standata.R` to validate complete fix

**SUCCESS CRITERIA**: 
- ✅ Generated Stan code compiles without errors  
- ✅ Trend effects correctly applied even with missing data
- ✅ No performance regression for complete data cases
- ✅ Robust handling of various missing data patterns

---

## 🛠️ **DEBUG TOOLS & VALIDATION**

### **Debug Script: `debug_stanvar_pipeline.R`**
**Purpose**: Comprehensive diagnostic tool to trace stanvar generation and identify Stan code corruption issues

**Key Capabilities**:
- **Step-by-step pipeline tracing**: Follows exact test pattern from mvgam_formula → stancode generation
- **Detailed stanvar inspection**: Analyzes each stanvar's class, block assignment, and content
- **Stan code structure validation**: Detects duplicated blocks, parameters in wrong locations
- **Corruption detection**: Identifies when stanvars cause brms to generate invalid Stan code
- **Mapping array verification**: Confirms presence of `obs_trend_time`/`obs_trend_series` for injection

**Current Findings**:
- ❌ **Root Issue**: `extract_trend_stanvars_from_setup()` returns 16 objects with `class: list` instead of proper `stanvar` objects
- ❌ **Stan Corruption**: Invalid stanvars cause brms to generate duplicate model/parameters blocks
- ❌ **Parameter Misplacement**: Parameters like `sigma_trend` appear in model block instead of parameters block
- ❌ **Missing Mapping Arrays**: No `obs_trend_time`/`obs_trend_series` arrays found, causing injection failure

**Usage for Validation**:
```r
# Run after each sub-task to verify fixes
Rscript debug_stanvar_pipeline.R

# Check for success indicators:
# ✓ All stanvars have "stanvar" class
# ✓ Parameters only in parameters block  
# ✓ No duplicate Stan blocks
# ✓ Mapping arrays present
```

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
