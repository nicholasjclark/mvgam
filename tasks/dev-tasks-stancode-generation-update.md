# Stan Code Generation System - Remaining Issues

## NON-NEGOTIABLE WORKFLOW
- Proceed with ONE priority task at a time
- Use the pathfinder agent to read `architecture/stan-data-flow-pipeline.md` and to systematically trace the flow from `stancode()` to complete Stan code creation
- The code-reviewer agent MUST be used to approve of any edits BEFORE they are implemented
- Following any edits to R code, agents MUST:
- 1. Regenerate currents by running `target_generation.R`
- 2. Use parallel general-purpose agents to **READ AND ANALYZE ALL** `current_stancode*` vs `target_stancode*` in the `tasks/` directory
- 3. Adhere to a STRICT TDD approach: no fix is verified until ALL currents have been compared to their respective targets

## CRITICAL SUBAGENT INSTRUCTIONS

**SUBAGENTS MUST ONLY READ AND COMPARE FILES - NEVER CREATE OR MODIFY**

When analyzing `current_stancode*` vs `target_stancode*` files:

- **DO**: Use Read tool to examine BOTH current and target files completely
- **DO**: Compare line-by-line differences between current vs target
- **DO**: Identify specific line numbers where differences occur
- **DO**: Report exact code snippets showing current vs target differences
- **DO**: Assess compilation readiness and identify syntax/logic errors

- **DO NOT**: Create new files
- **DO NOT**: Modify existing files  
- **DO NOT**: Write code to disk
- **DO NOT**: Use Write, Edit, or any file modification tools
- **DO NOT**: Attempt to "fix" files directly

**AGENT TASK**: Your ONLY job is to READ the existing files and report discrepancies with specific line numbers and code snippets.


## COMPLETED ISSUES ✅

### 1. VAR Array Dimension Extraction (stancode_3) - FIXED ✅
**Issue**: VAR(1) arrays generated instead of VAR(2): `array[1]` should be `array[2]`
**Solution**: Fixed `trend_specs$lags %||% 1` to `trend_specs$p %||% 1` in `generate_var_trend_stanvars()`
**Status**: VERIFIED - stancode_3 now shows `array[2]`

### 2. GP Prior Filtering - FIXED ✅  
**Issue**: `std_normal_lpdf(zgp_1)` priors being incorrectly removed by model block filtering
**Solution**: Made filtering regex more specific: `.*normal.*lpdf\\s*\\(` → `.*normal.*lpdf\\s*\\(\\s*Y\\s*\\|`
**Status**: VERIFIED - GP priors now preserved

### 3. Stan Reserved Words - FIXED ✅
**Issue**: `std_normal_lpdf` not in reserved words, causing incorrect renaming to `std_normal_lpdf_trend`
**Solution**: Added std_normal functions (`std_normal_lpdf`, `std_normal_cdf`, `std_normal_rng`) to reserved words
**Status**: VERIFIED - Function names no longer renamed

## CURRENT PRIORITY ISSUES

### 1. GP Prior Parameter Renaming (stancode_6, stancode_8) 🔴 CRITICAL
**Issue**: Trend model GP parameters in priors not renamed `zgp_1` → `zgp_1_trend`
**Evidence**: 
- stancode_8 has `zgp_1_trend` parameter declaration ✅
- stancode_8 missing `target += std_normal_lpdf(zgp_1_trend);` ❌
- stancode_6 same issue
**Impact**: Incomplete prior specification for trend GPs

**INVESTIGATION TASKS**:
- [ ] **Task A**: Trace model block prior extraction from trend models in `extract_and_rename_stan_blocks()`
- [ ] **Task B**: Verify if trend model priors use same renaming pipeline as parameters/tparameters blocks  
- [ ] **Task C**: Check if model block has separate processing path that bypasses parameter renaming
- [ ] **Task D**: Identify specific location where `zgp_1` inside `std_normal_lpdf(zgp_1)` fails to rename
- [ ] **Task E**: Test fix by modifying renaming pipeline and regenerating all current files
- [ ] **Task F**: Comprehensive validation - compare ALL current vs target files after fix

### 2. mu Logic Error (stancode_4) 
**Issue**: `inv()` applied after trend addition for Gamma family
**Problem**: Mathematical logic violation
  
### 3. Hard-coded Prior Parameters (stancode_5)
**Issue**: Using `0.05` instead of `change_scale_trend` data parameter
**Impact**: Ignores user-specified prior scales
---
