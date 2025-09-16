# Stan Code Generation System - Remaining Issues

## NON-NEGOTIABLE WORKFLOW

- Use parallel general-purpose agents to **READ AND ANALYZE ALL** `current_stancode*` vs `target_stancode*` in the `tasks/` directory
- Use the pathfinder agent to read `architecture/stan-data-flow-pipeline.md` and to systematically trace the flow from `stancode()` to complete Stan code creation
- Proceed with ONE priority task at a time
- After proposing fixes, regenerate currents by running `target_generation.R`
- Use parallel general-purpose agents to READ and systematically ANALYZE `current_stancode*` vs `target_stancode*` in the `tasks/` directory again to verify fixes and check for regressions
- TDD approach is crucial, no fix is verified until ALL currents have been compared to their respective targets

## CRITICAL AGENT INSTRUCTIONS

**AGENTS MUST ONLY READ AND COMPARE FILES - NEVER CREATE OR MODIFY**

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

## COMPLETED: Nonlinear Trend Parameter Naming Research ✅

**Status**: PARTIAL FIX IMPLEMENTED 
**Issue**: Fixed `is_multivariate_formula()` detection but parameter naming still incorrect

### Research Results ✅
- **Phase 1**: ✅ Complete trend formula processing pipeline mapped
- **Phase 2**: ✅ Root cause identified in `is_multivariate_formula()` function
- **Phase 3**: ✅ Solution designed and implemented

### Fix Applied ✅
**File**: `brms_integration.R:328-335`
**Function**: `is_multivariate_formula()`
```r
if (inherits(formula, "brmsformula")) {
  # For nonlinear formulas, pforms contain parameter definitions, not responses
  if (is_nonlinear_formula(formula)) {
    return(FALSE)
  }
  return(!is.null(formula$pforms) && length(formula$pforms) > 0)
}
```

**File**: `validations.R:558,566`
**Function**: `is_nonlinear_formula()`
- Fixed assertion to accept `brmsformula` objects
- Fixed detection to check `attr(formula$formula, "nl")`

### Current Status ⚠️
- ✅ `is_nonlinear_formula()` correctly returns `TRUE` for nonlinear formulas  
- ✅ `is_multivariate_formula()` correctly returns `FALSE` for nonlinear formulas
- ❌ **Still generating**: `Intercept_trend_y`, `N_trend_y`, `obs_trend_time_y`
- ✅ **Regression testing**: All other stancode cases preserved correctly

### Remaining Issue
Pipeline still applies multivariate treatment despite correct formula detection. Need to investigate:
- Response suffix application in trend processing
- Stan data extraction routing logic

---

## COMPLETED FIXES ✅

### 1. Missing Innovation Sampling Statements ✅
**File**: `stan_assembly.R:3807-3813`
**Function**: `generate_car_trend_stanvars()`
**Fix**: Added missing `to_vector(innovations_trend) ~ std_normal();` sampling statement for CAR trends
**Result**: All 7 trend types now have correct innovation sampling

### 2. CAR Trend Innovation Sampling ✅  
**Issue**: CAR trends created `innovations_trend` parameter but no sampling statement
**Fix**: Added explicit innovation sampling stanvar in CAR generation
**Result**: stancode_6 and stancode_7 now compile correctly

---

## CURRENT PRIORITY ISSUES

### 1. Nonlinear Pipeline Routing (stancode_9) ⚠️
**Status**: Formula detection fixed, parameter naming still broken
**Issue**: Despite correct `is_multivariate_formula() = FALSE`, pipeline still applies multivariate treatment
**Next**: Investigate trend processing pipeline downstream from formula detection

### 2. Missing N_trend Dimension Declaration (stancode_9) 
**Issue**: Missing `int<lower=1> N_trend;` declaration in data block
**Current**: References undefined `N_trend_y` and undefined `N_y`
**Expected**: Standard `N_trend` and `N` variables

### 3. VAR Array Dimension Extraction (stancode_3)
**Issue**: VAR(1) arrays generated instead of VAR(2): `array[1]` should be `array[2]`
**Impact**: Wrong parameter dimensions for VARMA(2,1) model

### 4. Missing Trend Injection (stancode_9)
**Issue**: Trend effects computed but never added to linear predictor  
**Missing**: `mu[n] += trend[obs_trend_time[n], obs_trend_series[n]];`

### 5. Hard-coded Prior Parameters (stancode_5)
**Issue**: Using `0.05` instead of `change_scale_trend` data parameter
**Impact**: Ignores user-specified prior scales

---

## SECONDARY PRIORITY

- Use parallel general-purpose agents to READ full current_stancode_* files and compare to their respective target_stancode_* files in tasks/. Each agent should:
  - Read architecture/architecture-decisions.md
  - Read architecture/stan-data-flow.md
  - Read target_generation.R
  - Read their respective current_stancode_* files and target_stancode_* files
  - Provide systematic, structured summaries of key discrepancies and where these discrepancies may originate
- Synthesize subagent findings and create a structured priority list of actions needed to 
