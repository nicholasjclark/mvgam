# Trend Constructor Simplification Implementation Plan

## Background: What Problem Are We Solving?

The mvgam trend system currently requires complex downstream decision-making spread across multiple files:

1. **Scattered Logic**: Validation rules, parameter monitoring, and Stan generation dispatch are hardcoded in different functions
2. **Registry Complexity**: Separate registry system requires manual maintenance and can get out of sync
3. **Complex Validation**: Hard-coded conditionals like `if (trend_type == "AR" || trend_type == "VAR")` throughout validation functions
4. **Difficult Extension**: Adding new trends requires updating multiple files and systems

### Our Solution
Make trend constructors **self-contained** - each trend object contains all information needed for validation, Stan generation, and post-processing. Use **convention-based dispatch** to eliminate manual registry management.

---

## Target Architecture: Self-Contained Trend Objects

### Enhanced mvgam_trend Object Structure
```r
# What trend objects will look like after enhancement:
trend_obj <- structure(list(
  # Core identification
  trend = "AR",                    # For dispatch: "AR" → generate_ar_trend_stanvars()
  time = "time",                   # Data column names
  series = "series",
  
  # Self-contained validation rules (no more hard-coded conditionals!)
  validation_rules = c(
    "requires_regular_intervals",
    "supports_factors",
    "supports_hierarchical"
  ),
  
  # Self-contained parameter monitoring (no more separate lists!)
  monitor_params = c("ar1_trend", "sigma_trend", "L_Omega_trend"),
  
  # Self-contained forecasting metadata
  forecast_metadata = list(
    function_name = "forecast_ar_rcpp",
    required_args = c("ar_coefficients", "last_state"),
    max_horizon = Inf
  ),
  
  # Configuration parameters
  p = 1, ma = FALSE, cor = FALSE, n_lv = NULL
), class = "mvgam_trend")
```

### Convention-Based Dispatch
No more manual registry entries! System automatically looks for:
- `trend = "AR"` → calls `generate_ar_trend_stanvars()`
- `trend = "GARCH"` → calls `generate_garch_trend_stanvars()`
- `forecast_metadata$function_name = "forecast_ar_rcpp"` → calls that function

---

## Implementation Steps

### Step 1: Enhanced mvgam_trend Structure ✅

#### 1.1-1.4: Foundation Components ✅
- [x] Field specification documented with snake_case conventions
- [x] Validation rules vocabulary: 11 rules grouped by category  
- [x] Metadata storage structures defined
- [x] Core validation function `validate_mvgam_trend()` implemented

#### 1.5 Helper Functions for Default Values ✅
- [x] Create `get_mvgam_trend_defaults()` function returning universal defaults
- [x] Create `apply_mvgam_trend_defaults()` that fills missing fields
- [x] Create `get_default_validation_rules()` for automatic rule assignment
- [x] Create `create_mvgam_trend()` template function

---

### Step 2: Architectural Separation (Clean Boundaries)

#### 2.1 Simplify Trend Constructors ✅
- [x] **Target**: Constructors become minimal, self-contained objects
- [x] Remove environment-dependent processing from constructors
- [x] Remove cross-parameter validation/modification from constructors  
- [x] Use `create_mvgam_trend()` helper for consistent structure
- [x] Test simplified RW constructor produces equivalent objects

```r
# Target pattern:
RW <- function(time = NA, series = NA, ma = FALSE, n_lv = NULL) {
  create_mvgam_trend("RW", .time = substitute(time), .series = substitute(series), ma = ma, n_lv = n_lv)
}
```

#### 2.2 Move Complex Logic to Appropriate Layers ✅
**Context**: RW and AR constructors simplified, validation and Stan assembly layers enhanced.

**Functions Updated**:
- ✅ `validate_time_series_for_trends()` in R/validations.R - added rule-based dispatch
- ✅ `generate_trend_injection_stanvars()` in R/stan_assembly.R - uses consistent naming
- ✅ `create_mvgam_trend()` exported with consistent parameter handling

**Migrations Completed**:
- [x] **Rule-based validation dispatch**: Created `apply_validation_rules()` system
- [x] **Consistent dispatch naming**: All functions use `trend_type` + suffix pattern  
- [x] **Simplified constructors**: RW and AR use clean `create_mvgam_trend()` pattern
- [x] **Automatic dispatch metadata**: Added `add_consistent_dispatch_metadata()`

---

### Step 3: Update Validation System (Enhanced Context)

#### 3.1 Enhance Validation with Data Context
- [ ] Move `validate_grouping_arguments()` calls to validation layer
- [ ] Move `validate_correlation_requirements()` to validation layer  
- [ ] Update `validate_time_series_for_trends()` to handle complex logic

#### 3.2 Rule-Based Validation Dispatch
- [ ] Create mapping: rule name → validation function
- [ ] Use `trend_obj$validation_rules` instead of hardcoded trend type checks
- [ ] Enable automatic validation based on trend object rules

---

### Step 4: Update Stan Assembly (Processing Layer)

#### 4.1 Move Parameter Processing to Stan Assembly
- [ ] Move `process_trend_params()` calls from constructors
- [ ] Move dynamic characteristic determination to stanvar generation
- [ ] Generate required parameters based on validated data structure

#### 4.2 Convention-Based Dispatch System
- [ ] Create `get_stanvar_generator_function(trend_type)` 
- [ ] Pattern: `"AR" → generate_ar_trend_stanvars()`
- [ ] Eliminate hardcoded function name fields in trend objects

---

### Step 5: Test and Validate Architectural Changes

#### 5.1 Test Simplified Constructors
- [ ] Start with RW constructor as proof-of-concept
- [ ] Ensure simplified constructor produces equivalent objects
- [ ] Test that downstream pipeline still works

#### 5.2 Update Remaining Constructors  
- [ ] Apply same pattern to AR, VAR, CAR, PW, ZMVN
- [ ] Test each constructor individually
- [ ] Ensure all integration points still function

## Success Criteria

1. **Adding new trend**: Requires only 10-15 lines of code using template
2. **No hard-coded lists**: All validation/monitoring based on trend object rules
3. **Convention-based**: No manual registry entries needed
4. **Clean separation**: Constructors create objects, validation/Stan assembly handle processing

---

## Current Status: Steps 1-4 Complete, Ready for Testing ✅

**Completed**:
- ✅ **Step 1**: Enhanced mvgam_trend structure with validation rules vocabulary
- ✅ **Step 2**: Simplified RW and AR constructors using `create_mvgam_trend()` 
- ✅ **Step 3**: Rule-based validation dispatch system implemented
- ✅ **Step 4**: Consistent function naming throughout system

**Key Achievements**:
- `create_mvgam_trend()` exported with consistent parameter handling (`.time`, `.series`, `.gr`, `.subgr`)
- All dispatch uses base `trend` type (no regex parsing needed)
- Automatic dispatch metadata generation ensures consistency
- Rule-based validation eliminates hardcoded trend type checks

**Next Task: Simplify Remaining Constructors**

### Constructors to Update (follow AR pattern):
  1. **VAR** - ✅ COMPLETED - Now returns "VAR" using create_mvgam_trend() pattern
  2. **PW** - ✅ COMPLETED - Now returns "PW" using create_mvgam_trend() pattern
     - **What was completed**: Simplified PW constructor from 92 lines to 22 lines
     - **Key changes**: 
       - Now returns base "PW" type instead of "PWlinear"/"PWlogistic" 
       - Uses `create_mvgam_trend()` with `.cap` parameter support
       - Replaced custom validation with checkmate assertions
       - Complex logic moved to validation/Stan assembly layers
       - Added `.cap` parameter support to `create_mvgam_trend()` function
     - **Missing validation functions added**: `validate_mvgam_trend()`, `validate_proportional()`, `validate_pos_integer()`, `validate_pos_real()`
     - **Testing**: Constructor works correctly, returns proper base type
  3. **CAR** - Needs simplification
  4. **ZMVN** - Needs simplification

## Task Implementation
- One sub-task at a time: DO NOT start the next sub-task until you ask the user for permission and they say "yes" or "y". 
- Add relevant tests to `tests/testthat/test-trend-registry.R`, `tests/testthat/test-trend-dispatcher.R` or `tests/testthat/test-stan-assembly-system.R`and ensure no tests are out of date.
- Be extremely careful to ensure you do not duplicate existing functionality and you remove old functionality if no longer needed.
- When you finish a sub-task, immediately mark it as completed by changing [ ] to [x].
- Summarize any new information that should be added or modified in `active/architecture-design.md` and `active/quick-reference.md`.
- Review this plan and ensure it provides enough context for agents to complete the next task without exceeding 300 lines total (be clear and direct).

## AI Instructions
When working with task lists, the AI must:

1. Regularly update the task list file after finishing any significant work.
2. You MUST use the `code-reviewer` agent to review any proposed changes to R code (NON-NEGOTIABLE)
3. Follow the completion protocol:
   - Mark each finished **sub‑task** `[x]`.
   - Mark the **parent task** `[x]` once **all** its subtasks are `[x]`.
4. Regularly use thinking to ensure implementations guarantee extendability, modularity and code simplicity.
5. Add newly discovered tasks.
6. Keep `active/architecture-decisions.md` accurate, up to date and less than 600 lines total.
7. Before starting work, check which sub‑task is next.
8. After implementing a sub‑task, update the file and then pause for user approval.
