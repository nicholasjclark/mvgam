# Stan Code Generation System - Remaining Tasks

## NON-NEGOTIABLE WORKFLOW
- Proceed with ONE priority task at a time
- Use the pathfinder agent to read `architecture/stan-data-flow-pipeline.md` and to systematically trace the flow from `stancode()` to complete Stan code creation
- The code-reviewer agent MUST be used to approve of any edits BEFORE they are implemented
- Following any edits to R code, agents MUST:
  1. Regenerate currents by running `target_generation.R`
  2. Use parallel general-purpose agents to **READ AND ANALYZE ALL** `current_stancode*` vs `target_stancode*` in the `tasks/` directory
  3. Adhere to a STRICT TDD approach: no fix is verified until ALL currents have been compared to their respective targets

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

**AGENT TASK**: Your ONLY job is to read the existing files and report discrepancies with specific line numbers and code snippets.

## TOP PRIORITY: Enhanced Dual-Context Trend Covariate System [INVESTIGATION COMPLETE]

### Context from Investigation
**Current System Analysis**:
- ✅ `validate_trend_covariates()`, `validate_trend_invariance()`, `extract_trend_data()` exist in R/validations.R
- ✅ `all.vars()` correctly handles complex expressions: `log(temp + 1)` → `"temp"`, `poly(temp, 2)` → `"temp"`
- ❌ No hierarchical grouping (gr/subgr) support in validation 
- ❌ No CAR formula restrictions enforced
- ❌ No trend metadata storage for prediction contexts
- ❌ Scattered validation calls need centralization

**Key Design Requirements**:
- **No Backward Compatibility**: Complete redesign acceptable
- **Dual-Context Support**: Same system handles fitting and prediction  
- **Metadata Storage**: Store trend requirements in fitted objects for prediction validation
- **Clean Integration**: Single entry point replaces scattered validation calls

### Priority -5: **Extract and Store Trend Constructor Metadata**
**TASK**: Parse complete trend metadata from constructors for dual-context use
**SCOPE**:
- Enhance `parse_trend_formula()` to extract gr/subgr from evaluated trend constructors  
- Extract required covariates from regular_terms using existing `all.vars()` pattern
- Create comprehensive metadata structure: grouping variables, covariates, trend type
- Handle both shared trends and response-specific trend collections
- Return structured metadata for storage in fitted objects
**IMPLEMENTATION PATTERN**:
```r
# Returns: list(
#   trend_type = "AR", gr = "site", subgr = "plot", 
#   covariates = c("temp", "rain"), is_car = FALSE,
#   grouping_structure = "hierarchical" | "series_only"
# )
```
**DELIVERABLE**: Complete trend metadata extraction system
**TIME LIMIT**: 15 minutes

### Priority -4: **Design Dual-Context extract_trend_data() System**
**TASK**: Create unified data extraction for both fitting and prediction contexts
**SCOPE**:
- **Fitting Mode**: `extract_trend_data(data, trend_formula, time_var, series_var)`
- **Prediction Mode**: `extract_trend_data(fitted_object, newdata)`
- Auto-detect context based on first argument type (data.frame vs mvgam object)
- Use stored metadata from fitted objects for prediction validation
- Apply appropriate grouping: (time, series) vs (time, gr) vs (time, gr, subgr)
- Handle missing combinations and irregular data appropriately
**IMPLEMENTATION PATTERN**:
```r
extract_trend_data <- function(x, ...) {
  if (is.data.frame(x)) {
    # Fitting context: x is data, ... contains trend_formula, etc.
  } else if (inherits(x, "mvgam")) {
    # Prediction context: x is fitted object, ... contains newdata
    # Use x$trend_metadata for validation
  }
}
```
**DELIVERABLE**: Unified extraction system supporting both contexts
**TIME LIMIT**: 15 minutes

### Priority -3: **Implement Hierarchical Grouping Validation**  
**TASK**: Enhance invariance validation for gr/subgr grouping structures
**SCOPE**:
- Modify `validate_trend_invariance()` to use metadata-driven grouping
- **Series-only grouping**: Current (time, series) validation
- **Hierarchical grouping (gr only)**: Validate invariance within (time, gr) groups
- **Nested grouping (gr + subgr)**: Validate invariance within (time, gr, subgr) groups
- Skip validation entirely for CAR models (no shared latent states)
- Clear error messages explaining grouping requirements for shared latent states
**DELIVERABLE**: Enhanced validation supporting all grouping patterns
**TIME LIMIT**: 15 minutes

### Priority -2: **Add CAR Model Formula Restrictions and Special Handling**
**TASK**: Enforce CAR-specific limitations and skip inappropriate validation
**SCOPE**:
- Detect CAR constructors during trend parsing  
- If CAR + non-empty regular_terms: error "CAR models cannot include trend covariates due to irregular time requirements"
- Skip `validate_trend_invariance()` entirely for CAR models
- Document that CAR uses series-specific evolution (no shared latent states)
- Handle CAR in dual-context extraction (no grouping beyond series)
**DELIVERABLE**: Complete CAR special handling with clear restrictions
**TIME LIMIT**: 10 minutes

### Priority -1: **Create validate_trend_setup() Master Function**
**TASK**: Centralize all validation logic with metadata return
**SCOPE**:
- Single entry point replacing all scattered validation calls
- Extract metadata from trend specifications automatically
- Apply appropriate validation based on trend type (CAR vs others)
- Return both validated trend data AND complete metadata structure  
- Handle response-specific vs shared trend validation paths
- Store metadata in format suitable for fitted object storage
**IMPLEMENTATION PATTERN**:
```r
validate_trend_setup <- function(data, trend_formula, response_vars, time_var, series_var) {
  metadata <- extract_trend_metadata(trend_formula)
  if (metadata$is_car && length(metadata$covariates) > 0) stop("CAR restrictions...")
  # Apply validation with metadata$grouping_structure
  trend_data <- extract_trend_data(data, trend_formula, time_var, series_var)
  list(trend_data = trend_data, metadata = metadata)
}
```
**DELIVERABLE**: Complete centralized validation with metadata output
**TIME LIMIT**: 15 minutes

### Priority 0: **Integrate Metadata Storage in mvgam Objects**
**TASK**: Store trend metadata in fitted objects for prediction use
**SCOPE**:
- Modify mvgam fitting pipeline to store validation metadata
- Add `trend_metadata` field to mvgam object structure
- Include: grouping variables, required covariates, trend specifications
- Enable prediction-time validation using stored metadata
- Update object creation in `create_mvgam_from_combined_fit()` or similar
**DELIVERABLE**: Fitted objects with complete trend metadata for prediction
**TIME LIMIT**: 10 minutes

### Priority 1: **Replace Scattered Validation Calls**
**TASK**: Clean integration of centralized system throughout codebase  
**SCOPE**:
- Replace validation calls in R/brms_integration.R setup_brms_lightweight()
- Replace validation calls in R/make_stan.R generation functions
- Use returned metadata for downstream processing
- Remove redundant validation functions if no longer needed
- Ensure clean flow from validation → metadata storage → prediction capability
**DELIVERABLE**: Complete integration with no scattered validation calls
**TIME LIMIT**: 10 minutes

### Priority 2: **Implement Prediction-Time Validation**
**TASK**: Validate newdata against stored trend requirements
**SCOPE**:
- Use stored metadata to validate newdata structure in prediction contexts
- Check required covariates present with same names/types
- Validate grouping variable structure matches training data
- Ensure time/series variables compatible with fitted model
- Clear error messages for prediction-time validation failures
- Integrate with existing prediction workflow seamlessly
**DELIVERABLE**: Complete prediction validation using fitted object metadata
**TIME LIMIT**: 15 minutes

### Priority 3: **Create Comprehensive Test Suite**
**TASK**: Test all functionality with focus on dual-context behavior
**SCOPE**:
- Test fitting context: various grouping patterns, CAR restrictions, multivariate models
- Test prediction context: metadata usage, newdata validation, error handling
- Test edge cases: missing groups, complex formulas, response-specific trends
- Test integration: end-to-end fitting → prediction workflow
- Verify no regression in existing functionality
**DELIVERABLE**: Complete test coverage for dual-context system  
**TIME LIMIT**: 15 minutes

### Priority 4: **Documentation and Verification**
**TASK**: Document new system and verify target compliance
**SCOPE**:
- Update roxygen documentation for all modified/new functions
- Document dual-context usage patterns and metadata storage
- Add examples showing prediction workflow with trend covariates
- Run target_generation.R and verify all outputs match targets
- Update architecture documentation with new validation flow
**DELIVERABLE**: Fully documented and verified dual-context system
**TIME LIMIT**: 10 minutes
