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

## CURRENT PRIORITY TASKS

### FUNDAMENTAL PROBLEM: Attribute-Based Time and Series Variables for Universal Grouping

## WHY WE NEED ATTRIBUTE-BASED VARIABLES (Essential Understanding for Junior Developers)

**Universal Principle**: mvgam **ALWAYS** groups trend data by `(time, series)` using **attribute-based accessors** for ALL operations:

1. **Covariate Invariance Validation**: Check that trend covariates are constant within each (time, series) combination
2. **Trend Data Extraction**: Reduce data to unique (time, series) combinations for Stan `matrix[N_trend, K_trend] X_trend`
3. **Prediction Processing**: Apply identical grouping logic to newdata

**The Scenario 3 Problem Revealed**:
```r
# target_generation.R line 26 shows the real issue:
#series = factor(rep(paste0("series", 1:n_series), each = n_time)), ← COMMENTED OUT!

# Scenario 3 setup:
mf3 <- mvgam_formula(
  bf(mvbind(count, biomass) ~ s(x)) + set_rescor(FALSE),
  trend_formula = ~ presence + VAR(p = 2, ma = TRUE) 
)

# Test data structure:
# - 72 observations, 24 time points
# - NO series column (commented out in target_generation.R)
# - Multivariate responses: count, biomass

# Current failure:
# extract_trend_data() tries to group by (time, series) → ERROR: no series column
# Expected: CREATE series via attributes, then group using attribute accessors → correct dimensions
```

**The Clean Solution**:
- **Store time and series as attributes** - never contaminate original data
- **Universal accessor functions** for all grouping operations
- **Same attribute creation logic** for both fitting and prediction contexts

## IMPLEMENTATION STRATEGY: Attribute-Based Variable System

**Key Insight**: Store time and series variables as attributes, access via universal functions

**Universal Interface**:
```r
# All downstream code uses these - never direct data access:
get_time_for_grouping(data)    # Returns attr(data, "mvgam_time")
get_series_for_grouping(data)  # Returns attr(data, "mvgam_series") 

# All grouping becomes:
data %>% group_by(
  time = get_time_for_grouping(data),
  series = get_series_for_grouping(data)
)
```

**Three Series Creation Strategies** (stored as attributes):
1. **Explicit Series**: `attr(data, "mvgam_series") <- data[[series_var]]`
2. **Hierarchical Series**: `attr(data, "mvgam_series") <- interaction(data[[gr_var]], data[[subgr_var]], sep='_')`  
3. **Missing Series**: `attr(data, "mvgam_series") <- create_from_multivariate_structure(data, response_vars)`

**Two Contexts Requiring Identical Logic**:
1. **Fitting Context**: `data + trend_formula` → create attributes before processing
2. **Prediction Context**: `mvgam_object + newdata` → apply same attribute creation to newdata

### Sub-Task A1: ✅ COMPLETED - Create Universal Variable Attribute System

**Objective**: Add core function to create time and series attributes, handling all three series creation strategies

**File**: `R/validations.R`
**Location**: Added after line 2534 (before `extract_trend_data()`)

**Function Added**:
```r
# Universal attribute-based variable system for mvgam grouping
ensure_mvgam_variables <- function(data, parsed_trend = NULL, time_var = "time", series_var = "series", response_vars = NULL) {
  checkmate::assert_data_frame(data)
  checkmate::assert_string(time_var)
  checkmate::assert_string(series_var)
  
  # Always create implicit time mapping for consistency (ALL MODELS)
  checkmate::assert_names(names(data), must.include = time_var)
  unique_times <- unique(data[[time_var]])
  time_mapping <- setNames(seq_along(unique_times), unique_times)
  attr(data, "mvgam_time") <- time_mapping[as.character(data[[time_var]])]
  attr(data, "mvgam_time_source") <- "implicit"
  attr(data, "mvgam_original_time") <- data[[time_var]]  # Store original for distance calculations
  
  # Three series creation strategies
  series_values <- NULL
  series_source <- NULL
  
  # Strategy 1: Hierarchical series (gr + subgr present)
  if (!is.null(parsed_trend$trend_model)) {
    gr_var <- if (!is.null(parsed_trend$trend_model$gr) && parsed_trend$trend_model$gr != "NA") parsed_trend$trend_model$gr else NULL
    subgr_var <- if (!is.null(parsed_trend$trend_model$subgr) && parsed_trend$trend_model$subgr != "NA") parsed_trend$trend_model$subgr else NULL
    
    if (!is.null(gr_var) && !is.null(subgr_var)) {
      checkmate::assert_names(names(data), must.include = c(gr_var, subgr_var))
      series_values <- interaction(data[[gr_var]], data[[subgr_var]], drop = TRUE, sep = '_', lex.order = TRUE)
      series_source <- "hierarchical"
    }
  }
  
  # Strategy 2: Explicit series (column exists)
  if (is.null(series_values) && series_var %in% names(data)) {
    series_values <- data[[series_var]]
    series_source <- "explicit"
  }
  
  # Strategy 3: Missing series (create from multivariate structure)
  if (is.null(series_values)) {
    if (!is.null(response_vars) && length(response_vars) > 1) {
      # Multivariate case: create series from response structure
      n_obs_per_response <- nrow(data) / length(response_vars)
      if (n_obs_per_response == floor(n_obs_per_response)) {
        series_values <- factor(rep(response_vars, each = n_obs_per_response))
        series_source <- "multivariate"
      } else {
        stop(insight::format_error(
          "Cannot create series from multivariate structure.",
          "Data has {nrow(data)} observations but {length(response_vars)} responses.",
          "Expected equal observations per response for automatic series creation."
        ), call. = FALSE)
      }
    } else {
      stop(insight::format_error(
        "No series variable found in data.",
        "Either provide {.field {series_var}} column, hierarchical grouping variables (gr and subgr), or specify response_vars for multivariate series creation."
      ), call. = FALSE)
    }
  }
  
  # Store series as attribute
  attr(data, "mvgam_series") <- series_values
  attr(data, "mvgam_series_source") <- series_source
  
  return(data)
}
```

**✅ COMPLETED**:
- Function added to R/validations.R at lines 2536-2600
- **Universal implicit time**: ALL models now use sequential time indices (1, 2, 3, ...) with original time preserved 
- Handles all three series creation strategies via attributes
- Uses checkmate validation per CLAUDE.md standards
- Works for both fitting and prediction contexts

### Sub-Task A2: ✅ COMPLETED - Add Universal Accessor Functions

**Objective**: Add accessor functions to retrieve time and series from attributes

**File**: `R/validations.R`
**Location**: Added after `ensure_mvgam_variables()` function

**Functions Added**:
```r
#' Get time variable for grouping operations
#' 
#' Retrieves implicit time indices from data attributes for consistent grouping
#' 
#' @param data Data frame with mvgam time attributes
#' @return Numeric vector of sequential time indices (1, 2, 3, ...)
#' @noRd
get_time_for_grouping <- function(data) {
  checkmate::assert_data_frame(data, min.rows = 1)
  
  time_values <- attr(data, "mvgam_time")
  if (is.null(time_values)) {
    stop(insight::format_error(
      "No time variable attribute found.",
      "Call ensure_mvgam_variables() first to create time attributes."
    ), call. = FALSE)
  }
  
  return(time_values)
}

#' Get series variable for grouping operations
#' 
#' Retrieves series values from data attributes for consistent grouping
#' 
#' @param data Data frame with mvgam series attributes
#' @return Factor or character vector of series identifiers
#' @noRd
get_series_for_grouping <- function(data) {
  checkmate::assert_data_frame(data, min.rows = 1)
  
  series_values <- attr(data, "mvgam_series")
  if (is.null(series_values)) {
    stop(insight::format_error(
      "No series variable attribute found.",
      "Call ensure_mvgam_variables() first to create series attributes."
    ), call. = FALSE)
  }
  
  return(series_values)
}

#' Check if mvgam variables are ready
#' 
#' Verifies that both time and series attributes exist on data object
#' 
#' @param data Data frame to check for mvgam attributes
#' @return Logical indicating if both time and series attributes exist
#' @noRd
has_mvgam_variables <- function(data) {
  checkmate::assert_data_frame(data)
  !is.null(attr(data, "mvgam_time")) && !is.null(attr(data, "mvgam_series"))
}

#' Remove mvgam variable attributes
#' 
#' Cleans up all mvgam-related attributes from data object
#' 
#' @param data Data frame with mvgam attributes to remove
#' @return Data frame with mvgam attributes removed
#' @noRd
remove_mvgam_variables <- function(data) {
  checkmate::assert_data_frame(data)
  attr(data, "mvgam_time") <- NULL
  attr(data, "mvgam_time_source") <- NULL
  attr(data, "mvgam_original_time") <- NULL
  attr(data, "mvgam_series") <- NULL
  attr(data, "mvgam_series_source") <- NULL
  return(data)
}
```

**✅ COMPLETED**:
- All accessor functions added to R/validations.R at lines 2602-2671
- **Roxygen documentation**: All functions have @noRd tags and proper documentation
- **Enhanced checkmate validation**: Added `min.rows = 1` validation, `response_vars` parameter validation
- Proper error handling using `insight::format_error()`
- Helper functions for checking and cleanup included
- Follows CLAUDE.md validation patterns

### Sub-Task A3: ✅ COMPLETED - Update extract_trend_data() Fitting Context

**Objective**: Modify extract_trend_data() to use attribute-based variables in fitting context

**✅ COMPLETED**:
- Modified `extract_trend_data()` to use attribute-based variables in fitting context
- Implemented universal (time, series) grouping using `get_time_for_grouping()` and `get_series_for_grouping()`
- Applied dplyr best practices: mutate-first pattern with `.data` pronouns
- Fixed series validation to only require `time_var` (series can be created via attributes)
- Fixed CAR validation to allow covariates for univariate models (n_series = 1)
- Attributes cleaned up after processing using `remove_mvgam_variables()`

### Sub-Task A3.1: 🔄 IN PROGRESS - Fix response_vars Parameter Threading

**Objective**: Thread `response_vars` parameter through pipeline to enable multivariate series creation

**✅ COMPLETED Threading**:
- Extended `parse_trend_formula()` to accept `response_vars` parameter
- Updated `extract_time_series_dimensions()` calls to pass `response_vars`
- Modified `generate_obs_trend_mapping()` to accept and use `response_vars`
- Extended `ensure_mvgam_variables()` to handle prediction context with metadata

**🔄 REMAINING ISSUE - Critical**: 
**ALL DOWNSTREAM FUNCTIONS must use attribute-based accessors after `ensure_mvgam_variables()` is called**

**PROBLEM**: Multiple functions still try to access `data[[time_var]]` and `data[[series_var]]` directly after attributes are created, causing:
- "undefined columns selected" errors
- "No time variable attribute found" errors  
- Data generation failures preventing X_trend dimension fixes

**MANDATORY REQUIREMENT**: After `ensure_mvgam_variables(data, ...)` is called, ALL subsequent code must use:
- ❌ **NEVER**: `data[[time_var]]` or `data[[series_var]]` 
- ✅ **ALWAYS**: `get_time_for_grouping(data)` and `get_series_for_grouping(data)`

**Remaining Locations to Fix** (found via `grep "data\[\[.*_var\]\]"`):
- `R/validations.R:1613-1614`: `obs_trend_time <- match(obs_data[[time_var]], ...)`
- `R/validations.R:2595,2597,2599`: Direct time access in `ensure_mvgam_variables()`  
- `R/validations.R:396,2127,2139`: Series validation checks
- Multiple other validation functions

**Evidence**: Scenarios 6-7 (CAR) work, but 1-5, 8-9 fail because they hit these remaining direct access points.

## PRIORITY: Eliminate Redundant extract_time_series_dimensions() Calls via Dependency Injection

**THE CORE PROBLEM**: `extract_time_series_dimensions()` is expensive (validates data, creates attributes, computes dimensions) and gets called 2-3 times with identical inputs:
- Path 1: `extract_trend_data()` → `parse_trend_formula()` → `extract_time_series_dimensions()`
- Path 2: `validate_time_series_for_trends()` → `extract_time_series_dimensions()`
- Path 3: `generate_obs_trend_mapping()` → `extract_time_series_dimensions()` (conditional)

**WHY THIS MATTERS**: 
- Performance waste: Same expensive computation 2-3 times per request
- Inconsistency risk: Multiple computations could theoretically yield different results
- Maintenance burden: Changes must be validated across multiple call paths

**THE DRY SOLUTION: Dependency Injection**
Compute dimensions ONCE at the top level, pass them down to all functions that need them. No caching, no R6, just clean parameter passing.

**Architecture**:
```
extract_and_validate_trend_components()
├── extract_time_series_dimensions() ← COMPUTE ONCE HERE
├── extract_trend_data(..., .precomputed_dimensions = dimensions)
├── validate_time_series_for_trends(..., .precomputed_dimensions = dimensions)
└── All functions receive dimensions as parameter, never recompute
```

### Sub-Task B1: ✅ COMPLETED - Modify extract_and_validate_trend_components to compute dimensions once

**Objective**: Make our consolidated function compute dimensions ONCE and pass them to all sub-functions

**File**: `R/validations.R`
**Function**: `extract_and_validate_trend_components()` starting at line 2862

**WHY**: This function is the single entry point for trend processing. Computing dimensions here and passing them down eliminates ALL redundant calls.

**Actions**:
1. Add dimensions computation at the beginning (after parameter validation):
```r
# Compute dimensions ONCE - this is the expensive operation we don't want to repeat
dimensions <- extract_time_series_dimensions(
  data, time_var, series_var, 
  trend_type = if (is_multivariate_trend_specs(mv_spec$trend_specs)) {
    first_spec <- mv_spec$trend_specs[[1]]
    first_spec$trend_type %||% first_spec$trend_model %||% first_spec$trend
  } else {
    mv_spec$trend_specs$trend_type %||% mv_spec$trend_specs$trend_model %||% mv_spec$trend_specs$trend
  },
  trend_specs = mv_spec$trend_specs,
  response_vars = response_vars,
  cached_formulas = mv_spec$cached_formulas
)
```

2. Modify the extract_trend_data call to use precomputed dimensions:
```r
result <- extract_trend_data(
  data, mv_spec$base_formula, time_var, series_var,
  response_vars = response_vars, 
  .return_metadata = TRUE,
  .precomputed_dimensions = dimensions  # Pass dimensions to avoid recomputation
)
```

3. Modify the validate_time_series_for_trends call to use precomputed dimensions:
```r
validation_result <- validate_time_series_for_trends(
  data, 
  mv_spec$trend_specs,
  response_vars = response_vars,
  cached_formulas = mv_spec$cached_formulas,
  .precomputed_dimensions = dimensions  # Pass dimensions to avoid recomputation
)
```

**Expected result**: Dimensions computed exactly ONCE per request

### Sub-Task B2: ✅ COMPLETED - Update extract_trend_data to accept precomputed dimensions

**Objective**: Modify function to use precomputed dimensions instead of triggering recomputation

**File**: `R/validations.R`  
**Function**: `extract_trend_data()` starting at line 2947

**WHY**: This function currently calls parse_trend_formula() which calls extract_time_series_dimensions(). We need to bypass this redundant call.

**Actions**:
1. Add new parameter `.precomputed_dimensions = NULL` to function signature
2. Modify the logic to use precomputed dimensions when available:
```r
if (!is.null(.precomputed_dimensions)) {
  # Use precomputed dimensions - skip parse_trend_formula call
  trend_variables <- .precomputed_dimensions$metadata$covariates %||% character(0)
  dimensions <- .precomputed_dimensions
} else {
  # Existing logic for backward compatibility
  parsed_trend <- parse_trend_formula(trend_formula, data = data, response_vars = response_vars)
  trend_variables <- parsed_trend$trend_variables %||% character(0)
  dimensions <- parsed_trend$dimensions
}
```

**Expected result**: Function uses precomputed dimensions, avoiding Path 1's redundant call

### Sub-Task B3: ✅ COMPLETED - Update validate_time_series_for_trends to accept precomputed dimensions

**Objective**: Modify function to use precomputed dimensions instead of calling extract_time_series_dimensions again

**File**: `R/validations.R`
**Function**: `validate_time_series_for_trends()` starting at line 1187

**WHY**: This function currently calls extract_time_series_dimensions() at line 1227. We need to use precomputed dimensions instead.

**Actions**:
1. Add new parameter `.precomputed_dimensions = NULL` to function signature
2. Modify the dimensions extraction logic:
```r
if (!is.null(.precomputed_dimensions)) {
  # Use precomputed dimensions - skip extraction
  dimensions <- .precomputed_dimensions
} else {
  # Existing logic for backward compatibility
  dimensions <- extract_time_series_dimensions(
    data = data,
    time_var = time_var,
    # ... existing parameters
  )
}
```

**Expected result**: Function uses precomputed dimensions, avoiding Path 2's redundant call

### Sub-Task B4: ✅ COMPLETED - Update parse_trend_formula to accept precomputed dimensions

**Objective**: Modify function to use precomputed dimensions instead of calling extract_time_series_dimensions

**File**: `R/trend_system.R`
**Function**: `parse_trend_formula()` around line 1697

**WHY**: This function calls extract_time_series_dimensions() at line 1854. When called from extract_trend_data with precomputed dimensions, it should skip this.

**Actions**:
1. Add new parameter `.precomputed_dimensions = NULL` to function signature
2. Modify the dimensions computation logic around line 1854:
```r
if (!is.null(.precomputed_dimensions)) {
  # Use precomputed dimensions
  trend_model$dimensions <- .precomputed_dimensions
  trend_model$metadata <- .precomputed_dimensions$metadata
} else if (!is.null(data)) {
  # Only compute if not provided
  dimensions <- extract_time_series_dimensions(...)
  trend_model$dimensions <- dimensions
  trend_model$metadata <- dimensions$metadata
}
```

**Expected result**: Function uses precomputed dimensions when available

### Sub-Task B5: ✅ COMPLETED - Test dependency injection implementation

**Objective**: Verify that extract_time_series_dimensions() is called exactly ONCE per request

**Actions**:
1. Add debug logging to extract_time_series_dimensions() to count calls:
```r
if (Sys.getenv("MVGAM_DEBUG") == "TRUE") {
  .mvgam_dimension_call_count <<- get0(".mvgam_dimension_call_count", ifnotfound = 0) + 1
  cat("[DEBUG] extract_time_series_dimensions call #", .mvgam_dimension_call_count, "\n")
}
```
2. Run `Rscript target_generation.R` with `MVGAM_DEBUG=TRUE`
3. Verify each scenario shows exactly ONE call to extract_time_series_dimensions
4. Confirm all scenarios generate successfully
5. Remove debug counter code after verification

**Expected result**: Performance improvement from single dimension computation per request

## NEXT PRIORITY: Comprehensive Stancode and Standata Validation

### Sub-Task C1: Parallel Subagent Analysis of All Generated Stan Files (45 minutes)

**Objective**: Deploy parallel general-purpose subagents to comprehensively analyze all `current_stancode_*.stan` and `current_standata_*.rds` files in the `tasks/` directory to identify any discrepancies between Stan code declarations and actual data objects.

**WHY THIS IS CRITICAL**: 
- All 9 models now generate successfully (✅ confirmed)
- Need to validate that Stan code declares correct data structures
- Need to verify standata provides exactly what Stan code expects
- Must catch dimension mismatches, missing variables, type errors before compilation

**Architecture**:
```
Deploy 9 parallel subagents (one per model):
├── Agent 1: Analyze current_stancode_1.stan + current_standata_1.rds
├── Agent 2: Analyze current_stancode_2.stan + current_standata_2.rds  
├── ...
└── Agent 9: Analyze current_stancode_9.stan + current_standata_9.rds
```

**Each Subagent Must**:
1. **READ** complete Stan code file (`.stan`)
2. **READ** complete Stan data file (`.rds`)  
3. **Extract** all data declarations from `data {}` block
4. **Verify** each declared variable exists in standata with correct:
   - Dimensions (e.g., `matrix[N_trend, K_trend] X_trend` → check `dim(standata$X_trend)`)
   - Type (int, real, vector, matrix, array)
   - Bounds (lower=0, upper=1, etc.)
5. **Identify** any missing variables, dimension mismatches, or type errors
6. **Report** findings with specific line numbers and variable names

**Subagent Instructions Template**:
```markdown
TASK: Comprehensive Stan Code vs Stan Data Validation for Model {N}

FILES TO ANALYZE:
- Stan code: C:\Users\uqnclar2\OneDrive - The University of Queensland\Desktop\mvgam\tasks\current_stancode_{N}.stan
- Stan data: C:\Users\uqnclar2\OneDrive - The University of Queensland\Desktop\mvgam\tasks\current_standata_{N}.rds

REQUIREMENTS:
1. READ both files completely
2. Extract ALL variable declarations from `data {}` block in Stan code
3. Load standata RDS file and verify each declared variable exists with correct dimensions/type
4. Report any discrepancies with specific details:
   - Variable name
   - Expected (from Stan code)  
   - Actual (from standata)
   - Line number in Stan code where declared

DO NOT modify files - READ and ANALYZE only.
Provide comprehensive report of all findings.
```

**Expected Deliverables**:
- 9 detailed reports identifying any Stan code/standata discrepancies
- Specific variable names, dimensions, and line numbers for all issues found
- Confirmation of which models have correct Stan code/standata alignment
- Priority list of any issues that need immediate attention

**Task Complete When**:
- All 9 models analyzed by parallel subagents
- Comprehensive validation reports received
- Any critical discrepancies identified and prioritized
- Stan compilation readiness assessed for all models

### Sub-Task A4: Handle Prediction Context with Attributes (15 minutes)

**Objective**: Ensure prediction context applies same attribute-based logic to newdata

**File**: `R/validations.R`
**Location**: Lines 2584-2591 (prediction context section)

**Add After Line 2591** (after grouping_vars extraction):
```r
# EXISTING (lines 2584-2591):
# Get grouping variables from metadata
grouping_vars <- character(0)
if (!is.null(metadata$variables$gr_var) && metadata$variables$gr_var != "NA") {
  grouping_vars <- c(grouping_vars, metadata$variables$gr_var)
}
if (!is.null(metadata$variables$subgr_var) && metadata$variables$subgr_var != "NA") {
  grouping_vars <- c(grouping_vars, metadata$variables$subgr_var)
}

# ADD AFTER (new lines):
# Apply same attribute creation to newdata using stored metadata
if (length(grouping_vars) >= 2) {
  # Hierarchical case: reconstruct parsed_trend structure
  dummy_parsed_trend <- list(
    trend_model = list(
      gr = metadata$variables$gr_var,
      subgr = metadata$variables$subgr_var
    )
  )
  data <- ensure_mvgam_variables(data, dummy_parsed_trend, time_var, series_var)
} else {
  # Use stored series source to determine strategy
  series_source <- metadata$series_source %||% "explicit"
  if (series_source == "multivariate") {
    # Apply same multivariate series creation
    response_vars <- metadata$response_vars
    data <- ensure_mvgam_variables(data, NULL, time_var, series_var, response_vars)
  } else {
    # Explicit series case
    data <- ensure_mvgam_variables(data, list(trend_model = NULL), time_var, series_var)
  }
}
```

**Also Update Metadata Storage** (add to the metadata object around line 2780):
```r
# ADD to metadata creation:
metadata <- list(
  # ... existing fields ...
  time_source = attr(data, "mvgam_time_source"),  # Always "implicit" for universal time mapping
  series_source = attr(data, "mvgam_series_source"),
  response_vars = response_vars  # Store for prediction context
)
```

**Task Complete When**:
- Prediction context applies identical attribute creation to newdata
- Stored metadata enables consistent series reconstruction
- Both hierarchical and multivariate cases handled in prediction
- newdata processed identically to original fitting data

### Sub-Task A5: Update Architecture Documentation (15 minutes)

**Objective**: Update architecture documents to reflect attribute-based variable system

**Files to Update**:
1. `architecture/architecture-decisions.md`
2. `architecture/stan-data-flow-pipeline.md`

**Changes to architecture-decisions.md** (Section 5: Variable Name Management Architecture):
```markdown
### 5. Attribute-Based Variable System Architecture

**Design Decision**: Use attribute-based time and series variable storage for universal (time, series) grouping without data contamination.

**Key Principles**:
- **Universal Grouping**: All trend processing uses (time, series) grouping via attribute accessors
- **Zero Contamination**: Original data never modified - variables stored as attributes only
- **Universal Implicit Time**: ALL models use sequential time indices (1, 2, 3, ...) with original time preserved
- **Three Series Strategies**: Explicit, hierarchical, and multivariate series creation
- **Dual Context**: Same logic for fitting (data + trend_formula) and prediction (mvgam_object + newdata)

**Implementation Pattern**:
```r
# Step 1: Create attributes for time and series
data <- ensure_mvgam_variables(data, parsed_trend, time_var, series_var, response_vars)

# Step 2: Universal grouping using accessors
data %>% group_by(
  time = get_time_for_grouping(data),      # Sequential indices (1, 2, 3, ...)
  series = get_series_for_grouping(data)   # Explicit/hierarchical/multivariate
)

# Step 3: Clean up attributes
data <- remove_mvgam_variables(data)
```

**Time Creation Strategy (Universal)**:
- **Implicit Time**: `attr(data, "mvgam_time") <- sequential indices mapped from unique times`
- **Original Preserved**: `attr(data, "mvgam_original_time") <- data[[time_var]]` for CAR distance calculations

**Series Creation Strategies**:
- **Explicit**: `attr(data, "mvgam_series") <- data[[series_var]]`
- **Hierarchical**: `attr(data, "mvgam_series") <- interaction(gr, subgr, sep='_')`
- **Multivariate**: `attr(data, "mvgam_series") <- factor(rep(response_vars, each = n_obs_per_response))`
```

**Changes to stan-data-flow-pipeline.md** (add new section):
```markdown
### Stage 2.5: Attribute-Based Variable Standardization

**Purpose**: Ensure universal (time, series) grouping using attribute-based variables with consistent time indexing

**Process**:
1. `extract_trend_data()` calls `ensure_mvgam_variables()` 
2. Creates universal implicit time: `attr(data, "mvgam_time")` with sequential indices (1, 2, 3, ...)
3. Preserves original time: `attr(data, "mvgam_original_time")` for CAR distance calculations
4. Creates series: `attr(data, "mvgam_series")` using appropriate strategy (explicit/hierarchical/multivariate)
5. All grouping uses `get_time_for_grouping()` and `get_series_for_grouping()`
6. Attributes cleaned up after processing via `remove_mvgam_variables()`

**Benefits**:
- **Consistent Stan indexing**: All models use sequential time indices
- **CAR model support**: Original time values available for distance matrices
- **Scenario 3 fix**: Gets series from multivariate structure → X_trend[24, 2] correct dimensions
```

**Task Complete When**:
- Both architecture documents updated with attribute-based system
- All three series creation strategies documented
- Examples show universal interface usage

### Sub-Task A6: Validation and Testing (15 minutes)

**Objective**: Verify scenario 3 fix and ensure no regressions in existing scenarios

**Steps**:
1. Run `target_generation.R` to test the new attribute system
2. Check that scenario 3 creates series from multivariate structure
3. Verify all scenarios work with attribute-based grouping
4. Test both fitting and prediction contexts

**Commands to Run**:
```bash
# Test the current implementation
Rscript target_generation.R

# Check scenario 3 dimensions specifically
Rscript -e "data3 <- readRDS('tasks/current_standata_3.rds'); cat('X_trend dimensions:', dim(data3$X_trend), '\n')"

# Check all generated data files exist
ls -la tasks/current_standata_*.rds
```

**Task Complete When**:
- Scenario 3: X_trend has dimensions [24, 2] (corrected from [72, 2])
- All scenarios generate without errors
- Both explicit and multivariate series creation work
- No regressions in existing functionality

**DELIVERABLE**: Attribute-based variable system that enables universal (time, series) grouping, fixing scenario 3 multivariate series creation while maintaining clean separation from original data.

