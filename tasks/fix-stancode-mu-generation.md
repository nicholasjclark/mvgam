# Fix Stan Code mu Generation Issues

## Problem Statement

The Stan code generation system is incorrectly handling intercepts in trend formulas and potentially other mu construction issues in observation models. This affects the correctness of generated Stan code across multiple model types.

## Workflow Process (NON-NEGOTIABLE)
1. **Acquire Package Context:** Read `architecture/architecture-decisions.md` and `architecture/quick-reference.md`
2. Use the pathfinder agent to trace the full mu generation and glm detection logic, for both univariate and multivariate models

## Specific Issues Identified

### 1. Unwanted Trend Intercepts
**Problem**: Trend formulas are getting intercepts added even when explicitly excluded.

**Example**:
```r
# This should NOT have an intercept in the trend model
make_stancode(
  mvgam_formula(
    y ~ x,
    trend_formula = ~ RW()  # Default includes intercept
  ),
  data = test_data$univariate,
  family = poisson()
)

# This should definitely NOT have an intercept  
make_stancode(
  mvgam_formula(
    y ~ x,
    trend_formula = ~ -1 + RW()  # Explicit no-intercept
  ),
  data = test_data$univariate,
  family = poisson()
)
```

**Expected Behavior**:
- `~ RW()` should include trend intercept by default
- `~ -1 + RW()` should exclude trend intercept completely
- `~ 0 + RW()` should exclude trend intercept completely

### 2. Observation Model mu Construction
**Problem**: Need to verify that observation models correctly construct mu linear predictors for:
- Simple fixed effects models
- Models with random effects  
- Models with smooths
- Models with interactions
- Distributional models (trends only on mu parameter)

### 3. Multivariate Model mu Construction  
**Problem**: Need to verify multivariate models correctly construct separate mu vectors for each response:
- `mvbind(y1, y2) ~ x` should create `mu_y1` and `mu_y2`
- `bf(y1 ~ x) + bf(y2 ~ z)` should create response-specific linear predictors
- Trend effects should be added correctly to each response

## Required Verification Tasks

### Task 1: Trend Intercept Control Verification
**Objective**: Ensure trend formulas respect intercept specifications.

**Models to Test** (from `fit_and_save_models.R`):
- **fit1**: `y ~ x, trend_formula = ~ RW()` → Should have `Intercept_trend` 
- **Test variant**: `y ~ x, trend_formula = ~ -1 + RW()` → Should NOT have `Intercept_trend`
- **Test variant**: `y ~ x, trend_formula = ~ 0 + RW()` → Should NOT have `Intercept_trend`

**Verification Steps**:
1. Generate Stan code for each variant
2. Search for `Intercept_trend` parameter declaration
3. Search for `Intercept_trend` usage in `mu_trend` construction
4. Verify `mu_trend = rep_vector(0.0, N_trend)` when no intercept
5. Verify `mu_trend = rep_vector(Intercept_trend, N_trend)` when intercept included

### Task 2: Observation Model mu Verification
**Objective**: Ensure observation models construct mu correctly across all patterns.

**Models to Test**:
- **fit1**: Basic fixed effects (`y ~ x`) 
- **fit6**: Random effects (`y ~ (1 | series)`)
- **fit8**: GP effects (`y ~ gp(x, k = 5)`)
- **fit3**: Smooth effects (`mvbind(count, biomass) ~ s(x)`)
- **fit9**: Nonlinear effects (`y ~ b1 * exp(b2 * x)`)

**Verification Points**:
1. **Fixed effects**: `mu += Xc * b` or GLM equivalent
2. **Intercept handling**: `mu += Intercept` when included  
3. **Random effects**: Proper `r_*` and `J_*` patterns
4. **Smooths**: `Zs_*` basis matrices and `zs_*` coefficients
5. **GP effects**: `gp_*` patterns and predictions
6. **GLM optimization**: Use of `*_glm_lpmf` functions when possible

### Task 3: Multivariate Model mu Verification
**Objective**: Ensure multivariate models create proper response-specific linear predictors.

**Models to Test**:
- **fit2**: `mvbind(count, biomass) ~ x` → Should create `mu_count`, `mu_biomass`
- **fit3**: `mvbind(count, biomass) ~ s(x)` → Response-specific smooths
- **fit4**: `bf(count ~ x) + bf(presence ~ x) + bf(biomass ~ x)` → Three separate mus

**Verification Points**:
1. **Response naming**: `mu_count`, `mu_biomass`, etc. (not `mu[1]`, `mu[2]`)
2. **Separate construction**: Each response gets its own mu computation
3. **Parameter suffixes**: All parameters should have response suffixes (`b_count`, `Intercept_biomass`)
4. **Trend integration**: Trends added correctly per response (`mu_count[n] += trend[...]`)

### Task 4: Distributional Model Verification
**Objective**: Ensure trends only apply to main parameter (mu) in distributional models.

**Test Cases**:
- `bf(y ~ s(x), sigma ~ s(z)), trend_formula = ~ AR(p = 1)` 
- Should add trends to `mu` computation only
- Should NOT add trends to `sigma` computation
- Verify auxiliary parameters work independently

### Task 5: Edge Case Verification
**Objective**: Test boundary conditions and special patterns.

**Cases to Test**:
- **Intercept-only models**: `y ~ 1, trend_formula = ~ RW()`
- **No-intercept obs**: `y ~ -1 + x, trend_formula = ~ RW()` 
- **No-intercept trend**: `y ~ x, trend_formula = ~ -1 + RW()`
- **No-intercept both**: `y ~ -1 + x, trend_formula = ~ -1 + RW()`
- **Trend-only**: `y ~ 1, trend_formula = ~ s(habitat) + AR()`

## Implementation Plan

### Phase 1: Diagnostic Script Creation
Create `tasks/diagnose_mu_generation.R`:
1. Load all models from `fit_and_save_models.R`
2. Generate Stan code for each model
3. Extract and analyze mu construction patterns  
4. Create systematic checks for each verification point
5. Generate detailed report of issues found

### Phase 2: Issue Identification and Root Cause Analysis
1. Identify exact locations in `R/stan_assembly.R` where intercept logic fails
2. Determine if issue is in:
   - Formula parsing (`R/brms_integration.R`)
   - Stan code generation (`R/stan_assembly.R`) 
   - Trend stanvar creation (`R/trend_generators/`)
   - brms integration points

### Phase 3: Fix Formula Parsing Bug in remove_trend_expressions()

**Root Cause**: The `remove_trend_expressions()` function in `R/validations.R` only handles addition (`+`) operations but not subtraction (`-`) operations, causing formulas like `~ RW() + x - 1` to incorrectly become `~ 1` instead of `~ x - 1`.

#### Task 3.1: Create Test Cases (15 min)
- Create file `tests/testthat/test-trend-formula-parsing.R`
- Add test cases for:
  - `~ RW() - 1` should become `~ -1`
  - `~ RW() + x - 1` should become `~ x - 1`
  - `~ RW() + gp(x) - 1` should become `~ gp(x) - 1`
  - `~ x + RW() - 1` should become `~ x - 1`
  - `~ -1 + RW()` should become `~ -1`
- Run tests to confirm they fail (expected behavior before fix)

#### Task 3.2: Locate and Document Current Code (15 min)
- Find `remove_trend_expressions()` in `R/validations.R`
- Add a comment block above the function documenting:
  - Current behavior (only handles `+` operations)
  - Bug description (loses `-` operations)
  - Link to this issue/task
- Identify the line numbers where changes will be made

#### Task 3.3: Create Backup and Debug Version (15 min)
- Copy current `remove_trend_expressions()` to `remove_trend_expressions_backup()`
- Add debug output to current function:
  ```r
  cat("DEBUG: Processing expr:", deparse(expr), "\n")
  cat("DEBUG: Is '+' call:", rlang::is_call(expr, "+"), "\n")
  cat("DEBUG: Is '-' call:", rlang::is_call(expr, "-"), "\n")
  ```
- Test with `~ RW() + x - 1` to see debug output

#### Task 3.4: Add Unary Minus Handling (15 min)
After the `if (rlang::is_call(expr, "+"))` block, add:
```r
} else if (rlang::is_call(expr, "-") && length(rlang::call_args(expr)) == 1) {
  # Unary minus (e.g., -1)
  args <- rlang::call_args(expr)
  arg <- remove_trend_expressions(args[[1]], trend_patterns, depth + 1)
  if (is.null(arg)) return(NULL)
  return(rlang::call2("-", arg))
```
- Test with `~ -1` to ensure it stays as `~ -1`

#### Task 3.5: Add Binary Minus Handling (15 min)
Continue the else-if chain:
```r
} else if (rlang::is_call(expr, "-") && length(rlang::call_args(expr)) == 2) {
  # Binary minus (e.g., x - 1)
  args <- rlang::call_args(expr)
  lhs <- remove_trend_expressions(args[[1]], trend_patterns, depth + 1)
  rhs <- remove_trend_expressions(args[[2]], trend_patterns, depth + 1)
  
  # Reconstruct based on what remains
  if (is.null(lhs) && is.null(rhs)) return(NULL)
  if (is.null(lhs)) return(rlang::call2("-", rhs))
  if (is.null(rhs)) return(lhs)
  return(rlang::call2("-", lhs, rhs))
```
- Test with `~ x - 1` to ensure it stays as `~ x - 1`

#### Task 3.6: Test Combined Operations (15 min)
- Test the complete fix with:
  - `~ RW() + x - 1` → should become `~ x - 1`
  - `~ RW() - 1` → should become `~ -1`
- Remove debug output added in Task 3.3
- Verify all tests from Task 3.1 now pass

#### Task 3.7: Test with Real Models (15 min)
Create and run `tasks/test-intercept-fix.R`:
```r
devtools::load_all()
test_data <- create_test_data()  # Use existing test data creation

# Test cases
test_formulas <- list(
  "RW() only" = ~ RW(),
  "RW() with -1" = ~ RW() - 1,
  "RW() + x - 1" = ~ RW() + x - 1,
  "RW() + gp(x) - 1" = ~ RW() + gp(x) - 1
)

for (name in names(test_formulas)) {
  stancode <- make_stancode(
    mvgam_formula(y ~ x, trend_formula = test_formulas[[name]]),
    data = test_data,
    family = poisson()
  )
  has_intercept <- grepl("Intercept_trend", stancode)
  cat(name, ": Intercept_trend =", has_intercept, "\n")
}
```

#### Task 3.8: Clean Up and Document (15 min)
- Remove `remove_trend_expressions_backup()` if fix works
- Remove or comment out helper function `should_trend_formula_have_intercept()` (no longer needed)
- Remove debug code from `setup_brms_lightweight()`
- Add roxygen comment to `remove_trend_expressions()`:
  ```r
  #' @details Now correctly handles both addition (+) and subtraction (-) operations
  #'   to preserve intercept removal (e.g., -1) and covariates when removing trend constructors
  ```

### Phase 4: Targeted Fixes for Remaining Issues
1. Verify observation model mu construction
2. Test multivariate model patterns
3. Validate distributional model restrictions
4. Handle edge cases properly

### Phase 5: Comprehensive Testing

#### Task 5.1: Run Comprehensive Intercept Tests (15 min)
- Run the diagnostic script: `Rscript tasks/diagnose_mu_generation.R`
- Verify that:
  - `~ RW()` generates NO intercept ✓
  - `~ RW() - 1` generates NO intercept ✓  
  - `~ RW() + x - 1` generates NO intercept ✓
  - `~ 1 + RW()` generates an intercept ✓
- Document results in `tasks/mu_generation_issues.md`

#### Task 5.2: Test All Model Patterns (15 min)
1. Run updated `fit_and_save_models.R` successfully
2. Verify all `tests/local/test-models-single.R` patterns work
3. Update any outdated test expectations
4. Ensure no regressions in existing functionality

**Success Criteria**:
1. **Formulas with `-1` preserve intercept removal** when trend constructors are removed
2. **Covariates are preserved** regardless of formula order
3. **All existing tests continue to pass**
4. **No Stan compilation errors** for any test case
5. **Debug output is completely removed** from final code

## Expected Deliverables

1. **Diagnostic script**: `tasks/diagnose_mu_generation.R`
2. **Issue report**: `tasks/mu_generation_issues.md` 
3. **Fixed Stan assembly**: Updated `R/stan_assembly.R`
4. **Verified models**: All 11 models from `fit_and_save_models.R` working
5. **Updated tests**: Any test updates needed for corrected behavior

## Success Criteria

1. **Trend intercepts**: Controlled correctly by formula specification
2. **Observation mus**: Constructed properly for all model types
3. **Multivariate mus**: Response-specific naming and construction
4. **Distributional models**: Trends only on main parameters
5. **Model fitting**: All test models compile and fit successfully
6. **Test coverage**: Comprehensive verification of mu generation patterns

## Notes

- Focus on **correctness over optimization** - ensure Stan code is mathematically correct first
- Use `validate = FALSE` in `make_stancode()` for faster debugging iteration  
- Compare against known working patterns from brms documentation
- Test both `cmdstan` and `rstan` backends if possible
- Document any breaking changes for users
- ALWAYS use `devtools::load_all()` at the start of ALL exploration and test scripts (NOT `library(mvgam)`)
- ALWAYS use the code-reviewer agent to review any proposed R code changes
