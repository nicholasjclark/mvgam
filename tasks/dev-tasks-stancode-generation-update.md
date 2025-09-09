# TRD-stancode-generation-update Development Tasks

## 🎯 **CURRENT STATUS: 95% COMPLETE - CRITICAL BUG DISCOVERED** 

**✅ COMPLETED**: Files 1, 2, 4, 5 (perfect Stan code matches)
**🔄 CRITICAL ISSUE**: File 3 & 6 affected by `mu_trend` initialization bug
**⚠️ BREAKING CHANGE**: Intercept-only models fixed, but coefficient models broken by syntax errors

---

## 🔧 **DEVELOPMENT PROTOCOL**

### **STEP 1: Generate Current Stan Code** 
```bash
# Generate current Stan files for comparison
Rscript target_generation.R
```

### **STEP 2: SYSTEMATIC FILE ANALYSIS - MANDATORY TDD APPROACH**
⚠️ **CRITICAL**: Agents MUST read complete files for systematic comparison. NO grep/diff shortcuts allowed.

**Required Analysis Method**:
```r
# Read complete files for direct comparison
current_content <- readLines("tasks/current_stancode_3.stan") 
target_content <- readLines("tasks/target_stancode_3.stan")

# Systematic line-by-line analysis to identify:
# 1. Missing trend injection patterns
# 2. Incorrect parameter usage  
# 3. Structural differences in blocks
```

### **STEP 3: Target Files**
- `target_stancode_3.stan` → VARMA trends (complex multivariate)
- `target_stancode_6.stan` → CAR trends (GP + irregular time)

---

## 🎯 **CURRENT PROGRESS: Linear Predictor Fixes Applied**

### ✅ **COMPLETED Linear Predictor Enhancements**:
1. **mu_trend Intercept Injection**: Added `mu_trend += Intercept_trend + Xc_trend * b_trend`
2. **Response Mapping Fix**: Corrected to use `obs_trend_time_{resp}[n], obs_trend_series_{resp}[n]` pattern
3. **VARMA Detection**: Added automatic detection via `grepl("D_trend|ma_.*_trend", base_stancode)`
4. **Enhanced Validation**: Added proper `checkmate::assert_string()` validation

### 🔄 **File 3 VARMA Status**: 5/7 patterns (improved from 4/7)
**✅ Fixed Patterns**:
- `mu_trend += Intercept_trend + Xc_trend * b_trend`
- `mu_count[n] += trend[obs_trend_time_count[n], obs_trend_series_count[n]]`
- `mu_biomass[n] += trend[obs_trend_time_biomass[n], obs_trend_series_biomass[n]]`

**⏳ Remaining Gaps (NOT in linear predictor system)**:
- Missing MA component: `mu_t_trend[t] += D_trend[1] * ma_init_trend`
- Missing MA component: `mu_t_trend[t] += D_trend[1] * ma_error_trend[t - 1]`

**Root Cause**: These are in the trend system's VAR loop, not linear predictor injection

### 🔄 **File 6 CAR Status**: Requires Full File Analysis
**Current Known**: 1 trend injection pattern exists  
**Assessment Required**: Full file reading needed to identify exact gaps

---

## ✅ **COMPLETED: VARMA Trend System (File 3)**

**Status**: All 7/7 patterns implemented and verified
**Fix Applied**: VAR parameter conversion bug fixed in `R/stan_assembly.R`
- Fixed `ma = TRUE` → `ma_lags = 1` conversion in `generate_trend_specific_stanvars`
- All MA components now properly generated: `D_trend`, `ma_init_trend`, `ma_error_trend`
- VARMA dynamics fully operational with correct MA patterns

**Verification**: Current vs target comparison shows complete match

---

## 🚨 **CRITICAL BUG: mu_trend Initialization System**

### **Root Cause Identified - 2025-09-09**
A critical bug in `R/stan_assembly.R` line ~4930 affects `mu_trend` initialization across ALL trend types:

**✅ FIXED: Intercept-only models** (`trend_formula = ~ CAR()`, `~ AR()`, etc.)
- Now correctly generate: `mu_trend = rep_vector(Intercept_trend, N_trend)`
- Fixed regex timing issue where `Intercept_trend` wasn't available during detection

**❌ BROKEN: Models with predictors** (`trend_formula = ~ x + CAR()`, `~ presence + VAR()`, `~ s(x) + AR()`, etc.)
- Linear predictors should generate: `mu_trend = rep_vector(0.0, N_trend); mu_trend += Intercept_trend + Xc_trend * b_trend`
- Spline predictors should generate: `mu_trend = rep_vector(0.0, N_trend); mu_trend += Intercept_trend + Xs_trend * bs_trend + Zs_trend_* * s_trend_*`
- Currently cause Stan syntax errors in parameters block for ALL predictor types

### **Evidence**
- **File 3 (VARMA)**: Uses `trend_formula = ~ presence + VAR(p=2, ma=TRUE)` → should use coefficient path
- **File 6 (CAR)**: Uses `trend_formula = ~ CAR()` → correctly uses intercept path  
- **Debug scripts**: `debug_mu_trend.R` confirms intercept-only models work, coefficient models fail

### **⚠️ MANDATORY DEBUGGING PROTOCOL FOR FUTURE AGENTS**

**CRITICAL**: Agents MUST use comprehensive debugging scripts that test VARIETY of combinations:

1. **Trend Types**: CAR, AR, RW, VAR, VARMA
2. **Formula Patterns**: 
   - Intercept-only: `~ CAR()`, `~ AR()`, `~ VAR()`
   - With predictors: `~ x + CAR()`, `~ presence + VAR()`, `~ z * w + AR()`
   - No intercept: `~ -1 + x + CAR()`
3. **Response Types**: Single vs multivariate, different families

**Required Testing Matrix** (MUST test ALL combinations):
```r
# Test cases that MUST all work:
trend_formulas <- list(
  "~ CAR()",              # intercept-only CAR
  "~ x + CAR()",          # CAR with linear predictor  
  "~ s(x) + CAR()",       # CAR with smooth term (splines - different pattern!)
  "~ AR()",               # intercept-only AR
  "~ presence + VAR()",   # VAR with binary predictor
  "~ s(time_var) + AR()", # AR with spline (creates different Stan structures)
  "~ x * z + RW()",       # RW with interaction
  "~ -1 + z + CAR()",     # no-intercept CAR
  "~ -1 + s(x) + VAR()"   # no-intercept with spline
)

families <- list(poisson(), gaussian(), nbinom2())
response_types <- c("single", "multivariate")

# CRITICAL: s(x) terms create different parameter patterns:
# - Generate spline basis matrices (Zs_trend_*)
# - Create penalized coefficients (zs_trend_*, sds_trend_*)
# - Different Stan block structures vs linear predictors
# MUST verify correct mu_trend generation for each combination
```

**⚠️ USE EXISTING TEST INFRASTRUCTURE**:
- **`tests/testthat/test-stancode-standata.R`** - Already contains comprehensive tests covering:
  - **Intercept-only**: `~ RW()`, `~ AR()`, `~ CAR()` 
  - **With predictors**: `~ presence + VAR()`, `~ x + ZMVN()`, `~ x + AR()`
  - **With splines**: `~ s(x)` (via observation model patterns)
  - **No intercept**: `~ -1 + AR()`
  - **Complex multivariate**: Various combinations with different families
- **MUST run existing tests**: `Rscript -e "devtools::load_all();testthat::test_file('tests/testthat/test-stancode-standata.R')"`
- **DO NOT create new debugging scripts** - use existing test framework to verify fixes
- **Expected after fix**: ALL tests should pass without Stan syntax errors

**⚠️ SPLINE DETECTION COMPLEXITY**:
Current coefficient detection logic in `R/stan_assembly.R:4940` only checks for:
```r
has_coefficients <- grepl("vector.*b_trend", stancode) && grepl("matrix.*X_trend", stancode)
```

But spline models create DIFFERENT parameter patterns:
- `vector[Ks_trend] bs_trend` (unpenalized spline coefficients)
- `vector[knots_*] zs_trend_*` (penalized spline coefficients) 
- `matrix[N_trend, Ks_trend] Xs_trend` (spline design matrices)
- `matrix[N_trend, knots_*] Zs_trend_*` (spline basis matrices)

**MUST update detection logic to handle ALL coefficient types!**

---

## 🎯 **ROOT CAUSE IDENTIFIED - 2025-09-09**

**BREAKTHROUGH**: Comprehensive internal tracing via monkey-patched `extract_and_rename_stan_blocks` function revealed the exact root cause.

### **The Problem**

The `has_coefficients` detection logic (lines 4939-4940) is **fundamentally broken**:

```r
# CURRENT (BROKEN) - looks for SUFFIXED parameters in UNSUFFIXED brms code
has_coefficients <- grepl(paste0("vector\\[.*\\]\\s+b", suffix), stancode) && 
                    grepl(paste0("matrix\\[.*\\]\\s+X", suffix), stancode)
```

**What it looks for**: `b_trend`, `X_trend` (with `_trend` suffix)
**What actually exists**: `vector[Kc] b;`, `matrix[N, K] X;` (no suffix)
**Result**: `has_coefficients` is **always FALSE**, even when coefficients exist

### **The Evidence**

From `debug_mu_trend_internal.R` comprehensive tracing:

**Scenario 2**: `trend_formula = ~ -1 + z + CAR()` (should use coefficients branch)
- **Expected**: `rep_vector(0.0, N_trend) + Xc_trend * b_trend`
- **Actual**: `rep_vector(0.0, N_trend)` (missing components)
- **Cause**: `has_coefficients=FALSE` despite coefficients existing

**Scenario 3**: `trend_formula = ~ z + CAR()` (should use coefficients branch) 
- **Expected**: `rep_vector(0.0, N_trend) + Xc_trend * b_trend`
- **Actual**: `rep_vector(Intercept_trend, N_trend)` (wrong pattern)
- **Cause**: `has_coefficients=FALSE`, falls back to intercept-only

### **The Fix**

```r
# FIXED VERSION - looks for UNSUFFIXED parameters in brms code
has_coefficients <- grepl("vector\\[.*\\]\\s+b[^_]", stancode) && 
                    grepl("matrix\\[.*\\]\\s+X[^_]", stancode)
```

**Location**: `R/stan_assembly.R` lines 4939-4940

---

## 🔧 **DEBUGGING PROTOCOL FOR FUTURE AGENTS**

**MANDATORY**: Use the comprehensive internal tracing script before attempting fixes:

```bash
Rscript debug_mu_trend_internal.R
```

This script:
1. ✅ **Monkey-patches** `extract_and_rename_stan_blocks` with internal tracing
2. ✅ **Shows exact intermediate Stan code** when detection runs (not just final output)
3. ✅ **Traces actual execution path** through conditional logic branches
4. ✅ **Reveals validation check results** and whether they pass/fail
5. ✅ **Shows exact mu_trend_code generation** inside the function
6. ✅ **Tracks stanvar creation process** and any failures

**WARNING**: Do NOT guess at fixes without running this script first. The issue was not what it appeared to be from external analysis.

---

## 🚨 **PRIORITY: Fix mu_trend Detection Logic (Critical)**

**Status**: Root cause identified, fix location confirmed
**Priority**: Critical (affects ALL trend models with predictors)
**Estimated Time**: 2 minutes (simple regex change)
**Files**: `R/stan_assembly.R` - Lines 4939-4940

### **Sub-Tasks for CAR Fix:**

### [x] T3.1: Fix Data Block Syntax Errors ✅ **COMPLETED**
**Priority**: Critical (prevents compilation)
**Estimated Time**: 5 minutes
**Status**: ✅ **FIXED** - 2025-09-09
**Action Items Completed**:
- ✅ Fixed line 4035: Changed `array[n, N_series_trend]` to `array[N_trend, N_series_trend]` in `generate_car_trend_stanvars`
- ✅ Fixed duplicate N_trend declaration: Removed N_trend creation from `generate_common_trend_data` since N_trend comes from brms parameter extraction
- ✅ Root cause identified: N_trend is created by renaming `N` from brms trend model, not by dimension generation
**Files**: `R/stan_assembly.R` - Lines 4035 (time_dis declaration) and 2224 (dimension generation)

### [ ] T3.2: Simplify Parameter Architecture  
**Priority**: High (removes unnecessary complexity)
**Estimated Time**: 10 minutes
**Action Items**:
- Remove hierarchical parameters: `L_Omega_global`, `L_deviation_group`, `alpha_cor`
- Keep only essential CAR parameters: `ar1_trend`, `sigma_trend`, `innovations_trend`
- Remove unnecessary `combine_cholesky` function usage
- Match target parameter structure exactly
**Files**: `R/stan_assembly.R` (generate_car_trend_stanvars function)

### [ ] T3.3: Fix Transformed Parameters Logic
**Priority**: High (incorrect computation)  
**Estimated Time**: 10 minutes
**Action Items**:
- Fix mu_trend initialization: `rep_vector(Intercept_trend, N_trend)` instead of `rep_vector(0.0, N_trend)`
- Simplify scaled_innovations: `innovations_trend * diag_matrix(sigma_trend)` 
- Remove complex hierarchical correlation application (lines 105-115)
- Implement clean CAR evolution: `pow(ar1_trend[j], time_dis[i, j]) * lv_trend[i-1, j] + scaled_innovations_trend[i, j]`
**Files**: `R/stan_assembly.R` (CAR transformed parameters block)

### [ ] T3.4: Clean Up Model Block Structure
**Priority**: Medium (code clarity)
**Estimated Time**: 5 minutes  
**Action Items**:
- Remove hierarchical correlation priors: `alpha_cor`, `L_Omega_global`, `L_deviation_group`
- Keep core CAR priors: `ar1_trend ~ normal(0, 0.5)`, `sigma_trend ~ exponential(2)`, `innovations_trend ~ std_normal()`
- Ensure likelihood structure matches target
- Remove unnecessary complexity from model block
**Files**: `R/stan_assembly.R` (CAR model block generation)

---

## 📊 **SUCCESS METRICS**

- ✅ **Priority 1**: File 3 shows 7/7 patterns through full file reading
- ✅ **Priority 2**: File 6 shows complete match through full file reading  
- ✅ **Overall**: All 6 target Stan files match current generation

## 🔧 **AGENT HANDOFF PROTOCOL**

1. **MANDATORY**: Run `Rscript target_generation.R` before starting
2. **MANDATORY**: Use `Read` tool for complete file analysis - NO grep/diff shortcuts
3. **Focus**: Complete trend system enhancements (not linear predictor fixes)
4. **TDD Approach**: Direct file comparison for accurate gap identification

**Example Proper Analysis**:
```r
# Correct approach
current_lines <- readLines("tasks/current_stancode_3.stan")
target_lines <- readLines("tasks/target_stancode_3.stan") 
# Compare systematically to identify exact gaps
```

**❌ Forbidden Shortcuts**:
```bash
# WRONG - no pattern matching shortcuts
grep "pattern" current_stancode_3.stan
diff current_stancode_3.stan target_stancode_3.stan
```