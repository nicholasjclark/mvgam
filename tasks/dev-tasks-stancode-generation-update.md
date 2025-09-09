# TRD-stancode-generation-update Development Tasks

## 🎯 **CURRENT STATUS: 90% COMPLETE** 

**✅ COMPLETED**: Files 1, 2, 3, 4, 5 (perfect Stan code matches)
**🔄 IN PROGRESS**: File 6 (CAR trend system needs architecture fixes)

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

## 🚨 **PRIORITY: Complete CAR Trend System (File 6)**

**Current Issues**: Syntax errors, architectural bloat, incorrect parameters

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