# Stan Code Generation System - Critical Issues

## COMPILATION BLOCKERS

### File 3 (VARMA Model) - RESOLVED
MA loop issue fixed in target. Status: RESOLVED

### File 6 (CAR + GP Trends) - MISSING FUNCTION
**Issue**: Missing `gp_exp_quad` function
**Location**: Functions block empty, but transformed parameters line 47 calls `gp_exp_quad()`
**Impact**: Immediate compilation failure
**Fix needed**: Add GP function to functions block

### File 7 (CAR + Monotonic) - MISSING FUNCTION
**Issue**: Missing `mo()` function for monotonic effects
**Location**: Functions block empty, but monotonic effects used in model
**Impact**: Immediate compilation failure  
**Fix needed**: Add monotonic function to functions block

### File 8 (Seasonal AR + GP) - VARIABLE SCOPE ERROR
**Issue**: Uses `gp_pred_1_trend[Jgp_1_trend]` before `gp_pred_1_trend` is computed
**Location**: Transformed parameters line 88
**Impact**: Stan compilation failure due to undefined variable
**Fix needed**: Reorder computations so GP predictions are computed before usage

## MINOR SYNTAX ISSUES

### File 1 (RW Basic)
- Variable ordering: lprior statements scattered instead of grouped early

### File 2 (RW Shared)  
- Unused variable: `matrix[N_lv_trend, N_lv_trend] Sigma_trend` declared but never used

### File 4 (Factor AR)
- Mathematical issue: `mu_biomass = inv(mu_biomass);` placement needs verification

### File 8 (Seasonal AR + GP) - MISSING PRIORS
- Missing `target += std_normal_lpdf(zgp_1_trend);` prior in model block

## IMMEDIATE PRIORITIES

1. **File 6**: Add missing `gp_exp_quad` function
2. **File 7**: Add missing `mo()` function  
3. **File 8**: Fix variable computation order
4. **File 8**: Add missing GP trend priors

## SYSTEM STATUS

- Compilation Ready: 5/8 files
- Z Matrix Fix: Complete and successful
- Critical Blockers: 3 files need function additions, 1 needs variable reordering

## NEXT ACTIONS

Focus on adding missing functions to Files 6 and 7, then fix variable ordering in File 8.