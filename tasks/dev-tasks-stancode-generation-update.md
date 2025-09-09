# TRD-stancode-generation-update Development Tasks

## 🎯 **CURRENT STATUS: 67% COMPLETE** 

**✅ COMPLETED**: Files 1, 2, 4, 5 (perfect Stan code matches)
**⏳ REMAINING**: Files 3, 6 (trend injection gaps)

---

## 🔧 **DEVELOPMENT PROTOCOL**

### **STEP 1: Generate Current Stan Code** 
```bash
# Generate current Stan files for comparison
source("target_generation.R")
```

### **STEP 2: Systematic Analysis**
```bash
# Check trend injection patterns 
for i in 3 6; do
  echo "File $i:"
  grep -c "mu.*+.*trend\|trend\[obs_trend" current_stancode_$i.stan || echo "  MISSING trend injection"
  grep -c "mu.*+.*trend\|trend\[obs_trend" target_stancode_$i.stan || echo "  Expected trend injection"
done
```

### **STEP 3: Target Files**
- `target_stancode_3.stan` → VARMA trends (complex multivariate)
- `target_stancode_6.stan` → CAR trends (GP + irregular time)

---

## 🚨 **PRIORITY 1: Fix VARMA Trend Injection (File 3)**

**Status**: 4/7 trend injections (missing 3 patterns)

**Root Cause**: VARMA multivariate models need enhanced trend injection for MA components

**Target Function**: `inject_multivariate_trends_into_linear_predictors()` in R/stan_assembly.R

**Expected Fix**: Generate proper MA trend injection patterns for VARMA models

**Validation**: 
```bash
# After fix - should show 7/7
grep -c "mu.*+.*trend\|trend\[obs_trend" current_stancode_3.stan
```

---

## 🚨 **PRIORITY 2: Fix CAR Trend Injection (File 6)**

**Status**: 1/2 trend injections (missing 1 pattern) 

**Root Cause**: CAR (Continuous Auto-Regressive) models with irregular time intervals need spatial-temporal trend injection

**Expected Fix**: Handle irregular time structure in CAR trend injection

**Validation**:
```bash  
# After fix - should show 2/2
grep -c "mu.*+.*trend\|trend\[obs_trend" current_stancode_6.stan
```

---

## 📊 **SUCCESS METRICS**

- ✅ **Priority 1**: File 3 shows 7/7 trend injections  
- ✅ **Priority 2**: File 6 shows 2/2 trend injections
- ✅ **Overall**: All 6 target Stan files match current generation

**Agent Handoff**: Run `source("target_generation.R")` before starting work. Focus on one priority at a time.