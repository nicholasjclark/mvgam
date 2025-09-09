# TRD-stancode-generation-update Development Tasks

## 📋 COMPLETED FOUNDATION WORK ✅

**Summary**: Complete prior system, parameter extraction, trend generators, and inspection functions (Steps 1-9) with 346+ tests passing across `R/priors.R`, `R/validations.R`, `R/brms_integration.R`, `R/stan_assembly.R`. Key achievements: standardized 3-stanvar pattern, mvgam_formula() interface, centralized prior resolution, and parameter extraction system.

---

## 📋 **SYSTEMATIC INSPECTION & DEVELOPMENT PROTOCOL** 🎯

**ALL agents MUST follow this systematic analysis approach:**

### **STEP 1: Generate Current Stan Code** 🏗️
**ALWAYS start here before making any changes:**
```bash
# Generate current Stan files for comparison
source("target_generation.R")
```
**Output**: Creates `tasks/current_stancode_1.stan` through `tasks/current_stancode_6.stan`

### **STEP 2: Systematic Analysis of All Files** 📊  
**Run these commands to identify patterns across all files:**

```bash
# Check for missing observation-to-trend mapping arrays (Critical Issue #1)
for i in 1 2 3 4 5 6; do
  echo "File $i:"
  grep -c "obs_trend_time" current_stancode_$i.stan || echo "  MISSING obs_trend_time arrays"
  grep -c "obs_trend_time" target_stancode_$i.stan || echo "  Target expects obs_trend_time arrays"
done

# Check GLM compatibility vectors (Critical Issue #2)
for i in 1 2 3 4 5 6; do
  echo "File $i:"
  echo "  Current mu_ones:"; grep -c "mu_ones" current_stancode_$i.stan || echo "    NONE"
  echo "  Target mu_ones:"; grep -c "mu_ones" target_stancode_$i.stan || echo "    NONE"
done

# Check trend injection patterns (Critical Issue #3)
for i in 1 2 3 4 5 6; do
  echo "File $i:"
  grep -c "mu.*+.*trend\|trend\[obs_trend" current_stancode_$i.stan || echo "  MISSING trend injection"
  grep -c "mu.*+.*trend\|trend\[obs_trend" target_stancode_$i.stan || echo "  Expected trend injection"
done
```

### **STEP 3: Target File Reference** 🏆
**VALIDATED target files (all pass `rstan::stanc()` checks):**
- `target_stancode_1.stan` → RW trends (univariate baseline)
- `target_stancode_2.stan` → Shared RW trends (multivariate) 
- `target_stancode_3.stan` → VARMA trends (complex multivariate)
- `target_stancode_4.stan` → Factor AR trends (Z matrix + multivariate)
- `target_stancode_5.stan` → PW trends (Prophet univariate)
- `target_stancode_6.stan` → CAR trends (GP + irregular time)

### **STEP 4: Focused Diff Analysis** 🔍
**For each problematic file, run detailed comparison:**
```bash
# Compare specific files showing major differences
diff current_stancode_2.stan target_stancode_2.stan  # Multivariate issues
diff current_stancode_3.stan target_stancode_3.stan  # VARMA issues  
diff current_stancode_4.stan target_stancode_4.stan  # Factor model issues
```

### **STEP 5: Test-Driven Validation** ✅
**After making changes:**
```bash
# Re-generate current files
source("target_generation.R")

# Re-run systematic analysis (Step 2 commands)
# Verify improvements in problematic patterns

# Run failing tests
Rscript -e "devtools::load_all(); testthat::test_file('tests/testthat/test-stancode-standata.R')"
```

### **KEY INSIGHT: Pattern-Based Failures** 💡
**Analysis reveals three systematic issues affecting multiple files:**
1. **Multivariate Data Block Generation** (Files 2,3,4) 
2. **Trend Injection Integration** (Files 2,3,4,6)
3. **Stan Code Structure** (All files)

**Files Working:** 1,5,6 (mostly univariate)
**Files Failing:** 2,3,4 (all multivariate)
**Root Cause:** Multivariate model handling in Stan assembly

## 🚨 **CURRENT PRIORITY OBJECTIVES** (Systematic Analysis Based)

**Based on systematic analysis of all 6 current vs target Stan file comparisons, three critical systemic issues identified:**

---

### **🎯 PRIORITY 1: Fix Multivariate Data Block Generation** 

**Impact:** Files 2, 3, 4 (all multivariate models) - ~50% of total failures

**Problem Analysis:**
- **Missing obs-to-trend mappings:** Files 2,3,4 have 0 `obs_trend_time_*` arrays vs targets expecting 4-6
- **Wrong GLM vectors:** Files 2,4 have 1 generic `mu_ones` vs targets expecting 4-5 response-specific vectors
- **Incomplete multivariate data:** Files 2,3,4 have ~50% fewer `N_count`/`N_biomass` references than targets

**Specific Missing Elements:**
```stan
// Expected in data block for multivariate models:
array[N_count] int obs_trend_time_count;
array[N_count] int obs_trend_series_count;
array[N_biomass] int obs_trend_time_biomass;
array[N_biomass] int obs_trend_series_biomass;
// etc. for each response

vector[1] mu_ones_count;      // not generic mu_ones
vector[1] mu_ones_biomass;    // response-specific GLM vectors
```

**Target Functions:** 
- `extract_multivariate_standata()` in `R/stan_assembly.R`
- `generate_obs_trend_mapping()` functions
- Multivariate stanvar generation pipeline

**Validation Command:**
```bash
# Before fix: Files 2,3,4 should show 0 obs_trend_time arrays
# After fix: Should match target counts (4-6 arrays per file)
grep -c "obs_trend_time" current_stancode_2.stan  # Should go from 0 → 4
```

---

### **🎯 PRIORITY 2: Fix Trend Injection Integration** 

**Impact:** Files 2, 3, 4, 6 - trend effects not properly integrated into linear predictors

**Problem Analysis:**
- **File 2:** 0 trend injections vs target expecting 2
- **File 3:** 4 vs target expecting 7 trend injections 
- **File 4:** 1 vs target expecting 3 trend injections
- **File 6:** 1 vs target expecting 2 trend injections

**Expected Pattern in Target Files:**
```stan
// In transformed parameters:
vector[N_count] mu_count = Xc_count * b_count;
vector[N_biomass] mu_biomass = Xc_biomass * b_biomass;

// Inject trends:
for (n in 1:N_count) {
  mu_count[n] += Intercept_count + trend[obs_trend_time_count[n], obs_trend_series_count[n]];
}
for (n in 1:N_biomass) {
  mu_biomass[n] += trend[obs_trend_time_biomass[n], obs_trend_series_biomass[n]];
}
```

**Target Functions:**
- `inject_multivariate_trends_into_linear_predictors()` or similar
- `inject_trend_into_linear_predictor()` for multivariate models
- Linear predictor setup in transformed parameters

**Validation Command:**
```bash
# Count trend injection patterns - should match target expectations
grep -c "mu.*+=.*trend\[obs_trend" current_stancode_3.stan  # Should go from 4 → 7
```

---

### **🎯 PRIORITY 3: Standardize Stan Code Structure**

**Impact:** All files - inconsistent code organization and headers

**Problem Analysis:**
- Wrong model headers ("generated with brms" vs "generated with mvgam")
- Parameter block organization differs from targets
- Transformed parameters computation approaches inconsistent
- Model block structure varies from expected patterns

**Target Pattern Example:**
```stan
// generated with mvgam 2.0.0  // NOT "generated with brms"

// Consistent parameter organization
parameters {
  // AR coefficients with correlation
  vector<lower=-1,upper=1>[N_lv_trend] ar1_trend;
  
  // Innovation parameters  
  vector<lower=0>[N_lv_trend] sigma_trend;
  cholesky_factor_corr[N_lv_trend] L_Omega_trend;
}
```

**Target Functions:**
- Stan code header generation
- Parameter block formatting functions
- Overall code structure templates

---

## 📈 **IMPLEMENTATION STRATEGY**

### **Phase 1: Multivariate Data Foundation** (Highest Impact)
1. Fix `extract_multivariate_standata()` to generate complete observation-to-trend mappings
2. Fix GLM vector generation for multiple responses
3. Validate using Files 2,3,4 - should see immediate improvement in systematic analysis

### **Phase 2: Trend Integration** (Medium Impact) 
1. Fix trend injection for multivariate linear predictors
2. Ensure proper transformed parameters structure
3. Validate trend effects properly applied to each response

### **Phase 3: Code Standardization** (Polish)
1. Standardize headers and code structure
2. Ensure consistent parameter organization
3. Final validation against all target files

## 🎯 **SUCCESS METRICS**
- **Priority 1 Success:** Files 2,3,4 systematic analysis shows obs_trend arrays match target counts
- **Priority 2 Success:** Files 2,3,4,6 show proper trend injection pattern counts  
- **Priority 3 Success:** All files have consistent structure matching targets
- **Overall Success:** Systematic analysis shows minimal differences vs targets

## 🔧 **KEY DEVELOPMENT FILES**
- `R/stan_assembly.R`: Core Stan generation and multivariate handling
- `R/brms_integration.R`: Multivariate formula processing
- `R/validations.R`: Data structure validation and preparation
- `R/trend_system.R`: Individual trend type implementations

---

## ✅ **COMPLETED FOUNDATION WORK** (Reference Only)

**Individual task completions from previous development phases:**

- ✅ **D1.1-D1.3**: RW trend generation (univariate models working)
- ✅ **D2.1-D2.3**: AR seasonal patterns (parameter naming, initialization, dynamics)
- ✅ **D3.1-D3.3**: Multivariate formula parsing (mvbrmsformula support)
- ✅ **D4.2-D4.3**: Factor model Z_raw construction (LV_raw removed)
- ✅ **Test Infrastructure**: Syntax errors and regex patterns corrected
- ✅ **Architecture Cleanup**: Removed outdated parameter references

**Current Status:** ~328 tests passing, ~135 failures remaining (~71% pass rate)
**Key Insight:** Individual fixes helped but **systematic multivariate issues** need coordinated approach

---

## 🎯 **CURRENT STATUS: PRIORITY 1 PARTIALLY COMPLETE** ✅⚠️

**COMPLETED**: Response-specific GLM vector generation 
- ✅ Enhanced `detect_glm_usage()` function with response-specific analysis
- ✅ Implemented multivariate GLM stanvar creation in `R/stan_assembly.R` lines 814-862
- ✅ Files 2,4 now generate correct response-specific GLM vectors:
  - File 2: `mu_ones_count`, `mu_ones_biomass` ✅
  - File 4: `mu_ones_count`, `mu_ones_presence` ✅ (correctly omits `mu_ones_biomass` for `gamma_lpdf`)

**REMAINING ISSUE**: Response-specific observation-to-trend mapping arrays
- ❌ Files 2,3,4 have generic `obs_trend_time`, `obs_trend_series` arrays
- ❌ Targets expect response-specific arrays: `obs_trend_time_count`, `obs_trend_time_presence`, etc.
- ❌ Current: 2 generic arrays vs Target: 4-6 response-specific arrays per file

**ROOT CAUSE IDENTIFIED**: Multivariate mapping generation not response-specific
- **Current Problem**: System generates `array[N] int obs_trend_time` instead of `array[N_count] int obs_trend_time_count`
- **Critical Gap**: Need response-specific mapping arrays for each response variable in multivariate models

---

## 🚀 **IMMEDIATE IMPLEMENTATION PLAN: Response-Specific Observation-to-Trend Mapping** 

**OBJECTIVE**: Complete Priority 1 by implementing response-specific observation-to-trend mapping arrays for multivariate models

### **📋 Step 1: Fix Multivariate Mapping Array Generation** ⏳
**Location**: `R/stan_assembly.R` lines 749-791 (shared trends logic)
**Objective**: Generate response-specific mapping arrays instead of generic ones

**Current Issue**:
```stan
// Current (wrong):
array[N] int obs_trend_time;
array[N] int obs_trend_series;

// Target (correct):
array[N_count] int obs_trend_time_count;
array[N_count] int obs_trend_series_count;
array[N_biomass] int obs_trend_time_biomass;
array[N_biomass] int obs_trend_series_biomass;
```

**Root Cause**: Shared trends logic at lines 757-759 uses generic mapping instead of generating response-specific arrays

**Implementation Strategy**:
1. **Enhance shared trends case**: Modify lines 757-791 to generate response-specific mapping stanvars
2. **Generate per-response arrays**: Create `obs_trend_time_{response}` and `obs_trend_series_{response}` for each response
3. **Use existing infrastructure**: Leverage `dimensions$mappings` structure that already contains per-response mapping data

### **📋 Step 2: Update Shared Trends Logic for Response-Specific Arrays** ⏳
**Target Code Location**: `R/stan_assembly.R` lines 757-759 (shared trends case)
**Change Required**: Generate separate stanvars for each response instead of using first mapping only

**Implementation**:
```r
# Current (line 757-759):
} else if (is.null(response_name) && length(dimensions$mappings) > 1) {
  # SHARED TRENDS: Use first mapping since all responses share same trend structure
  dimensions$mappings[[1]]

# Target (enhanced):
} else if (is.null(response_name) && length(dimensions$mappings) > 1) {
  # SHARED TRENDS: Generate response-specific mapping arrays for all responses
  # Each response gets its own obs_trend_time_{resp} and obs_trend_series_{resp} arrays
```

### **📋 Step 3: Validation Commands** ✅
**Before Implementation**: Check current mapping array counts
```bash
cd tasks/
# Current status (should show generic arrays):
grep -c "obs_trend_time" current_stancode_2.stan  # Should be 1 (generic)
grep -c "obs_trend_time_" current_stancode_2.stan  # Should be 0 (no response-specific)

# Target status (what we want to achieve):
grep -c "obs_trend_time_" target_stancode_2.stan  # Should be 2 (count, biomass)
```

**After Implementation**: Verify response-specific arrays generated
```bash
# Success criteria - should match target counts:
grep -c "obs_trend_time_" current_stancode_2.stan  # Should be 2
grep -c "obs_trend_time_" current_stancode_4.stan  # Should be 3
```

---

## 🎯 **NEXT STEPS AFTER COMPLETION**

**Once mapping arrays are fixed, Priority 1 will be COMPLETE**
- ✅ GLM vectors: Response-specific generation working
- ✅ Mapping arrays: Response-specific generation (target)

**Next Priority**: Priority 2 - Fix Trend Injection Integration
- Focus on trend injection patterns in transformed parameters block
- Target: Proper `mu_* += trend[obs_trend_time_*[n], obs_trend_series_*[n]]` patterns

---

## 🔄 **NEXT STEPS FOR AGENTS**

**IMMEDIATE PRIORITY**: Complete Steps 1-4 above to finish Priority 1 objective

1. **Implement Step 1**: Add `detect_response_glm_usage()` function
2. **Implement Step 2**: Enhance shared trend logic in `extract_trend_stanvars_from_setup()`
3. **Implement Step 3**: Add `find_matching_brace()` helper function  
4. **Validate Step 4**: Run systematic analysis to confirm response-specific GLM vectors
5. **Move to Priority 2**: Trend injection integration (only after Priority 1 complete)

**Agent Handoff Protocol:**
- Run `source("target_generation.R")` and systematic analysis commands BEFORE starting
- Implement steps in order (dependencies: Step 2 needs Step 1)
- Show before/after GLM vector counts for validation
- Update completion status in this document
