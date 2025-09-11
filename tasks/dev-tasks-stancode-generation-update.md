# TRD-stancode-generation-update Development Tasks

## 🔧 **MANDATORY DEVELOPMENT PROTOCOL**

### **STEP 1: Generate Fresh Stan Code (Before ANY Changes)**
**ALWAYS use general-purpose agent to get baseline**:
```bash
Rscript target_generation.R
```

### **STEP 2: Complete File Analysis (MANDATORY After Every Fix)**
⚠️ **CRITICAL**: Use **general-purpose agent** to READ ALL 6 complete files systematically. 

**Required Analysis Protocol**:
```r
# NEVER use grep/diff shortcuts - read complete files
for (i in 1:6) {
  current <- readLines(paste0("tasks/current_stancode_", i, ".stan"))
  target <- readLines(paste0("tasks/target_stancode_", i, ".stan"))
  
  # Compare every section:
  # 1. Functions block completeness
  # 2. Data block parameter presence  
  # 3. Parameters block declarations
  # 4. Transformed parameters variable order
  # 5. Model block statement placement
  # 6. Generated quantities consistency
}
```

### **STEP 3: Internal Debugging with Monkey Patching**
**When root causes unclear, use comprehensive internal tracing**:
```r
# Example: Monkey patch stan generation functions
original_func <- mvgam:::generate_var_trend_stanvars
mvgam:::generate_var_trend_stanvars <- function(...) {
  cat("=== TRACING VAR GENERATION ===\n")
  cat("Template data:", list(...), "\n")
  result <- original_func(...)
  cat("Generated code first 10 lines:\n")
  print(head(strsplit(result$code, "\n")[[1]], 10))
  return(result)
}
```

### **STEP 4: Regression Testing (NON-NEGOTIABLE)**
**After EVERY proposed fix**:
1. **Generate all 6 files** with `Rscript target_generation.R`  
2. **Analyze all 6 comparisons** with general-purpose agent
3. **Verify no regressions** - previously passing files must still pass
4. **Document exact changes** in each file before/after

---

## 🔄 **DEVELOPMENT WORKFLOW**

1. **Work incrementally**: Complete one step at a time
2. **Test after each step**: Verify no regressions with `Rscript target_generation.R`
3. **Use agents**: General-purpose agent for analysis, pathfinder for mapping functions
4. **Focus**: ONE issue at a time - duplication fix must complete before priorities 3-4
5. **Validate**: Every change tested across all 6 files

---

## 🎯 **PRIORITY TASKS (Next Two High-Priority Fixes)**

### **PRIORITY TASK 1: Fix File 6 (CAR trends) - Missing Critical Variables**

**Status**: HIGH PRIORITY - CAR trends completely non-functional

**Critical Issues Identified**:
1. **Missing `scaled_innovations_trend` declaration** - Essential for trend computation
2. **Missing `mu_trend` definition** in trend computation block
3. **GP component missing integration** - GP functions present but not connected
4. **Trend computation incomplete** - Missing essential variable references

**Impact**: CAR trends completely broken due to missing variables

**Fix Strategy**:
1. Add missing `scaled_innovations_trend` variable declaration
2. Ensure `mu_trend` is properly defined and accessible
3. Connect GP component integration in trend computation
4. Verify CAR-specific time distance calculations are present

**Verification Requirements**:
- [ ] `scaled_innovations_trend` declared in transformed parameters
- [ ] `mu_trend` accessible in trend computation context
- [ ] GP functions properly integrated with CAR dynamics
- [ ] All CAR-specific variables present in data/parameters blocks

### **PRIORITY TASK 2: Fix File 3 (VARMA trends) - Template & Syntax Issues**

**Status**: HIGH PRIORITY - VARMA trends non-functional

**Critical Issues Identified**:
1. **Template variables hardcoded** instead of being replaced with actual values
2. **Modern Stan array syntax conflicts** (`array[,] matrix[,]` vs older syntax)
3. **Parameter duplication** in multiple parameter blocks
4. **Malformed syntax** in hyperparameter arrays in transformed data
5. **Data block structure** missing proper trend covariates

**Impact**: VARMA trends non-functional with compilation errors

**Fix Strategy**:
1. Fix template variable replacement system (ensure {lags}, {response} etc. are replaced)
2. Standardize array syntax to be compatible across Stan versions
3. Eliminate parameter block duplications
4. Fix transformed data block syntax errors
5. **PRESERVE sqrtm function** - it's mathematically correct, target was updated for Stan compatibility

**Verification Requirements**:
- [ ] No template placeholders ({lags}, {response}) remain in final code
- [ ] Array syntax consistent throughout (prefer modern syntax where supported)
- [ ] No duplicate parameter declarations
- [ ] Transformed data block compiles without syntax errors
- [ ] sqrtm function matches updated target (with positive definite check)

### **TDD WORKFLOW FOR EACH PRIORITY TASK**

**Phase 1: Analysis**
1. **Fresh Baseline**: Generate all 6 files with `Rscript target_generation.R`
2. **Focus Analysis**: Use general-purpose agent to analyze the specific priority file
3. **Issue Identification**: Identify exact lines, variables, and syntax problems
4. **Impact Assessment**: Understand how the issues affect Stan compilation

**Phase 2: Fix Implementation**
1. **Targeted Fix**: Implement specific fix for identified issues
2. **Minimal Change**: Make the smallest change necessary to resolve the issue
3. **Code Review**: Use code-reviewer agent to validate the proposed changes

**Phase 3: TDD Validation**
1. **Regenerate All**: Run `Rscript target_generation.R` to generate all 6 files
2. **Full Comparison**: Use general-purpose agent to read ALL 6 current vs target files
3. **Regression Check**: Ensure no previously working files are broken
4. **Progress Verification**: Confirm the specific issue was resolved

**Phase 4: Documentation & Iteration**
1. **Document Changes**: Record what was changed and why
2. **Update Status**: Mark task components complete/incomplete
3. **Next Issue**: If multiple issues in same file, tackle next one
4. **Final Verification**: Complete task only when general-purpose agent confirms PASS

### **IMPLEMENTATION ORDER**

**Recommended Sequence**:
1. **Priority Task 1 (File 6)** - CAR trends missing variables
   - Cleaner fix with straightforward variable additions
   - Should resolve completely with single focused effort

2. **Priority Task 2 (File 3)** - VARMA template/syntax 
   - More complex multi-part fix
   - May require iterative fixes across multiple components

**Success Criteria**:
- **File 6**: Current matches target structure, all CAR variables present, compilable
- **File 3**: No template placeholders, clean syntax, proper VARMA implementation, compilable
