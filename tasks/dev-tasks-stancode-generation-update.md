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
1. **Generate all 7 files** with `Rscript target_generation.R`  
2. **Analyze all 7 comparisons** with general-purpose agent
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

### **CURRENT STATUS**

**Completed**:
- ✅ Removed 4 redundant functions from old mu analysis system
- ✅ Fixed identifier extraction bug in `extract_all_identifiers()`
- ✅ Added missing Stan types to reserved words (`simplex`, `row_vector`, etc.)
- ✅ All 7 target files generate without "variable mapping" errors

### **REMAINING TASKS**

#### **HIGH PRIORITY: File 3 VARMA Model Issues**
**Status**: Critical compilation errors

- **Duplicate blocks**: Remove duplicate A_trend computation blocks 
- **Missing mu_trend construction**: Add `mu_trend += Intercept_trend + Xc_trend * b_trend;`
- **Syntax errors**: Fix malformed hyperparameter arrays in transformed data section

#### **HIGH PRIORITY: File 4 Factor Model Issues**  
**Status**: Structural problems preventing compilation

- **Duplicate Z matrix**: Remove duplicate Z matrix declaration
- **Missing mu_trend construction**: Add proper mu_trend building code
- **Computation order**: Fix mu_biomass `inv()` application timing

---

## **VALIDATION PROTOCOL**

After completing each task:
1. Run `Rscript target_generation.R`
2. Verify specific issue resolved
3. Check no regressions in other files
4. Update task status

