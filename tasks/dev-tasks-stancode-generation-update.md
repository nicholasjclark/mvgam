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

## 🔄 **REMAINING PRIORITIES**

### **Priority 1: Block Placement Error (File 4)**
**Issue**: Linear predictor computation in model block instead of transformed parameters
**Target**: Move `mu_biomass` computation to correct Stan block

### **Priority 2: Missing GP Integration (File 6)** 
**Issue**: GP functions and computation completely absent from CAR+GP models
**Target**: Add missing `gp_exp_quad()` functions and GP computation blocks

## 🔄 **DEVELOPMENT WORKFLOW**

1. **Work incrementally**: Complete one step at a time
2. **Test after each step**: Verify no regressions with `Rscript target_generation.R`
3. **Use agents**: General-purpose agent for analysis, pathfinder for mapping functions
4. **Focus**: ONE issue at a time - duplication fix must complete before priorities 3-4
5. **Validate**: Every change tested across all 6 files

**Current Status**: Ready to begin Step 1 of Universal Infrastructure Split implementation
