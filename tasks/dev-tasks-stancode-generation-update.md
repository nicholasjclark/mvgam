# TRD-stancode-generation-update Development Tasks

## 🎯 **CURRENT STATUS: 3 CRITICAL COMPILATION FAILURES** 

**Status After Priority 1 Fix - 2025-09-09**:
- **✅ PASSING**: Files 1, 3, 5 (RW, VARMA, and PW trends)
- **🚨 CRITICAL**: Files 2, 4, 6 (compilation/structural failures)

**3 Remaining Critical Issues Preventing Stan Compilation**:
1. **File 2 (Shared RW)**: Variable order - `trend` used before declaration
2. **File 4 (Factor AR)**: Linear predictor computation in wrong Stan block
3. **File 6 (CAR+GP)**: Missing GP functions and computation completely

---

## 🔧 **MANDATORY DEVELOPMENT PROTOCOL**

### **STEP 1: Generate Fresh Stan Code (Before ANY Changes)**
**ALWAYS run this first to get baseline**:
```bash
# Use general-purpose agent to generate all 6 files
Rscript target_generation.R
```

### **STEP 2: Complete File Analysis (MANDATORY After Every Fix)**
⚠️ **CRITICAL**: Use **general-purpose agent** to read ALL 6 complete files systematically. 

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

## 🚨 **PRIORITY ISSUES (IN ORDER OF CRITICALITY)**

### **✅ Priority 1: Template System Failure (File 3 - COMPLETED)**

---

### **Priority 2: Variable Declaration Order (File 2 - CRITICAL)**
**Problem**: `trend` matrix used before definition
```stan
// BROKEN - Line 58 uses 'trend' before it's defined
mu_biomass[n] += trend[obs_trend_time_biomass[n], obs_trend_series_biomass[n]];
// But 'trend' not declared until line 93!

// CORRECT - Target has proper order:
// 1. lprior, 2. mu_trend, 3. trend computation, 4. THEN linear predictors
```

**Investigation Steps**:
1. **Use general-purpose agent** to trace multivariate response + shared trend assembly
2. **Examine** variable dependency graph in shared trend generation  
3. **Compare** target file structure vs current generation order
4. **Monkey patch** block assembly functions to see ordering decisions

**Success Criteria**: File 2 compiles with proper variable order

**Regression Test**: Verify Files 1, 3, 4, 5, 6 maintain correct ordering

---

### **Priority 3: Block Placement Error (File 4 - CRITICAL)**
**Problem**: Linear predictor computation in model block instead of transformed parameters
```stan
// BROKEN - In model block (line 149)
vector[N_biomass] mu_biomass = rep_vector(0.0, N_biomass);
mu_biomass += Intercept_biomass + Xc_biomass * b_biomass;
mu_biomass = inv(mu_biomass);  // This is wrong!

// CORRECT - Should be in transformed parameters block
```

**Investigation Steps**:
1. **Use general-purpose agent** to examine factor model + multivariate response handling
2. **Trace** where `mu_biomass = inv(mu_biomass)` statement originates
3. **Compare** transformed parameters vs model block contents in target
4. **Check** if this affects other multivariate factor models

**Success Criteria**: File 4 has linear predictors in transformed parameters block

**Regression Test**: All other multivariate models (Files 2, 3) maintain correct structure

---

### **Priority 4: Missing GP Integration (File 6 - CRITICAL)**
**Problem**: GP functions and computation completely absent
```stan
// MISSING - Functions block should have:
vector gp_exp_quad(data array[] vector x, real sdgp, vector lscale, vector zgp)

// MISSING - Model block should have:
vector[Nsubgp_1_trend] gp_pred_1_trend = gp_exp_quad(Xgp_1_trend, ...);
```

**Investigation Steps**:
1. **Use general-purpose agent** to compare functions blocks current vs target
2. **Examine** CAR trend generation for GP smooth integration
3. **Check** if GP functions are being excluded during assembly
4. **Trace** GP parameter flow from data through to computation

**Success Criteria**: File 6 has complete GP functions and computation

**Regression Test**: Non-GP models (Files 1-5) don't gain unwanted GP components

---

## 🔧 **DEBUGGING TOOLKIT**

### **Monkey Patching Examples**
```r
# Template replacement debugging
debug_templates <- function() {
  original <- mvgam:::apply_trend_templates
  mvgam:::apply_trend_templates <- function(template, data, ...) {
    cat("TEMPLATE:", substr(template, 1, 100), "...\n")
    cat("DATA:", str(data), "\n") 
    result <- original(template, data, ...)
    cat("RESULT:", substr(result, 1, 100), "...\n")
    return(result)
  }
}

# Block assembly debugging  
debug_assembly <- function() {
  original <- mvgam:::assemble_stan_blocks
  mvgam:::assemble_stan_blocks <- function(blocks, ...) {
    cat("ASSEMBLING BLOCKS:", names(blocks), "\n")
    lapply(names(blocks), function(name) {
      cat("BLOCK", name, "length:", nchar(blocks[[name]]), "\n")
      cat("First 5 lines:\n")
      print(head(strsplit(blocks[[name]], "\n")[[1]], 5))
    })
    return(original(blocks, ...))
  }
}
```

### **Complete File Analysis Protocol**
**MANDATORY**: Use general-purpose agent to read and analyze ALL 6 file pairs completely.

**For each file pair (current_stancode_X.stan vs target_stancode_X.stan)**:
1. **Read ENTIRE current file contents** using Read tool
2. **Read ENTIRE target file contents** using Read tool  
3. **Compare every single section systematically**:
   - Functions block: What functions are missing/different?
   - Data block: What parameters are missing/different?
   - Transformed data: What computations are missing/different?
   - Parameters: What declarations are missing/different?
   - Transformed parameters: What variables are missing/different? What order differences?
   - Model block: What statements are missing/different? What placement errors?
   - Generated quantities: What computations are missing/different?

4. **Report exact discrepancies** with line numbers and specific missing content
5. **Identify structural issues** like variable order, block placement, incomplete functions

**NO AUTOMATED PATTERN MATCHING** - This must be human-like analysis of complete file contents.

**Example Analysis Format**:
```
File 3 Analysis:
- Current functions block: Missing rev_mapping function (lines 50-80 in target)
- Current parameters block: Has {lags} placeholders instead of actual "2" (lines 315, 369, 373)  
- Current transformed parameters: Missing proper D_trend computation (lines 325-350 in target)
- Status: BROKEN - Template replacement failed
```

---

## 📊 **SUCCESS METRICS**

**Phase 1 Complete When**:
- All 6 files compile without syntax errors
- Files 1, 5 maintain current near-perfect status  
- Files 2, 3, 4, 6 match their targets structurally

**Regression Prevention**:
- Every fix must be validated against all 6 files
- No previously working functionality may break
- All changes documented with before/after comparisons

**Quality Gates**:
1. ✅ **Generate**: `Rscript target_generation.R` succeeds
2. ✅ **Parse**: All 6 files parse as valid Stan code
3. ✅ **Compare**: General-purpose agent analysis shows matches
4. ✅ **Compile**: All 6 files compile with test data

---

## 🔄 **AGENT HANDOFF PROTOCOL**

1. **MANDATORY START**: Use general-purpose agent to run `Rscript target_generation.R`
2. **MANDATORY ANALYSIS**: Use general-purpose agent for complete file analysis - NO shortcuts
3. **FOCUS**: Work on ONE priority issue at a time  
4. **VALIDATE**: After every change, analyze all 6 files for regressions
5. **DOCUMENT**: Record exact changes made and their effects
6. **DEBUG**: Use monkey patching when root causes are unclear
7. **STOP**: After each priority fix, get user approval before next issue

**Golden Rule**: Better to fix one issue perfectly with no regressions than to fix multiple issues with side effects.