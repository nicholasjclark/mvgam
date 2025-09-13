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

### **CURRENT STATUS: Comprehensive Mu Analysis System Implemented**

**Major Implementation Complete**: Created new robust mu expression classification and analysis system
- **New System**: `R/mu_expression_analysis.R` with comprehensive structural analysis functions
- **Test Success**: All 186 mu expression classification tests pass (100% success rate)  
- **Architecture**: Replaced regex-based pattern matching with structural analysis approach
- **Capabilities**: Handles all brms mu varieties with execution order planning and dependency analysis

### **IMMEDIATE PRIORITIES**

#### **Priority 1: Code Cleanup - Remove Redundant Functions** ⚠️ **URGENT**
**Status**: [ ] Not Started  
**Goal**: Eliminate duplicate/obsolete functions replaced by new system

**Functions to REMOVE (Complete Redundancy)**:
- `extract_mu_construction_from_model_block()` (R/stan_assembly.R:5559-5595) 
- `extract_mu_assignment_lines()` (R/stan_assembly.R:5258-5293) - Duplicate of new version
- `extract_referenced_variables_from_mu_lines()` (R/stan_assembly.R:5302-5318)
- `parse_variable_references()` (R/stan_assembly.R:5464-5499)

**Functions to KEEP**:
- `reconstruct_mu_trend_with_renamed_vars()` - Core reconstruction, interface-compatible  
- `find_variable_declarations()` - Shared utility used by new system
- Code injection utilities (`insert_after_mu_lines_in_model_block()`, etc.)

#### **Priority 2: Generate Target Files and Assess Impact** 
**Status**: [ ] Not Started - Depends on Priority 1 completion  
**Goal**: Determine if comprehensive mu system resolved original target file issues

**Critical Questions**:
- Do the original Stan code generation problems still exist after new mu system?
- Are Steps 2-5 from original plan still needed or were they symptoms of the old regex system?
- Which target files now pass vs fail with the new comprehensive approach?

---

## **ORIGINAL PLAN SECTIONS (For Reference)**

**Note**: The sections below represent the original 5-step plan. Priority 1-2 above should be completed first to determine which (if any) of these steps are still needed after implementing the comprehensive mu analysis system.

**🔬 MANDATORY PREPARATION - Use Subagents for Analysis**:
1. **Use pathfinder agent** to analyze current variable extraction in `extract_and_rename_stan_blocks()` and `find_variable_declarations()`
2. **Use parallel general-purpose agents** to analyze variable patterns by type:
   - Agent 1: Function names across all patterns (`mo`, `gp_exp_quad`, `spd_gp_*`, etc.)
   - Agent 2: Index variables (`n`, `i`, `j`, `m`, `d`, special indices like `Igp_1_1`)
   - Agent 3: Data/parameter variables that need mapping (`bsp`, `simo_1`, `Xgp_1`, `Xs`, etc.)
   - Agent 4: GLM optimization variables (`Xc`, `b`, `means_X`)
   - Instruct ALL agents to run and inspect tasks/brms_mu_variety.R FIRST

**🎯 ENHANCED VARIABLE CLASSIFICATION**:

**Implementation**:
```r
# Extend R/mu_expression_analysis.R with comprehensive analysis
analyze_expression_variables(expr, classification) -> list(
  parameters,     # Variables needing _trend mapping
  functions,      # Function names to exclude from mapping  
  indices,        # Loop/index variables to exclude
  dependencies,   # Variables this expression depends on
  glm_variables,  # Additional GLM optimization variables
  needs_mapping   # Final list of variables requiring mapping
)

# Enhanced function detection for all pattern types
extract_function_dependencies(expr) # mo(), gp_exp_quad(), spd_gp_*, sqrt(), etc.
extract_index_dependencies(expr)    # n, i, j, m, d, Igp_*, Jgp_*, etc.  
extract_glm_dependencies(stancode)  # Xc, b, means_X from GLM optimization
```

**🔑 CRITICAL VARIABLE CATEGORIES**:

**Functions to EXCLUDE from mapping**:
- Built-in Stan: `mo()`, `sqrt()`, `dot_product()`, `sum()`, `rows()`
- brms GP functions: `gp_exp_quad()`, `spd_gp_exp_quad()`, `spd_gp_matern32()`
- Mathematical: `exp()`, `log()`, `square()`, `cholesky_decompose()`

**Index Variables to EXCLUDE from mapping**:
- Loop indices: `n`, `i`, `j`, `k`, `m`, `d` 
- GP-specific indices: `Igp_1_1`, `Jgp_1`, `Nsubgp_1`
- Array dimension references in patterns

**Parameters to INCLUDE in mapping**:
- Monotonic: `bsp`, `simo_1`, `Xmo_1`, `con_simo_1`, `Jmo`, `Ksp`, `Imo`
- GP variables: `sdgp_1`, `lscale_1`, `zgp_1`, `Xgp_1`, `slambda_1`
- Spline variables: `bs`, `s_1_1`, `sds_1`, `zs_1_1`, `Xs`, `Zs_1_1`
- Random effects: `r_1_1`, `sd_1`, `z_1`, `J_1`, `Z_1_1`
- GLM optimization: `b`, `Xc`, `means_X` (when GLM pattern detected)

**🧪 COMPREHENSIVE TESTING STRATEGY**:
1. **Pattern-Specific Tests**: Each of 11+ patterns with correct variable classification
2. **GLM Optimization Tests**: Proper detection of `Xc`, `b` variables in GLM cases
3. **Dependency Chain Tests**: Variables extracted for multi-stage computations
4. **Target 7 Validation**: Monotonic case extracts `bsp`, `simo_1`, `Xmo_1` but excludes `mo`, `n`
5. **False Positive Tests**: Ensure functions/indices never included in mapping

**Success Criteria**: 
- Perfect variable classification for all brms_mu_variety.R patterns
- No functions or indices incorrectly included in variable mapping
- GLM optimization variables properly detected and included
- Target 7 monotonic + CAR case resolves all missing variable errors

#### **Step 3: Comprehensive Variable Resolution System** 🎯 *Surgical Replacement of Current System*
**Goal**: Replace current variable mapping with **systematic Stan block search** handling all pattern complexities  
**Duration**: 3-4 hours | **Status**: [ ] Not Started

**🔬 MANDATORY PREPARATION - Use Subagents for Integration**:
1. **Use pathfinder agent** to locate exact lines in `extract_and_rename_stan_blocks()` that need surgical replacement
2. **Use code-reviewer agent** to review the replacement strategy before implementation
3. **Use general-purpose agent** to analyze current `find_variable_declarations()` and identify enhancement needs
4. **Use pathfinder agent** to map dependencies between variable resolution functions

**🎯 SURGICAL REPLACEMENT TARGET**:
**File**: `R/stan_assembly.R`  
**Function**: `extract_and_rename_stan_blocks()`  
**Lines**: 5151-5182 (current variable mapping logic)  
**Replacement**: Enhanced comprehensive variable resolution system

**Implementation**:
```r
# SURGICAL REPLACEMENT in R/stan_assembly.R extract_and_rename_stan_blocks()
resolve_variables_comprehensively(variables, stancode, pattern_info) -> found_variables

# Enhanced resolution logic
resolve_variables_comprehensively <- function(variables, stancode, pattern_info) {
  # Use Step 2 variable classification to only search variables needing mapping
  variables_needing_mapping <- filter_mappable_variables(variables, pattern_info)
  
  # Systematic block search with priority ordering
  found_vars <- list()
  
  # Priority 1: Parameters block (most trend variables)
  found_vars$parameters <- search_parameters_block(stancode, variables_needing_mapping)
  
  # Priority 2: Data block (monotonic, GP data structures)
  found_vars$data <- search_data_block(stancode, variables_needing_mapping) 
  
  # Priority 3: Transformed parameters (computed variables like r_1_1, s_1_1)
  found_vars$transformed_params <- search_transformed_parameters_block(stancode, variables_needing_mapping)
  
  # Priority 4: Transformed data (GLM optimization variables like Xc, means_X)
  found_vars$transformed_data <- search_transformed_data_block(stancode, variables_needing_mapping)
  
  # Priority 5: Computed variables (GP predictions, multi-stage computations)
  found_vars$computed <- extract_computed_variables(stancode, variables_needing_mapping)
  
  # Validation and comprehensive error reporting
  validate_variable_resolution(variables_needing_mapping, found_vars)
  
  return(unique(unlist(found_vars)))
}

# Handle GLM optimization cases
handle_glm_optimization_variables <- function(stancode, detected_glm_patterns) {
  # Extract additional variables needed for GLM cases (Xc, b, means_X)
  # Add missing terms to variable mapping for complete mu_trend construction
}
```

**🔑 KEY ENHANCEMENTS**:

1. **Pattern-Aware Search**: Use Step 2 pattern classification to guide search strategy
2. **Block Priority System**: Search most likely blocks first for efficiency  
3. **GLM Optimization Handling**: Special case detection and variable extraction
4. **Multi-Stage Dependencies**: Handle complex dependency chains from Step 1
5. **Enhanced Error Messages**: Detailed reporting of where variables were/weren't found

**🧪 COMPREHENSIVE TESTING STRATEGY**:
1. **Target 7 Monotonic**: Should find `bsp`, `simo_1`, `Xmo_1` in parameters/data blocks
2. **GP Pattern Tests**: Should find all GP variables across different blocks  
3. **GLM Optimization Tests**: Should detect and include `Xc`, `b`, `means_X` variables
4. **Multi-Stage Tests**: Should handle dependency chains for complex patterns
5. **Regression Tests**: All previously working patterns continue to work
6. **Performance Tests**: Search efficiency with large Stan models

**🎯 INTEGRATION VALIDATION**:
- **Before replacement**: Run `Rscript target_generation.R` to establish baseline
- **After replacement**: Verify all 7 target files continue to work + Target 7 now succeeds
- **Use general-purpose agent**: Analyze all 7 current vs target files for any regressions

**Success Criteria**: 
- Target 7 generates successfully without "missing variables" error
- All existing working target files continue to generate correctly  
- No performance degradation in variable resolution
- Clean integration with Steps 1-2 outputs

#### **Step 4: Advanced Structure-Aware Code Generation** 🏗️ *Complex Multi-Pattern Assembly*
**Goal**: Generate **sophisticated mu_trend construction** handling all 11+ patterns with proper ordering, dependencies, and structure  
**Duration**: 4-6 hours | **Status**: [ ] Not Started

**🔬 MANDATORY PREPARATION - Use Subagents for Code Generation Strategy**:
1. **Use pathfinder agent** to analyze current `reconstruct_mu_trend_with_renamed_vars()` function structure and limitations
2. **Use code-reviewer agent** to review the enhanced code generation approach before implementation  
3. **Use parallel general-purpose agents** to analyze target structures by pattern type:
   - Agent 1: Loop structures needed (monotonic, random effects patterns)
   - Agent 2: Vectorized patterns (GP, splines) and proper ordering
   - Agent 3: Multi-stage dependency ordering (GP spectral, computed variable chains)
   - Agent 4: GLM optimization additions (`mu_trend += Xc_trend * b_trend`)
   - Instruct ALL agents to run and inspect tasks/brms_mu_variety.R FIRST

**🎯 ENHANCED CODE GENERATION ARCHITECTURE**:

**Implementation**:
```r
# MAJOR ENHANCEMENT to R/stan_assembly.R reconstruct_mu_trend_with_renamed_vars()
generate_structured_mu_trend(analyzed_expressions, mapping, time_param, pattern_info) -> stan_code_lines

# Multi-pattern code generation system
generate_structured_mu_trend <- function(analyzed_expressions, mapping, time_param, pattern_info) {
  # Stage 0: Initialization (always first)
  code_blocks <- list()
  code_blocks$init <- generate_mu_trend_initialization(time_param)
  
  # Stage 1: Computed variable declarations (execution_order=0)
  computed_vars <- filter_by_execution_order(analyzed_expressions, 0)
  if (length(computed_vars) > 0) {
    code_blocks$computed <- generate_computed_declarations(computed_vars, mapping)
  }
  
  # Stage 2: Simple vectorized assignments (execution_order=1)  
  simple_assigns <- filter_by_execution_order(analyzed_expressions, 1)
  if (length(simple_assigns) > 0) {
    code_blocks$vectorized <- generate_vectorized_assignments(simple_assigns, mapping)
  }
  
  # Stage 3: GLM optimization additions (when detected)
  if (pattern_info$has_glm_optimization) {
    code_blocks$glm_additions <- generate_glm_optimization_additions(pattern_info$glm_variables, mapping)
  }
  
  # Stage 4: Indexed subset assignments (execution_order=2)
  indexed_subsets <- filter_by_execution_order(analyzed_expressions, 2) 
  if (length(indexed_subsets) > 0) {
    code_blocks$indexed <- generate_indexed_assignments(indexed_subsets, mapping)
  }
  
  # Stage 5: Loop-based assignments (execution_order=3)
  loop_expressions <- filter_by_execution_order(analyzed_expressions, 3)
  if (length(loop_expressions) > 0) {
    code_blocks$loops <- generate_loop_structures(loop_expressions, mapping, time_param)
  }
  
  # Combine with proper spacing and ordering
  return(assemble_code_blocks(code_blocks))
}

# Advanced loop structure generation
generate_loop_structures <- function(loop_expressions, mapping, time_param) {
  # Group compatible loop expressions that can share the same for loop
  loop_groups <- group_compatible_loops(loop_expressions)
  
  all_loop_code <- character(0)
  for (group in loop_groups) {
    loop_body <- generate_loop_body(group, mapping) 
    loop_wrapper <- c(
      "",  # Empty line for readability
      paste0("for (n in 1:", time_param, ") {"),
      paste0("  ", loop_body),  # Proper indentation
      "}"
    )
    all_loop_code <- c(all_loop_code, loop_wrapper)
  }
  
  return(all_loop_code)
}

# GLM optimization handling
generate_glm_optimization_additions <- function(glm_variables, mapping) {
  # Add missing terms like: mu_trend += Xc_trend * b_trend;
  missing_terms <- character(0)
  if ("Xc" %in% glm_variables && "b" %in% glm_variables) {
    missing_terms <- c(missing_terms, "mu_trend += Xc_trend * b_trend;")
  }
  return(missing_terms)
}
```

**🔑 SOPHISTICATED FEATURES**:

1. **Multi-Stage Dependency Handling**: Proper ordering for complex GP spectral patterns
2. **Loop Optimization**: Group compatible expressions into shared loops
3. **GLM Optimization Integration**: Add missing `mu_trend += Xc_trend * b_trend` terms
4. **Indentation Management**: Clean, readable Stan code formatting
5. **Pattern-Specific Generation**: Different strategies for different expression types
6. **Execution Order Enforcement**: Stage 0 → Stage 1 → Stage 2 → Stage 3 ordering

**🎯 TARGET STRUCTURE EXAMPLES**:

**Monotonic + CAR (Target 7)**:
```stan
vector[N_trend] mu_trend = rep_vector(0.0, N_trend);
mu_trend += Intercept_trend;

for (n in 1:N_trend) {
  mu_trend[n] += (bsp_trend[1]) * mo(simo_1_trend, Xmo_1_trend[n]);
}
```

**Multi-GP Complex**:
```stan
vector[N_trend] mu_trend = rep_vector(0.0, N_trend);
vector[NBgp_1] rgp_1_trend = sqrt(spd_gp_matern32(...)) .* zgp_1_1_trend;
vector[Nsubgp_1] gp_pred_1_trend = Xgp_1_1_trend * rgp_1_1_trend;
mu_trend[Igp_1_1_trend] += Cgp_1_1_trend .* gp_pred_1_1_trend[Jgp_1_1_trend];
mu_trend += Intercept_trend;
```

**🧪 COMPREHENSIVE TESTING STRATEGY**:
1. **Target 7 Structure**: Exact match with proper loop formatting and indentation
2. **Multi-Pattern Tests**: Complex patterns with proper execution ordering
3. **GLM Integration Tests**: Missing terms properly added for GLM optimization cases
4. **Dependency Chain Tests**: Multi-stage computations in correct order
5. **Indentation Tests**: Clean, readable output formatting
6. **Performance Tests**: Code generation efficiency with complex patterns

**Success Criteria**: 
- Target 7 generates with exact expected loop structure and indentation
- All complex multi-pattern cases generate with proper ordering
- GLM optimization cases include all necessary missing terms
- Generated code is clean, readable, and Stan-compilable
- No regressions in existing working patterns

#### **Step 5: Comprehensive Integration & Full System Validation** ✅ *Complete System Integration & Testing*
**Goal**: **Comprehensive integration testing**, optimization, documentation, and **bulletproof validation** across all patterns  
**Duration**: 4-5 hours | **Status**: [ ] Not Started

**🔬 MANDATORY PREPARATION - Use Subagents for Comprehensive Validation**:
1. **Use general-purpose agent** to run `Rscript target_generation.R` and analyze ALL 7 target file outputs
2. **Use parallel general-purpose agents** for detailed file-by-file comparison:
   - Agent 1: Compare current vs target for files 1-2 (simple patterns)
   - Agent 2: Compare current vs target for files 3-4 (complex multivariate patterns)
   - Agent 3: Compare current vs target for files 5-6 (GP and CAR patterns)
   - Agent 4: Compare current vs target for file 7 (monotonic + CAR - critical success test)
3. **Use code-reviewer agent** to review the complete integrated system implementation
4. **Use r-test-runner agent** to execute comprehensive test suites for all new functionality

**🎯 COMPREHENSIVE INTEGRATION TESTING**:

**Phase 1: System Integration**
```r
# Integration testing protocol
integration_test_protocol <- function() {
  # Test all 11+ patterns from brms_mu_variety.R
  test_all_pattern_types()
  
  # Test multi-stage dependency chains
  test_dependency_resolution()
  
  # Test GLM optimization handling
  test_glm_optimization_cases()
  
  # Test performance with complex models
  performance_benchmark_suite()
}
```

**Phase 2: Target File Validation**
1. **Baseline Comparison**: Pre-integration vs post-integration results
2. **Target 7 Critical Test**: Monotonic + CAR must generate successfully
3. **Regression Prevention**: Files 1-6 must continue working without changes
4. **Structure Validation**: Generated code matches expected patterns exactly

**Phase 3: Edge Case & Error Handling**
1. **Malformed Pattern Tests**: How system handles unexpected brms patterns
2. **Missing Variable Tests**: Comprehensive error reporting validation
3. **Performance Stress Tests**: Large Stan models with many patterns
4. **Memory Usage Tests**: Ensure no memory leaks in pattern processing

**🔑 COMPREHENSIVE FEATURES TO VALIDATE**:

1. **Pattern Recognition System**: All 11+ patterns correctly classified
2. **Variable Resolution System**: All variable types correctly mapped/excluded
3. **Dependency Management**: Multi-stage computations properly ordered
4. **Code Generation System**: All structure types properly generated
5. **GLM Optimization**: Missing terms correctly added
6. **Error Handling**: Clear, actionable error messages with `insight::format_error()`
7. **Performance**: No significant slowdown in Stan code generation

**🧪 BULLETPROOF TESTING STRATEGY**:

**Target File Success Matrix**:
```
File 1 (Simple RW): ✅ Working → ✅ Continue Working
File 2 (GP): ✅ Working → ✅ Continue Working  
File 3 (VARMA): ❌ Issues → ✅ Validate No Regressions
File 4 (Complex): ❌ Issues → ✅ Validate No Regressions
File 5 (Splines): ✅ Working → ✅ Continue Working
File 6 (CAR+GP): ✅ Working → ✅ Continue Working
File 7 (Monotonic+CAR): ❌ FAILING → ✅ SUCCESS REQUIRED
```

**Pattern Coverage Tests**:
- [x] Monotonic loops: `for (n in 1:N) { mu[n] += (bsp[1]) * mo(...); }`
- [x] GP direct: `mu += Intercept + gp_pred_1[Jgp_1];`
- [x] GP spectral: Multi-stage `rgp_1` → `gp_pred_1` → `mu`
- [x] Multi-GP indexed: `mu[Igp_1_1] += Cgp_1_1 .* gp_pred_1_1[Jgp_1_1];`
- [x] Splines vectorized: `mu += Intercept + Xs * bs + Zs_1_1 * s_1_1;`
- [x] Random effects loops: `mu[n] += r_1_1[J_1[n]] * Z_1_1[n];`
- [x] GLM optimization: Missing `mu += Xc * b` terms added

**Performance Benchmarks**:
- Pattern classification speed: <100ms for typical Stan models
- Variable resolution speed: <200ms for complex models  
- Code generation speed: <50ms for any pattern complexity
- Memory usage: No significant increase over current system

**🎯 DOCUMENTATION & CODE REVIEW**:

1. **Complete roxygen2 Documentation**: All new functions fully documented
2. **Code Review Compliance**: Passes code-reviewer agent with no HIGH priority issues
3. **Architecture Documentation**: Update system design docs with new pattern architecture
4. **Performance Documentation**: Benchmark results and optimization notes

**🔄 FINAL VALIDATION PROTOCOL**:

```bash
# Complete system validation sequence
Rscript target_generation.R                    # Generate all 7 files
# Use general-purpose agent to analyze results
# Use code-reviewer agent for final code review
# Use r-test-runner agent for comprehensive tests
```

**Success Criteria**: 
- **CRITICAL**: Target 7 (monotonic + CAR) generates successfully without errors
- All 7 target files generate (no regressions in working files)
- All 11+ brms patterns correctly handled
- GLM optimization cases properly integrated
- Performance benchmarks met
- Code review passes with no HIGH priority issues
- Comprehensive test suite passes 100%
- Documentation complete and accurate

**Final Deliverable**: **Bulletproof mu pattern analysis system** ready for production use with full brms compatibility

### **IMPLEMENTATION GUIDELINES**

**Incremental Development**:
- Each step is independently testable and deployable
- Steps 1-2 can be developed/tested without touching existing Stan generation  
- Step 3 surgically replaces current problematic code
- Steps 4-5 integrate the complete system

**Regression Prevention**:
- Run `Rscript target_generation.R` after each step
- Use general-purpose agent to validate all 6 files remain working
- Document any changes to existing behavior

**Priority Level**: HIGH - This addresses the root architectural issues causing multiple file generation problems

---

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
