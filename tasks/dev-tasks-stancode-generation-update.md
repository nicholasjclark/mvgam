# Stan Code Generation System - Critical Issues

## NON-NEGOTIABLE WORKFLOW

- Use parallel general-purpose agents to **READ AND ANALYZE ALL** `current_stancode*` vs `target_stancode*` in the `tasks/` directory
- Use the pathfinder agent to read `architecture/stan-data-flow-pipeline.md` and to systematically trace the flow from `stancode()` to complete Stan code creation
- Proceed with ONE priority task at a time
- After proposing fixes, regenerate currents by running `target_generation.R`
- Use parallel general-purpose agents to READ and systematically ANALYZE `current_stancode*` vs `target_stancode*` in the `tasks/` directory again to verify fixes and check for regressions
- TDD approach is crucial, no fix is verified until ALL currents have been compared to their respective targets

## CRITICAL AGENT INSTRUCTIONS

**AGENTS MUST ONLY READ AND COMPARE FILES - NEVER CREATE OR MODIFY**

When analyzing `current_stancode*` vs `target_stancode*` files:

- **DO**: Use Read tool to examine BOTH current and target files completely
- **DO**: Compare line-by-line differences between current vs target
- **DO**: Identify specific line numbers where differences occur
- **DO**: Report exact code snippets showing current vs target differences
- **DO**: Assess compilation readiness and identify syntax/logic errors

- **DO NOT**: Create new files
- **DO NOT**: Modify existing files  
- **DO NOT**: Write code to disk
- **DO NOT**: Use Write, Edit, or any file modification tools
- **DO NOT**: Attempt to "fix" files directly

**AGENT TASK**: Your ONLY job is to READ the existing files and report discrepancies with specific line numbers and code snippets.

## IMMEDIATE PRIORITIES (Updated: 2025-09-14)

### **NEW: CLEANUP TASKS FIRST**

1. **CRITICAL: Clean Up Complex Variable Registry System** - Remove unused complex logic from R/stan_assembly.R:
   - **Lines 5082-5083**: Remove `variable_registry <- create_variable_registry(stancode)` call and related code
   - **Lines 5089-5090**: Remove `variable_registry = variable_registry` parameter from `reconstruct_mu_trend_with_renamed_vars()` call
   - **Lines 5316-5375**: Remove entire `create_variable_registry()` function and `extract_variables_from_block_content()` function
   - **Lines 5392-5440**: Remove complex `extract_variable_name_from_declaration()` function with fragile regex patterns
   - **Lines 5245-5260**: Revert `should_include_in_transformed_parameters()` to original signature (remove `variable_registry` parameter)
   - **Lines 5635**: Change `should_include_in_transformed_parameters(decl, variable_registry)` back to `should_include_in_transformed_parameters(decl)`
   - **Lines 5599**: Remove `variable_registry` parameter from `reconstruct_mu_trend_with_renamed_vars()` function signature

2. **CRITICAL: Test New Variable Deduplication System** - Verify the new simple `deduplicate_stan_variables()` system works:
   - **Test**: Run `target_generation.R` to regenerate all current stancode files
   - **Verify**: Check that duplicate variable errors are eliminated (no "Identifier already in use" compilation errors)
   - **Compare**: Run parallel agents to analyze current vs target files and verify deduplication worked
   - **Integration**: Confirm `deduplicate_stan_variables()` is called after `deduplicate_stan_functions()` at line 335

### **STAN CODE GENERATION ISSUES**

3. **CRITICAL: File 8 GP Dependency Ordering** - Current lines 80-82 have incorrect order. `gp_pred_1_trend` uses `rgp_1` before `rgp_1` is defined. Must reorder declarations.

4. **CRITICAL: File 8 Block Contamination** - Current lines 79, 81, 83-85 have data/parameter declarations contaminating transformed parameters block. These should be in their proper blocks.

5. **HIGH: File 8 Missing GP Trend Prior** - Add missing `target += std_normal_lpdf(zgp_1_trend);` prior in model block.

6. **HIGH: File 4 Mathematical Ordering Issue** - Move `mu_biomass = inv(mu_biomass);` to occur AFTER trend effects are added, not before. Currently mathematically incorrect.

7. **MEDIUM: File 7 Missing Prior** - Add missing `to_vector(innovations_trend) ~ std_normal();` prior statement.

## COMPILATION STATUS (Updated: 2025-01-13)

- **Will Compile**: Files 1, 2, 5, 6 (4/8 files)
- **Will NOT Compile**: Files 3, 4, 7, 8 (4/8 files)
- **Progress**: Semantic refactor successfully included GP declarations in File 8, but dependency ordering and block contamination issues remain.

## MINOR SYNTAX ISSUES (Non-blocking)

### File 1 (RW Basic)
- Variable ordering: lprior statements scattered instead of grouped early

### File 2 (RW Shared)  
- Unused variable: `matrix[N_lv_trend, N_lv_trend] Sigma_trend` declared but never used

## PROGRESS UPDATE (2025-09-14)

### Completed Fix: Simple Variable Deduplication System

**Issue Resolved**: Variables were appearing in both data/parameters blocks AND transformed parameters blocks, causing "Identifier already in use" Stan compilation errors.

**Solution Implemented**: Added `deduplicate_stan_variables()` system in `R/stan_assembly.R` following the exact pattern of `deduplicate_stan_functions()`:

**Key Functions Added** (lines 6834+):
- `deduplicate_stan_variables()` - Main deduplication function with precedence rules
- `extract_variables_from_block()` - Extract variable names from specific Stan blocks  
- `extract_variable_from_line()` - Simple token-based variable name extraction
- `remove_variables_from_block()` - Remove duplicate variables from lower-priority blocks
- `replace_stan_block_content()` - Replace Stan block content with filtered version

**Integration Point**: Line 335 in `generate_combined_stancode()` - calls `deduplicate_stan_variables()` immediately after `deduplicate_stan_functions()`

**Precedence Rules**: data > transformed data > parameters > transformed parameters

**Current Status**: New simple deduplication system implemented. Needs cleanup of old complex Variable Registry System logic and full testing.

### Outstanding Cleanup Required

**Complex Variable Registry System**: The previous attempt using fragile regex patterns and complex variable registries needs to be completely removed from R/stan_assembly.R (see CLEANUP TASKS above).

### Next Steps

1. **CLEANUP FIRST**: Remove all complex Variable Registry System logic
2. **TEST**: Verify new deduplication system eliminates compilation errors
3. **ADDRESS**: Remaining Stan code generation issues (Files 3, 4, 7, 8)
