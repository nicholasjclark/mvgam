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

## IMMEDIATE PRIORITIES (Updated: 2025-01-13)

1. **CRITICAL: File 8 GP Dependency Ordering** - Current lines 80-82 have incorrect order. `gp_pred_1_trend` uses `rgp_1` before `rgp_1` is defined. Must reorder declarations.

2. **CRITICAL: File 8 Block Contamination** - Current lines 79, 81, 83-85 have data/parameter declarations contaminating transformed parameters block. These should be in their proper blocks.

3. **HIGH: File 8 Missing GP Trend Prior** - Add missing `target += std_normal_lpdf(zgp_1_trend);` prior in model block.

4. **HIGH: File 4 Mathematical Ordering Issue** - Move `mu_biomass = inv(mu_biomass);` to occur AFTER trend effects are added, not before. Currently mathematically incorrect.

5. **MEDIUM: File 7 Missing Prior** - Add missing `to_vector(innovations_trend) ~ std_normal();` prior statement.

## COMPILATION STATUS (Updated: 2025-01-13)

- **Will Compile**: Files 1, 2, 5, 6 (4/8 files)
- **Will NOT Compile**: Files 3, 4, 7, 8 (4/8 files)
- **Progress**: Semantic refactor successfully included GP declarations in File 8, but dependency ordering and block contamination issues remain.

## MINOR SYNTAX ISSUES (Non-blocking)

### File 1 (RW Basic)
- Variable ordering: lprior statements scattered instead of grouped early

### File 2 (RW Shared)  
- Unused variable: `matrix[N_lv_trend, N_lv_trend] Sigma_trend` declared but never used

## PROGRESS UPDATE (2025-01-13)

### Completed Fix: Semantic Refactor for GP Declaration Inclusion

**Issue Resolved**: The `should_include_in_transformed_parameters()` function was using brittle regex patterns that rejected valid GP variable declarations.

**Solution Implemented**: Replaced 15+ fragile regex patterns with 3 semantic rules in `R/stan_assembly.R`:
1. No assignment = data/parameter declaration (exclude)
2. Simple initialization = basic constant/vector init (exclude)  
3. Complex assignment = computation requiring dependency ordering (include)

**Current Status**: GP declarations now successfully included in transformed parameters blocks across all files. Dependency tracking system working correctly.

**Next Steps**: Address remaining compilation issues with proper declaration ordering and block cleanup.
