# Stan Code Generation System - Remaining Issues

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

1. **CRITICAL: File 8 GP Dependency Ordering** - Current lines 79-81 have incorrect order. `gp_pred_1_trend` uses `rgp_1` before `rgp_1` is defined. Must reorder declarations.

2. **CRITICAL: File 8 Block Contamination** - Current lines 79-81 have declarations that should be in proper blocks rather than transformed parameters.

3. **HIGH: File 8 Missing GP Trend Prior** - Add missing `target += std_normal_lpdf(zgp_1_trend);` prior in model block.

4. **HIGH: File 4 Mathematical Ordering Issue** - Move `mu_biomass = inv(mu_biomass);` to occur AFTER trend effects are added, not before. Currently mathematically incorrect.

5. **MEDIUM: File 7 Missing Prior** - Add missing `to_vector(innovations_trend) ~ std_normal();` prior statement.

6. **LOW: File 3 Complex Function Issues** - Complex function implementations may cause compilation issues.

7. **LOW: File 1 Variable Ordering** - lprior statements scattered instead of grouped early.

8. **LOW: File 2 Unused Variable** - `matrix[N_lv_trend, N_lv_trend] Sigma_trend` declared but never used.

## COMPILATION STATUS (Updated: 2025-09-14)

- **Will Compile**: Files 1, 2, 5, 6 (4/8 files) - Duplicate variable issues resolved
- **Will NOT Compile**: Files 3, 4, 7, 8 (4/8 files) - Various structural issues remain
- **Progress**: Variable deduplication system now functional, eliminating duplicate declaration errors

## COMPLETED WORK (2025-09-14)

### Variable Deduplication System
- Removed complex variable registry system (200+ lines of fragile regex code)
- Fixed `extract_variable_from_line()` bug where Stan constraint syntax was incorrectly treated as assignments
- Implemented token-based variable extraction that correctly handles `<lower=1>` and similar constraints
- System now successfully removes duplicate variable declarations across all Stan blocks
- Precedence rules: data > transformed data > parameters > transformed parameters
