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

## IMMEDIATE PRIORITIES (Updated: 2025-09-15)

1. **CRITICAL: Zero-Dependency Function Investigation** - Investigate 10 functions in `stan_assembly.R` with zero internal dependencies to determine retention necessity:
   - Functions: `parse_model_cmdstanr`, `parse_model_rstan`, `prepare_mvgam_stancode`, `prepare_stan_data`, `prepare_stanvars_for_brms`, `remove_variables_from_block`, `rename_multivariate_parameters`, `rename_univariate_parameters`, `generate_stan_array_declaration`, `extract_variables_from_block`
   - **AGENT TASK**: For each function, investigate:
     a) Direct calls from exported functions or main workflow chains
     b) Dynamic dispatch usage (search for function name strings in `get()` calls)
     c) Test file references (`tests/testthat/`)
     d) Cross-file usage patterns
     e) Whether function serves as utility/infrastructure support
   - **GOAL**: Create definitive retain/remove recommendations with evidence
   - **METHOD**: Use pathfinder agent to trace call chains, grep for dynamic references, examine test patterns

2. **HIGH: File 8 Missing GP Trend Prior** - Add missing `target += std_normal_lpdf(zgp_1_trend);` prior in model block.

3. **HIGH: File 4 Mathematical Ordering Issue** - Move `mu_biomass = inv(mu_biomass);` to occur AFTER trend effects are added, not before. Currently mathematically incorrect.

4. **MEDIUM: File 7 Missing Prior** - Add missing `to_vector(innovations_trend) ~ std_normal();` prior statement.

5. **LOW: File 3 Complex Function Issues** - Complex function implementations may cause compilation issues.

6. **LOW: File 1 Variable Ordering** - lprior statements scattered instead of grouped early.

7. **LOW: File 2 Unused Variable** - `matrix[N_lv_trend, N_lv_trend] Sigma_trend` declared but never used.
