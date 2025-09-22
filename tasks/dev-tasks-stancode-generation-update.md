# Stan Code Generation System - Remaining Tasks

## NON-NEGOTIABLE WORKFLOW
- Proceed with ONE priority task at a time
- Use the pathfinder agent to read `architecture/stan-data-flow-pipeline.md` and to systematically trace the flow from `stancode()` to complete Stan code creation
- The code-reviewer agent MUST be used to approve of any edits BEFORE they are implemented
- Following any edits to R code, agents MUST:
  1. Regenerate currents by running `target_generation.R`
  2. Use parallel general-purpose agents to **READ AND ANALYZE ALL** `current_stancode*` vs `target_stancode*` in the `tasks/` directory
  3. Adhere to a STRICT TDD approach: no fix is verified until ALL currents have been compared to their respective targets

## CRITICAL SUBAGENT INSTRUCTIONS

**SUBAGENTS MUST ONLY READ AND COMPARE FILES - NEVER CREATE OR MODIFY**

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

**AGENT TASK**: Your ONLY job is to read the existing files and report discrepancies with specific line numbers and code snippets.
  
### Priority 1: DRY Consolidation - mvgam() calls stancode() internally
**CRITICAL ARCHITECTURAL IMPROVEMENT**

Based on pathfinder analysis, both `mvgam()` and `stancode()` flows converge at `generate_combined_stancode_and_data()`. Consolidate so mvgam() internally uses stancode() for single source of truth.

**Sub-tasks:**
0.1. **Modify mvgam_single_dataset() lines 140-144** (mvgam_core.R)
   - Replace direct `generate_combined_stancode_and_data()` call
   - Use `stancode()` and `standata()` inspection functions instead
   - Ensure mvgam_formula() construction works correctly

0.2. **Integrate polish_generated_stan_code() into stancode() pipeline**
   - Add polishing step to `generate_combined_stancode()` after line 334
   - Apply `paste(polish_generated_stan_code(combined_stancode), collapse = "\n")`
   - Ensure polishing happens before validation

0.3. **Test consolidation with code-reviewer**
   - Verify both `mvgam()` and `stancode()` produce identical results
   - Ensure no regressions in Stan code generation
   - Validate that polishing applies to both paths

0.4. **Update function dependencies and documentation**
   - Update roxygen docs for affected functions
   - Ensure dependency chain is clear and maintainable

**Expected Outcome**: 
- Single source of truth for Stan code generation in `stancode()`
- Automatic polishing applied consistently to both `mvgam()` and `stancode()` paths
- Cleaner architecture with DRY principles
