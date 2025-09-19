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

## PRIORITY ISSUES - NEXT AGENT START HERE ⭐

### 1. **PRIORITY**: Parameter Naming Consistency Issues
**Location**: File 8 (current_stancode_8.stan vs target_stancode_8.stan)
**Issue**: Variable naming inconsistency in GP computation
**Evidence**: Current uses `rgp_1`, target uses `rgp_1_trend`
**Investigation Needed**: Systematic review of all trend parameter naming to ensure `_trend` suffix consistency

### 2. **PRIORITY**: Missing Prior Declarations  
**Location**: File 9 (current_stancode_9.stan vs target_stancode_9.stan)
**Issue**: Missing trend parameter priors in transformed parameters block
**Evidence**: Target includes `lprior += student_t_lpdf(Intercept_trend | 3, 0, 2.5);` which may be missing in current
**Investigation Needed**: Comprehensive diff of prior computations between current vs target

### 3. **PRIORITY**: Computational Logic Differences
**Location**: Files 8 & 9 (nonlinear and GP computation approaches)
**Issue**: Different computation strategies between current vs target files
**Evidence**: File 9 uses single-line vs two-operation nonlinear computation; File 8 may have GP placement differences
**Investigation Needed**: Line-by-line logic comparison to identify functional vs cosmetic differences
