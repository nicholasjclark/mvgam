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

## PRIORITY ISSUES

### 1. File 4 - Biomass mu Logic Error  
**Issue**: `inv()` transformation applied before trend addition
**Current**: `mu_biomass = inv(mu_biomass)` then `mu_biomass[n] += trend[...]`
**Target**: Add trend first, then apply `inv()` transformation
**Location**: Lines 146-156 in current_stancode_4.stan

### 2. File 5 - Hard-coded Prior Parameter
**Issue**: Using literal `0.05` instead of `change_scale_trend` parameter
**Current**: `to_vector(delta_trend) ~ double_exponential(0, 0.05);`
**Target**: `to_vector(delta_trend) ~ double_exponential(0, change_scale_trend);`
**Location**: Line 137 in current_stancode_5.stan

### 2. Files 6 & 8 - Order of lprior statements on model block
**Issue**: lprior statements scattered throughout model block
**Target**: all lprior statements together at the end of the block
**Location**: Line 126 in current_stancode_8.stan
