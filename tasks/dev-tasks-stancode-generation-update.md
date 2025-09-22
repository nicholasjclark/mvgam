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
  
### Priority 1: Modify `target_fitting.R` to fit quick models for each target [COMPLETED]
- Inspect the contents of `tasks/` to see the current_stancode and current_standata objects
- Read `target_generation.R` in root to understand where these model files came from
- Modify `target_fitting.R` to attempt fitting for each of the modelling scenarios and to log useful information about warnings or errors
- Run the modified `target_fitting.R` and summarise findings for the user

**RESULTS**: 7/9 compilation success, 4/9 fitting success. Key issues identified:
- Stan syntax errors (missing semicolons) in TARGET 6 & 8
- Data dimension mismatches in multivariate models (TARGET 2, 3, 4)
- Complex fitting failures requiring investigation

### Priority 2: Fix Stan syntax errors in prior statements (15 min task)
**ISSUE**: TARGET 6 & 8 fail compilation due to missing semicolons in lprior statements
**TASK**: 
- Locate and inspect `lprior` rearrangement steps in `R/stan_polish.R`
- Think hard about why semicolons are being lost and, if necessary, write a debugging script to investigate
- Make necessary edits AFTER receiving code-reviewer approval
**VALIDATION**: Re-run `target_generation.R` for TARGET 6 & 8, verify compilation success
**TIME LIMIT**: 15 minutes

### Priority 3: Investigate data dimension calculation for multivariate models (15 min task)
**ISSUE**: TARGET 2, 3, 4 show `times_trend; dims declared=(72,3); dims found=(24,3)` mismatch
**TASK**:
- Use pathfinder agent to locate dimension calculation functions in time series processing
- Examine how `n_time_trend` is calculated for multivariate vs univariate models
- Compare expected vs actual dimensions: declared (72,3) suggests 24*3 time points, found (24,3) suggests incorrect calculation
- Identify the specific function/location where multivariate time dimensions are incorrectly computed
**VALIDATION**: Document exact location and logic causing dimension mismatch
**TIME LIMIT**: 15 minutes

### Priority 4: Fix times_trend dimension calculation logic (15 min task)
**ISSUE**: Multivariate models calculate time dimensions incorrectly
**TASK**:
- Based on Priority 3 findings, fix the dimension calculation logic
- Ensure multivariate models properly account for: n_time × n_series structure
- Verify `times_trend` array is correctly sized as (n_time * n_series, n_lv_trend) for multivariate cases
- Focus on functions that create time indexing arrays for Stan data
**VALIDATION**: Re-run `target_generation.R` for TARGET 2, 3, 4 and verify no dimension errors
**TIME LIMIT**: 15 minutes

### Priority 5: Debug complex model fitting failures (15 min task)
**ISSUE**: Even with fixed dimensions, some complex models fail during MCMC sampling
**TASK**:
- Focus on models that compile but fail fitting (post-dimension fixes)
- Examine Stan model structure for potential initialization or parameter constraint issues
- Check for: unbounded parameters, improper priors, matrix rank issues in factor models
- Generate diagnostic output for failed models using cmdstan error messages
**VALIDATION**: Identify specific Stan modeling issues preventing successful sampling
**TIME LIMIT**: 15 minutes

### Priority 6: Comprehensive validation and testing (15 min task)
**ISSUE**: Need to verify all fixes work together
**TASK**:
- Re-run complete `target_fitting.R` after all fixes implemented
- Compare results to initial baseline: aim for >90% compilation success, >70% fitting success
- Document remaining issues (if any) with specific error messages and line numbers
- Generate final validation report comparing before/after success rates
**VALIDATION**: All 9 targets should compile successfully, most should fit successfully
**TIME LIMIT**: 15 minutes
