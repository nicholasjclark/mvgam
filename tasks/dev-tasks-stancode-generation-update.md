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

### Priority 0: CRITICAL MISMATCH - Stan Code vs Stan Data Variables
**ISSUE**: Stan code declares data variables not present in corresponding standata files
**AFFECTED**: Scenario 3 (VARMA trends) - compilation fails due to missing data variables
**ERROR MESSAGE**: "Missing input data for the following data variables: Ks_count, Xs_count, nb_count_1, knots_count_1, Zs_count_1_1, Ks_biomass, Xs_biomass, nb_biomass_1, knots_biomass_1, Zs_biomass_1_1, K_trend, Kc_trend, X_trend"

**PROBLEM ANALYSIS**:
- **Stan Code Declares**: Spline-related variables (`Ks_count`, `Xs_count`, `knots_count_1`, `Zs_count_1_1`, etc.)
- **Stan Data Missing**: These variables not generated in `current_standata_3.rds`
- **Root Cause**: Mismatch between Stan code generation and Stan data generation processes
- **Impact**: Model cannot even initialize - immediate compilation failure

**MISSING DATA VARIABLES (from current_stancode_3.stan)**:
```stan
// Spline data for count response
int Ks_count;
matrix[N_count, Ks_count] Xs_count;
int nb_count_1;
array[nb_count_1] int knots_count_1;
matrix[N_count, knots_count_1[1]] Zs_count_1_1;

// Spline data for biomass response  
int Ks_biomass;
matrix[N_biomass, Ks_biomass] Xs_biomass;
int nb_biomass_1;
array[nb_biomass_1] int knots_biomass_1;
matrix[N_biomass, knots_biomass_1[1]] Zs_biomass_1_1;

// Trend design matrix
int<lower=1> K_trend;
int<lower=1> Kc_trend;
matrix[N_trend, K_trend] X_trend;
```

**SYSTEMATIC PROBLEM**: 
- Stan code generation includes brms spline terms
- Stan data generation doesn't create corresponding spline data
- Indicates broader issue with brms→mvgam Stan integration

**VALIDATION**: Ensure `current_standata_3.rds` contains ALL variables declared in `current_stancode_3.stan`
**TIME LIMIT**: 15 minutes

### Priority 1: Debug complex model fitting failures
- Re-run complete `target_fitting.R` and summarise findings
**ISSUE**: Even with fixed dimensions, some complex models fail during MCMC sampling
**TASK**:
- Focus on models that compile but fail fitting (post-dimension fixes)
- Examine Stan model structure for potential initialization or parameter constraint issues
- Check for: unbounded parameters, improper priors, matrix rank issues in factor models
- Generate diagnostic output for failed models using cmdstan error messages
**VALIDATION**: Identify specific Stan modeling issues preventing successful sampling
**TIME LIMIT**: 15 minutes
