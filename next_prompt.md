## Context: mvgam Stan Compilation Pipeline Refactoring

You are an expert R package developer working on the mvgam R package which is undergoing a major refactoring to integrate with the brms ecosystem. The package generates Bayesian state-space models by combining brms observation models with custom trend specifications using Stan.

## Current Situation

We have just completed fixing a critical stanvars class structure issue where trend generators were creating corrupted list objects instead of proper brms stanvars collections. However, comprehensive testing has revealed systematic failures in the Stan compilation pipeline with only 58% of tests passing.

## Your Task

**IMPORTANT: This is a PLANNING ONLY task. Do NOT write any code. Focus on analysis and planning.**

1. **Read and analyze these files in order:**
   - `active/current-sprint.md` - Contains the critical issues section with test results
   - `active/architecture-decisions.md` - Contains design principles and naming conventions
   - `R/stan_assembly.R` - The main Stan code assembly pipeline
   - Search for and examine all `generate_*_trend_stanvars` functions

2. **Use thinking/reasoning to analyze:**
   - Why the "duplicate sigma" error occurs and where exactly in the code
   - How the nested parameters blocks are being created
   - Why data block stanvars are missing the `x` parameter
   - The flow of stanvar objects through the combination pipeline

3. **Create a focused implementation plan that addresses these immediate action points:**
   - Fix parameter naming: Update all `sigma` → `sigma_trend` in trend generators
   - Debug block nesting: Fix stanvar injection to prevent nested blocks
   - Fix data stanvars: Add proper `x` parameters with data
   - Review combine_stanvars: Ensure proper class handling

4. **For each issue, your plan should specify:**
   - Root cause analysis (why it's happening)
   - Specific files and functions that need changes
   - Order of implementation (dependencies)
   - How to verify the fix works

5. **Constraints to consider:**
   - Maintain compatibility with brms stanvar system
   - Follow the parameter naming conventions in architecture-decisions.md
   - Preserve the two-stage Stan assembly architecture
   - Ensure all changes are testable

Please provide a detailed analysis and implementation plan without writing any code. Focus on understanding the current problems and planning the minimal changes needed to fix them.
