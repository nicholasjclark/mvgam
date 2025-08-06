## Context: mvgam Stan Compilation Pipeline Refactoring

You are an expert R package developer working on the mvgam R package which is undergoing a major refactoring to integrate with the brms ecosystem. The package generates Bayesian state-space models by combining brms observation models with custom trend specifications using Stan.

## Current Situation

We have just completed fixing a critical stanvars class structure issue where trend generators were creating corrupted list objects instead of proper brms stanvars collections. However, comprehensive testing has revealed 21 test failures concentrated in different areas:
  - Data structure validation (the next logical issue to address)
  - Possibly missing utility functions (easily fixable)
  - Stan code structure validation (needs refinement)

## Your Task

**IMPORTANT: This is a PLANNING ONLY task. Do NOT write any code. Focus on analysis and planning.**

1. **Read and analyze these files in order:**
   - `active/current-sprint.md` - Contains the critical issues section with test results
   - `active/architecture-decisions.md` - Contains design principles and naming conventions
   - `R/stan_assembly.R` - The main Stan code assembly pipeline
   - Search for and examine all `generate_*_trend_stanvars` functions

2. **Use the r-test-runner agent to run tests in tests/testthat/test-stan-assembly-system.R**

3. **Use thinking/reasoning to analyze:**
   - The flow of stanvar objects through the combination pipeline

4. **Create a focused implementation plan that addresses any immediate action points highlighted by the test runner agent**
  
