## Context: mvgam Stan Compilation Pipeline Refactoring

You are an AI assistant helping me to refactor an existing R package called mvgam. This package will build custom Bayesian state-space models that combine sub-models: a latent state model and an observation model. Both of these sub-models will leverage the brms R package to build initial Stan code, data objects and to accept custom prior specifications, and my package will then integrate the two sub-models into a single Stan file that may also incorporate custom dynamic trends. My package will then pass the full Stan file and data objects to rstan or cmdstanr for fitting the model in Stan. 

## Current Situation

We have just completed fixing a critical stanvars class structure issue where our custom trend generators were creating corrupted list objects instead of proper brms stanvars collections. This will help us to inject appropriate Stan code snippets into the brms-generated observation Stan model, as well as any necessary data that needs to be added to the brms-generated Stan data objects. Now we are moving into testing of the full Stan model and Stan code constructions, as well as testing if models compile properly.

## Your Task

Please follow these steps sequentially:

1. **Read and analyze these files in order:**
   - `active/current-sprint.md` - Contains the critical issues section with test results
   - `active/architecture-decisions.md` - Contains design principles and naming conventions
   - `R/stan_assembly.R` - The main Stan code assembly pipeline
   - `R/mvgam_core.R` - The main user-facing function for defining models

2. **Use thinking/reasoning to analyze:**
   - The formula interface and how trend constructors are called and validated
   - The flow of stanvar objects through the combination pipeline
   - The merging of trend and observation data and model objects

3. **Create a five-step implementation plan to address any immediate action points highlighted in `active/current-sprint.md`**. Use natural language to describe the plan, not code examples.

If any of these tasks are ambiguous or unclear to you, please ask me targeted questions so that we can come up with a robust plan together.


What else do you need to know before you can implement this?
