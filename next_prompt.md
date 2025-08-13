## Context: mvgam Stan Compilation Pipeline Refactoring

You are an AI assistant helping me to refactor an existing R package called mvgam. This package will build custom Bayesian state-space models that combine sub-models: a latent state model and an observation model. Both of these sub-models will leverage the brms R package to build initial Stan code, data objects and to accept custom prior specifications, and my package will then integrate the two sub-models into a single Stan file that may also incorporate custom dynamic trends. My package will then pass the full Stan file and data objects to rstan or cmdstanr for fitting the model in Stan. 

## Current Situation

We have just completed building our stanvars class structure to allow custom trend generators to create valid brms stanvars collections. This will help us to inject appropriate Stan code snippets into the brms-generated observation Stan model, as well as any necessary data that needs to be added to the brms-generated Stan data objects.

## Your Task

Please follow these steps sequentially:

1. **Read and analyze these files in order:**
   - `active/quick-reference.md`
   - `active/architecture-decisions.md`
   - `active/current-sprint.md`

3. **Gather refactoring function context**:
   - Use the architecture-analyzer agent to provide a systematic overview of the following R files: `R/stan_assembly.R`, `R/mvgam_core.R`, `R/brms_integration.R`, `R/trend_system.R`, `R/validations.R`

4. **Use thinking/reasoning to analyze:**
   - The formula interface and how trend constructors are called and validated
   - The flow of data and formulae to stanvar objects and then through the combination pipeline
   - The merging of trend and observation data and model objects

5. **Create a five-step implementation plan to address any immediate action points highlighted in `active/current-sprint.md`**. Use natural language to describe the plan, not code examples.

If any of these tasks are ambiguous or unclear to you, please ask me targeted questions so that we can come up with a robust plan together.
