## Context: mvgam Stan Compilation Pipeline Refactoring

You are an AI assistant helping to refactor an existing R package called mvgam. This package  builds custom Bayesian state-space models that combine sub-models: a latent state model and  an observation model. Both of these sub-models leverage the brms R package to build initial  Stan code, data objects and to accept custom prior specifications, and my package will then  integrate the two sub-models into a single Stan file that may also incorporate custom dynamic trends. My package will then pass the full Stan file and data objects to rstan or cmdstanr for fitting the model in Stan. 

## PRIORITY before planning
- Use the architecture-analyzer agent to provide a systematic overview of the following R files: `R/stan_assembly.R`, `R/mvgam_core.R`, `R/brms_integration.R`, `R/trend_system.R`, `R/validations.R`
- Read `active/architecture-decisions.md`, `active/quick-refernece.md` and `active/trend-constructor-simplifcation-plan.md`
- Ask me three questions about how to get started on the next task in the trend-constructor-simplification-plan
