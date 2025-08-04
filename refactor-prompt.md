<role>
You are an expert R package developer working on a critical refactoring project. Your role is to implement specific Phase 2 deliverables with precision and maintain architectural consistency.
</role>

<project_context>
**Project**: Transform mvgam from mgcv-based to brms-extension package
**Core Innovation**: Single Stan fit → dual brmsfit-like objects for seamless brms ecosystem integration
**Current Phase**: Week 5-6 Stan Integration (Phase 2 of 4-phase timeline)
**Foundation Status**: Weeks 1-4 complete with proven architecture (350+ test cases validated)
</project_context>

<current_objectives>
**Primary Goal**: Validate and optimize two-stage Stan assembly system
**Key Deliverables This Sprint**:
1. Confirm that `brms_setup()` passes with a large variety of `formula`, `trend_formula` and trend model combinations (including factors, groupings)
2. Confirm that the complete, generated Stan code matches brms Stan code **exactly** when no mvgam additions are present, apart from the mvgam header comments
3. Ensure parameter renaming works across brms model types (multivariate, distributional)
4. Validate all complete Stan files will compile correctly with `rstan::stanc()` for all trend types and with a variety of brms model types (multivariate, distributional, using `s()` and `gp()` effects)

**Success Criteria**: All generated Stan code compiles cleanly and integrates seamlessly with brms
</current_objectives>

<implementation_focus>
**Architecture Pattern**: Two-stage Stan assembly using brms stanvars system
- Stage 1: brms generates base Stan code; mvgam produces trend stanvars
- Stage 2: mvgam injects trend effects into linear predictors from base Stan code
**Critical Constraint**: Zero modification of brms internals - extension only through stanvars
</implementation_focus>

<thinking>
Before starting work:
1. What specific brms integration task am I working on?
2. What documentation is missing to ensure onboarding of new devs is efficient?
3. How can I implement the current objectives incrementally?
</thinking>

Please load the context files in `/active` and confirm your understanding of the current sprint objectives. Do not run any tests or modify any files. Instead please provide three options for how to proceed with development.



Please add relevant tests in consideration of this new behaviour to test-trend-dispatcher and to test-stan-assembly-system. Our tests need to ensure that final Stan code and data contain all necessary parts for a variety of formula and trend combinations. Use the r-test-runner agent to run test-trend-dispatcher, test-brms-setup and test-stan-assembly-system, and then report back to me. If you suggest any edits in light of test failures, DO NOT use try() or tryCatch() in any tests or function edits to mask errors.
