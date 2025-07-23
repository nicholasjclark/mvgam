# Requirements: Summary Refactor

## Problem Statement
The current `summary.mvgam` function is a monolithic 1200+ line function that prints directly to console and doesn't create a persistent, structured object that users can save and reload for later analysis.

## Minimal Solution
Create a structured, classed `mvgam_summary` object with organized components and its own print method, using modular functions that can be reused elsewhere in the package.

## Users & Use Cases
- Primary User: Any mvgam package user needs to save summary objects and extract key information later
- Use Case: User fits a model, creates a summary, saves their workspace, and later reloads to print the summary or extract specific elements like coefficient estimates, formula calls, or Stan diagnostics

## Success Criteria
- [ ] `summary.mvgam()` returns a structured `mvgam_summary` object instead of just printing
- [ ] Object can be saved/loaded and maintains all essential information
- [ ] `print.mvgam_summary()` method produces output identical to current function
- [ ] Modular component functions can be reused in `print.mvgam()` and other functions
- [ ] Object structure follows logical groupings: model specification, parameter estimates, diagnostics, sampling info

## Non-Goals
- Changing the printed output format or content
- Adding new summary statistics or diagnostics
- Modifying the function signature or user-facing API

## Status
- Created: 2025-01-23
- Status: Draft
- Next Step: Design specification