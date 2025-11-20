# Development Tasks: GLM and Mu Translation System Refactoring

## IMPORTANT: Development Guidelines

**MUST READ BEFORE STARTING:**
1. **Follow CLAUDE.md**: All development must strictly adhere to guidelines in `/CLAUDE.md`
2. **15-Minute Rule**: Each subtask must be completable in ≤15 minutes for junior R developers
3. **Load Functions**: ALWAYS run `devtools::load_all()` before testing or debugging
4. **Code Review**: Get code reviewer approval for ALL proposed changes before implementation
5. **One Task at a Time**: Work on only ONE subtask at a time - complete it fully before moving to next
6. **Validation Commands**: Run `devtools::test()` after changes, `devtools::check()` for full validation
7. **Documentation**: Include roxygen2 documentation with `@export` tags for all user-facing functions
8. **Testing Strategy**: Leverage existing extensive test suite rather than creating new test files
9. **Completion Criteria**: Each task includes "task complete when..." validation criteria
10. **Junior Developer Focus**: Assume basic R knowledge but provide explicit package development guidance

## Relevant Files

### R Package Files
- `R/glm_analysis.R` - New unified GLM analysis system
- `R/state_pipeline.R` - New immutable state management and linear pipeline
- `R/stan_assembly.R` - Modified existing Stan assembly system
- `R/mu_expression_analysis.R` - Enhanced integration with existing mu classification
- `R/validations.R` - Enhanced validation functions

### Test Files (Leverage Existing)
- `tests/testthat/test-stancode-standata.R` - Primary validation for stancode generation and GLM compatibility
- `tests/testthat/test-stan-assembly-system.R` - Integration testing for Stan assembly pipeline
- `tests/testthat/test-mu-expression-classification.R` - Mu pattern classification validation

### Context Files
- `tasks/fit_and_save_models.R` - All 13 model types for compatibility validation
- `R/mu_expression_analysis.R` - Existing mu classification system to integrate with
- `tests/testthat/test-stancode-standata.R` - Existing stancode tests for regression testing

### Package Structure Files
- `NAMESPACE` - Export declarations (auto-generated via roxygen2 `@export` tags)
- `DESCRIPTION` - No new dependencies needed

## Notes for Junior R Developers
- All functions must include roxygen2 documentation with `@param`, `@return`, `@examples`
- User-facing functions need `@export` tag, internal functions use `@noRd`
- Use `devtools::test()` to run all tests, `devtools::check()` for full package validation
- Each task should take 10-15 minutes maximum
- Include validation criteria: "Task complete when [specific condition met]"
- Follow existing naming patterns in the codebase
- Error messages should use `insight::format_error()` for consistency
- **BANNED WORDS**: Never use meaningless words like "comprehensive", "unified", "enhanced", "robust", "simplified", "complete", "advanced", "improved", "optimized", "efficient", "powerful", "flexible", or "intelligent" in any code, comments, or documentation. Describe WHAT code does and WHY, not how supposedly good it is

## Tasks

- [ ] **1.0 GLM Analysis Consolidation**
  - **Note**: Use pathfinder agent to locate all GLM detection calls and analyze current patterns before implementing new system
  - [x] 1.1 Create `R/glm_analysis.R` with `analyze_stan()` function that detects GLM usage, mu patterns, and creates structured analysis result object. Task complete when function accepts stan code string and returns S3 object with GLM patterns identified.
  - [x] 1.2 Implement `glm_result()` constructor using S3 pattern that stores GLM patterns, optimization decisions, and mu classification data. Task complete when constructor creates valid S3 object with required fields and validation.
  - [x] 1.3 Add `detect_glm_patterns()` function that consolidates the 6 current detection calls into single comprehensive scan
  - [x] 1.4 Create `preserve_glm_opts()` function that identifies when GLM optimization should be maintained vs. converted to standard form
  - [x] 1.5 Add validation using `checkmate::assert_*()` for all analysis inputs and outputs following package patterns. Task complete when all functions have input validation with informative error messages using `insight::format_error()`.

- [ ] **2.0 Immutable State Management Reform**
  - **Note**: Use pathfinder agent to trace current `processed_glm_lines` usage and recursive patterns in `insert_after_mu_lines_in_model_block()`
  - [x] 2.1 Create immutable state objects with `processing_state()` S3 constructor containing code, analysis, operations, and stage. Task complete when constructor creates state objects with validated fields and proper S3 class.
  - [x] 2.2 Implement `to_analysis()` state transition function that analyzes GLM patterns and returns new state object
  - [x] 2.3 Create `to_conversion()` transition that converts GLM to standard form when needed, using analysis from previous state
  - [x] 2.4 Add `to_injection()` transition that injects trend effects after mu construction
  - [x] 2.5 Implement `to_assembly()` final transition that assembles processed Stan code into final result

- [ ] **3.0 Linear Processing Pipeline Implementation**
  - **Note**: Use r-package-analyzer agent to examine how brms handles GLM optimization decisions and integration patterns
  - [x] 3.1 Create `transform_glm_code()` main pipeline function that chains state transitions linearly: analysis → conversion → injection → assembly
  - [x] 3.2 Integrate with existing `extract_mu_construction_with_classification()` by calling it in `to_injection()` transition
  - [x] 3.3 Implement `skip_glm_conversion()` logic that preserves GLM optimization when no trends require standard parameterization
  - [x] 3.4 Add `track_operations()` function that maintains immutable list of completed operations for debugging and validation
  - [x] 3.5 Create error handling using `insight::format_error()` with clear state information when pipeline transitions fail

- [ ] **4.0 DRY Code Consolidation**
  - **Note**: Use pathfinder agent to locate all duplicate GLM processing functions and identify consolidation opportunities
  - [ ] 4.1 Replace scattered `parse_glm_parameters()` calls with single `parse_glm_once()` function used in analysis phase
  - [ ] 4.2 Consolidate GLM transformation logic into `transform_glm()` function that works with structured analysis results
  - [ ] 4.3 Unify code generation using existing stanvars patterns with `build_stanvars()` helper function
  - [ ] 4.4 Remove duplicate functions: eliminate redundant GLM detection in `detect_glm_usage()` scattered across 6 locations
  - [ ] 4.5 Update existing functions to use new analysis system: modify `inject_trend_into_linear_predictor()` to accept analysis objects

- [ ] **5.0 Compatibility Testing and Validation**
  - **Note**: Use pathfinder agent to locate existing test patterns and integration points for extending current test coverage
  - [ ] 5.1 Add unit tests for `analyze_stan()`, `glm_result()`, and `detect_glm_patterns()` functions to existing `test-stan-assembly-system.R`
  - [ ] 5.2 Add state transition tests for all `to_*()` functions and `proc_state()` constructor to existing `test-stan-assembly-system.R`
  - [ ] 5.3 Enhance existing tests in `test-stancode-standata.R` to validate GLM pattern detection and trend combinations using new pipeline
  - [ ] 5.4 Run all 13 models from `tasks/fit_and_save_models.R` to validate functional equivalence - NO new test files needed
  - [ ] 5.5 Add performance validation to existing `test-stan-compilation-validation.R` to verify GLM optimization preservation

## Implementation Guidelines

### Phase 1: Foundation (Tasks 1.0-2.0)
- Start with GLM analysis consolidation to create structured foundation
- Build immutable state management to replace recursive pattern
- Focus on eliminating infinite recursion bugs immediately

### Phase 2: Integration (Tasks 3.0-4.0)  
- Implement linear pipeline using foundation from Phase 1
- Integrate with existing mu classification system
- Consolidate scattered GLM logic into unified system

### Phase 3: Validation (Task 5.0)
- Comprehensive testing of all components
- Validate functional equivalence with all existing models
- Performance regression testing for GLM optimization preservation

### Success Criteria
- **Recursion Elimination**: Zero recursive function calls in GLM processing
- **Analysis Efficiency**: Single GLM detection call per Stan code generation (from 6+ to 1)
- **GLM Preservation**: Performance maintained for models without trends
- **Model Compatibility**: All 13 models from `tasks/fit_and_save_models.R` continue working
- **Code Quality**: >90% reduction in GLM-related code duplication

### Function Naming Conventions
- Use short, lowercase names: `analyze_stan()`, `proc_state()`, `to_analysis()`
- Follow existing package patterns: S3 constructors, validation with `checkmate`
- Error handling with `insight::format_error()` for user-friendly messages
- Internal functions use `@noRd` documentation tag