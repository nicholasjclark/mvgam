# Technical Requirements Document: GLM and Mu Translation System Refactoring

## Executive Summary

This TRD defines the requirements for refactoring mvgam's GLM and mu translation system to eliminate bugs, reduce code duplication, and improve maintainability while preserving GLM optimization performance benefits.

## Problem Statement

### Current Issues
1. **Infinite recursion bug** in `insert_after_mu_lines_in_model_block()` causing system crashes
2. **Extreme code duplication** with 5+ identical GLM detection calls per Stan generation  
3. **State tracking failures** due to line index invalidation during code modifications
4. **Complex debugging** due to non-linear processing flow with recursive function calls
5. **Maintenance burden** from scattered GLM transformation logic across multiple files

### Root Cause Analysis
The current system attempts to modify brms-generated GLM code post-hoc instead of working with brms's GLM decisions. This creates temporal dependency problems where trends must be injected after GLM optimization has already occurred.

## Functional Requirements

### FR-1: Unified GLM Analysis System
- **Requirement**: Replace 5+ redundant GLM detection calls with a single comprehensive analysis
- **Acceptance Criteria**: 
  - Only one GLM detection call per Stan code generation
  - Analysis captures all GLM patterns and mu construction types
  - Results cached and reused throughout processing pipeline

### FR-2: Linear Transformation Pipeline  
- **Requirement**: Eliminate recursive processing in favor of linear pipeline
- **Acceptance Criteria**:
  - Zero recursive function calls in GLM transformation logic
  - Clear analysis → transformation → injection → assembly flow
  - Deterministic processing order with no state dependencies

### FR-3: DRY GLM Pattern Handling
- **Requirement**: Consolidate duplicate GLM transformation code into reusable functions
- **Acceptance Criteria**:
  - Single implementation for each GLM pattern type
  - Shared utility functions for common operations
  - Configuration-driven pattern matching instead of hardcoded logic

### FR-4: GLM Optimization Preservation
- **Requirement**: Preserve brms's GLM optimization when it provides performance benefits
- **Acceptance Criteria**:
  - GLM functions retained for responses without trends
  - Standard parameterization only when trends require it
  - Minimal performance impact on non-trend models

### FR-5: Maintainable Architecture
- **Requirement**: Create debuggable, testable code structure
- **Acceptance Criteria**:
  - Clear separation of concerns between analysis, transformation, and injection
  - Each component independently testable
  - Comprehensive logging and error reporting

## Technical Requirements

### TR-1: GLM Analysis Engine
- **Component**: Unified GLM detection and classification system
- **Specifications**:
  - Single entry point: `analyze_glm_usage(stancode, trend_info)`
  - Return structured analysis with GLM patterns, mu construction types, response mappings
  - Support all 5 mu construction patterns identified by agents

### TR-2: Linear Transformation Engine
- **Component**: Non-recursive GLM-to-standard conversion system
- **Specifications**:
  - Pattern-specific transformation functions
  - Leverage `mu_expression_analysis.R` for structural analysis  
  - Deterministic transformation rules based on pattern classification

### TR-3: Trend Injection System
- **Component**: Unified trend injection that works with all mu patterns
- **Specifications**:
  - Pattern-agnostic trend injection API
  - Support for response-specific trend injection in multivariate models
  - Preserve parameter naming and Stan code structure

### TR-4: Pipeline Controller
- **Component**: Orchestration layer managing the linear processing flow
- **Specifications**:
  - Clear phase management: analyze → transform → inject → assemble
  - State tracking through structured data objects (not line indices)
  - Comprehensive error handling and rollback capabilities

## Compatibility Requirements

### CR-1: Model Compatibility
- **Requirement**: All models in `tasks/fit_and_save_models.R` must continue working
- **Test Coverage**: All 13 model types (GAM, multivariate, trend combinations)
- **Validation**: Existing integration tests must pass without modification

### CR-2: Test Suite Compatibility  
- **Requirement**: All stancode and standata tests must pass
- **Coverage**: Both unit tests and integration tests
- **Validation**: No test modifications required (tests validate functionality, not implementation)

### CR-3: Stan Code Functional Equivalence
- **Requirement**: Generated Stan code must be functionally equivalent (not necessarily identical)
- **Acceptance**: Same model fitting results and posterior distributions
- **Performance**: GLM optimization preserved where applicable

## Implementation Approach

### Phase 1: Analysis System (Linear Pipeline Foundation)
1. **Unified Analysis Implementation**
   - Create single GLM analysis function replacing multiple detection calls
   - Implement structural analysis using existing `mu_expression_analysis.R`
   - Build comprehensive GLM pattern classification

### Phase 2: Transformation Engine  
1. **Linear Transformation System**
   - Implement pattern-specific transformation functions
   - Create non-recursive GLM conversion logic
   - Build deterministic transformation pipeline

### Phase 3: Injection and Assembly
1. **Trend Injection System**
   - Create pattern-agnostic trend injection API
   - Implement response-specific handling for multivariate models
   - Build unified mu construction pipeline

### Phase 4: Pipeline Integration
1. **Linear Controller Implementation**
   - Create orchestration layer with clear phase management
   - Implement structured state tracking
   - Add comprehensive error handling and logging

### Phase 5: Testing and Validation
1. **Comprehensive Testing**
   - Unit tests for each component
   - Integration tests with all model types
   - Performance regression testing
   - Edge case validation

## Success Metrics

### Performance Metrics
- **GLM Detection Efficiency**: Reduce from 5+ calls to 1 call per generation (80%+ reduction)
- **Processing Time**: Maintain or improve Stan code generation performance
- **Memory Usage**: No significant increase in memory footprint

### Quality Metrics  
- **Bug Elimination**: Zero recursive processing bugs
- **Code Duplication**: <10% code duplication in GLM transformation logic
- **Test Coverage**: >95% code coverage for new components

### Maintainability Metrics
- **Debugging Time**: <50% reduction in time to diagnose GLM-related issues
- **Code Complexity**: Linear processing flow with clear separation of concerns
- **Documentation**: Comprehensive function documentation and architectural diagrams

## Risk Assessment

### High Risk
- **Stan Code Changes**: Functional equivalence validation across all model types
- **Performance Regression**: Ensuring GLM optimization preservation

### Medium Risk  
- **Integration Complexity**: Coordinating changes across multiple interdependent files
- **Test Maintenance**: Ensuring comprehensive test coverage during refactoring

### Low Risk
- **API Changes**: Internal functions not subject to backward compatibility constraints

## Dependencies

### Internal Dependencies
- `mu_expression_analysis.R` - Structural analysis system
- `tasks/fit_and_save_models.R` - Model compatibility validation
- `tests/testthat/` - Test suite for validation

### External Dependencies
- **brms**: GLM optimization and Stan code generation
- **Stan**: Compiled model execution
- **testthat**: Unit testing framework

## Acceptance Criteria

### Must Have
- [ ] Zero infinite recursion bugs in GLM processing
- [ ] Single GLM analysis call per Stan generation  
- [ ] All models in `fit_and_save_models.R` continue working
- [ ] All existing tests pass without modification
- [ ] GLM optimization preserved for applicable models

### Should Have  
- [ ] >80% reduction in GLM-related code duplication
- [ ] Clear linear processing pipeline with no recursive calls
- [ ] Comprehensive unit test coverage for new components
- [ ] Improved debugging capabilities with structured logging

### Nice to Have
- [ ] Performance improvements in Stan code generation
- [ ] Enhanced error reporting with specific GLM pattern information
- [ ] Architectural documentation and flow diagrams

## Timeline Estimate

- **Phase 1 (Analysis System)**: 1-2 weeks
- **Phase 2 (Transformation Engine)**: 2-3 weeks  
- **Phase 3 (Injection System)**: 1-2 weeks
- **Phase 4 (Pipeline Integration)**: 1-2 weeks
- **Phase 5 (Testing & Validation)**: 1-2 weeks

**Total Estimated Duration**: 6-11 weeks depending on complexity of edge cases discovered during implementation.

## Conclusion

This refactoring addresses critical bugs and maintainability issues while preserving performance benefits. The linear pipeline approach provides a clear path to eliminate recursion bugs and code duplication while maintaining compatibility with existing models and tests.