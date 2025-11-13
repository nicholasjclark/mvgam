# Dev Tasks: Unified GLM Pipeline Implementation

## Overview

This document tracks the implementation of a unified linear pipeline to replace the current recursive GLM processing system in mvgam Stan code generation. The current system suffers from infinite recursion, redundant processing (5+ GLM detection calls), and complex state tracking that leads to maintenance issues.

## Problem Statement

The current Stan assembly system has these critical issues:
- **Infinite recursion** in `insert_after_mu_lines_in_model_block()`
- **Redundant processing** with 5+ calls to `detect_glm_usage()` on the same code
- **Complex state tracking** using `processed_glm_lines` that fails in practice
- **Multiple conflicting phases** (pre-processing, main processing, post-processing)
- **Dual architectures** for univariate vs multivariate processing

## Solution Architecture

Replace the recursive system with a **linear pipeline** that processes Stan code once through clear stages:
1. **Analysis**: Single comprehensive GLM detection and pattern classification
2. **Transformation**: Single-pass GLM-to-standard conversion where needed
3. **Injection**: Pattern-agnostic trend injection into prepared code
4. **Assembly**: Final code assembly without additional transformations

## Task Breakdown

### Task 1: Single GLM Detection and Analysis ⏳
**Status**: Not Started  
**Priority**: High - Foundation for all other tasks  
**Estimated Effort**: Medium

**Objective**: Replace multiple redundant GLM detection calls with comprehensive analysis

**Requirements**:
- Create single analysis function that examines Stan code once
- Classify all mu construction patterns: GLM, explicit, nonlinear, multivariate, hybrid
- Generate complete processing plan indicating what transformations are needed
- Eliminate the 5+ redundant `detect_glm_usage()` calls across the codebase

**Success Criteria**:
- GLM detection happens exactly once per Stan assembly operation
- All 5 mu construction patterns are correctly identified
- Processing plan correctly indicates required transformations for each pattern
- No redundant pattern analysis throughout the codebase

**Dependencies**: None  
**Blocks**: Tasks 2, 3, 4

### Task 2: Linear GLM-to-Standard Converter ⏳
**Status**: Not Started  
**Priority**: High - Eliminates core recursion  
**Estimated Effort**: Large

**Objective**: Replace recursive GLM conversion with single-pass transformation

**Requirements**:
- Create non-recursive function for GLM-to-standard form conversion
- Handle all supported GLM types (poisson_log_glm, normal_id_glm, neg_binomial_2_log_glm, etc.)
- Generate proper mu construction lines (`mu += Xc * b;`, etc.) in single pass
- Preserve GLM optimization for responses without trend injection
- Extract GLM parameters correctly for all family types

**Success Criteria**:
- All GLM patterns converted to standard form without recursion
- Generated mu construction follows proper Stan syntax
- GLM parameter extraction works for all supported family types
- No infinite loops or recursive function calls
- Original GLM optimization preserved where trends not injected

**Dependencies**: Task 1  
**Blocks**: Tasks 3, 4

### Task 3: Unified Trend Injection Engine ⏳
**Status**: Not Started  
**Priority**: High - Core functionality  
**Estimated Effort**: Large

**Objective**: Create pattern-agnostic trend injection system

**Requirements**:
- Single injection function handling all 5 mu construction patterns uniformly
- Correct injection points for explicit mu lines (`mu += trend[...]`)
- Proper handling of nonlinear loop structures with expression wrapping
- Multivariate response support with response-specific injection
- Maintain correct Stan execution order and syntax

**Success Criteria**:
- Trend injection works correctly for all 5 mu construction patterns
- Generated trend code follows proper Stan syntax
- Injection points are correctly identified and utilized
- No conflicts between different pattern types in hybrid models
- Multivariate models handle response-specific trend injection

**Dependencies**: Tasks 1, 2  
**Blocks**: Task 4

### Task 4: Linear Pipeline Controller ⏳
**Status**: Not Started  
**Priority**: High - System integration  
**Estimated Effort**: Medium

**Objective**: Replace `inject_trend_into_linear_predictor()` with linear orchestrator

**Requirements**:
- Create new entry point coordinating analysis → transformation → injection flow
- Eliminate dual-phase processing (conversion + post-processing)
- Replace complex `processed_glm_lines` tracking with simple status flags
- Unified logic working for both univariate and multivariate cases
- Integration with existing mvgam Stan assembly infrastructure

**Success Criteria**:
- Linear pipeline processes Stan code exactly once
- No dual-phase or redundant processing stages
- State management through simple flags instead of complex tracking
- Unified entry point works for all model types
- Integration maintains compatibility with existing mvgam functionality

**Dependencies**: Tasks 1, 2, 3  
**Blocks**: Task 5

### Task 5: Remove Recursive Infrastructure ⏳
**Status**: Not Started  
**Priority**: Medium - Cleanup  
**Estimated Effort**: Medium

**Objective**: Eliminate recursive functions and complex state tracking

**Requirements**:
- Replace or refactor `insert_after_mu_lines_in_model_block()` to be non-recursive
- Remove `processed_glm_lines` state tracking system throughout codebase
- Eliminate redundant post-processing phases (`transform_glm_calls_post_processing()`)
- Clean up function interfaces to remove recursion-related parameters
- Update documentation to reflect new linear architecture

**Success Criteria**:
- No recursive function calls in GLM processing pipeline
- Complex state tracking systems removed
- Redundant processing phases eliminated
- Function interfaces simplified and clarified
- Documentation updated to reflect new architecture

**Dependencies**: Task 4  
**Blocks**: Task 6

### Task 6: Integration and Testing ⏳
**Status**: Not Started  
**Priority**: High - Validation  
**Estimated Effort**: Large

**Objective**: Integrate pipeline with mvgam and validate functionality

**Requirements**:
- Update all Stan assembly entry points to use new linear pipeline
- Ensure backward compatibility with existing prediction and model fitting
- Comprehensive testing of all 5 mu construction patterns with trend injection
- Performance validation showing elimination of redundant processing
- Integration testing with brms stancode input and mvgam model fitting

**Success Criteria**:
- All existing mvgam functionality works with new pipeline
- No infinite recursion in any model fitting scenarios
- All 5 mu construction patterns work correctly with trend injection
- Performance improvements from eliminating redundant processing
- Comprehensive test coverage of new architecture

**Dependencies**: Task 5  
**Blocks**: None

## Current Issues Context

### Infinite Recursion Mechanism
The recursion occurs in this cycle:
1. `insert_after_mu_lines_in_model_block()` detects GLM calls
2. Calls `convert_glm_to_standard_form()` to convert them  
3. `convert_glm_to_standard_form()` partially converts GLM calls
4. `insert_after_mu_lines_in_model_block()` calls itself recursively
5. Recursion continues because GLM patterns are still detected

### Redundant GLM Detection Calls
Current code calls `detect_glm_usage()` in multiple locations:
- `generate_base_stancode_with_stanvars()`: Lines 640, 662
- `inject_trend_into_linear_predictor()`: Line 1147  
- `insert_after_mu_lines_in_model_block()`: Line 1391
- `inject_multivariate_trends_into_linear_predictors()`: Line 1751

### Complex State Tracking Issues
The `processed_glm_lines` parameter intended to prevent re-processing fails because:
- Line number tracking doesn't work across different text representations
- GLM conversion changes line structure but tracking doesn't account for this
- Multiple functions modify the same code without coordinated state management

## Success Metrics

### Performance Metrics
- **GLM Detection**: From 5+ calls to 1 call per Stan assembly operation
- **Processing Phases**: From 3 phases to 1 linear pipeline
- **Function Calls**: Eliminate recursive calls entirely

### Functionality Metrics
- **Pattern Support**: All 5 mu construction patterns working correctly
- **Model Types**: Univariate and multivariate models supported
- **Compatibility**: All existing mvgam functionality preserved

### Code Quality Metrics  
- **Complexity**: Simplified function interfaces and logic
- **Maintainability**: Clear separation of concerns and responsibilities
- **Debuggability**: Linear execution flow that can be easily traced

## Related Files

### Primary Implementation Files
- `R/stan_assembly.R` - Main GLM processing and trend injection logic
- `R/brms_integration.R` - Integration with brms stancode generation

### Testing Files
- `tasks/debug_intercept_only.R` - Current debugging script that triggers infinite recursion
- `tasks/minimal_debug.R` - Minimal test case for Stan code generation

### Documentation Files
- `tasks/dev-tasks-prediction-system.md` - Related prediction extraction system work
- `architecture/dependency-graph.md` - Overall package architecture

## Notes

This unified pipeline implementation addresses fundamental architectural issues rather than applying patches to the existing recursive system. The linear pipeline approach ensures:
- **Predictable execution flow** that can be easily debugged and maintained
- **Single responsibility functions** with clear separation of concerns  
- **Elimination of recursive complexity** that leads to infinite loops
- **Performance improvements** through elimination of redundant processing
- **Extensibility** for future mu construction patterns and trend types

The implementation should maintain full backward compatibility while providing a foundation for future enhancements to the Stan code generation system.