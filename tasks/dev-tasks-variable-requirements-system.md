# Variable Requirements System Implementation Task List

## Overview

Implement a robust Variable Requirements System for mvgam that consolidates formula parsing, reduces redundant validation, and provides a clean API for variable requirement management.

**NO BACKWARD COMPATIBILITY** - Complete replacement of existing system for cleaner architecture.

## Implementation Tasks

### T1.1: Create lightweight variable requirements constructor ✓ 15 min
- [X] **Create simple constructor function**: `create_variable_requirements()`
- [X] **Return lightweight named list** with fields:
  - `response_vars` (character)
  - `time_vars` (character) 
  - `series_vars` (character)
  - `grouping_vars` (character)
  - `capacity_vars` (character)
  - `all_vars` (character)
  - `context` (character)
  - `metadata` (list with has_nonlinear, multivariate)
- [X] **Focus on performance** - no S3 overhead for internal use
- [X] **Add checkmate validation** for inputs only
- [X] **Code Review**: Submit to code-reviewer agent after completion

### T2.1: Create extract_formula_variables core function ✓ 15 min
- [ ] Use parallel pathfinder and general purpose agents to understand variable extraction from formulae and nonlinear / multivariate formula requirements
- [ ] Read `architecture/stan-data-flow-pipeline.md` to understand where formula extraction should happen
- [ ] **Function**: `extract_formula_variables(formula, trend_formula = NULL, data = NULL)`
- [ ] **Consolidate 20+ terms() calls and 10+ all.vars() calls** throughout codebase
- [ ] **Handle formula types**:
  - Standard R formulas (`y ~ x`)
  - brms formulas (`bf()`, `mvbf()`)
  - Complex expressions with transformations, nonlinear and distributional formulas
- [ ] **Return**: Named list from `create_variable_requirements()`
- [ ] **Focus on eliminating parsing redundancy**
- [ ] **Code Review**: Submit to code-reviewer agent BEFORE making edits

### T2.2: Add nonlinear formula detection and filtering ✓ 15 min
- [ ] Use parallel pathfinder and general purpose agents to understand variable extraction from formulae and nonlinear / multivariate formula requirements
- [ ] Use the r-package-analyzer to understand nonlinear formulas in brms and their internal representations
- [ ] **Enhance extract_formula_variables()** for nonlinear formulas
- [ ] **Detect nonlinear patterns**:
  - `formula$pforms` (distributional parameters)
  - `formula$nlpars` (nonlinear parameters)  
  - `attr(formula, "nl")` attributes
- [ ] **Filter latent parameters** from validation requirements
- [ ] **Handle distributional parameters** separately
- [ ] **Code Review**: Submit to code-reviewer agent BEFORE making edits

### T2.3: Add multivariate formula support ✓ 15 min
- [ ] Use parallel pathfinder and general purpose agents to understand variable extraction from formulae and nonlinear / multivariate formula requirements
- [ ] Use the r-package-analyzer to understand multivariate formulas in brms and their internal representations
- [ ] **Handle multivariate constructs**:
  - `mvbind(y1, y2) ~ x` formulas
  - `bf()` and `mvbf()` objects
  - Named lists for response-specific trends
- [ ] **Extract response-specific requirements**
- [ ] **Support different trend formulas per response**
- [ ] **Integrate with existing multivariate detection logic**
- [ ] **Code Review**: Submit to code-reviewer agent BEFORE making edits

### T3.1: Replace validate_time_series_for_trends() ✓ 15 min
- [ ] Use parallel pathfinder and general purpose agents to understand variable extraction from formulae and nonlinear / multivariate formula requirements
- [ ] **File**: `R/validations.R`
- [ ] **Complete replacement** using new variable requirements system
- [ ] **Integration points**:
  - Early variable extraction using extract_formula_variables()
  - Single comprehensive validation call
  - Enhanced return structure with requirements
- [ ] **Remove redundant formula parsing**
- [ ] **Maintain existing error message quality**
- [ ] **Code Review**: Submit to code-reviewer agent BEFORE making edits

### T3.2: Replace validate_required_variables() ✓ 15 min  
- [ ] Use parallel pathfinder and general purpose agents to understand variable extraction from formulae,  nonlinear / multivariate formula requirements and validate_requried_variables()
- [ ] **Complete replacement** with validate_variable_requirements_exist()
- [ ] **Use pre-computed requirements** instead of formula parsing
- [ ] **Enhanced error messages** with comprehensive context
- [ ] **Better missing variable reporting**
- [ ] **Context-aware validation**
- [ ] **Code Review**: Submit to code-reviewer agent BEFORE making edits

### T3.3: Update validation pipeline integration ✓ 15 min
- [ ] Use parallel pathfinder and general purpose agents to understand variable extraction from formulae, nonlinear and multivariate formula parsing
- [ ] **Remove redundant formula parsing** throughout validation pipeline
- [ ] **Update all validation functions** to use pre-computed requirements
- [ ] **Simplify validation call chains** 
- [ ] **Maintain validation rule dispatch system**
- [ ] **Ensure consistent error handling**
- [ ] **Code Review**: Submit to code-reviewer agent BEFORE making edits

### T4.1: Add comprehensive unit tests ✓ 15 min
- [ ] **File**: `tests/testthat/test-variable-requirements.R` (new)
- [ ] **Test coverage**:
  - Constructor function behavior and validation
  - extract_formula_variables() functionality  
  - Nonlinear, distributional and multivariate formula handling
  - Error conditions and edge cases
- [ ] **Follow existing test patterns** from test-mvgam-formula.R
- [ ] **Code Review**: Submit to code-reviewer agent BEFORE making edits

### T4.2: Add integration tests ✓ 15 min
- [ ] **Test end-to-end pipeline** with new validation system
- [ ] **Test integration** with existing mvgam workflow
- [ ] **Test complex formula scenarios**
- [ ] **Error propagation testing**
- [ ] **Code Review**: Submit to code-reviewer agent BEFORE making edits

### T5.1: Add roxygen2 documentation ✓ 15 min
- [ ] **Document all new functions** following mvgam standards:
  - @title, @description, @param, @return, @examples
  - @author Nicholas J Clark
  - @export for user-facing, @noRd for internal
- [ ] **Cross-reference existing functions**
- [ ] **Update validation function documentation**
- [ ] **Code Review**: Submit to code-reviewer agent BEFORE making edits


## Relevant Files

### **Test Files (for patterns/integration)**
- `test-mvgam-formula.R` - Formula handling patterns
- `test-setup-brms.R` - brms integration, validation patterns  
- `test-stancode-standata.R` - Formula processing and data validation
- `setup.R` - Helper functions and testing utilities

### **Implementation Files**
- `R/validations.R` (modify) - Lightweight constructor and core functions, replace validation functions 
- `tests/testthat/test-variable-requirements.R` (new) - Comprehensive tests

## Success Criteria

1. **80% reduction** in formula parsing calls throughout codebase
2. **All tests pass** including new comprehensive test suite
3. **Cleaner error messages** with comprehensive variable context
4. **Simplified validation pipeline** with single source of truth
5. **Code review approval** for all sub-tasks and final implementation
6. **CLAUDE.md compliance** for all new code
7. **Performance improvement** measurable in complex formula scenarios

## Implementation Notes

- **Each sub-task ≤ 15 minutes** for focused implementation
- **Code review after each sub-task** for iterative quality control
- **No backward compatibility** constraints for cleaner design
- **Complete replacement** of existing validation patterns
- **Strong emphasis** on following mvgam architectural patterns
- **Comprehensive testing** at each implementation step

## Architecture Benefits

- **Single source of truth** for variable requirements
- **Eliminated redundancy** in formula parsing
- **Better error messages** with comprehensive context  
- **Cleaner code structure** with centralized variable logic
- **Enhanced maintainability** through consolidated validation
- **Performance improvements** through reduced overhead
