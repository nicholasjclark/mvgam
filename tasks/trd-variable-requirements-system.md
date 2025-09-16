# Task Requirements Document: Variable Requirements System

## 1. Problem Statement

**Current Issue:** The mvgam package has significant architectural inefficiency in formula processing and variable validation:

### Performance Problems
- **Formula parsing happens 5+ times** across the validation pipeline for the same formulas
- **`all.vars()` called repeatedly** instead of extracting variables once
- **Validation functions re-analyze** formulas that were already processed
- **Complex formula parsing scattered** across multiple files without coordination

### Code Quality Issues  
- **Duplicated variable extraction logic** in multiple validation functions
- **Inconsistent nonlinear parameter handling** across different validation points
- **Formula objects passed around** when only variable names are actually needed
- **Validation functions tightly coupled** to formula parsing instead of using structured data

### User Experience Problems
- **Poor error messages** due to context loss during multiple parsing steps
- **Inconsistent validation behavior** across different parts of the pipeline
- **Difficult debugging** when formula processing fails at different pipeline stages

## 2. Solution Overview

**Variable Requirements System:** A centralized system that extracts all variable requirements once at pipeline start and provides structured access throughout the validation pipeline.

### Core Components
- **`mvgam_variable_requirements` S3 class** - Structured storage of all variable requirements
- **`extract_formula_variables()` function** - Single comprehensive formula analysis
- **Enhanced validation pipeline** - Uses pre-computed requirements instead of re-parsing formulas
- **Simplified validation functions** - Focus on data checking rather than formula parsing

## 3. Benefits

### Performance Improvements
- **80% reduction in formula parsing overhead** - parse once, use everywhere
- **Faster validation pipeline** - no redundant formula analysis
- **Reduced memory usage** - structured requirements smaller than repeated formula objects

### Code Quality Improvements  
- **DRY compliance** - single source of truth for variable requirements
- **Cleaner architecture** - separation of formula analysis from validation logic
- **Better maintainability** - centralized variable logic easier to modify and debug
- **Reduced complexity** - validation functions become simpler and more focused

### User Experience Improvements
- **Better error messages** - comprehensive variable context available for all validation
- **Consistent validation behavior** - single processing pipeline ensures uniform handling
- **Clearer debugging** - centralized variable analysis makes issues easier to trace

## 4. Technical Approach

### Architecture Pattern
**Early Extraction → Structured Storage → Consistent Usage**

```
Formula Input → extract_formula_variables() → mvgam_variable_requirements → Validation Pipeline
```

### Integration Strategy
- **Replace existing validation** - no backward compatibility needed for cleaner design
- **Maintain error quality** - preserve or improve existing error message patterns
- **Follow mvgam patterns** - use existing S3, checkmate, and insight conventions

### Implementation Scope
- **Core S3 class system** for structured variable storage
- **Comprehensive formula analysis** supporting all mvgam formula types  
- **Complete validation pipeline replacement** using structured requirements
- **Full test coverage** ensuring no regressions

## 5. Success Criteria

### Measurable Improvements
1. **Performance**: Demonstrable reduction in formula parsing calls (target: 80% reduction)
2. **Code Quality**: Reduced code duplication and improved maintainability metrics
3. **User Experience**: Improved error message quality and validation consistency

### Technical Validation
1. **All existing tests pass** - no regressions in functionality
2. **New comprehensive test suite** - covers all edge cases and integration scenarios
3. **Code review approval** - meets all CLAUDE.md standards and R package best practices

## 6. User Impact

### Developer Benefits
- **Easier maintenance** - centralized variable logic
- **Clearer debugging** - structured requirements object shows exactly what variables are needed
- **Better testing** - isolated variable extraction can be unit tested independently

### End User Benefits  
- **Faster model setup** - reduced validation overhead
- **Better error messages** - clearer information about missing or invalid variables
- **More reliable validation** - consistent behavior across all formula types

### Ecosystem Benefits
- **Foundation for future features** - structured variable requirements enable new validation capabilities
- **Better brms integration** - cleaner interface with brms formula processing
- **Extensibility** - easier to add new formula types or validation rules

## 7. Risk Mitigation

### Implementation Risks
- **Breaking changes accepted** - no backward compatibility constraints simplify implementation
- **Comprehensive testing** - extensive test suite prevents regressions
- **Iterative code review** - review after each sub-task ensures quality

### Technical Risks
- **Formula complexity** - comprehensive analysis ensures all formula types covered
- **Integration complexity** - focused integration point reduces coupling issues
- **Performance verification** - benchmarking ensures expected improvements achieved

## 8. Timeline

**Estimated Implementation:** 12 sub-tasks × 15 minutes = 3 hours focused development time

**Approach:** Each sub-task includes implementation + code review for quality assurance

**Validation:** Comprehensive testing and performance measurement throughout implementation

---

**Rationale:** This system addresses fundamental architectural inefficiencies that impact both developer productivity and user experience. The centralized variable requirements approach follows DRY principles while maintaining the robust validation that prevents Stan compilation errors.