# Current Sprint: Registry-Based Stan Assembly (Weeks 5-6)

**Status**: Active Development  
**Goal**: Centralized trend registration system with Stan code generation

## Immediate Objectives

### Week 5 Deliverables
- [ ] `register_custom_trend()` public API function
- [ ] Registry dispatch system for trend types
- [ ] Factor model compatibility detection
- [ ] Basic stanvars generation for AR, RW trends

### Week 6 Deliverables  
- [ ] VAR, ZMVN trend support in registry
- [ ] Non-centered parameterization validation
- [ ] Integration testing with multivariate models
- [ ] Performance validation (<1ms registry lookup)

## Critical Implementation Patterns

### Registry Architecture
```r
# Core registry structure
mvgam_trend_registry <- new.env(parent = emptyenv())

register_custom_trend <- function(name, generator_fn, validator_fn, 
                                  factor_compatible = TRUE) {
  entry <- list(
    generator = generator_fn,
    validator = validator_fn, 
    factor_compatible = factor_compatible,
    registered_date = Sys.time()
  )
  mvgam_trend_registry[[name]] <- entry
}

# Dispatch pattern
generate_trend_stanvars <- function(trend_spec, data_info) {
  trend_type <- extract_trend_type(trend_spec)
  
  if (!exists(trend_type, envir = mvgam_trend_registry)) {
    stop("Unknown trend type: ", trend_type)
  }
  
  generator <- mvgam_trend_registry[[trend_type]]$generator
  generator(trend_spec, data_info)
}
```

### Factor Model Compatibility Matrix
```r
# Built-in compatibility rules
FACTOR_COMPATIBLE_TRENDS <- c("AR", "RW", "VAR", "ZMVN")
FACTOR_INCOMPATIBLE_TRENDS <- c("PW", "CAR")  # Series-specific logic

validate_factor_compatibility <- function(trend_type, n_lv, n_series) {
  if (n_lv < n_series && !trend_type %in% FACTOR_COMPATIBLE_TRENDS) {
    stop(sprintf(
      "Trend type '%s' incompatible with factor models (n_lv < n_series)",
      trend_type
    ))
  }
}
```

### Stan Code Generation Pattern
```r
# Two-stage assembly following brms patterns
generate_trend_stanvars_complete <- function(trend_obj, data_info) {
  # Stage 1: Generate data stanvars
  data_stanvars <- generate_trend_data_stanvars(trend_obj, data_info)
  
  # Stage 2: Generate code stanvars  
  code_stanvars <- generate_trend_code_stanvars(trend_obj, data_info)
  
  # Stage 3: Validate no conflicts with brms
  validate_stanvars_compatibility(c(data_stanvars, code_stanvars))
  
  return(c(data_stanvars, code_stanvars))
}
```

## Decision Points This Sprint

### Registry Performance Target
- **Target**: <1ms lookup overhead
- **Test**: Benchmark registry dispatch vs direct function calls
- **Fallback**: Direct dispatch if registry overhead >1ms

### Factor Model Validation Strategy
- **Approach**: Pre-validate during trend specification parsing
- **Implementation**: Add compatibility metadata to registry entries
- **Error Timing**: Fail fast during model setup, not Stan compilation

### Stanvars Naming Convention
- **Pattern**: `mvgam_<trend_type>_<component>`
- **Examples**: `mvgam_ar_data`, `mvgam_rw_params`, `mvgam_var_model`
- **Validation**: Check no conflicts with brms reserved names

## Success Criteria This Sprint

### Functional Requirements
- [ ] Registry system handles AR, RW with correct Stan code generation
- [ ] Factor compatibility validation prevents invalid combinations
- [ ] Generated stanvars integrate cleanly with brms standata system
- [ ] Non-centered parameterization maintained across all trends

### Performance Requirements
- [ ] Registry lookup adds <1ms overhead to model setup
- [ ] Stan code generation comparable to current direct approach
- [ ] Memory usage stable with registry size

### Integration Tests
- [ ] Basic multivariate model with response-specific trends
- [ ] Factor model with compatible trend types
- [ ] Validation errors for incompatible combinations
- [ ] Stan code compiles without syntax errors

## Next Sprint Preview (Week 7-8)
- Enhanced validation framework with registry integration
- User extension system documentation
- Factor model implementation and testing
