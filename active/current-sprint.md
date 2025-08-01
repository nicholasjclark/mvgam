# Current Sprint: Registry-Based Stan Assembly (Weeks 5-6)

**Status**: Active Development  
**Goal**: Centralized trend registration system with Stan code generation  
**Foundation**: Weeks 1-4 complete with proven architecture and 350+ test cases

## Immediate Objectives

### Current Architecture Status ✅
**Foundation Complete**: All Phase 1 deliverables validated
- Single-fit dual-object system implemented and tested
- Multiple imputation with Rubin's rules pooling operational  
- brms integration via `backend = "mock"` confirmed (10-50x speedup)
- Formula validation system complete with autocorrelation separation
- Trend dispatcher with custom registration (`register_trend_type()`) functional

### Week 5 Deliverables
- [ ] Validate two-stage Stan assembly with hierarchical stancode generators
- [ ] Streamline `custom_trend()` vs `register_trend_type()` - resolve redundancy
- [ ] Ensure Stan data/code without mvgam additions matches brms versions exactly
- [ ] Validate parameter renaming across observation models (multivariate, distributional)
- [ ] Confirm mvgam-generated Stan code passes `rstan::stanc()` compilation

### Week 6 Deliverables  
- [ ] Integration testing with enhanced trend architecture
- [ ] Real Stan code compilation validation for all trend types
- [ ] End-to-end model fitting with grouping and correlation features
- [ ] Performance validation (<1ms registry lookup confirmed)
- [ ] Factor model compatibility testing with n_lv < n_series scenarios

## Critical Implementation Patterns

### Existing Architecture (Weeks 1-4 ✅)
```r
# Validated components ready for Stan integration
mvgam_enhanced()              # Single-fit dual-object system
parse_multivariate_trends()   # Response-specific trend mapping  
setup_brms_lightweight()      # Mock backend optimization
validate_autocor_separation() # Context-aware autocorr validation
register_trend_type()         # Custom trend registration
```

### Two-Stage Stan Assembly (Week 5 Focus)
```r
# Stage 1: Generate base Stan model with trend stanvars
base_stancode <- brms::stancode(obs_formula, data, stanvars = trend_stanvars)
base_standata <- brms::standata(obs_formula, data, stanvars = trend_stanvars)

# Stage 2: Inject trend effects into linear predictors  
final_stancode <- inject_trend_into_linear_predictor(base_stancode, trend_spec)
# standata already complete via stanvars - no modification needed
```

### Existing Registry Architecture
```r
# Already implemented and tested
register_trend_type <- function(name, stancode_fun, forecast_fun = NULL, 
                               stanvars_fun = NULL, validate_fun = NULL, ...) {
  # Validation and registration logic already functional
}

# Current challenge: Streamline with custom_trend() to avoid redundancy
```

### Factor Model Compatibility (Already Validated)
```r
# Implemented constraint validation
FACTOR_COMPATIBLE_TRENDS <- c("AR", "RW", "VAR", "ZMVN")
FACTOR_INCOMPATIBLE_TRENDS <- c("PW", "CAR")  

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

### Registry Redundancy Resolution (Week 5 Priority)
- **Issue**: Both `custom_trend()` and `register_trend_type()` exist with overlapping functionality
- **Decision Needed**: Consolidate into single registration system
- **Impact**: Affects user extension documentation and API consistency

### Stan Code Validation Strategy (Week 5)
- **Approach**: Two-stage validation - syntax first, then compilation
- **Implementation**: `rstan::stanc()` validation for all generated code
- **Performance**: Validate compilation time doesn't exceed setup gains

### Parameter Renaming Scope (Week 5-6)
- **Challenge**: Ensure parameter renaming works across all brms model types
- **Test Cases**: Multivariate, distributional, hurdle, zero-inflated models
- **Validation**: Generated parameters match brms conventions exactly

## Success Criteria This Sprint

### Week 5 Success Metrics
- [ ] Stan assembly system produces compilable code for AR, RW trends
- [ ] Generated Stan code matches brms output when no mvgam additions present
- [ ] Registry consolidation eliminates API redundancy
- [ ] Parameter renaming validated across 3+ brms model types
- [ ] All generated Stan code passes `rstan::stanc()` without errors

### Week 6 Success Metrics
- [ ] Integration tests pass for multivariate models with response-specific trends
- [ ] Factor model compatibility confirmed with live Stan compilation
- [ ] End-to-end fitting works with grouping and correlation features
- [ ] Performance benchmarks confirm registry overhead <1ms
- [ ] Stan code generation handles edge cases (missing data, irregular timing)

### Foundation Validation (Already Complete ✅)
- [x] Dual-object system functional with brms ecosystem methods
- [x] Multiple imputation with Rubin's rules pooling operational
- [x] Formula validation prevents autocorrelation conflicts
- [x] Trend dispatcher handles custom registration
- [x] Mock backend provides 10-50x setup speedup

## Next Sprint Preview (Week 7-8)
- Enhanced validation framework with registry integration
- User extension system documentation
- Factor model implementation and testing
