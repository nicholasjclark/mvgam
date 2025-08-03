# Current Sprint: Registry-Based Stan Assembly (Weeks 5-6)

**Status**: Week 5 in preparation
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
- [ ] **Validate two-stage Stan assembly** - Complete system operational with full test coverage
- [ ] **Streamline registry functions** - No redundancy found; well-designed complementary functions
- [ ] **Stan code matches brms exactly** - Validated via `generate_base_brms_stancode()`
- [ ] **Parameter renaming validation** - Works across multivariate, distributional models  
- [ ] **Stan compilation validation** - Complete framework with `rstan::stanc()` integration

### Week 6 Deliverables (Updated Focus)
- [ ] **Integration testing** - Comprehensive tests covering models with full `formula` and `trend_formula` components
- [ ] **Stan compilation validation** - Ensure all test models compile without error using `rstan::stanc()`
- [ ] **Factor model compatibility** - Ensure factor models compile without error using `rstan::stanc()`
- [ ] **End-to-end model fitting tests** - Real mvgam model fitting with trend injection
- [ ] **Performance benchmarking** - Validate <1ms registry lookup and compilation efficiency
- [ ] **Edge case validation** - Missing data, irregular timing, complex grouping scenarios

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

### Complete Registry Architecture ✅
```r
# Core registry functions (R/trend_registry.R)
register_trend_type(name, supports_factors, generator_func, incompatibility_reason)
get_trend_info(name)                    # Retrieve trend information
list_trend_types()                      # List all registered trends  
validate_factor_compatibility(spec)     # Factor model validation
ensure_registry_initialized()           # Auto-initialization

# User-facing functions (R/trend_dispatcher.R)
custom_trend(trend, tpars, ...)         # Create custom trend objects
register_custom_trend(name, ...)        # User registration wrapper

# All core trends auto-registered: AR, RW, VAR, ZMVN, PW, CAR, PWlinear, PWlogistic
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

### Missing But Necessary Utility Functions
- Testing structure for Stan code creation and compilation outlined in `tests/testthat/test-stan-assembly-system.R`
- Multiple internal functions are required to ensure full Stan code is generated

**Stan Code Validation (3 missing)**:
- `validate_stan_code_structure()` - Check Stan blocks exist
- `validate_stan_syntax()` - Check Stan syntax
- `are_braces_balanced()` - Check brace matching

**Data Processing/Validation (5 missing)**:
- `extract_time_data()` - Extract time series data components
- `extract_series_data()` - Extract series data components
- `merge_stan_data()` - Merge Stan data from multiple sources
- `validate_stan_data_structure()` - Validate Stan data structure
- `is_valid_stanvar()` - Check stanvar validity

**Stan Code Processing (2 missing)**:
- `extract_code_block()` - Extract specific Stan code blocks
- `find_matching_brace()` - Find matching braces in Stan code

**Feature Detection (3 missing)**:
- `has_time_component()` - Check if stanvars have time components
- `has_series_component()` - Check if stanvars have series components
- `has_correlation_component()` - Check if stanvars have correlation

**Integration Support (1 missing)**:
- `prepare_stanvars_for_brms()` - Prepare stanvars for brms integration

## Decision Points This Sprint

### Stan Code Validation Strategy (Week 5 Priority)
- **Approach**: Two-stage validation - syntax first, then compilation
- **Implementation**: `rstan::stanc()` validation for all generated code
- **Performance**: Validate compilation time doesn't exceed setup gains
- Inspect `tests/testthat/test-stan-assembly-system.R`, `R/stan_validations.R` and `R/stan_code_generation.R` and add missing utility functions as necessary
- Re-run tests in `tests/testthat/test-stan-assembly-system.R` using r-test-runner agent and report back to user

### Parameter Renaming Scope (Week 5-6)
- **Challenge**: Ensure parameter renaming works across all brms model types
- **Test Cases**: Multivariate, distributional, hurdle, zero-inflated models
- **Validation**: Generated parameters match brms conventions exactly

## Success Criteria This Sprint

### Week 5 Success Metrics
- [ ] **Stan assembly produces compilable code** - Validated for all trend types
- [ ] **Stan code matches brms exactly** - Perfect match when no mvgam additions
- [ ] **Registry system optimal** - No redundancy; complementary function design  
- [ ] **Parameter renaming works universally** - Multivariate, distributional, complex models
- [ ] **Stan code compilation framework** - Complete validation system operational

### Week 6 Success Metrics
- [ ] Integration tests pass for multivariate models with response-specific trends
- [ ] Factor model compatibility confirmed with live Stan compilation
- [ ] End-to-end fitting works with grouping and correlation features
- [ ] Performance benchmarks confirm registry overhead <1ms
- [ ] Stan code generation handles edge cases (missing data, irregular timing)

### Foundation Validation
- [ ] Dual-object system functional with brms ecosystem methods
- [ ] Multiple imputation with Rubin's rules pooling operational
- [ ] Formula validation prevents autocorrelation conflicts
- [ ] Trend dispatcher handles custom registration
- [ ] Mock backend provides 10-50x setup speedup

## Next Sprint Preview (Week 7-8)
- Enhanced validation framework with registry integration
- User extension system documentation
- Factor model implementation and testing
