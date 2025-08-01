# MVGam → brms Extension Full Timeline

**Version**: 1.9  
**Date**: 2025-01-30  
**Status**: Implementation in Progress (Week 5)

## Executive Summary

Transform mvgam from mgcv-based package into specialized brms extension adding State-Space modeling, N-mixture/occupancy models, and JSDMs. **Core Innovation**: brms generates linear predictors, single combined Stan model, dual brmsfit-like objects for post-processing. **New Features**: Native multiple imputation with Rubin's rules pooling and enhanced leave-future-out cross-validation.

## Implementation Timeline (16 Weeks)

### Phase 1: Foundation ✅ **COMPLETE** (Weeks 1-4)

#### Week 1: Trend Dispatcher System ✅ 
**Status**: Complete  
**Achievement**: Registry-based trend constructor system enabling `RW()`, `AR()`, `VAR()`, `CAR()`

#### Week 2: Formula Integration & Autocorrelation Validation ✅
**Status**: Complete  
**Achievement**: 
- Multivariate formula parsing with `mvbf()` support
- Context-aware autocorrelation validation preventing conflicts

#### Week 3: brms Setup Optimization ✅  
**Status**: Complete
**Achievement**: Confirmed `backend = "mock"` provides 10-50x faster initialization for brms setup

#### Week 4: Single-Fit Architecture & Multiple Imputation ✅
**Status**: Complete and Validated
**Achievement**: 
- Revolutionary single-fit dual-object system
- Native multiple imputation detection with automatic Rubin's rules pooling
- 6 comprehensive test files with 350+ test cases created
- Foundation testing complete - architecture proven functional

### Phase 2: Stan Integration 🔄 **IN PROGRESS** (Weeks 5-8)

#### Week 5-6: Registry-Based Stan Assembly ⚠️ **CURRENT SPRINT**
**Objectives**:
- Public API for custom trend registration: `register_custom_trend()`
- Generate stanvars that inject temporal components into brms Stan code
- Generators handle factor vs full models internally
- Maintain non-centered parameterization and consistent LV → trend patterns
- Integrate with existing `time` and `series` parameter system

**Key Deliverables**:
- [ ] Registry dispatch system for trend types
- [ ] Factor model compatibility detection
- [ ] Basic stanvars generation for AR, RW trends
- [ ] VAR, ZMVN trend support in registry
- [ ] Performance validation (<1ms registry lookup)

**Factor Model Support**:
- **Compatible**: AR, RW, VAR, ZMVN (stationary dynamics work with factor structure)
- **Incompatible**: PW (series-specific changepoints), CAR (irregular time spacing)

#### Week 7: Enhanced Validation & User Extension System
**Objectives**:
- Registry-based validation framework with centralized error messages
- Intelligent autocorrelation validation using registry information
- Multivariate model validation: trends allowed for each response (`mu_*`) parameter
- Hurdle and distributional model validation: trends only for main (`mu`) parameter

**Deliverables**:
- [ ] Comprehensive validation framework integrated with registry
- [ ] User extension documentation and examples
- [ ] Context-aware error messages for common mistakes
- [ ] Integration testing with multivariate models

#### Week 8: Factor Model Implementation & Testing
**Objectives**:
- Complete factor vs full model Stan code for compatible trends
- Loading matrix estimation with identification constraints
- Integration testing with registry dispatch system
- Performance validation

**Deliverables**:
- [ ] Factor model Stan code generation
- [ ] Loading matrix estimation with proper identification
- [ ] Complete integration test suite
- [ ] Performance benchmarks for registry system

### Phase 3: Optimization & Methods (Weeks 9-12)

#### Week 9-10: Rcpp Functions & LFO-CV Implementation
**Objectives**:
- Implement higher-order AR/VAR with embedded predictors using Rcpp
- Matrix-based JSDGAM prediction for 10-100x speedup
- Core LFO-CV implementation with multiple imputation pooling
- Comprehensive evaluation metrics (ELPD, RMSE, MAE, coverage, CRPS, energy scores)

**Key Functions**:
```cpp
// Higher-order AR/VAR with embedded predictors
// [[Rcpp::export]]
Rcpp::NumericVector ar_p_recursC(Rcpp::NumericVector phi_coeffs, ...);

// Matrix-based JSDGAM prediction (10-100x speedup)
// [[Rcpp::export]]
arma::mat fast_jsdgam_predict(const arma::mat& design_matrices, ...);
```

**Deliverables**:
- [ ] Rcpp acceleration functions for time series prediction
- [ ] LFO-CV framework with time-aware evaluation
- [ ] Multiple imputation pooling for cross-validation
- [ ] Parallel computation support for large evaluations

#### Week 11: brms Prediction Integration & Multiple Imputation Pooling
**Objectives**:
- Seamless integration with brms prediction infrastructure
- Rubin's rules pooling for parameter estimates and predictions
- Support for all brms ecosystem methods (posterior_predict, fitted, etc.)

**Implementation Strategy**:
```r
# Prediction methods with multiple imputation support
posterior_predict.mvgam <- function(object, newdata = NULL, resp = NULL, ...) {
  if (inherits(object, "mvgam_pooled")) {
    return(posterior_predict_multiple(object, newdata, resp, ...))
  }
  
  # Use brms::prepare_predictions() for full pipeline compatibility
  prep <- brms::prepare_predictions(object$obs_fit, newdata = newdata, ...)
  base_linpred <- brms::posterior_linpred_draws(prep)
  trend_effects <- predict_trend_effects(object, prep)
  prep <- update_brmsprep_linpred(prep, base_linpred + trend_effects)
  
  return(brms::posterior_predict_draws(prep))
}
```

**Deliverables**:
- [ ] Complete brms prediction method integration
- [ ] Rubin's rules pooling implementation
- [ ] Multiple imputation workflow validation
- [ ] Ecosystem compatibility testing

#### Week 12: Method System Integration & Residual Functions
**Objectives**:
- Critical methods: `log_lik()`, `update()`, `print()`, `loo()` with multiple imputation support
- Dunn-Smyth randomized quantile residuals for all brms families
- Complete method system integration

**Key Focus Areas**:
- **Residual Functions**: Implement Dunn-Smyth randomized quantile residuals for all brms families or develop general solution using inverse CDF transformations
- **Method Integration**: Ensure all brms ecosystem methods work seamlessly with mvgam objects
- **Multiple Imputation**: Full method support for pooled objects

**Deliverables**:
- [ ] Complete method system with multiple imputation support
- [ ] Dunn-Smyth residuals for model diagnostics
- [ ] Integration testing with brms ecosystem
- [ ] Performance optimization for method dispatch

### Phase 4: Testing & Launch (Weeks 13-16)

#### Week 13-14: Comprehensive Testing
**Objectives**:
- Stan code injection safety testing across all trend types
- Multiple imputation workflow validation with real datasets
- LFO-CV performance and accuracy testing
- Multivariate model integration testing with complex scenarios

**Testing Scope**:
- **Stan Code Safety**: All trend types compile and run correctly
- **Multiple Imputation**: Dataset structure consistency, pooling accuracy
- **LFO-CV**: Time series structure, forecast horizon limits, evaluation metrics
- **Multivariate Models**: Response-specific trends, correlation preservation
- **Edge Cases**: Missing data patterns, irregular time series, factor models

**Deliverables**:
- [ ] Complete test suite with >90% coverage
- [ ] Performance benchmarks against targets
- [ ] Integration testing with real-world datasets
- [ ] Stress testing for edge cases and error conditions

#### Week 15: Performance Optimization
**Objectives**:
- Memory usage optimization and object compression
- LFO-CV optimization with Rcpp acceleration
- Automated benchmarking validation against performance targets

**Performance Targets**:
- **brms setup**: 10-50x faster initialization
- **JSDGAM prediction**: 10-100x speedup (Rcpp vs R loops)
- **LFO-CV evaluation**: 10x speedup for large datasets
- **Memory usage**: 30-50% reduction through optimization

**Deliverables**:
- [ ] Memory optimization implementation
- [ ] Performance benchmarking suite
- [ ] Automated performance regression testing
- [ ] Optimization documentation and guidelines

#### Week 16: Documentation & Release
**Objectives**:
- Complete function documentation with roxygen2
- Key vignettes covering major features
- Migration guide for existing mvgam users
- Community feedback integration and release preparation

**Documentation Deliverables**:
- [ ] Complete function documentation
- [ ] Vignette: Multivariate State-Space models
- [ ] Vignette: Autocorrelation integration (observation vs State-Space)
- [ ] Vignette: Multiple imputation workflows
- [ ] Vignette: Enhanced LFO-CV for time series
- [ ] Migration guide: mvgam 1.x → brms-extension
- [ ] Performance guide: Optimization strategies

## Success Criteria

### Performance Targets
- [x] brms setup: 10-50x faster initialization ✅ **ACHIEVED**
- [ ] JSDGAM prediction: 10-100x speedup (Rcpp vs R loops)
- [ ] LFO-CV evaluation: 10x speedup for large datasets
- [ ] Memory usage: 30-50% reduction

### Functionality Requirements
- [x] All existing mvgam features preserved ✅ **CONFIRMED**
- [x] Single-fit dual-object architecture ✅ **IMPLEMENTED**
- [x] Native multiple imputation with Rubin's rules ✅ **IMPLEMENTED**
- [ ] Full brms compatibility: formulas/families/priors/response helpers/loo/waic/pp_check/diagnostics
- [ ] Multivariate models with response-specific trends
- [ ] Intelligent autocorrelation validation
- [ ] Complete data integration: Trend data seamlessly integrated via brms standata system
- [ ] Threading compatibility: Support brms within-chain parallelization
- [ ] Name conflict prevention: No stanvars conflicts with brms data variables
- [ ] Enhanced LFO-CV: Time series evaluation with comprehensive metrics
- [ ] Dunn-Smyth residuals: For all brms families or general randomized quantile solution
- [ ] >90% test coverage

### Architecture Requirements
- [x] Zero modification of brms internals ✅ **CONFIRMED**
- [x] stanvars-based extension system ✅ **IMPLEMENTED**
- [x] Dual brmsfit-like objects for ecosystem integration ✅ **IMPLEMENTED**
- [x] Context-aware autocorrelation validation ✅ **IMPLEMENTED**
- [ ] Registry-based trend extension system
- [ ] Factor model compatibility framework
- [ ] Non-centered parameterization standard
- [ ] Thread-safe Stan code generation

## Key Innovation Points

1. **Autocorrelation Intelligence**: First package to properly distinguish observation-level residual correlation from State-Space dynamics
2. **Multivariate State-Space**: Response-specific trends while preserving brms cross-response correlations
3. **Multiple Imputation Integration**: Seamless support with Rubin's rules pooling
4. **Enhanced Time Series Cross-Validation**: Comprehensive LFO-CV with distributional evaluation
5. **Stan Extension Pattern**: Using brms `stanvars` for State-Space injection
6. **Method System Integration**: Dual brmsfit-like objects enabling seamless brms ecosystem compatibility

## Dependencies & Migration

**Core Dependencies**: brms (≥2.19.0), Stan (≥2.30.0), Rcpp, checkmate, insight, bayesplot

**Migration Strategy**: 
- Target brms 2.x stable API as primary
- Add brms 3.0 compatibility when released
- Maintain backward compatibility where possible
- Clear migration guide for breaking changes

## Risk Assessment & Mitigation

### High-Risk Areas

**Week 5-6: Registry System Performance**
- **Risk**: Registry lookup overhead >1ms impacts model setup time
- **Mitigation**: Benchmark early, fallback to direct dispatch if needed
- **Monitoring**: Automated performance tests in CI

**Week 7-8: Factor Model Complexity**
- **Risk**: Factor model Stan code generation becomes too complex
- **Mitigation**: Start with simple cases, iterate to complexity
- **Monitoring**: Integration tests with increasing complexity

**Week 9-10: Rcpp Integration**
- **Risk**: C++ compilation issues across platforms
- **Mitigation**: Start with simple functions, comprehensive testing
- **Monitoring**: Multi-platform CI testing

**Week 13-15: Performance Targets**
- **Risk**: Failing to achieve 10-100x speedup targets
- **Mitigation**: Profile early, optimize incrementally, adjust targets if needed
- **Monitoring**: Continuous benchmarking against baselines

### Contingency Plans

**Registry Performance Issues**:
- Fallback to direct function dispatch
- Lazy loading of registry entries
- Compiled registry for faster lookup

**Stan Code Complexity**:
- Reduce feature scope for initial release
- Defer advanced factor models to v2.0
- Focus on core AR/RW/VAR trends first

**Testing Timeline Pressure**:
- Prioritize core functionality testing
- Automated test generation for edge cases
- Community beta testing program

## Post-Launch Roadmap

### Version 2.0 Enhancements (Future)
- Advanced factor model features (time-varying loadings)
- Custom family extensions (N-mixture, occupancy models)
- Spatial autocorrelation integration
- Advanced missing data patterns
- Real-time prediction interfaces

### Community Integration
- CRAN submission timeline
- Academic paper publication
- Conference presentations
- Workshop development
- User feedback integration process
