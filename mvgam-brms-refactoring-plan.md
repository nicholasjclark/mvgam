# mvgam → brms Extension Refactoring Plan

**Version**: 1.9  
**Date**: 2025-01-25  
**Status**: Implementation Ready

## Executive Summary

Transform mvgam from mgcv-based package into specialized brms extension adding State-Space modeling, N-mixture/occupancy models, and JSDMs. **Core Innovation**: brms generates linear predictors (`mu`, `mu_trend`), single combined Stan model, dual brmsfit-like objects for post-processing. **New Features**: Native multiple imputation with Rubin's rules pooling and enhanced leave-future-out cross-validation.

**Key Architecture Reference**: See `brms-stan-integration-patterns.md` for comprehensive analysis of brms design patterns, modular Stan code generation, and implementation strategies to guide this refactoring.

## Critical Design Principles

### 1. Single-Fit Dual-Object Architecture
- **brms role**: Linear predictor generation, Stan code setup, internal object creation (no fitting)
- **Single fit**: Combined Stan model with observation + trend components  
- **Dual objects**: Internal brmsfit-like objects for post-processing
- **Multivariate support**: Response-specific trends with preserved cross-response correlations

### 2. Formula-Centric Interface
```r
# Standard State-Space
mvgam(y ~ s(x), trend_formula = ~ RW(cor = TRUE), data = data)

# Distributional models: trends ONLY apply to main response parameter
mvgam(
  bf(y ~ s(x), sigma ~ s(z)),           # Distributional regression
  trend_formula = ~ AR(p = 1),          # Applied ONLY to mu (main parameter)
  family = gaussian(),
  data = data
)

# Multiple imputation with State-Space trends
mvgam(
  y ~ s(x), 
  trend_formula = ~ AR(p = 1),
  data = imputed_data_list,             # List of multiply imputed datasets
  combine = TRUE                        # Pool results using Rubin's rules
)

# Multivariate with response-specific trends
mvgam(
  bf(count ~ temp, biomass ~ precip) + set_rescor(FALSE),
  trend_formula = bf(
    count = ~ AR(p = 1),        # Population dynamics
    biomass = ~ RW(cor = TRUE), # Correlated biomass trends
    presence = NULL             # No trend (static occupancy)
  ),
  family = c(poisson(), gaussian(), bernoulli()),
  data = data
)

# Intelligent autocorrelation separation
mvgam(
  count ~ Trt + unstr(visit, patient),  # ✅ Observation-level correlation
  trend_formula = ~ AR(p = 1),          # ✅ State-Space dynamics
  data = data
)

# FORBIDDEN: Trend-level brms autocor
mvgam(
  y ~ s(x1),
  trend_formula = ~ s(time) + ar(p = 1),  # ❌ Conflicts with mvgam AR()
  data = data
)
```

### 3. Stan Code Strategy: Two-Stage Assembly

**Philosophy**: Leverage brms's modular Stan code generation system (detailed in `brms-stan-integration-patterns.md` Section 1-3) to inject State-Space components without modifying core brms functionality. Use the REF MCP server to explore documentation on the Stan modelling language when designing or reviewing Stan code

**Stage 1**: Generate complete trend stanvars and let brms handle data integration
```r
# Generate both data and code stanvars (following brms-stan-integration-patterns.md Section 3.1)
trend_stanvars <- generate_trend_stanvars_complete(trend_obj, data_info)

# brms generates both code and data with trend integration
base_stancode <- brms::stancode(obs_formula, data, stanvars = trend_stanvars)
base_standata <- brms::standata(obs_formula, data, stanvars = trend_stanvars)
```

**Stage 2**: Modify observation linear predictor with missing data preservation
```r
final_stancode <- inject_trend_into_linear_predictor(base_stancode, trend_spec)
# final_standata already contains trend data via stanvars - no modification needed
```

**Critical Stan Pattern** (following brms non-vectorized likelihood patterns from `brms-stan-integration-patterns.md` Section 4):
```stan
// brms produces mu and mu_trend for full time grid
mu_combined = mu + mu_trend;

// Likelihood only for non-missing observations (using obs_ind tracking)
{
  vector[n_nonmissing] selected_mu = mu_combined[obs_ind];
  flat_ys ~ family_distribution(selected_mu, ...);
}

// Trends evolve over ALL timesteps
trend[2:T] ~ normal(trend[1:(T-1)], sigma_trend);
```

### 4. Leave-Future-Out Cross-Validation

Enhanced time series cross-validation with brms ecosystem integration:

**Key Features**:
- Time-aware evaluation preserving temporal ordering
- Distributional forecast assessment (ELPD, RMSE, MAE, coverage, CRPS, energy scores)
- Multiple imputation pooling support
- Multivariate evaluation (joint and response-specific)
- Parallel computation for large evaluations

### 5. Validation Framework

**Context-Aware Autocorrelation** (implements intelligent separation described in `brms-stan-integration-patterns.md` Key Innovation #1)
**Key Validation Areas** (following brms validation patterns from `brms-stan-integration-patterns.md` Section 8):
- Distributional model restriction: Trends only for main response parameter (Section 6 of patterns doc)
- Response helper compatibility: `mi()`, `weights()`, `cens()`, `trunc()`, `trials()` 
- Multiple imputation: Dataset structure consistency, pooling compatibility
- Hurdle/zero-inflated models: Trends restricted to main (`mu`) parameter (Section 6)
- LFO-CV: Time series structure, forecast horizon limits, evaluation timepoint validity

## Implementation Timeline (16 Weeks)

### Phase 1: Foundation (Weeks 1-4)

#### Week 1: Trend Dispatcher System ✅ **COMPLETE**
Registry-based trend constructor system enabling `RW()`, `AR()`, `VAR()`, `CAR()`

#### Week 2: Formula Integration & Autocorrelation Validation ✅ **COMPLETE**
- Multivariate formula parsing with `mvbf()` support
- Context-aware autocorrelation validation preventing conflicts

#### Week 3: brms Setup Optimization ✅ **CONFIRMED**
- **Confirmed**: `backend = "mock"` is superior to `chains = 0` for setup speed
- **Locked Design**: Use `backend = "mock"` for all brms lightweight setup operations

#### Week 4: Single-Fit Architecture & Multiple Imputation ✅ **COMPLETE**
**Status**: Fully implemented and validated
**Achievement**: Revolutionary single-fit dual-object system with native multiple imputation

**Testing & Validation:**
- 6 comprehensive test files with 350+ test cases created
- Core architecture functions validated in production environment
- Foundation testing complete - architecture proven functional
- Strategic pivot to integration testing for Phase 2 Stan work

**Key Innovations Delivered:**
- Single fit produces dual brmsfit-like objects for seamless ecosystem integration
- Native multiple imputation detection with automatic Rubin's rules pooling
- Response-specific trend specifications in multivariate models
- Lightweight brms setup with 10-50x performance improvement
- Future-compatible parameter extraction with brms 3.0 considerations

### Phase 2: Stan Integration (Weeks 5-8)

#### Week 5-6: Registry-Based Stan Assembly with Simplified Architecture

**Key Innovation**: Replace complex conditional logic with **centralized registry system** that automatically handles factor model compatibility and validation: 
- Public API for custom trend registration: `register_custom_trend()`
- Generate stanvars that inject temporal components into brms Stan code
- Generators handle factor vs full models internally
- Maintain non-centered parameterization and consistent LV → trend patterns
- Integrate with existing `time` and `series` parameter system
- Follow brms stanvars patterns for data integration and threading compatibility

**Factor Model Support**:
- **Compatible**: AR, RW, VAR, ZMVN (stationary dynamics work with factor structure)
- **Incompatible**: PW (series-specific changepoints), CAR (irregular time spacing)
- **Pattern**: `n_lv < n_series` creates dynamic factor model with estimated loading matrix Z

#### Week 7: Enhanced Validation & User Extension System
- Registry-based validation framework with centralized error messages
- Intelligent autocorrelation validation using registry information
- Multivariate model validation: trends allowed for each response (`mu_*`) parameter
- Hurdle and distributional model validation: trends only for main (`mu`) parameter

#### Week 8: Factor Model Implementation & Testing
- Complete factor vs full model Stan code for compatible trends
- Loading matrix estimation with identification constraints
- Integration testing with registry dispatch system
- Performance validation (registry lookup should add <1ms overhead)

### Phase 3: Optimization & Methods (Weeks 9-12)

#### Week 9-10: Rcpp Functions & LFO-CV Implementation
```cpp
// Higher-order AR/VAR with embedded predictors
// [[Rcpp::export]]
Rcpp::NumericVector ar_p_recursC(Rcpp::NumericVector phi_coeffs, ...);

// Matrix-based JSDGAM prediction (10-100x speedup)
// [[Rcpp::export]]
arma::mat fast_jsdgam_predict(const arma::mat& design_matrices, ...);
```

Core LFO-CV implementation with multiple imputation pooling and comprehensive metrics.

#### Week 11: brms Prediction Integration & Multiple Imputation Pooling

**Implementation Strategy**: Follow brms ecosystem integration patterns from `brms-stan-integration-patterns.md` Section 4 to ensure seamless compatibility with all brms methods.

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

# Rubin's rules pooling for parameter estimates
pool_mvgam_fits <- function(fits) {
  estimates_list <- map(fits, extract_fit_estimates)
  pooled_estimates <- apply_rubins_rules(estimates_list)
  pooled_fit <- create_pooled_mvgam(fits[[1]], pooled_estimates)
  attr(pooled_fit, "individual_fits") <- fits
  attr(pooled_fit, "n_imputations") <- length(fits)
  class(pooled_fit) <- c("mvgam_pooled", class(fits[[1]]))
  return(pooled_fit)
}
```

#### Week 12: Method System Integration & Residual Functions
Critical methods: `log_lik()`, `update()`, `print()`, `loo()` with multiple imputation support

**Residual Functions**: Implement Dunn-Smyth randomized quantile residuals for all brms families or develop general solution using inverse CDF transformations. Essential for model diagnostics across the full range of brms observation families.

**Method Integration**: Follow brms method compatibility patterns from `brms-stan-integration-patterns.md` conclusion to ensure all brms ecosystem methods work seamlessly with mvgam objects.

### Phase 4: Testing & Launch (Weeks 13-16)

#### Week 13-14: Comprehensive Testing
- Stan code injection safety testing
- Multiple imputation workflow validation
- LFO-CV performance and accuracy testing
- Multivariate model integration testing

#### Week 15: Performance Optimization
- Memory usage optimization and object compression
- LFO-CV optimization with Rcpp acceleration
- Automated benchmarking validation

#### Week 16: Documentation & Release
- Function documentation with roxygen2
- Key vignettes: multivariate models, autocorrelation integration, multiple imputation, LFO-CV
- Migration guide and community feedback integration

## Key Innovation Points

1. **Autocorrelation Intelligence**: First package to properly distinguish observation-level residual correlation from State-Space dynamics (detailed in `brms-stan-integration-patterns.md` Critical Success Factor #2)
2. **Multivariate State-Space**: Response-specific trends while preserving brms cross-response correlations (Section 5 multivariate patterns)
3. **Multiple Imputation Integration**: Seamless support with Rubin's rules pooling
4. **Enhanced Time Series Cross-Validation**: Comprehensive LFO-CV with distributional evaluation
5. **Stan Extension Pattern**: Using brms `stanvars` for State-Space injection (Section 3 stanvars system)
6. **Method System Integration**: Dual brmsfit-like objects enabling seamless brms ecosystem compatibility (Section 4 ecosystem integration)

## Success Criteria

### Performance Targets
- [ ] brms setup: 10-50x faster initialization
- [ ] JSDGAM prediction: 10-100x speedup (Rcpp vs R loops)
- [ ] LFO-CV evaluation: 10x speedup for large datasets
- [ ] Memory usage: 30-50% reduction

### Functionality Requirements
- [ ] All existing mvgam features preserved
- [ ] Full brms compatibility: formulas/families/priors/response helpers/loo/waic/pp_check/diagnostics
- [ ] Multivariate models with response-specific trends
- [ ] Intelligent autocorrelation validation
- [ ] Complete data integration: Trend data seamlessly integrated via brms standata system
- [ ] Threading compatibility: Support brms within-chain parallelization
- [ ] Name conflict prevention: No stanvars conflicts with brms data variables
- [ ] Multiple imputation: Native support with Rubin's rules pooling
- [ ] Enhanced LFO-CV: Time series evaluation with comprehensive metrics
- [ ] Dunn-Smyth residuals: For all brms families or general randomized quantile solution
- [ ] >90% test coverage

## Dependencies & Migration

**Core Dependencies**: brms (≥2.19.0), Stan (≥2.30.0), Rcpp, checkmate, insight, bayesplot

**Migration Strategy**: 
- Target brms 2.x stable API as primary
- Add brms 3.0 compatibility when released
- Maintain backward compatibility where possible
- Clear migration guide for breaking changes

**Implementation Guide**: The brms design patterns documented in `brms-stan-integration-patterns.md` provide the architectural foundation for this refactoring, ensuring compatibility and leveraging brms's sophisticated code generation system.
