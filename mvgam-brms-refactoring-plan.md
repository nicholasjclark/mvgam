# mvgam → brms Extension Refactoring Plan

**Version**: 1.1 (Streamlined)  
**Date**: 2025-01-24  
**Status**: Design Complete - Ready for Implementation

## Executive Summary

Transform mvgam from mgcv-based standalone package into specialized brms extension adding State-Space modeling, N-mixture/occupancy models, and JSDMs. Architecture: brms for setup/internal objects, single combined Stan model, dual brmsfit-like objects for post-processing. **JSDGAM function** follows identical architecture with specialized validation for multi-species factor models.

## Core Architecture

### Single-Fit Dual-Object Strategy
- **brms for**: Setup, Stan code generation, internal object creation (no fitting)
- **Single fit**: Combined Stan model with observation + trend components  
- **Dual objects**: Internal brmsfit-like objects for post-processing
- **State-Space link**: Observation and trend layers connected in Stan

### Design Principles
1. **Observation layer**: Uses brms exactly as-is (no parameter renaming)
2. **Trend layer**: brms functionality with `_trend` suffixed parameters
3. **Formula-centric**: `trend_formula = ~ s(time) + RW(cor = TRUE)`
4. **Single learning curve**: Only need `brms::prior()` and `brms::set_prior()`

## Development Standards

### Validation Framework
- **checkmate**: Parameter validation (`assert_*()`)
- **insight**: Error/warning formatting (`format_error()`, `format_warning()`)
- **rlang**: Session warnings (`warn(..., .frequency = "once")`)

### Key Validation Differences: mvgam() vs jsdgam()

**mvgam() validation**:
- Single or multiple series allowed
- Optional trend components
- Standard State-Space constraints

**jsdgam() validation**:
- Requires multiple series (`n_series > 1`)
- Requires `lv > 0` (latent factors)
- Factor model identifiability constraints
- Specialized data2 processing for factor loadings
- **Three-level hierarchy** for `occ()` and `nmix()` families:
  - Level 1: Environmental factors → Species latent abundance/occupancy
  - Level 2: Species trends/dynamics  
  - Level 3: Observation process (detection/counting)

### Code Standards
- **80 char lines**, snake_case, tidyverse conventions
- Comprehensive roxygen2 documentation with examples
- Unit tests with >90% coverage

## Implementation Timeline (16 Weeks)

### Phase 1: Foundation (Weeks 1-4)

#### Week 1: Trend Dispatcher
```r
# Registry-based trend constructor system
trend <- function(type = "RW", ...) {
  get_trend_constructor(type)(...)
}

RW <- function(cor = FALSE, ma = FALSE, gr = 'NA', subgr = 'series', n_lv = NULL, ...) {
  structure(list(trend_type = "RW", cor = cor, ma = ma, gr = gr, 
                 subgr = subgr, n_lv = n_lv, formula_term = TRUE),
           class = c("mvgam_trend_term", "RW"))
}
```

#### Week 2: Formula Integration
```r
mvgam(
  formula = y ~ s(x1) + (1|group),
  trend_formula = ~ s(time) + cov1 + RW(cor = TRUE),
  priors = c(...),          # Standard brms (unchanged)
  trend_priors = c(...),    # Auto-renamed with _trend suffix
  data = data, data2 = data2, ...
)
```

#### Week 3: brms Setup Optimization
Benchmark and implement fastest brms setup method (`backend = "mock"` vs `chains = 0`).

#### Week 4: Single-Fit Architecture
```r
mvgam <- function(formula, trend_formula = NULL, ...) {
  # 1. Setup obs/trend models (no fitting)
  obs_setup <- setup_brms_lightweight(formula, data, ...)
  trend_setup <- setup_brms_lightweight(trend_spec$base_formula, ...)
  
  # 2. Extract trend stanvars from trend_setup's generated Stan code
  trend_stanvars <- extract_trend_stanvars_from_setup(trend_setup, trend_spec)
  
  # 3. Generate combined Stan code using trend stanvars
  combined_stancode <- generate_combined_stancode(obs_setup, trend_stanvars)
  
  # 4. Fit single model & create dual objects
  combined_fit <- fit_combined_stan_model(combined_stancode)
  return(create_mvgam_from_combined_fit(combined_fit, obs_setup, trend_setup))
}
```

**Critical Missing Data Handling**: State-Space models require latent states to evolve over ALL timesteps, including when observations are missing. Implementation must ensure:
- brms doesn't drop missing observations from likelihood computation
- Latent states continue evolving: `trend[t] ~ normal(trend[t-1], sigma)` for all t
- brms produces `mu` (observation linear predictor) and `mu_trend` (trend linear predictor)
- Final integration: `mu_combined = mu + mu_trend` preserving missing data patterns

### Stan Code Generation Efficiency Guidelines

**Bob Carpenter's Optimization Principles**:

#### 1. Vectorization Over Loops
Stan performs much better with vectorized operations than nested loops. For trend components:
```stan
// Efficient: Vectorized State-Space dynamics
target += normal_lpdf(trend[2:T] | trend[1:(T-1)] + X_trend[2:T] * b_trend, sigma_trend);

// Inefficient: Loop-based approach
for (t in 2:T) {
  target += normal_lpdf(trend[t] | trend[t-1] + X_trend[t] * b_trend, sigma_trend);
}
```

#### 2. Matrix Operations for Linear Predictors
Matrix operations are "much much faster than indexing". Apply to trend integration:
```stan
// Efficient: Matrix-based trend addition
mu += to_vector(trend[time, series]);

// Inefficient: Index-based approach  
for (n in 1:N) {
  mu[n] += trend[time[n], series[n]];
}
```

#### 3. State-Space Vectorization Patterns
For autoregressive processes, use slice notation for vectorized priors:
```stan
// AR(1) vectorized correctly
trend[1] ~ normal(X_trend[1] * b_trend, sigma_trend);
trend[2:T] ~ normal(trend[1:(T-1)] + X_trend[2:T] * b_trend, sigma_trend);
```

#### 4. Avoid Redundant Computations
Factor out terms to avoid redundant computations in AD stack:
```stan
// Efficient: Pre-compute linear predictor
vector[T] trend_mu = X_trend * b_trend;
trend[2:T] ~ normal(trend[1:(T-1)] + trend_mu[2:T], sigma_trend);

// Inefficient: Recompute X_trend * b_trend each time
trend[2:T] ~ normal(trend[1:(T-1)] + X_trend[2:T] * b_trend, sigma_trend);
```

#### 5. Use target += for Complex Models
For complex State-Space models, target += syntax provides more control and is required for certain post-processing:
```stan
// Complex trend dynamics with embedded predictors
target += normal_lpdf(trend_raw[1] | X_trend[1] * b_trend, sigma_trend);
for (t in 2:T) {
  target += normal_lpdf(trend_raw[t] | trend_raw[t-1] + X_trend[t] * b_trend, sigma_trend);
}
```

### Week 5-6: Two-Stage Stan Assembly
**Stage 1**: Extract trend stanvars from trend_setup and generate base model
```r
# Extract Stan code blocks from trend brms setup
trend_stanvars <- extract_trend_stanvars_from_setup(trend_setup, trend_spec)
# trend_stanvars contains: parameters, transformed parameters, model blocks

# Generate base brms model with trend components as stanvars
base_stancode <- brms::stancode(obs_formula, data, stanvars = trend_stanvars)
```

**Stage 2**: Modify observation linear predictor with missing data preservation
```r
final_stancode <- inject_trend_into_linear_predictor(base_stancode, trend_spec)
```

**Trend Stanvar Extraction**: Parse trend_setup$stancode to extract:
- Parameter declarations (`vector<lower=0>[n_series] sigma_trend;`)
- Transformed parameters (`matrix[T,n_series] mu_trend;`) - brms generates this
- Model dynamics (`trend[t] ~ normal(trend[t-1], sigma_trend);`) - ALL timesteps
- Generated quantities for predictions

**Missing Data Architecture**: Critical adaptation from current mvgam approach:
- **Current pattern**: Replace NAs with imputed values, track via `obs_ind` and use `flat_ys` for likelihood
- **brms handles**: `mu` (observation linear predictor) and `mu_trend` (trend linear predictor)
- **Key requirement**: Preserve mvgam's missing data tracking while letting brms generate linear predictors
- **Integration pattern**: `mu_final = mu + mu_trend` with missing data awareness

**Missing Data Strategy**:
```r
# Adapt current mvgam approach for brms compatibility
prepare_missing_data_for_brms <- function(data, formula, trend_formula) {
  # 1. Replace NAs with imputed values (following current mvgam pattern)
  # 2. Create obs_ind tracking which observations are non-missing
  # 3. Ensure brms can compute mu/mu_trend for full time grid
  # 4. Pass obs_ind to Stan for likelihood computation on non-missing only
  # 5. Preserve State-Space evolution over ALL timesteps
}
```

**Benefits**: Uses brms's designed extension mechanism, targeted modification only.

#### Week 7: Dynamic Prior System & Missing Data Handling
```r
# Dual prior arguments with systematic _trend renaming
rename_trend_priors_dynamic <- function(trend_priors) {
  # b → b_trend, Intercept → Intercept_trend, etc.
}

# Extract trend components from brms-generated Stan code
extract_trend_stanvars_from_setup <- function(trend_setup, trend_spec) {
  # Parse trend_setup$stancode to extract Stan blocks
  trend_stancode <- brms::stancode(trend_setup)
  
  # Extract relevant blocks with _trend parameter renaming
  parameters_block <- extract_and_rename_parameters(trend_stancode)
  tparameters_block <- extract_and_rename_tparameters(trend_stancode)
  model_block <- extract_and_rename_model(trend_stancode, trend_spec)
  
  # Create stanvars for injection into observation model
  stanvars <- list(
    brms::stanvar(block = "parameters", scode = parameters_block),
    brms::stanvar(block = "transformed parameters", scode = tparameters_block),
    brms::stanvar(block = "model", scode = model_block)
  )
  
  return(stanvars)
}

# Handle missing data following current mvgam pattern
prepare_missing_data_for_brms <- function(data, formula, trend_formula) {
  # 1. Replace NAs with imputed values (preserve current mvgam approach)
  # 2. Create obs_ind tracking non-missing observations  
  # 3. Ensure brms computes mu/mu_trend for full time grid
  # 4. Pass obs_ind to Stan for selective likelihood computation
  # 5. Preserve State-Space evolution over ALL timesteps
}
```

**Stan Integration Pattern Following Current mvgam**:
```stan
// brms produces mu and mu_trend for full time grid
mu_combined = mu + mu_trend;

// Likelihood only for non-missing observations (current mvgam pattern)
{
  vector[n_nonmissing] flat_trends;
  flat_trends = to_vector(mu_combined)[obs_ind];  // Use obs_ind tracking
  flat_ys ~ family_distribution(flat_trends, ...);
}

// But trends evolve over ALL timesteps
trend[2:T] ~ normal(trend[1:(T-1)], sigma_trend);
```

#### Week 8: Higher-Order Models & data2 Integration
- Extended AR/VAR: `AR(p = c(1, 12, 24))`, `VAR(p = 3)`
- Factor loading constraints via data2
- Custom families: `nmix()`, `tweedie()`, `occ()` with three-level hierarchical structure
- **Three-Level JSDGAM**: For `occ()`/`nmix()` families only, supporting:
  - Environmental factors → Species latent abundance/occupancy
  - Species factor-mediated trends  
  - Observation process (detection/counting)

**JSDGAM Function**: Closely mimics `mvgam()` architecture but with specialized validation:
```r
jsdgam <- function(formula, data, data2 = NULL, lv = 0, family = gaussian(), 
                   trend_formula = NULL, latent_formula = NULL, ...) {
  # Different validation: require multiple series, check lv > 0
  validate_jsdgam_inputs(formula, data, lv, family)
  
  # Three-level hierarchy for occ()/nmix() families
  if (family$family %in% c("occ", "nmix")) {
    # Level 1: Environmental factors → Species abundance/occupancy
    latent_setup <- setup_brms_lightweight(latent_formula, data, data2, ...)
    # Level 2: Species trends (same as standard jsdgam)
    trend_setup <- setup_brms_lightweight(trend_spec$base_formula, ...)
    # Level 3: Observation process (detection/counting)
    obs_setup <- setup_brms_lightweight(formula, data, data2, family, ...)
    
    # Extract stanvars for three-level model
    latent_stanvars <- extract_latent_abundance_stanvars(latent_setup, lv)
    trend_stanvars <- extract_trend_stanvars_with_factors(trend_setup, lv)
    
    # Combine all three levels in Stan code
    combined_stancode <- generate_three_level_stancode(obs_setup, trend_stanvars, 
                                                      latent_stanvars, family)
  } else {
    # Standard two-level jsdgam (same as mvgam pattern)
    obs_setup <- setup_brms_lightweight(formula, data, data2, family, ...)
    trend_setup <- setup_brms_lightweight(trend_spec$base_formula, ...)
    # ... standard pattern
  }
}
```

**Three-Level Prediction Strategy**: 
- **Level 1 predictions**: `type = 'latent_abundance'` or `type = 'latent_occupancy'`
- **Level 2 predictions**: `type = 'trend'` (factor-mediated trends)
- **Level 3 predictions**: `type = 'response'` (observed detections/counts)

### Phase 3: Optimization (Weeks 9-12)

#### Week 9-10: Rcpp Functions
```cpp
// Higher-order processes with embedded predictors
// [[Rcpp::export]]
Rcpp::NumericVector ar_p_recursC(Rcpp::NumericVector phi_coeffs,
                                  double drift, Rcpp::NumericVector linpreds,
                                  Rcpp::NumericVector errors, int h, int p);

// Fast JSDGAM prediction (replaces R loops at line 309+)
// [[Rcpp::export]]
arma::mat fast_jsdgam_predict(const arma::mat& design_matrices, ...);
```

#### Week 11: brms Prediction Integration
```r
posterior_predict.mvgam <- function(object, newdata = NULL, ...) {
  # Use brms::prepare_predictions() for full pipeline
  prep <- brms::prepare_predictions(object$obs_fit, newdata = newdata, ...)
  
  # Add trend effects to linear predictor
  base_linpred <- brms::posterior_linpred_draws(prep)
  trend_effects <- predict_trend_effects(object, prep)
  prep <- update_brmsprep_linpred(prep, base_linpred + trend_effects)
  
  # Let brms handle family transformations
  return(brms::posterior_predict_draws(prep))
}

# JSDGAM-specific prediction for three-level models
posterior_predict.jsdgam <- function(object, newdata = NULL, type = "response", ...) {
  if (object$family$family %in% c("occ", "nmix") && type %in% c("latent_abundance", "latent_occupancy")) {
    # Level 1: Environmental factors → Species abundance/occupancy
    return(predict_latent_abundance_jsdgam(object, newdata, type, ...))
  } else if (type == "trend") {
    # Level 2: Factor-mediated species trends
    return(predict_factor_trends_jsdgam(object, newdata, ...))
  } else {
    # Level 3: Standard observation process (use mvgam method)
    NextMethod()
  }
}
```

#### Week 12: Method System
Full brms method support using dual brmsfit-like objects.

### Phase 4: Testing & Launch (Weeks 13-16)

#### Week 13-14: Comprehensive Testing
- All trend types, formula parsing, higher-order models
- Data2 integration, Rcpp optimization, prior system
- Performance benchmarking vs current implementation

#### Week 15: Performance Optimization
- Object footprint reduction
- Memory usage optimization
- Final performance validation

#### Week 16: Documentation & Release Prep
- Complete documentation updates
- Migration guide from v1.x to v2.0
- Community testing feedback integration

## Key Benefits

### Performance Targets
- **brms Setup**: 10-50x faster initialization
- **JSDGAM Prediction**: 10-100x speedup (Rcpp vs R loops)
- **Memory Usage**: 30-50% reduction
- **Prior System**: 5-10x speedup

### User Experience
- **Formula-centric**: Everything in formulas
- **Single learning curve**: Only brms prior syntax needed
- **Full compatibility**: All current brms functionality preserved
- **Better errors**: Smart validation with suggestions

### Architecture
- **Modular**: Clear observation/State-Space separation
- **Extensible**: Easy new trend types via dispatcher
- **Future-proof**: Leverages brms development
- **Maintainable**: Single source of truth

## Risk Mitigation & brms 3.0 Compatibility

### brms 3.0 Breaking Changes Impact

**Timeline Reality**: As of July 2018, Paul Bürkner stated brms 3.0 is "still a long way to go". Current milestone shows 24 open issues with no set release date. **Our 16-week refactoring timeline likely precedes brms 3.0 release.**

**API Changes**:
- `make_stancode`/`make_standata` transformed into generic S3 methods with aliases for backward compatibility
- `get_prior` becomes alias of new generic `default_prior` method
- brmsfit object cleanup: removal of `family`, `autocor`, `ranef` elements; data restructuring

**Stan Code Generation Changes**:
- No automatic Stan code canonicalization with cmdstanr backend
- Switch to new Stan array syntax (requires Stan ≥2.26)
- Parameter class name improvements affecting our `_trend` renaming strategy

**Reduced Risk Assessment**: Given brms 3.0's extended timeline, we can:
1. **Implement with brms 2.x as primary target** (current stable API)
2. **Add 3.0 compatibility as future enhancement** rather than core requirement
3. **Monitor milestone progress** and adjust strategy if 3.0 release accelerates

**Mitigation Strategies**:
1. **Conservative API Usage**: Use established brms 2.x patterns that are less likely to change
```r
# Robust approach - use current stable API
stancode_obs <- brms::stancode(obs_formula, data, ...)
stancode_trend <- brms::stancode(trend_formula, trend_data, ...)
```

2. **Defensive Programming**: Plan for object structure evolution
```r
# Future-proof extraction
extract_family_safe <- function(brms_object) {
  if ("family" %in% names(brms_object)) {
    return(brms_object$family)  # Current structure
  } else if (!is.null(brms_object$formula$family)) {
    return(brms_object$formula$family)  # Potential 3.0 structure
  }
  stop("Cannot extract family from brmsfit object")
}
```

3. **Version Detection Layer**: Implement when 3.0 approaches
```r
# Future enhancement - not required for initial implementation
brms_version_compat <- function() {
  version <- packageVersion("brms")
  list(
    major = version$major,
    supports_new_api = version >= "3.0.0",
    requires_compatibility_layer = version >= "3.0.0"
  )
}
```

### Primary Risks & Solutions
1. **Stan Integration Complexity** → Extensive testing, fallback patterns
2. **Performance Regression** → Profile bottlenecks, parallel implementation
3. **brms API Changes** → Version-specific adaptations, compatibility testing
4. **Linear Predictor Detection** → Multiple patterns, explicit fallback
5. **Data Alignment** → Validation functions, comprehensive testing

### Contingency Plans
- Maintain parallel old/new implementations during transition
- Backward compatibility layer for interface changes
- Comprehensive test suite covering edge cases
- Performance monitoring and automated benchmarks

## Success Metrics

- [ ] All existing functionality preserved
- [ ] Performance targets achieved  
- [ ] Memory usage reduced by 30-50%
- [ ] Simplified user interface working
- [ ] Full brms compatibility maintained
- [ ] >90% test coverage achieved

## Migration Strategy

1. **Weeks 1-8**: Development branch with `use_brms_backend` flag
2. **Weeks 9-12**: Alpha testing with community feedback
3. **Weeks 13-16**: Beta release, default to new architecture
4. **Post-release**: mvgam v2.0 production, remove old implementation

## Quality Assurance

### Standards
- All PRs require maintainer review
- New features need unit tests with >90% coverage
- Performance benchmarks for optimization claims
- Complete documentation for user-facing changes

### Testing Strategy
- Unit/Integration/Performance/Regression/Edge case tests
- Automated benchmarking suite
- Memory usage tracking
- Community alpha/beta testing

---

**Next Step**: Begin Week 1 - Trend Type Dispatcher System  
**Dependencies**: brms (≥2.19.0), Stan (≥2.30.0), Rcpp, checkmate, insight

### brms 3.0 Transition Strategy

**Primary Development (Weeks 1-16)**: Target brms 2.x stable API  
**Future Enhancement**: Add brms 3.0 compatibility when released  
**Monitoring**: Track brms 3.0 milestone progress for timeline adjustments
