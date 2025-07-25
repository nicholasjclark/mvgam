# mvgam → brms Extension Refactoring Plan

**Version**: 1.5 (Balanced Implementation Guide)  
**Date**: 2025-01-25  
**Status**: Ready for Implementation

## Executive Summary

Transform mvgam from mgcv-based standalone package into specialized brms extension adding State-Space modeling, N-mixture/occupancy models, and JSDMs. **Core Innovation**: brms generates linear predictors (`mu`, `mu_trend`), single combined Stan model, dual brmsfit-like objects for post-processing. Full multivariate model support preserving brms cross-response correlations while adding State-Space dynamics.

## Critical Design Principles

### 1. **Single-Fit Dual-Object Architecture**
- **brms role**: Linear predictor generation (`mu` + `mu_trend`), Stan code setup, internal object creation (no fitting)
- **Single fit**: Combined Stan model with observation + trend components  
- **Dual objects**: Internal brmsfit-like objects for post-processing
- **State-Space link**: Observation and trend layers connected in Stan
- **Multivariate support**: Response-specific trends with preserved cross-response correlations

### 2. **Formula-Centric Interface**
```r
# Standard State-Space
mvgam(y ~ s(x), trend_formula = ~ RW(cor = TRUE), data = data)

# Multivariate with response-specific trends
mvgam(
  mvbf(count ~ temp, biomass ~ precip),
  trend_formula = list(
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

mvgam(
  y ~ x + ar(time, group),        # ✅ Residual AR at obs level
  trend_formula = ~ RW(cor = TRUE), # ✅ Latent trends
  data = data
)

# FORBIDDEN: Trend-level brms autocor
mvgam(
  y ~ s(x1),
  trend_formula = ~ s(time) + ar(p = 1),  # ❌ Conflicts with mvgam AR()
  data = data
)
```

### 3. **Stan Code Strategy: Two-Stage Assembly**

**Stage 1**: Extract trend stanvars from brms setup
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

**Critical Stan Pattern**:
```stan
// brms produces mu and mu_trend for full time grid
mu_combined = mu + mu_trend;

// Multivariate case: per-response handling
if (is_multivariate) {
  mu1_combined = mu1 + mu1_trend;
  mu2_combined = mu2 + mu2_trend;
}

// Likelihood only for non-missing observations (using obs_ind tracking)
{
  vector[n_nonmissing] selected_mu = mu_combined[obs_ind];
  flat_ys ~ family_distribution(selected_mu, ...);
}

// Trends evolve over ALL timesteps
trend[2:T] ~ normal(trend[1:(T-1)], sigma_trend);
```

**Stan Modification Requirements**:
- Uses brms's designed extension mechanism (`stanvars`)
- Preserves parameter constraints and transformations
- Handles nonlinear formula complexity (`bf(nl = TRUE)`)
- Maintains brms correlation structures
- Supports multivariate response-specific trends

### 4. **Validation Framework**

**Context-Aware Autocorrelation**:
```r
validate_autocor_usage <- function(formula, trend_formula) {
  # Observation-level autocor: ALLOWED (residual correlation structures)
  obs_autocor <- extract_autocor_terms(formula)
  if (length(obs_autocor) > 0) {
    insight::format_warning(
      "Using brms autocorrelation for observation-level residual structure.",
      "This models residual correlation and complements State-Space trends.",
      .frequency = "once"
    )
  }
  
  # Trend-level autocor: FORBIDDEN (conflicts with State-Space dynamics)
  trend_autocor <- extract_autocor_terms(trend_formula)
  if (length(trend_autocor) > 0) {
    stop(insight::format_error(
      "brms autocorrelation terms not allowed in trend_formula.",
      "Use mvgam trend types: ar(p = 1) → AR(p = 1)"
    ))
  }
}
```

**Multivariate Validation**:
- Response-trend mapping consistency
- Family compatibility with trend types  
- Missing data consistency across responses
- Cross-response correlation preservation

## Multivariate Model Integration

### Architecture Extension
Each response variable can have its own State-Space trend component while preserving brms cross-response correlations.

### Stan Code Architecture
```stan
// Multiple trend parameters: b_trend_y1, b_trend_y2, etc.
// Response-specific missing data: obs_ind_1, obs_ind_2 tracking
// Conditional trend evolution: Only for responses with trends
// Preserve brms correlations: Don't interfere with cross-response structures

vector[N] mu1 = mu1_base + mu1_trend;  // Response 1: base + trend
vector[N] mu2 = mu2_base + mu2_trend;  // Response 2: base + trend  
vector[N] mu3 = mu3_base;              // Response 3: base only (no trend)

// Preserve brms multivariate correlation structure while adding State-Space dynamics
{
  vector[n_nonmissing_1] selected_mu1 = mu1[obs_ind_1];
  vector[n_nonmissing_2] selected_mu2 = mu2[obs_ind_2];
  vector[n_nonmissing_3] selected_mu3 = mu3[obs_ind_3];
  
  flat_y1s ~ poisson_log(selected_mu1);
  flat_y2s ~ normal(selected_mu2, sigma_y2);
  flat_y3s ~ bernoulli_logit(selected_mu3);
}

// State-Space evolution for each trend component
if (has_trend_y1) trend_y1[2:T] ~ normal(trend_y1[1:(T-1)], sigma_trend_y1);
if (has_trend_y2) trend_y2[2:T] ~ normal(trend_y2[1:(T-1)], sigma_trend_y2);
```

### Implementation Requirements
```r
parse_multivariate_trends <- function(formula, trend_formula) {
  if (is.mvbrmsformula(formula)) {
    response_names <- extract_response_names(formula)
    
    if (is.list(trend_formula)) {
      validate_trend_response_mapping(response_names, trend_formula)
    } else if (!is.null(trend_formula)) {
      # Single trend applied to all responses
      trend_formula <- rep(list(trend_formula), length(response_names))
      names(trend_formula) <- response_names
    }
  }
  return(list(responses = response_names, trends = trend_formula))
}
```

## Development Standards

### brms Compatibility Strategy
**Target**: brms 2.x stable API as primary, add 3.0 compatibility when released
**Monitoring**: Automated compatibility testing across brms versions
**Fallback**: Graceful degradation strategies for API changes

### Validation Tools
- **checkmate**: Parameter validation (`assert_*()`)
- **insight**: Error/warning formatting (`format_error()`, `format_warning()`)
- **rlang**: Session warnings (`warn(..., .frequency = "once")`)

### Code Standards
- **80 char lines**, snake_case, tidyverse conventions
- Comprehensive roxygen2 documentation with examples
- Unit tests with >90% coverage

## Implementation Timeline (16 Weeks)

### Phase 1: Foundation (Weeks 1-4)

#### Week 1: Trend Dispatcher System
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

#### Week 2: Formula Integration & Intelligent Autocor Validation
- Multivariate formula parsing with `mvbf()` support
- Context-aware autocorrelation validation
- Educational error messages for trend vs observation level conflicts

#### Week 3: brms Setup Optimization
- Benchmark fastest brms setup method (`backend = "mock"` vs `chains = 0`)
- Optimize for 10-50x setup speed improvement

#### Week 4: Single-Fit Architecture & Backend Strategy
```r
mvgam <- function(formula, trend_formula = NULL, backend = NULL, ...) {
  # 1. Parse multivariate formulas if needed
  mv_spec <- parse_multivariate_trends(formula, trend_formula)
  
  # 2. Setup obs/trend models (no fitting)
  obs_setup <- setup_brms_lightweight(formula, data, ...)
  trend_setup <- setup_brms_lightweight(trend_spec$base_formula, ...)
  
  # 3. Extract trend stanvars and generate combined Stan code
  trend_stanvars <- extract_trend_stanvars_from_setup(trend_setup, trend_spec)
  combined_stancode <- generate_combined_stancode(obs_setup, trend_stanvars)
  
  # 4. Use enhanced mvgam backend system
  combined_fit <- fit_mvgam_model(combined_stancode, combined_standata, 
                                  backend = backend, ...)
  return(create_mvgam_from_combined_fit(combined_fit, obs_setup, trend_setup))
}
```

**Backend Strategy**: Enhance existing mvgam approach maintaining message silencing, progress feedback, error handling, and threading options.

### Phase 2: Stan Integration (Weeks 5-8)

#### Week 5-6: Two-Stage Stan Assembly
- Extract trend stanvars from trend_setup and generate base model
- Modify observation linear predictor with missing data preservation
- Handle nonlinear model complexity (`bf(nl = TRUE)`)
- Ensure compatibility with all mvgam trend types (RW, AR, VAR, GP, CAR)

#### Week 7: Dynamic Prior System & Context-Aware Validation
- Intelligent autocorrelation validation based on formula context
- Extract trend components from brms-generated Stan code
- Apply systematic `_trend` suffix handling
- Handle multivariate response-specific parameters

#### Week 8: Higher-Order Models & Three-Level JSDGAM
- Extended AR/VAR: `AR(p = c(1, 12, 24))`, `VAR(p = 3)`
- Custom families: `nmix()`, `tweedie()`, `occ()` with three-level hierarchy
- JSDGAM specialized validation for multi-species factor models

### Phase 3: Optimization & Methods (Weeks 9-12)

#### Week 9-10: Rcpp Functions
```cpp
// Higher-order AR/VAR with embedded predictors
// [[Rcpp::export]]
Rcpp::NumericVector ar_p_recursC(Rcpp::NumericVector phi_coeffs, ...);

// Matrix-based JSDGAM prediction (10-100x speedup)
// [[Rcpp::export]]
arma::mat fast_jsdgam_predict(const arma::mat& design_matrices, ...);

// Multivariate-aware prediction functions
// [[Rcpp::export]]
arma::cube fast_multivariate_predict(const arma::cube& design_arrays, ...);
```

#### Week 11: brms Prediction Integration
```r
posterior_predict.mvgam <- function(object, newdata = NULL, resp = NULL, ...) {
  if (is_multivariate(object)) {
    if (!is.null(resp)) {
      return(predict_single_response(object, resp, newdata, ...))
    } else {
      return(predict_all_responses(object, newdata, ...))
    }
  }
  
  # Use brms::prepare_predictions() for full pipeline
  prep <- brms::prepare_predictions(object$obs_fit, newdata = newdata, ...)
  base_linpred <- brms::posterior_linpred_draws(prep)
  trend_effects <- predict_trend_effects(object, prep)
  prep <- update_brmsprep_linpred(prep, base_linpred + trend_effects)
  
  return(brms::posterior_predict_draws(prep))
}
```

#### Week 12: Method System Integration
```r
# Critical: log_lik integration following brms patterns
log_lik.mvgam <- function(object, resp = NULL, ...) {
  if (is_multivariate(object)) {
    if (!is.null(resp)) {
      return(compute_response_log_lik(object, resp, ...))
    } else {
      return(compute_multivariate_log_lik(object, ...))
    }
  }
  
  # Standard univariate approach with brms integration
  prep <- brms::prepare_predictions(object$obs_fit, ...)
  prep <- add_trend_effects_to_prep(object, prep)
  return(compute_log_lik_matrix(prep, object))
}

# Seamless loo/waic/pp_check integration with bayesplot ecosystem
```

### Phase 4: Testing & Launch (Weeks 13-16)

#### Week 13-14: Comprehensive Testing Matrix

**Stan Code Injection Safety**:
```r
test_that("Stan modification preserves brms parameter constraints", {
  # Parameter transformation preservation
  fit_constrained <- mvgam(mvbf(y ~ x, family = Gamma(link = "log")), 
                          trend_formula = ~ AR(p = 1), data = data)
  draws <- as_draws_df(fit_constrained)
  expect_true(all(draws$shape > 0))
  
  # Nonlinear parameter constraints preserved
  fit_nl <- mvgam(bf(y ~ a * exp(-b * x), a ~ 1, b ~ 1, nl = TRUE),
                  trend_formula = ~ RW(), data = nldata)
  expect_parameter_bounds_preserved(fit_nl)
})
```

**Multivariate Missing Data**:
```r
test_that("complex missing data patterns work correctly", {
  mvdata_missing$y1[c(5:10, 25:30)] <- NA  # Block missing
  mvdata_missing$y2[c(1:3, 15:20)] <- NA   # Different pattern
  
  fit_missing <- mvgam(mvbf(y1 ~ x1, y2 ~ x2),
                      trend_formula = list(y1 = ~ AR(p = 1), y2 = ~ RW()),
                      data = mvdata_missing)
  
  # Trends evolve over ALL time points, likelihood only for non-missing
  expect_equal(get_trend_length(fit_missing), nrow(mvdata_missing))
  expect_different_obs_indices(fit_missing, "y1", "y2")
})
```

**Cross-Response Correlation Preservation**:
```r
test_that("complex correlation structures preserved", {
  fit_multiple_corr <- mvgam(
    mvbf(y1 ~ x, y2 ~ x, y3 ~ x) + set_rescor(TRUE) +
      autocor(ar(p = 1, formula = ~ time | group)),
    trend_formula = list(y1 = ~ AR(p = 1), y2 = ~ RW(cor = TRUE), y3 = ~ VAR(p = 1)),
    data = complex_corr_data
  )
  
  # Should have: brms residual + observation AR + mvgam State-Space correlations
  expect_correlation_separation(fit_multiple_corr)
})
```

**Performance Validation**:
- Setup speed: 10x improvement target
- JSDGAM prediction: 100x speedup with Rcpp
- Memory usage: 30% reduction target
- Formula complexity matrix: all combinations working

#### Week 15: Performance Optimization & Object Footprint
- Memory usage optimization and object compression
- Final performance validation against targets
- Automated benchmarking integration

#### Week 16: Documentation & Release Preparation
- Updated function documentation with roxygen2
- Key vignettes: multivariate models, autocorrelation integration, migration guide
- Performance benchmarks documentation
- Community feedback integration

## Key Innovation Points

### 1. **Autocorrelation Intelligence**
First package to properly distinguish observation-level residual correlation from State-Space dynamics, enabling sophisticated multi-level temporal structures.

### 2. **Multivariate State-Space**
Native support for response-specific trends while preserving brms cross-response correlations - unprecedented capability in the ecosystem.

### 3. **Stan Extension Pattern**
Using brms `stanvars` mechanism for State-Space injection - provides scalable approach for other time series extensions.

### 4. **Method System Integration**
Dual brmsfit-like objects enabling seamless bayesplot/loo/waic compatibility while maintaining mvgam-specific functionality.

## Risk Mitigation

### Primary Implementation Risks & Solutions

1. **Stan Code Injection Complexity**
   - **Risk**: Breaking brms parameter transformations in complex models
   - **Mitigation**: AST-based modification with extensive validation, fallback to limited modification approach

2. **Multivariate Missing Data Complexity**
   - **Risk**: Cross-response missing patterns breaking trend evolution
   - **Mitigation**: Preserve proven `obs_ind` tracking approach, comprehensive testing matrix

3. **Backend Maintenance Burden**
   - **Risk**: brms internal changes breaking mvgam compatibility
   - **Mitigation**: Automated monitoring system, version compatibility matrix, graceful degradation

4. **Performance Regression**
   - **Risk**: New architecture slower than current mvgam
   - **Mitigation**: Automated benchmarking throughout development, Rcpp optimization, profiling

### Fallback Strategies
- **Limited Stan modification**: If AST parsing proves too complex, target simple linear predictors first
- **Legacy method fallback**: Graceful degradation to v1.x methods for brms incompatibilities
- **Parallel implementation**: Maintain old/new implementations during transition

## Success Criteria

### Performance Targets
- [ ] brms setup: 10-50x faster initialization
- [ ] JSDGAM prediction: 10-100x speedup (Rcpp vs R loops)
- [ ] Memory usage: 30-50% reduction
- [ ] Multivariate scaling: Linear with number of responses

### Functionality Preservation & Enhancement
- [ ] All existing mvgam features preserved
- [ ] Full brms compatibility maintained (all formula types, families, priors)
- [ ] Seamless bayesplot/loo/waic integration
- [ ] >90% test coverage achieved
- [ ] Multivariate models with response-specific trends working
- [ ] Cross-response correlations preserved in multivariate State-Space models
- [ ] Intelligent autocorrelation validation preventing conflicts

## Dependencies & Migration

**Core Dependencies**: brms (≥2.19.0), Stan (≥2.30.0), Rcpp, checkmate, insight, bayesplot

**Quality Assurance**: All PRs require maintainer review, comprehensive unit tests, automated performance benchmarking

**Migration Strategy**: 
- Target brms 2.x stable API as primary
- Add brms 3.0 compatibility when released
- Maintain backward compatibility where possible
- Clear migration guide for breaking changes

---

**Next Step**: Begin Week 1 - Trend Type Dispatcher System  
**Critical Success Factor**: Stan code modification that preserves all brms functionality while seamlessly adding State-Space dynamics  
**Key Innovation**: Leverage brms linear predictor generation + mvgam State-Space expertise + native multivariate support
