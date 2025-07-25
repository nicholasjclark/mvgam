# mvgam → brms Extension Refactoring Plan

**Version**: 1.9  
**Date**: 2025-01-25  
**Status**: Implementation Ready

## Executive Summary

Transform mvgam from mgcv-based package into specialized brms extension adding State-Space modeling, N-mixture/occupancy models, and JSDMs. **Core Innovation**: brms generates linear predictors (`mu`, `mu_trend`), single combined Stan model, dual brmsfit-like objects for post-processing. **New Features**: Native multiple imputation with Rubin's rules pooling and enhanced leave-future-out cross-validation.

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

# FORBIDDEN: Trend-level brms autocor
mvgam(
  y ~ s(x1),
  trend_formula = ~ s(time) + ar(p = 1),  # ❌ Conflicts with mvgam AR()
  data = data
)
```

### 3. Stan Code Strategy: Two-Stage Assembly

**Stage 1**: Extract trend stanvars from brms setup
```r
trend_stanvars <- extract_trend_stanvars_from_setup(trend_setup, trend_spec)
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

```r
lfo_cv <- function(object, K = NULL, fc_horizon = 1, 
                   eval_timepoints = NULL, compare_null = TRUE,
                   n_cores = 1, silent = TRUE, ...) {
  
  # Support multiple imputation and multivariate objects
  if (inherits(object, "mvgam_pooled")) {
    return(lfo_cv_multiple(object, K, fc_horizon, eval_timepoints, ...))
  }
  
  if (is_multivariate(object)) {
    return(lfo_cv_multivariate(object, K, fc_horizon, eval_timepoints, ...))
  }
  
  # Core implementation with parallel support
  timepoints <- extract_timepoints(object)
  eval_points <- determine_eval_timepoints(timepoints, K, eval_timepoints)
  splits <- create_lfo_splits(object, eval_points, fc_horizon)
  
  lfo_results <- if (n_cores > 1) {
    parallel_lfo_evaluation(splits, n_cores, silent, ...)
  } else {
    sequential_lfo_evaluation(splits, silent, ...)
  }
  
  metrics <- compute_lfo_metrics(lfo_results, compare_null)
  
  return(structure(list(
    forecasts = lfo_results$forecasts,
    observations = lfo_results$observations,
    elpd_estimates = metrics$elpd,
    forecast_metrics = metrics$forecasts,
    timepoints = eval_points,
    fc_horizon = fc_horizon,
    model_call = object$call
  ), class = c("mvgam_lfo", "lfo")))
}
```

**Key Features**:
- Time-aware evaluation preserving temporal ordering
- Distributional forecast assessment (ELPD, RMSE, MAE, coverage, CRPS, energy scores)
- Multiple imputation pooling support
- Multivariate evaluation (joint and response-specific)
- Parallel computation for large evaluations

### 5. Validation Framework

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

**Key Validation Areas**:
- Distributional model restriction: Trends only for main response parameter
- Response helper compatibility: `mi()`, `weights()`, `cens()`, `trunc()`, `trials()`
- Multiple imputation: Dataset structure consistency, pooling compatibility
- Hurdle/zero-inflated models: Trends restricted to main (`mu`) parameter
- LFO-CV: Time series structure, forecast horizon limits, evaluation timepoint validity

## Implementation Timeline (16 Weeks)

### Phase 1: Foundation (Weeks 1-4)

#### Week 1: Trend Dispatcher System
Registry-based trend constructor system enabling `RW()`, `AR()`, `VAR()`, `GP()`, `CAR()`

#### Week 2: Formula Integration & Autocorrelation Validation
- Multivariate formula parsing with `mvbf()` support
- Context-aware autocorrelation validation preventing conflicts

#### Week 3: brms Setup Optimization
- Benchmark fastest brms setup method (`backend = "mock"` vs `chains = 0`)
- Target 10-50x setup speed improvement

#### Week 4: Single-Fit Architecture & Multiple Imputation
```r
mvgam <- function(formula, trend_formula = NULL, data = NULL, backend = NULL, 
                  combine = TRUE, ...) {
  
  # Handle multiple imputation input
  if (is.list(data) && !is.data.frame(data)) {
    if (combine) {
      return(mvgam_multiple(formula, trend_formula, data, backend, ...))
    } else {
      return(map(data, ~ mvgam(formula, trend_formula, .x, backend, ...)))
    }
  }
  
  # Parse multivariate formulas, setup models, extract stanvars, fit
  mv_spec <- parse_multivariate_trends(formula, trend_formula)
  obs_setup <- setup_brms_lightweight(formula, data, ...)
  trend_setup <- setup_brms_lightweight(trend_spec$base_formula, ...)
  trend_stanvars <- extract_trend_stanvars_from_setup(trend_setup, trend_spec)
  combined_stancode <- generate_combined_stancode(obs_setup, trend_stanvars)
  combined_fit <- fit_mvgam_model(combined_stancode, combined_standata, 
                                  backend = backend, ...)
  return(create_mvgam_from_combined_fit(combined_fit, obs_setup, trend_setup))
}
```

### Phase 2: Stan Integration (Weeks 5-8)

#### Week 5-6: Two-Stage Stan Assembly
- Extract trend stanvars from trend_setup
- Modify observation linear predictor with missing data preservation
- Handle nonlinear model complexity (`bf(nl = TRUE)`)

#### Week 7: Validation & Hurdle Model Support
- Intelligent autocorrelation validation based on formula context
- Hurdle model validation: Trends only for main (`mu`) parameter
- Parameter name recognition: `b_hu_*` vs `b_*` handling

#### Week 8: Higher-Order Models & Custom Families
- Extended AR/VAR: `AR(p = c(1, 12, 24))`, `VAR(p = 3)`
- Custom families: `tweedie()`, `nmix()`, `occ()` with three-level hierarchy

### Phase 3: Optimization & Methods (Weeks 9-12)

#### Week 9-10: Rcpp Functions & LFO-CV Implementation
```cpp
// Higher-order AR/VAR with embedded predictors
// [[Rcpp::export]]
Rcpp::NumericVector ar_p_recursC(Rcpp::NumericVector phi_coeffs, ...);

// Matrix-based JSDGAM prediction (10-100x speedup)
// [[Rcpp::export]]
arma::mat fast_jsdgam_predict(const arma::mat& design_matrices, ...);

// LFO-CV optimization functions
// [[Rcpp::export]]
arma::mat fast_lfo_metrics(const arma::cube& forecasts, const arma::mat& observations);
```

Core LFO-CV implementation with parallel support, multiple imputation pooling, and comprehensive metrics.

#### Week 11: brms Prediction Integration & Multiple Imputation Pooling
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

#### Week 12: Method System Integration
Critical methods: `log_lik()`, `update()`, `print()`, `loo()` with multiple imputation support

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

1. **Autocorrelation Intelligence**: First package to properly distinguish observation-level residual correlation from State-Space dynamics
2. **Multivariate State-Space**: Response-specific trends while preserving brms cross-response correlations
3. **Multiple Imputation Integration**: Seamless support with Rubin's rules pooling
4. **Enhanced Time Series Cross-Validation**: Comprehensive LFO-CV with distributional evaluation
5. **Stan Extension Pattern**: Using brms `stanvars` for State-Space injection
6. **Method System Integration**: Dual brmsfit-like objects enabling seamless brms ecosystem compatibility

## Success Criteria

### Performance Targets
- [ ] brms setup: 10-50x faster initialization
- [ ] JSDGAM prediction: 10-100x speedup (Rcpp vs R loops)
- [ ] LFO-CV evaluation: 10x speedup for large datasets
- [ ] Memory usage: 30-50% reduction

### Functionality Requirements
- [ ] All existing mvgam features preserved
- [ ] Full brms compatibility: formulas/families/priors/response helpers
- [ ] Multiple imputation: Native support with Rubin's rules pooling
- [ ] Enhanced LFO-CV: Time series evaluation with comprehensive metrics
- [ ] Seamless brms ecosystem integration: loo/waic/pp_check/diagnostics
- [ ] >90% test coverage
- [ ] Multivariate models with response-specific trends
- [ ] Intelligent autocorrelation validation

## Dependencies & Migration

**Core Dependencies**: brms (≥2.19.0), Stan (≥2.30.0), Rcpp, checkmate, insight, bayesplot

**Migration Strategy**: 
- Target brms 2.x stable API as primary
- Add brms 3.0 compatibility when released
- Maintain backward compatibility where possible
- Clear migration guide for breaking changes

---

**Next Step**: Begin Week 1 - Trend Type Dispatcher System  
**Critical Success Factor**: Stan code modification preserving all brms functionality while seamlessly adding State-Space dynamics, multiple imputation support, and enhanced LFO-CV
