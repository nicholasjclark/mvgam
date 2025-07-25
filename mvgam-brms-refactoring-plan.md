# mvgam → brms Extension Refactoring Plan

**Version**: 1.2 (Final)  
**Date**: 2025-01-24  
**Status**: Ready for Implementation

## Executive Summary

Transform mvgam from mgcv-based standalone package into specialized brms extension adding State-Space modeling, N-mixture/occupancy models, and JSDMs. Architecture: brms generates linear predictors (`mu`, `mu_trend`), single combined Stan model, dual brmsfit-like objects for post-processing. **JSDGAM function** follows identical architecture with specialized validation for multi-species factor models.

## Core Architecture

### Single-Fit Dual-Object Strategy
- **brms for**: Linear predictor generation (`mu` + `mu_trend`), Stan code setup, internal object creation (no fitting)
- **Single fit**: Combined Stan model with observation + trend components  
- **Dual objects**: Internal brmsfit-like objects for post-processing
- **State-Space link**: Observation and trend layers connected in Stan

### Design Principles
1. **Observation layer**: Uses brms exactly as-is (no parameter renaming)
2. **Trend layer**: brms functionality with `_trend` suffixed parameters
3. **Formula-centric**: `trend_formula = ~ s(time) + RW(cor = TRUE)`
4. **Single learning curve**: Only need `brms::prior()` and `brms::set_prior()`
5. **Efficiency-first**: No predictions in Stan `generated_quantities`, on-demand computation like brms

## Development Standards

### brms 3.0 Compatibility
**Timeline Reality**: brms 3.0 is "still a long way to go" with no set release date. Our 16-week timeline likely precedes brms 3.0 release.

**Strategy**: Target brms 2.x stable API as primary, add 3.0 compatibility as future enhancement when needed.

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
  formula = y ~ s(x1) + (1|group),  # Standard linear formula
  # OR
  formula = bf(y ~ alpha * exp(beta * x), alpha ~ s(x1), beta ~ 1, nl = TRUE),  # Nonlinear bf()
  
  trend_formula = ~ s(time) + cov1 + RW(cor = TRUE),  # Standard trend formula  
  # OR  
  trend_formula = bf(~ gamma * exp(delta * time) + AR(p = 2), gamma ~ 1, delta ~ 1, nl = TRUE),  # Nonlinear trend
  
  priors = c(...),          # Standard brms (unchanged)
  trend_priors = c(...),    # Auto-renamed with _trend suffix
  data = data, data2 = data2, ...
)
```

**Formula Complexity Handling**: Support bf() calls in both formula and trend_formula:
- **Standard formulas**: Direct brms processing, straightforward trend integration
- **Nonlinear bf() formulas**: Complex Stan code parsing, multiple linear predictor identification
- **Mixed scenarios**: Linear observation + nonlinear trend, or vice versa
- **Full mvgam compatibility**: All trend types (RW, AR, VAR, GP, etc.) and factor models work with any formula type

#### Week 3: brms Setup Optimization
Benchmark and implement fastest brms setup method (`backend = "mock"` vs `chains = 0`).

#### Week 4: Single-Fit Architecture & Backend Strategy
```r
mvgam <- function(formula, trend_formula = NULL, backend = NULL, ...) {
  # 1. Setup obs/trend models (no fitting)
  obs_setup <- setup_brms_lightweight(formula, data, ...)
  trend_setup <- setup_brms_lightweight(trend_spec$base_formula, ...)
  
  # 2. Extract trend stanvars from trend_setup's generated Stan code
  trend_stanvars <- extract_trend_stanvars_from_setup(trend_setup, trend_spec)
  
  # 3. Generate combined Stan code using trend stanvars
  combined_stancode <- generate_combined_stancode(obs_setup, trend_stanvars)
  
  # 4. Use enhanced mvgam backend system (adapted from brms)
  combined_fit <- fit_mvgam_model(combined_stancode, combined_standata, 
                                  backend = backend, silent = silent, ...)
  return(create_mvgam_from_combined_fit(combined_fit, obs_setup, trend_setup))
}

# Current approach: Enhance existing mvgam backend system
fit_mvgam_model <- function(stancode, standata, backend = NULL, algorithm = "sampling",
                           iter = 2000, warmup = 1000, chains = 4, cores = 1,
                           threads = NULL, silent = 1, control = list(), ...) {
  
  # Use existing mvgam backend approach (adapted from brms internals)
  # This preserves important features like:
  # - Message silencing (silent parameter)
  # - Progress control and user feedback
  # - Error handling and diagnostics
  # - Threading and parallelization options
  # - Algorithm flexibility (sampling, vb, fixed_param)
  
  backend <- backend %||% getOption("brms.backend", "rstan")
  
  if (backend == "rstan") {
    fitted_model <- mvgam_fit_rstan(stancode, standata, algorithm, iter, warmup,
                                   chains, cores, silent, control, ...)
  } else if (backend == "cmdstanr") {
    fitted_model <- mvgam_fit_cmdstanr(stancode, standata, algorithm, iter, warmup,
                                      chains, cores, threads, silent, control, ...)
  }
  
  return(fitted_model)
}
```

**Backend Strategy**: Maintain and enhance current mvgam approach:
- **Keep copied brms internals**: Provides essential user experience features
- **Message silencing**: `silent` parameter for clean output control
- **Progress feedback**: User-friendly progress messages and warnings
- **Error handling**: Robust diagnostics and helpful error messages  
- **Full feature parity**: All brms backend capabilities preserved
- **Update synchronization**: Periodically sync with latest brms backend improvements

### Phase 2: Stan Generation (Weeks 5-8)

#### Week 5-6: Two-Stage Stan Assembly

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

**Nonlinear Model Complexity**: brms nonlinear models (`bf(nl = TRUE)`) introduce significant Stan code complexity:
- Custom functions and parameter declarations beyond standard linear predictors
- Nonlinear parameters as placeholders for linear predictors: `alpha ~ 1 + (1|group)`
- Complex prediction pathways requiring specialized trend integration
- **Critical requirement**: Must ensure trend injection works with any bf() complexity while preserving all mvgam trend types (RW, AR, VAR, GP, CAR, etc.) and factor model capabilities

**Benefits**: Uses brms's designed extension mechanism (`stanvars`), targeted modification only.

#### Week 7: Dynamic Prior System & Complex Formula Handling
```r
# Extract trend components from brms-generated Stan code
extract_trend_stanvars_from_setup <- function(trend_setup, trend_spec) {
  # Apply systematic _trend suffix: b → b_trend, Intercept → Intercept_trend
  # Handle nonlinear parameters: nlpar renaming for complex bf() formulas
  # Create stanvars for injection: parameters, transformed parameters, model blocks
  # Monitor trend matrix only - no predictions in generated_quantities for efficiency
}

# Parse and validate complex formula combinations
parse_formula_complexity <- function(formula, trend_formula) {
  # Detect bf() calls in either formula or trend_formula
  # Extract nonlinear parameters and their linear predictors
  # Validate mvgam trend compatibility with nonlinear specifications
  # Plan integration strategy based on formula complexity
}

# Handle missing data following current mvgam pattern
prepare_missing_data_for_brms <- function(data, formula, trend_formula) {
  # Replace NAs with imputed values, create obs_ind tracking non-missing
  # Ensure brms computes mu/mu_trend for full time grid (works with bf() formulas)
  # Pass obs_ind to Stan for selective likelihood computation
}
```

**Stan Integration Pattern**:
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

#### Week 8: Higher-Order Models & Three-Level JSDGAM
```r
# Extended AR/VAR: AR(p = c(1, 12, 24)), VAR(p = 3)
# Custom families: nmix(), tweedie(), occ() with three-level hierarchical structure

jsdgam <- function(formula, data, data2 = NULL, lv = 0, family = gaussian(), 
                   trend_formula = NULL, latent_formula = NULL, ...) {
  # Three-level hierarchy for occ()/nmix() families
  if (family$family %in% c("occ", "nmix")) {
    # Level 1: Environmental factors → Species abundance/occupancy
    latent_setup <- setup_brms_lightweight(latent_formula, data, data2, ...)
    # Level 2: Species trends + Level 3: Observation process
    # Generate three-level stancode
  } else {
    # Standard two-level jsdgam (same as mvgam pattern)
  }
}
```

### Phase 3: Optimization (Weeks 9-12)

#### Week 9-10: Rcpp Functions Following Carpenter's Efficiency Guidelines
```cpp
// Vectorized operations, minimal indexing, pre-compute repeated calculations
// Higher-order AR/VAR with embedded predictors
// [[Rcpp::export]]
Rcpp::NumericVector ar_p_recursC(Rcpp::NumericVector phi_coeffs, ...);

// Matrix-based JSDGAM prediction (replace R loops for 10-100x speedup)
// [[Rcpp::export]]
arma::mat fast_jsdgam_predict(const arma::mat& design_matrices, ...);

// Forecasting from monitored trend states (no Stan generated_quantities)
// [[Rcpp::export]]
arma::mat forecast_from_final_states(const arma::mat& final_states, int h, ...);
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

# Forecasting extends from monitored trend states
forecast.mvgam <- function(object, h = NULL, ...) {
  final_states <- extract_final_trend_states(object)
  forecast_trends <- forecast_trend_from_states_rcpp(final_states, h, ...)
  return(combine_trend_obs_forecasts(object, forecast_trends, ...))
}
```

#### Week 12: Method System Integration
```r
# Full brms method support using dual brmsfit-like objects
summary.mvgam <- function(object, ...) {
  # Use dual objects for comprehensive summaries
}

# Critical: log_lik integration following brms patterns
log_lik.mvgam <- function(object, newdata = NULL, re_formula = NULL, 
                          resp = NULL, ndraws = NULL, draw_ids = NULL,
                          pointwise = FALSE, combine = TRUE, cores = NULL, ...) {
  # Step 1: Use brms::prepare_predictions() for base setup
  prep <- brms::prepare_predictions(object$obs_fit, newdata = newdata, 
                                   re_formula = re_formula, resp = resp,
                                   ndraws = ndraws, draw_ids = draw_ids, ...)
  
  # Step 2: Add trend effects to linear predictor
  prep <- add_trend_effects_to_prep(object, prep)
  
  # Step 3: Compute log-likelihood using family-specific functions
  if (pointwise) {
    # Return function for loo/waic compatibility
    return(create_pointwise_log_lik_function(prep, object))
  } else {
    # Return S x N matrix of log-likelihood draws
    return(compute_log_lik_matrix(prep, object, combine, cores))
  }
}

# Seamless loo integration 
loo.mvgam <- function(x, ..., compare = TRUE, resp = NULL, pointwise = FALSE,
                      moment_match = FALSE, reloo = FALSE, save_psis = FALSE) {
  # Leverage brms loo method with mvgam log_lik
  log_lik_matrix <- log_lik(x, pointwise = pointwise, ...)
  
  if (pointwise) {
    # Use function method for efficiency
    return(loo::loo(log_lik_matrix, ...))
  } else {
    # Use matrix method 
    return(loo::loo(log_lik_matrix, save_psis = save_psis, ...))
  }
}

# Additional model evaluation methods
waic.mvgam <- function(x, ...) {
  log_lik_matrix <- log_lik(x, ...)
  return(loo::waic(log_lik_matrix, ...))
}

# Posterior predictive checks using bayesplot
pp_check.mvgam <- function(object, type = "dens_overlay", ndraws = NULL,
                           prefix = c("ppc", "ppd"), group = NULL, x = NULL,
                           newdata = NULL, resp = NULL, draw_ids = NULL, ...) {
  # Validate plot type (leverages bayesplot error messages for invalid types)
  valid_types <- bayesplot::available_ppc(pattern = "")
  type <- match.arg(type, valid_types)
  prefix <- match.arg(prefix)
  
  # Get observed data from fitted object
  y <- get_y(object, resp = resp)
  
  # Generate posterior predictive draws using mvgam prediction methods
  yrep <- posterior_predict(object, newdata = newdata, resp = resp, 
                           ndraws = ndraws, draw_ids = draw_ids, ...)
  
  # Get bayesplot ppc function
  ppc_fun <- get(paste0(prefix, "_", type), asNamespace("bayesplot"))
  
  # Handle grouped plots
  if ("group" %in% names(formals(ppc_fun))) {
    if (is.null(group)) {
      stop2("Argument 'group' is required for ppc type '", type, "'.")
    }
    group_var <- get_data(object)[[group]]
    return(ppc_fun(y, yrep, group = group_var, ...))
  }
  
  # Handle plots with x variable
  if ("x" %in% names(formals(ppc_fun)) && !is.null(x)) {
    x_var <- get_data(object)[[x]]
    return(ppc_fun(y, yrep, x = x_var, ...))
  }
  
  # Standard ppc plot
  return(ppc_fun(y, yrep, ...))
}
```

### Phase 4: Testing & Launch (Weeks 13-16)

#### Week 13-14: Comprehensive Testing
- **Formula complexity matrix**: Test all combinations of linear/nonlinear observation and trend formulas
  - Linear obs + Linear trend: Standard mvgam functionality
  - Linear obs + Nonlinear trend: `bf(~ gamma * exp(delta * time) + RW(), gamma ~ 1, delta ~ 1, nl = TRUE)`
  - Nonlinear obs + Linear trend: `bf(y ~ alpha * exp(beta * x), alpha ~ s(x), beta ~ 1, nl = TRUE)` + standard trends
  - Nonlinear obs + Nonlinear trend: Complex bf() in both formulas
- **All mvgam features preserved**: RW, AR, VAR, GP, CAR, factor models, three-level JSDGAM
- **Missing data + complexity**: obs_ind tracking with bf() formulas
- **Model evaluation integration**: log_lik, loo, waic, pp_check methods work seamlessly with brms/bayesplot ecosystem
- **Performance benchmarking** across complexity combinations

#### Week 15: Performance Optimization
Object footprint reduction, memory usage optimization, final performance validation.

#### Week 16: Documentation & Release
Complete documentation updates, migration guide from v1.x to v2.0, community feedback integration.

## Key Benefits

### Performance Targets
- **brms Setup**: 10-50x faster initialization
- **JSDGAM Prediction**: 10-100x speedup (Rcpp vs R loops)
- **Memory Usage**: 30-50% reduction
- **Stan Efficiency**: No generated_quantities predictions, on-demand computation

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

## Risk Mitigation

### Primary Risks & Solutions
1. **Stan Integration Complexity** → Extensive testing, fallback patterns
2. **Performance Regression** → Profile bottlenecks, parallel implementation
3. **Missing Data Handling** → Preserve proven mvgam approach with obs_ind tracking
4. **brms API Changes** → Version detection, compatibility testing when 3.0 approaches
5. **Nonlinear Model Integration** → Complex Stan code modification, specialized testing for `bf(nl = TRUE)` models
6. **Backend Maintenance** → Periodic synchronization with brms backend updates, maintain feature parity

### Contingency Plans
- Maintain parallel old/new implementations during transition
- **Backend synchronization**: Establish process for updating mvgam backends when brms internals evolve
- Comprehensive test suite covering edge cases
- Performance monitoring and automated benchmarks

## Success Metrics

- [ ] All existing functionality preserved
- [ ] Performance targets achieved  
- [ ] Memory usage reduced by 30-50%
- [ ] Simplified user interface working
- [ ] Full brms compatibility maintained
- [ ] >90% test coverage achieved
- [ ] **Model evaluation methods** (log_lik, loo, waic, pp_check) work seamlessly with brms/bayesplot ecosystem

## Migration Strategy

**Primary Development (Weeks 1-16)**: Target brms 2.x stable API  
**Future Enhancement**: Add brms 3.0 compatibility when released  
**Quality Assurance**: All PRs require maintainer review, unit tests with >90% coverage

**Dependencies**: brms (≥2.19.0), Stan (≥2.30.0), Rcpp, checkmate, insight

---

**Next Step**: Begin Week 1 - Trend Type Dispatcher System  
**Key Innovation**: Leverage brms linear predictor generation + mvgam State-Space expertise
