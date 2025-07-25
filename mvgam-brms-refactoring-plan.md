# mvgam → brms Extension Refactoring Plan

**Version**: 1.0  
**Date**: 2025-01-24  
**Status**: Design Complete - Ready for Implementation

## Executive Summary

Transform mvgam from an mgcv-based standalone package into a specialized brms extension that adds State-Space modeling, N-mixture/occupancy models, and Joint Species Distribution Models (JSDMs) to brms's foundation. The architecture uses brms for setup and internal object creation, fits a single combined Stan model, and maintains dual brmsfit-like objects internally for post-processing.

## Core Architecture

### Single-Fit Dual-Object Strategy
- **brms used for**: Setup, Stan code generation, internal object creation (no fitting)
- **Single fitting**: Combined Stan model containing both observation and trend components  
- **Dual objects**: Maintain brmsfit-like objects internally for post-processing
- **State-Space connection**: Links observation and trend layers in Stan code

### Key Design Principles
1. **Observation layer**: Uses brms exactly as-is (no parameter renaming)
2. **Trend layer**: Uses brms functionality but with `_trend` suffixed parameter names
3. **Formula-centric**: Trend models specified in trend_formula (e.g., `~ s(time) + RW(cor = TRUE)`)
4. **Single learning curve**: Users only need to know `brms::prior()` and `brms::set_prior()` to change priors

## Development Standards and Validation

### Validation and Error Handling Standards

All mvgam functions must follow these validation patterns:

#### Required Packages
- `checkmate`: Parameter validation and type checking
- `insight`: Error and warning message formatting  
- `rlang`: Session-wide warnings and advanced error handling

#### Validation Patterns
1. **Input Validation**: Use `checkmate::assert_*()` for all function parameters
2. **Error Messages**: Use `insight::format_error()` for user-friendly error formatting
3. **Warnings**: Use `insight::format_warning()` for informative warnings
4. **Session Warnings**: Use `rlang::warn(..., .frequency = "once")` for one-time warnings

#### Message Formatting Standards
- Use `{.field parameter_name}` for parameter highlighting
- Include suggested solutions in error messages
- Provide context about why constraints exist
- Use consistent terminology across the package

#### Dynamic Factor Model Constraints
Special validation for identifiability constraints:
- `n_lv > 0` cannot be combined with `gr != 'NA'` or `subgr != 'series'`
- `n_lv > 0` cannot be combined with `ma = TRUE`
- Only certain trend types support `n_lv > 0` (checked via `characteristics$supports_factors`)
- Variance parameters fixed for dynamic factor models (one-time warning)

#### Implementation Throughout Features
- **Week 1-2**: Implement in trend constructors and validation
- **Week 3-4**: Apply to brms setup functions  
- **Week 5-8**: Use in Stan code generation and prior system
- **Week 9-12**: Apply to prediction/forecasting functions
- **Week 13-16**: Comprehensive validation testing

### Code Style Standards

#### R Code Formatting
- **Line length**: Maximum 80 characters per line
- **Roxygen indentation**: Use indents for continuation lines in documentation
- **Comment style**: Use sentence case, avoid ALL CAPS in comments
- **Function names**: Use snake_case following tidyverse conventions
- **Consistency**: Follow existing package conventions where established

#### Documentation Standards
- Keep roxygen2 lines under 80 characters
- Indent continuation lines with 2 spaces
- Use descriptive parameter descriptions
- Include examples for exported functions
- Apply tidyverse styling throughout

## Implementation Timeline

### Phase 1: Foundation and Dispatching System (Weeks 1-4)

#### Week 1: Trend Type Dispatcher System
**Files**: `R/trend_dispatcher.R` (new), `R/mvgam_trend_types.R` (extend)

```r
# Higher-level trend dispatcher for easy extension
trend <- function(type = "RW", ...) {
  constructor <- get_trend_constructor(type)
  do.call(constructor, list(...))
}

# Registry of trend constructors
get_trend_constructor <- function(type) {
  trend_registry <- list(
    "RW" = RW, "AR" = AR, "VAR" = VAR, "GP" = GP, "CAR" = CAR, 
    "PW" = PW, "ZMVN" = ZMVN
    # Easy to add new types
  )
  return(trend_registry[[type]])
}

# Enhanced constructors with formula integration
RW <- function(cor = FALSE, ma = FALSE, gr = 'NA', subgr = 'series', ...) {
  structure(list(
    trend_type = "RW", cor = cor, ma = ma, gr = gr, subgr = subgr,
    formula_term = TRUE
  ), class = c("mvgam_trend_term", "RW"))
}
```

#### Week 2: Formula-Based Trend Specification
**Files**: `R/mvgam_formula_parsing.R` (new), `R/mvgam.R` (major refactor)

```r
# New intuitive approach - everything in the formula
mvgam(
  formula = y ~ s(x1) + (1|group),
  trend_formula = ~ s(time) + cov1 + RW(cor = TRUE),  # Trend model in formula
  priors = c(...),          # Standard brms priors (unchanged)
  trend_priors = c(...),    # Standard brms syntax, automatically renamed
  data = data,
  data2 = data2,
  ...
)

# Parse trend_formula to extract trend components
# trend constructors will also include an option for 'lv' to trigger dynamic factor models
parse_trend_formula <- function(trend_formula) {
  # Extract trend components (RW(), AR(), etc.) from regular predictors
  # Return base_formula + trend_components + trend_model
}
```

#### Week 3: brms Initialization Optimization
**Files**: `R/brms_setup_benchmark.R` (new), `R/mvgam_brms_setup.R` (new)

```r
# Test fastest brms setup method
benchmark_brms_setup <- function(formula, data, ...) {
  # Method 1: backend = "mock" (current gp.R pattern at line 97)
  # Method 2: chains = 0
  # Method 3: dry_run = TRUE (if available)
}

# Optimized setup using fastest method
setup_brms_lightweight <- function(formula, data, data2 = NULL, ...) {
  brms_setup <- brms::brm(
    formula = formula, data = data, data2 = data2,
    mock_fit = 1, backend = "mock", rename = FALSE, ...
  )
  return(trim_brms_setup(brms_setup))
}
```

#### Week 4: Single-Fit Architecture Implementation
**Files**: `R/mvgam_brms_setup.R` (extend), `R/mvgam.R` (continue refactor)

```r
mvgam <- function(formula, trend_formula = NULL, data, data2 = NULL,
                  priors = NULL, trend_priors = NULL,
                  family = gaussian(), ...) {
  
  # Step 1: Setup observation model (no fitting but ensure data are arranged by index..time..index as in current validate_series_time.R)
  obs_setup <- setup_brms_lightweight(formula, data, data2, family, prior = priors)
  
  # Step 2: Setup trend model if needed (no fitting)
  trend_setup <- NULL
  if (!is.null(trend_formula)) {
    trend_spec <- parse_trend_formula(trend_formula)
    renamed_trend_priors <- rename_trend_priors_dynamic(trend_priors)
    trend_setup <- setup_brms_lightweight(
      trend_spec$base_formula, prepare_trend_data(data, trend_spec),
      data2, prior = renamed_trend_priors
    )
  }
  
  # Step 3: Generate combined Stan code
  combined_stancode <- generate_combined_stancode(obs_setup, trend_setup, trend_spec)
  
  # Step 4: Fit single combined model
  combined_fit <- fit_combined_stan_model(combined_stancode, combined_standata)
  
  # Step 5: Create mvgam object with dual brmsfit-like objects
  return(create_mvgam_from_combined_fit(combined_fit, obs_setup, trend_setup))
}
```

### Phase 2: Stan Code Generation and Prior System (Weeks 5-8)

#### Week 5: glue-Based Stan Code Generation
**Files**: `R/stan_glue_templates.R` (new), `R/mvgam_stancode.R` (new)

#### Core brms Integration Strategy

**Public API Approach**: Use only public brms functions to avoid dependency on internal APIs:
- Generate complete Stan code using `brms::stancode()` for both observation and trend components
- Extract Stan code blocks using robust regex patterns that target stable Stan syntax
- Apply systematic parameter renaming to trend components  
- Combine components using glue templates for clean assembly

**Key Integration Pattern**:
```r
# Generate complete brms Stan code for both components
obs_stancode <- brms::stancode(obs_formula, obs_data, obs_family, obs_priors)
trend_stancode <- brms::stancode(trend_formula, trend_data, trend_family, trend_priors)

# Extract components, rename trend parameters, combine with State-Space dynamics
combined_stancode <- merge_stan_components(obs_stancode, trend_stancode, statespace_dynamics)
```

**Function Deduplication**: Ensure `combine_functions_safely()` prevents duplicate function definitions:
- Parse and deduplicate `#include` statements
- Extract individual function definitions and check for name conflicts
- Merge identical functions, warn about conflicting implementations
- Reassemble with proper Stan formatting

```r
# Generate Stan code using brms-style named list returns
# Following stan_prior() patterns: return $par, $tpar_def, $model_prior blocks
generate_trend_parameters <- function(trend_model, n_series, n_lv, max_lag = 1, ...) {
  # Return named list with Stan code blocks (following brms convention)
  switch(trend_model,
    "RW" = list(
      par = glue::glue("vector<lower=0>[{n_series}] sigma_trend;"),
      tpar_def = glue::glue("matrix[T,{n_series}] trend_raw;"),
      model_prior = "" # Will be populated by stan_trend_prior()
    ),
    "AR" = list(
      par = glue::glue("
        vector<lower=-1,upper=1>[{n_series}] phi_ar;
        vector<lower=0>[{n_series}] sigma_trend;
      "),
      tpar_def = glue::glue("matrix[T,{n_series}] trend_raw;"),
      model_prior = ""
    ),
    "VAR" = list(
      par = glue::glue("
        matrix<lower=-1,upper=1>[{n_series},{n_series * max_lag}] phi_var;
        vector<lower=0>[{n_series}] sigma_trend;
      "),
      tpar_def = glue::glue("matrix[T,{n_series}] trend_raw;"),
      model_prior = ""
    )
  )
}

# State-Space dynamics with embedded trend predictors
# all trend predictors will need to be able to work as dynamic factors if 'lv' is used in trend
# constructors
generate_rw_dynamics <- function(has_predictors = FALSE, n_series, ...) {
  if (has_predictors) {
    glue::glue("
      // Random Walk with trend predictors embedded in dynamics
      for (s in 1:{n_series}) {{
        trend_raw[s][1] ~ normal(X_trend[1, s] * b_trend[s], sigma_trend[s]);
        for (t in 2:T) {{
          trend_raw[s][t] ~ normal(trend_raw[s][t-1] + X_trend[t, s] * b_trend[s], sigma_trend[s]);
        }}
      }}
    ")
  } else {
    # Standard dynamics without predictors
  }
}
```

#### Week 6: Dynamic Prior System
**Files**: `R/mvgam_priors_modern.R` (new), `R/get_mvgam_priors.R` (refactor)

```r
# Dual prior arguments with dynamic renaming
mvgam(
  priors = c(
    brms::set_prior("normal(0, 1)", class = "Intercept"),  # → Intercept (unchanged)
    brms::set_prior("normal(0, 0.5)", class = "b")         # → b (unchanged)
  ),
  trend_priors = c(
    brms::set_prior("normal(0, 1)", class = "Intercept"),  # → Intercept_trend
    brms::set_prior("normal(0, 0.5)", class = "b"),       # → b_trend
    brms::set_prior("gamma(1, 1)", class = "sds")         # → sds_trend
  )
)

# Dynamic parameter renaming following brms patterns
rename_trend_priors_dynamic <- function(trend_priors) {
  # Apply systematic _trend suffix to all trend parameters
  # Handle special cases that shouldn't get suffix
  # Follow brms rename_pars.R patterns for extensibility
}

# Dynamic Stan parameter renaming
rename_trend_parameters_in_stan_dynamic <- function(stan_code) {
  # Get current brms parameter patterns dynamically
  # Apply systematic renaming: b → b_trend, Intercept → Intercept_trend, etc.
  # Context-aware renaming to avoid false positives
}
```

#### Following brms Prior Architecture

The trend prior system should mirror brms's `stan_prior()` patterns:

```r
# Following brms stan_prior() signature for trend parameters
stan_trend_prior <- function(prior, class, coef = NULL, group = NULL,
                           type = "real", suffix = "_trend", px = list(), ...) {
  # Apply systematic _trend suffix to all parameter names
  # Use brms's bound handling: stan_type_add_bounds()
  # Generate both parameter declarations and prior statements
  # Handle vectorized vs individual coefficient priors
}

# Integration with existing brms prior objects
rename_trend_priors_dynamic <- function(trend_priors) {
  # Extract class/coef/group from brms prior objects
  # Apply _trend suffix while preserving brms structure
  # Return modified prior objects for stan_trend_prior()
}
```

### Systematic Parameter Renaming

Apply `_trend` suffix to all trend parameters using stable brms patterns:
- Target fundamental brms parameter names: `b`, `Intercept`, `sigma`, `sd_`, `z_`, `r_`
- Use word boundary regex patterns to avoid partial matches
- Validate renaming success to ensure no conflicts remain
- Handle both parameter declarations and usage consistently

#### Week 7: data2 Integration for Factor Loadings
**Files**: `R/mvgam_factor_loadings.R` (new), `R/jsdgam.R` (refactor)

#### brms Parameter Naming Conventions

Following brms systematic naming (from `stan_predictor.R` patterns):
- **Base parameters**: `b`, `Intercept`, `sd`, `sigma`
- **Suffixed parameters**: `b_1`, `Intercept_mu2`, `sd_3` 
- **Group parameters**: Parameters get group identifiers: `sd_group_1`
- **Prefixed parameters**: Special terms get prefixes: `bs` (splines), `ar` (autocorr)

For mvgam trend parameters, apply systematic `_trend` suffix:
```r
# Original brms parameter → Trend equivalent
"b" → "b_trend"
"Intercept" → "Intercept_trend"  
"sd" → "sd_trend"
"sigma" → "sigma_trend"
```

```r
jsdgam <- function(formula, data, data2 = NULL, lv = 0, family = gaussian(), ...) {
  
  # Process data2 for factor loading constraints/priors
  factor_constraints <- process_factor_data2(data2, lv, data)
  
  # Use data2 in both brms setups
  obs_setup <- setup_brms_lightweight(formula, data, data2, family)
  lv_setup <- setup_brms_lightweight(lv_formula, lv_data, data2)
  
  # Generate Stan code with data2 constraints
  combined_code <- generate_jsdgam_stancode(obs_setup, lv_setup, factor_constraints)
}

# Support different constraint types from data2
process_factor_data2 <- function(data2, lv, data) {
  constraints <- list()
  if ("loading_matrix" %in% names(data2)) {
    constraints$constraint_type <- "fixed_structure"
  }
  if ("species_groups" %in% names(data2)) {
    constraints$constraint_type <- "grouped_loadings" 
  }
  if ("phylogeny" %in% names(data2)) {
    constraints$constraint_type <- "phylogenetic"
  }
  return(constraints)
}
```

#### Week 8: Higher-Order VAR/AR Extensions
**Files**: `R/mvgam_trend_types.R` (extend), `R/stan_trend_higher_order.R` (new)

#### Stan Block Merging Strategy

**Block Extraction**: Parse Stan code into standard blocks (functions, data, parameters, model, etc.) using robust regex patterns that target Stan syntax structure rather than brms internals.

**Component Integration**: 
- **Functions**: Deduplicate includes and custom functions
- **Data/Parameters**: Simple concatenation with clear commenting
- **Model**: Combine priors, insert State-Space dynamics, integrate trends into observation linear predictor
- **Assembly**: Use glue templates for clean, readable Stan program construction

```r
# Extended constructors supporting higher orders
AR <- function(p = 1, ma = FALSE, cor = FALSE, gr = 'NA', subgr = 'series', n_lv = NULL, ...) {
  if (p < 1 || p > 10) stop("AR order must be between 1 and 10")
  structure(list(trend_model = "AR", p = p, ma = ma, cor = cor, gr = gr, subgr = subgr, n_lv),
           class = c("mvgam_trend_term", "AR"))
}

VAR <- function(p = 1, cor = TRUE, ma = FALSE, gr = 'NA', subgr = 'series', n_lv = NULL, ...) {
  if (p < 1 || p > 5) stop("VAR order must be between 1 and 5")
  structure(list(trend_model = "VAR", p = p, cor = cor, ma = ma, gr = gr, subgr = subgr, n_lv),
           class = c("mvgam_trend_term", "VAR"))
}

# Higher-order Stan code generation may look something like this, but will need specifics
# from Sarah Heaps' recent publications to finalize the implementation
generate_var_dynamics <- function(has_predictors = FALSE, max_lag = 1, n_series, ...) {
  glue::glue("
    // VAR({max_lag}) with trend predictors embedded in dynamics
    for (t in {max_lag + 1}:T) {{
      vector[{n_series}] mu_var = X_trend[t] * b_trend;
      for (lag in 1:{max_lag}) {{
        mu_var += phi_var[1:{n_series}, ((lag-1)*{n_series}+1):(lag*{n_series})] * trend_raw[t-lag];
      }}
      trend_raw[t] ~ multi_normal(mu_var, Sigma_var);
    }}
  ")
}
```

### Phase 3: Rcpp Optimization and Post-Processing (Weeks 9-12)

#### Week 9: Rcpp Functions for All Trend Types
**Files**: `src/trend_funs.cpp` (extend), `R/rcpp_trend_interface.R` (new)

```cpp
// Higher-order AR process (extending current ar3_recursC pattern)
// [[Rcpp::export]]
Rcpp::NumericVector ar_p_recursC(Rcpp::NumericVector phi_coeffs,
                                  double drift,
                                  Rcpp::NumericVector linpreds,
                                  Rcpp::NumericVector errors,
                                  Rcpp::NumericVector last_trends, 
                                  int h, int p) {
  int T = h + p;
  Rcpp::NumericVector states(T);
  
  // Initialize with last p states
  for(int i = 0; i < p; ++i) {
    states(i) = last_trends(i);
  }
  
  // AR(p) recursion with embedded linear predictors
  for(int t = p; t < T; ++t) {
    states(t) = drift + linpreds(t);
    for(int lag = 1; lag <= p; ++lag) {
      states(t) += phi_coeffs(lag-1) * (states(t - lag) - linpreds(t - lag));
    }
    states(t) += errors(t);
  }
  
  return states[Rcpp::Range(p, T-1)];
}

// Higher-order VAR process
// [[Rcpp::export]]
arma::mat var_p_recursC(arma::cube A_array,  // 3D array for multiple lag matrices
                        arma::mat linpreds, arma::mat errors,
                        arma::rowvec drift, arma::mat last_trends,
                        int h, int p) {
  // VAR(p) implementation with embedded linear predictors
}

// CAR process optimization
// [[Rcpp::export]]  
Rcpp::NumericVector car_recursC(double phi_car, Rcpp::NumericVector linpreds,
                                Rcpp::NumericVector errors, Rcpp::NumericVector times,
                                double last_trend, int h) {
  // Continuous-time AR with embedded linear predictors
}
```

#### Week 10: Fast JSDGAM Prediction with Rcpp
**Files**: `src/jsdgam_predict.cpp` (new), `R/predict_jsdgam_rcpp.R` (new)

```cpp
// Fast JSDGAM prediction (replaces slow R loops at line 309+ in predict.mvgam.R)
// [[Rcpp::export]]
arma::mat fast_jsdgam_predict(const arma::mat& design_matrices,
                              const arma::mat& betas,
                              const arma::vec& offsets,
                              const arma::cube& lv_coefs,
                              const arma::uvec& series_indices,
                              int n_draws, int n_obs, int n_lv) {
  
  // Step 1: Compute LV predictions
  arma::cube lv_preds = compute_lv_predictions_rcpp(
    design_matrices, betas, offsets, n_draws, n_obs, n_lv
  );
  
  // Step 2: Apply factor loadings (replaces nested R loops)
  arma::mat final_preds = jsdgam_predict_rcpp(
    lv_coefs, lv_preds, series_indices, n_draws, n_obs
  );
  
  return final_preds;
}
```

```r
# R interface for fast JSDGAM prediction
predict_jsdgam_optimized <- function(object, newdata, type = "response", ...) {
  # Call optimized Rcpp function (replaces entire R loop at line 309+)
  trend_predictions <- fast_jsdgam_predict(
    design_matrices = object$prediction_matrices$design_matrices,
    betas = extract_parameter_matrix(object$stan_fit, "b_trend"),
    offsets = object$prediction_matrices$offsets,
    lv_coefs = object$lv_coefs,
    series_indices = as.integer(newdata$series),
    n_draws = n_draws, n_obs = n_obs, n_lv = n_lv
  )
  # Continue with observation model prediction...
}
```

#### Week 11: Coordinated Prediction/Forecasting
**Files**: `R/predict.mvgam.R` (major refactor), `R/forecast.mvgam.R` (refactor)

```r
# Updated predict.mvgam with Rcpp optimization
predict.mvgam <- function(object, newdata = NULL, type = "response", ...) {
  
  if (inherits(object, 'jsdgam')) {
    # Use optimized Rcpp prediction for JSDGAM (replaces lines 309-365)
    return(predict_jsdgam_optimized(object, newdata, type, ...))
    
  } else if (object$dual_object) {
    # Standard dual object prediction with trend models
    obs_pred <- brms::predict(object$brms_fit, newdata = newdata, ...)
    trend_pred <- predict_trend_optimized(object$trend_fit, newdata, ...)
    return(coordinate_predictions(obs_pred, trend_pred, object))
    
  } else {
    # Single object prediction
    return(brms::predict(object$brms_fit, newdata = newdata, ...))
  }
}

# Forecasting with Rcpp optimization
forecast.mvgam <- function(object, newdata = NULL, h = NULL, ...) {
  if (object$dual_object && !is.null(object$trend_fit)) {
    # Use optimized Rcpp functions for trend forecasting
    trend_forecast <- forecast_trend_rcpp(
      trend_model = object$trend_model,
      trend_pars = extract_forecast_parameters(object$trend_fit),
      linpreds = extract_trend_linpreds(newdata, object),
      h = h
    )
    # Continue with observation forecasting...
  }
}
```

#### Week 12: Method System Integration
**Files**: `R/summary.mvgam.R`, `R/marginaleffects.mvgam.R`, `R/conditional_effects.R`,`R/plot.mvgam.R` (refactor)

```r
# Summary method using dual brmsfit-like objects
summary.mvgam <- function(object, ...) {
  obs_summary <- brms::summary(object$brms_fit, ...)
  
  if (object$dual_object && !is.null(object$trend_fit)) {
    trend_summary <- brms::summary(object$trend_fit, ...)
    return(combine_mvgam_summaries(obs_summary, trend_summary, object))
  } else {
    return(adapt_brms_summary(obs_summary, object))
  }
}

# Plotting using brmsfit-like objects
plot.mvgam <- function(x, type = 'residuals', ...) {
  switch(type,
    'residuals' = plot_mvgam_residuals(x, ...),
    'forecast' = plot_mvgam_forecasts(x, ...),
    'trend' = plot_mvgam_trends(x, ...),
    'series' = plot_mvgam_series(x, ...)
  )
}
```

### Phase 4: Testing and Optimization (Weeks 13-16)

#### Week 13-14: Comprehensive Testing
**Files**: `tests/testthat/test-*.R` (major updates)

**Key Testing Areas**:
1. Trend dispatcher system with all trend types
2. Formula-based trend specification parsing
3. Higher-order AR/VAR models (p > 1) 
4. Grouping options (gr/subgr) compatibility
5. data2 integration for factor loadings
6. Rcpp optimization functions
7. Single-fit produces correct dual objects
8. Prior system with dynamic renaming
9. brms compatibility maintained

#### Week 15: Performance Optimization
**Files**: `R/mvgam_optimization.R` (new)

```r
# Object footprint optimization
optimize_mvgam_object <- function(object, level = "standard") {
  if (object$dual_object) {
    object$brms_fit <- trim_brmsfit_like(object$brms_fit, level)
    if (!is.null(object$trend_fit)) {
      object$trend_fit <- trim_brmsfit_like(object$trend_fit, level)
    }
    object$model_data <- optimize_shared_data(object$model_data, level)
  }
  return(object)
}

# Performance benchmarking
benchmark_performance <- function() {
  # Compare new vs old implementation speeds
  # Test memory usage improvements
  # Validate Rcpp optimization benefits
  # Test brms setup method speeds
}
```

#### Week 16: Documentation and Finalization
**Files**: Documentation updates across package

**Documentation Tasks**:
1. Update all roxygen2 comments for refactored functions
2. Update README with new brms foundation information
3. Revise vignettes to demonstrate brms integration benefits
4. Create migration guide from mvgam v1.x to v2.0
5. Update CLAUDE.md with new architecture patterns
6. Document new capabilities enabled by brms integration
7. **brms Compatibility Maintenance**: Document stable brms patterns, compatibility testing procedures, and version monitoring requirements

## Migration/Deployment Plan

### Phase 1: Development Branch (Weeks 1-8)
1. Create `feature/brms-integration` branch
2. Implement foundation components
3. Enable switching via `use_brms_backend = TRUE/FALSE`
4. Maintain parallel old/new implementations

### Phase 2: Alpha Testing (Weeks 9-12)
1. Release alpha with single-fit architecture
2. Test performance vs. current mgcv implementation  
3. Validate Rcpp optimization benefits
4. Test data2 integration for factor models
5. Collect community feedback

### Phase 3: Beta Release (Weeks 13-16)
1. Default to single-fit brms architecture
2. Comprehensive testing and optimization
3. Update all documentation
4. Prepare migration tools

### Phase 4: Production Release
1. Release as mvgam v2.0
2. Remove old mgcv implementation
3. Update package dependencies
4. Announce architectural improvements

## Key Architectural Benefits

### Performance Improvements
1. **brms Setup**: 10-50x faster initialization using optimal method (`backend = "mock"` vs `chains = 0`)
2. **JSDGAM Prediction**: 10-100x speedup by replacing R loops with Rcpp (line 309+ optimization)
3. **Prior System**: 5-10x speedup by eliminating recursive calls and using vectorized operations
4. **Memory Usage**: 30-50% reduction through object trimming and optimization

### User Experience Improvements  
1. **Formula-centric design**: Everything specified in formulas (`~ s(time) + RW(cor = TRUE)`)
2. **Single learning curve**: Only need to know `brms::prior()` and `brms::set_prior()`
3. **Full brms compatibility**: All current brms functionality preserved
4. **Better error messages**: Smart parameter name suggestions and validation

### Maintainability Improvements
1. **Modular architecture**: Clear separation between observation and State-Space layers
2. **Extensible design**: Easy to add new trend types through dispatcher system  
3. **Future-proof priors**: Dynamic parameter discovery keeps up with brms evolution
4. **Single source of truth**: Leverages brms development rather than reimplementing

### Stan Code Generation Benefits
1. **brms Compatibility**: Follows exact brms code generation patterns for seamless integration
2. **Modular Assembly**: Uses brms's `collapse_lists()` pattern for combining code blocks  
3. **Systematic Naming**: Leverages brms parameter naming conventions with `_trend` suffix
4. **Prior Integration**: Reuses brms prior handling infrastructure with automatic parameter renaming
5. **Extensibility**: New trend types follow established brms generator patterns

## Risk Mitigation

### Potential Failure Points
1. **Stan Code Integration**: Combined Stan code complexity
2. **Performance Regression**: Single fit slower than current approach
3. **Object Coordination**: brmsfit-like objects coordination issues
4. **User Interface Changes**: Breaking changes disrupting workflows
5. **brms API Changes**: Public Stan code structure or parameter naming changes
6. **Function Conflicts**: Same function names with different implementations in observation vs trend

### Contingency Plans
1. **Stan Issues**: Extensive testing, fallback to current generation
2. **Performance**: Profile bottlenecks, maintain parallel implementation
3. **Coordination**: Robust testing, error handling, fallback modes
4. **Interface**: Backward compatibility layer, migration tools
5. **API Monitoring**: Compatibility testing, graceful degradation, version-specific adaptations
6. **Function Resolution**: Clear conflict detection, user warnings, precedence rules

## Success Metrics

### Technical Metrics
- [ ] All existing mvgam functionality preserved
- [ ] Performance equal or better than current implementation
- [ ] Memory usage reduced by target amounts
- [ ] All tests passing with new architecture

### User Experience Metrics  
- [ ] Simplified prior specification syntax
- [ ] Formula-based trend specification working
- [ ] Full brms compatibility maintained
- [ ] Clear migration path provided

### Development Metrics
- [ ] Modular, extensible architecture
- [ ] Comprehensive test coverage
- [ ] Clear documentation and examples
- [ ] Community feedback incorporated

## Quality Assurance

### Code Review Standards
- All pull requests require review from at least one maintainer
- New features require unit tests with >90% coverage
- Documentation updates required for all user-facing changes
- Performance benchmarks required for optimization claims

### Testing Strategy
1. **Unit Tests**: Individual function validation
2. **Integration Tests**: End-to-end workflow validation
3. **Performance Tests**: Speed and memory benchmarks
4. **Regression Tests**: Ensure backward compatibility
5. **Edge Case Tests**: Boundary conditions and error handling

### Documentation Requirements
- All exported functions must have complete roxygen2 documentation
- Examples must be runnable and demonstrate key functionality
- Vignettes updated to reflect new capabilities
- Migration guide with before/after code comparisons

## Communication Plan

### Developer Communication
- Weekly progress updates during implementation phases
- Bi-weekly architecture review meetings
- Monthly stakeholder briefings
- Real-time communication via dedicated Slack channel

### Community Engagement
- Alpha release announcement with call for testing
- Beta release with detailed changelog and migration guide
- Webinar demonstrating new capabilities
- Conference presentations at relevant R/Stan events

### Documentation Timeline
- **Week 4**: Initial architecture documentation complete
- **Week 8**: Stan code generation patterns documented
- **Week 12**: Method integration patterns documented
- **Week 16**: Complete user documentation and migration guide

## Post-Release Maintenance

### Version Support Strategy
- mvgam v1.x: Security patches only after v2.0 release
- mvgam v2.0: Full feature support and bug fixes
- Deprecation notices for removed functionality with alternatives

### Performance Monitoring
- Automated benchmarking suite for key workflows
- Memory usage tracking for large datasets
- User feedback collection on performance improvements
- Regular profiling to identify optimization opportunities

### Feature Roadmap
- **v2.1**: Additional trend types based on user requests
- **v2.2**: Enhanced factor model capabilities
- **v2.3**: Advanced forecasting methods
- **v3.0**: Next-generation State-Space modeling features

---

**Plan Status**: Design Complete - Ready for Implementation  
**Next Step**: Begin Phase 1, Week 1 - Trend Type Dispatcher System  
**Document Location**: `mvgam-brms-refactoring-plan.md`  
**Plan Approval**: Pending stakeholder review and technical validation

## Appendix: Technical Dependencies

### R Package Dependencies
- **Core**: brms (>= 2.19.0), Stan (>= 2.30.0)
- **Optimization**: Rcpp (>= 1.0.10), RcppArmadillo (>= 0.12.0)
- **Validation**: checkmate (>= 2.1.0), insight (>= 0.18.0)
- **Utilities**: rlang (>= 1.1.0), glue (>= 1.6.0)

### System Requirements
- **Minimum R Version**: 4.1.0
- **Stan Version**: 2.30.0 or higher
- **Memory**: 8GB RAM recommended for large models
- **Disk Space**: 2GB for package compilation

### Development Environment
- **IDE**: RStudio (>= 2022.07.0) recommended
- **Version Control**: Git with conventional commit messages
- **CI/CD**: GitHub Actions for automated testing
- **Documentation**: pkgdown for website generation

This comprehensive refactoring plan provides a clear roadmap for transforming mvgam into a brms-based extension while maintaining all current functionality and significantly improving performance and user experience.
