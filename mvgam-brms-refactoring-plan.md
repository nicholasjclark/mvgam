# mvgam → brms Extension Refactoring Plan

**Version**: 1.6 (Multiple Imputation Integration)  
**Date**: 2025-01-25  
**Status**: Ready for Implementation

## Executive Summary

Transform mvgam from mgcv-based standalone package into specialized brms extension adding State-Space modeling, N-mixture/occupancy models, and JSDMs. **Core Innovation**: brms generates linear predictors (`mu`, `mu_trend`), single combined Stan model, dual brmsfit-like objects for post-processing. Full multivariate model support preserving brms cross-response correlations while adding State-Space dynamics. **New**: Native multiple imputation support following brms patterns with Rubin's rules pooling.

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

# Distributional models: trends ONLY apply to main response parameter
mvgam(
  bf(y ~ s(x), sigma ~ s(z)),           # Distributional regression
  trend_formula = ~ AR(p = 1),          # Applied ONLY to mu (main parameter)
  family = gaussian(),
  data = data
)

# NOT ALLOWED: Trends in non-main distributional parameters
# mvgam(
#   bf(y ~ s(x), sigma ~ s(z)),
#   trend_formula = list(mu = ~ AR(p = 1), sigma = ~ RW()),  # ❌ FORBIDDEN
#   data = data
# )

# Response helpers with State-Space trends (applied to main parameter only)
mvgam(
  bf(y | mi() ~ s(x), sigma ~ s(z)),    # Missing value imputation
  trend_formula = ~ AR(p = 1),          # Applied to mu only
  data = data
)

# Multiple imputation with State-Space trends
mvgam(
  y ~ s(x), 
  trend_formula = ~ AR(p = 1),
  data = imputed_data_list,             # List of multiply imputed datasets
  combine = TRUE                        # Pool results using Rubin's rules
)

mvgam(
  bf(y | weights(w) ~ s(x)),            # Weighted observations
  trend_formula = ~ RW(cor = TRUE),     # Applied to main parameter
  data = data
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
- Handles distributional regression (trends apply ONLY to main response parameter)
- Handles nonlinear formula complexity (`bf(nl = TRUE)`)
- Supports response helpers (`mi()`, `weights()`, `cens()`, `trunc()`, `trials()`, etc.)
- Maintains brms correlation structures
- Supports multivariate response-specific trends
- **Restriction**: No latent state trends for distributional parameters (sigma, nu, tau)

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

validate_distributional_trends <- function(formula, trend_formula) {
  # Check if formula has distributional components
  if (is_distributional(formula) && is.list(trend_formula)) {
    param_names <- names(trend_formula)
    non_main_params <- setdiff(param_names, c("mu", get_main_parameter(formula)))
    
    if (length(non_main_params) > 0) {
      stop(insight::format_error(
        "State-Space trends not allowed for distributional parameters.",
        "x" = paste("Found trends for:", paste(non_main_params, collapse = ", ")),
        "i" = "trends can only be applied to the main response parameter",
        "i" = "Use brms smooths for distributional parameters: sigma ~ s(time)"
      ))
    }
  }
  
  # Check hurdle model restrictions
  if (is_hurdle_family(formula) && is.list(trend_formula)) {
    param_names <- names(trend_formula)
    hurdle_params <- param_names[grepl("^hu", param_names)]
    
    if (length(hurdle_params) > 0) {
      stop(insight::format_error(
        "State-Space trends not allowed for hurdle parameters.",
        "x" = paste("Found trends for hurdle parameters:", paste(hurdle_params, collapse = ", ")),
        "i" = "trends can only be applied to the main response parameter (mu)",
        "i" = "The hurdle process should use fixed/smooth effects: hu ~ s(time)"
      ))
    }
  }
}

validate_multiple_imputation_data <- function(data_list) {
  if (!is.list(data_list) || length(data_list) < 2) {
    stop(insight::format_error(
      "Multiple imputation requires list of 2+ datasets.",
      "i" = "Use mice::complete(mice_object, 'all') or similar"
    ))
  }
  
  # Validate structure consistency across imputations
  ref_structure <- sapply(data_list[[1]], class)
  for (i in seq_along(data_list)[-1]) {
    curr_structure <- sapply(data_list[[i]], class)
    if (!identical(ref_structure, curr_structure)) {
      stop(insight::format_error(
        "Inconsistent data structure across imputations.",
        "x" = paste("Imputation", i, "differs from first dataset")
      ))
    }
  }
}
```

**Multivariate Validation**:
- Response-trend mapping consistency
- Family compatibility with trend types  
- Missing data consistency across responses
- Cross-response correlation preservation
- **Distributional model restriction**: Trends only allowed for main response parameter
- **Response helper validation**: Compatibility with `mi()`, `weights()`, `cens()`, `trunc()`, `trials()`, etc.
- **Multiple imputation validation**: Dataset structure consistency, pooling compatibility
- **Hurdle model validation**: Trends restricted to main (`mu`) parameter, proper `hu` parameter handling (similar strategy for zero_inflated_* or zero_one_inflated_* families)

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
  
  # Validate distributional models: trends only for main parameter
  validate_distributional_trends(formula, trend_formula)
  
  # Validate hurdle models: trends only for main parameter  
  validate_hurdle_trends(formula, trend_formula)
  
  return(list(responses = response_names, trends = trend_formula))
}

# Helper functions for hurdle model support
is_hurdle_family <- function(formula) {
  family_info <- extract_family_info(formula)
  return(grepl("hurdle", family_info$family, ignore.case = TRUE))
}

extract_parameters <- function(fit) {
  # Extract parameter names handling hurdle conventions
  draws <- as_draws_df(fit)
  param_names <- names(draws)
  
  # Separate main and hurdle parameters
  main_params <- param_names[!grepl("^b_hu_", param_names)]
  hurdle_params <- param_names[grepl("^b_hu_", param_names)]
  
  return(list(main = main_params, hurdle = hurdle_params))
}

validate_hurdle_trends <- function(formula, trend_formula) {
  if (is_hurdle_family(formula) && is.list(trend_formula)) {
    param_names <- names(trend_formula)
    hurdle_params <- param_names[grepl("^hu", param_names)]
    
    if (length(hurdle_params) > 0) {
      stop(insight::format_error(
        "State-Space trends not allowed for hurdle parameters.",
        "x" = paste("Found trends for:", paste(hurdle_params, collapse = ", ")),
        "i" = "trends can only be applied to the main response parameter",
        "i" = "The hurdle process should use fixed/smooth effects: hu ~ s(time)"
      ))
    }
  }
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

#### Week 4: Single-Fit Architecture & Backend Strategy + Multiple Imputation
```r
mvgam <- function(formula, trend_formula = NULL, data = NULL, backend = NULL, 
                  combine = TRUE, file_refit = getOption("mvgam.file_refit", "never"), 
                  ...) {
  
  # 1. Handle multiple imputation input
  if (is.list(data) && !is.data.frame(data)) {
    if (combine) {
      return(mvgam_multiple(formula, trend_formula, data, backend, file_refit, ...))
    } else {
      # Return list of individual fits (useful for diagnostics)
      return(map(data, ~ mvgam(formula, trend_formula, .x, backend, ...)))
    }
  }
  
  # 2. Parse multivariate formulas if needed
  mv_spec <- parse_multivariate_trends(formula, trend_formula)
  
  # 3. Setup obs/trend models (no fitting)
  obs_setup <- setup_brms_lightweight(formula, data, ...)
  trend_setup <- setup_brms_lightweight(trend_spec$base_formula, ...)
  
  # 4. Extract trend stanvars and generate combined Stan code
  trend_stanvars <- extract_trend_stanvars_from_setup(trend_setup, trend_spec)
  combined_stancode <- generate_combined_stancode(obs_setup, trend_stanvars)
  
  # 5. Use enhanced mvgam backend system
  combined_fit <- fit_mvgam_model(combined_stancode, combined_standata, 
                                  backend = backend, ...)
  return(create_mvgam_from_combined_fit(combined_fit, obs_setup, trend_setup))
}

# Multiple imputation implementation following brms patterns
mvgam_multiple <- function(formula, trend_formula, data_list, backend, file_refit, ...) {
  validate_multiple_imputation_data(data_list)
  
  # Fit models to each imputed dataset with file caching
  fits <- map(seq_along(data_list), function(i) {
    fit_file <- if (!is.null(file_refit) && file_refit != "never") {
      paste0(tools::file_path_sans_ext(file_refit), "_imp", i, ".rds")
    } else NULL
    
    mvgam(formula = formula, trend_formula = trend_formula,
          data = data_list[[i]], backend = backend, file = fit_file, ...)
  })
  
  # Pool results using Rubin's rules
  return(pool_mvgam_fits(fits))
}
```

**Backend Strategy**: Enhance existing mvgam approach maintaining message silencing, progress feedback, error handling, and threading options.

### Phase 2: Stan Integration (Weeks 5-8)

#### Week 5-6: Two-Stage Stan Assembly
- Extract trend stanvars from trend_setup and generate base model
- Modify observation linear predictor with missing data preservation
- Handle nonlinear model complexity (`bf(nl = TRUE)`)
- Ensure compatibility with all mvgam trend types (RW, AR, VAR, GP, CAR)

#### Week 7: Dynamic Prior System & Context-Aware Validation + Hurdle Model Support
- Intelligent autocorrelation validation based on formula context
- Extract trend components from brms-generated Stan code
- Apply systematic `_trend` suffix handling
- Handle multivariate response-specific parameters
- **Hurdle model validation**: Ensure trends only applied to main (`mu`) parameter, not hurdle (`hu`) parameter
- **Parameter name recognition**: Properly handle `b_hu_*` vs `b_*` parameter naming conventions
- **Stan code integration**: Ensure hurdle and trend processes don't interfere

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

#### Week 11: brms Prediction Integration + Multiple Imputation Pooling
```r
# Prediction methods with multiple imputation support
posterior_predict.mvgam <- function(object, newdata = NULL, resp = NULL, 
                                   allow_new_levels = FALSE, ...) {
  if (inherits(object, "mvgam_pooled")) {
    return(posterior_predict_multiple(object, newdata, resp, ...))
  }
  
  if (is_multivariate(object)) {
    if (!is.null(resp)) {
      return(predict_single_response(object, resp, newdata, ...))
    } else {
      return(predict_all_responses(object, newdata, ...))
    }
  }
  
  # Use brms::prepare_predictions() for full pipeline
  prep <- brms::prepare_predictions(object$obs_fit, newdata = newdata, 
                                   allow_new_levels = allow_new_levels, ...)
  base_linpred <- brms::posterior_linpred_draws(prep)
  trend_effects <- predict_trend_effects(object, prep)
  prep <- update_brmsprep_linpred(prep, base_linpred + trend_effects)
  
  return(brms::posterior_predict_draws(prep))
}

# Rubin's rules pooling for parameter estimates
pool_mvgam_fits <- function(fits) {
  validate_compatible_fits(fits)
  estimates_list <- map(fits, extract_fit_estimates)
  pooled_estimates <- apply_rubins_rules(estimates_list)
  
  # Create pooled mvgam object with individual fits stored
  pooled_fit <- create_pooled_mvgam(fits[[1]], pooled_estimates)
  attr(pooled_fit, "individual_fits") <- fits
  attr(pooled_fit, "n_imputations") <- length(fits)
  class(pooled_fit) <- c("mvgam_pooled", class(fits[[1]]))
  
  return(pooled_fit)
}
```

#### Week 12: Method System Integration + Multiple Imputation Methods
```r
# Critical: log_lik integration following brms patterns
log_lik.mvgam <- function(object, resp = NULL, ...) {
  if (inherits(object, "mvgam_pooled")) {
    # Pool log-likelihood across imputations (average)
    ll_list <- map(attr(object, "individual_fits"), ~ log_lik(.x, resp = resp, ...))
    pooled_ll <- Reduce("+", ll_list) / length(ll_list)
    attr(pooled_ll, "n_imputations") <- length(ll_list)
    return(pooled_ll)
  }
  
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

# Enhanced update() method with multiple imputation support
update.mvgam <- function(object, formula. = NULL, trend_formula. = NULL, 
                        newdata = NULL, recompile = NULL, combine = TRUE, ...) {
  
  # Handle multiple imputation objects
  if (inherits(object, "mvgam_pooled")) {
    individual_fits <- attr(object, "individual_fits")
    updated_fits <- map(individual_fits, ~ update(.x, formula. = formula., 
      trend_formula. = trend_formula., newdata = newdata, recompile = recompile, ...))
    return(if (combine) pool_mvgam_fits(updated_fits) else updated_fits)
  }
  
  # Standard update validation and processing
  validate_update_arguments(object, formula., trend_formula., newdata, ...)
  
  if (!is.null(formula.)) {
    new_formula <- update_formula_safely(object$formula, formula.)
    validate_formula_change_compatibility(object, new_formula)
  }
  
  if (!is.null(trend_formula.)) {
    new_trend_formula <- update_trend_formula_safely(object$trend_formula, trend_formula.)
    validate_trend_change_compatibility(object, new_trend_formula)
  }
  
  needs_recompile <- determine_recompilation_need(object, formula., trend_formula., ...)
  if (is.null(recompile)) recompile <- needs_recompile
  
  updated_args <- prepare_update_args(object, formula., trend_formula., ...)
  do.call(mvgam, updated_args)
}

# Additional multiple imputation methods
print.mvgam_pooled <- function(x, ...) {
  cat("Multiple Imputation mvgam Model\n")
  cat("Number of imputations:", attr(x, "n_imputations"), "\n")
  cat("Pooling method: Rubin's rules\n\n")
  NextMethod("print")
  cat("\nNote: Estimates pooled across", attr(x, "n_imputations"), "imputations\n")
}

# Model comparison with MI pooling following Vehtari et al.
loo.mvgam_pooled <- function(x, ...) {
  individual_fits <- attr(x, "individual_fits")
  loo_list <- map(individual_fits, brms::loo)
  pooled_loo <- pool_loo_estimates(loo_list)
  attr(pooled_loo, "n_imputations") <- length(individual_fits)
  return(pooled_loo)
}
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

test_that("distributional models: trends only for main parameter", {
  # Valid: trend only for main parameter
  fit_distr_valid <- mvgam(
    bf(y ~ s(x), sigma ~ s(z)),
    trend_formula = ~ AR(p = 1),  # Applied to mu only
    family = gaussian(),
    data = data
  )
  expect_no_error(fit_distr_valid)
  
  # Invalid: attempting trends for distributional parameters
  expect_error(
    mvgam(
      bf(y ~ s(x), sigma ~ s(z)),
      trend_formula = list(mu = ~ AR(p = 1), sigma = ~ RW()),  # sigma trend forbidden
      family = gaussian(),
      data = data
    ),
    "State-Space trends not allowed for distributional parameters"
  )
})

test_that("response helpers work with State-Space trends", {
  # Missing value imputation with trends
  fit_mi <- mvgam(
    bf(y | mi() ~ s(x)),
    trend_formula = ~ AR(p = 1),
    data = data_with_missing
  )
  expect_missing_imputation_correct(fit_mi)
  
  # Weighted observations
  fit_weights <- mvgam(
    bf(y | weights(w) ~ s(x)),
    trend_formula = ~ RW(),
    data = weighted_data
  )
  expect_weights_handled_correctly(fit_weights)
})
```

**Multiple Imputation Testing**:
```r
test_that("multiple imputation with State-Space trends works", {
  # Basic MI workflow
  mice_data <- mice::mice(data_with_missing, m = 5, printFlag = FALSE)
  imputed_datasets <- mice::complete(mice_data, "all")
  
  fit_mi <- mvgam(y ~ s(x), trend_formula = ~ AR(p = 1), 
                  data = imputed_datasets, combine = TRUE)
  
  expect_s3_class(fit_mi, "mvgam_pooled")
  expect_equal(attr(fit_mi, "n_imputations"), 5)
  
  # Pooled estimates follow Rubin's rules
  pooled_coefs <- coef(fit_mi)
  individual_fits <- attr(fit_mi, "individual_fits")
  expect_length(individual_fits, 5)
  
  # Predictions pool appropriately
  pred_pooled <- posterior_predict(fit_mi, newdata = test_data)
  expect_true(attr(pred_pooled, "n_imputations") == 5)
  
  # Model comparison works
  loo_pooled <- loo(fit_mi)
  expect_s3_class(loo_pooled, "loo_pooled")
})

test_that("multivariate MI with response-specific trends", {
  fit_mv_mi <- mvgam(
    mvbf(y1 ~ x, y2 ~ z),
    trend_formula = list(y1 = ~ AR(p = 1), y2 = ~ RW()),
    data = imputed_datasets_mv,
    combine = TRUE
  )
  
  expect_s3_class(fit_mv_mi, c("mvgam_pooled", "mvgam"))
  expect_true(is_multivariate(fit_mv_mi))
  
  # Response-specific predictions work
  pred_y1 <- posterior_predict(fit_mv_mi, resp = "y1")
  pred_y2 <- posterior_predict(fit_mv_mi, resp = "y2")
  expect_different_dimensions(pred_y1, pred_y2)
})
```

**Enhanced update() method testing**:
```r
test_that("enhanced update() method works correctly", {
  # Basic formula updates
  fit_base <- mvgam(y ~ s(x), trend_formula = ~ AR(p = 1), data = data)
  fit_updated <- update(fit_base, formula. = . ~ . + z)
  expect_formula_updated_correctly(fit_updated)
  
  # Multiple imputation updates
  fit_mi_updated <- update(fit_mi, trend_formula. = ~ RW())
  expect_s3_class(fit_mi_updated, "mvgam_pooled")
  expect_equal(attr(fit_mi_updated, "n_imputations"), 5)
  
  # Recompilation intelligence
  fit_no_recompile <- update(fit_base, iter = 4000)  # Should not recompile
  expect_false(attr(fit_no_recompile, "recompiled"))
})
```

**Performance Validation**:
- Setup speed: 10x improvement target
- JSDGAM prediction: 100x speedup with Rcpp
- Memory usage: 30% reduction target
- **Formula complexity matrix**: all combinations working
  - Linear/smooth/nonlinear × RW/AR/VAR/GP trends
  - Distributional models: mu/sigma/nu/tau parameter trends
  - Response helpers: mi()/weights()/cens()/trunc()/trials() compatibility
  - Multivariate: shared vs response-specific trends
  - **Multiple imputation**: Rubin's rules accuracy, pooling diagnostics
  
#### Week 15: Performance Optimization & Object Footprint
- Memory usage optimization and object compression
- Final performance validation against targets
- Automated benchmarking integration

#### Week 16: Documentation & Release Preparation
- Updated function documentation with roxygen2
- Key vignettes: multivariate models, autocorrelation integration, multiple imputation workflow, migration guide
- Performance benchmarks documentation
- **Enhanced update() method documentation**: Examples showing formula updates, trend changes, recompilation logic
- **Multiple imputation documentation**: Workflow examples, pooling diagnostics, comparison with brms patterns
- Community feedback integration

## Key Innovation Points

### 1. **Autocorrelation Intelligence**
First package to properly distinguish observation-level residual correlation from State-Space dynamics, enabling sophisticated multi-level temporal structures.

### 2. **Multivariate State-Space**
Native support for response-specific trends while preserving brms cross-response correlations - unprecedented capability in the ecosystem.

### 3. **Multiple Imputation Integration**
Seamless multiple imputation support with Rubin's rules pooling, extending brms patterns to State-Space models - first implementation in time series modeling ecosystem.

### 4. **Stan Extension Pattern**
Using brms `stanvars` mechanism for State-Space injection - provides scalable approach for other time series extensions.

### 5. **Method System Integration**
Dual brmsfit-like objects enabling seamless brms ecosystem compatibility:
- **Model evaluation**: loo/waic/pp_check with bayesplot integration
- **Diagnostics**: rhat/neff_ratio/mcmc_plot/nuts_params via brms methods  
- **Prediction**: posterior_predict/fitted/residuals following brms patterns
- **Multiple imputation**: Rubin's rules pooling for all inference methods
- **State-Space extensions**: mvgam-specific functionality (forecasting, trend extraction) built on top

## Risk Mitigation

### Primary Implementation Risks & Solutions

1. **Stan Code Injection Complexity**
   - **Risk**: Breaking brms parameter transformations in complex models
   - **Mitigation**: AST-based modification with extensive validation, fallback to limited modification approach

2. **Multivariate Missing Data Complexity**
   - **Risk**: Cross-response missing patterns breaking trend evolution
   - **Mitigation**: Preserve proven `obs_ind` tracking approach, comprehensive testing matrix

3. **Multiple Imputation Pooling Accuracy**
   - **Risk**: Incorrect Rubin's rules implementation affecting inference
   - **Mitigation**: Extensive validation against known MI results, comparison with mice/brms patterns

4. **Backend Maintenance Burden**
   - **Risk**: brms internal changes breaking mvgam compatibility
   - **Mitigation**: Automated monitoring system, version compatibility matrix, graceful degradation

5. **Performance Regression**
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
- [ ] Full brms compatibility maintained:
  - [ ] All formula types: linear/smooth/nonlinear/multivariate
  - [ ] All families and link functions
  - [ ] Distributional regression: main parameter trends only (others via brms smooths)
  - [ ] Response helpers: mi()/weights()/cens()/trunc()/trials()/rate() etc.
  - [ ] Prior specification syntax
  - [ ] **Multiple imputation**: Native support with Rubin's rules pooling
- [ ] Seamless brms ecosystem integration:
  - [ ] Model evaluation: loo/waic/pp_check with bayesplot
  - [ ] Diagnostics: rhat/neff_ratio/mcmc_plot/nuts_params via brms methods
  - [ ] Prediction: posterior_predict/fitted/residuals following brms patterns
  - [ ] Model updating: enhanced update() method with comprehensive validation
  - [ ] **MI-enhanced methods**: All methods work with pooled multiple imputation objects
- [ ] >90% test coverage achieved
- [ ] Multivariate models with response-specific trends working
- [ ] Cross-response correlations preserved in multivariate State-Space models
- [ ] Intelligent autocorrelation validation preventing conflicts
- [ ] **Multiple imputation validation**: Dataset consistency, pooling diagnostics

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
**Critical Success Factor**: Stan code modification that preserves all brms functionality while seamlessly adding State-Space dynamics and multiple imputation support  
**Key Innovation**: Leverage brms linear predictor generation + mvgam State-Space expertise + native multivariate support + Rubin's rules pooling
