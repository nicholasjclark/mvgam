# brms Integration Patterns for mvgam

**Purpose**: Essential brms design patterns extracted for mvgam implementation

## Key Integration Strategies

### 1. stanvars System for Code Injection

**Core Pattern**: Use brms stanvars to inject custom Stan code without modifying brms internals

```r
# Create stanvars for both data and code
trend_stanvars <- list(
  # Data injection
  stanvar(ytimes_trend, "ytimes_trend", 
          scode = "matrix[n, n_lv] ytimes_trend;",
          block = "data"),
  
  # Parameter injection  
  stanvar(NULL, "trend_params",
          scode = "matrix[n, n_lv] LV_raw;",
          block = "parameters"),
          
  # Model code injection
  stanvar(NULL, "trend_evolution", 
          scode = "LV[2:n] ~ multi_normal_cholesky(mu_trend, L_sigma);",
          block = "model")
)

# Let brms handle integration
stancode <- brms::stancode(obs_formula, data, stanvars = trend_stanvars)
standata <- brms::standata(obs_formula, data, stanvars = trend_stanvars)
```

### 2. Two-Stage Stan Assembly

**Stage 1**: Generate base Stan model with trend stanvars
```r
# brms builds complete model including trend components
base_stancode <- brms::stancode(obs_formula, data, stanvars = trend_stanvars)
base_standata <- brms::standata(obs_formula, data, stanvars = trend_stanvars)
```

**Stage 2**: Post-process to inject trend effects
```r
# Modify linear predictor to include trend effects
final_stancode <- inject_trend_into_linear_predictor(base_stancode, trend_spec)
# standata already complete via stanvars - no modification needed
```

### 3. Response-Specific Parameter Naming

**Pattern**: Follow brms multivariate naming for consistency
```stan
// Single response
vector[N] mu = rep_vector(0.0, N);
vector[N] mu_trend;           // mvgam trend component
mu += mu_trend;               // Combine effects

// Multivariate responses
vector[N_count] mu_count = rep_vector(0.0, N_count);
vector[N_biomass] mu_biomass = rep_vector(0.0, N_biomass);
vector[N_count] mu_trend_count;     // Response-specific trends
vector[N_biomass] mu_trend_biomass;

mu_count += mu_trend_count;
mu_biomass += mu_trend_biomass;
```

### 4. Non-Vectorized Likelihood for Missing Data

**Pattern**: Follow brms non-vectorized handling when observations don't align with parameter arrays
```stan
// Trends evolve over ALL timesteps
for (t in 2:T) {
  trend[t] ~ normal(phi * trend[t-1], sigma_trend);
}

// Likelihood only for non-missing observations
{
  vector[n_nonmissing] mu_obs = mu_combined[obs_ind];
  vector[n_nonmissing] y_obs = Y[obs_ind]; 
  y_obs ~ normal(mu_obs, sigma);
}
```

### 5. Modular Data Generation

**Pattern**: Leverage brms data generation architecture
```r
# brms data generation flow
.standata <- function(bframe, data, prior, stanvars, ...) {
  # Core brms data
  out <- data_response(bframe, data, ...)       # Y, trials, censoring
  c(out) <- data_predictor(bframe, data, ...)   # X matrices, effects
  c(out) <- data_gr_global(bframe, data)        # Random effects structure
  
  # Inject trend data via stanvars
  if (is.stanvars(stanvars)) {
    stanvars_data <- subset_stanvars(stanvars, block = "data")
    out[names(stanvars_data)] <- from_list(stanvars_data, "sdata")
  }
  
  return(out)
}

# mvgam leverages this by providing complete stanvars
generate_trend_data_stanvars <- function(trend_obj, data_info) {
  # Time indexing
  ytimes_stanvar <- stanvar(
    x = create_time_matrix(data_info),
    name = "ytimes_trend",
    scode = "matrix[n, n_lv] ytimes_trend;"
  )
  
  # Missing data indicators
  obs_ind_stanvar <- stanvar(
    x = which(!is.na(data_info$response)),
    name = "obs_ind", 
    scode = "array[n_nonmissing] int obs_ind;"
  )
  
  return(list(ytimes_stanvar, obs_ind_stanvar))
}
```

## Method Integration Patterns

### 1. Dual brmsfit Object Strategy

**Pattern**: Create brmsfit-like objects that work with brms ecosystem
```r
create_mvgam_brmsfit <- function(fit, obs_formula, trend_formula) {
  # Extract observation components
  obs_fit <- extract_observation_components(fit, obs_formula)
  class(obs_fit) <- c("brmsfit", "mvgam_obs")
  
  # Extract trend components  
  trend_fit <- extract_trend_components(fit, trend_formula)
  class(trend_fit) <- c("brmsfit", "mvgam_trend")
  
  # Combined mvgam object
  mvgam_fit <- list(
    obs_fit = obs_fit,
    trend_fit = trend_fit,
    combined_fit = fit,
    formula = obs_formula,
    trend_formula = trend_formula
  )
  class(mvgam_fit) <- c("mvgam", "list")
  
  return(mvgam_fit)
}
```

### 2. Method Dispatch Integration

**Pattern**: Leverage brms methods through dual objects
```r
# Prediction methods
posterior_predict.mvgam <- function(object, newdata = NULL, ...) {
  # Use brms prediction infrastructure
  prep <- brms::prepare_predictions(object$obs_fit, newdata = newdata, ...)
  
  # Add trend effects
  base_linpred <- brms::posterior_linpred_draws(prep)
  trend_effects <- predict_trend_effects(object, prep)
  
  # Update preparation object
  prep <- update_brmsprep_linpred(prep, base_linpred + trend_effects)
  
  # Use brms final prediction
  return(brms::posterior_predict_draws(prep))
}

# Model comparison methods
loo.mvgam <- function(x, ...) {
  # Extract log-likelihood from combined fit
  log_lik_array <- extract_log_lik(x$combined_fit, ...)
  loo::loo.array(log_lik_array, ...)
}

# Diagnostic methods
pp_check.mvgam <- function(object, ...) {
  # Use brms posterior predictive checking
  brms::pp_check(object$obs_fit, ...)
}
```

### 3. Multiple Imputation Integration

**Pattern**: Extend brms workflow for multiple datasets
```r
fit_multiple_imputation <- function(formula, trend_formula, data_list, ...) {
  # Fit each imputed dataset
  fits <- map(data_list, function(data) {
    mvgam(formula, trend_formula = trend_formula, data = data, ...)
  })
  
  # Pool using Rubin's rules
  pooled_fit <- pool_mvgam_fits(fits)
  
  # Preserve individual fits for diagnostics
  attr(pooled_fit, "individual_fits") <- fits
  attr(pooled_fit, "n_imputations") <- length(fits)
  class(pooled_fit) <- c("mvgam_pooled", class(pooled_fit))
  
  return(pooled_fit)
}

# Pooling implementation
pool_mvgam_fits <- function(fits) {
  # Extract parameter estimates from each fit
  estimates_list <- map(fits, extract_parameter_estimates)
  
  # Apply Rubin's rules
  pooled_estimates <- apply_rubins_rules(estimates_list)
  
  # Create pooled object based on first fit structure
  pooled_fit <- fits[[1]]
  pooled_fit <- update_fit_estimates(pooled_fit, pooled_estimates)
  
  return(pooled_fit)
}
```

## Threading and Performance Integration

### 1. Within-Chain Parallelization

**Pattern**: Preserve brms threading capabilities
```r
# Threading-compatible stanvars
trend_stanvars <- stanvar(
  x = time_matrix,
  name = "ytimes_trend",
  scode = "matrix[n, n_lv] ytimes_trend;",
  pll_args = "matrix ytimes_trend"  # Enable threading
)

# Threading setup in standata
if (use_threading(threads)) {
  # brms handles grainsize calculation
  out$grainsize <- threads$grainsize
  if (is.null(out$grainsize)) {
    out$grainsize <- ceiling(out$N / (2 * threads$threads))
    out$grainsize <- max(100, out$grainsize)
  }
}
```

### 2. Optimization Preservation  

**Pattern**: Let brms choose optimizations
```r
# brms automatically selects GLM primitives when appropriate
if (use_glm_primitive(bterms)) {
  # Efficient GLM likelihood
  p <- args_glm_primitive(bterms$dpars$mu, ...)
  lpdf <- sdist("poisson_log_glm", p$x, p$alpha, p$beta)
} else {
  # Standard parameterization with trend effects
  p <- stan_log_lik_dpars(bterms)
  p$mu <- paste0("(", p$mu, " + mu_trend)")  # Inject trend
  lpdf <- sdist("poisson_lpdf", p$mu)
}
```

## Family Integration Patterns

### 1. Universal Family Support

**Pattern**: Work with any brms family without modification
```r
# mvgam doesn't reimplement families - just adds trends
generate_trend_likelihood_modification <- function(base_lpdf, trend_spec) {
  # Extract parameter that gets trend effect (usually mu)
  main_param <- extract_main_parameter(base_lpdf)
  
  # Inject trend into main parameter
  modified_lpdf <- str_replace(
    base_lpdf,
    pattern = main_param,
    replacement = paste0("(", main_param, " + mu_trend)")
  )
  
  return(modified_lpdf)
}
```

### 2. Distributional Model Restriction

**Pattern**: Validate trend placement in distributional models
```r
validate_distributional_trends <- function(bterms, trend_spec) {
  # Only allow trends on main parameter (usually mu)
  main_param <- "mu"  # Could extract from bterms if needed
  aux_params <- setdiff(names(bterms$dpars), main_param)
  
  # Check trend specification doesn't target auxiliary parameters
  trend_targets <- extract_trend_targets(trend_spec)
  invalid_targets <- intersect(trend_targets, aux_params)
  
  if (length(invalid_targets) > 0) {
    stop("Trends only supported for main parameter (mu), not: ",
         paste(invalid_targets, collapse = ", "))
  }
}
```

## Validation Integration

### 1. Leverage brms Validation

**Pattern**: Use brms validation infrastructure
```r
# brms handles most validation - mvgam adds trend-specific checks
validate_mvgam_formula <- function(formula, trend_formula, data, family, ...) {
  # Use brms validation first
  bterms <- brms::validate_formula(formula, data = data, family = family, ...)
  
  # Add mvgam-specific validation
  validate_trend_formula_compatibility(formula, trend_formula)
  validate_autocorrelation_separation(formula, trend_formula)
  validate_distributional_trend_placement(bterms, trend_formula)
  
  return(bterms)
}
```

### 2. Intelligent Error Messages

**Pattern**: Context-aware guidance
```r
detect_autocorr_conflict <- function(formula, trend_formula) {
  obs_autocor <- extract_brms_autocor_terms(formula)
  trend_autocor <- extract_brms_autocor_terms(trend_formula)
  
  if (length(obs_autocor) > 0) {
    message("Detected observation-level autocorrelation - this complements State-Space trends")
  }
  
  if (length(trend_autocor) > 0) {
    stop(
      "brms autocorrelation terms found in trend_formula: ", 
      paste(trend_autocor, collapse = ", "), "\n",
      "Use mvgam trend syntax instead:\n",
      "  ar(p = 1) → AR(p = 1)\n",
      "  ma(q = 1) → MA(q = 1)"
    )
  }
}
```

## Critical Success Factors

### 1. Zero brms Modification
- All extensions through stanvars and post-processing
- Preserve every brms capability
- Maintain ecosystem compatibility

### 2. Intelligent Feature Separation  
- brms: observation-level correlation, families, priors
- mvgam: State-Space temporal dynamics  
- Clear validation prevents conflicts

### 3. Method System Integration
- Dual objects enable brms method dispatch
- All brms ecosystem tools work seamlessly
- Custom methods extend rather than replace

### 4. Performance Preservation
- Leverage all brms optimizations
- Threading compatibility maintained
- Memory efficiency through shared structures
