# brms Stan Integration Patterns for mvgam

**Version**: 1.0  
**Date**: 2025-01-30  
**Context**: Synthesis of brms design patterns for mvgam's Stan code integration features

## Executive Summary

This document synthesizes key design and architectural patterns from brms's Stan code generation system, focusing on how mvgam can leverage brms for flexible Stan model generation while extending it with State-Space dynamics. The analysis covers brms's modular code generation, dynamic parameterization, stanvars system, and multivariate handling.

## Key brms Design Patterns

### 1. Modular Stan Code Generation Architecture

**Pattern**: brms uses a **dispatch-based modular system** where different components generate independent Stan code snippets that are assembled into complete models.

**Core Components**:
```r
# Main dispatcher for predictor terms
stan_predictor.bframel <- function(x, ...) {
  collapse_lists(
    stan_fe(x, ...),      # Fixed effects
    stan_thres(x, ...),   # Ordinal thresholds  
    stan_sp(x, ...),      # Splines/smooths
    stan_cs(x, ...),      # Category-specific effects
    stan_sm(x, ...),      # Special smooth terms
    stan_gp(x, ...),      # Gaussian processes
    stan_ac(x, ...),      # Autocorrelation
    stan_offset(x, ...),  # Offsets
    stan_bhaz(x, ...)     # Baseline hazards
  )
}
```

**mvgam Integration Strategy**:
- **Extend the modular system** by adding trend-specific generators
- **Leverage existing structure** for observation model components
- **Inject State-Space components** through stanvars without breaking existing functionality

### 2. Dynamic Parameter Naming and Response Handling

**Pattern**: brms generates **response-specific parameter names** dynamically, enabling flexible multivariate and distributional modeling.

**Response-Specific Naming**:
```stan
// Single response
vector[N] mu = rep_vector(0.0, N);
real<lower=0> sigma;

// Multivariate responses  
vector[N_tarsus] mu_tarsus = rep_vector(0.0, N_tarsus);
vector[N_back] mu_back = rep_vector(0.0, N_back);
real<lower=0> sigma_tarsus;
real<lower=0> sigma_back;

// Distributional parameters
vector[N] mu = rep_vector(0.0, N);
vector[N] sigma = rep_vector(0.0, N);  // sigma ~ group effects
```

**mvgam Implementation**:
- **Generate trend parameters** with response-specific names: `mu_trend_tarsus`, `mu_trend_back`
- **Combine with observation predictors**: `mu_combined = mu + mu_trend`
- **Preserve brms naming conventions** for seamless ecosystem integration

### 3. stanvars System for Code Injection

**Pattern**: brms uses the **stanvars system** to inject custom Stan code into specific program blocks, enabling extensibility without modifying core generation.

**stanvar Structure**:
```r
stanvar <- function(x = NULL, name = NULL, scode = NULL,
                    block = "data", position = "start",
                    pll_args = NULL)

# Blocks: "data", "tdata", "parameters", "tparameters", 
#         "model", "likelihood", "genquant", "functions"
```

**mvgam Trend Injection Strategy**:
```r
# Stage 1: Extract trend stanvars from brms setup
trend_stanvars <- extract_trend_stanvars_from_setup(trend_setup, trend_spec)
base_stancode <- brms::stancode(obs_formula, data, stanvars = trend_stanvars)

# Stage 2: Modify observation linear predictor
final_stancode <- inject_trend_into_linear_predictor(base_stancode, trend_spec)
```

### 3.1. brms Stan Data Generation Architecture

**Pattern**: brms uses a **modular, layered data generation system** that assembles Stan data through specialized functions, enabling systematic data injection.

**Core Data Generation Flow**:
```r
standata.default <- function(object, data, family, prior, stanvars, ...) {
  # 1. Validate and process formula
  bterms <- brmsterms(object)
  bframe <- brmsframe(bterms, data)
  
  # 2. Generate core data components
  .standata(bframe, data, prior, stanvars, ...)
}

.standata <- function(bframe, data, prior, stanvars, ...) {
  # Response data (Y, trials, censoring, etc.)
  out <- data_response(bframe, data, ...)
  
  # Predictor data (X matrices, group-level effects, splines)
  c(out) <- data_predictor(bframe, data, prior, ...)
  
  # Global group-level data 
  c(out) <- data_gr_global(bframe, data2)
  
  # Measurement error data
  c(out) <- data_Xme(bframe, data)
  
  # Inject stanvars data
  if (is.stanvars(stanvars)) {
    stanvars_data <- subset_stanvars(stanvars, block = "data")
    out[names(stanvars_data)] <- from_list(stanvars_data, "sdata")
  }
  
  return(out)
}
```

**Key Data Components**:
- **Response Data**: `data_response()` - Y variables, censoring, truncation, trials
- **Predictor Data**: `data_predictor()` - Design matrices, coefficients counts  
- **Group-Level Data**: `data_gr_global()` - Random effects grouping structure
- **Special Effects**: `data_sp()`, `data_sm()` - Splines, special terms
- **stanvars Injection**: Custom data variables from user-defined stanvars

**mvgam Data Integration Strategy**:
```r
# Generate trend-specific data through stanvars
trend_data_stanvars <- generate_trend_data_stanvars(trend_obj, data_info)

# Examples of trend data variables
trend_stanvars <- list(
  # Time indexing for trends
  stanvar(ytimes_trend, "ytimes_trend", 
          scode = "matrix[n, n_lv] ytimes_trend;"),
  
  # Missing data indicators  
  stanvar(obs_ind, "obs_ind",
          scode = "array[n_nonmissing] int obs_ind;"),
          
  # Time distances for CAR models
  stanvar(time_dis, "time_dis",
          scode = "matrix[n, n_lv] time_dis;")
)

# Let brms handle standard data + trend data
combined_standata <- standata(obs_formula, data, 
                             stanvars = trend_stanvars, ...)
```

### 4. Non-Vectorized Likelihood Handling

**Pattern**: brms automatically detects when likelihood contributions cannot be vectorized and **wraps them in observation loops**.

**Vectorization Detection**:
```r
stan_log_lik_general <- function(ll, bterms, threads, ...) {
  require_n <- grepl(stan_nn_regex(), ll$args)  # Needs [n] indexing?
  n <- str_if(require_n, stan_nn(threads), stan_slice(threads))
  # ...
  if (grepl(stan_nn_regex(), out) && !nzchar(mix)) {
    # Loop over likelihood if it cannot be vectorized
    out <- paste0(
      "  for (n in 1:N", resp, ") {\n",
      stan_nn_def(threads),
      "  ", out,
      "  }\n"
    )
  }
}
```

**mvgam Missing Data Pattern**:
```stan
// brms produces mu and mu_trend for full time grid
mu_combined = mu + mu_trend;

// Likelihood only for non-missing observations
{
  vector[n_nonmissing] selected_mu = mu_combined[obs_ind];
  flat_ys ~ family_distribution(selected_mu, ...);
}

// Trends evolve over ALL timesteps (including missing)
trend[2:T] ~ normal(trend[1:(T-1)], sigma_trend);
```

### 5. Multivariate Model Architecture

**Pattern**: brms handles multivariate models by **combining separate linear predictors** in the model block, using response arrays and correlation structures.

**Multivariate Assembly**:
```stan
transformed data {
  array[N] vector[nresp] Y;  // Response array
  for (n in 1:N) {
    Y[n] = transpose([Y_tarsus[n], Y_back[n]]);
  }
}

model {
  // Separate predictor initialization
  vector[N_tarsus] mu_tarsus = rep_vector(0.0, N_tarsus);
  vector[N_back] mu_back = rep_vector(0.0, N_back);
  
  // Individual predictor building
  mu_tarsus += Intercept_tarsus + Xc_tarsus * b_tarsus;
  mu_back += Intercept_back + Xc_back * b_back;
  
  // Combine into multivariate array
  array[N] vector[nresp] Mu;
  for (n in 1:N) {
    Mu[n] = transpose([mu_tarsus[n], mu_back[n]]);
  }
  
  // Multivariate likelihood
  target += multi_normal_cholesky_lpdf(Y | Mu, LSigma);
}
```

**mvgam State-Space Extension**:
- **Generate separate trend predictors**: `mu_trend_tarsus`, `mu_trend_back`
- **Response-specific trend types**: Allow different dynamics per response
- **Preserve correlation structure**: brms handles residual correlations, mvgam adds State-Space dynamics

### 6. Distributional Parameter Handling

**Pattern**: brms treats **all parameters as potential linear predictors**, enabling distributional regression where any parameter can vary with predictors.

**Distributional Architecture**:
```stan
model {
  // mu parameter (main)
  vector[N] mu = rep_vector(0.0, N);
  mu += Intercept + Xc * b;
  
  // sigma parameter (distributional)
  vector[N] sigma = rep_vector(0.0, N);
  sigma += Intercept_sigma + Xc_sigma * b_sigma;
  sigma = exp(sigma);  // Apply link function
  
  target += normal_lpdf(Y | mu, sigma);
}
```

**mvgam Trend Restriction**:
- **Trends only for main parameter** (`mu`): State-Space dynamics typically model the mean process
- **Validate distributional models**: Prevent trends on auxiliary parameters like `sigma`, `zi`
- **Clear error messages**: Guide users on appropriate trend placement

### 7. Link Function and Family Integration

**Pattern**: brms has **comprehensive family support** with automatic link function handling and built-in Stan optimizations.

**Family-Specific Optimizations**:
```r
# Automatic optimization selection
if (use_glm_primitive(bterms)) {
  # Use efficient GLM primitives when possible
  p <- args_glm_primitive(bterms$dpars$mu, ...)
  out <- sdist("poisson_log_glm", p$x, p$alpha, p$beta)
} else {
  # Standard parameterization
  p <- stan_log_lik_dpars(bterms)
  lpdf <- stan_log_lik_simple_lpdf("poisson", bterms)
  out <- sdist(lpdf, p$mu)
}
```

**mvgam Family Integration**:
- **Leverage all brms families**: No need to reimplement observation models
- **Preserve optimizations**: Let brms choose GLM primitives where appropriate
- **Focus on State-Space**: mvgam adds temporal dynamics to any brms family

### 8. Prior Integration and Validation

**Pattern**: brms has **sophisticated prior handling** with automatic transformations and validation.

**Prior Assembly**:
```stan
transformed parameters {
  real lprior = 0;  // Prior contributions to the log posterior
  lprior += student_t_lpdf(Intercept_tarsus | 3, 0.1, 2.5);
  lprior += student_t_lpdf(sigma_tarsus | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);  // Jacobian adjustment
  lprior += lkj_corr_cholesky_lpdf(Lrescor | 1);
}

model {
  // ... likelihood ...
  target += lprior;  // Add prior contributions
}
```

**mvgam Prior Strategy**:
- **Reuse brms prior system** for observation parameters
- **Add trend-specific priors** through stanvars
- **Maintain prior validation** and transformation patterns

## Implementation Recommendations

### 1. Two-Stage Stan Assembly with Data Integration

**Recommended Approach**:
1. **Stage 1**: Use brms to generate base Stan code and data with trend stanvars
2. **Stage 2**: Post-process to inject trend effects into linear predictors

```r
# Generate trend data and code stanvars together
trend_stanvars <- generate_trend_stanvars_complete(trend_obj, data_info)

# Let brms build the base model with trend data
base_stancode <- brms::stancode(obs_formula, data, stanvars = trend_stanvars)
base_standata <- brms::standata(obs_formula, data, stanvars = trend_stanvars)

# Inject trend effects (code modification)
final_stancode <- inject_trend_into_linear_predictor(base_stancode, trend_spec)

# Combined Stan data already includes trend-specific data via stanvars
final_standata <- base_standata  # No modification needed
```

**Data Generation Strategy**: Leverage brms's modular data system:
```r
generate_trend_stanvars_complete <- function(trend_obj, data_info) {
  # Generate both data and code stanvars together
  data_stanvars <- generate_trend_data_stanvars(trend_obj, data_info)
  code_stanvars <- generate_trend_code_stanvars(trend_obj, data_info)
  
  return(c(data_stanvars, code_stanvars))
}
```

### 2. Non-Centered Parameterization Pattern

**Follow brms Patterns**:
```stan
parameters {
  // Raw innovations (non-centered)
  matrix[n, n_lv] LV_raw;
  vector<lower=0>[n_lv] sigma;
}

transformed parameters {
  // Transform to actual latent variables
  matrix[n, n_lv] LV;
  LV = LV_raw .* rep_matrix(sigma', rows(LV_raw));
  
  // Apply State-Space evolution
  for (j in 1:n_lv) {
    LV[1, j] += trend_mus[ytimes_trend[1, j]];
    for (i in 2:n) {
      LV[i, j] += trend_mus[ytimes_trend[i, j]] + 
                  ar1[j] * (LV[i - 1, j] - trend_mus[ytimes_trend[i - 1, j]]);
    }
  }
}

model {
  // Sample only innovations (efficient)
  to_vector(LV_raw) ~ std_normal();
  sigma ~ inv_gamma(1.418, 0.452);
}
```

### 3. Validation Framework Integration

**Leverage brms Validation Patterns**:
```r
validate_autocor_usage <- function(formula, trend_formula) {
  # Allow brms autocorrelation in observation formulas
  obs_autocor <- extract_autocor_terms(formula)
  if (length(obs_autocor) > 0) {
    insight::format_warning(
      "Using brms autocorrelation for observation-level residual structure.",
      "This models residual correlation and complements State-Space trends.",
      .frequency = "once"
    )
  }
  
  # Prevent conflicts in trend formulas
  trend_autocor <- extract_autocor_terms(trend_formula)
  if (length(trend_autocor) > 0) {
    stop(insight::format_error(
      "brms autocorrelation terms not allowed in trend_formula.",
      "Use mvgam trend types: ar(p = 1) → AR(p = 1)"
    ))
  }
}
```

### 4. Multivariate Integration Strategy

**Response-Specific Trends**:
```r
# Parse multivariate trend specifications
mv_spec <- parse_multivariate_trends(
  formula = bf(mvbind(count, biomass) ~ temp + precip),
  trend_formula = bf(
    count ~ AR(p = 1),        # Population dynamics
    biomass ~ RW(cor = TRUE), # Correlated biomass trends
    presence ~ NULL           # No trend (static occupancy)
  )
)

# Generate response-specific stanvars
for (resp in mv_spec$responses) {
  if (!is.null(mv_spec$trends[[resp]])) {
    trend_stanvars <- c(trend_stanvars, 
      generate_trend_stanvars(mv_spec$trends[[resp]], resp))
  }
}
```

### 5. Data Validation and Threading Integration

**brms Data Validation Pattern**: brms has comprehensive data validation that mvgam should leverage:
```r
# brms validation workflow
standata.default <- function(object, data, family, ...) {
  # 1. Formula validation
  object <- validate_formula(object, data, family, ...)
  
  # 2. Data validation  
  data <- validate_data(data, bterms, knots, data2, ...)
  
  # 3. Frame validation
  bframe <- brmsframe(bterms, data)
  
  # 4. Generate standata with validation
  .standata(bframe, data, ...)
}
```

**mvgam Data Validation Strategy**:
```r
validate_mvgam_data_integration <- function(trend_data, obs_data, bframe) {
  # Validate trend data compatibility with brms structure
  validate_trend_data_structure(trend_data, bframe)
  
  # Check for name conflicts with brms data
  validate_no_standata_conflicts(trend_data, obs_data)
  
  # Validate time series structure
  validate_time_series_structure(trend_data, bframe)
  
  return(TRUE)
}
```

**Threading Compatibility**: brms supports within-chain parallelization that mvgam must preserve:
```r
# brms threading integration in standata
if (use_threading(threads)) {
  out$grainsize <- threads$grainsize
  if (is.null(out$grainsize)) {
    out$grainsize <- ceiling(out$N / (2 * threads$threads))
    out$grainsize <- max(100, out$grainsize)
  }
}

# stanvars must include pll_args for threading
trend_stanvars <- stanvar(
  scode = "matrix[n, n_lv] ytimes_trend;",
  pll_args = "matrix ytimes_trend"  # Enable threading compatibility
)
```

### 6. Family Extension Points

**Custom Family Integration**:
```r
# mvgam-specific families that extend brms
stan_log_lik_nmix <- function(bterms, ...) {
  # N-mixture model with detection probability
  p <- stan_log_lik_dpars(bterms, reqn = TRUE)
  sdist("nmix", p$mu, p$phi, p$n_sites, vec = FALSE)
}

# Integrate with brms family system
register_family_extension("nmix", stan_log_lik_nmix)
```

## Critical Success Factors

### 1. Minimal brms Code Modification

**Philosophy**: Extend brms through **composition, not modification**
- Use stanvars for all State-Space code injection
- Preserve all existing brms functionality
- Maintain compatibility with brms ecosystem (loo, bayesplot, etc.)

### 2. Intelligent Autocorrelation Separation

**Key Innovation**: First package to properly distinguish observation-level residual correlation from State-Space dynamics
- **brms autocorrelation**: Residual correlation structure (`ar()`, `ma()`, `arma()`)
- **mvgam trends**: State-Space temporal dynamics (`AR()`, `RW()`, `VAR()`)
- **Clear validation**: Prevent conflicts and guide proper usage

### 3. Stan Code and Data Safety

**Validation Strategy**:
- **Syntax validation**: Check generated Stan code compiles
- **Parameter consistency**: Ensure parameter names don't conflict
- **Block organization**: Maintain proper Stan program structure
- **Threading compatibility**: Support brms threading optimizations
- **Data consistency**: Validate trend data integrates seamlessly with brms standata
- **Name conflict prevention**: Ensure stanvars don't overwrite brms data variables

### 4. Ecosystem Integration

**brms Method Compatibility**:
```r
# Ensure all brms methods work with mvgam objects
posterior_predict.mvgam <- function(object, ...) {
  # Combine brms prediction infrastructure with trend effects
  prep <- brms::prepare_predictions(object$obs_fit, ...)
  trend_effects <- predict_trend_effects(object, prep)
  prep <- update_brmsprep_linpred(prep, prep$dpars$mu + trend_effects)
  brms::posterior_predict_draws(prep)
}
```

## Conclusion

The brms architecture provides an excellent foundation for mvgam's State-Space extensions. By leveraging brms's modular code generation, stanvars system, and comprehensive family support, mvgam can focus on its core innovation: adding temporal dynamics to any brms model while preserving full ecosystem compatibility.

The key insight is that **brms handles the observation model complexity** (families, links, priors, multivariate structures) while **mvgam adds the temporal evolution layer** through carefully designed Stan code injection. This division of responsibilities allows both packages to excel in their domains while providing users with unprecedented modeling flexibility.