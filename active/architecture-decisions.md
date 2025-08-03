# mvgam Architecture Decisions

**Purpose**: Core design principles and patterns guiding implementation

## Foundation Decisions

### 1. Single-Fit Dual-Object Architecture
**Decision**: One Stan fit produces two brmsfit-like objects for post-processing  
**Rationale**: Enables seamless brms ecosystem integration while preserving multivariate correlations  
**Implementation**: 
- brms generates linear predictors (`mu`, `mu_trend`)
- Single combined Stan model with observation + trend components
- Dual objects created from same fit for different aspects

### 2. Formula-Centric Interface Design
**Decision**: Extend brms formula syntax with `trend_formula` parameter  
**Pattern**:
```r
mvgam(
  formula,              # Observation model (standard brms)
  trend_formula,        # State-Space dynamics (mvgam extension)
  data, family, ...
)
```

### 3. Stan Integration Strategy: Two-Stage Assembly
**Decision**: Leverage brms stanvars system rather than modifying brms internals  
**Stage 1**: Generate trend stanvars and let brms handle data integration  
**Stage 2**: Post-process Stan code to inject trend effects into linear predictors

### 4. Autocorrelation Separation Principle
**Critical Innovation**: Distinguish observation-level correlation from State-Space dynamics

**Allowed Combinations**:
```r
# ✅ Observation-level residual correlation + State-Space trends
mvgam(
  count ~ Trt + unstr(visit, patient),  # brms residual structure
  trend_formula = ~ AR(p = 1),          # mvgam temporal dynamics
  data = data
)
```

**Forbidden Patterns**:
```r
# ❌ brms autocorr in trend formula (conflicting temporal models)
mvgam(
  y ~ s(x),
  trend_formula = ~ s(time) + ar(p = 1),  # CONFLICT
  data = data
)
```

## Stan Code Design Patterns

### 1. Non-Vectorized Likelihood with Missing Data
**Pattern**: Follow brms non-vectorized likelihood handling for missing observations
```stan
// Trends evolve over ALL timesteps (including missing)
trend[2:T] ~ normal(trend[1:(T-1)], sigma_trend);

// Likelihood only for non-missing observations
{
  vector[n_nonmissing] selected_mu = mu_combined[obs_ind];
  flat_ys ~ family_distribution(selected_mu, ...);
}
```

### 2. Response-Specific Parameter Naming
**Pattern**: Follow brms multivariate naming conventions
```stan
// Multivariate responses generate response-specific parameters
vector[N_count] mu_count = rep_vector(0.0, N_count);
vector[N_biomass] mu_biomass = rep_vector(0.0, N_biomass);

// Trend effects with matching names
vector[N_count] mu_trend_count;
vector[N_biomass] mu_trend_biomass;

// Combined effects
mu_count += mu_trend_count;
mu_biomass += mu_trend_biomass;
```

### 3. Non-Centered Parameterization Standard
**Decision**: Always use non-centered parameterization for trends
```stan
parameters {
  matrix[n, n_lv] LV_raw;          // Raw innovations
  vector<lower=0>[n_lv] sigma;     // Scaling parameters
}

transformed parameters {
  matrix[n, n_lv] LV;
  LV = LV_raw .* rep_matrix(sigma', rows(LV_raw));
  // Apply State-Space evolution...
}
```

### 4. Factor Model Architecture: Matrix Z Patterns
**Critical Design**: Factor models are a capability of compatible trend types, not a separate trend type

**Factor Model Detection Logic**:
- **Trigger**: Presence of `n_lv` parameter in trend specification
- **Validation**: Only factor-compatible trend types can accept `n_lv` parameter
- **Requirement**: `n_lv < n_series` (fewer latent variables than observed series)
- **Compatible Trends**: AR, RW, VAR (including correlated variants)
- **Incompatible Trends**: CAR (spatial structure), None (no dynamics)

**Non-Factor Model Pattern** (no `n_lv` specified or `n_lv >= n_series`):
```stan
transformed data {
  // Diagonal matrix Z for non-factor model
  matrix[n_series, n_lv] Z = diag_matrix(rep_vector(1.0, n_lv));
}

transformed parameters {
  matrix[n, n_series] trend;
  
  // Derived latent trends with no factor model
  // Z must be diagonal matrix in transformed data
  // mu_trend captures linear predictors from trend_formula
  for (i in 1:n) {
    for (s in 1:n_series) {
      trend[i, s] = dot_product(Z[s, :], LV[i, :]) + mu_trend[ytimes[i, s]];
    }
  }
}
```

**Factor Model Pattern** (`n_lv` specified and `n_lv < n_series`):
```stan
parameters {
  // Loading matrix Z for factor model
  matrix[n_series, n_lv] Z;
  // Raw latent states (variances fixed to 1)
  matrix[n, n_lv] LV_raw;
}

transformed parameters {
  matrix[n, n_lv] LV;
  matrix[n, n_series] trend;
  
  // Apply dynamics with fixed variance = 1
  LV = LV_raw;
  for (j in 1:n_lv) {
    for (i in 2:n) {
      LV[i, j] = LV[i-1, j] + LV[i, j];
    }
  }
  
  // Derived latent trends with factor model
  // Z must be estimated in parameters
  // mu_trend captures linear predictors from trend_formula
  for (i in 1:n) {
    for (s in 1:n_series) {
      trend[i, s] = dot_product(Z[s, :], LV[i, :]) + mu_trend[ytimes[i, s]];
    }
  }
}

model {
  // Priors for factor model innovations (variance = 1)
  to_vector(LV_raw) ~ std_normal();
  // Priors for loading matrix Z
  to_vector(Z) ~ normal(0, 1);
}
```

**Key Factor Model Requirements**:
1. **Detection**: Factor models triggered by `n_lv < n_series` on compatible trend types
2. **Validation**: Registry-based compatibility checking prevents invalid factor models
3. **Variance Constraint**: Dynamic factor variances must be fixed to 1 for identifiability
4. **Matrix Z Location**: Estimated in `parameters` block (factor model) vs `transformed data` (non-factor)
5. **Universal Computation**: All trends use `trend[i, s] = dot_product(Z[s, :], LV[i, :]) + mu_trend[ytimes[i, s]]`
6. **Code Deduplication**: Shared utility functions ensure consistent patterns across trend types
7. **Registration**: New trend types must explicitly declare factor compatibility in registry

### 5. Code Deduplication for User Extensibility

**Design Principle**: Eliminate redundant code patterns to simplify custom trend development

**Shared Utility Functions** (`R/trend_injection_generators.R`):
```r
# Matrix Z generation based on factor model status
generate_matrix_z_stanvars(is_factor_model, n_lv, n_series)

# Universal trend computation pattern 
generate_trend_computation_code(n_lv, n_series)

# Factor model priors with variance constraints
generate_factor_model_priors(is_factor_model, n_lv)

# Hierarchical correlation support (NEW)
generate_hierarchical_functions()
generate_hierarchical_correlation_params(n_groups, n_subgroups)
generate_hierarchical_correlation_priors(n_groups)
```

### 6. Hierarchical Correlation Architecture

**Design Principle**: All trends (AR, VAR, CAR, ZMVN) support hierarchical correlations with groups and subgroups

**Hierarchical Correlation Detection Logic**:
- **Trigger**: Presence of `gr` parameter in trend specification (`trend_spec$gr != 'NA'`)
- **Structure**: Global correlation + group-specific deviations with partial pooling
- **Compatibility**: Works with both factor and non-factor models
- **Universal Support**: AR, VAR, CAR, and ZMVN all use identical hierarchical patterns

**Hierarchical Correlation Pattern**:
```stan
// Shared hierarchical correlation functions
functions {
  matrix combine_cholesky(matrix global_chol_cor, matrix local_chol_cor, real alpha) {
    // Combines global and group-specific correlations with mixing parameter alpha
  }
}

parameters {
  // Global correlation structure (shared across all groups)
  cholesky_factor_corr[n_subgroups] L_Omega_global;
  // Group-specific correlation deviations  
  array[n_groups] cholesky_factor_corr[n_subgroups] L_deviation_group;
  // Mixing parameter for hierarchical correlation (0=local, 1=global)
  real<lower=0, upper=1> alpha_cor;
}

model {
  // Derived hierarchical correlation matrices
  array[n_groups] cholesky_factor_corr[n_subgroups] L_Omega_group;
  for (g in 1:n_groups) {
    L_Omega_group[g] = combine_cholesky(L_Omega_global, L_deviation_group[g], alpha_cor);
  }
  
  // Apply group-specific correlations to trend dynamics
  for (i in 1:n_unit) {
    for (g in 1:n_groups) {
      // Trend-specific dynamics with hierarchical correlation structure
      to_vector(LV[group_unit_indices[i, g]]) ~ multi_normal_cholesky(mu, L_Omega_group[g]);
    }
  }
  
  // Hierarchical correlation priors
  alpha_cor ~ beta(3, 2);                         // Favor global structure
  L_Omega_global ~ lkj_corr_cholesky(1);          // Weakly informative global prior
  for (g in 1:n_groups) {
    L_deviation_group[g] ~ lkj_corr_cholesky(6);  // More regularized group deviations
  }
}
```

**Key Hierarchical Requirements**:
1. **Universal Support**: All trends (AR, VAR, CAR, ZMVN) support hierarchical correlations
2. **Code Deduplication**: Shared utility functions ensure consistent hierarchical patterns
3. **Flexible Grouping**: Works with any unit variable (time, site, etc.) and grouping structure
4. **Factor Compatibility**: Hierarchical correlations work with both factor and non-factor models
5. **Partial Pooling**: Alpha parameter controls mixing between global and group-specific correlations

**Benefits for Custom Trend Development**:
- **Consistency**: All factor-compatible trends use identical matrix Z patterns
- **Simplicity**: Custom trends call utility functions rather than duplicating Stan code
- **Maintainability**: Bug fixes and improvements apply to all trend types automatically
- **Documentation**: Utility functions contain detailed comments explaining WHY patterns exist

**Custom Trend Template**:
```r
generate_custom_trend_stanvars <- function(trend_spec, data_info) {
  # Extract parameters
  n_lv <- trend_spec$n_lv %||% 1
  n_series <- data_info$n_series %||% 1
  is_factor_model <- !is.null(trend_spec$n_lv) && n_lv < n_series
  
  # Use shared utilities for consistent patterns
  stanvars <- list()
  stanvars <- c(stanvars, generate_matrix_z_stanvars(is_factor_model, n_lv, n_series))
  
  # Add custom trend-specific dynamics here
  stanvars$custom_dynamics <- stanvar(...)
  
  # Use shared trend computation and priors
  stanvars <- c(stanvars, generate_trend_computation_code(n_lv, n_series))
  if (is_factor_model) {
    stanvars <- c(stanvars, generate_factor_model_priors(is_factor_model, n_lv))
  }
  
  return(stanvars)
}
```

## Validation Framework Principles

### 1. Context-Aware Validation
**Principle**: Validation behavior depends on model context (multivariate, distributional, etc.)

### 2. Distributional Model Restrictions
**Rule**: Trends only apply to main response parameter (`mu`)
**Validation**: Prevent trends on auxiliary parameters (`sigma`, `zi`, `hu`)

### 3. Fast-Fail Strategy
**Principle**: Validate during model setup, not Stan compilation
**Implementation**: Check trend compatibility, formula conflicts, data structure early

## Performance Requirements

### 1. brms Setup Optimization
**Decision**: Use `backend = "mock"` for lightweight brms setup (10-50x speedup)
**Application**: All internal brms calls for stancode/standata generation

### 2. Registry Lookup Performance
**Target**: <1ms overhead for trend type dispatch
**Fallback**: Direct function calls if registry becomes bottleneck

### 3. Memory Optimization Targets
- 30-50% reduction in object sizes through compressed storage
- Efficient missing data handling without redundant copies
- Shared parameter storage across dual objects

## Multiple Imputation Architecture

### 1. Native Support Decision
**Decision**: Build multiple imputation into core mvgam rather than external wrapper
**Benefits**: Seamless Rubin's rules pooling, integrated LFO-CV, consistent interfaces

### 2. Pooling Strategy
**Approach**: Pool at parameter level using Rubin's rules
**Implementation**: 
- Individual fits stored with pooled summary object
- All methods (predict, loo, etc.) work with pooled object
- Access to individual fits preserved for diagnostics

## Ecosystem Integration Principles

### 1. brms Method Compatibility
**Requirement**: All brms ecosystem methods must work with mvgam objects
**Strategy**: Dual brmsfit-like objects enable method dispatch to brms infrastructure

### 2. Stan Optimization Preservation
**Principle**: Preserve all brms Stan optimizations (GLM primitives, threading, etc.)
**Implementation**: Let brms handle observation model complexity entirely

### 3. Backward Compatibility
**Policy**: Maintain compatibility with existing mvgam interfaces where possible
**Exception**: Breaking changes allowed only when essential for brms integration
