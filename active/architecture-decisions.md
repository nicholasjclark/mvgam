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
