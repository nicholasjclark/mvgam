# mvgam Architecture Decisions

**Purpose**: Core design principles and patterns guiding implementation

## Foundation Decisions

### 1. Single-Fit Dual-Object Architecture
**Decision**: One mvgam object containing two brmsfit-like objects for post-processing  
**Rationale**: Enables seamless brms ecosystem integration while preserving multivariate correlations  
**Implementation**: 
- brms generates linear predictors (`mu`, `mu_trend`) from two formulae
- Stan models from two brmsfit objects (using `backend = "mock"`) are combined to contain observation + trend components
- Combined Stan model passed to Stan for joint estimation

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

**Trend Formula Logic**:
- **Missing or NULL `trend_formula`**: Pure brms model (no trend components added)
- **Present `trend_formula`**: mvgam State-Space model with trend dynamics

**Default Trend Behavior**:
```r
# Pure brms - no trend components
mvgam(y ~ x1 + x2, data = data)
mvgam(y ~ x1 + x2, trend_formula = NULL, data = data)

# mvgam with default ZMVN() multivariate Gaussian latent states
mvgam(y ~ x1 + x2, trend_formula = ~ -1, data = data)
mvgam(y ~ x1 + x2, trend_formula = ~ 1, data = data)

# mvgam with explicit trend specification
mvgam(y ~ x1 + x2, trend_formula = ~ AR(), data = data)
```

### 3. Stan Integration Strategy: Two-Stage Assembly
**Decision**: Leverage brms stanvars system rather than modifying brms internals  
**Stage 1**: Generate trend stanvars and trend Stan data  
**Stage 2**: Post-process observation model Stan code to inject trend effects and create the combined model

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
- **Compatible Trends**: AR, RW, VAR, ZMVN (including correlated and grouped variants)
- **Incompatible Trends**: CAR (continuous time), PW (piecewise)

**Key Factor Model Requirements**:
1. **Detection**: Factor models triggered by `n_lv < n_series` on compatible trend types
2. **Validation**: Registry-based compatibility checking prevents invalid factor models
3. **Variance Constraint**: Dynamic factor variances must be fixed to 1 for identifiability
4. **Matrix Z Location**: Estimated in `parameters` block (factor model) vs `transformed data` (non-factor), or supplied in 'data' block when trend mapping
5. **Universal Computation**: All factor models use `trend[i, s] = dot_product(Z[s, :], LV[i, :]) + mu_trend[ytimes[i, s]]`; non-factor models use `trend[i, s] = dot_product(Z[s, :], LV[i, :]);`
6. **Code Deduplication**: Shared utility functions ensure consistent patterns across trend types
7. **Registration**: New trend types must explicitly declare factor compatibility in registry

### 5. Code Deduplication for User Extensibility

**Design Principle**: Eliminate redundant code to simplify custom trend development

**Shared Utility Functions** (`R/trend_injection_generators.R`)

### 6. Hierarchical Correlation Architecture

**Design Principle**: Some trends (AR, VAR, CAR, ZMVN) support hierarchical correlations with groups and subgroups

**Hierarchical Correlation Detection Logic**:
- **Trigger**: Presence of `gr` parameter in trend specification (`trend_spec$gr != 'NA'`)
- **Structure**: Global correlation + group-specific deviations with partial pooling
- **Compatibility**: Works with factor and non-factor models
- **Universal Support**: AR, VAR, CAR, and ZMVN all use identical hierarchical patterns

**Key Hierarchical Requirements**:
1. **Support**: Some trends (AR, VAR, CAR, ZMVN) support hierarchical correlations
2. **Code Deduplication**: Shared utility functions ensure consistent hierarchical patterns
3. **Flexible Grouping**: Works with any unit variable (time, site, etc.) and grouping structure
4. **Partial Pooling**: Alpha parameter controls mixing between global and group-specific correlations

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
**Decision**: Use `backend = "mock"` for lightweight brms setup
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

### 3. Trend Constructor Interface Standardization
**Decision**: All trend constructors output only a `trend` field, removing redundant `trend_model` field
**Rationale**: Eliminates interface inconsistencies and simplifies trend type identification

**Standardized Output Pattern**:
```r
# All constructors now return consistent trend field
RW()              # trend = 'RW'
AR(p = 1)         # trend = 'AR1' 
AR(p = c(1, 12))  # trend = 'AR(1,12)'
VAR(p = 2)        # trend = 'VAR2'
CAR()             # trend = 'CAR'
GP()              # trend = 'GP'
PW(growth = 'linear')   # trend = 'PWlinear'
PW(growth = 'logistic') # trend = 'PWlogistic'
ZMVN()            # trend = 'ZMVN'
```

### 4. Variable Name Management Architecture

**Critical Design Decision**: Store variable names in final mvgam object for clean downstream processing
```r
# Stored in final mvgam object
variable_info = list(
  trend = list(
    series = "actual_series_col",    # from trend constructor
    time = "actual_time_col",        # from trend constructor  
    gr = "group_col",                # NULL if not specified
    subgr = "subgroup_col"           # NULL if not specified
  ),
  response = list(
    y = c("count", "biomass"),       # response variable names
    # any other observation-specific variables
  )
)
```

**Implementation Strategy**:
- Validation functions use variable names from trend constructors
- Final mvgam object stores complete variable mapping
- Post-processing methods reference `variable_info`

### 5. Data Ordering Architecture

**Critical Design Decision**: Explicit data ordering for Stan without data pollution

**Clean Ordering Strategy**:
- **Separation of Concerns**: Keep validation logic separate from ordering logic
- **No Data Pollution**: Never add tracking columns to the user's data
- **Explicit Stan Requirements**: Data must be ordered by series then time for efficient Stan computation
- **Metadata Preservation**: Store ordering information separately for post-processing restoration
- **Single Point of Truth**: One function responsible for Stan data ordering

### 6. Data Validation System for Trends
**Entry Point**: `validate_time_series_for_trends()` calls `validate_series_time()` using variable names from trend constructors  
**Core Validation**: `validate_series_time()` ensures time/series exist as factors, then calls `validate_grouping_structure()` for gr/subgr  
**Factor Management**: `validate_factor_levels()` checks for unused levels; `validate_complete_grouping()` ensures hierarchical consistency  
**Stan Preparation**: `prepare_stan_data()` auto-drops unused levels and orders data (series-first, time-within-series) for efficient computation

### 7. Backward Compatibility
**Policy**: Maintain compatibility with existing mvgam interfaces where possible
**Exception**: Breaking changes allowed only when essential for brms integration

### 7. Stanvars Combination Architecture

**Critical Design Decision**: All trend generators must return proper brms "stanvars" class objects

**Key Requirements**:
1. **Never use** `stanvars$name <- brms::stanvar(...)` pattern
2. **Always create** individual stanvar variables
3. **Always use** `combine_stanvars()` for combining
4. **Always validate** that returned object has class "stanvars"

### 8. Trend Parameter Naming Convention

**Critical Design Decision**: Trend parameters (variances, ar parameters, etc..) must avoid naming conflicts with brms observation model parameters

**Naming Convention**:
```stan
// Trend variance parameters - use _trend suffix
vector<lower=0>[n_lv] sigma_trend;     // Univariate trend variances
matrix[n_lv, n_lv] Sigma_trend;        // Multivariate trend covariance

// NOT allowed - conflicts with brms parameters
vector<lower=0>[n_lv] sigma;           // CONFLICTS with gaussian family
matrix[n_lv, n_lv] Sigma;              // CONFLICTS with multivariate families
```

**Standardized Parameter Names**:
- **Trend variances**: `sigma_trend`
- **Trend covariances**: `Sigma_trend` (full covariance) and `L_Omega_trend` (Cholesky factor)
- **AR coefficients**: `ar{lag}_trend` (never just `ar` which conflicts with brms)
- **VAR coefficients**: `A{lag}_trend`
- **Factor loadings**: `Z` (matrix), `z_loading` (vectors)

### 9. brms Stanvar Block Naming Conventions

**Critical Implementation Detail**: brms uses abbreviated block names internally that differ from full Stan block names

**brms Abbreviated Names** (used in stanvar objects):
- `"data"` → `data`
- `"tdata"` → `transformed data`  
- `"parameters"` → `parameters`
- `"tparameters"` → `transformed parameters`
- `"model"` → `model`
- `"genquant"` → `generated quantities`

**Key Points**:
1. **stanvar creation**: Use abbreviated names (`"tparameters"`, `"tdata"`)
2. **Validation functions**: Must accept both abbreviated and full names
3. **Test expectations**: Should expect abbreviated names in stanvar objects
4. **Stan compilation**: brms automatically converts abbreviated to full names
5. **Don't manually convert**: Let brms handle the name conversion internally

**Example**:
```r
# ✅ Correct - use abbreviated name
stanvar(name = "test", scode = "real x;", block = "tparameters")

# ❌ Wrong - don't use full name  
stanvar(name = "test", scode = "real x;", block = "transformed parameters")

# ✅ Validation functions should accept both
valid_blocks <- c("tparameters", "transformed_parameters", "tdata", "transformed_data", ...)
```

## Developer Onboarding Guide

**Consolidated Architecture Files (4 Core Files):**
- `R/trend_system.R` - Complete trend infrastructure (registry, parsing, constructors)
- `R/stan_assembly.R` - Two-stage Stan assembly orchestration
- `R/brms_integration.R` - Enhanced brms setup and ecosystem integration
- `R/mvgam_core.R` - Enhanced fitting, dual-object system, multiple imputation
- `R/validations.R` - Type checks and argument validations

**Test Infrastructure:**
- `tests/testthat/test-trend-dispatcher.R` - Trend system validation
- `tests/testthat/test-brms-setup.R` - brms integration testing
- `tests/testthat/test-stan-assembly-system.R` - Stan assembly validation

**Architecture Documentation:**
- `active/current-sprint.md` - Current status and achievements  
- `active/architecture-decisions.md` - Core design principles
- `active/quick-reference.md` - Developer quick start guide
