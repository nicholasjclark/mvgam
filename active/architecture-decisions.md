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
- IMPORTANT: This is a complete overhaul. Avoid backward compatibility unless specifically requested by the user

### 2. Formula-Centric Interface Design
**Decision**: Extend brms formula syntax with `trend_formula` parameter  
**Pattern**:
```r
mvgam(
  formula,              # Observation model
  trend_formula,        # State-Space dynamics
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
**Decision**: Leverage brms stanvars system with enhanced processing layer
**Stage 1**: Generate trend stanvars and trend Stan data in `generate_trend_stanvars()`
**Stage 2**: Post-process observation model Stan code to inject trend effects and create the combined model

**Critical Enhancement**: Stan assembly layer now handles complex logic moved from constructors:
- **Parameter Processing**: `process_trend_params()` called during stanvar generation, not construction
- **Dynamic Characteristics**: Correlation requirements, factor compatibility determined with data context
- **Environment-Dependent Logic**: Grouping variables processed with actual data structure
- **Dispatch Resolution**: Convention-based function lookup replaces hardcoded dispatch fields

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

### 3. Factor Model Architecture: Matrix Z Patterns
**Design Principle**: Factor models are a capability of compatible trend types

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
4. **Matrix Z Location**: Estimated in `parameters` block (factor model) vs `transformed data` (non-factor), or supplied in `data` block when trend mapping
5. **Universal Computation**: All factor models use `trend[i, s] = dot_product(Z[s, :], LV[i, :]) + mu_trend[ytimes[i, s]]`; non-factor models use `trend[i, s] = dot_product(Z[s, :], LV[i, :]);`
6. **Code Deduplication**: Shared utility functions ensure consistent patterns across trend types
7. **Registration**: New trend types must explicitly declare factor compatibility in registry

### 4. Code Deduplication for User Extensibility

**Design Principle**: Eliminate redundant code to simplify custom trend development

### 5. Hierarchical Correlation Architecture

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

### 2. Memory Optimization Targets
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

### 3. Simplified Constructor Architecture (2024 Enhancement)
**Decision**: Trend constructors become minimal object creators using `create_mvgam_trend()`
**Rationale**: Separation of concerns - constructors create, other layers process

**Constructor Pattern**:
```r
# All constructors now follow this pattern:
AR <- function(time = NA, series = NA, p = 1, ma = FALSE, cor = FALSE, gr = NA, subgr = NA, n_lv = NULL) {
  # Basic parameter validation only
  checkmate::assert_int(p, lower = 1)
  checkmate::assert_logical(ma, len = 1)
  checkmate::assert_logical(cor, len = 1)
  
  # Create object with base type
  create_mvgam_trend(
    "AR",  # Base type used for ALL dispatch
    .time = substitute(time),
    .series = substitute(series),
    .gr = substitute(gr),
    .subgr = substitute(subgr),
    p = p,
    ma = ma,
    cor = cor,
    n_lv = n_lv
  )
}
```

**Key Changes**:
- **Base type dispatch**: Always use "AR", "RW", "VAR", "PW" (never "AR1", "VAR2", "PWlinear", etc.)
- **Exported helper**: `create_mvgam_trend()` with consistent dot-prefix parameters (.time, .series, .gr, .subgr)
- **Rule-based validation**: Validation dispatch based on `validation_rules` field automatically assigned
- **Automatic metadata**: Dispatch function names generated via convention (generate_ar_trend_stanvars, forecast_ar_rcpp)
- **No complex logic**: All processing moved to validation/Stan assembly layers

**Complex Logic Migration**:
- **Grouping Processing** → `apply_validation_rules()` in R/validations.R (has data context)
- **Parameter Processing** → `generate_trend_injection_stanvars()` in R/stan_assembly.R (has full context)
- **Correlation Requirements** → validation layer rule-based dispatch
- **Dynamic Characteristics** → Stan assembly layer (has validated data)
- **Dispatch Functions** → convention-based naming eliminates hardcoding

**Standardized Output Pattern**:
```r
# Constructor outputs (always base types for dispatch)
RW()              # trend = 'RW'
AR(p = 1)         # trend = 'AR' (not 'AR1')
AR(p = c(1, 12))  # trend = 'AR' (not 'AR(1,12)')
VAR(p = 2)        # trend = 'VAR' (not 'VAR2')
CAR()             # trend = 'CAR'
PW(growth = 'linear')   # trend = 'PW' (not 'PWlinear')
PW(growth = 'logistic') # trend = 'PW' (not 'PWlogistic')
ZMVN()            # trend = 'ZMVN'

# Base type used for ALL dispatch throughout system
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

### 9. Automatic Parameter Monitoring System

**Critical Design Decision**: Trend objects automatically discover their own monitoring requirements using convention-based parameter generation

**Architecture Pattern**:
```r
# Simple character vector field in trend objects
trend_obj$monitor_params <- c("sigma_trend", "ar1_trend", "ar12_trend", "L_Omega_trend", "Z")

# Automatic discovery function
monitor_params <- generate_monitor_params(trend_spec)
```

**Key Features**:
- **Convention-Based**: Leverages existing "_trend" suffix pattern
- **Type-Specific Generators**: Each trend type has dedicated discovery function
- **Automatic Enhancement**: `add_monitor_params()` enriches trend objects
- **Smart Normalization**: AR1→AR, VAR2→VAR for consistent dispatch
- **Standalone Design**: Clean separation from brms observation parameters

**Implementation (R/trend_system.R)**:
- `generate_monitor_params(trend_spec)` - Core discovery function
- `generate_ar_monitor_params()`, `generate_var_monitor_params()`, etc. - Type-specific generators  
- `normalize_trend_type()` - Handles trend type variations
- `add_monitor_params(trend_obj)` - Enhancement function

**Benefits**:
1. **Zero Manual Specification**: No manual monitor_params maintenance required
2. **Future-Proof**: New trends just add their generator function
3. **Complex Lag Support**: Handles AR(p = c(1,12)) → ar1_trend, ar12_trend automatically
4. **Factor Model Integration**: Automatically includes "Z" when n_lv specified
5. **Correlation Awareness**: Adds L_Omega_trend, Sigma_trend when cor = TRUE

### 10. Ultra-Efficient Forecasting System

**Critical Design Decision**: Forecasting system optimized for maximum runtime speed with minimal storage overhead

**Architecture Pattern**:
```r
# Minimal metadata stored in final mvgam object
mvgam_fit$forecast_dispatch <- list(
  function_name = "forecast_ar_rcpp",               # Convention-based
  required_params = c("ar1_trend", "sigma_trend"), # For lazy extraction
  time_info = list(                                 # For validation/horizon calculation
    last_time = 50,
    time_variable = "month", 
    regular_intervals = TRUE
  )
)
```

**Speed Optimizations**:
- **Lazy Parameter Extraction**: Extract only when forecast() called using pre-stored parameter names
- **Zero Time Overhead**: Pre-computed time structure enables fast newdata validation and horizon calculation
- **Direct Function Dispatch**: Convention-based function names enable immediate calls without registry lookups
- **Memory Efficient**: Store only dispatch metadata, not actual parameter values or duplicate states

**Implementation (R/trend_system.R)**:
- `generate_forecast_metadata(trend_spec)` - Creates dispatch information
- `filter_*_forecast_params()` - Trend-specific parameter filtering for minimal extraction
- Convention: `"AR" → forecast_ar_rcpp()` for consistent naming

**Forecasting Workflow**:
```r
# Ultra-fast forecast execution
forecast.mvgam <- function(object, newdata) {
  params <- extract_parameters_fast(object$fit, object$forecast_dispatch$required_params)
  horizon <- calculate_horizon(newdata, object$forecast_dispatch$time_info)
  do.call(object$forecast_dispatch$function_name, list(params, object$states, horizon))
}
```

**Benefits**:
1. **Near-Zero Runtime Overhead**: Forecast calls are direct function dispatches
2. **Minimal Memory Usage**: No duplicate parameter or state storage
3. **Fast Validation**: Pre-computed time structures for newdata validation
4. **Stan Integration**: Leverages existing monitor_params system for state tracking
5. **Extensible**: New trends just specify function name + required parameters

### 11. brms Stanvar Block Naming Conventions

**brms Uses Abbreviated Names** (used in stanvar objects):
- `"data"` → `data`
- `"tdata"` → `transformed data`  
- `"parameters"` → `parameters`
- `"tparameters"` → `transformed parameters`
- `"model"` → `model`
- `"genquant"` → `generated quantities`

**Key Points**:
1. **stanvar creation**: Use abbreviated names (`"tparameters"`, `"tdata"`)
2. **Validation functions**: Must accept both abbreviated and full names
3. **Stan compilation**: brms automatically converts abbreviated to full names

**Example**:
```r
# ✅ Correct - use abbreviated name
stanvar(name = "test", scode = "real x;", block = "tparameters")

# ❌ Wrong - don't use full name  
stanvar(name = "test", scode = "real x;", block = "transformed parameters")

# ✅ Validation functions should accept both
valid_blocks <- c("tparameters", "transformed_parameters", "tdata", "transformed_data", ...)
```

### brms multivariate data and code generation
  1. Response-specific naming: brms uses `mu_y1`, `mu_y2` (not array indexing)
  2. No `nresp` for mixed families: Only present for multivariate Gaussian with correlations
  3. Linear predictors in model block: Computed as vectors when needed (random effects, etc.)
  4. Suffix pattern: All parameters/data get `_y1`, `_y2` suffixes per response
  5. Independent treatment: Each response can have completely different model structures

## Critical Integration Points

### Data Flow Overview
```
User Input → mvgam() → parse_multivariate_trends() → setup_brms_lightweight() 
→ validate_time_series_for_trends() → extract_time_series_dimensions()
→ trend_specs$dimensions ← dimensions
→ generate_combined_stancode() → extract_trend_stanvars_from_setup() 
→ fit_mvgam_model() → create_mvgam_from_combined_fit()
```

**Critical Validation Flow**:
```
data + trend_specs → validate_time_series_for_trends()
                  → extract_time_series_dimensions() 
                  → dimensions{n_time, n_series, n_obs, time_var, series_var}
                  → trend_specs$dimensions = dimensions
                  → extract_trend_stanvars_from_setup() uses pre-calculated dimensions
```

### Key Data Structures
```r
# Single trend with pre-calculated dimensions
trend_spec = list(
  trend = "AR1", p = 1, cor = TRUE, n_lv = 2, 
  time = "time", series = "series",
  dimensions = list(
    n_time = 50, n_series = 3, n_obs = 150,
    time_var = "time", series_var = "series",
    time_range = c(1, 50), unique_times = 1:50
  )
)

# Multivariate trends with dimensions per response
mv_spec$trend_specs = list(
  count = list(
    trend = "AR1", p = 1,
    dimensions = list(n_time = 50, n_series = 2, n_obs = 100, ...)
  ),
  biomass = list(
    trend = "RW", 
    dimensions = list(n_time = 50, n_series = 2, n_obs = 100, ...)
  )
)
```

### Multivariate Trend Detection Logic

**Design Pattern**: Distinguish between univariate trend specifications and multivariate collections

**Detection Algorithm** (in `is_multivariate_trend_specs()`):
```r
# Univariate: Has trend field (any of these)
trend_specs = list(trend = "RW", ...) 
trend_specs = list(trend_type = "AR", ...)
trend_specs = list(trend_model = "VAR", ...)

# Multivariate: Named list WITHOUT trend fields
trend_specs = list(
  count = list(trend = "RW", ...),
  biomass = list(trend_type = "AR", ...)
)
```

**Key Requirements**:
1. **Field Compatibility**: Supports `trend`, `trend_type`, and `trend_model` field names, but this needs to be streamlined and simplified as we do not need to maintain backward compatibility
2. **Multivariate Recognition**: If a named list lacks any trend-identifying field, it's treated as multivariate
3. **Response-Specific Processing**: Each response gets processed with appropriate response suffix (`_count`, `_biomass`)

### Time Series Dimension Management System

**Design Principle**: Single source of truth for all time series dimensions, eliminating circular dependencies

**Critical Data Flow**:
```r
# Stage 1: Early validation and dimension extraction
data + trend_specs -> validate_time_series_for_trends() -> dimensions
                         |
                         v
                  extract_time_series_dimensions()
                         |
                         v
                    dimensions = {
                      n_time, n_series, n_obs,
                      time_var, series_var,
                      time_range, unique_times
                    }

# Stage 2: Dimension injection into trend specifications  
trend_specs$dimensions <- dimensions

# Stage 3: Trend stanvar generation (no circular dependency)
extract_trend_stanvars_from_setup() -> uses trend_specs$dimensions
```

**Key Architecture Decisions**:

1. **Early Dimension Calculation**: `extract_time_series_dimensions()` calculates all timing information directly from data during validation
2. **No Circular Dependencies**: Eliminates previous pattern where `extract_trend_stanvars_from_setup()` tried to extract `n_obs` from `trend_specs` that didn't have it
3. **Validation Integration**: `validate_time_series_for_trends()` now returns both validated data AND calculated dimensions
4. **CAR Exception Handling**: CAR trends skip regular interval validation but still get basic dimensions

**Dimension Validation Rules**:
- **Non-CAR trends** (RW, AR, VAR, ZMVN, PW): Require regular time intervals using `validate_regular_time_intervals()`
- **CAR trends**: Allow irregular intervals, can calculate time distances dynamically in `calculate_car_time_distances()`
- **All trends**: Must have consistent series and time variable identification

**Implementation Functions**:
```r
# Core dimension extraction (R/validations.R)
extract_time_series_dimensions(data, time_var, series_var, trend_type)

# Validation with dimension return (R/validations.R) 
validate_time_series_for_trends(data, trend_specs) -> list(data, dimensions)

# Trend stanvar generation expecting pre-calculated dimensions (R/stan_assembly.R)
extract_trend_stanvars_from_setup(trend_setup, trend_specs) # trend_specs$dimensions required
```

**Benefits**:
1. **Reliability**: All dimensions calculated from actual data, not missing specification fields
2. **Performance**: Dimensions calculated once during validation, reused everywhere
3. **Maintainability**: Clear separation between validation logic and trend generation
4. **Robustness**: Eliminates missing `n_time`/`n_obs` errors in trend generators

## Enhanced Architecture (2025-08-16)

### 12. Automated Registry System with Convention-Based Dispatch

**Critical Design Decision**: Auto-discovery of trend types eliminates manual registry maintenance while preserving explicit capability declaration.

**Convention Pattern**:
```r
# For trend type "FOO", developers define:
generate_foo_trend_stanvars(trend_specs, data_info)  # Stan code generator
foo_trend_properties() -> list(supports_factors = TRUE/FALSE, incompatibility_reason = "...")
# Zero registration calls needed - auto-discovered on package load
```

**Key Implementation**:
- `auto_register_trend_types()` discovers functions using naming conventions
- `validate_trend_properties()` ensures proper capability declaration with fail-fast validation
- Registry-enhanced error messages in Stan assembly provide helpful guidance
- Manual `register_custom_trend()` available but guides users toward conventions

**Benefits**: Adding new trends requires only 2 functions following clear patterns. No manual registry maintenance, immediate error feedback, future-proof extensibility.

### 13. Enhanced Validation Layer with Rule-Based Dispatch

**Critical Design Decision**: Move complex parameter processing from constructors to validation layer where data context is available.

**Rule-Based Architecture**:
```r
# Trend objects specify validation rules
trend_obj$validation_rules <- c("requires_parameter_processing", "supports_factors")
# Automatic dispatch via apply_validation_rules()
```

**Key Functions**:
- `validate_and_process_trend_parameters()` handles complex parameter processing with data context
- `process_lag_parameters()` sorts and validates AR/VAR lag structures
- `process_capacity_parameter()` validates PW capacity with data column checking
- Rule dispatch table maps rules to validation functions automatically

**Benefits**: Clean separation of concerns, data-aware validation, simplified constructors, extensible rule system.

## Critical Integration Requirements

**Stan Assembly Layer Enhancement**:
- `generate_trend_stanvars()` handles parameter processing moved from constructors
- `validate_time_series_for_trends()` handles grouping/correlation logic with data context  
- Convention-based dispatch: `"AR" → generate_ar_trend_stanvars()` replaces hardcoded fields

**Development Requirements**:
- **Simplified Constructors**: Use `create_mvgam_trend()` helper for all new trend types
- **Enhanced Validation**: Process grouping variables with data context in validation layer
- **Enhanced Stan Assembly**: Handle dynamic characteristics and parameter processing with full context
