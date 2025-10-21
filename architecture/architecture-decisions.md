# mvgam Architecture Decisions

**Purpose**: Core design principles and patterns guiding implementation

## Foundation Decisions

### 1. Single-Fit Lazy Categorization Architecture
- mvgam object stores complete stanfit from combined model in `fit` slot
- Parameter categorization computed on-demand at extraction time
- Simple `exclude` field for parameter filtering
- Pattern: Store complete, categorize lazily, filter on demand
- brms generates linear predictors (`mu`, `mu_trend`) from two formulae
- Stan models from two brmsfit objects (using `backend = "mock"`) are combined
- Combined Stan model passed to Stan for joint estimation

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

### 4. Parameter Extraction and Injection System
- Comprehensive parameter renaming system
- Systematic `_trend` suffix application to all trend model parameters/data

**Implementation**:
- `extract_and_rename_trend_parameters()` in `R/stan_assembly.R`: Processes brms trend models and renames all parameters/data with `_trend` suffix
- `create_times_trend_matrix()`: Generates 2D integer arrays using explicit Stan syntax `int times_trend[n_time, n_series];`  
- Stanvars collection architecture: Returns proper brms `stanvars` collections compatible with `c()` combination method
- Reserved word filtering: Excludes 432 Stan reserved words from renaming

**Critical Integration Points**:
- `generate_combined_stancode()` workflow: Parameter extraction integrated between Stage 1 (trend generation) and Stage 2 (injection)
- Bidirectional parameter mapping: Maintains prediction compatibility with original brms parameter structure
- Multivariate support: Handles both shared trends and response-specific trend patterns

### 4. Centralized Prior Resolution System
**Decision**: Single helper function for all trend parameter priors across all trend types  
**Pattern**:
```r
# In any trend generator
sigma_prior <- get_trend_parameter_prior(prior, "sigma_trend")
ar1_prior <- get_trend_parameter_prior(prior, "ar1_trend")
stan_code <- glue("sigma_trend ~ {sigma_prior}; ar1_trend ~ {ar1_prior};")
```

**Resolution Strategy**:
1. **User specification first**: Check brmsprior object for custom prior
2. **Common default fallback**: Use `common_trend_priors` defaults
3. **Empty string**: Let Stan use its built-in defaults

**Implementation**:
- `get_trend_parameter_prior()` in `R/priors.R` provides centralized resolution
- `common_trend_priors` object defines shared parameters (sigma_trend, ar1_trend, LV, Z, etc.)
- **Parameter Processing**: `process_trend_params()` called during stanvar generation, not construction
- **Dynamic Characteristics**: Correlation requirements, factor compatibility determined with data context
- **Environment-Dependent Logic**: Grouping variables processed with actual data structure

### 5. S3 Generic Pattern for Inspection Functions
**Decision**: Create mvgam-owned S3 generics with explicit brms delegation
**Implementation**: 
```r
get_prior <- function(object, ...) UseMethod("get_prior")
get_prior.formula <- function(object, ...) brms::get_prior(object, ...)
get_prior.mvgam_formula <- function(object, data, ...) { /* trend integration */ }
```

### 6. Autocorrelation Separation Principle
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

### 1. Response-Specific Parameter Naming
**Pattern**: Follow brms multivariate naming conventions
```r
// Trend effects with matching names
vector[N_count] mu_count_trend;
vector[N_biomass] mu_biomass_trend;

// Combined effects
mu_count += mu_count_trend;
mu_biomass += mu_biomass_trend;
```

### 2. Factor Model Architecture: Matrix Z Patterns
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
5. **Universal Computation**: All trend models use `trend[i, s] = dot_product(Z[s, :], LV[i, :]) + mu_trend[times_trend[i, s]]`
6. **Code Deduplication**: Shared utility functions ensure consistent patterns across trend types
7. **Registration**: New trend types must explicitly declare factor compatibility in registry

### 3. Code Deduplication for User Extensibility

**Design Principle**: Eliminate redundant code to simplify custom trend development

### 4. Hierarchical Correlation Architecture

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

## Ecosystem Integration Principles

### 1. Lazy Parameter Categorization System

**Design Decision**: Store complete stanfit, compute parameter categorization on-demand.

**Storage Pattern**:
```r
mvgam_object$fit <- stanfit  # Complete stanfit
mvgam_object$exclude <- c("lprior", "lp__")  # Parameters to hide
```

**Categorization Patterns** (computed dynamically):
1. **observation**: Fixed effects, random effects, smooths, family parameters (`b_`, `sd_`, `r_`, `s_`, `sigma`, `shape`, `nu`, `phi`)
2. **trend**: Trend states, innovations, AR/VAR coefficients, correlations (`_trend` suffix, `trend[`, `lv_trend`, `mu_trend`)
3. **factor_loadings**: Loading matrices and vectors (`Z[`, `z_loading`)
4. **diagnostic**: Log posterior and prior (`lp__`, `lprior`)

**Lazy Evaluation Pattern**:
```r
# Variables method delegates to fit
variables.mvgam <- function(x, ...) {
  variables(x$fit, ...)
}

# Categorization computed when needed
categorize_parameters <- function(x, category = NULL) {
  all_pars <- variables(x$fit)
  all_pars <- setdiff(all_pars, x$exclude)

  if (is.null(category)) {
    return(all_pars)
  }

  # Apply category patterns dynamically
  patterns <- get_category_patterns(category)
  all_pars[grepl(paste(patterns, collapse = "|"), all_pars)]
}

# Parameter extraction applies filtering
as_draws.mvgam <- function(x, variable = NULL, ...) {
  draws <- as_draws(x$fit, ...)
  if (!is.null(variable)) {
    draws <- subset_draws(draws, variable = variable)
  }
  draws <- rename_pars(draws, x)
  draws
}
```

**Benefits**:
- Minimal storage overhead
- Parameter info always current with fit object
- Consistent with ecosystem patterns
- Simplified object construction

### 2. brms Method Compatibility
**Requirement**: All brms ecosystem methods must work with mvgam objects
**Strategy**: mvgam inherits from brmsfit, delegates to fit slot

### 3. Stan Optimization Preservation
**Principle**: Preserve all brms Stan optimizations (GLM primitives, threading, etc.)
**Implementation**: Let brms handle observation model complexity entirely

### 3. Simplified Constructor Architecture
-  Trend constructors become minimal object creators using `create_mvgam_trend()`

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

- **Base type dispatch**: Always use "AR", "RW", "VAR", "PW" (never "AR1", "VAR2", "PWlinear", etc.)
- **Exported helper**: `create_mvgam_trend()` with consistent dot-prefix parameters (.time, .series, .gr, .subgr)
- **Rule-based validation**: Validation dispatch based on `validation_rules` field automatically assigned
- **Automatic metadata**: Dispatch function names generated via convention (generate_ar_trend_stanvars, forecast_ar_rcpp)
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

### 4. Attribute-Based Variable System Architecture

**Design Decision**: Use attribute-based time and series variable storage for universal (time, series) grouping without data contamination.

**Key Principles**:
- **Universal Grouping**: All trend processing uses (time, series) grouping via attribute accessors
- **Zero Contamination**: Original data never modified - variables stored as attributes only
- **Universal Implicit Time**: ALL models use sequential time indices (1, 2, 3, ...) with original time preserved
- **Three Series Strategies**: Explicit, hierarchical, and multivariate series creation
- **Dual Context**: Same logic for fitting (data + trend_formula) and prediction (mvgam_object + newdata)

**Implementation Pattern**:
```r
# Step 1: Create attributes for time and series
data <- ensure_mvgam_variables(data, parsed_trend, time_var, series_var, response_vars)

# Step 2: Universal grouping using accessors
data %>% group_by(
  time = get_time_for_grouping(data),      # Sequential indices (1, 2, 3, ...)
  series = get_series_for_grouping(data)   # Explicit/hierarchical/multivariate
)

# Step 3: Clean up attributes
data <- remove_mvgam_variables(data)
```

**Time Creation Strategy (Universal)**:
- **Implicit Time**: `attr(data, "mvgam_time") <- sequential indices mapped from unique times`
- **Original Preserved**: `attr(data, "mvgam_original_time") <- data[[time_var]]` for CAR distance calculations

**Series Creation Strategies**:
- **Explicit**: `attr(data, "mvgam_series") <- data[[series_var]]`
- **Hierarchical**: `attr(data, "mvgam_series") <- interaction(gr, subgr, sep='_')`
- **Multivariate**: `attr(data, "mvgam_series") <- factor(rep(response_vars, each = n_obs_per_response))`

**Core Functions (R/validations.R)**:
- `ensure_mvgam_variables()`: Creates time and series attributes using appropriate strategy
- `get_time_for_grouping()`: Universal time accessor for all grouping operations
- `get_series_for_grouping()`: Universal series accessor for all grouping operations
- `has_mvgam_variables()`: Checks if attributes are ready for processing
- `remove_mvgam_variables()`: Cleans up all mvgam-related attributes

**Benefits**:
- **Consistent Stan indexing**: All models use sequential time indices
- **CAR model support**: Original time values available for distance matrices  
- **Multivariate series creation**: Handles missing series column by creating from response structure
- **Zero data pollution**: Original data frame completely unmodified
- **Prediction consistency**: Same attribute creation logic for fitting and prediction

### 5. Data Ordering and Validation Architecture

**Data Ordering for Stan**:
- **Attribute-Based Ordering**: Uses `get_time_for_grouping()` and `get_series_for_grouping()` for consistent ordering
- **Zero Data Pollution**: Original data never modified - all processing via attributes
- **Stan Requirements**: Data ordered by series then time using attribute-based indices
- **Metadata Preservation**: Original variable names and ordering stored separately for post-processing
- **Single Point of Truth**: `ensure_mvgam_variables()` responsible for all variable standardization

### 6. Data Validation System for Trends
**Entry Point**: `validate_time_series_for_trends()` uses pre-computed dimensions and attribute-based variables
**Attribute Creation**: `ensure_mvgam_variables()` creates time/series attributes using appropriate strategy (explicit/hierarchical/multivariate)
**Core Validation**: `validate_series_time()` works with attribute-based variables via accessor functions
**Factor Management**: `validate_factor_levels()` and `validate_complete_grouping()` ensure data consistency using attributes
**Stan Preparation**: Data preparation uses attribute-based grouping for efficient Stan computation with original data preservation

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
```r
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

## Stan Code Data Flow Architecture

### **Complete Stan Code Flow**

**Purpose**: Define exactly how trend values are computed and combined with brms observation models in Stan code.

**Stage 1: Trend Value Computation** (in transformed parameters block):
```r
// Universal computation for ALL trend models:
for (i in 1:n_trend) {
  for (s in 1:n_lv_trend) {
    trend[i, s] = dot_product(Z[s, :], lv_trend[i, :]) + mu_trend[times_trend[i, s]];
  }
}
```

**Stage 2: Observation-Trend Integration** (in transformed parameters block):
```r
// Combine brms linear predictor (mu) with mvgam trend effects:
for (n in 1:N) {
  mu[n] += trend[obs_trend_time[n], obs_trend_series[n]];
}
// Result: mu[n] = brms_effects + trend_effects for each observation
```

**Key Data Structures**:
- `trend[n_trend, n_lv_trend]`: Matrix of computed trend values over time and series  
- `times_trend[n_trend, n_lv_trend]`: Maps trend positions to time indices for mu_trend access
- `obs_trend_time[N]`, `obs_trend_series[N]`: Map each brms observation to trend matrix position
- `mu[N]`: brms linear predictor, extended with trend effects
- `mu_trend[...]`: Trend intercepts/means from brms trend model
- `Z[n_series_trend, n_lv_trend]`: Factor loadings matrix
- `lv_trend[n_trend, n_lv_trend]`: Latent variable matrix (dynamic states)

**Critical Design Decision**: The `trend` matrix contains the **final computed trend values** that get added to `mu`. The `mu_trend` array contains **trend intercepts** used during trend computation. Both components are essential for all trend models.

### Enhanced mu_trend Construction System

**Architecture**: Enhanced variable-tracing system that extracts actual brms model block patterns instead of hardcoded simple patterns.

**Two-Path Approach**:
1. **Enhanced Path**: Detects and integrates complex patterns from brms trend_formula (GP predictions, random effects, splines, mixed patterns)
2. **Fallback Path**: Creates simple intercept + coefficient patterns for basic cases

**Pattern Categories**:
- **Declared Parameters**: Variables declared in parameters/data blocks (e.g., `Intercept`, `b`, `Xs`) - mapped during block processing
- **Computed Variables**: Variables computed in transformed parameters blocks (e.g., `gp_pred_1 = gp_exp_quad(...)`, `r_1_1 = sd_1 * z_1`) - require special mapping handling

**Examples**:
```stan
// Simple pattern (fallback path):
mu_trend = rep_vector(Intercept_trend, N_trend);

// Complex pattern (enhanced path):
mu_trend = rep_vector(0.0, N_trend);
mu_trend += Intercept_trend + Xs_trend * bs_trend + Zs_1_1_trend * s_1_1_trend;  // Splines
mu_trend += Intercept_trend + gp_pred_1_trend[Jgp_1_trend];  // GP predictions
mu_trend += Intercept_trend + r_1_1_trend[J_1_trend[n]] * Z_1_1_trend[n];  // Random effects
```

**Implementation**: `extract_and_rename_stan_blocks()` uses variable-tracing to extract mu construction patterns from brms model blocks, then renames variables with `_trend` suffix for proper integration.

## Critical Integration Points

### Centralized Mapping Generation Architecture

**Design Decision**: Mapping generation is centralized within `extract_time_series_dimensions()` to eliminate complex parameter threading and ensure mappings are always available when dimensions are computed.

**Rationale**: 
- Every trend model needs both dimensions and observation-to-trend mappings
- Generating them together eliminates threading response variables through multiple function calls
- Centralizes all data structure analysis in one location for better maintainability
- Ensures mappings are always consistent with dimension calculations

### Data Flow Overview
```
User Input → mvgam() → parse_multivariate_trends() → setup_brms_lightweight() 
→ validate_time_series_for_trends() → extract_time_series_dimensions(response_vars)
→ {dimensions + mappings} ← comprehensive time series analysis
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

**Key Architecture Decisions**:

1. **Early Dimension Calculation**: `extract_time_series_dimensions()` calculates all timing information directly from data during validation
2. **No Circular Dependencies**: Eliminates previous pattern where `extract_trend_stanvars_from_setup()` tried to extract `n_obs` from `trend_specs` that didn't have it
3. **Validation Integration**: `validate_time_series_for_trends()` now returns both validated data AND calculated dimensions
4. **CAR Exception Handling**: CAR trends skip regular interval validation but still get basic dimensions

**Dimension Validation Rules**:
- **Non-CAR trends** (RW, AR, VAR, ZMVN, PW): Require regular time intervals using `validate_regular_time_intervals()`
- **CAR trends**: Allow irregular intervals, can calculate time distances dynamically in `calculate_car_time_distances()`
- **All trends**: Must have consistent series and time variable identification

### 14. Prior Specification Using Native brms Classes

**Critical Design Decision**: Use `brmsprior` class throughout rather than creating custom mvgamprior class

**Implementation Strategy**:
- Use existing `brmsprior` data frame structure with standard columns
- Distinguish trend vs observation parameters via `class` column naming (`"ar1_trend"`, `"sigma_trend"`)
- Apply `_trend` suffix convention consistently for all trend parameters
- **Recommended User Workflow**: Use `brms::prior()`, `brms::prior_string()`, `brms::prior_()` for trend parameters rather than `brms::set_prior()` which has strict validation
- Leverage brms `get_prior()` for observation model priors directly
- **Avoid set_prior() for trend parameters**: brms::set_prior() validates against known parameter classes and will reject custom trend suffixes

## Enhanced Architecture

### 12. Integrated Prior Generation System

**Critical Design Decision**: Replace manual prior generators with convention-based dispatch using existing trend infrastructure.

**Architecture**: 
- **Automated Prior Generation**: Uses `trend_obj$monitor_params` as authoritative source
- **Convention-Based Dispatch**: Optional `get_[trend]_parameter_prior()` for customization
- **Parameter Type Defaults**: Intelligent defaults based on parameter name patterns
- **Factor Model Support**: Handles `Z` matrix and `_trend` suffixed parameters correctly


### 13. Registry System with Auto-Discovery

**Decision**: Auto-discovery eliminates manual registry maintenance.
**Pattern**: For trend "FOO", define `generate_foo_trend_stanvars()` + `foo_trend_properties()`

### 14. Enhanced Validation with Rule-Based Dispatch  

**Decision**: Move parameter processing to validation layer with data context.
**Architecture**: `trend_obj$validation_rules` drives automatic dispatch

### 15. Ultra-Clean Stanvar Architecture

- **Single Source of Truth**: Only `generate_common_trend_data()` creates dimension stanvars
- **Injection Function Orchestration**: `generate_trend_injection_stanvars()` manages complete stanvar assembly:
  - Creates dimensions via `generate_common_trend_data()`
  - Creates shared innovations via `generate_shared_innovation_stanvars()`
  - Calls trend generators for trend-specific logic only
  - Handles cross-cutting validation via `validate_no_factor_hierarchical()`
- **Clean Trend Generators**: Focus purely on trend-specific stanvars, assume dimensions exist in context
- **No Duplication Possible**: By design, no function can create dimensions twice

### 16. Trend Model Distribution Constraints

**Critical Architectural Constraint**: All trend models are univariate Gaussian State-Space models.

**Design Decision**: Trend components are ALWAYS Gaussian regardless of observation model family.

**Key Implications**:
- **Likelihood Filtering**: When extracting model blocks from brms trend models, only Gaussian likelihood patterns need to be filtered (normal distribution, normal_lpdf, normal_glm_lpdf)
- **Parameter Simplification**: Trend model parameters are always continuous (no discrete distribution handling needed)
- **Stan Code Generation**: Trend block patterns are predictable and consistent across all trend types
- **Validation Simplification**: No need to validate trend distribution families or discrete parameter constraints

**Implementation Guidance**:
- `extract_non_likelihood_from_model_block()` should focus on Gaussian likelihood exclusion patterns
- Trend generators can assume Gaussian innovations and continuous parameter spaces
- Documentation should clarify that observation models can be any brms-supported family while trends remain Gaussian

### 17. mvgam Formula Interface and Prior Inspection System

**Design Decision**: Extend brms `get_prior` generic with mvgam_formula S3 method for unified prior inspection.

**Architecture Components**:
- **mvgam_formula Constructor**: Lightweight container pairing observation + trend formulas
- **S3 Method Dispatch**: `get_prior.mvgam_formula()` extends brms functionality cleanly
- **Native brms Compatibility**: When `trend_formula = NULL`, behaves identically to `brms::get_prior`
- **Unified Prior Objects**: Uses native `brmsprior` class with `trend_component` attribute

**Key Design Patterns**:
```r
# Clean interface - data/family provided to inspection functions
mf <- mvgam_formula(y ~ x, trend_formula = ~ AR(p = 1))
priors <- get_prior(mf, data = dat, family = poisson())

# Perfect brms equivalence when no trends
priors_obs_only <- get_prior(mvgam_formula(y ~ x), data = dat)
# Identical to: brms::get_prior(y ~ x, data = dat)
```

**Integration Points**:
- **Existing Helper Functions**: Leverages `extract_observation_priors()`, `extract_trend_priors()`, `combine_obs_trend_priors()`
- **Response Name Extraction**: Uses `extract_response_names()` from brms integration layer
- **Validation Standards**: Full `checkmate::assert_*()` validation with `insight::format_error()` messaging

### 18. Formula Term Restrictions: Observation vs Trend Formulas

**Observation Formula**: Full brms compatibility - ALL terms allowed including `offset()`, `weights()`, `cens()`, `trunc()`, `mi()`, `trials()`, autocorr terms

**Trend Formula**: State-Space restricted - Only fixed effects, interactions, random effects, smooths, `gp()` allowed. Defaults to ZMVN() if no trend constructor specified.

**Forbidden in trend_formula**: All brms addition-terms (`offset()`, `weights()`, `cens()`, etc.) and brms autocorr (`ar()`, `ma()`, etc.)

**User Documentation**: Complete list of forbidden terms and rationale documented in `?mvgam_formula` with examples of correct/incorrect usage patterns.

### 19. Dual-Context Function Architecture for Validation and Prediction

**Design Principle**: Functions that process trend metadata and data must work seamlessly in both fitting and prediction contexts to enable robust forecasting and model evaluation.

**Architectural Pattern**:
```r
# Dual-context function signature
function_name <- function(data, trend_formula = NULL, ..., 
                         mvgam_object = NULL, newdata = NULL) {
  if (!is.null(mvgam_object)) {
    # PREDICTION CONTEXT: Use stored metadata from fitted object
    metadata <- mvgam_object$trend_metadata
    # Process newdata using stored requirements
  } else {
    # FITTING CONTEXT: Extract metadata from formulas and data
    # Store metadata for future prediction use
  }
  # Common processing logic
}
```

**Key Design Requirements**:
1. **Context Detection**: Automatically detect context based on presence of `mvgam_object` parameter
2. **Metadata Storage**: Store comprehensive trend metadata in fitted objects for prediction validation
3. **Consistent Output**: Same output structure regardless of context
4. **DRY Principle**: Single function with shared processing logic
5. **Validation Consistency**: Same validation rules applied in both contexts

**Metadata Storage Structure in mvgam Objects**:
```r
mvgam_object$trend_metadata <- list(
  # Variable names for validation
  variables = list(
    time_var = "time",
    series_var = "series", 
    gr_var = "site",      # NULL if not used
    subgr_var = "plot"    # NULL if not used
  ),
  
  # Extracted covariates for prediction validation
  covariates = c("temperature", "rainfall"),
  
  # Trend specification details
  trend_type = "AR",
  is_car = FALSE,
  grouping_structure = "hierarchical",  # "series_only", "hierarchical", "nested_hierarchical"
  
  # Model parameters for forecasting
  n_lv = 2,
  cor = TRUE,
  ma = FALSE, 
  lags = c(1, 12)
)
```

**Implementation Examples**:
- `extract_trend_data()`: Dual-context data extraction (R/validations.R:2671)
- `validate_trend_setup()`: Future master validation function
- `extract_trend_metadata()`: Future unified metadata extraction

**Benefits**:
- **User Experience**: Consistent interface across fitting and prediction
- **Maintainability**: Single function to maintain instead of separate fitting/prediction versions
- **Robustness**: Prediction validation uses exactly the same metadata and rules as fitting
- **Extensibility**: New trend types automatically support both contexts

**Future Applications**: This pattern should be applied to all validation and data processing functions that will need to work during prediction, including hierarchical validation, CAR special handling, and metadata-driven processing.
