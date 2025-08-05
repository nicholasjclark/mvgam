# mvgam-brms Quick Reference

## Core Formula Patterns

### Standard State-Space
```r
mvgam(y ~ s(x), trend_formula = ~ s(habitat) + RW(series = series, time = time, cor = TRUE, n_lv = 3, ...), data = data)
```

### Distributional Models (trends ONLY on mu)
```r
mvgam(
  bf(y ~ s(x), sigma ~ s(z)),           # Distributional regression
  trend_formula = ~ AR(p = 1),          # Applied ONLY to mu parameter
  family = gaussian(),
  data = data
)
```

### Multiple Imputation
```r
mvgam(
  y ~ s(x), 
  trend_formula = ~ AR(p = 1),
  data = imputed_data_list,             # List of multiply imputed datasets
  combine = TRUE                        # Pool results using Rubin's rules
)
```

### Multivariate with Response-Specific Trends
```r
mvgam(
  bf(count ~ temp, biomass ~ precip) + set_rescor(FALSE),
  trend_formula = bf(
    count = ~ AR(p = 1),        # Population dynamics
    biomass = ~ RW(cor = TRUE), # Correlated biomass trends
    presence = NULL             # No trend (static occupancy)
  ),
  family = c(poisson(), gaussian(), bernoulli()),
  data = data
)
```

## Autocorrelation Rules

### ✅ ALLOWED: Observation-level + State-Space
```r
mvgam(
  count ~ Trt + unstr(visit, patient),  # Observation-level correlation
  trend_formula = ~ AR(p = 1),          # State-Space dynamics
  data = data
)
```

### ❌ FORBIDDEN: brms autocorr in trend formula
```r
mvgam(
  y ~ s(x),
  trend_formula = ~ s(time) + ar(p = 1),  # CONFLICTS with mvgam AR()
  data = data
)
```

## Trend Registry System ✅ **COMPLETE**

### Registry Architecture (R/trend_registry.R)

**Core Functions:**
```r
# Registry management
register_trend_type(name, supports_factors, generator_func, incompatibility_reason)
get_trend_info(name)                    # Retrieve registered trend info
list_trend_types()                      # List all available trends
validate_factor_compatibility(spec)     # Automatic validation
ensure_registry_initialized()           # Auto-load core trends

# User extension functions
register_custom_trend(name, ...)        # User-facing registration
custom_trend(trend, tpars, ...)         # Create custom trend objects
```

**Auto-registered Core Trends:**
- `AR`, `RW`, `VAR`, `ZMVN` (factor-compatible)
- `PW`, `CAR` (factor-incompatible)

### Factor Model Compatibility (Automatic Validation)

**✅ Compatible (n_lv parameter supported):**
- `AR(p = 1, n_lv = 3)` - Autoregressive with latent factors
- `RW(cor = TRUE, n_lv = 2)` - Random walk with factor structure 
- `VAR(p = 1, n_lv = 4)` - Vector autoregression with factors
- `ZMVN(n_lv = 2)` - Zero-mean multivariate normal factors

**❌ Incompatible (automatic error with n_lv):**
- `PW(n_lv = 2)` → Error: "Piecewise trends require series-specific changepoint modeling"
- `CAR(n_lv = 2)` → Error: "Continuous-time AR requires series-specific irregular time intervals"

## Two-Stage Stan Assembly System

### Stage 1: Registry-Based Stanvar Generation (R/trend_injection_generators.R)
```r
# Registry dispatch to trend-specific generators
trend_stanvars <- generate_trend_injection_stanvars(trend_spec, data_info)

# Registry automatically selects appropriate generator:
# - generate_rw_injection_stanvars()
# - generate_ar_injection_stanvars() 
# - generate_var_injection_stanvars()
# - generate_zmvn_injection_stanvars()
# - etc.
```

### Stage 2: brms Integration with Stan Assembly (R/stan_code_generation.R)
```r
# The injection system modifies brms-generated Stan code by:
# 1. Finding/creating transformed parameters block
# 2. Adding trend effects to mu parameters (linear predictors)
# 3. Preserving all brms optimizations and structure

inject_trend_into_linear_predictor(base_stancode, trend_stanvars, trend_spec)
```

## Stan Code Validation Framework (R/stan_validation.R)
```r
# Unified comprehensive validation using rstan::stanc()
validate_stan_code(stan_code, backend = "rstan", silent = FALSE)  # Primary validation function

# Optional structural pre-checks for optimization
validate_stan_code_structure(stan_code)    # Check required blocks exist
are_braces_balanced(stan_code)             # Check brace matching
```

## Stan Assembly Integration with mvgam_enhanced()

### Key Integration Points

**Trend Stanvar Generation**:
- `extract_trend_stanvars_from_setup()` automatically calls `generate_trend_injection_stanvars()`
- Registry system dispatches to appropriate generator (`generate_rw_injection_stanvars()`, etc.)
- Factor model compatibility validated via `validate_factor_compatibility()`

**Stan Code Assembly**:
- `generate_base_brms_stancode()` creates observation model with injected trend stanvars
- `inject_trend_into_linear_predictor()` modifies Stan code to add trend effects to `mu`
- `validate_stan_code()` provides comprehensive validation using `rstan::stanc()` directly

**Data Integration**:
- `extract_trend_data_from_stanvars()` extracts time/series components 
- `merge_stan_data()` combines observation and trend data with conflict resolution
- `validate_stan_data_structure()` ensures proper Stan data types

## Validation Framework

### Fast-Fail Principles
1. **Formula conflicts**: Check during setup, not Stan compilation
2. **Trend compatibility**: Validate with factor models early
3. **Data structure**: Ensure time series integrity before fitting

### Distributional Model Rules
- Trends apply **ONLY** to main parameter (`mu`)
- Auxiliary parameters (`sigma`, `zi`, `hu`) use standard brms approach
- Clear error messages guide proper usage

### Multiple Imputation Requirements
- Consistent variable structure across imputations
- Time series alignment preserved
- Pooling compatibility validated

## Common Error Patterns

### Trend Formula Conflicts
```r
# ❌ Wrong: brms autocorr in trend
trend_formula = ~ s(time) + ar(p = 1)

# ✅ Correct: mvgam trend types only
trend_formula = ~ AR(p = 1)
```

### Factor Model Incompatibility
```r
# ❌ Wrong: Incompatible trend with factor model
mvgam(..., trend_formula = ~ PW(n_lv = 2))

# ✅ Correct: Compatible trend
mvgam(..., trend_formula = ~ AR(p = 1, n_lv = 2))
```

### Distributional Trend Misplacement
```r
# ❌ Wrong: Trend on auxiliary parameter
bf(y ~ s(x), sigma ~ s(z) + AR(p = 1))

# ✅ Correct: Trend only on main parameter
bf(y ~ s(x) + AR(p = 1), sigma ~ s(z))
```
