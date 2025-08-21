# mvgam-brms Quick Reference

## Core Formula Patterns

### Standard State-Space
```r
mvgam(y ~ s(x), trend_formula = ~ s(habitat) + RW(series = series, time = time, cor = TRUE, n_lv = 3, ...), data = data)
```

## Observation Formula Diversity

### Univariate Formula Patterns
```r
# Simple univariate - no trends
mvgam(count ~ temperature + s(time), data = data)

# Simple univariate with trend
mvgam(count ~ temperature, trend_formula = ~ RW(), data = data)

# Transformed response with complex trend
mvgam(log(biomass) ~ habitat + s(latitude, longitude), 
      trend_formula = ~ AR(p = 2, cor = TRUE), data = data)

# Complex smooths with factor trends  
mvgam(abundance ~ t2(temperature, precipitation) + s(site, bs = "re"),
      trend_formula = ~ ZMVN(n_lv = 2), data = data)

# Distributional regression with trend on mu only
mvgam(bf(count ~ s(x), sigma ~ habitat), 
      trend_formula = ~ CAR(), family = poisson(), data = data)
```

### Multivariate Formula Patterns (TRUE multivariate - multiple responses)
```r
# Pattern 1: mvbind() with shared trend - classic multivariate
mvgam(mvbind(count, biomass, presence) ~ temperature + precipitation,
      trend_formula = ~ AR(p = 1, cor = TRUE), data = data)

# Pattern 2: mvbind() with no trend (pure observation model)
mvgam(mvbind(abundance, diversity) ~ temp + precip, data = data)

# Pattern 3: bf() with multiple responses and shared trend
mvgam(bf(count ~ temp, biomass ~ precip),
      trend_formula = ~ VAR(lags = 1), data = data)

# Pattern 4: Combined bf() objects with different families and shared trend
mvgam(
  bf(abundance ~ x, family = poisson()) +
  bf(presence ~ x, family = bernoulli()) + 
  bf(diversity ~ x, family = Gamma()),
  trend_formula = ~ RW(cor = TRUE, n_lv = 2),
  data = data
)

# Pattern 5: mvbf() wrapper with complex trend
mvgam(mvbf(
  bf(count ~ temperature, family = poisson()),
  bf(biomass ~ precipitation, family = gaussian())
), trend_formula = ~ AR(p = 1, cor = TRUE), data = data)

# Pattern 6: Response-specific trends (different trend per response)
mvgam(
  bf(abundance ~ x, family = poisson()) +
  bf(presence ~ x, family = bernoulli()) +
  bf(diversity ~ x, family = Gamma()),
  trend_formula = list(
    abundance = ~ AR(p = 1),        # Population dynamics
    presence = ~ RW(),              # Occupancy trends  
    diversity = ~ PW(n_change = 2)  # Piecewise trends
  ),
  data = data
)
```

### Binomial Trial Patterns (NOT multivariate - single response with trials)
```r
# cbind() for binomial trials - UNIVARIATE model with success/failure counts
mvgam(cbind(successes, failures) ~ treatment + s(time), 
      family = binomial(), data = data)

# cbind() with trend (still univariate - trend applies to success probability)
mvgam(cbind(successes, failures) ~ treatment,
      trend_formula = ~ AR(p = 1), family = binomial(), data = data)

# Note: cbind() creates trial structure, not multiple responses
# This is fundamentally different from mvbind() multivariate models
```

### Special Cases and Edge Patterns
```r
# No trend formula (defaults to ZMVN for state-space, or no trend)
mvgam(count ~ s(temperature), data = data)

# Trend-only model (minimal observation effects)  
mvgam(y ~ 1, trend_formula = ~ s(habitat) + VAR(lags = 2), data = data)

# Complex multivariate with mixed trend types
mvgam(mvbind(count, biomass) ~ temperature,
      trend_formula = list(
        count = ~ AR(p = 2),          # Different dynamics
        biomass = ~ RW(cor = FALSE)   # Per response
      ), data = data)

# Hierarchical trends with grouping
mvgam(count ~ treatment,
      trend_formula = ~ AR(p = 1, gr = site, cor = TRUE), data = data)

# Factor model with latent variables
mvgam(mvbind(sp1, sp2, sp3, sp4) ~ habitat,
      trend_formula = ~ AR(p = 1, n_lv = 2, cor = TRUE), data = data)
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

### Multivariate with Multivariate Trends
```r
mvgam(
  bf(count ~ temp, biomass ~ precip) + set_rescor(FALSE),
  trend_formula = ~ AR(p = 1, cor = TRUE),
  family = c(poisson(), gaussian(), bernoulli()),
  data = data
)
```

### Multivariate with Response-Specific Trends (no factors, correlations or groupings allowed)
```r
mvgam(
  bf(abund ~ x, family = poisson()) +
  bf(presabs ~ x, family = bernoulli()) +
  bf(divers ~ x, family = Gamma()),
  trend_formula = list(
    abund = ~ AR(p = 1),        # Population dynamics
    divers = ~ RW(),            # Biomass trends
    presabs = NULL              # No trend (static occupancy, defaults to ZMVN())
  ),
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

### Registry Architecture (R/trend_system.R)

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

### Stage 1: Registry-Based Stanvar Generation (R/stan_assembly.R)
```r
# Registry dispatch to trend-specific generators
trend_stanvars <- generate_trend_injection_stanvars(trend_spec, data_info)

# Registry automatically selects appropriate generator:
# - generate_rw_trend_stanvars()
# - generate_ar_trend_stanvars() 
# - generate_var_trend_stanvars()
# - generate_zmvn_trend_stanvars()
# - generate_car_trend_stanvars()
# - generate_pw_trend_stanvars()
```

### Stage 2: brms Integration with Stan Assembly (R/stan_assembly.R)
```r
# The injection system modifies brms-generated Stan code by:
# 1. Finding/creating transformed parameters block
# 2. Adding trend effects to mu parameters (linear predictors)
# 3. Preserving all brms optimizations and structure

inject_trend_into_linear_predictor(base_stancode, trend_stanvars, trend_spec)
```

## Centralized Prior Resolution (R/priors.R)

### Pattern for All Trend Generators
```r
# Replace hardcoded priors with centralized helper
sigma_prior <- get_trend_parameter_prior(prior, "sigma_trend")
ar1_prior <- get_trend_parameter_prior(prior, "ar1_trend")

# Use in Stan code generation
stan_code <- glue("
  sigma_trend ~ {sigma_prior};
  ar1_trend ~ {ar1_prior};
")
```

### Resolution Strategy
1. **User specification first**: Check brmsprior object for custom prior
2. **Common default fallback**: Use `common_trend_priors` defaults  
3. **Empty string fallback**: Let Stan use built-in defaults

### Benefits
- **DRY**: Eliminates hardcoded "exponential(1)" across generators
- **Extensible**: New parameters automatically work everywhere
- **Future-proof**: New trend types inherit prior support

## Stan Code Validation Framework (R/validations.R)
```r
# Unified comprehensive validation using rstan::stanc()
validate_stan_code(stan_code, backend = "rstan", silent = FALSE)  # Primary validation function

# Optional structural pre-checks for optimization
validate_stan_code_structure(stan_code)    # Check required blocks exist
are_braces_balanced(stan_code)             # Check brace matching
```

## Stan Assembly Integration with mvgam()

### Key Integration Points

**Trend Stanvar Generation**:
- `extract_trend_stanvars_from_setup()` automatically calls `generate_trend_injection_stanvars()`
- Registry system dispatches to appropriate generator (`generate_rw_trend_stanvars()`, etc.)
- Factor model compatibility validated via `validate_factor_compatibility()`

**Stan Code Assembly**:
- `generate_base_brms_stancode()` creates observation model with injected trend stanvars
- `inject_trend_into_linear_predictor()` modifies Stan code to add trend effects to `mu`
- `validate_stan_code()` provides comprehensive validation using `rstan::stanc()` directly

**Data Integration**:
- `prepare_stan_data()` prepares and orders time/series data for Stan
- `combine_stan_data()` combines observation and trend data with conflict resolution
- `validate_combined_standata()` ensures proper Stan data structure and types

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

## Development Workflow

### Testing with mvgam Data
```r
# Use consistent test data
data("portal_data", package = "mvgam")
# OR
test_data <- mvgam:::example_data  # Internal test dataset
```

### Stan Backend Integration
```r
# cmdstanr (preferred)
model <- cmdstanr::cmdstan_model(write_stan_file(stancode))
fit <- model$sample(data = standata)

# rstan (fallback) 
fit <- rstan::stan(model_code = stancode, data = standata)
```

### Development Testing Pattern
```r
# 1. Start simple
mvgam(y ~ 1, trend_formula = ~ RW(), data = test_data)

# 2. Add complexity incrementally  
mvgam(y ~ s(x), trend_formula = ~ AR(p = 1), data = test_data)

# 3. Validate Stan code during development
validate_stan_code(stancode, silent = FALSE)  # Full output for debugging
```

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
