# mvgam-brms Quick Reference

## Prior Inspection Workflow
```r
# Create mvgam_formula object
mf <- mvgam_formula(count ~ treatment, trend_formula = ~ AR(p = 1))

# Inspect available priors
priors <- get_prior(mf, data = data, family = poisson())

# Filter by component  
obs_priors <- priors[priors$trend_component == "observation", ]
trend_priors <- priors[priors$trend_component == "trend", ]
```

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
mvgam(bf(biomass ~ s(x), sigma ~ habitat),
      trend_formula = ~ CAR(), family = gaussian(), data = data)
```

### Multivariate Formula Patterns (TRUE multivariate - multiple responses)
```r
# Pattern 1: mvbind() with shared trend - classic multivariate
mvgam(mvbind(count, biomass, presence) ~ temperature + precipitation,
      trend_formula = ~ AR(p = 1, cor = TRUE), data = data)

# Pattern 2: mvbind() with no trend (pure observation model)
mvgam(mvbind(abundance, diversity) ~ temp + precip, data = data)

# Pattern 3: added bf() objects with a shared trend
mvgam(bf(count ~ temp) + bf(biomass ~ precip) + set_rescor(FALSE),
      trend_formula = ~ VAR(p = 1), data = data)

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
# No trend formula: a brms model, which needs no series column
mvgam(count ~ s(temperature), data = data)

# One series still needs its series column when a trend is used
data$series <- factor("series_1")
mvgam(count ~ 1, trend_formula = ~ AR(), data = data)

# Trend-only model (minimal observation effects)  
mvgam(y ~ 1, trend_formula = ~ s(habitat) + VAR(p = 2), data = data)

# Hierarchical trends with grouping
mvgam(count ~ treatment,
      trend_formula = ~ AR(p = 1, gr = site, cor = TRUE), data = data)

# Factor model with latent variables
mvgam(mvbind(sp1, sp2, sp3, sp4) ~ habitat,
      trend_formula = ~ AR(p = 1, n_lv = 2, cor = TRUE), data = data)
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

## Trend Registry System

### Registry Architecture (R/trend_system.R)

The registry holds the six built-in trends and no others. Trend `FOO`
defines `generate_foo_trend_stanvars()` and `foo_trend_properties()`.
`register_core_trends()` finds both by name on first use of the
registry.

**Core Functions:**
```r
# Internal
get_trend_info(name)                    # A trend's registered entry
ensure_registry_initialized()           # Register the trends on first use
trend_property(name, field)             # One entry field; "none" for "None"

# Exported
list_trend_types()                      # The trends and their factor support
```

**Registry entry:** `supports_factors`, `covariance_pattern`,
`stationary_source`, `requires_regular_intervals`, `generator` and
`incompatibility_reason`. Each is declared by the trend's properties
function and none has a default.

**Registered Trends:**
- `AR`, `RW`, `VAR`, `ZMVN` (factor-compatible)
- `PW`, `CAR` (factor-incompatible)

### Factor Model Compatibility (Automatic Validation)

**✅ Compatible (n_lv parameter supported):**
- `AR(p = 1, n_lv = 3)` - Autoregressive with latent factors
- `RW(cor = TRUE, n_lv = 2)` - Random walk with factor structure 
- `VAR(p = 1, n_lv = 4)` - Vector autoregression with factors
- `ZMVN(n_lv = 2)` - Zero-mean multivariate normal factors

**❌ Incompatible (automatic error with n_lv):**
- `PW(n_lv = 2)` → Error: "Piecewise trends model changepoints separately for each series."
- `CAR(n_lv = 2)` → Error: "Continuous-time AR dynamics follow each series' own irregular time gaps."

## Two-Stage Stan Assembly System

### Stage 1: Registry-Based Stanvar Generation (R/stan_assembly.R)
```r
# Takes the trend's generator from its registry entry
trend_stanvars <- generate_trend_specific_stanvars(
  trend_specs, data_info, response_suffix, prior
)

# The registered generators:
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

inject_trend_into_linear_predictors(base_stancode, resps)
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

## Stan Code Validation (R/validations.R)
```r
# Parses the program with the backend's Stan compiler
validate_stan_code(stan_code, backend = "rstan", silent = FALSE)
```

## Stan Assembly Integration with mvgam()

### Key Integration Points

**Trend Stanvar Generation**:
- `extract_trend_stanvars_from_setup()` calls `generate_trend_specific_stanvars()`
- That function takes the generator from the trend's registry entry (`generate_rw_trend_stanvars()`, etc.)
- `prepare_trend_specs()` refuses a factor model on a trend without a factor form, through `enforce_factor_support_against_specs()`

**Stan Code Assembly**:
- `generate_base_stancode_with_stanvars()` creates the observation model with the trend stanvars injected
- `inject_trend_into_linear_predictors()` modifies Stan code to add trend effects to `mu`
- `validate_stan_code()` parses the result with the backend's Stan compiler

**Data Integration**:
- Data preparation and ordering for time/series data for Stan

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

# ✅ Correct: Trend in trend_formula, which joins mu only
mvgam(bf(y ~ s(x), sigma ~ s(z)), trend_formula = ~ AR(p = 1), data = data)
```
