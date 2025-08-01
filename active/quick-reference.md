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

## Trend Types Registry

### Compatible with Factor Models (n_lv < n_series)
- `AR(p = 1)` - Autoregressive process
- `RW(cor = TRUE)` - Random walk with correlations
- `VAR(p = 1)` - Vector autoregression
- `ZMVN()` - Zero-mean multivariate normal

### Incompatible with Factor Models
- `PW()` - Piecewise trends (series-specific changepoints)
- `CAR()` - Conditional autoregressive (irregular time spacing)

## Stan Code Patterns

### Data Integration via stanvars
```r
# Generate complete stanvars (data + code)
trend_stanvars <- generate_trend_stanvars_complete(trend_obj, data_info)

# Let brms handle integration
stancode <- brms::stancode(obs_formula, data, stanvars = trend_stanvars)
standata <- brms::standata(obs_formula, data, stanvars = trend_stanvars)
```

### Missing Data Likelihood Pattern
```stan
// Combined linear predictor for all timepoints
mu_combined = mu + mu_trend;

// Likelihood only for observed data
{
  vector[n_nonmissing] selected_mu = mu_combined[obs_ind];
  flat_ys ~ family_distribution(selected_mu, ...);
}

// Trends evolve over ALL timesteps
trend[2:T] ~ normal(trend[1:(T-1)], sigma_trend);
```

### Non-Centered Parameterization
```stan
parameters {
  matrix[n, n_lv] LV_raw;          // Raw innovations
  vector<lower=0>[n_lv] sigma;     // Scaling
}

transformed parameters {
  matrix[n, n_lv] LV = LV_raw .* rep_matrix(sigma', rows(LV_raw));
  // Apply trend evolution...
}
```

## Performance Targets

### Setup Performance
- **brms initialization**: 10-50x faster with `backend = "mock"`
- **Registry lookup**: <1ms overhead for trend dispatch
- **JSDGAM prediction**: 10-100x speedup with Rcpp

### Memory Optimization
- **Object size**: 30-50% reduction through compression
- **Missing data**: Efficient handling without redundant copies
- **Dual objects**: Shared parameter storage

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

## Context Loading Guide
- Validation work: `reference/validation-rules.md`
- brms integration strategies: `reference/integration-patterns.md`  
- Complete timeline: `planning/full-timeline.md`
