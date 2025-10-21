# Parameter Naming Pattern Findings for summary.mvgam()

## Research Date: 2025-10-21
## Source: 5 fitted mvgam models (Targets 1-5 from target_generation.R)

---

## 1. posterior::summarise_draws() Column Naming

**CONFIRMED**: Column names are **lowercase** with decimal points in quantile names:

| prob argument | Lower quantile | Upper quantile |
|---------------|----------------|----------------|
| 0.90 | `q5` | `q95` |
| 0.95 | `q2.5` | `q97.5` |
| 0.99 | `q0.5` | `q99.5` |

**Standard columns**:
- `variable` - parameter name
- `mean` - posterior mean
- `sd` - posterior standard deviation
- `q2.5`, `q97.5` - quantiles (names vary with prob)
- `rhat` - convergence diagnostic
- `ess_bulk` - bulk effective sample size
- `ess_tail` - tail effective sample size

---

## 2. Fixed Effects Naming

### Univariate Models
- Intercept: `b_Intercept`
- Covariates: `b_x`, `b_temperature`
- Trend formula: `b_Intercept_trend`, `b_trend[1]`

### Multivariate Models
- Response-specific coefficients use **brackets**: `b_count[1]`, `b_biomass[1]`
- Response-specific intercepts use **underscores**: `b_count_Intercept`, `b_biomass_Intercept`

**Pattern to match**:
```r
fixed_effects <- grep("^b_", all_pars, value = TRUE)
```

---

## 3. Smooth Parameters (GAM terms)

From Target 3 (VARMA with smooths):

**Two patterns**:
1. **Smooth SDs**: `sds_count_1[1]`, `sds_biomass_1[1]`
2. **Smooth coefficients**: `s_count_1_1[1]`, `s_count_1_1[2]`, ...

**Pattern to match**:
```r
smooth_pars <- grep("^s(ds)?_", all_pars, value = TRUE)
```

---

## 4. Family Parameters - Response-Specific Naming

**CRITICAL FINDING**: Multivariate models use **response names**, NOT y1/y2 suffixes

### Examples
- Gaussian sigma: `sigma_count`, `sigma_biomass`
- Gamma shape: `shape_biomass`
- **NOT**: `sigma_y1`, `sigma_y2`

**Implication**: Must use actual response names from `mvbind(count, biomass)` when categorizing parameters

**Pattern to match**:
```r
family_pars <- grep("^(sigma|shape|nu|phi|zi|hu)(_|$)", all_pars, value = TRUE)
# Then filter out trend parameters:
observation_family_pars <- family_pars[!grepl("_trend", family_pars)]
```

---

## 5. Trend Parameters

All trend parameters have `_trend` suffix:

### Common Patterns
- Intercepts: `Intercept_trend`
- Variances: `sigma_trend[1]`, `sigma_trend[2]`
- Correlation matrices: `L_Omega_trend[1,1]`, `L_Omega_trend[2,1]`, ...
- Innovations: `innovations_trend[1,1]`, `innovations_trend[2,1]`, ...
- Fixed effects in trend formula: `b_Intercept_trend`, `b_trend[1]`

### Trend Type-Specific
- **AR/VAR**: `ar1_trend`, `ar12_trend`, `A1_trend[*,*]`, `A2_trend[*,*]`
- **Piecewise**: `k_trend[*]`, `m_trend[*]`, `delta_trend[*,*]`

**Pattern to match**:
```r
trend_pars <- grep("_trend", all_pars, value = TRUE)
```

---

## 6. Factor Loadings (Factor Models)

From Target 4 (Factor AR model with n_lv=2):

**Pattern**: Clean 2D matrix indexing
- `Z[1,1]`, `Z[2,1]`, `Z[3,1]` (loadings for series 1,2,3 on latent variable 1)
- `Z[1,2]`, `Z[2,2]`, `Z[3,2]` (loadings for series 1,2,3 on latent variable 2)

**Pattern to match**:
```r
z_loadings <- grep("^Z\\[", all_pars, value = TRUE)
```

---

## 7. Latent States (Usually Excluded from Summaries)

**High-dimensional arrays** that should typically be excluded:

- `trend[i,s]` - Trend states (time × series)
- `lv_trend[i,k]` - Latent variables (time × n_lv)

**Count**:
- Target 1 (RW, univariate): 48 trend states
- Target 2 (RW, multivariate): 96 trend states
- Target 4 (Factor AR): 120 latent states

**Recommendation**: **Exclude from default summary** (too many to display), but provide option to access via `variables.mvgam()` or specialized extraction functions.

---

## 8. Random Effects (Not Observed in Current Models)

**Note**: None of the 5 tested models included random effects in the observation formula.

**Expected patterns** (based on brms conventions):
- SDs: `sd_groupname_paramname`
- Correlations: `cor_groupname_paramname1__paramname2`
- Individual effects: `r_groupname[level,param]`

**Action**: Add Target 6 model with random effects to research script for complete coverage.

---

## Summary: Parameter Categorization Logic

### For Observation Model Parameters

```r
# Fixed effects (excluding trend formula effects)
obs_fixed <- grep("^b_", all_pars, value = TRUE)
obs_fixed <- obs_fixed[!grepl("_trend", obs_fixed)]

# Smooths
smooth_pars <- grep("^s(ds)?_", all_pars, value = TRUE)

# Random effects
sd_pars <- grep("^sd_", all_pars, value = TRUE)
r_pars <- grep("^r_", all_pars, value = TRUE)

# Family parameters (exclude trend)
family_pars <- grep("^(sigma|shape|nu|phi|zi|hu)(_|$)", all_pars, value = TRUE)
obs_family_pars <- family_pars[!grepl("_trend", family_pars)]
```

### For Trend Model Parameters

```r
# All trend parameters
trend_pars <- grep("_trend", all_pars, value = TRUE)

# Factor loadings
z_loadings <- grep("^Z\\[", all_pars, value = TRUE)

# Exclude latent states from default summary
exclude_states <- grep("^(trend|lv_trend)\\[", all_pars, value = TRUE)
```

---

## Implementation Checklist for summary.mvgam()

- [ ] Use lowercase column names: `q2.5`, `q97.5`, `rhat`, `ess_bulk`, `ess_tail`
- [ ] Handle response-specific naming for multivariate models (use actual response names)
- [ ] Support smooth parameters (`s_*`, `sds_*`)
- [ ] Properly categorize trend vs observation parameters (`_trend` suffix)
- [ ] Extract factor loadings separately (`Z[*,*]`)
- [ ] Exclude high-dimensional latent states by default
- [ ] Test with all 5 target models to ensure comprehensive coverage
