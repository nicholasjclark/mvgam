# mvgam Quick Reference

Formula patterns, function names and common errors for day-to-day
work. `architecture-decisions.md` gives the reason for each rule, and
`stan-data-flow-pipeline.md` follows Stan code generation step by
step.

## Prior inspection
```r
mf <- mvgam_formula(count ~ treatment, trend_formula = ~ AR(p = 1))
priors <- get_prior(mf, data = data, family = poisson())

# The trend_component column separates the two sides
obs_priors <- priors[priors$trend_component == "observation", ]
trend_priors <- priors[priors$trend_component == "trend", ]
```

## Univariate formula patterns
```r
# No trend: a brms model, which needs no series column
mvgam(count ~ temperature + s(time), data = data)

# A trend, with its covariates in the trend formula
mvgam(y ~ s(x), trend_formula = ~ s(habitat) + RW(cor = TRUE, n_lv = 3),
      data = data)

# Transformed response
mvgam(log(biomass) ~ habitat + s(latitude, longitude),
      trend_formula = ~ AR(p = 2, cor = TRUE), data = data)

# Factor model on ZMVN
mvgam(abundance ~ t2(temperature, precipitation) + s(site, bs = "re"),
      trend_formula = ~ ZMVN(n_lv = 2), data = data)

# Distributional regression: the trend joins mu only
mvgam(bf(biomass ~ s(x), sigma ~ habitat),
      trend_formula = ~ CAR(), family = gaussian(), data = data)

# One series still needs its series column when a trend is used
data$series <- factor("series_1")
mvgam(count ~ 1, trend_formula = ~ AR(), data = data)

# Hierarchical trend: gr needs subgr
mvgam(count ~ treatment,
      trend_formula = ~ AR(p = 1, gr = region, subgr = species),
      data = data)
```

## Multivariate formula patterns
```r
# mvbind() with a shared trend
mvgam(mvbind(count, biomass, presence) ~ temperature + precipitation,
      trend_formula = ~ AR(p = 1, cor = TRUE), data = data)

# mvbind() with no trend
mvgam(mvbind(abundance, diversity) ~ temp + precip, data = data)

# Added bf() objects with a shared trend
mvgam(bf(count ~ temp) + bf(biomass ~ precip) + set_rescor(FALSE),
      trend_formula = ~ VAR(p = 1), data = data)

# A family per response
mvgam(
  bf(abundance ~ x, family = poisson()) +
    bf(presence ~ x, family = bernoulli()) +
    bf(diversity ~ x, family = Gamma()),
  trend_formula = ~ RW(cor = TRUE, n_lv = 2),
  data = data
)

# Factor model over four species
mvgam(mvbind(sp1, sp2, sp3, sp4) ~ habitat,
      trend_formula = ~ AR(p = 1, n_lv = 2, cor = TRUE), data = data)
```

`cbind(successes, failures)` with `family = binomial()` is one
response with trials, and a trend on it applies to the success
probability. `mvbind()` is the multivariate form.

## Multiple imputation
```r
mvgam(y ~ s(x), trend_formula = ~ AR(p = 1),
      data = imputed_data_list,   # a list of imputed data frames
      combine = TRUE)             # pool the fits
```

## Trend registry (R/trend_system.R)
```r
# Internal
get_trend_info(name)            # a trend's registered entry
trend_property(name, field)     # one entry field; "none" for "None"
ensure_registry_initialized()   # register the trends on first use

# Exported
list_trend_types()              # the trends and their factor support
```

`AR()`, `RW()`, `VAR()` and `ZMVN()` take `n_lv`. `PW()` and `CAR()`
refuse it:

- `PW(n_lv = 2)`: "Piecewise trends model changepoints separately for
  each series."
- `CAR(n_lv = 2)`: "Continuous-time AR dynamics follow each series'
  own irregular time gaps."

## Stan code during development
```r
stancode(mf, data = data, family = poisson())
standata(mf, data = data, family = poisson())
validate_stan_code(stan_code, backend = "rstan", silent = FALSE)
```

`data("portal_data", package = "mvgam")` gives a real series for
small test fits.

## Common errors
```r
# brms autocorrelation in the trend formula is refused
trend_formula = ~ s(time) + ar(p = 1)
# Observation-level correlation belongs in the observation formula
mvgam(count ~ Trt + unstr(visit, patient), trend_formula = ~ AR(p = 1),
      data = data)

# A factor model on a trend without a factor form is refused
trend_formula = ~ PW(n_lv = 2)
trend_formula = ~ AR(p = 1, n_lv = 2)

# A trend constructor in a distributional formula is refused
bf(y ~ s(x), sigma ~ s(z) + AR(p = 1))
mvgam(bf(y ~ s(x), sigma ~ s(z)), trend_formula = ~ AR(p = 1),
      data = data)
```
