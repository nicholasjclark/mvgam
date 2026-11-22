# Occupancy family for repeated detection and non-detection

For detections recorded on repeated visits to sites whose occupancy
stays fixed across the visits, such as camera-trap or call surveys
within a season. The data hold one row per visit with a 0 / 1 response.
The visits sharing a `series` and a `time` belong to one closure unit,
which is occupied with probability `mu`. A visit to an occupied unit
detects the species with probability `p`. The likelihood sums over the
unit's two states. A unit with a detection is occupied for certain
(MacKenzie et al. 2002).

## Usage

``` r
occ(multi_season = FALSE)
```

## Arguments

- multi_season:

  Logical. `TRUE` keys a closure unit on `(series, site, time)`, for
  several seasons of visits to each site. The data then need a `site`
  column, which
  [`pivot_detection_array()`](https://nicholasjclark.github.io/mvgam/dev/reference/pivot_detection_array.md)
  creates from a four-dimensional array with
  `multi_season = "hierarchical"`. Each unit holds the visits of one
  site in one season.

## Value

A family object to pass to
[`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md)
or
[`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md).

## Details

`bf(y ~ elev, p ~ tod)` models occupancy and detection, each with its
own formula on the logit link.
[`predict()`](https://rdrr.io/r/stats/predict.html) with
`type = "latent_state"` gives the probability that each unit is occupied
given its detections. `type = "detection"` gives the detection
probability of each visit. The data need no `cap` column.

## Priors

The occupancy intercept takes brms's `student_t(3, 0, 2.5)` on the logit
scale. A scalar `p` takes `beta(1, 1)`. With a formula, its intercept
takes `normal(0, 1.5)`.
[`get_prior()`](https://nicholasjclark.github.io/mvgam/dev/reference/get_prior.md)
lists the defaults, and `prior(..., dpar = "p")` replaces one.

## Identification

The detections identify occupancy and detection apart through repeated
visits and through covariates that act on one of them. With one visit
per unit and no covariate, the data identify only the product `mu * p`.
Only the priors then set the two apart (Royle and Dorazio 2008).
[`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md)
warns on such data. It also warns when more than 30% of units have a
single visit.

## Joint species models

[`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md)
with
[`nmix()`](https://nicholasjclark.github.io/mvgam/dev/reference/nmix.md)
or `occ()` fits several species at once. Latent factors shared by the
species enter the state `mu`. `p` keeps its own formula.
`factor_formula = ~ s(env, by = lv_axis())` lets each factor follow an
environmental gradient.

## Importing array data

Detection histories from unmarked, spOccupancy or ubms come as a
site-by-visit matrix, an array with species or seasons as further
dimensions, or a list of matrices.
[`pivot_detection_array()`](https://nicholasjclark.github.io/mvgam/dev/reference/pivot_detection_array.md)
converts any of these to the long format, broadcasting site, season and
visit covariates to their rows.

## Threading

The density sums over units through Stan's `reduce_sum()`. Fitting with
`threads = 2` or more splits the units across threads.

## References

MacKenzie, D. I., Nichols, J. D., Lachman, G. B., Droege, S., Royle, J.
A. and Langtimm, C. A. (2002). Estimating site occupancy rates when
detection probabilities are less than one. *Ecology*, 83, 2248-2255.
[doi:10.1890/0012-9658(2002)083\[2248:ESORWD\]2.0.CO;2](https://doi.org/10.1890/0012-9658%282002%29083%5B2248%3AESORWD%5D2.0.CO%3B2)

Royle, J. A. and Dorazio, R. M. (2008). *Hierarchical Modeling and
Inference in Ecology*. Academic Press.

## See also

[`nmix()`](https://nicholasjclark.github.io/mvgam/dev/reference/nmix.md)
for the related N-mixture family and
[`pp_check.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/pp_check.mvgam.md)
for closure-unit goodness-of-fit checks. The [N-mixture
article](https://nicholasjclark.github.io/mvgam/dev/articles/nmix.html)
fits occupancy and N-mixture models from data preparation to inference.
The [data
article](https://nicholasjclark.github.io/mvgam/dev/articles/data.html)
sets out the row layout that `occ()` expects.

Other observation families:
[`beta_nb()`](https://nicholasjclark.github.io/mvgam/dev/reference/beta_nb.md),
[`categ()`](https://nicholasjclark.github.io/mvgam/dev/reference/categ.md),
[`com_binomial()`](https://nicholasjclark.github.io/mvgam/dev/reference/com_binomial.md),
[`diri()`](https://nicholasjclark.github.io/mvgam/dev/reference/diri.md),
[`multi()`](https://nicholasjclark.github.io/mvgam/dev/reference/multi.md),
[`mvgam_families`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_families.md),
[`mvn()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvn.md),
[`mvt()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvt.md),
[`nmix()`](https://nicholasjclark.github.io/mvgam/dev/reference/nmix.md),
[`tweedie()`](https://nicholasjclark.github.io/mvgam/dev/reference/tweedie.md)

## Examples

``` r
# \dontrun{
# Detections at 50 sites, each visited four times, with covariates
# on occupancy and on detection
set.seed(6)
simdat <- sim_closure_unit_data(family = occ(), n_species = 1L,
                                n_sites = 50L, n_visits = 4L,
                                type = 1L)
mod <- mvgam(
  bf(y ~ env, p ~ tod_c),
  data   = simdat$data_train,
  family = occ(),
  chains = 2,
  silent = 2
)
summary(mod)
#>  Family: occ 
#>   Links: mu = logit; p = logit 
#> Formula: y ~ env 
#>          p ~ tod_c 
#>    Data: simdat$data_train (Number of observations: 200) 
#>   Draws: 2 chains, each with iter = 2000; warmup = 1000; thin = 1; 
#>          total post-warmup draws = 2000
#> 
#> == Observation Model ==
#> Regression Coefficients:
#>             Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept       0.56      0.52    -0.36     1.71 1.00     1450     1005
#> p_Intercept     0.21      0.24    -0.26     0.68 1.00     1812     1156
#> env            -3.35      1.25    -6.34    -1.44 1.00     1460      941
#> p_tod_c        -0.32      0.07    -0.47    -0.19 1.00     1664     1059
#> 
#> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).
#> 
#> Next steps:
#>   - `pp_check(fit, type = "fit_stat")`: goodness of fit of unit totals
#>   - `plot(hindcast(fit, type = "latent_state"))`: posterior occupancy of each unit
#>   - `forecast(fit, newdata = ...)`: out-of-sample forecasts
#>   - `loo(fit)` / `loo_compare(...)`: model fit + comparison
#>   - `conditional_effects(fit)`: covariate effects
#> Use `how_to_cite(fit)` for a methods paragraph and `methods_md(fit)` for its equations.

# Occupancy along the gradient and detection through the day
conditional_effects(mod, effects = "env", type = "latent_state")

conditional_effects(mod, effects = "tod_c", type = "detection")


# Occupancy and detection, separated by the model
head(predict(mod, type = "latent_state"))
#>          Estimate   Est.Error         Q2.5       Q97.5
#> [1,] 0.0082040856 0.010155958 6.730307e-04 0.032788195
#> [2,] 1.0000000000 0.000000000 1.000000e+00 1.000000000
#> [3,] 0.0020931340 0.002648679 9.090934e-05 0.009373608
#> [4,] 0.0006963093 0.001295766 1.429801e-06 0.004289626
#> [5,] 1.0000000000 0.000000000 1.000000e+00 1.000000000
#> [6,] 1.0000000000 0.000000000 1.000000e+00 1.000000000
head(predict(mod, type = "detection"))
#>       Estimate  Est.Error      Q2.5     Q97.5
#> [1,] 0.5707887 0.05902508 0.4560035 0.6823230
#> [2,] 0.7584860 0.06166589 0.6261865 0.8650297
#> [3,] 0.8044009 0.06015120 0.6721414 0.9032749
#> [4,] 0.6111829 0.05972532 0.4935669 0.7227784
#> [5,] 0.2945619 0.06414099 0.1775159 0.4272538
#> [6,] 0.6655555 0.06094458 0.5418565 0.7752948

# Next, pp_check(mod, type = "fit_stat") tests the fit of each unit
# }
```
