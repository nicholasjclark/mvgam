# Widely applicable information criterion for mvgam models

Passes the pointwise log likelihood of
[`log_lik.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/log_lik.mvgam.md)
to [`loo::waic()`](https://mc-stan.org/loo/reference/waic.html).
[`loo.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/loo.mvgam.md)
estimates the same ELPD by PSIS and reports Pareto k diagnostics that
say when to distrust it. Vehtari et al. (2017) recommend it over WAIC.

## Usage

``` r
# S3 method for class 'mvgam'
waic(
  x,
  ...,
  compare = TRUE,
  resp = NULL,
  pointwise = FALSE,
  model_names = NULL,
  incl_autocor = TRUE,
  incl_dynamics = NULL,
  by_series = FALSE
)
```

## Arguments

- x:

  A fitted `mvgam` or `jsdgam` object.

- ...:

  Further arguments to
  [`log_lik.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/log_lik.mvgam.md),
  such as `ndraws`.

- compare, resp, pointwise, model_names:

  The arguments of
  [brms::waic.brmsfit](https://paulbuerkner.com/brms/reference/waic.brmsfit.html).
  `resp` scopes a multivariate fit to one response. Supplying one of the
  others raises an error.
  [`loo_compare()`](https://mc-stan.org/loo/reference/loo_compare.html)
  ranks several [`waic()`](https://mc-stan.org/loo/reference/waic.html)
  results.

- incl_autocor:

  Logical, default `TRUE`. Passed to
  [`log_lik.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/log_lik.mvgam.md).
  `TRUE` scores each observation under the latent trend state the model
  inferred at that time. The ELPD then describes the series that was
  observed. `FALSE` scores the deterministic submodel alone.

- incl_dynamics:

  Deprecated alias for `incl_autocor`.

- by_series:

  Logical, default `FALSE`. `TRUE` returns a data frame with one row per
  series and columns `series`, `elpd`, `se_elpd`, `p` and `n_obs`, as
  `spOccupancy::waicOcc(by.sp = TRUE)` ranks species. An occupancy or
  N-mixture fit is scored per site, each site counted in its species.

## Value

A `waic` object (see
[`loo::waic()`](https://mc-stan.org/loo/reference/waic.html)), or with
`by_series = TRUE` a data frame of per-series estimates.

## References

Vehtari, A., Gelman, A. and Gabry, J. (2017). Practical Bayesian model
evaluation using leave-one-out cross-validation and WAIC. *Statistics
and Computing*, 27:1413-1432.
[doi:10.1007/s11222-016-9696-4](https://doi.org/10.1007/s11222-016-9696-4)

## See also

[`loo.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/loo.mvgam.md)
estimates the same ELPD by PSIS.
[`lfo_cv.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/lfo_cv.mvgam.md)
scores forecasts from rolling origins. The [forecast evaluation
article](https://nicholasjclark.github.io/mvgam/articles/forecast_evaluation.html)
compares models by approximate and exact cross-validation.

Other model comparison:
[`bridge_sampler.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/bridge_sampler.mvgam.md),
[`compare_elpds()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_elpds.md),
[`kfold.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/kfold.mvgam.md),
[`lfo_cv.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/lfo_cv.mvgam.md),
[`log_lik.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/log_lik.mvgam.md),
[`loo.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/loo.mvgam.md),
[`loo_compare.mvgam_lfo()`](https://nicholasjclark.github.io/mvgam/dev/reference/loo_compare.mvgam_lfo.md),
[`mvgam_loo_extras`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_loo_extras.md),
[`posterior_average.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_average.mvgam.md)

## Examples

``` r
# \dontrun{
# A Poisson GAM without a latent trend. PSIS approximates each
# held-out count of such a model well (see Details of ?loo.mvgam)
set.seed(13)
simdat <- sim_mvgam(family = poisson(), n_series = 1L,
                    n_timepoints = 120L, trend_model = "None")
mod <- mvgam(y ~ s(x), data = simdat$data_train,
             family = poisson(), chains = 2, silent = 2)
#> Warning: 2 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
# }
# \dontrun{
# The WAIC estimate of the ELPD, with its effective number of
# parameters p_waic. A p_waic above 0.4 for any observation raises
# a warning that recommends loo()
waic(mod)
#> Warning: 
#> 4 (4.4%) p_waic estimates greater than 0.4. We recommend trying loo instead.
#> 
#> Computed from 2000 posterior draws and 90 log-likelihood terms.
#> 
#>           Estimate   SE
#> elpd_waic   -208.7  8.5
#> p_waic         7.9  1.5
#> waic         417.5 16.9
#> 
#> 4 (4.4%) p_waic estimates greater than 0.4. We recommend trying loo instead. 

# The PSIS estimate of the same ELPD. Its Pareto k diagnostics mark
# the observations it estimates poorly
loo(mod)
#> 
#> Computed from 2000 posterior draws and 90 log-likelihood terms.
#> 
#>          Estimate   SE
#> elpd_loo   -208.8  8.5
#> p_loo         8.0  1.5
#> looic       417.7 17.0
#> ------
#> MCSE of elpd_loo is 0.1.
#> MCSE and ESS estimates assume MCMC draws (r_eff in [0.4, 1.2]).
#> 
#> All Pareto k estimates are good (k < 0.7).
#> See help('pareto-k-diagnostic') for details.

# Next, loo_compare() ranks several models by either estimate
# }
```
