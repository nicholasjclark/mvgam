# LOO information criteria for mvgam models

Extract the LOOIC (leave-one-out information criterion) using
[`loo::loo()`](https://mc-stan.org/loo/reference/loo.html).

## Usage

``` r
# S3 method for class 'mvgam'
loo(
  x,
  ...,
  compare = TRUE,
  resp = NULL,
  pointwise = FALSE,
  moment_match = FALSE,
  reloo = FALSE,
  k_threshold = 0.7,
  save_psis = FALSE,
  moment_match_args = list(),
  reloo_args = list(),
  model_names = NULL,
  incl_autocor = TRUE,
  incl_dynamics = NULL,
  by_series = FALSE
)

# S3 method for class 'mvgam'
loo_compare(
  x,
  ...,
  criterion = c("loo", "waic"),
  model_names = NULL,
  incl_autocor = TRUE,
  incl_dynamics = NULL
)
```

## Arguments

- x:

  A fitted `mvgam` or `jsdgam` object.

- ...:

  For [`loo()`](https://mc-stan.org/loo/reference/loo.html), further
  arguments to
  [`loo::loo()`](https://mc-stan.org/loo/reference/loo.html). For
  [`loo_compare()`](https://mc-stan.org/loo/reference/loo_compare.html),
  further fitted models.

- compare, resp, pointwise, moment_match, reloo, k_threshold, save_psis,
  moment_match_args, reloo_args:

  The arguments of
  [brms::loo.brmsfit](https://paulbuerkner.com/brms/reference/loo.brmsfit.html).
  mvgam uses two of them. `resp` scopes a multivariate fit to one
  response. `save_psis` keeps the smoothing object. `pointwise` would
  stream the density. `moment_match` and `reloo` would refit the model,
  configured by `k_threshold` and the `*_args` lists. `compare` and
  `model_names` would rank several models, which `loo_compare.mvgam()`
  does. mvgam runs none of these. Supplying one raises an error naming
  it.

- model_names:

  Character vector of row names for the comparison, one per model.
  `NULL` takes them from the call.

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

- criterion:

  `"loo"` (the default) or `"waic"`.

## Value

For `loo.mvgam`, an object of class `psis_loo` (see
[`loo::loo()`](https://mc-stan.org/loo/reference/loo.html)). For
`loo_compare.mvgam`, the `compare.loo` table of
[`loo::loo_compare()`](https://mc-stan.org/loo/reference/loo_compare.html),
one row per model, best first. Its `p_worse`, `diag_diff` and
`diag_elpd` columns follow Sivula et al. (2025).

## Details

When comparing two (or more) fitted `mvgam` models, we can estimate the
difference in their in-sample predictive accuracies using the Expected
Log Predictive Density (ELPD). This metric can be approximated using
Pareto Smoothed Importance Sampling (PSIS), which re-weights posterior
draws to approximate predictions for a datapoint had it not been
included in the original model fit (i.e. leave-one-out
cross-validation).

See [`loo::loo()`](https://mc-stan.org/loo/reference/loo.html) and
[`loo::loo_compare()`](https://mc-stan.org/loo/reference/loo_compare.html)
for further details on how this importance sampling works.

A fit with a latent trend routinely strains this approximation. Leaving
an observation out keeps its influence on the state the model inferred
at that time. The re-weighted density still conditions on a state
estimated from that observation. The ELPD then measures conditional
in-sample fit. The effective number of parameters climbs towards the
number of observations. Check the Pareto \\k\\ diagnostics before
trusting a comparison.
[`lfo_cv.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/lfo_cv.mvgam.md)
refits along the time axis and scores each window against data the model
has not seen.
[`forecast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/forecast.mvgam.md)
with
[`score.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/score.mvgam_forecast.md)
scores a held-out horizon.

## References

Vehtari, A., Gelman, A. and Gabry, J. (2017). Practical Bayesian model
evaluation using leave-one-out cross-validation and WAIC. *Statistics
and Computing*, 27:1413-1432.
[doi:10.1007/s11222-016-9696-4](https://doi.org/10.1007/s11222-016-9696-4)

Burkner, P.-C., Gabry, J. and Vehtari, A. (2020). Approximate
leave-future-out cross-validation for Bayesian time series models.
*Journal of Statistical Computation and Simulation*, 90(14):2499-2523.
[doi:10.1080/00949655.2020.1783262](https://doi.org/10.1080/00949655.2020.1783262)

Sivula, T., Magnusson, M., Matamoros, A. A. and Vehtari, A. (2025).
Uncertainty in Bayesian leave-one-out cross-validation based model
comparison. *Bayesian Analysis*.
[doi:10.1214/25-BA1569](https://doi.org/10.1214/25-BA1569)

## See also

[`lfo_cv.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/lfo_cv.mvgam.md)
scores forecasts by refitting along the time axis.
[`kfold.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/kfold.mvgam.md)
runs grouped k-fold cross-validation.
[`loo_model_weights.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_loo_extras.md)
weights models by their predictions. The [forecast evaluation
article](https://nicholasjclark.github.io/mvgam/articles/forecast_evaluation.html)
compares models by approximate and exact cross-validation.

Other model comparison:
[`bridge_sampler.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/bridge_sampler.mvgam.md),
[`compare_elpds()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_elpds.md),
[`kfold.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/kfold.mvgam.md),
[`lfo_cv.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/lfo_cv.mvgam.md),
[`log_lik.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/log_lik.mvgam.md),
[`loo_compare.mvgam_lfo()`](https://nicholasjclark.github.io/mvgam/dev/reference/loo_compare.mvgam_lfo.md),
[`mvgam_loo_extras`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_loo_extras.md),
[`posterior_average.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_average.mvgam.md),
[`waic.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/waic.mvgam.md)

## Author

Nicholas J Clark

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
# The same smooth of x replaced by a linear effect
mod_lin <- update(mod, formula. = y ~ x)

# The ELPD estimate, with Pareto k diagnostics for each observation
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

# The better model comes first, with elpd_diff = 0. The diagnostic
# columns say how far the difference can be trusted
loo_compare(mod, mod_lin)
#>    model elpd_diff se_diff p_worse diag_diff diag_elpd
#>      mod       0.0     0.0      NA                    
#>  mod_lin     -26.1     9.7    1.00   N < 100          
#> 
#> Diagnostic flags present.
#> See ?`loo-glossary` (sections `diag_diff` and `diag_elpd`)
#> or https://mc-stan.org/loo/reference/loo-glossary.html.

# Next: lfo_cv() scores each model's forecasts of data it has not
# seen
# }
```
