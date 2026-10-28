# Approximate leave-one-out cross-validation for mvgam models

Estimates the expected log predictive density (ELPD) of each observation
had it been left out of the fit, with
[`loo::loo()`](https://mc-stan.org/loo/reference/loo.html). The ELPD
measures how well a model predicts data it has not seen. Higher is
better. Each observation also takes a Pareto \\k\\ diagnostic. Above 0.7
the approximation is unreliable for that observation.

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
  response. `save_psis` keeps the smoothing object. Supplying any of the
  others raises an error. To compare several models, pass the fits to
  `loo_compare.mvgam()`.

- model_names:

  Character vector of row names for the comparison, one per model.
  `NULL` takes them from the call.

- incl_autocor:

  Logical, default `TRUE`. Passed to
  [`log_lik.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/log_lik.mvgam.md).
  `TRUE` scores each observation under the latent trend state the model
  inferred at that time. The ELPD then describes the series that was
  observed. `FALSE` scores the covariate part of the model alone.

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

Exact leave-one-out cross-validation would refit the model once per
observation. Pareto smoothed importance sampling (PSIS) avoids the
refits. It re-weights the posterior draws of the one fit to approximate
the posterior without each observation (Vehtari et al. 2017).
[`loo::loo()`](https://mc-stan.org/loo/reference/loo.html) describes the
method.

The approximation often fails for a fit with a latent trend. Each
observation strongly informs the state at its own time. Removing it
changes that part of the posterior a great deal. The importance weights
then vary widely. Many observations get a Pareto \\k\\ above 0.7. The
effective number of parameters also climbs towards the number of
observations. Check the Pareto \\k\\ diagnostics before trusting a
comparison.
[`lfo_cv.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/lfo_cv.mvgam.md)
refits along the time axis and scores each window against data the model
has not seen.
[`forecast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/forecast.mvgam.md)
with
[`score.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/score.mvgam_forecast.md)
scores a held-out horizon.

## Which comparison tool?

- Fit to the observed data:

  [`loo()`](https://mc-stan.org/loo/reference/loo.html) or
  [`waic.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/waic.mvgam.md),
  ranked with `loo_compare.mvgam()`. Trust it for a model without a
  trend, or once the Pareto \\k\\ diagnostics are low.

- Skill at forecasting:

  [`lfo_cv.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/lfo_cv.mvgam.md)
  on each model, ranked with
  [`loo_compare.mvgam_lfo()`](https://nicholasjclark.github.io/mvgam/dev/reference/loo_compare.mvgam_lfo.md)
  and plotted with
  [`compare_elpds()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_elpds.md).

- Accuracy over one held-out horizon:

  [`forecast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/forecast.mvgam.md)
  with
  [`score.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/score.mvgam_forecast.md),
  several models at once with
  [`compare_scores()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_scores.md).

- Prediction for held-out sites or species:

  [`kfold.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/kfold.mvgam.md)
  with `group`.

The ELPD is higher for the better model. CRPS, DRPS and the other scores
of
[`score.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/score.mvgam_forecast.md)
are lower for the better model.

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
article](https://nicholasjclark.github.io/mvgam/dev/articles/forecast_evaluation.html)
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
#> Warning: 3 of 2000 (0.0%) transitions ended with a divergence.
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
#> elpd_loo   -208.9  8.5
#> p_loo         8.0  1.6
#> looic       417.7 16.9
#> ------
#> MCSE of elpd_loo is 0.1.
#> MCSE and ESS estimates assume MCMC draws (r_eff in [0.6, 1.2]).
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
