# Model-averaged posterior draws and predictions across mvgam fits

Bayesian model averaging of two or more `mvgam` fits, as in
[`brms::posterior_average.brmsfit()`](https://paulbuerkner.com/brms/reference/posterior_average.brmsfit.html)
and
[`brms::pp_average.brmsfit()`](https://paulbuerkner.com/brms/reference/pp_average.brmsfit.html).
Each fit contributes draws in proportion to its model weight.
[`posterior_average()`](https://paulbuerkner.com/brms/reference/posterior_average.brmsfit.html)
averages parameter draws.
[`pp_average()`](https://paulbuerkner.com/brms/reference/pp_average.brmsfit.html)
averages predictions.

## Usage

``` r
# S3 method for class 'mvgam'
posterior_average(
  x,
  ...,
  variable = NULL,
  pars = NULL,
  weights = "stacking",
  ndraws = NULL,
  nsamples = NULL,
  missing = NULL,
  model_names = NULL,
  control = list(),
  seed = NULL
)

# S3 method for class 'mvgam'
pp_average(
  x,
  ...,
  weights = "stacking",
  method = "posterior_predict",
  ndraws = NULL,
  nsamples = NULL,
  summary = TRUE,
  probs = c(0.025, 0.975),
  robust = FALSE,
  model_names = NULL,
  control = list(),
  seed = NULL
)
```

## Arguments

- x:

  A fitted `mvgam` object. Further fits are passed via `...`.

- ...:

  Further fitted `mvgam` objects to average over. For
  [`pp_average()`](https://paulbuerkner.com/brms/reference/pp_average.brmsfit.html),
  named arguments are passed to the prediction function `method` names.

- variable:

  Optional character vector of parameter names to extract, as
  [`variables.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/index-mvgam.md)
  lists them. A bare vector name selects every element. `NULL` (the
  default) selects all parameters shared across fits (or all parameters
  across the union when `missing` is supplied), excluding `"lp__"`.

- pars:

  Deprecated brms alias for `variable`.

- weights:

  A numeric vector with one weight per fit, scaled to sum to one, or one
  of `"stacking"`, `"pseudobma"`, `"pseudobma+"`, `"loo"` and `"waic"`.
  [mvgam_loo_extras](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_loo_extras.md)
  describes each.

- ndraws:

  Total number of model-averaged draws to return. Defaults to the number
  of draws in the first fit.

- nsamples:

  Deprecated brms alias for `ndraws`.

- missing:

  Values for parameters some fits lack. `NULL` (the default) restricts
  `variable` to the parameters every fit has and refuses a name outside
  them. A list keyed by parameter name gives each parameter its own
  value. A single number fills every parameter a fit lacks.

- model_names:

  Optional character vector of model labels; overrides the names
  inferred from the call.

- control:

  A list of extra arguments passed to
  [`loo::loo_model_weights()`](https://mc-stan.org/loo/reference/loo_model_weights.html)
  when computing stacking / pseudo-BMA weights.

- seed:

  Optional integer seed. The session's random state is restored when the
  call returns.

- method:

  Which prediction function
  [`pp_average()`](https://paulbuerkner.com/brms/reference/pp_average.brmsfit.html)
  calls on each fit before averaging: `"posterior_predict"` (the
  default), `"posterior_epred"` or `"posterior_linpred"`.

- summary:

  Logical. If `TRUE` (the default),
  [`pp_average()`](https://paulbuerkner.com/brms/reference/pp_average.brmsfit.html)
  summarises the averaged predictions with
  [`posterior_summary.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_diagnostics.md)
  as a mean, an error and two quantiles. `FALSE` returns the averaged
  draws.

- probs:

  Numeric vector of probabilities for quantile computation. Default is
  `c(0.025, 0.975)` for 95% credible intervals. Can be a single value or
  multiple values.

- robust:

  Logical. If `FALSE` (the default), uses mean and standard deviation
  for summaries. If `TRUE`, uses median and median absolute deviation
  (MAD).

## Value

[`posterior_average()`](https://paulbuerkner.com/brms/reference/posterior_average.brmsfit.html)
returns a `data.frame` of model-averaged posterior draws.
[`pp_average()`](https://paulbuerkner.com/brms/reference/pp_average.brmsfit.html)
returns a numeric matrix of averaged predictions. With `summary = TRUE`
its rows are observations and its columns summary statistics. Otherwise
its rows are draws and its columns observations. Attribute `weights`
holds the weight of each fit. Attribute `ndraws` holds the number of
draws taken from each fit.

## See also

[`loo_model_weights.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_loo_extras.md)
computes the weights.
[`ensemble()`](https://nicholasjclark.github.io/mvgam/dev/reference/ensemble.mvgam_forecast.md)
averages forecasts. The [forecast evaluation
article](https://nicholasjclark.github.io/mvgam/articles/forecast_evaluation.html)
weights and combines competing models.

Other model comparison:
[`bridge_sampler.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/bridge_sampler.mvgam.md),
[`compare_elpds()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_elpds.md),
[`kfold.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/kfold.mvgam.md),
[`lfo_cv.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/lfo_cv.mvgam.md),
[`log_lik.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/log_lik.mvgam.md),
[`loo.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/loo.mvgam.md),
[`loo_compare.mvgam_lfo()`](https://nicholasjclark.github.io/mvgam/dev/reference/loo_compare.mvgam_lfo.md),
[`mvgam_loo_extras`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_loo_extras.md),
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

# Stacking weights from approximate leave-one-out cross-validation
loo_model_weights(mod, mod_lin)
#> Method: stacking
#> ------
#>         weight
#> mod     0.898 
#> mod_lin 0.102 

# The intercept, averaged over both fits under those weights
pa <- posterior_average(mod, mod_lin, variable = "b_Intercept")
attr(pa, "weights")
#>      mod  mod_lin 
#> 0.897515 0.102485 
summary(pa)
#>   b_Intercept   
#>  Min.   :1.496  
#>  1st Qu.:1.598  
#>  Median :1.632  
#>  Mean   :1.632  
#>  3rd Qu.:1.667  
#>  Max.   :1.805  

# Averaged expected counts, one row per observation
head(pp_average(mod, mod_lin, method = "posterior_epred"))
#>       Estimate Est.Error     Q2.5     Q97.5
#> [1,]  2.895976 0.4848519 2.058548  3.941971
#> [2,] 12.971016 2.0798592 7.892636 16.448289
#> [3,]  7.127309 0.6490576 5.932693  8.456080
#> [4,]  8.690601 1.0261885 6.943644 11.103724
#> [5,]  2.769417 0.5250686 1.881987  3.980057
#> [6,]  8.318495 1.8816762 5.470898 12.854960
# }
```
