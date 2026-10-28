# Combine forecasts from mvgam models into a weighted ensemble

Build a single `mvgam_forecast` from the hindcast and forecast draws of
two or more `mvgam_forecast` objects. Each input supplies a share of the
draws set by its weight. Weights are even by default.

## Usage

``` r
ensemble(object, ...)

# S3 method for class 'mvgam_forecast'
ensemble(object, ..., weights = NULL, ndraws = 5000L, seed = NULL)
```

## Arguments

- object:

  An `mvgam_forecast` object returned by
  [`forecast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/forecast.mvgam.md)
  or
  [`hindcast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.mvgam.md).

- ...:

  More `mvgam_forecast` objects to combine.

- weights:

  Either `NULL` (the default, equivalent to even weighting), or a
  numeric vector with one non-negative entry per supplied forecast. The
  weights are normalised to sum to 1. Strategies such as stacking need
  the fitted models. Compute them with
  [`loo_model_weights.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_loo_extras.md)
  on the fits, or with
  [`loo_model_weights.mvgam_lfo()`](https://nicholasjclark.github.io/mvgam/dev/reference/loo_model_weights.mvgam_lfo.md)
  on their
  [`lfo_cv()`](https://nicholasjclark.github.io/mvgam/dev/reference/lfo_cv.mvgam.md)
  results. Pass the numeric result here.

- ndraws:

  Positive integer giving the total number of ensemble draws to return
  per series. Defaults to `5000`. A model allocated more draws than it
  holds is sampled with replacement.

- seed:

  Optional integer seed. The session's random state is restored when the
  call returns.

## Value

An object of class `mvgam_forecast` containing the ensembled hindcasts
and forecasts. Suitable for
[`plot.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_forecast.md)
and
[`score.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/score.mvgam_forecast.md).
Attribute `weights` records the per-model weights used. Attribute
`ndraws_per_model` records the integer draw counts drawn from each
input.

## Details

Per-model weights are split into integer draw counts that sum to
`ndraws` by the largest-remainder rule, after
`brms::round_largest_remainder()`. Each model then gives one set of
sampled draws. Every series, in the hindcasts and in the forecasts,
takes that same set. The ensemble keeps the dependence between series
and the pairing of each hindcast with its forecast.

The returned object takes its series names, family, training data and
test data from the first input. All inputs must share the series names,
the hindcast lengths, the forecast horizons and, when present, the test
observations.

A multivariate fit on a wide frame gives one forecast whose series are
its responses. The ensemble is a forecast over those series. On a long
frame the ensemble keeps one forecast per response. Every response takes
the same draws.

## See also

[`score.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/score.mvgam_forecast.md)
scores the ensemble.
[`loo_model_weights.mvgam_lfo()`](https://nicholasjclark.github.io/mvgam/dev/reference/loo_model_weights.mvgam_lfo.md)
computes weights from forecast performance.
[`pp_average.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_average.mvgam.md)
averages predictions in place of forecasts. The [forecast evaluation
article](https://nicholasjclark.github.io/mvgam/dev/articles/forecast_evaluation.html)
weights an ensemble by leave-future-out stacking weights.

Other forecasting:
[`compare_scores()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_scores.md),
[`forecast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/forecast.mvgam.md),
[`hindcast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.mvgam.md),
[`lfo_cv.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/lfo_cv.mvgam.md),
[`score.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/score.mvgam_forecast.md)

## Author

Nicholas J Clark

## Examples

``` r
# \dontrun{
set.seed(1)
simdat <- sim_mvgam(family = poisson(), n_series = 1L,
                    n_timepoints = 120L, trend_model = AR())
# Two models of one count series: a smooth of x with and without
# an AR(1) trend. The trend model samples with smaller steps
# (adapt_delta = 0.95). That avoids divergent transitions when
# lfo_cv() refits it to shorter stretches of the series
mod_gam <- mvgam(y ~ s(x, k = 5), data = simdat$data_train,
                 family = poisson(), chains = 2, silent = 2)
#> Warning: 209 of 2000 (10.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
mod_ar <- mvgam(y ~ s(x, k = 5), trend_formula = ~ AR(p = 1),
                data = simdat$data_train, family = poisson(),
                chains = 2, silent = 2,
                control = list(adapt_delta = 0.95))
#> Warning: 3 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
# }
# \dontrun{
fc_gam <- forecast(mod_gam, newdata = simdat$data_test)
fc_ar <- forecast(mod_ar, newdata = simdat$data_test)

# An ensemble that draws 30% of its draws from the GAM
ens <- ensemble(fc_gam, fc_ar, weights = c(0.3, 0.7))
attr(ens, "ndraws_per_model")
#> fc_gam  fc_ar 
#>   1500   3500 
plot(ens)


# The ensemble scored against its components
scores <- compare_scores(fc_gam, fc_ar, ens, score = "drps",
                         model_names = c("GAM", "AR", "Ensemble"))
plot(scores, relative = "GAM")

# }
```
