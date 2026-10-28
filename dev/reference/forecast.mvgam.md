# Posterior forecasts for an mvgam state-space model

Posterior forecasts at future time points, with posterior hindcasts at
the training time points. Each draw propagates the fitted latent state
through the trend's dynamics. The observation family then maps the trend
to the response scale.

## Usage

``` r
# S3 method for class 'mvgam'
forecast(
  object,
  newdata = NULL,
  ...,
  type = c("response", "link", "expected", "trend"),
  component = NULL,
  ndraws = NULL,
  draw_ids = NULL,
  coef_uncertainty = TRUE,
  trend_uncertainty = TRUE,
  obs_uncertainty = TRUE,
  resp = NULL
)
```

## Arguments

- object:

  A fitted
  [mvgam](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md)
  object.

- newdata:

  Optional `data.frame` with the columns of the training data, holding
  the time and series cells to forecast. Rows with times beyond the
  training grid set the horizon. When `NULL`,
  [`forecast()`](https://generics.r-lib.org/reference/forecast.html)
  uses the held-out data given to `mvgam(newdata = )`. A fit without
  held-out data needs `newdata`.

  Five trends step once per time point: AR(), RW(), VAR(), LLT() and
  PW(). Their forecast times must continue the training series at its
  own spacing, without a gap. After training ends at `t = 30`, a request
  for `t = 41:42` is refused: two rows would report a two-step spread
  for a twelve-step horizon. Supply every intervening time, whether or
  not the response is observed there.
  [`CAR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
  takes the elapsed gap into its kernel. Each series may have its own
  forecast times, all after the last fitted time.
  [`ZMVN()`](https://nicholasjclark.github.io/mvgam/dev/reference/ZMVN.md)
  has no temporal dynamics and takes any later times.
  [`ZMVN()`](https://nicholasjclark.github.io/mvgam/dev/reference/ZMVN.md)
  and
  [`PW()`](https://nicholasjclark.github.io/mvgam/dev/reference/piecewise_trends.md)
  need one set of forecast times shared by every series.

- ...:

  Unused. Anything passed here is refused.

- type:

  The scale of the draws:

  - `"response"` (the default) samples from the observation family.

  - `"expected"` returns the family's mean. A binomial mean is
    `trials * p` and a zero-inflated mean is `(1 - zi) * mu`.

  - `"link"` returns the linear predictor on the link scale.

  - `"trend"` returns the latent trend on the link scale.

- component:

  `NULL` (the default) for the summed trend, or the names of state
  components of the trend, with `type = "trend"`. Several names give the
  sum of their components.
  [`LLT()`](https://nicholasjclark.github.io/mvgam/dev/reference/LLT.md)
  has `"level"`, the stochastic level, and `"slope"`, its growth per
  time step. A seasonal
  [`LLT()`](https://nicholasjclark.github.io/mvgam/dev/reference/LLT.md)
  adds `"season"`. `"predictor"` holds the effects of the trend
  formula's covariates. `c("level", "predictor")` gives the level with
  those effects. A factor model projects each component through its
  loadings, as it projects the trend. The result is an ordinary
  [mvgam_forecast-class](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md)
  with the components in its `component` slot.

- ndraws:

  Positive integer specifying number of posterior draws to use. NULL
  (default) uses all available draws.

- draw_ids:

  Optional integer vector selecting a subset of posterior draw indices
  to use. NULL (default) uses all draws (subject to `ndraws`).

- coef_uncertainty:

  Logical. When `FALSE`, every forecast draw uses the first of the draws
  `ndraws` or `draw_ids` select for every coefficient in the observation
  and trend formulas: population-level effects, smooth bases, random
  effect levels and Hilbert-space `gp(x, k)` bases. Defaults to `TRUE`.

- trend_uncertainty:

  Logical. When `FALSE`, every forecast draw uses the first of the
  selected draws for the trend dynamics parameters (`sigma`, AR, MA and
  VAR coefficients), the last fitted state and the trend formula's
  predictor `mu_trend`. Each draw still takes fresh innovations. The
  spread then shows innovation noise alone. Defaults to `TRUE`.

- obs_uncertainty:

  Logical. When `FALSE`, skips observation-family sampling for
  `type = "response"`, returning the family mean instead. Defaults to
  `TRUE`.

- resp:

  Character specifying which response variable for multivariate models.
  NULL (default) returns predictions for all responses.

## Value

An object of class
[mvgam_forecast-class](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md).
Hindcasts and forecasts are returned as named lists of
`[ndraws, n_times]` matrices, one per series.

## See also

[`hindcast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.mvgam.md)
returns the fitted latent states.
[`posterior_predict.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_predict.mvgam.md)
and
[`posterior_epred.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_epred.mvgam.md)
predict by integrating over the trend's stochastic dynamics.
[`forecast()`](https://generics.r-lib.org/reference/forecast.html)
extrapolates the fitted latent state.
[`score.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/score.mvgam_forecast.md)
scores a forecast against held-out data. The [forecast evaluation
article](https://nicholasjclark.github.io/mvgam/articles/forecast_evaluation.html)
scores, compares and combines the forecasts of three models of rodent
counts.

Other forecasting:
[`compare_scores()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_scores.md),
[`ensemble.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/ensemble.mvgam_forecast.md),
[`hindcast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.mvgam.md),
[`lfo_cv.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/lfo_cv.mvgam.md),
[`score.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/score.mvgam_forecast.md)

## Examples

``` r
# \dontrun{
set.seed(11)
simdat <- sim_mvgam(family = poisson(), n_series = 1L,
                     n_timepoints = 120L, trend_model = AR(),
                     proportional_train = 0.75)

mod <- mvgam(
  y ~ s(x),
  trend_formula = ~ AR(p = 1),
  data    = simdat$data_train,
  newdata = simdat$data_test,
  family  = poisson(),
  chains  = 2, silent = 2
)
#> Warning: 386 of 2000 (19.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.

# The held-out data given to mvgam() set the forecast horizon
fc <- forecast(mod)
plot(fc)


# The latent AR(1) trend alone, on the log scale
plot(forecast(mod, type = "trend"))


# The expected counts, without the Poisson sampling noise
plot(forecast(mod, type = "expected"))


# Next, score the forecast against the held-out counts
head(score(fc, score = "crps")[[1]])
#>       score in_interval interval_width eval_horizon score_type
#> 1 1.8327820           1            0.9            1       crps
#> 2 1.9730375           1            0.9            2       crps
#> 3 2.5622158           1            0.9            3       crps
#> 4 1.3601955           1            0.9            4       crps
#> 5 0.6947603           1            0.9            5       crps
#> 6 3.9339607           1            0.9            6       crps
# }
```
