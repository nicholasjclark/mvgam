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

  Optional `data.frame` with the columns of the training data, one row
  per series and future time to forecast. When `NULL`,
  [`forecast()`](https://generics.r-lib.org/reference/forecast.html)
  uses the held-out data given to `mvgam(newdata = )`. A fit without
  held-out data needs `newdata`.

  The trend decides which future times are allowed:

  - [`AR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md),
    [`RW()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md),
    [`VAR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md),
    [`LLT()`](https://nicholasjclark.github.io/mvgam/dev/reference/LLT.md)
    and
    [`PW()`](https://nicholasjclark.github.io/mvgam/dev/reference/piecewise_trends.md)
    step once per time point. Their forecast times must continue each
    series at its own spacing, with no gap. After training ends at
    `t = 30`, rows for `t = 41:42` alone are refused. The trend would
    step once and twice, though those times are 11 and 12 steps past the
    end of training. Supply every time in between, with or without a
    response.

  - [`CAR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
    allows for the gap between times. Each series may have its own
    forecast times, all after the last fitted time.

  - [`ZMVN()`](https://nicholasjclark.github.io/mvgam/dev/reference/ZMVN.md)
    has no time dynamics and takes any later times.

  [`ZMVN()`](https://nicholasjclark.github.io/mvgam/dev/reference/ZMVN.md)
  and
  [`PW()`](https://nicholasjclark.github.io/mvgam/dev/reference/piecewise_trends.md)
  need the same forecast times for every series.

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
  loadings, as it projects the trend.

- ndraws:

  Positive integer specifying number of posterior draws to use. NULL
  (default) uses all available draws.

- draw_ids:

  Optional integer vector selecting a subset of posterior draw indices
  to use. NULL (default) uses all draws (subject to `ndraws`).

- coef_uncertainty:

  Logical, `TRUE` by default. `FALSE` holds the coefficients of the
  observation and trend formulas at one posterior draw, the first that
  `ndraws` or `draw_ids` selects. That includes smooth, random-effect
  and Hilbert-space `gp(x, k)` coefficients. The forecast then leaves
  out their uncertainty. Turning off one source of uncertainty at a time
  shows how much each contributes to a forecast.

- trend_uncertainty:

  Logical, `TRUE` by default. `FALSE` holds the parameters of the trend
  dynamics (`sigma`, AR, MA and VAR coefficients), the last fitted state
  and the trend formula's predictor at that same draw. Each draw still
  takes new innovations. The spread then shows innovation noise alone.

- obs_uncertainty:

  Logical, `TRUE` by default. `FALSE` returns the family's mean under
  `type = "response"`, in place of a sampled observation.

- resp:

  Character specifying which response variable for multivariate models.
  NULL (default) returns predictions for all responses.

## Value

An object of class
[mvgam_forecast-class](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md).
Hindcasts and forecasts are returned as named lists of
`[ndraws, n_times]` matrices, one per series. A forecast of state
components names them in its `component` slot.

## See also

[`hindcast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.mvgam.md)
returns the fitted latent states.
[`posterior_predict.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_predict.mvgam.md)
and
[`posterior_epred.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_epred.mvgam.md)
predict from the covariate part of the model.
[`forecast()`](https://generics.r-lib.org/reference/forecast.html)
extrapolates the fitted latent state.
[`score.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/score.mvgam_forecast.md)
scores a forecast against held-out data. The [forecast evaluation
article](https://nicholasjclark.github.io/mvgam/dev/articles/forecast_evaluation.html)
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
  y ~ s(x, k = 5),
  trend_formula = ~ AR(p = 1),
  data    = simdat$data_train,
  newdata = simdat$data_test,
  family  = poisson(),
  chains  = 2, silent = 2
)
#> Warning: 36 of 2000 (2.0%) transitions ended with a divergence.
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
#> 1 1.7075098           1            0.9            1       crps
#> 2 2.2048900           1            0.9            2       crps
#> 3 2.2784688           1            0.9            3       crps
#> 4 1.4094038           1            0.9            4       crps
#> 5 0.6126763           1            0.9            5       crps
#> 6 4.3284377           1            0.9            6       crps
# }
```
