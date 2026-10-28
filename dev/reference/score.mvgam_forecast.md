# Proper scoring rules for `mvgam_forecast` objects

Scores the held-out forecasts of an `mvgam_forecast` object from
[`forecast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/forecast.mvgam.md)
or
[`hindcast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.mvgam.md)
against the observations, with one of eleven scoring rules. Lower scores
are better.

## Usage

``` r
# S3 method for class 'mvgam_forecast'
score(
  object,
  score = "crps",
  interval_width = 0.9,
  quantile_level = 0.5,
  lower = -Inf,
  upper = Inf,
  log = FALSE,
  weights = NULL,
  ...
)
```

## Arguments

- object:

  An `mvgam_forecast` object whose `type` is `"response"` or
  `"expected"`. The held-out observations are then on the scale of the
  forecasts.

- score:

  Character; one of the names under 'Available scores'. `"drps"` suits
  counts, `"crps"` continuous data and `"brier"` binary data. `"energy"`
  and `"variogram"` score several series jointly. Lower scores are
  better.

- interval_width:

  Width of the central interval that sets the per-series `in_interval`
  indicator and the `"sis"` penalty. The indicator is computed for
  `"crps"`, `"drps"`, `"sis"` and the joint scores. The other scores
  report it as `NA`.

- quantile_level:

  Level of the `"qs"` score. Defaults to `0.5`, the median.

- lower, upper:

  Thresholds of `"twcrps"` and `"twenergy"`. The defaults `-Inf` and
  `Inf` give the unweighted scores.

- log:

  Logical. `TRUE` applies `log(x + 0.001)` to observations and draws
  before scoring. Taken by `"crps"`, `"sis"`, `"energy"` and
  `"variogram"`. Passing `TRUE` with any other score is an error.

- weights:

  Optional numeric vector of non-negative weights, one per series, for
  `"variogram"`.

- ...:

  Unused. Anything passed here is refused.

## Value

A named `list` with one data frame per series plus a trailing
`all_series` summary. Each series' data frame has columns `score`,
`in_interval`, `interval_width`, `eval_horizon` and `score_type`, the
requested `score`. A joint score leaves the per-series `score` column
`NA`.

For a univariate score, `all_series` sums the scores over the series at
each forecast time, with `score_type` prefixed `"sum_"`. Series forecast
at their own times, as
[`CAR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
allows, are summed at the times they share. Its `eval_horizon` then
counts the times any series is forecast at. For a joint score,
`all_series` holds the score at each horizon. A joint score needs every
series at the same forecast times.

## Available scores

- `"crps"`:

  Continuous ranked probability score (Gneiting and Raftery 2007) of the
  empirical distribution of the draws.

- `"drps"`:

  Discrete ranked probability score, the CRPS of a count summed over its
  integer support.

- `"sis"`:

  Scaled interval score: the width of the central interval plus a
  penalty for an observation outside it, scaled by
  `2 / (1 - interval_width)`.

- `"brier"`:

  Brier score of a binary outcome: the squared difference between the
  observation and the mean of the draws.

- `"logs"`:

  Log score, `-log f(y)` for a kernel density estimate of the draws.

- `"dss"`:

  Dawid-Sebastiani score, which depends on the predictive mean and
  standard deviation alone.

- `"qs"`:

  Quantile (pinball) score at `quantile_level`.

- `"twcrps"`:

  Threshold-weighted CRPS, which scores the forecast inside
  `[lower, upper]`.

- `"energy"`:

  Energy score of the series jointly.

- `"variogram"`:

  Variogram score of order 0.5 of the series jointly (Scheuerer and
  Hamill 2015).

- `"twenergy"`:

  Threshold-weighted energy score.

## References

Gneiting, T. and Raftery, A. E. (2007). Strictly Proper Scoring Rules,
Prediction, and Estimation. *Journal of the American Statistical
Association*, 102(477), 359-378.
[doi:10.1198/016214506000001437](https://doi.org/10.1198/016214506000001437)

Scheuerer, M. and Hamill, T. M. (2015). Variogram-based proper scoring
rules for probabilistic forecasts of multivariate quantities. *Monthly
Weather Review*, 143(4), 1321-1334.
[doi:10.1175/MWR-D-14-00269.1](https://doi.org/10.1175/MWR-D-14-00269.1)

## See also

[`forecast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/forecast.mvgam.md)
produces the forecast.
[`compare_scores()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_scores.md)
scores several forecasts with one rule.
[`ensemble.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/ensemble.mvgam_forecast.md)
combines forecasts. The [forecast evaluation
article](https://nicholasjclark.github.io/mvgam/dev/articles/forecast_evaluation.html)
compares models by their scores at each horizon.

Other forecasting:
[`compare_scores()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_scores.md),
[`ensemble.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/ensemble.mvgam_forecast.md),
[`forecast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/forecast.mvgam.md),
[`hindcast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.mvgam.md),
[`lfo_cv.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/lfo_cv.mvgam.md)

## Examples

``` r
# \dontrun{
set.seed(11)
simdat <- sim_mvgam(family = poisson(), n_series = 2L,
                    n_timepoints = 120L, trend_model = AR())
mod <- mvgam(y ~ s(x, k = 5),
             trend_formula = ~ AR(p = 1),
             data = simdat$data_train,
             family = poisson(),
             chains = 2, silent = 2)
#> Warning: 491 of 2000 (25.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.

# Forecast the held-out times, then score each series
fc <- forecast(mod, newdata = simdat$data_test)
sc <- score(fc, score = "drps")
head(sc$series_1)
#>       score in_interval interval_width eval_horizon score_type
#> 1 12.018737           0            0.9            1       drps
#> 2  6.756861           1            0.9            2       drps
#> 3  5.054312           0            0.9            3       drps
#> 4  5.029708           1            0.9            4       drps
#> 5 32.373675           0            0.9            5       drps
#> 6  3.049877           1            0.9            6       drps

# The DRPS summed over series at each forecast horizon
library(ggplot2)
ggplot(sc$all_series, aes(eval_horizon, score)) +
  geom_line(colour = "#8F2727") +
  geom_point(colour = "#8F2727") +
  labs(x = "Horizon", y = "DRPS summed over series") +
  mvgam_theme()


# A joint score of both series at each horizon
head(score(fc, score = "energy")$all_series)
#>       score eval_horizon score_type
#> 1 11.037514            1     energy
#> 2 10.566751            2     energy
#> 3  5.058591            3     energy
#> 4  5.834418            4     energy
#> 5 32.058046            5     energy
#> 6  3.370899            6     energy

# Next: compare_scores() ranks several models by one rule
# }
```
