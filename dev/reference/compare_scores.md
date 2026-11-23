# Tidy comparison of forecast scores across mvgam models

Scores the forecasts of several models of the same held-out data with
one rule, then stacks the results in one table for comparison. Lower
scores are better. The table has one row per model, series and forecast
horizon. A joint score (`"energy"`, `"variogram"` or `"twenergy"`)
scores all series together. Its per-series rows hold `NA`. The rows with
`series == "all_series"` hold its value.

## Usage

``` r
compare_scores(..., score = "crps", model_names = NULL, score_args = list())

# S3 method for class 'mvgam_compare_scores'
plot(x, relative = NULL, facet = TRUE, ...)

# S3 method for class 'mvgam_compare_scores'
summary(object, ...)

# S3 method for class 'mvgam_compare_scores'
print(x, digits = 2L, ...)
```

## Arguments

- ...:

  Two or more `mvgam_forecast` objects from
  [`forecast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/forecast.mvgam.md).
  A named argument, as in `compare_scores(GAM = fc1, AR = fc2)`, names
  its model. An unnamed one takes the expression passed, such as
  `"fc1"`.

- score:

  Character name of the scoring rule. The section 'Available scores' of
  [`score.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/score.mvgam_forecast.md)
  lists them.

- model_names:

  Optional character vector of model names, one per forecast. It
  replaces the names taken from the call.

- score_args:

  Optional named list of further arguments of
  [`score.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/score.mvgam_forecast.md),
  such as `score_args = list(interval_width = 0.6)`.

- x:

  An `mvgam_compare_scores` object.

- relative:

  Optional name of a model in `x$model`. Each score is then drawn less
  that model's at the same series and horizon, with a dashed zero line.
  Negative values favour the other model. The baseline itself is left
  out.

- facet:

  Logical. `TRUE` (the default) gives a univariate score one panel per
  series. `FALSE` overlays the series, told apart by line type. A joint
  score ignores it.

- object:

  An `mvgam_compare_scores` object.

- digits:

  Integer number of significant digits. Defaults to 2.

## Value

A `tibble` of class `mvgam_compare_scores`. Its
[`print()`](https://rdrr.io/r/base/print.html) method ranks the models
by their mean score. Its columns are:

- `model`: a factor with one level per forecast.

- `series`: a factor of series names, with an `"all_series"` level for a
  joint score.

- `eval_horizon`: the forecast horizon, 1 for the first held-out time.

- `score`: the value of the score.

- `in_interval`: whether the observation fell in the central interval,
  as
  [`score.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/score.mvgam_forecast.md)
  reports it.

[`summary()`](https://rdrr.io/r/base/summary.html) returns the scores
wide, one column per model and one row per series and horizon, or per
horizon for a joint score.
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) draws each
model's score by horizon. A univariate score takes one panel per series
and a joint score one panel. An ensemble plots as one more model.

## See also

[`score.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/score.mvgam_forecast.md),
[`compare_elpds()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_elpds.md),
[`forecast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/forecast.mvgam.md),
[`ensemble.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/ensemble.mvgam_forecast.md)

Other forecasting:
[`ensemble.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/ensemble.mvgam_forecast.md),
[`forecast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/forecast.mvgam.md),
[`forecast_uncertainty()`](https://nicholasjclark.github.io/mvgam/dev/reference/forecast_uncertainty.md),
[`hindcast()`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.mvgam.md),
[`lfo_cv.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/lfo_cv.mvgam.md),
[`score()`](https://nicholasjclark.github.io/mvgam/dev/reference/score.mvgam_forecast.md)

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
#> Warning: 8 of 2000 (0.0%) transitions ended with a divergence.
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

# One row per model and horizon, scored by the discrete ranked
# probability score (lower is better)
scores <- compare_scores(fc_gam, fc_ar, score = "drps",
                         model_names = c("GAM", "AR"))
summary(scores)
#> # A tibble: 30 × 4
#>    series   eval_horizon    GAM    AR
#>    <fct>           <int>  <dbl> <dbl>
#>  1 series_1            1  2.54  1.41 
#>  2 series_1            2  2.53  1.20 
#>  3 series_1            3 10.5   6.50 
#>  4 series_1            4  8.16  6.09 
#>  5 series_1            5  8.80  4.39 
#>  6 series_1            6  4.20  2.11 
#>  7 series_1            7  0.729 0.912
#>  8 series_1            8  0.816 0.856
#>  9 series_1            9  2.87  1.92 
#> 10 series_1           10  4.49  2.77 
#> # ℹ 20 more rows

# The AR model's DRPS minus the GAM's. Negative values favour the
# AR model
plot(scores, relative = "GAM")


# Next, ensemble() combines the two forecasts
# }
```
