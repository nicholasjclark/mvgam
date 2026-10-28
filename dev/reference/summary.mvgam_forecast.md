# Posterior summary of hindcast and forecast objects

Posterior medians and quantiles of the hindcast and forecast
distributions of each series, with the observed values in `data` and
`newdata` when the object holds `type = 'response'` predictions.

## Usage

``` r
# S3 method for class 'mvgam_forecast'
summary(object, probs = c(0.025, 0.975), ...)
```

## Arguments

- object:

  An `mvgam_forecast` object from
  [`hindcast()`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.md)
  or [`forecast()`](https://generics.r-lib.org/reference/forecast.html).

- probs:

  The lower and upper quantiles to report in addition to the median.

- ...:

  Unused. Anything passed here is refused.

## Value

A long-format `tibble` with one row per series and time, holding the
posterior median and the two quantiles of the predictions, the observed
value for `type = 'response'` and the prediction type. State components
of the trend add a `component` column, their names joined by `" + "`. A
multivariate fit on a long frame adds a leading `resp` column naming the
response.

## See also

[`forecast.mvgam`](https://nicholasjclark.github.io/mvgam/dev/reference/forecast.mvgam.md),
[`plot.mvgam_forecast`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_forecast.md)

## Author

Nicholas J Clark
