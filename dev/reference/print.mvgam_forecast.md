# Compact summary of an `mvgam_forecast` object

Without a dedicated method,
[`print()`](https://rdrr.io/r/base/print.html) falls back to the default
list printer and dumps every posterior matrix in the object, which is
dozens of pages of numbers for any non-trivial fit. This method prints a
single-screen header with the family, the number of series, the size of
the training and forecast windows, and the number of posterior draws.
Use
[`summary.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/summary.mvgam_forecast.md)
for the per-timepoint posterior summary tibble or
[`plot.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_forecast.md)
for the visual.

## Usage

``` r
# S3 method for class 'mvgam_forecast'
print(x, ...)
```

## Arguments

- x:

  An `mvgam_forecast` object from
  [`forecast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/forecast.mvgam.md),
  [`hindcast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.mvgam.md)
  or
  [`ensemble.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/ensemble.mvgam_forecast.md).

- ...:

  Ignored.

## Value

The `mvgam_forecast` object `x`, returned invisibly.

## See also

[`summary.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/summary.mvgam_forecast.md),
[`plot.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_forecast.md),
[`forecast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/forecast.mvgam.md),
[`hindcast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.mvgam.md),
[`ensemble.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/ensemble.mvgam_forecast.md),
[`score.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/score.mvgam_forecast.md),
[`compare_scores()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_scores.md)
