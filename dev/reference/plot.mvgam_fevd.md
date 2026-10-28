# Plot forecast error variance decompositions from an `mvgam_fevd` object

This function takes an `mvgam_fevd` object and produces a plot of the
posterior median contributions to forecast variance for each series in
the fitted Vector Autoregression

## Usage

``` r
# S3 method for class 'mvgam_fevd'
plot(x, series = NULL, contributing = NULL, ...)
```

## Arguments

- x:

  `list` object of class `mvgam_fevd`. See
  [`fevd()`](https://nicholasjclark.github.io/mvgam/dev/reference/fevd.mvgam.md)

- series:

  Optional positions or names of the target processes shown as facets.
  Useful on hierarchical VAR fits where the raw K x K panel would run to
  dozens of panels. Defaults to all processes.

- contributing:

  Optional positions or names of the source processes stacked as
  contributions to each target. The shares of the sources drawn are
  renormalised to sum to one. On a hierarchical VAR this isolates the
  dependencies within one group. Defaults to all processes.

- ...:

  Unused. Anything passed here is refused.

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object, which can be further customized using the ggplot2 package

## See also

[`fevd()`](https://nicholasjclark.github.io/mvgam/dev/reference/fevd.mvgam.md),
[`irf()`](https://nicholasjclark.github.io/mvgam/dev/reference/irf.mvgam.md),
[`stability()`](https://nicholasjclark.github.io/mvgam/dev/reference/stability.mvgam.md),
[`plot.mvgam_irf()`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_irf.md),
[`plot.mvgam_stability()`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_stability.md),
[`plot.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_forecast.md)

## Author

Nicholas J Clark
