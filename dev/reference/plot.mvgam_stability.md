# Plot posterior distributions of latent VAR stability metrics

Draws a histogram of the posterior draws of each metric
[`stability.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/stability.mvgam.md)
returned. A dashed line at zero on the reactivity panel marks the
threshold above which the system amplifies a shock before it decays. A
hierarchical VAR draws a median and interval for each panel.

## Usage

``` r
# S3 method for class 'mvgam_stability'
plot(
  x,
  variables = c("reactivity", "mean_return_rate", "var_return_rate"),
  bins = 30L,
  ...
)
```

## Arguments

- x:

  A `mvgam_stability` object returned by
  [`stability()`](https://nicholasjclark.github.io/mvgam/dev/reference/stability.mvgam.md).

- variables:

  Character vector of the metrics to draw. Defaults to reactivity and
  the mean and variance return rates. Any column of `x` can be named.

- bins:

  Number of histogram bins.

- ...:

  Unused. Anything passed here is refused.

## Value

A `ggplot` object.

## See also

[`stability()`](https://nicholasjclark.github.io/mvgam/dev/reference/stability.mvgam.md),
[`irf()`](https://nicholasjclark.github.io/mvgam/dev/reference/irf.mvgam.md),
[`fevd()`](https://nicholasjclark.github.io/mvgam/dev/reference/fevd.mvgam.md),
[`plot.mvgam_irf()`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_irf.md),
[`plot.mvgam_fevd()`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_fevd.md),
[`plot.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_forecast.md)
