# Posterior summary of forecast error variance decompositions

This function takes an `mvgam_fevd` object and calculates a posterior
summary of the error variance decompositions of each series, at all
horizons

## Usage

``` r
# S3 method for class 'mvgam_fevd'
summary(object, probs = c(0.025, 0.975), ...)
```

## Arguments

- object:

  an object of class `mvgam_fevd` obtained using the
  [`fevd()`](https://nicholasjclark.github.io/mvgam/dev/reference/fevd.mvgam.md)
  function. This object will contain draws from the posterior
  distribution of the forecast error variance decompositions.

- probs:

  The upper and lower percentiles to be computed by the `quantile`
  function, in addition to the median

- ...:

  Unused. Anything passed here is refused.

## Value

A long-format `tibble` / `data.frame` reporting the posterior median,
upper and lower percentiles of the error variance decompositions of each
series at all horizons.

## See also

[`fevd`](https://nicholasjclark.github.io/mvgam/dev/reference/fevd.mvgam.md),
[`plot.mvgam_fevd`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_fevd.md)

## Author

Nicholas J Clark
