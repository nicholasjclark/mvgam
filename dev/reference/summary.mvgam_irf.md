# Posterior summary of impulse responses

This function takes an `mvgam_irf` object and calculates a posterior
summary of the impulse responses of each series to shocks from each of
the other series, at all horizons

## Usage

``` r
# S3 method for class 'mvgam_irf'
summary(object, probs = c(0.025, 0.975), ...)
```

## Arguments

- object:

  an object of class `mvgam_irf` obtained using the
  [`irf()`](https://nicholasjclark.github.io/mvgam/dev/reference/irf.mvgam.md)
  function. This object will contain draws from the posterior
  distribution of the impulse responses.

- probs:

  The upper and lower percentiles to be computed by the `quantile`
  function, in addition to the median

- ...:

  Unused. Anything passed here is refused.

## Value

A long-format `tibble` / `data.frame` reporting the posterior median,
upper and lower percentiles of the impulse responses of each series to
shocks from each of the other series at all horizons.

## See also

[`irf`](https://nicholasjclark.github.io/mvgam/dev/reference/irf.mvgam.md),
[`plot.mvgam_irf`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_irf.md)

## Author

Nicholas J Clark
