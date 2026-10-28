# Plot the factor-implied shared variation matrix

Heatmap of the posterior point estimate of \\\Delta = Z \Sigma Z^\top\\.
Diagonal cells show the variance the factors capture in each series.
Off-diagonal cells show the covariance between series, positive or
negative. The fill scale is symmetric around zero. A positive and a
negative covariance of equal size take colours of equal intensity.

## Usage

``` r
# S3 method for class 'mvgam_shared_variation'
plot(x, type = c("point", "uncertainty"), ...)
```

## Arguments

- x:

  A `mvgam_shared_variation` object returned by
  [`shared_variation()`](https://nicholasjclark.github.io/mvgam/dev/reference/shared_variation.md).

- type:

  One of `"point"` (default) showing the posterior point estimate, or
  `"uncertainty"` showing the per-entry posterior spread (`delta_se`).

- ...:

  Unused. Anything passed here is refused.

## Value

A `ggplot` object.

## See also

[`shared_variation()`](https://nicholasjclark.github.io/mvgam/dev/reference/shared_variation.md),
[`plot.mvgam_residcor()`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_residcor.md)
for the implied correlation structure as a residual-correlation heatmap.

## Author

Nicholas J Clark
