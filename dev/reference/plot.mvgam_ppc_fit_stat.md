# Plot a closure-unit GOF discrepancy result

Scatter of `T(y_rep, theta)` against `T(y_obs, theta)` per posterior
draw, with the 1:1 reference line and the Bayesian p-value annotated in
the subtitle.

## Usage

``` r
# S3 method for class 'mvgam_ppc_fit_stat'
plot(x, ...)
```

## Arguments

- x:

  A `mvgam_ppc_fit_stat` object.

- ...:

  Unused. Anything passed here is refused.

## Value

A `ggplot` object.

## Author

Nicholas J Clark
