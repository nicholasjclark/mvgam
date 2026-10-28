# Plot Pareto-k and per-fold scores from an `mvgam_lfo` object

One panel for the Pareto k of each fold and one for each score. The
dashed line in each panel is the threshold above which points are
highlighted. For Pareto k it is the threshold
[`lfo_cv()`](https://nicholasjclark.github.io/mvgam/dev/reference/lfo_cv.mvgam.md)
used. For ELPD it is the 15% quantile, with lower values highlighted.
For the other scores it is the 85% quantile.

## Usage

``` r
# S3 method for class 'mvgam_lfo'
plot(x, ...)
```

## Arguments

- x:

  An object of class `mvgam_lfo`.

- ...:

  Unused. Anything passed here is refused.

## Value

A `ggplot` object.
