# Diagnostic plot for an `mvgam_kfold` object

One panel for the ELPD of each group and one for its Pareto k, as in
[`plot.mvgam_lfo()`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_lfo.md).
Dark red points mark a Pareto k above the dashed threshold, where PSIS
is unreliable. In the ELPD panel they mark the groups below the 15%
quantile.

## Usage

``` r
# S3 method for class 'mvgam_kfold'
plot(x, ...)
```

## Arguments

- x:

  An `mvgam_kfold` object.

- ...:

  Unused. Anything passed here is refused.

## Value

A `ggplot` object.

## See also

[`plot.mvgam_lfo()`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_lfo.md)
for the matching time-block CV plot.
