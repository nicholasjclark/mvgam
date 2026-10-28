# Summary method for sim_jsdgam() output

Reports the simulated design, the per-species generative parameters and
the range of the residual correlations the loadings imply, which is what
a fit of these data is asked to recover.

## Usage

``` r
# S3 method for class 'mvgam_sim_jsdgam'
summary(object, ...)
```

## Arguments

- object:

  A `mvgam_sim_jsdgam` list returned by
  [`sim_jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/sim_jsdgam.md).

- ...:

  Unused. Anything passed here is refused.

## Value

An object of class `mvgam_sim_jsdgam_summary`, with its own
[print()](https://nicholasjclark.github.io/mvgam/dev/reference/print.mvgam_sim_jsdgam_summary.md)
method.
