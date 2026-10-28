# Summary method for sim_closure_unit_data() output

The dimensions, the recipe, the ranges of the true state and detection
parameters and the share of non-zero observations. A share of 0 or 1
marks data with no information on detection.

## Usage

``` r
# S3 method for class 'mvgam_sim_closure_unit'
summary(object, ...)
```

## Arguments

- object:

  A `mvgam_sim_closure_unit` list returned by
  [`sim_closure_unit_data()`](https://nicholasjclark.github.io/mvgam/dev/reference/sim_closure_unit_data.md).

- ...:

  Unused. Anything passed here is refused.

## Value

An object of class `mvgam_sim_closure_unit_summary`: a list of per-unit
detection and abundance summaries with its own
[print()](https://nicholasjclark.github.io/mvgam/dev/reference/print.mvgam_sim_closure_unit_summary.md)
method.
