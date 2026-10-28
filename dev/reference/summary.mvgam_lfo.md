# Posterior summary of an `mvgam_lfo` object

One row per evaluation time. `refit_here` is `TRUE` where the model was
refitted for that origin. `pareto_k` is the Pareto k of the importance
weights, `NA` at a refit. `n_obs` counts the responses scored. One
column follows for each score requested from
[`lfo_cv()`](https://nicholasjclark.github.io/mvgam/dev/reference/lfo_cv.mvgam.md).

## Usage

``` r
# S3 method for class 'mvgam_lfo'
summary(object, ...)
```

## Arguments

- object:

  An object of class `mvgam_lfo`.

- ...:

  Unused. Anything passed here is refused.

## Value

A `tibble` with columns `eval_time`, `refit_here`, `pareto_k`, `n_obs`
and one per score.
