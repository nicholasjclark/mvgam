# Print headline summary of an `mvgam_lfo` object

Prints the horizon, the Pareto k threshold and the counts of evaluations
and refits. Each score is given as its sum and mean over folds.
[`summary.mvgam_lfo()`](https://nicholasjclark.github.io/mvgam/dev/reference/summary.mvgam_lfo.md)
returns the per-fold table.

## Usage

``` r
# S3 method for class 'mvgam_lfo'
print(x, ...)
```

## Arguments

- x:

  An object of class `mvgam_lfo`.

- ...:

  Unused.

## Value

The `mvgam_lfo` object `x`, returned invisibly.
