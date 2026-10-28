# Print an `mvgam_kfold` object

Prints the ELPD with its standard error, the folds and refits and the
Pareto k of the PSIS estimates. The first list below names the groups
with the lowest ELPD. The second names the refitted groups whose ELPD
moved most from the PSIS estimate.

## Usage

``` r
# S3 method for class 'mvgam_kfold'
print(x, digits = 2L, ...)
```

## Arguments

- x:

  An `mvgam_kfold` object.

- digits:

  Integer number of decimal places. Defaults to 2.

- ...:

  Unused.

## Value

`x`, invisibly.
