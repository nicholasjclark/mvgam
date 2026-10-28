# Number of levels per grouping factor in a fitted mvgam model

The number of levels of each grouping factor of the observation formula,
as
[`brms::ngrps.brmsfit()`](https://paulbuerkner.com/brms/reference/ngrps.brmsfit.html)
gives it.

## Usage

``` r
# S3 method for class 'mvgam'
ngrps(object, ...)
```

## Arguments

- object:

  A fitted `mvgam` object.

- ...:

  Unused. Anything passed here is refused.

## Value

A named list of integers, one per grouping factor, or `NULL` for a model
without group-level effects.

## See also

[`brms::ngrps.brmsfit()`](https://paulbuerkner.com/brms/reference/ngrps.brmsfit.html),
[`ranef.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/ranef.mvgam.md),
[`VarCorr.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/VarCorr.mvgam.md).

## Author

Nicholas J Clark
