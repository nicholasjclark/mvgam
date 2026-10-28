# Wide-format summary of a `compare_scores()` table

Pivots the long-format `mvgam_compare_scores` tibble to one row per
evaluation cell (per series x horizon for univariate scores, per horizon
for joint scores) and one column per model.

## Usage

``` r
# S3 method for class 'mvgam_compare_scores'
summary(object, ...)
```

## Arguments

- object:

  A `mvgam_compare_scores` object from
  [`compare_scores()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_scores.md).

- ...:

  Unused. Anything passed here is refused.

## Value

A `tibble` with one row per series x horizon cell (univariate scores) or
per horizon (joint scores), one column of scores per model.

## See also

[`compare_scores()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_scores.md),
[`plot.mvgam_compare_scores()`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_compare_scores.md)

## Author

Nicholas J Clark
