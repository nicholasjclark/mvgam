# Tidy summary of an `mvgam_residcor` object

Returns a tibble with one row per unique upper-triangle pair of series,
sorted by `prob_nonzero` descending (so the most credibly non-zero pairs
appear first).

## Usage

``` r
# S3 method for class 'mvgam_residcor'
summary(object, ...)
```

## Arguments

- object:

  An `mvgam_residcor` object returned by
  [`residual_cor()`](https://nicholasjclark.github.io/mvgam/dev/reference/residual_cor.md)
  with `summary = TRUE`.

- ...:

  Unused. Anything passed here is refused.

## Value

A [`tibble::tibble`](https://tibble.tidyverse.org/reference/tibble.html)
with columns `series_1`, `series_2`, `Estimate`, `Est.Error`, `Q_lower`,
`Q_upper`, `ESS`, `prob_positive`, `prob_negative`, `prob_nonzero`,
`sig` (whether `prob_nonzero > prob_threshold`). `ESS` is the per-entry
effective sample size of the Fisher-z correlation draws. A low value
marks a pair the chains sampled poorly, however tight its interval.
