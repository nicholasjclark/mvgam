# Plot per-step LFO ELPDs from `compare_elpds()`

Draws a `ggplot2` line + point chart of leave-future-out ELPDs over the
rolling-origin evaluation grid. Each model gets a coloured line. Refit
timepoints (where Pareto-k crossed the threshold and the model was
refit) are highlighted with a hollow marker so the reader can see where
mvgam stopped trusting the PSIS approximation.

## Usage

``` r
# S3 method for class 'mvgam_compare_elpds'
plot(x, relative = NULL, cumulative = FALSE, ...)
```

## Arguments

- x:

  A `mvgam_compare_elpds` object from
  [`compare_elpds()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_elpds.md).

- relative:

  Optional character name of a baseline model already in `x$model`. When
  supplied, per-step ELPDs are differenced against that model at each
  evaluation timepoint and the plot adds a dashed zero line; positive
  values then mean "better than the baseline". The baseline itself is
  dropped from the plot.

- cumulative:

  Logical. When `TRUE` (default `FALSE`) the y-axis becomes the
  cumulative ELPD up to each evaluation timepoint, which is the quantity
  [`loo_compare()`](https://mc-stan.org/loo/reference/loo_compare.html)
  sums across folds.

- ...:

  Unused. Anything passed here is refused.

## Value

A `ggplot` object.

## See also

[`compare_elpds()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_elpds.md),
[`lfo_cv()`](https://nicholasjclark.github.io/mvgam/dev/reference/lfo_cv.mvgam.md),
[`loo_compare.mvgam_lfo()`](https://nicholasjclark.github.io/mvgam/dev/reference/loo_compare.mvgam_lfo.md),
[`plot.mvgam_compare_scores()`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_compare_scores.md)

## Author

Nicholas J Clark
