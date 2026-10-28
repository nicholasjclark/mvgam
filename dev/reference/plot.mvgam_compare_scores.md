# Plot per-model score trajectories from `compare_scores()`

Draws a `ggplot2` line + point chart of forecast scores from a
`mvgam_compare_scores` object. The shape adapts to the score type:

- Univariate scores (`"crps"`, `"drps"`, `"logs"`, `"brier"`, `"sis"`,
  `"dss"`, `"qs"`, `"twcrps"`) plot per-series with one facet panel per
  series; toggle off with `facet = FALSE` to overlay every series on one
  panel.

- Joint multivariate scores (`"energy"`, `"variogram"`, `"twenergy"`)
  plot a single panel from the `all_series` rows.

An ensemble passed to
[`compare_scores()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_scores.md)
is plotted as one more model and compared with its components.

## Usage

``` r
# S3 method for class 'mvgam_compare_scores'
plot(x, relative = NULL, facet = TRUE, ...)
```

## Arguments

- x:

  A `mvgam_compare_scores` object from
  [`compare_scores()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_scores.md).

- relative:

  Optional character name of a baseline model already in `x$model`. When
  supplied, scores are differenced against that model at each series x
  horizon cell and the plot adds a dashed zero line; negative values
  then mean "better than the baseline". The baseline model itself is
  dropped from the plot since its values are identically zero.

- facet:

  Logical. When `TRUE` (default) univariate scores are faceted by
  series; when `FALSE` series are overlaid on one panel with linetype
  distinguishing them. Ignored for joint scores.

- ...:

  Unused. Anything passed here is refused.

## Value

A `ggplot` object.

## See also

[`compare_scores()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_scores.md),
[`score.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/score.mvgam_forecast.md),
[`plot.mvgam_compare_elpds()`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_compare_elpds.md),
[`ensemble.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/ensemble.mvgam_forecast.md)

## Author

Nicholas J Clark
