# Plot residual correlations or precisions from a `mvgam_residcor` object

Heatmap of the residual correlations or partial correlations from a
Joint Species Distribution (`jsdgam`) or dynamic factor (`mvgam`) model.

## Usage

``` r
# S3 method for class 'mvgam_residcor'
plot(
  x,
  type = c("correlation", "precision"),
  cluster = FALSE,
  rescale = FALSE,
  ...
)
```

## Arguments

- x:

  A `mvgam_residcor` object returned by
  `residual_cor(..., summary = TRUE)`.

- type:

  Character. Which matrix to plot. `"correlation"` (the default) draws
  `x$cor`. `"precision"` draws `x$prec`, the partial correlations
  (Ovaskainen et al. 2016).
  [`residual_cor()`](https://nicholasjclark.github.io/mvgam/dev/reference/residual_cor.md)
  computes them under `partial = TRUE`.

- cluster:

  Logical. When `TRUE`, the matrix is reordered by an approximate
  Robinson ordering (Gruvaeus & Wainer 1972; average linkage on the
  `1 - cormat` distance). Clusters of positive and negative correlations
  then lie adjacent. Defaults to `FALSE`.

- rescale:

  Logical. By default colour spans `[-1, 1]`, the full range of a
  correlation. Panels from different fits are then comparable. A
  correlation of 0.4 looks unlike one of 1. Set `TRUE` to scale colour
  to the entries present, for a fit whose correlations are all small.
  Defaults to `FALSE`.

- ...:

  Unused. Anything passed here is refused.

## Value

A `ggplot` object.

## Details

Only the lower triangle of off-diagonal entries is drawn; a symmetric
matrix says everything once and the diagonal is `1` for any correlation.
Fill shows the posterior point estimate. A black outline marks each pair
whose credible interval excludes zero.
[`summary.mvgam_residcor()`](https://nicholasjclark.github.io/mvgam/dev/reference/summary.mvgam_residcor.md)
reports each pair's interval and `prob_nonzero`.

## See also

[`residual_cor()`](https://nicholasjclark.github.io/mvgam/dev/reference/residual_cor.md),
[`summary.mvgam_residcor()`](https://nicholasjclark.github.io/mvgam/dev/reference/summary.mvgam_residcor.md)

## Author

Nicholas J Clark
