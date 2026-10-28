# Plot method for an `mvgam_residcor_list`

Faceted heatmap panel across the per-group correlation matrices returned
by `residual_cor(mod, by_group = TRUE)` on a hierarchical trend fit.
Each panel is drawn as by
[`plot.mvgam_residcor()`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_residcor.md),
one per group, with fill showing the posterior point estimate and an
outline on each pair whose interval excludes zero. The facets share
colour limits. Panels are then directly comparable. The `_global`
reference matrix is included by default. Drop it with
`include_global = FALSE`.

## Usage

``` r
# S3 method for class 'mvgam_residcor_list'
plot(x, include_global = TRUE, ncol = NULL, rescale = FALSE, ...)
```

## Arguments

- x:

  An `mvgam_residcor_list` returned by
  `residual_cor(mod, by_group = TRUE)`.

- include_global:

  Logical. If `TRUE` (default), include the `_global` reference
  correlation as one panel.

- ncol:

  Optional integer. Number of facet columns; defaults to
  `ceiling(sqrt(n_panels))`.

- rescale:

  Logical. By default colour spans `[-1, 1]`, the full range of a
  correlation. Panels from different fits are then comparable. A
  correlation of 0.4 looks unlike one of 1. Set `TRUE` to scale colour
  to the entries present, for a fit whose correlations are all small.
  Defaults to `FALSE`.

- ...:

  Unused. Anything passed here is refused.

## Value

A [ggplot2::ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## See also

[`residual_cor()`](https://nicholasjclark.github.io/mvgam/dev/reference/residual_cor.md),
[`plot.mvgam_residcor()`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_residcor.md),
[`plot.mvgam_var_matrix_list()`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_var_matrix_list.md).

## Author

Nicholas J Clark
