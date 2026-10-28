# Plot the VAR transition matrices of several panels

`type = "heatmap"` draws one heatmap per panel, as
[`plot.mvgam_var_matrix()`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_var_matrix.md)
draws one, with fill limits shared across panels. `type = "diagonal"`
draws the persistence of each process in each panel as a point with its
interval.

## Usage

``` r
# S3 method for class 'mvgam_var_matrix_list'
plot(x, type = c("heatmap", "diagonal"), ncol = NULL, ...)
```

## Arguments

- x:

  An `mvgam_var_matrix_list` returned by
  [`posterior_transition_matrix()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_transition_matrix.md)
  with `groups` set.

- type:

  `"heatmap"` (default) or `"diagonal"`.

- ncol:

  The number of facet columns of the heatmaps, or `NULL` for
  `ceiling(sqrt(length(x)))`.

- ...:

  Unused. Anything passed here is refused.

## Value

A [ggplot2::ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## See also

[`posterior_transition_matrix()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_transition_matrix.md),
[`plot.mvgam_var_matrix()`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_var_matrix.md),
[`plot.mvgam_residcor_list()`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_residcor_list.md).

## Author

Nicholas J Clark
