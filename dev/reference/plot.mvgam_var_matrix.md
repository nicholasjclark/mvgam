# Plot a VAR transition matrix

Draws the posterior estimate of the matrix as a heatmap. Row \\i\\,
column \\j\\ is the effect of process \\j\\ at time \\t - l\\ on process
\\i\\ at time \\t\\. A black outline marks each cell whose interval
excludes zero. The colour scale is symmetric about zero and spans the
largest absolute cell.

## Usage

``` r
# S3 method for class 'mvgam_var_matrix'
plot(x, cluster = FALSE, ...)
```

## Arguments

- x:

  An `mvgam_var_matrix` object returned by
  [`posterior_transition_matrix()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_transition_matrix.md).

- cluster:

  Logical. `TRUE` orders the processes by a Robinson clustering of
  `1 - A`, which suits a matrix close to symmetric.

- ...:

  Unused. Anything passed here is refused.

## Value

A [ggplot2::ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## See also

[`posterior_transition_matrix()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_transition_matrix.md),
[`print.mvgam_var_matrix()`](https://nicholasjclark.github.io/mvgam/dev/reference/print.mvgam_var_matrix.md),
[`plot.mvgam_var_matrix_list()`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_var_matrix_list.md),
[`plot.mvgam_residcor()`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_residcor.md).

## Author

Nicholas J Clark
