# Print the VAR transition matrices of several panels

Prints the number of panels, the dimension and the mean absolute
off-diagonal cell of each panel. The panel with the largest mean has the
strongest effects between processes.

## Usage

``` r
# S3 method for class 'mvgam_var_matrix_list'
print(x, ...)
```

## Arguments

- x:

  An `mvgam_var_matrix_list` returned by
  [`posterior_transition_matrix()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_transition_matrix.md)
  with `groups` set.

- ...:

  Ignored.

## Value

`x`, invisibly.

## See also

[`posterior_transition_matrix()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_transition_matrix.md),
[`plot.mvgam_var_matrix_list()`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_var_matrix_list.md),
[`print.mvgam_var_matrix()`](https://nicholasjclark.github.io/mvgam/dev/reference/print.mvgam_var_matrix.md).

## Author

Nicholas J Clark
