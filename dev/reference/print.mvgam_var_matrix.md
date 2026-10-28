# Print a VAR transition matrix

Prints the panel, the dimension, the mean absolute off-diagonal cell and
the posterior estimate of the matrix. The interval of each cell is in
`A_lower` and `A_upper`.

## Usage

``` r
# S3 method for class 'mvgam_var_matrix'
print(x, digits = 2L, ...)
```

## Arguments

- x:

  An `mvgam_var_matrix` object returned by
  [`posterior_transition_matrix()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_transition_matrix.md).

- digits:

  Integer. Digits used when rounding the printed matrix.

- ...:

  Ignored.

## Value

`x`, invisibly.

## See also

[`posterior_transition_matrix()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_transition_matrix.md),
[`plot.mvgam_var_matrix()`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_var_matrix.md),
[`print.mvgam_var_matrix_list()`](https://nicholasjclark.github.io/mvgam/dev/reference/print.mvgam_var_matrix_list.md).

## Author

Nicholas J Clark
