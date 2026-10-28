# Plot the posterior active-factor probabilities

Bar plot of the posterior probability that each dimension of the factor
covariance is active, largest first, under the rule
[`active_factors()`](https://nicholasjclark.github.io/mvgam/dev/reference/active_factors.md)
applied. A dashed line marks `prob`. The subtitle reports the posterior
median count with its 95% credible interval.

## Usage

``` r
# S3 method for class 'mvgam_active_factors'
plot(x, ...)
```

## Arguments

- x:

  An `mvgam_active_factors` object returned by
  [`active_factors()`](https://nicholasjclark.github.io/mvgam/dev/reference/active_factors.md).

- ...:

  Unused. Anything passed here is refused.

## Value

A `ggplot` object.

## See also

[`active_factors()`](https://nicholasjclark.github.io/mvgam/dev/reference/active_factors.md)

## Author

Nicholas J Clark
