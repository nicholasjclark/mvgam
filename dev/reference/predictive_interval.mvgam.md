# Posterior predictive intervals on a fitted mvgam model

Central quantile intervals of the draws
[`posterior_predict.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_predict.mvgam.md)
returns, one per observation.

## Usage

``` r
# S3 method for class 'mvgam'
predictive_interval(object, prob = 0.9, ...)
```

## Arguments

- object:

  A fitted `mvgam` object.

- prob:

  Numeric in `(0, 1)`. The probability mass each interval holds.
  Defaults to `0.9`.

- ...:

  Arguments of
  [`posterior_predict.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_predict.mvgam.md),
  such as `newdata` or `incl_autocor`.

## Value

A numeric matrix with one row per observation and two columns, the lower
and upper bounds.

## See also

[`posterior_interval.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_interval.mvgam.md)
gives intervals of parameters.
[`pp_check.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/pp_check.mvgam.md)
plots predictive draws against the data.

Other predictions:
[`conditional_effects.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/conditional_effects.mvgam.md),
[`conditional_smooths.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/conditional_smooths.mvgam.md),
[`fitted.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/fitted.mvgam.md),
[`hindcast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.mvgam.md),
[`posterior_epred.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_epred.mvgam.md),
[`posterior_linpred.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_linpred.mvgam.md),
[`posterior_predict.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_predict.mvgam.md),
[`posterior_smooths.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_smooths.mvgam.md),
[`predict.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/predict.mvgam.md),
[`predictive_error.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/predictive_error.mvgam.md)

## Author

Nicholas J Clark

## Examples

``` r
# \dontrun{
set.seed(13)
simdat <- sim_mvgam(family = poisson(), n_series = 1L,
                    n_timepoints = 120L, trend_model = AR())
mod <- mvgam(y ~ s(x), trend_formula = ~ AR(p = 1),
             data = simdat$data_train, family = poisson(),
             chains = 2, silent = 2)
#> Warning: 9 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.

# 90 percent intervals, one row per observation
intervals <- predictive_interval(mod)
head(intervals)
#>      5%   95%
#> [1,]  0  5.00
#> [2,]  7 23.00
#> [3,]  3 13.05
#> [4,]  4 14.00
#> [5,]  1  9.00
#> [6,]  2 14.00

# The share of observed counts the intervals cover
y <- simdat$data_train$y
mean(y >= intervals[, 1] & y <= intervals[, 2])
#> [1] 0.8

# Next: pp_check() plots the predictive draws against the data
# }
```
