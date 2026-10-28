# Log-likelihood of a fitted mvgam model

The posterior mean of the summed log likelihood, with the `df` and
`nobs` attributes that
[`stats::AIC()`](https://rdrr.io/r/stats/AIC.html) and
[`stats::BIC()`](https://rdrr.io/r/stats/AIC.html) use.

## Usage

``` r
# S3 method for class 'mvgam'
logLik(object, pointwise = FALSE, ...)
```

## Arguments

- object:

  A fitted `mvgam` object.

- pointwise:

  Logical. `TRUE` returns the `[ndraws, nobs]` matrix of
  [`log_lik.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/log_lik.mvgam.md).
  Defaults to `FALSE`.

- ...:

  Further arguments to
  [`log_lik.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/log_lik.mvgam.md),
  such as `newdata` or `ndraws`.

## Value

A `logLik` object. `df` counts the parameters
[`variables()`](https://nicholasjclark.github.io/mvgam/dev/reference/index-mvgam.md)
lists. With `pointwise = TRUE`, the matrix of
[`log_lik.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/log_lik.mvgam.md).

## Details

Each latent state counts as a parameter, which makes AIC and BIC coarse
for a state-space model.
[`loo.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/loo.mvgam.md)
estimates the ELPD with diagnostics for its reliability.

## See also

[`log_lik.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/log_lik.mvgam.md)
returns the pointwise matrix.
