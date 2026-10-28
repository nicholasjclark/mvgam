# The priors of a model formula

Covers a `formula`, `brmsformula` or `mvbrmsformula`, which it wraps in
the
[`mvgam_formula`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_formula.md)
it describes with any `trend_formula` in the call. The table then
carries the defaults mvgam gives its own families, with or without a
trend.

## Usage

``` r
# Default S3 method
get_prior(object, ...)

# Default S3 method
default_prior(object, ...)
```

## Arguments

- object:

  Model specification object

- ...:

  Arguments of
  [`get_prior.mvgam_formula()`](https://nicholasjclark.github.io/mvgam/dev/reference/get_prior.mvgam_formula.md),
  such as `data`, `family` and `trend_formula`

## Value

A `brmsprior` data frame
