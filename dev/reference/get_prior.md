# The priors of a model

Methods:

- [`get_prior.mvgam_formula()`](https://nicholasjclark.github.io/mvgam/dev/reference/get_prior.mvgam_formula.md)
  lists the priors of a model before it is fitted.

- [`get_prior.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/get_prior.mvgam.md)
  returns the prior table of a model.

- [`get_prior.default()`](https://nicholasjclark.github.io/mvgam/dev/reference/get_prior.default.md)
  takes a `formula` or `brmsformula` with the arguments of
  [`get_prior.mvgam_formula()`](https://nicholasjclark.github.io/mvgam/dev/reference/get_prior.mvgam_formula.md).

`default_prior()` is the same generic under the newer brms name.

## Usage

``` r
get_prior(object, ...)

default_prior(object, ...)
```

## Arguments

- object:

  An
  [`mvgam_formula()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_formula.md),
  an `mvgam` model, or a model formula.

- ...:

  Arguments of the method.

## Value

A `brmsprior` data frame.

## See also

[`mvgam_formula()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_formula.md),
[`brms::get_prior()`](https://paulbuerkner.com/brms/reference/default_prior.html)
