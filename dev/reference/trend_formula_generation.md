# Stan program and data from a formula carrying a `trend_formula`

[`stancode()`](https://paulbuerkner.com/brms/reference/stancode.html)
and
[`standata()`](https://paulbuerkner.com/brms/reference/standata.html)
delegate a plain formula to brms. A `trend_formula` in the same call, or
a family mvgam builds such as
[`tweedie()`](https://nicholasjclark.github.io/mvgam/dev/reference/tweedie.md)
or
[`nmix()`](https://nicholasjclark.github.io/mvgam/dev/reference/nmix.md),
names an mvgam model. These methods then build the
[`mvgam_formula`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_formula.md)
that call describes and generate its program.

## Usage

``` r
# S3 method for class 'formula'
stancode(object, ...)

# S3 method for class 'bform'
stancode(object, ...)

# S3 method for class 'formula'
standata(object, ...)

# S3 method for class 'bform'
standata(object, ...)
```

## Arguments

- object:

  A `formula`, or a `brmsformula` or `mvbrmsformula` (both of class
  `bform`).

- ...:

  Arguments for
  [`stancode.mvgam_formula`](https://nicholasjclark.github.io/mvgam/dev/reference/stancode.mvgam_formula.md)
  or for brms, `data` and `family` among them.

## Value

A character string of Stan code, or a list of Stan data.

## See also

[`mvgam_formula`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_formula.md),
[`stancode.mvgam_formula`](https://nicholasjclark.github.io/mvgam/dev/reference/stancode.mvgam_formula.md),
[`standata.mvgam_formula`](https://nicholasjclark.github.io/mvgam/dev/reference/stancode.mvgam_formula.md)
