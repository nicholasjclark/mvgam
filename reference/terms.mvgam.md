# Model terms for a fitted mvgam object

[`terms()`](https://rdrr.io/r/stats/terms.html) reports a model's
structure to a caller that does not know its class.
[`model.frame()`](https://rdrr.io/r/stats/model.frame.html) is the
accessor usually paired with it. The object holds the model's responses
on the left and the covariates of both submodels on the right.
[`stats::terms()`](https://rdrr.io/r/stats/terms.html) has no meaning
for the bar syntax of group-level terms.
[`insight::find_random()`](https://easystats.github.io/insight/reference/find_random.html)
returns those.

## Usage

``` r
# S3 method for class 'mvgam'
terms(x, ...)
```

## Arguments

- x:

  A fitted `mvgam` object.

- ...:

  Unused. Anything passed here is refused.

## Value

An object of class `terms`.
