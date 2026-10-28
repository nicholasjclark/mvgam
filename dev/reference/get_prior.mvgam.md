# The prior table of an mvgam model

[`get_prior()`](https://nicholasjclark.github.io/mvgam/dev/reference/get_prior.md),
[`default_prior()`](https://nicholasjclark.github.io/mvgam/dev/reference/get_prior.md)
and
[`prior_summary()`](https://mc-stan.org/rstantools/reference/prior_summary.html)
return the one prior table a model was built with. Edit it and pass it
to `update(fit, prior = ...)` to refit under other priors without
recompiling.

## Usage

``` r
# S3 method for class 'mvgam'
get_prior(object, ...)

# S3 method for class 'mvgam'
default_prior(object, ...)
```

## Arguments

- object:

  An `mvgam` model, fitted or built with `run_model = FALSE`.

- ...:

  Unused. Anything passed here is refused.

## Value

A `brmsprior` data frame of the observation and trend priors. A row the
user set has `source = "user"`. brms sets the source of the others,
mostly `"default"`.

## See also

Other priors:
[`get_prior.mvgam_formula()`](https://nicholasjclark.github.io/mvgam/dev/reference/get_prior.mvgam_formula.md)
