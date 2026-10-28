# Sampler control settings of a fitted mvgam model

The NUTS settings the model was sampled with, such as `adapt_delta`,
`max_treedepth` and `stepsize`.

## Usage

``` r
# S3 method for class 'mvgam'
control_params(x, ...)
```

## Arguments

- x:

  A fitted `mvgam` model.

- ...:

  Unused. Anything passed here is refused.

## Value

A named list, as
[`brms::control_params()`](https://paulbuerkner.com/brms/reference/control_params.html)
returns for a `brmsfit`. A fit with no NUTS settings, such as a
variational fit, gives an empty list.
