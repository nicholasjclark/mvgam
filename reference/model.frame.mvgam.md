# Extract a model frame from a fitted mvgam object

Returns the model's training data restricted to the response plus the
variables `find_predictors()` reports. Used by upstream consumers
(`insight`,
[`marginaleffects::datagrid`](https://rdrr.io/pkg/marginaleffects/man/datagrid.html))
when they walk the model frame to build prediction grids.

## Usage

``` r
# S3 method for class 'mvgam'
model.frame(formula, trend_effects = FALSE, ...)
```

## Arguments

- formula:

  A fitted `mvgam` object.

- trend_effects:

  Logical; if `TRUE` and the model has a trend formula, return the trend
  submodel's covariates only.

- ...:

  Unused. Anything passed here is refused.

## Value

A data frame.
