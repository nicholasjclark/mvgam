# Extract number of observations from mvgam object

Extract number of observations from mvgam object

## Usage

``` r
# S3 method for class 'mvgam'
nobs(object, ...)
```

## Arguments

- object:

  mvgam object

- ...:

  Unused. Anything passed here is refused.

## Value

Integer count of the rows the model was given, matching
`nrow(model.frame(object))`. A frame mvgam fits is rectangular, so an
unobserved cell is part of the design and is counted.
