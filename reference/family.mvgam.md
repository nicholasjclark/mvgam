# Extract family from mvgam object

Extract family from mvgam object

## Usage

``` r
# S3 method for class 'mvgam'
family(object, resp = NULL, ...)
```

## Arguments

- object:

  mvgam object

- resp:

  Optional name of one response of a multivariate model. Without it, a
  model with several responses returns one family per response.

- ...:

  Unused. Anything passed here is refused.

## Value

A family object, or a list of them named by response
