# Plot a summarised impulse response or variance decomposition

An impulse response draws the posterior median and interval of each
shock-response pair against the forecast horizon, with a dashed line at
zero. A variance decomposition stacks the median shares of its sources
in one panel per target, as
[`plot.mvgam_fevd()`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_fevd.md)
does.

## Usage

``` r
# S3 method for class 'mvgam_var_surface_summary'
plot(
  x,
  series = NULL,
  responses = NULL,
  contributing = NULL,
  shocks = NULL,
  ...
)
```

## Arguments

- x:

  A `mvgam_irf_summary` or `mvgam_fevd_summary`

- series:

  Positions or names of the processes to keep. On an impulse response
  these are the processes a shock originates in. On a variance
  decomposition they are the targets whose forecast error is decomposed.
  Default keeps every process.

- responses:

  Impulse responses only. Positions or names of the processes whose
  response to the shock is drawn. Default keeps every process.

- contributing:

  Variance decompositions only. Positions or names of the source
  processes stacked in each bar. The shares of the sources drawn are
  renormalised to sum to one.

- shocks:

  Optional character vector naming shock-response pairs directly, for a
  selection the index arguments cannot express.

- ...:

  Unused. Anything passed here is refused.

## Value

A `ggplot` object
