# Plot summarised stability metrics

Draws the histogram of each metric from the bins
[`summary()`](https://rdrr.io/r/base/summary.html) recorded. A histogram
shows whether the mass of reactivity crosses zero. Pass
`intervals = TRUE` for a median and interval instead. A hierarchical VAR
draws a median and interval for each panel.

## Usage

``` r
# S3 method for class 'mvgam_stability_summary'
plot(
  x,
  variables = c("reactivity", "mean_return_rate", "var_return_rate"),
  intervals = FALSE,
  ...
)
```

## Arguments

- x:

  An object of class `mvgam_stability_summary`

- variables:

  Metrics to draw

- intervals:

  Logical. `TRUE` draws each metric as a median and interval. `FALSE`
  (the default) draws its binned posterior.

- ...:

  Unused. Anything passed here is refused.

## Value

A `ggplot` object
