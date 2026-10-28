# Summarise posterior stability metrics

Reports the posterior median and interval of each stability metric.
[`stability()`](https://nicholasjclark.github.io/mvgam/dev/reference/stability.mvgam.md)
returns this summary by default, as the other post-fit summaries of a
VAR do. `stability(summary = FALSE)` returns the draws.

## Usage

``` r
# S3 method for class 'mvgam_stability'
summary(object, probs = c(0.025, 0.975), robust = TRUE, bins = 30L, ...)
```

## Arguments

- object:

  An object of class `mvgam_stability`

- probs:

  The lower and upper percentiles of the interval reported with the
  median

- robust:

  Logical. `TRUE` (the default) reports the median and median absolute
  deviation. `FALSE` reports the mean and standard deviation.

- bins:

  Number of bins that record the shape of each metric for
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html)

- ...:

  Unused. Anything passed here is refused.

## Value

A `data.frame` with one row per metric. On a hierarchical VAR it holds
one row per panel and metric, with the panel in `group`.
