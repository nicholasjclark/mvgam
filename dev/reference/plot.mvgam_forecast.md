# Plot posterior hindcasts and forecasts

Plot method for `mvgam_forecast` objects returned by
[`hindcast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.mvgam.md)
and
[`forecast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/forecast.mvgam.md).
Draws one panel per series (faceted by default) with nested quantile
ribbons for the forecast, a single grey ribbon for the hindcast,
posterior median lines and the observations as points. A dashed vertical
line marks the boundary between training and forecast times when both
are drawn.

## Usage

``` r
# S3 method for class 'mvgam_forecast'
plot(
  x,
  series = NULL,
  probs = c(0.5, 0.8, 0.95),
  hindcast = TRUE,
  forecast = TRUE,
  newdata_obs = TRUE,
  ...
)
```

## Arguments

- x:

  An `mvgam_forecast` object.

- series:

  Optional integer vector, character vector of series names, or `"all"`.
  When `NULL` (the default), every series is plotted and the panels are
  faceted via
  [`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html).

- probs:

  Numeric vector of credible interval widths for the forecast ribbon.
  Defaults to `c(0.5, 0.8, 0.95)` which draws three nested bands from
  the outer 95% to the inner 50%. Hindcast ribbons always use the widest
  entry.

- hindcast, forecast:

  Logical. Whether the hindcast and the forecast are drawn. Both default
  to `TRUE`. Setting one to `FALSE` draws the other alone.

- newdata_obs:

  Logical. When `TRUE` (the default) and the object carries
  `test_observations`, the test observations are overlaid on the
  forecast region.

- ...:

  Unused. Anything passed here is refused.

## Value

A `ggplot` object.

## Details

The forecast takes the shades of the red bayesplot scheme. The hindcast
band is light grey, which separates in-sample uncertainty from the
nested forecast bands.

For state-extrapolating prediction over arbitrary newdata see
[`forecast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/forecast.mvgam.md)
and
[`hindcast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.mvgam.md);
for marginal-MC prediction see
[`posterior_predict.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_predict.mvgam.md)
/
[`posterior_epred.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_epred.mvgam.md).

## See also

[`forecast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/forecast.mvgam.md),
[`hindcast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.mvgam.md),
[`score.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/score.mvgam_forecast.md),
[`summary.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/summary.mvgam_forecast.md),
[`irf()`](https://nicholasjclark.github.io/mvgam/dev/reference/irf.mvgam.md),
[`fevd()`](https://nicholasjclark.github.io/mvgam/dev/reference/fevd.mvgam.md),
[`stability()`](https://nicholasjclark.github.io/mvgam/dev/reference/stability.mvgam.md)

## Author

Nicholas J Clark
