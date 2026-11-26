# `mvgam_forecast` object description

A `mvgam_forecast` object returned by function
[`hindcast`](https://nicholasjclark.github.io/mvgam/reference/hindcast.mvgam.md)
or [`forecast`](https://generics.r-lib.org/reference/forecast.html). Run
`methods(class = "mvgam_forecast")` to see an overview of available
methods.

## Usage

``` r
# S3 method for class 'mvgam_forecast'
summary(object, probs = c(0.025, 0.975), ...)

# S3 method for class 'mvgam_forecast'
print(x, ...)
```

## Arguments

- object:

  An `mvgam_forecast` object.

- probs:

  The lower and upper quantiles reported with the median. Defaults to
  `c(0.025, 0.975)`.

- ...:

  Unused. Anything passed here is refused.

- x:

  An `mvgam_forecast` object.

## Details

A `mvgam_forecast` object contains the following elements:

- `family` `character` description of the observation distribution.

- `family_pars` `list` containing draws of family-specific parameters
  (e.g. shape, scale, overdispersion). Only returned when
  `type = "link"`; otherwise `NULL`.

- `type` The type of predictions included (`"link"`, `"response"`,
  `"expected"`, or `"trend"`).

- `series_names` Factor of series names, taken from
  `levels(data$series)` in the original model fit.

- `train_observations`, `train_times` Lists of length `n_series` holding
  the training responses and the time of each hindcast column. A
  closure-unit family's `"expected"` and `"response"` hindcasts have one
  column per visit. Their times repeat.

- `test_observations`, `test_times` If
  [`forecast()`](https://generics.r-lib.org/reference/forecast.html)
  produced the object, the corresponding test response vectors and time
  grids. `NULL` otherwise (e.g. from
  [`hindcast()`](https://nicholasjclark.github.io/mvgam/reference/hindcast.mvgam.md)).

- `hindcasts` Named list of posterior hindcast draw matrices, one
  `(ndraws x n_train)` matrix per series.

- `forecasts` Named list of posterior forecast draw matrices when
  produced by
  [`forecast()`](https://generics.r-lib.org/reference/forecast.html),
  otherwise `NULL`.

- `site_units` `TRUE` when the fit came from
  [`jsdgam()`](https://nicholasjclark.github.io/mvgam/reference/jsdgam.md).
  Its units are sites, which
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) refuses to
  draw along a time axis.

- `component` The state components of the trend the draws hold, such as
  `"slope"`, or `NULL` for the summed trend. Set through the `component`
  argument of
  [`hindcast.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/hindcast.mvgam.md)
  and
  [`forecast.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/forecast.mvgam.md).

## Methods

[`print()`](https://rdrr.io/r/base/print.html) gives the family, the
number of series, the sizes of the training and forecast windows and the
number of draws. [`summary()`](https://rdrr.io/r/base/summary.html)
returns a long tibble with one row per series and time: the median and
the two quantiles of `probs`, the observed value for `type = "response"`
and the prediction type. Trend components add a `component` column,
their names joined by `" + "`. A multivariate fit on a long frame adds a
leading `resp` column.
[`plot.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/reference/plot.mvgam_forecast.md)
draws the object.

## See also

[mvgam](https://nicholasjclark.github.io/mvgam/reference/mvgam.md),
[hindcast.mvgam](https://nicholasjclark.github.io/mvgam/reference/hindcast.mvgam.md),
[forecast.mvgam](https://nicholasjclark.github.io/mvgam/reference/forecast.mvgam.md)

## Author

Nicholas J Clark
