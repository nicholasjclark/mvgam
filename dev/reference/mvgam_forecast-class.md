# `mvgam_forecast` object description

A `mvgam_forecast` object returned by function
[`hindcast`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.md)
or [`forecast`](https://generics.r-lib.org/reference/forecast.html). Run
`methods(class = "mvgam_forecast")` to see an overview of available
methods.

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
  [`hindcast()`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.md)).

- `hindcasts` Named list of posterior hindcast draw matrices, one
  `(ndraws x n_train)` matrix per series.

- `forecasts` Named list of posterior forecast draw matrices when
  produced by
  [`forecast()`](https://generics.r-lib.org/reference/forecast.html),
  otherwise `NULL`.

- `site_units` `TRUE` when the fit came from
  [`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md).
  Its units are sites, which
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) refuses to
  draw along a time axis.

- `component` The state components of the trend the draws hold, such as
  `"slope"`, or `NULL` for the summed trend. Set through the `component`
  argument of
  [`hindcast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.mvgam.md)
  and
  [`forecast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/forecast.mvgam.md).

## See also

[mvgam](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md),
[hindcast.mvgam](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.mvgam.md),
[forecast.mvgam](https://nicholasjclark.github.io/mvgam/dev/reference/forecast.mvgam.md)

## Author

Nicholas J Clark
