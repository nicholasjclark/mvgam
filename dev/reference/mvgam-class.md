# Fitted `mvgam` object description

A fitted `mvgam` object returned by function
[`mvgam`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md).
Run `methods(class = "mvgam")` to see an overview of available methods.

## Details

A fit from
[`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md)
has class `c("mvgam", "brmsfit")`. The accessors are the supported way
in.
[`variables()`](https://nicholasjclark.github.io/mvgam/dev/reference/index-mvgam.md),
[`as_draws_df()`](https://mc-stan.org/posterior/reference/draws_df.html)
and [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html)
return the posterior.
[`stancode()`](https://paulbuerkner.com/brms/reference/stancode.html)
and
[`standata()`](https://paulbuerkner.com/brms/reference/standata.html)
return the program and its data.
[`prior_summary()`](https://mc-stan.org/rstantools/reference/prior_summary.html)
returns the prior table. The elements are listed below.

The fit and its inputs:

- `fit` The `stanfit` object with the posterior draws of the combined
  observation and trend model

- `formula` The observation formula, as brms validated it

- `trend_formula` The trend formula without its trend constructor. brms
  builds the trend submodel from this formula. `NULL` when no
  `trend_formula` was supplied

- `trend_call` The trend formula as the user wrote it, constructor
  included. [`update()`](https://rdrr.io/r/stats/update.html) rebuilds
  the model from it. `NULL` when no `trend_formula` was supplied

- `family` The observation `family` object

- `prior` A `brmsprior` table of the priors in the compiled Stan program

- `data` The observation model frame

- `test_data` The `newdata` supplied at fitting, or `NULL`

- `data.name` The deparsed name of the `data` argument

- `codegen` The `knots`, `sample_prior`, `drop_unused_levels` and
  `normalize` settings the Stan program was generated under.
  [`update()`](https://rdrr.io/r/stats/update.html) passes them to the
  refit

- `silent` The verbosity of the call.
  [`update()`](https://rdrr.io/r/stats/update.html) passes it to the
  refit

- `save_pars` The
  [`brms::save_pars()`](https://paulbuerkner.com/brms/reference/save_pars.html)
  object the posterior was saved under

The Stan program:

- `stancode` The combined Stan program as a `character` string

- `standata` The Stan data list the program was fitted to

The model specification, used by prediction and forecasting:

- `mv_spec` The parsed model specification, including the trend
  specifications

- `trend_metadata` The trend type, its lag orders, the number of latent
  factors and the record of the model's axes. Prediction, forecasting
  and plotting use `axes` for the series and times the model was fitted
  on:

  - `axes$series$levels` The series, in the order of the trend matrix
    columns. Summaries, plots and forecasts label series with these

  - `axes$series$source` How mvgam built the series: `explicit` from a
    series column, `hierarchical` from `gr` and `subgr`, or
    `multivariate` from the responses of a wide formula

  - `axes$series$n` The number of series, equal to `N_series_trend` in
    the Stan data

  - `axes$series$groups` The group of each series, in the same order.
    `NULL` for a trend without `gr`

  - `axes$series$last_time` The last time each series was observed, in
    the same order. A forecast for a series starts after this time. A
    [`CAR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
    forecast starts after the last fitted time for every series

  - `axes$time$values` The ordered times the model was fitted on, in
    their original units.
    [`CAR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
    and Gaussian process terms compute their time gaps from these

  - `axes$time$n` The number of time points

  - `axes$time$step` The spacing between time points, or `NA` for
    irregular times.
    [`forecast()`](https://generics.r-lib.org/reference/forecast.html)
    extends the time grid by this step

  - `axes$time$observation_gap` The median gap between two consecutive
    observations of one series.
    [`CAR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
    measures its time gaps in this unit

  - `axes$factor$n_lv` The number of latent factors, equal to the number
    of columns of the loadings and of `lv_trend`

  - `axes$grain` `lv` when a `by = lv_axis()` term puts the trend design
    on the factor axis, otherwise `series`

  - `axes$vars` The column names for time, series, `gr` and `subgr`:
    `time_var`, `series_var`, `gr_var` and `subgr_var`

  - `axes$group_levels` The training levels of the `gr` and `subgr`
    columns. Prediction refuses new data with any other level

  `trend_metadata` is `NULL` for a model without a trend whose data lack
  a time column or a series column.

- `obs_model` A `brmsfit` of the observation model. Prediction at new
  data builds design matrices from it

- `trend_model` A `brmsfit` of the trend submodel, used the same way.
  `NULL` when no `trend_formula` was supplied

How it was fitted:

- `backend` `Character`, either `rstan` or `cmdstanr`

- `algorithm` `Character`, one of `sampling`, `laplace`, `pathfinder`,
  `meanfield` or `fullrank`

- `init` The initial-value specification as supplied (`"random"`, `"0"`,
  `"pathfinder"`, or a numeric value, list or function)

- `criteria` A named `list` of model-fit criteria that
  [`add_criterion()`](https://paulbuerkner.com/brms/reference/add_criterion.html)
  has computed, empty on a new fit

- `call` The matched call

- `brms_version`, `mvgam_version` The package versions the model was
  fitted under

- `stan_version` The version of Stan the backend compiled with, `NA`
  under the `"mock"` backend

- `creation_time` A `POSIXct` timestamp

A fit from
[`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md)
has class `c("mvgam", "jsdgam", "brmsfit")` and one further element. Its
`data` carries `time` and `series` columns copied from the `unit` and
`species` columns:

- `jsdgam_args` The arguments of the
  [`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md)
  call, with `unit` and `species` as column names.
  [`update()`](https://rdrr.io/r/stats/update.html) refits through
  [`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md)
  with them. Prediction accepts a frame naming the `unit` and `species`
  columns alone

## See also

[mvgam](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md),
[jsdgam](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md),
[mvgam_forecast](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_forecast-class.md)

## Author

Nicholas J Clark
