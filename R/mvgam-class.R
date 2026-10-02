#' Fitted `mvgam` object description
#'
#' A fitted \code{mvgam} object returned by function \code{\link{mvgam}}.
#' Run `methods(class = "mvgam")` to see an overview of available methods.
#'
#' @details A fit from [mvgam()] has class `c("mvgam", "brmsfit")`. The
#'   accessors are the supported way in: `variables()`, `as_draws_df()`
#'   and `as.data.frame()` return the posterior, `stancode()` and
#'   `standata()` return the program and its data, and
#'   `prior_summary()` returns the prior table. The elements are listed
#'   below.
#'
#'   The fit and its inputs:
#'
#'   - `fit` The `stanfit` object with the posterior draws of the
#'     combined observation and trend model
#'
#'   - `formula` The observation formula, as brms validated it
#'
#'   - `trend_formula` The trend formula without its trend constructor.
#'     brms builds the trend submodel from this formula. `NULL` when no
#'     `trend_formula` was supplied
#'
#'   - `trend_call` The trend formula as the user wrote it, constructor
#'     included. [update()] rebuilds the model from it. `NULL` when no
#'     `trend_formula` was supplied
#'
#'   - `family` The observation `family` object
#'
#'   - `prior` A `brmsprior` table of the priors in the compiled Stan
#'     program
#'
#'   - `data` The observation model frame
#'
#'   - `test_data` The `newdata` supplied at fitting, or `NULL`
#'
#'   - `data.name` The deparsed name of the `data` argument
#'
#'   - `codegen` The `knots`, `sample_prior`, `drop_unused_levels` and
#'     `normalize` settings the Stan program was generated under.
#'     [update()] passes them to the refit
#'
#'   - `silent` The verbosity of the call. [update()] passes it to the
#'     refit
#'
#'   - `save_pars` The [brms::save_pars()] object the posterior was
#'     saved under
#'
#'   The Stan program:
#'
#'   - `stancode` The combined Stan program as a `character` string
#'
#'   - `standata` The Stan data list the program was fitted to
#'
#'   The model specification, used by prediction and forecasting:
#'
#'   - `mv_spec` The parsed model specification, including the trend
#'     specifications
#'
#'   - `trend_metadata` The trend type, its lag orders, the number of
#'     latent factors and the record of the model's axes. Prediction,
#'     forecasting and plotting use `axes` for the series and times the
#'     model was fitted on:
#'
#'     - `axes$series$levels` The series, in the order of the trend
#'       matrix columns. Summaries, plots and forecasts label series
#'       with these
#'     - `axes$series$source` How mvgam built the series: `explicit`
#'       from a series column, `hierarchical` from `gr` and `subgr`,
#'       or `multivariate` from the responses of a wide formula
#'     - `axes$series$n` The number of series, equal to
#'       `N_series_trend` in the Stan data
#'     - `axes$series$groups` The group of each series, in the same
#'       order. `NULL` for a trend without `gr`
#'     - `axes$series$last_time` The last time each series was
#'       observed, in the same order. A forecast for a series starts
#'       after this time
#'     - `axes$time$values` The ordered times the model was fitted on,
#'       in their original units. `CAR()` and Gaussian process terms
#'       compute their time gaps from these
#'     - `axes$time$n` The number of time points
#'     - `axes$time$step` The spacing between time points, or `NA` for
#'       irregular times. [forecast()] extends the time grid by this
#'       step
#'     - `axes$factor$n_lv` The number of latent factors, equal to the
#'       number of columns of the loadings and of `lv_trend`
#'     - `axes$grain` `lv` when a `by = lv_axis()` term puts the trend
#'       design on the factor axis, and `series` otherwise
#'     - `axes$vars` The column names for time, series, `gr` and
#'       `subgr`: `time_var`, `series_var`, `gr_var` and `subgr_var`
#'     - `axes$group_levels` The training levels of the `gr` and
#'       `subgr` columns. Prediction refuses new data with any other
#'       level
#'
#'     `trend_metadata` is `NULL` for a model without a trend whose
#'     data lack a time column or a series column.
#'
#'   - `obs_model` A `brmsfit` of the observation model. Prediction at
#'     new data builds design matrices from it
#'
#'   - `trend_model` A `brmsfit` of the trend submodel, used the same
#'     way. `NULL` when no `trend_formula` was supplied
#'
#'   How it was fitted:
#'
#'   - `backend` `Character`, either `rstan` or `cmdstanr`
#'
#'   - `algorithm` `Character`, one of `sampling`, `laplace`,
#'     `pathfinder`, `meanfield` or `fullrank`
#'
#'   - `init` The initial-value specification as supplied
#'     (`"random"`, `"0"`, `"pathfinder"`, or a numeric value, list or
#'     function)
#'
#'   - `criteria` A named `list` of model-fit criteria that
#'     [add_criterion()] has computed, empty on a new fit
#'
#'   - `call` The matched call
#'
#'   - `brms_version`, `mvgam_version` The package versions the model
#'     was fitted under
#'
#'   - `stan_version` The version of Stan the backend compiled with,
#'     `NA` under the `"mock"` backend
#'
#'   - `creation_time` A `POSIXct` timestamp
#'
#'   A fit from [jsdgam()] has class `c("mvgam", "jsdgam", "brmsfit")`
#'   and one further element. Its `data` carries `time` and `series`
#'   columns copied from the `unit` and `species` columns:
#'
#'   - `jsdgam_args` The arguments of the `jsdgam()` call, with `unit`
#'     and `species` as column names. [update()] refits through
#'     `jsdgam()` with them, and prediction accepts a frame naming the
#'     `unit` and `species` columns alone
#'
#' @seealso [mvgam], [jsdgam], [mvgam_forecast-class]
#'
#' @author Nicholas J Clark
#'
#' @name mvgam-class
NULL
