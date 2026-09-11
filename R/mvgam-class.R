#' Fitted `mvgam` object description
#'
#' A fitted \code{mvgam} object returned by function \code{\link{mvgam}}.
#' Run `methods(class = "mvgam")` to see an overview of available methods.
#'
#' @details An `mvgam` object inherits from `brmsfit` and carries the
#'   following elements. Reach for the accessors before the slots
#'   themselves: `variables()`, `as_draws_df()` and `as.data.frame()`
#'   read the posterior, `stancode()` and `standata()` return the
#'   program and its data, and `prior_summary()` returns the prior
#'   table.
#'
#'   The fit and what it was fitted to:
#'
#'   - `fit` The `stanfit` object holding the posterior draws of the
#'     combined observation and trend model
#'
#'   - `formula` The observation formula, as brms validated it
#'
#'   - `trend_formula` The trend formula with the trend constructor
#'     removed, which is the formula the trend submodel was built
#'     from. `NULL` when no `trend_formula` was supplied
#'
#'   - `trend_call` The trend formula as the user wrote it, with the
#'     constructor intact, so `update()` can rebuild the model without
#'     reconstructing the call. `NULL` when no `trend_formula` was
#'     supplied
#'
#'   - `family` The observation `family` object
#'
#'   - `prior` A `brmsprior` table of the priors the model sampled
#'     under, read from the compiled Stan program so it cannot
#'     disagree with what the sampler ran
#'
#'   - `data` The observation model frame
#'
#'   - `test_data` The `newdata` supplied at fitting, or `NULL`
#'
#'   - `data.name` The deparsed name of the `data` argument
#'
#'   - `codegen` The `knots`, `sample_prior`, `drop_unused_levels` and
#'     `normalize` settings the Stan program was generated under, so
#'     that [update()] rebuilds the same program rather than the
#'     default one
#'
#'   The Stan program:
#'
#'   - `stancode` The combined Stan program as a `character` string
#'
#'   - `standata` The `standata` list the program was fitted to
#'
#'   - `exclude` Parameter names withheld from summaries, `lprior` and
#'     `lp__`
#'
#'   The model specification, used by prediction and forecasting:
#'
#'   - `mv_spec` The parsed model specification, including the trend
#'     specifications
#'
#'   - `trend_metadata` The resolved trend details a prediction needs,
#'     including the time and series variables, the trend type and the
#'     number of latent factors. Its `axes` entry is the record of
#'     which series and which times the model was fitted on, and is
#'     what post-processing reads rather than rebuilding either axis
#'     from the training data:
#'
#'     - `axes$series$levels` The series, in the order the trend
#'       matrix numbers its columns. Every label a summary, plot or
#'       forecast shows comes from here
#'     - `axes$series$source` How the axis was arrived at: `explicit`
#'       from a series column, `hierarchical` from `gr` and `subgr`,
#'       or `multivariate` from the responses of a wide formula
#'     - `axes$series$n` The number of series, matching
#'       `N_series_trend` in the Stan data
#'     - `axes$series$groups` The group each series belongs to, in the
#'       same order, which is the order Stan subscripts
#'       `group_inds_trend` with. `NULL` when the trend names no
#'       grouping
#'     - `axes$series$last_time` The last occasion each series was
#'       observed on, in the same order, which is where a forecast
#'       for that series begins
#'     - `axes$time$values` The times the model was fitted on, ordered
#'       and in their original units, from which `CAR()` and the
#'       Gaussian processes take their gaps
#'     - `axes$time$n` How many occasions there are. The integer
#'       index a trend steps along is `match()` into `values`
#'     - `axes$time$step` The spacing a forecast extends the grid by,
#'       `NA` when the times are irregular
#'     - `axes$factor$n_lv` The number of latent factors, which is the
#'       column count of the loadings and of `lv_trend`
#'     - `axes$grain` What the second dimension of `times_trend`
#'       indexes: `series` ordinarily, or `lv` where a term written
#'       with `by = lv_axis()` puts the trend design on the factor
#'       axis
#'     - `axes$vars` The columns a row is placed by: `time_var`,
#'       `series_var`, `gr_var`, `subgr_var` and `response_vars`.
#'       Prediction reads these to identify a frame the model has
#'       never seen
#'
#'     A model fitted before this record existed carries no `axes`,
#'     and its series are read from `levels$series` instead.
#'
#'   - `trend_components` Per-component trend information derived from
#'     the posterior. `NULL` when the model has no trend
#'
#'   - `series_info` The number of series. Empty when the data name
#'     no series, in which case the count is read from `axes`
#'
#'   - `time_info` The number of time points, or empty when the data
#'     carry no time variable
#'
#'   - `obs_model` A `brmsfit` holding the observation-side model brms
#'     generated, used as the design-matrix source for prediction at
#'     new data
#'
#'   - `trend_model` A `brmsfit` holding the trend-side model, serving
#'     the same purpose for the trend submodel. `NULL` when no
#'     `trend_formula` was supplied
#'
#'   How it was fitted:
#'
#'   - `backend` `Character`, either `rstan` or `cmdstanr`
#'
#'   - `algorithm` `Character`, one of `sampling`, `laplace`,
#'     `pathfinder`, `meanfield` or `fullrank`
#'
#'   - `init` The initial-value specification, kept as the user wrote
#'     it (`"random"`, `"0"`, `"pathfinder"`, or a numeric value, list
#'     or function)
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
#'   A fit from [jsdgam()] has class `c("mvgam", "jsdgam")` and carries
#'   four further elements, which [ordinate()], [residual_cor()] and
#'   the `print` method read:
#'
#'   - `obs_data` The data frame as `jsdgam()` prepared it, with the
#'     `time` and `series` columns it derives from `unit` and `species`
#'
#'   - `model_data` The same frame, carrying a `prepped_trend_model`
#'     attribute that records the `unit` and `species` column names
#'
#'   - `model_spec` A `list` whose `is_jsdgam` element marks the fit
#'
#'
#' @seealso [mvgam], [jsdgam], [mvgam_forecast-class]
#'
#' @author Nicholas J Clark
#'
#' @name mvgam-class
NULL
