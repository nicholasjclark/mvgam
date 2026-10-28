# Expected values of an mvgam model

Expected values E\[Y \| X\] of an mvgam model, or its linear predictor
with `scale = "linear"`, at the training observations by default. The
interface follows
[`fitted.brmsfit()`](https://paulbuerkner.com/brms/reference/fitted.brmsfit.html).

## Usage

``` r
# S3 method for class 'mvgam'
fitted(
  object,
  newdata = NULL,
  re_formula = NULL,
  scale = c("response", "linear"),
  components = c("response", "latent_state", "detection"),
  unit_level = NULL,
  resp = NULL,
  ndraws = NULL,
  draw_ids = NULL,
  summary = TRUE,
  robust = FALSE,
  probs = c(0.025, 0.975),
  process_error = FALSE,
  incl_autocor = is.null(newdata),
  allow_new_levels = FALSE,
  sample_new_levels = "uncertainty",
  ...
)
```

## Arguments

- object:

  An object of class `mvgam`.

- newdata:

  Optional data frame with covariates for prediction. If NULL, uses
  original training data stored in the model object.

- re_formula:

  Group-level terms to include: `NULL` (the default) for every one, `NA`
  for none. A formula choosing some of them is not supported.

- scale:

  Character; one of `"response"` (default) or `"linear"`. `"response"`
  returns expected values on the response scale via
  [`posterior_epred.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_epred.mvgam.md).
  `"linear"` returns the linear predictor on the link scale via
  [`posterior_linpred.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_linpred.mvgam.md).
  Ignored when `components` is not `"response"`.

- components:

  Character; one of `"response"` (default), `"latent_state"`, or
  `"detection"`. The quantity to return:

  - `"response"`: the expected value of each row of the data, on the
    scale `scale` sets.

  - `"latent_state"`, for
    [`nmix()`](https://nicholasjclark.github.io/mvgam/dev/reference/nmix.md)
    and
    [`occ()`](https://nicholasjclark.github.io/mvgam/dev/reference/occ.md)
    only: the latent state of each closure unit given its detections.
    That is the abundance `N` for
    [`nmix()`](https://nicholasjclark.github.io/mvgam/dev/reference/nmix.md)
    and the probability of occupancy `P(z = 1 | y)` for
    [`occ()`](https://nicholasjclark.github.io/mvgam/dev/reference/occ.md).
    [`predict.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/predict.mvgam.md)
    returns the same with `type = "latent_state"`.

  - `"detection"`, for
    [`nmix()`](https://nicholasjclark.github.io/mvgam/dev/reference/nmix.md)
    and
    [`occ()`](https://nicholasjclark.github.io/mvgam/dev/reference/occ.md)
    only: the detection probability of each visit, as
    `type = "detection"` of
    [`predict.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/predict.mvgam.md)
    returns it.

  The argument matches `components` of `flocker::fitted_flocker()`.

- unit_level:

  Logical or `NULL` (default). When `TRUE` for a closure-unit detection
  family and `components = "response"`, sums the per-visit response
  matrix over the visits of each unit. Other `components` have fixed
  grains and ignore it. So do families that do not aggregate visits.

- resp:

  Character specifying which response variable for multivariate models.
  NULL (default) returns predictions for all responses.

- ndraws:

  Positive integer specifying number of posterior draws to use. NULL
  (default) uses all available draws.

- draw_ids:

  Optional integer vector selecting a subset of posterior draw indices
  to use. NULL (default) uses all draws (subject to `ndraws`).

- summary:

  Logical. If `TRUE` (the default), returns summary statistics. If
  `FALSE`, returns the full matrix of posterior draws.

- robust:

  Logical. If `FALSE` (the default), uses mean and standard deviation
  for summaries. If `TRUE`, uses median and median absolute deviation
  (MAD).

- probs:

  Numeric vector of probabilities for quantile computation. Default is
  `c(0.025, 0.975)` for 95% credible intervals.

- process_error:

  Logical. `FALSE`, the default, leaves the trend's innovations out of
  the prediction. `TRUE` adds a random deviation at each time and
  series, drawn anew for every posterior draw from the spread the trend
  shows over the long run. For an AR(1) trend with innovation SD
  \\\sigma\\ and coefficient \\\rho\\ that spread has variance
  \\\sigma^2/(1-\rho^2)\\. On a log link the deviations raise the
  average prediction by the factor \\\exp(\sigma^2/(2(1-\rho^2)))\\. A
  random walk has no long-run spread. A
  [`ZMVN()`](https://nicholasjclark.github.io/mvgam/dev/reference/ZMVN.md)
  trend has no dynamics. These two draw from the innovation covariance
  itself.
  [`CAR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
  draws from one long-run covariance at every time of an irregular grid.
  The argument applies under `incl_autocor = FALSE`. The fitted state
  already holds every innovation.

  Each call draws new innovations. Two calls on one fit give different
  results. Set a seed to reproduce a result.

- incl_autocor:

  Logical; whether the fitted values include the latent trend state the
  model inferred at each time. Defaults to `TRUE` for the training data,
  as in
  [`brms::fitted.brmsfit()`](https://paulbuerkner.com/brms/reference/fitted.brmsfit.html).
  Defaults to `FALSE` with `newdata`. Under `TRUE` the observed response
  minus the fitted value is the ordinary residual of
  [`residuals.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/residuals.mvgam.md).

- allow_new_levels:

  Logical; accepted for brms compatibility. A grouping level the model
  never saw is refused whatever this is set to. A new level has no
  fitted random effect. For a new series, it has no latent state to
  propagate. Predict for levels the fit knows, or refit with the new
  levels included.

- sample_new_levels:

  Character; accepted for brms compatibility. It has no effect, because
  new levels are refused. See `allow_new_levels`.

- ...:

  Unused. Anything passed here is refused.

## Value

With `summary = FALSE`, an `[ndraws, nobs]` matrix of draws. With
`summary = TRUE`, a matrix with one row per observation and the columns:

- `Estimate`: the posterior mean, or the median under `robust = TRUE`.

- `Est.Error`: the posterior SD, or the MAD under `robust = TRUE`.

- `Q*`: one quantile column per value of `probs`.

A multivariate model with `resp = NULL` gives a named list of these, one
per response.

## Details

As in brms, fitted values leave out the observation noise.
[`posterior_predict.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_predict.mvgam.md)
adds it. Use
[`predict.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/predict.mvgam.md)
for predictive draws.

For the training data the fitted values include the trend state the
model inferred at each time.
[`hindcast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.mvgam.md)
returns the same state. With `newdata`, or under `incl_autocor = FALSE`,
the fitted values describe the covariate part of the model.
`process_error` then sets the trend's part. `FALSE` keeps the trend's
covariate effects. `TRUE` also samples a state per draw from the trend's
dynamics.

The brms arguments `dpar`, `nlpar` and `sort` are refused.
[`posterior_linpred.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_linpred.mvgam.md)
returns a distributional parameter through its own `dpar`. Rows come
back in the order of the frame.

## See also

[`residuals.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/residuals.mvgam.md)
compares the fitted values with the data.
[`hindcast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.mvgam.md)
returns the fitted latent states.
[`forecast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/forecast.mvgam.md)
predicts future times.

Other predictions:
[`conditional_effects.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/conditional_effects.mvgam.md),
[`conditional_smooths.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/conditional_smooths.mvgam.md),
[`hindcast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.mvgam.md),
[`posterior_epred.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_epred.mvgam.md),
[`posterior_linpred.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_linpred.mvgam.md),
[`posterior_predict.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_predict.mvgam.md),
[`posterior_smooths.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_smooths.mvgam.md),
[`predict.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/predict.mvgam.md),
[`predictive_error.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/predictive_error.mvgam.md),
[`predictive_interval.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/predictive_interval.mvgam.md)

## Examples

``` r
# \dontrun{
set.seed(13)
simdat <- sim_mvgam(family = poisson(), n_series = 1L,
                     n_timepoints = 120L, trend_model = AR())
mod <- mvgam(y ~ s(x, k = 5), trend_formula = ~ AR(p = 1),
              data    = simdat$data_train,
              family  = poisson(),
              chains  = 2, silent = 2)
#> Warning: 5 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.

# Expected counts at the training observations: the posterior mean,
# error and 95% interval of each
head(fitted(mod))
#>       Estimate Est.Error      Q2.5     Q97.5
#> [1,]  2.213963 0.9497584 0.8443745  4.477423
#> [2,] 12.868985 3.2440969 7.6262390 20.232959
#> [3,]  6.133415 1.8591295 3.1063965 10.225225
#> [4,]  4.267246 1.5377098 1.7451359  7.820154
#> [5,]  3.792407 1.4073015 1.6404318  7.154235
#> [6,]  5.103720 1.7479758 2.3529934  9.123604

# The covariate effects alone, without the inferred trend state
head(fitted(mod, incl_autocor = FALSE))
#>       Estimate Est.Error     Q2.5     Q97.5
#> [1,]  2.179257 0.4797681 1.370309  3.240618
#> [2,] 12.501430 2.3923246 8.346400 18.081087
#> [3,]  8.302232 1.3883129 5.749874 11.304569
#> [4,]  8.900367 1.6533494 6.174799 12.591126
#> [5,]  4.558828 1.0862699 2.787153  6.950084
#> [6,]  6.484566 1.7949456 3.705107 10.530367

# One row per posterior draw and one column per observation
dim(fitted(mod, summary = FALSE))
#> [1] 2000   90

# Next: residuals() and pp_check() compare the fit with the data
# }
```
