# Expected Values from the Posterior Distribution

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
  `"detection"`. Selects which posterior component to return.

  - `"response"` keeps the brms-default behaviour: marginal expected
    values at the per-row grain, scaled via `scale`.

  - `"latent_state"` (closure-unit families only) routes through
    [`predict.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/predict.mvgam.md)
    with `type = "latent_state"`, returning the conditional latent state
    per closure unit (`P(z = 1 | y)` for
    [`occ()`](https://nicholasjclark.github.io/mvgam/dev/reference/occ.md),
    posterior `N` for
    [`nmix()`](https://nicholasjclark.github.io/mvgam/dev/reference/nmix.md)
    variants).

  - `"detection"` (closure-unit families only) routes through
    [`predict.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/predict.mvgam.md)
    with `type = "detection"`, returning per- visit detection
    probabilities.

  Modelled on the `flocker::fitted_flocker(components = ...)` argument
  so flocker users have a one-call equivalent.

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
  `c(0.025, 0.975)` for 95% credible intervals. Can be a single value or
  multiple values.

- process_error:

  Logical; if `TRUE`, the trend adds a sampled innovation to its
  deterministic submodel. Each posterior draw takes one innovation from
  the distribution the trend settles into. An autoregressive trend
  settles into its stationary distribution: an AR(1) draws at
  \\\sigma^2/(1-\rho^2)\\. The Jensen correction on a log link is
  \\\sigma^2/(2(1-\rho^2))\\. A random walk lacks a stationary
  distribution and a
  [`ZMVN()`](https://nicholasjclark.github.io/mvgam/dev/reference/ZMVN.md)
  trend lacks dynamics. Both draw their innovations directly.
  [`CAR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
  uses one stationary covariance at every time of an irregular grid and
  draws from it. If `FALSE`, the default, the trend contributes its
  deterministic submodel alone, at its own per-draw values. The argument
  applies under `incl_autocor = FALSE`. Conditioning on the fitted state
  already fixes every innovation.

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

  Character; accepted for brms compatibility and not used, since new
  levels are refused. See `allow_new_levels`.

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
the fitted values describe the covariate structure. `process_error` then
sets the trend's part. `FALSE` keeps its deterministic submodel. `TRUE`
samples a state per draw from the trend's dynamics.

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
mod <- mvgam(y ~ s(x), trend_formula = ~ AR(p = 1),
              data    = simdat$data_train,
              family  = poisson(),
              chains  = 2, silent = 2)
#> Warning: 9 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.

# Expected counts at the training observations: the posterior mean,
# error and 95% interval of each
head(fitted(mod))
#>       Estimate Est.Error      Q2.5     Q97.5
#> [1,]  1.968095 0.8326505 0.7564453  3.990160
#> [2,] 13.323024 3.1017601 7.9543207 20.025617
#> [3,]  5.844418 1.7971733 2.8605365 10.201195
#> [4,]  4.231734 1.4844296 1.9142345  7.607356
#> [5,]  3.824684 1.3981040 1.6954879  6.957924
#> [6,]  5.382676 1.8392005 2.4784256  9.685531

# The covariate effects alone, without the inferred trend state
head(fitted(mod, incl_autocor = FALSE))
#>       Estimate Est.Error     Q2.5     Q97.5
#> [1,]  1.983064 0.4905737 1.167715  3.023842
#> [2,] 14.446002 3.2741695 9.484913 21.450717
#> [3,]  7.837157 1.4931008 5.516031 10.913494
#> [4,]  8.326702 1.7752656 5.625857 12.037871
#> [5,]  4.850117 1.1292925 3.004808  7.364216
#> [6,]  7.250155 2.4695363 3.815116 12.638388

# One row per posterior draw and one column per observation
dim(fitted(mod, summary = FALSE))
#> [1] 2000   90

# Next: residuals() and pp_check() compare the fit with the data
# }
```
