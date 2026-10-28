# Expected values of the response for each posterior draw

Draws of \\E\[Y \mid X\]\\, the mean of the response given each
posterior draw's parameters. The observation and trend predictors are
summed on the link scale. The family's mean then maps the sum onto the
response scale.
[`posterior_predict.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_predict.mvgam.md)
adds the observation noise around these means.

## Usage

``` r
# S3 method for class 'mvgam'
posterior_epred(
  object,
  newdata = NULL,
  process_error = FALSE,
  incl_autocor = FALSE,
  ndraws = NULL,
  draw_ids = NULL,
  re_formula = NULL,
  allow_new_levels = FALSE,
  sample_new_levels = "uncertainty",
  resp = NULL,
  ...
)
```

## Arguments

- object:

  A fitted mvgam object from
  [`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md).

- newdata:

  Optional data frame with covariates for prediction. If NULL, uses
  original training data stored in the model object.

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

  Logical; which prediction surface the trend contribution comes from.
  `FALSE`, the default, omits the fitted `trend[t, s]` and uses the two
  submodels' covariate structure. The effect is the same at the first
  time point as at the fiftieth. `TRUE` uses the latent state the model
  inferred at each time. That is the quantity
  [`hindcast()`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.md)
  returns. The ELPD is computed from it.
  [`loo_R2.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_loo_extras.md)
  and
  [`bayes_R2.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_diagnostics.md)
  request this surface. An R^2 describes the observed series. A row
  whose time falls outside the fitted grid is refused under `TRUE`.
  [`forecast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/forecast.mvgam.md)
  predicts those times. An in-sample interval under `TRUE` conditions on
  states estimated from the same observations. It covers them more often
  than its nominal level. Calibration is judged out of sample, with
  [`forecast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/forecast.mvgam.md)
  or
  [`lfo_cv()`](https://nicholasjclark.github.io/mvgam/dev/reference/lfo_cv.mvgam.md).

  The default suits a model whose covariates explain most of the signal.
  When most of the series-level variation lies in the trend, as in a fit
  with few covariates and a strong autoregressive process, this surface
  has little to say.
  [`hindcast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.mvgam.md)
  and
  [`forecast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/forecast.mvgam.md)
  are more informative there.

- ndraws:

  Positive integer specifying number of posterior draws to use. NULL
  (default) uses all available draws.

- draw_ids:

  Optional integer vector selecting a subset of posterior draw indices
  to use. NULL (default) uses all draws (subject to `ndraws`).

- re_formula:

  Group-level terms to include: `NULL` (the default) for every one, `NA`
  for none. A formula choosing some of them is not supported.

- allow_new_levels:

  Logical; accepted for brms compatibility. A grouping level the model
  never saw is refused whatever this is set to. A new level has no
  fitted random effect. For a new series, it has no latent state to
  propagate. Predict for levels the fit knows, or refit with the new
  levels included.

- sample_new_levels:

  Character; accepted for brms compatibility and not used, since new
  levels are refused. See `allow_new_levels`.

- resp:

  Character specifying which response variable for multivariate models.
  NULL (default) returns predictions for all responses.

- ...:

  Unused. Anything passed here is refused.

## Value

An `[ndraws, nobs]` matrix of expected values on the response scale, one
row per draw and one column per row of `newdata`. An ordinal response
gives an `[ndraws, nobs, ncat]` array of category probabilities. A
multivariate model with `resp = NULL` gives a named list, one element
per response.

## Details

For most families the mean is the inverse link of the predictor. A
binomial mean is `trials * p`, with the trials taken from the `trials()`
term evaluated against `newdata`. A zero-inflated or hurdle mean scales
the base mean by the probability of a non-zero draw. A lognormal mean
carries the correction `exp(mu + sigma^2 / 2)`. A
[`trunc()`](https://rdrr.io/r/base/Round.html) term gives the mean of
the distribution truncated to its bounds.

The mean of an
[`nmix()`](https://nicholasjclark.github.io/mvgam/dev/reference/nmix.md)
or
[`occ()`](https://nicholasjclark.github.io/mvgam/dev/reference/occ.md)
visit thins the latent abundance or occupancy by that visit's detection
probability. A composition family normalises its means across the
components of each unit.

## See also

[`posterior_linpred.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_linpred.mvgam.md)
returns the predictor on the link scale.
[`hindcast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.mvgam.md)
and
[`forecast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/forecast.mvgam.md)
take the trend from its fitted state, in and beyond the training times.
The [forecast evaluation
article](https://nicholasjclark.github.io/mvgam/articles/forecast_evaluation.html)
scores predictions of held-out times.

Other predictions:
[`conditional_effects.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/conditional_effects.mvgam.md),
[`conditional_smooths.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/conditional_smooths.mvgam.md),
[`fitted.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/fitted.mvgam.md),
[`hindcast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.mvgam.md),
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
             data = simdat$data_train, family = poisson(),
             chains = 2, silent = 2)
#> Warning: 9 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.

# One row per draw, one column per observation
ep <- posterior_epred(mod)
dim(ep)
#> [1] 2000   90

# The covariate surface and the surface that follows the fitted
# trend, against the observed counts
ep_state <- posterior_epred(mod, incl_autocor = TRUE)
library(ggplot2)
ggplot(simdat$data_train, aes(time, y)) +
  geom_point(colour = "grey60") +
  geom_line(aes(y = colMeans(ep), colour = "Covariates")) +
  geom_line(aes(y = colMeans(ep_state), colour = "Fitted trend")) +
  scale_colour_manual(values = c("#DCBCBC", "#8F2727"), name = NULL) +
  labs(y = "Count") +
  mvgam_theme()


# Sampling trend innovations raises a log-link mean by the Jensen
# correction
c(fixed = mean(ep),
  innovations = mean(posterior_epred(mod, process_error = TRUE)))
#>       fixed innovations 
#>    6.260141    7.402289 

# Next: posterior_predict() adds observation noise to these means
# }
```
