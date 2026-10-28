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

  Logical. Chooses which version of the trend each prediction uses.
  `FALSE`, the default, uses the covariate part of the model: the
  effects in the observation formula and the trend formula, without the
  trend's innovations. A prediction then depends on the covariates
  alone. It is the same at the first time point as at the fiftieth.
  Questions about covariate effects use this version. `TRUE` uses the
  fitted state, the value the trend took at each observed time as
  [`hindcast()`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.md)
  returns it. The section 'Which prediction function?' of
  [`predict.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/predict.mvgam.md)
  compares the functions.

  An ELPD or an \\R^2\\ describes the observed series.
  [`log_lik()`](https://mc-stan.org/rstantools/reference/log_lik.html),
  [`loo_R2.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_loo_extras.md)
  and
  [`bayes_R2.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_diagnostics.md)
  use `TRUE` for that reason. A time outside the training data is
  refused under `TRUE`.
  [`forecast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/forecast.mvgam.md)
  predicts those times. An in-sample interval under `TRUE` conditions on
  states estimated from the same observations. It covers them more often
  than its nominal level. Calibration is judged out of sample, with
  [`forecast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/forecast.mvgam.md)
  or
  [`lfo_cv()`](https://nicholasjclark.github.io/mvgam/dev/reference/lfo_cv.mvgam.md).

  The default suits a model whose covariates explain most of the signal.
  When most of the series-level variation lies in the trend, as in a fit
  with few covariates and a strong autoregressive process, the covariate
  part has little to say.
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

  Character; accepted for brms compatibility. It has no effect, because
  new levels are refused. See `allow_new_levels`.

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

## Trend state

By default the trend enters through its covariate part, without the
state it took at each observed time. The brms counterparts of this
method use that fitted state. `incl_autocor = TRUE` uses it here too, as
[`fitted.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/fitted.mvgam.md)
and
[`pp_check.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/pp_check.mvgam.md)
do on the training data. The section 'Which prediction function?' of
[`predict.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/predict.mvgam.md)
compares the prediction functions.

## See also

[`posterior_linpred.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_linpred.mvgam.md)
returns the predictor on the link scale.
[`hindcast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.mvgam.md)
and
[`forecast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/forecast.mvgam.md)
take the trend from its fitted state, in and beyond the training times.
The [forecast evaluation
article](https://nicholasjclark.github.io/mvgam/dev/articles/forecast_evaluation.html)
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
mod <- mvgam(y ~ s(x, k = 5), trend_formula = ~ AR(p = 1),
             data = simdat$data_train, family = poisson(),
             chains = 2, silent = 2)
#> Warning: 5 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.

# One row per draw, one column per observation
ep <- posterior_epred(mod)
dim(ep)
#> [1] 2000   90

# Expected counts from the covariate part and from the fitted state,
# against the observed counts
ep_state <- posterior_epred(mod, incl_autocor = TRUE)
library(ggplot2)
ggplot(simdat$data_train, aes(time, y)) +
  geom_point(colour = "grey60") +
  geom_line(aes(y = colMeans(ep), colour = "Covariates")) +
  geom_line(aes(y = colMeans(ep_state), colour = "Fitted trend")) +
  scale_colour_manual(values = c("#DCBCBC", "#8F2727"), name = NULL) +
  labs(y = "Count") +
  mvgam_theme()


# Sampling the trend's innovations raises the average of a log-link
# prediction
c(fixed = mean(ep),
  innovations = mean(posterior_epred(mod, process_error = TRUE)))
#>       fixed innovations 
#>    6.150306    7.364187 

# Next: posterior_predict() adds observation noise to these means
# }
```
