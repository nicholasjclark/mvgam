# Linear predictor draws for each posterior draw

Draws of the linear predictor \\\eta\\, the sum of the observation and
trend predictors on the link scale. A Poisson predictor is on the log
scale and a binomial predictor on the logit scale.
[`posterior_epred.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_epred.mvgam.md)
maps it onto the response scale.

## Usage

``` r
# S3 method for class 'mvgam'
posterior_linpred(
  object,
  transform = FALSE,
  newdata = NULL,
  process_error = FALSE,
  incl_autocor = FALSE,
  ndraws = NULL,
  draw_ids = NULL,
  re_formula = NULL,
  allow_new_levels = FALSE,
  sample_new_levels = "uncertainty",
  resp = NULL,
  dpar = NULL,
  ...
)
```

## Arguments

- object:

  A fitted mvgam object from
  [`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md).

- transform:

  Logical. `FALSE` (the default) returns the link scale. `TRUE` applies
  the inverse link of `mu`. That gives `mu` on its own scale. A binomial
  `mu` is a probability. A hurdle or zero-inflated `mu` is the base
  distribution's parameter. Neither is the mean of the response, which
  [`posterior_epred.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_epred.mvgam.md)
  returns.

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

- dpar:

  Character naming a distributional parameter, such as `"sigma"` or
  `"nu"`. The default `NULL` returns the predictor of the mean. A
  parameter with a formula of its own returns its linear predictor. That
  predictor holds no trend term. A parameter sampled as a scalar has no
  link scale. Its draws fill every row on their own scale, whatever
  `transform` is.

- ...:

  Unused. Anything passed here is refused.

## Value

An `[ndraws, nobs]` matrix, one row per draw and one column per row of
`newdata`. It is on the link scale unless `transform = TRUE` or `dpar`
names a scalar parameter. A multivariate model with `resp = NULL` gives
a named list, one element per response.

## Details

The observation predictor holds the fixed, group-level, smooth and
Gaussian process terms of `formula`. The trend predictor holds the terms
of `trend_formula`. `process_error = TRUE` adds a sampled deviation at
each time and series, on the link scale.
[`posterior_epred.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_epred.mvgam.md)
and
[`posterior_predict.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_predict.mvgam.md)
apply the family's mean and noise to that same predictor.

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

[`posterior_epred.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_epred.mvgam.md)
returns expected values.
[`posterior_predict.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_predict.mvgam.md)
returns predictive draws.
[`hindcast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.mvgam.md)
and
[`forecast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/forecast.mvgam.md)
take the trend from its fitted state. The [forecast evaluation
article](https://nicholasjclark.github.io/mvgam/dev/articles/forecast_evaluation.html)
scores predictions of held-out times.

Other predictions:
[`conditional_effects.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/conditional_effects.mvgam.md),
[`conditional_smooths.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/conditional_smooths.mvgam.md),
[`fitted.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/fitted.mvgam.md),
[`hindcast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.mvgam.md),
[`posterior_epred.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_epred.mvgam.md),
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

# Log rates, one row per draw and one column per observation
lp <- posterior_linpred(mod, draw_ids = 1:50)
dim(lp)
#> [1] 50 90

# A Poisson mean is the inverse link of the predictor
all.equal(exp(lp), posterior_epred(mod, draw_ids = 1:50))
#> [1] TRUE

# The predictor under the fitted trend state tracks the series
lp_state <- posterior_linpred(mod, incl_autocor = TRUE)
library(ggplot2)
ggplot(simdat$data_train, aes(time, log1p(y))) +
  geom_point(colour = "grey60") +
  geom_line(aes(y = colMeans(lp_state)), colour = "#8F2727",
            linewidth = 1) +
  labs(y = "log(1 + count)") +
  mvgam_theme()


# Next: posterior_epred() maps the predictor to expected counts
# }
```
