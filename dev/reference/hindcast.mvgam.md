# Posterior hindcasts for an mvgam state-space model

Posterior draws at the training time points. Each draw takes the latent
state the model inferred at each time. The observation family maps it to
the response scale. A hindcast shows how closely the fitted states track
the data.
[`forecast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/forecast.mvgam.md)
predicts unseen times.

## Usage

``` r
# S3 method for class 'mvgam'
hindcast(
  object,
  ...,
  type = c("response", "link", "expected", "trend", "latent_state"),
  component = NULL,
  ndraws = NULL,
  draw_ids = NULL,
  obs_uncertainty = TRUE,
  process_error = FALSE,
  resp = NULL
)
```

## Arguments

- object:

  A fitted
  [mvgam](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md)
  object.

- ...:

  Unused. Anything passed here is refused.

- type:

  The scale of the draws, as in
  [`forecast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/forecast.mvgam.md),
  or `"latent_state"` for a closure-unit family. `"latent_state"`
  returns the occupancy probability `psi` of an
  [`occ()`](https://nicholasjclark.github.io/mvgam/dev/reference/occ.md)
  model or the abundance `N` of an
  [`nmix()`](https://nicholasjclark.github.io/mvgam/dev/reference/nmix.md)
  model at each closure unit, as an `mvgam_latent_state` object with
  [`print()`](https://rdrr.io/r/base/print.html),
  [`summary()`](https://rdrr.io/r/base/summary.html),
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) and
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods.

- component:

  `NULL` (the default) for the summed trend, or the names of state
  components of the trend, with `type = "trend"`. Several names give the
  sum of their components.
  [`LLT()`](https://nicholasjclark.github.io/mvgam/dev/reference/LLT.md)
  has `"level"`, the stochastic level, and `"slope"`, its growth per
  time step. A seasonal
  [`LLT()`](https://nicholasjclark.github.io/mvgam/dev/reference/LLT.md)
  adds `"season"`. `"predictor"` holds the effects of the trend
  formula's covariates. `c("level", "predictor")` gives the level with
  those effects. A factor model projects each component through its
  loadings, as it projects the trend.

- ndraws:

  Positive integer specifying number of posterior draws to use. NULL
  (default) uses all available draws.

- draw_ids:

  Optional integer vector selecting a subset of posterior draw indices
  to use. NULL (default) uses all draws (subject to `ndraws`).

- obs_uncertainty:

  Logical, `TRUE` by default. `FALSE` returns the family's mean under
  `type = "response"`, in place of a sampled observation.

- process_error:

  Logical. `FALSE` (the default) takes the fitted latent state of each
  draw. `type = "response"` then adds the observation noise alone.
  `TRUE` replaces the fitted states with the covariate part plus newly
  drawn innovations, as
  [`posterior_epred.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_epred.mvgam.md)
  gives with the same argument. The hindcast then shows what the model
  predicts without the observed history. A `component` is a fitted state
  and needs `FALSE`.

- resp:

  Character specifying which response variable for multivariate models.
  NULL (default) returns predictions for all responses.

## Value

An
[mvgam_forecast-class](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md)
object. Its hindcasts are a named list of `[ndraws, n_times]` matrices,
one per series. Its forecast slots are `NULL`.

## See also

[`forecast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/forecast.mvgam.md)
extrapolates the fitted state.
[`posterior_predict.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_predict.mvgam.md)
and
[`posterior_epred.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_epred.mvgam.md)
predict from the covariate part of the model. The [multivariate
article](https://nicholasjclark.github.io/mvgam/dev/articles/mvbf.html)
compares `type = "trend"` hindcasts of one latent state from a joint
model and from each data source alone. The [N-mixture
article](https://nicholasjclark.github.io/mvgam/dev/articles/nmix.html)
summarises `type = "latent_state"` hindcasts of abundance.

Other predictions:
[`conditional_effects.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/conditional_effects.mvgam.md),
[`conditional_smooths.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/conditional_smooths.mvgam.md),
[`fitted.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/fitted.mvgam.md),
[`posterior_epred.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_epred.mvgam.md),
[`posterior_linpred.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_linpred.mvgam.md),
[`posterior_predict.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_predict.mvgam.md),
[`posterior_smooths.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_smooths.mvgam.md),
[`predict.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/predict.mvgam.md),
[`predictive_error.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/predictive_error.mvgam.md),
[`predictive_interval.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/predictive_interval.mvgam.md)

Other forecasting:
[`compare_scores()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_scores.md),
[`ensemble.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/ensemble.mvgam_forecast.md),
[`forecast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/forecast.mvgam.md),
[`lfo_cv.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/lfo_cv.mvgam.md),
[`score.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/score.mvgam_forecast.md)

## Examples

``` r
# \dontrun{
set.seed(11)
simdat <- sim_mvgam(family = poisson(), n_series = 1L,
                     n_timepoints = 120L, trend_model = AR())

mod <- mvgam(
  y ~ s(x, k = 5),
  trend_formula = ~ AR(p = 1),
  data    = simdat$data_train,
  family  = poisson(),
  chains  = 2, silent = 2
)
#> Warning: 36 of 2000 (2.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.

# The fitted states track the training counts
hc <- hindcast(mod)
plot(hc)


# The AR(1) trend on the log scale
plot(hindcast(mod, type = "trend"))


# Fresh innovations in place of the fitted states give the wider
# spread a model without the observed history would predict
plot(hindcast(mod, process_error = TRUE))


# A local linear trend's slope, the level's growth per step
mod_llt <- update(mod, trend_formula = ~ LLT())
#> Warning: 2 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
plot(hindcast(mod_llt, type = "trend", component = "slope"))


# Next, forecast the held-out times with forecast()
# }
```
