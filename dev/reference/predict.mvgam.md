# Draws from the Posterior Distribution of an mvgam Model

Posterior predictions for mvgam models. `type` selects the quantity:
[`posterior_predict.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_predict.mvgam.md)
for `"response"` (the default),
[`posterior_linpred.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_linpred.mvgam.md)
for `"link"` and
[`posterior_epred.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_epred.mvgam.md)
for `"expected"`. `"variance"` gives the family's conditional variance.

## Usage

``` r
# S3 method for class 'mvgam'
predict(
  object,
  newdata = NULL,
  type = c("response", "link", "expected", "variance", "terms", "latent_state",
    "detection"),
  process_error = FALSE,
  incl_autocor = FALSE,
  ndraws = NULL,
  draw_ids = NULL,
  re_formula = NULL,
  allow_new_levels = FALSE,
  sample_new_levels = "uncertainty",
  resp = NULL,
  summary = TRUE,
  robust = FALSE,
  probs = c(0.025, 0.975),
  ...
)
```

## Arguments

- object:

  An object of class `mvgam`.

- newdata:

  Optional data frame with covariates for prediction. If NULL, uses
  original training data stored in the model object.

- type:

  Character; one of `"response"` (default), `"link"`, `"expected"`,
  `"variance"`, `"terms"`, `"latent_state"`, `"detection"`. See Details.

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

`type` selects the quantity returned:

- `"response"` (default): draws from the posterior predictive
  distribution on the outcome scale, from
  [`posterior_predict.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_predict.mvgam.md).

- `"link"`: the linear predictor on the link scale, from
  [`posterior_linpred.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_linpred.mvgam.md).

- `"expected"`: the expectation E\[Y \| X\] on the response scale, from
  [`posterior_epred.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_epred.mvgam.md).

- `"variance"`: the conditional variance Var\[Y \| theta\] per draw,
  from the family's mean-variance relation. It covers the gaussian,
  student, lognormal, poisson, bernoulli, binomial, negbinomial, gamma
  and beta families. Other families raise an error. By default the
  variance is the observation noise around the mean of the two
  submodels. `process_error = TRUE` also conditions on each draw's
  sampled trend innovation.

- `"terms"`: refused with a pointer to the functions that decompose a
  prediction.
  [`posterior_smooths.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_smooths.mvgam.md)
  returns per-smooth draws.
  [`fixef.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_diagnostics.md)
  and
  [`ranef.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/ranef.mvgam.md)
  return the parametric and random-effect components.

- `"latent_state"`: for closure-unit families, the posterior of the
  latent state of each closure unit given its detection history:

  - [`nmix()`](https://nicholasjclark.github.io/mvgam/dev/reference/nmix.md):
    the latent abundance `N` given the observed visit counts (Royle
    2004).

  - [`occ()`](https://nicholasjclark.github.io/mvgam/dev/reference/occ.md):
    the occupancy probability `P(z = 1 | y_g)`.

- `"detection"`: for
  [`nmix()`](https://nicholasjclark.github.io/mvgam/dev/reference/nmix.md)
  and
  [`occ()`](https://nicholasjclark.github.io/mvgam/dev/reference/occ.md),
  the per-visit detection probability \\p\_{g,j}\\ on the response
  scale.

## See also

[`fitted.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/fitted.mvgam.md)
returns expected values with the inferred trend state.
[`forecast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/forecast.mvgam.md)
predicts future times from the fitted state.
[`conditional_effects.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/conditional_effects.mvgam.md)
plots predictions against a covariate.

Other predictions:
[`conditional_effects.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/conditional_effects.mvgam.md),
[`conditional_smooths.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/conditional_smooths.mvgam.md),
[`fitted.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/fitted.mvgam.md),
[`hindcast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.mvgam.md),
[`posterior_epred.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_epred.mvgam.md),
[`posterior_linpred.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_linpred.mvgam.md),
[`posterior_predict.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_predict.mvgam.md),
[`posterior_smooths.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_smooths.mvgam.md),
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

# Predicted counts with their 95% intervals
head(predict(mod))
#>      Estimate Est.Error Q2.5 Q97.5
#> [1,]   1.9970  1.490336    0     5
#> [2,]  14.3895  5.090751    6    25
#> [3,]   7.8510  3.292905    2    15
#> [4,]   8.3570  3.358153    3    16
#> [5,]   4.9305  2.465098    1    11
#> [6,]   7.2530  3.652761    2    15

# The same fit on the link scale, as an expected count and as the
# variance of a count. A Poisson's variance equals its mean.
head(predict(mod, type = "link"))
#>      Estimate Est.Error      Q2.5    Q97.5
#> [1,] 0.655104 0.2437725 0.1550484 1.106528
#> [2,] 2.647316 0.2124152 2.2497025 3.065758
#> [3,] 2.042712 0.1775426 1.7076587 2.390000
#> [4,] 2.099559 0.1963396 1.7273732 2.488058
#> [5,] 1.552701 0.2296787 1.1002138 1.996633
#> [6,] 1.930901 0.3130066 1.3389709 2.536739
head(cbind(
  expected = predict(mod, type = "expected")[, "Estimate"],
  variance = predict(mod, type = "variance")[, "Estimate"]
))
#>       expected  variance
#> [1,]  1.983064  1.983064
#> [2,] 14.446002 14.446002
#> [3,]  7.837157  7.837157
#> [4,]  8.326702  8.326702
#> [5,]  4.850117  4.850117
#> [6,]  7.250155  7.250155

# `summary = FALSE` returns the draws, one row per draw
dim(predict(mod, summary = FALSE))
#> [1] 2000   90

# Next: forecast() predicts beyond the training times from the
# fitted trend state
# }
```
