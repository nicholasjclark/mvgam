# Posterior predictions from an mvgam model

Predicts new outcomes, their expectation or the linear predictor from
every posterior draw, for the training data or for new data. The section
'Which prediction function?' compares
[`predict()`](https://rdrr.io/r/stats/predict.html) with
[`fitted.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/fitted.mvgam.md),
[`hindcast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.mvgam.md)
and
[`forecast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/forecast.mvgam.md).

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
  [`hindcast()`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.mvgam.md)
  returns it. The section 'Which prediction function?' of
  `predict.mvgam()` compares the functions.

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
  for none, or a formula naming the terms to keep, such as `~ (1 | g)`.
  A formula applies to the observation and the trend formulas alike. Any
  term it does not name is dropped.

- allow_new_levels:

  Logical. `TRUE` predicts for grouping levels the model never saw, with
  coefficients `sample_new_levels` draws. A new series stays refused: it
  has no latent state to propagate.

- sample_new_levels:

  How a new level's coefficients are drawn: `"uncertainty"` (the
  default) takes those of a fitted level chosen in each draw,
  `"old_levels"` those of one fitted level for all draws and
  `"gaussian"` draws them from the group's normal distribution. Each
  distributional parameter draws its own.

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
  `c(0.025, 0.975)` for 95% credible intervals.

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
  variance is the observation noise around the mean of the covariate
  part. `process_error = TRUE` also conditions on each draw's sampled
  trend innovation.

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

  `incl_autocor = TRUE` takes the state from the fitted latent trend,
  which a joint model shares across species.

- `"detection"`: for
  [`nmix()`](https://nicholasjclark.github.io/mvgam/dev/reference/nmix.md)
  and
  [`occ()`](https://nicholasjclark.github.io/mvgam/dev/reference/occ.md),
  the per-visit detection probability \\p\_{g,j}\\ on the response
  scale.

## Which prediction function?

Every prediction adds the latent trend to the observation model. The
functions differ in the version of the trend they use and in whether
they add observation noise.

|  |  |  |  |
|----|----|----|----|
| Function | Times | Trend by default | Observation noise |
| [`fitted()`](https://rdrr.io/r/stats/fitted.values.html) | training data | fitted state | no |
| [`predict()`](https://rdrr.io/r/stats/predict.html) | any | covariate part | under `type = "response"` |
| `posterior_predict()` | any | covariate part | yes |
| `posterior_epred()`, `posterior_linpred()` | any | covariate part | no |
| [`hindcast()`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.mvgam.md) | training data | fitted state | under `type = "response"` |
| [`forecast()`](https://generics.r-lib.org/reference/forecast.html) | after the training data | fitted state, carried forward | under `type = "response"` |
| [`residuals()`](https://rdrr.io/r/stats/residuals.html), [`pp_check()`](https://nicholasjclark.github.io/mvgam/dev/reference/pp_check.mvgam.md) | training data | fitted state | yes |

The fitted state is the value the trend took at each observed time, as
the model inferred it. The covariate part keeps the effects of the
observation and trend formulas and leaves out the trend's innovations.
Questions about covariates use it, as
[`conditional_effects.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/conditional_effects.mvgam.md)
and the marginaleffects functions do. `incl_autocor = TRUE` switches
[`predict()`](https://rdrr.io/r/stats/predict.html) and the
`posterior_*()` methods to the fitted state, the default of their brms
counterparts. [`fitted()`](https://rdrr.io/r/stats/fitted.values.html)
uses the covariate part for `newdata`.

## References

Royle, J. A. (2004). N-mixture models for estimating population size
from spatially replicated counts. *Biometrics*, 60, 108-115.
[doi:10.1111/j.0006-341X.2004.00142.x](https://doi.org/10.1111/j.0006-341X.2004.00142.x)

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
[`hindcast()`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.mvgam.md),
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
mod <- mvgam(y ~ s(x, k = 5), trend_formula = ~ AR(p = 1),
              data    = simdat$data_train,
              family  = poisson(),
              chains  = 2, silent = 2)
#> Warning: 9 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.

# Predicted counts with their 95% intervals
head(predict(mod))
#>      Estimate Est.Error  Q2.5 Q97.5
#> [1,]   2.1990  1.548095 0.000     6
#> [2,]  12.2930  4.300046 5.000    22
#> [3,]   8.2280  3.120879 3.000    15
#> [4,]   8.8850  3.509549 3.000    17
#> [5,]   4.6105  2.399514 1.000    10
#> [6,]   6.5915  3.159370 1.975    14

# The same fit on the link scale, as an expected count and as the
# variance of a count. A Poisson's variance equals its mean.
head(predict(mod, type = "link"))
#>      Estimate Est.Error      Q2.5    Q97.5
#> [1,] 0.749281 0.2279990 0.2666378 1.198096
#> [2,] 2.491617 0.1969154 2.1045983 2.862579
#> [3,] 2.090921 0.1743357 1.7310270 2.428086
#> [4,] 2.168891 0.1964567 1.7868074 2.565988
#> [5,] 1.490823 0.2435497 0.9877793 1.973285
#> [6,] 1.846478 0.2786232 1.2904415 2.412849
head(cbind(
  expected = predict(mod, type = "expected")[, "Estimate"],
  variance = predict(mod, type = "variance")[, "Estimate"]
))
#>       expected  variance
#> [1,]  2.170475  2.170475
#> [2,] 12.315698 12.315698
#> [3,]  8.215015  8.215015
#> [4,]  8.920217  8.920217
#> [5,]  4.572829  4.572829
#> [6,]  6.589161  6.589161

# `summary = FALSE` returns the draws, one row per draw
dim(predict(mod, summary = FALSE))
#> [1] 2000   90

# Next: forecast() predicts beyond the training times from the
# fitted trend state
# }
```
