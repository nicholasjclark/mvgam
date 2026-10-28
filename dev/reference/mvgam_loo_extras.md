# Leave-one-out predictions, R-squared and criteria for mvgam models

The brms methods that take their weights from
[`loo.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/loo.mvgam.md).
[`loo_predict()`](https://mc-stan.org/rstantools/reference/loo-prediction.html),
[`loo_epred()`](https://paulbuerkner.com/brms/reference/loo_predict.brmsfit.html)
and
[`loo_linpred()`](https://mc-stan.org/rstantools/reference/loo-prediction.html)
reweight posterior predictions by the PSIS weights of each observation,
approximating the prediction made without it.
[`loo_predictive_interval()`](https://mc-stan.org/rstantools/reference/loo-prediction.html)
gives the leave-one-out predictive interval.
[`loo_R2()`](https://mc-stan.org/rstantools/reference/bayes_R2.html) is
the Bayesian R^2 of Gelman et al. (2019) computed from leave-one-out
predictions.
[`add_criterion()`](https://paulbuerkner.com/brms/reference/add_criterion.html)
stores criteria on the fit.
[`LOO()`](https://paulbuerkner.com/brms/reference/loo.brmsfit.html) and
[`WAIC()`](https://paulbuerkner.com/brms/reference/waic.brmsfit.html)
are the brms aliases of
[`loo.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/loo.mvgam.md)
and
[`waic.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/waic.mvgam.md).

## Usage

``` r
# S3 method for class 'mvgam'
LOO(
  x,
  ...,
  compare = TRUE,
  resp = NULL,
  pointwise = FALSE,
  moment_match = FALSE,
  reloo = FALSE,
  k_threshold = 0.7,
  save_psis = FALSE,
  moment_match_args = list(),
  reloo_args = list(),
  model_names = NULL
)

# S3 method for class 'mvgam'
WAIC(
  x,
  ...,
  compare = TRUE,
  resp = NULL,
  pointwise = FALSE,
  model_names = NULL
)

# S3 method for class 'mvgam'
loo_R2(
  object,
  resp = NULL,
  summary = TRUE,
  robust = FALSE,
  probs = c(0.025, 0.975),
  seed = NULL,
  args_epred = list(),
  args_loglik = list(),
  ...
)

# S3 method for class 'mvgam'
loo_predict(
  object,
  type = c("mean", "var", "quantile"),
  probs = 0.5,
  psis_object = NULL,
  resp = NULL,
  ...
)

# S3 method for class 'mvgam'
loo_epred(
  object,
  type = c("mean", "var", "quantile"),
  probs = 0.5,
  psis_object = NULL,
  resp = NULL,
  ...
)

# S3 method for class 'mvgam'
loo_linpred(
  object,
  type = c("mean", "var", "quantile"),
  probs = 0.5,
  psis_object = NULL,
  resp = NULL,
  ...
)

# S3 method for class 'mvgam'
loo_predictive_interval(object, prob = 0.9, psis_object = NULL, ...)

# S3 method for class 'mvgam'
loo_subsample(x, ..., compare = TRUE, resp = NULL, model_names = NULL)

# S3 method for class 'mvgam'
loo_moment_match(
  x,
  loo = NULL,
  k_threshold = 0.7,
  newdata = NULL,
  resp = NULL,
  check = TRUE,
  recompile = FALSE,
  ...
)

# S3 method for class 'mvgam'
loo_model_weights(x, ..., model_names = NULL)

# S3 method for class 'mvgam'
add_criterion(
  x,
  criterion,
  model_name = NULL,
  overwrite = FALSE,
  file = NULL,
  force_save = FALSE,
  ...
)
```

## Arguments

- x, object:

  A fitted `mvgam` object.

- ...:

  Further arguments to the method each function calls.
  [`loo_R2()`](https://mc-stan.org/rstantools/reference/bayes_R2.html)
  refuses them.

- compare, pointwise, moment_match, reloo, k_threshold, save_psis,
  moment_match_args, reloo_args:

  As in
  [`loo.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/loo.mvgam.md),
  which describes the two it uses and refuses the rest.

- resp:

  Optional name of one response of a multivariate model.

- model_names:

  Optional character vector of model names, one per forecast. It
  replaces the names taken from the call.

- summary:

  Logical. `TRUE` (the default) returns the summary matrix with
  `Estimate`, `Est.Error` and interval columns. `FALSE` returns the
  draws.

- robust:

  Logical. If `FALSE` (the default), uses mean and standard deviation
  for summaries. If `TRUE`, uses median and median absolute deviation
  (MAD).

- probs:

  Numeric vector of probabilities for quantile computation. Default is
  `c(0.025, 0.975)` for 95% credible intervals.

- seed:

  Optional integer seed. The session's random state is restored when the
  call returns.

- args_epred, args_loglik:

  Lists of further arguments to
  [`posterior_epred.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_epred.mvgam.md)
  and
  [`log_lik.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/log_lik.mvgam.md)
  for
  [`loo_R2()`](https://mc-stan.org/rstantools/reference/bayes_R2.html).

- type:

  The expectation: `"mean"` (the default), `"var"` or `"quantile"`.

- psis_object:

  Optional `psis` object from `loo(x, save_psis = TRUE)`, reused across
  calls.

- prob:

  Interval mass of
  [`loo_predictive_interval()`](https://mc-stan.org/rstantools/reference/loo-prediction.html),
  0.9 by default as in brms.

- loo, newdata, check, recompile:

  Accepted for the brms signature of
  [`loo_moment_match()`](https://mc-stan.org/loo/reference/loo_moment_match.html),
  which mvgam refuses.

- criterion:

  Character vector of criteria for
  [`add_criterion()`](https://paulbuerkner.com/brms/reference/add_criterion.html):
  `"loo"`, `"waic"`, `"bayes_R2"` or `"loo_R2"`.

- model_name:

  Optional name stored in `x$model_name`.

- overwrite:

  Logical. `TRUE` recomputes criteria `x$criteria` already holds.

- file:

  Optional path without extension. The fit is saved to
  `paste0(file, ".rds")` with its criteria.

- force_save:

  Unused. Accepted for the brms signature.

## Value

[`loo_predict()`](https://mc-stan.org/rstantools/reference/loo-prediction.html),
[`loo_epred()`](https://paulbuerkner.com/brms/reference/loo_predict.brmsfit.html)
and
[`loo_linpred()`](https://mc-stan.org/rstantools/reference/loo-prediction.html)
return a matrix with one row per scored observation and a column per
expectation.
[`loo_predictive_interval()`](https://mc-stan.org/rstantools/reference/loo-prediction.html)
returns its two quantile columns.
[`loo_R2()`](https://mc-stan.org/rstantools/reference/bayes_R2.html)
returns a one-row summary, or its draws with `summary = FALSE`.
[`add_criterion()`](https://paulbuerkner.com/brms/reference/add_criterion.html)
returns `x` with the criteria in `x$criteria`.
[`loo_model_weights()`](https://mc-stan.org/loo/reference/loo_model_weights.html)
returns the weights of
[`loo::loo_model_weights()`](https://mc-stan.org/loo/reference/loo_model_weights.html).

## The observations scored

The weights and the predictions describe the observation the likelihood
scored. For most families that is a row of the data. The detection
families
([`occ()`](https://nicholasjclark.github.io/mvgam/dev/reference/occ.md),
[`nmix()`](https://nicholasjclark.github.io/mvgam/dev/reference/nmix.md))
write their likelihood per closure unit, marginalising its visits.
[`loo_predict()`](https://mc-stan.org/rstantools/reference/loo-prediction.html),
[`loo_epred()`](https://paulbuerkner.com/brms/reference/loo_predict.brmsfit.html),
[`loo_predictive_interval()`](https://mc-stan.org/rstantools/reference/loo-prediction.html)
and [`loo_R2()`](https://mc-stan.org/rstantools/reference/bayes_R2.html)
then sum a unit's visits and return one value per unit, in the column
order of
[`log_lik.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/log_lik.mvgam.md).
[`loo_linpred()`](https://mc-stan.org/rstantools/reference/loo-prediction.html)
is refused for them. A sum of linear predictors over visits has no
interpretation.

[`mvn()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvn.md)
and
[`mvt()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvt.md)
keep one density per row. The composition families
([`diri()`](https://nicholasjclark.github.io/mvgam/dev/reference/diri.md),
[`multi()`](https://nicholasjclark.github.io/mvgam/dev/reference/multi.md),
[`categ()`](https://nicholasjclark.github.io/mvgam/dev/reference/categ.md))
hold one joint density per site. These methods refuse them.

## References

Gelman, A., Goodrich, B., Gabry, J. and Vehtari, A. (2019). R-squared
for Bayesian regression models. *The American Statistician*,
73(3):307-309.
[doi:10.1080/00031305.2018.1549100](https://doi.org/10.1080/00031305.2018.1549100)

## See also

[`loo.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/loo.mvgam.md)
estimates the ELPD these weights come from.
[`bayes_R2.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_diagnostics.md)
is the in-sample R^2.
[`lfo_cv.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/lfo_cv.mvgam.md)
scores forecasts of data the model has not seen.

Other model comparison:
[`bridge_sampler.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/bridge_sampler.mvgam.md),
[`compare_elpds()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_elpds.md),
[`kfold.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/kfold.mvgam.md),
[`lfo_cv.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/lfo_cv.mvgam.md),
[`log_lik.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/log_lik.mvgam.md),
[`loo.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/loo.mvgam.md),
[`loo_compare.mvgam_lfo()`](https://nicholasjclark.github.io/mvgam/dev/reference/loo_compare.mvgam_lfo.md),
[`posterior_average.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_average.mvgam.md),
[`waic.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/waic.mvgam.md)

## Author

Nicholas J Clark

## Examples

``` r
# \dontrun{
# A Poisson GAM without a latent trend. PSIS approximates each
# held-out count of such a model well (see Details of ?loo.mvgam)
set.seed(13)
simdat <- sim_mvgam(family = poisson(), n_series = 1L,
                    n_timepoints = 120L, trend_model = "None")
mod <- mvgam(y ~ s(x), data = simdat$data_train,
             family = poisson(), chains = 2, silent = 2)
#> Warning: 3 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
# }
# \dontrun{
# The leave-one-out R^2 is lower than the in-sample one
loo_R2(mod)
#>     Estimate  Est.Error     Q2.5     Q97.5
#> R2 0.5864293 0.06594508 0.437431 0.6934755
bayes_R2(mod)
#>     Estimate  Est.Error      Q2.5     Q97.5
#> R2 0.6295285 0.04993805 0.5201681 0.7112319

# One PSIS object serves every leave-one-out prediction
psis <- loo(mod, save_psis = TRUE)$psis_object
head(loo_predict(mod, psis_object = psis))
#>           mean
#> [1,]  2.726495
#> [2,] 13.067724
#> [3,]  7.096046
#> [4,]  8.603916
#> [5,]  2.913653
#> [6,]  9.077012
head(loo_predictive_interval(mod, psis_object = psis))
#>      q5 q95
#> [1,]  0   6
#> [2,]  7  20
#> [3,]  3  12
#> [4,]  4  14
#> [5,]  0   6
#> [6,]  4  15

# Next, loo_compare() ranks models by their ELPD
# }
```
