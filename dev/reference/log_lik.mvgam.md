# Pointwise log-likelihoods from a fitted mvgam model

The log density of each observation under each posterior draw, as
[`brms::log_lik.brmsfit()`](https://paulbuerkner.com/brms/reference/log_lik.brmsfit.html)
returns it.
[`loo.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/loo.mvgam.md),
[`waic.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/waic.mvgam.md),
[`lfo_cv.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/lfo_cv.mvgam.md)
and
[`kfold.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/kfold.mvgam.md)
score models from it.

## Usage

``` r
# S3 method for class 'mvgam'
log_lik(
  object,
  newdata = NULL,
  re_formula = NULL,
  resp = NULL,
  ndraws = NULL,
  draw_ids = NULL,
  incl_autocor = TRUE,
  process_error = NULL,
  allow_new_levels = FALSE,
  sample_new_levels = "uncertainty",
  ...
)
```

## Arguments

- object:

  A fitted `mvgam` object.

- newdata:

  Optional data frame holding the response and every covariate of the
  model. `NULL` (the default) takes the training data.

- re_formula:

  Group-level terms to include: `NULL` (the default) for every one, `NA`
  for none. A formula choosing some of them is not supported.

- resp:

  Optional name of one response of a multivariate model.

- ndraws:

  Positive integer specifying number of posterior draws to use. NULL
  (default) uses all available draws.

- draw_ids:

  Optional integer vector selecting a subset of posterior draw indices
  to use. NULL (default) uses all draws (subject to `ndraws`).

- incl_autocor:

  Logical. `TRUE` (the default) evaluates each density under the latent
  trend state the model inferred at that time, the state
  [`hindcast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.mvgam.md)
  returns. The ELPD then describes the series that was observed. `FALSE`
  keeps only the trend's deterministic terms and leaves the
  autocorrelation out.

- process_error:

  Deprecated alias for `incl_autocor`.

- allow_new_levels:

  Logical. `TRUE` scores rows whose grouping level the training data
  lacked, as
  [`kfold.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/kfold.mvgam.md)
  needs when a fold holds out whole groups. Defaults to `FALSE`.

- sample_new_levels:

  How a new level's coefficients are drawn: `"uncertainty"` (the
  default), `"gaussian"` or `"old_levels"`.

- ...:

  Unused. Anything passed here is refused.

## Value

A numeric `[ndraws, nobs]` matrix of log densities. A missing response
gives a column of `NA`. For a multivariate fit with `resp = NULL`, each
column sums the densities of the responses measured in that row, as brms
does. An occupancy, N-mixture or composition family has one density per
site, carried on the site's first row.

## Details

Each family uses the brms parameterisation. For example,
[`negbinomial()`](https://paulbuerkner.com/brms/reference/brmsfamily.html)
takes a mean and shape,
[`Beta()`](https://paulbuerkner.com/brms/reference/brmsfamily.html) a
mean and precision. Hurdle and zero-inflated families mix a mass at zero
with their base family. The
[`weights()`](https://rdrr.io/r/stats/weights.html), `cens()` and
[`trunc()`](https://rdrr.io/r/base/Round.html) addition terms change
each row's contribution as they do in the Stan model.

The
[`posterior_epred.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_epred.mvgam.md)
family of methods integrates over the trend dynamics. A covariate effect
then has the same meaning at every time. Those predictions describe no
particular observed series and do not give an ELPD.

## See also

[`loo.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/loo.mvgam.md)
and
[`waic.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/waic.mvgam.md)
estimate the ELPD from this matrix.
[`hindcast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.mvgam.md)
returns the latent states it conditions on. The [forecast evaluation
article](https://nicholasjclark.github.io/mvgam/articles/forecast_evaluation.html)
compares models by cross-validation.

Other model comparison:
[`bridge_sampler.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/bridge_sampler.mvgam.md),
[`compare_elpds()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_elpds.md),
[`kfold.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/kfold.mvgam.md),
[`lfo_cv.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/lfo_cv.mvgam.md),
[`loo.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/loo.mvgam.md),
[`loo_compare.mvgam_lfo()`](https://nicholasjclark.github.io/mvgam/dev/reference/loo_compare.mvgam_lfo.md),
[`mvgam_loo_extras`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_loo_extras.md),
[`posterior_average.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_average.mvgam.md),
[`waic.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/waic.mvgam.md)

## Examples

``` r
# \dontrun{
set.seed(13)
simdat <- sim_mvgam(family = poisson(), n_series = 1L,
                     n_timepoints = 120L, trend_model = AR())

mod <- mvgam(y ~ s(x),
              trend_formula = ~ AR(p = 1),
              data    = simdat$data_train,
              family  = poisson(),
              chains  = 2, silent = 2)
#> Warning: 9 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.

# One row per draw, one column per observation
ll <- log_lik(mod)
dim(ll)
#> [1] 2000   90

# Without the latent trend, the densities are far lower. The trend
# explains most of the series
ll_fixed <- log_lik(mod, incl_autocor = FALSE)
c(with_trend = mean(rowSums(ll)), without = mean(rowSums(ll_fixed)))
#> with_trend    without 
#>  -196.7973  -289.2782 

# Next, loo() estimates the ELPD from these densities
# }
```
