# Posterior summaries and sampler diagnostics of mvgam models

The brms and bayesplot accessors for a fitted model.
[`fixef()`](https://rdrr.io/pkg/nlme/man/fixed.effects.html),
[`coef()`](https://rdrr.io/r/stats/coef.html) and
[`vcov()`](https://rdrr.io/r/stats/vcov.html) summarise the
population-level coefficients of the observation formula, or of the
trend formula with `trend_effects = TRUE`.
[`posterior_summary()`](https://paulbuerkner.com/brms/reference/posterior_summary.html)
summarises any parameters.
[`rhat()`](https://mc-stan.org/posterior/reference/rhat.html) and
[`neff_ratio()`](https://mc-stan.org/bayesplot/reference/bayesplot-extractors.html)
report convergence.
[`nuts_params()`](https://mc-stan.org/bayesplot/reference/bayesplot-extractors.html)
and
[`log_posterior()`](https://mc-stan.org/bayesplot/reference/bayesplot-extractors.html)
return the sampler's own quantities.
[`bayes_R2()`](https://mc-stan.org/rstantools/reference/bayes_R2.html)
is the Bayesian R^2 of Gelman et al. (2019).
[`ndraws()`](https://mc-stan.org/posterior/reference/draws-index.html),
[`nchains()`](https://mc-stan.org/posterior/reference/draws-index.html),
[`niterations()`](https://mc-stan.org/posterior/reference/draws-index.html)
and
[`nvariables()`](https://mc-stan.org/posterior/reference/variables.html)
count the posterior.

## Usage

``` r
# S3 method for class 'mvgam'
coef(object, summary = TRUE, trend_effects = FALSE, ...)

# S3 method for class 'mvgam'
nuts_params(object, pars = NULL, ...)

# S3 method for class 'mvgam'
log_posterior(object, ...)

# S3 method for class 'mvgam'
rhat(x, pars = NULL, ...)

# S3 method for class 'mvgam'
neff_ratio(object, pars = NULL, ...)

# S3 method for class 'mvgam'
fixef(
  object,
  summary = TRUE,
  robust = FALSE,
  probs = c(0.025, 0.975),
  pars = NULL,
  trend_effects = FALSE,
  ...
)

# S3 method for class 'mvgam'
bayes_R2(
  object,
  resp = NULL,
  summary = TRUE,
  robust = FALSE,
  probs = c(0.025, 0.975),
  ...
)

# S3 method for class 'mvgam'
prior_summary(object, ...)

# S3 method for class 'mvgam'
ndraws(x, ...)

# S3 method for class 'mvgam'
nchains(x, ...)

# S3 method for class 'mvgam'
niterations(x, ...)

# S3 method for class 'mvgam'
nvariables(x, ...)

# S3 method for class 'mvgam'
posterior_summary(
  x,
  pars = NULL,
  variable = NULL,
  probs = c(0.025, 0.975),
  robust = FALSE,
  ...
)

# S3 method for class 'mvgam'
getCall(x, ...)

# S3 method for class 'mvgam'
vcov(object, correlation = FALSE, pars = NULL, trend_effects = FALSE, ...)
```

## Arguments

- object, x:

  A fitted `mvgam` object.

- summary:

  Logical. `TRUE` (the default) returns a summary. `FALSE` returns the
  draws. [`coef()`](https://rdrr.io/r/stats/coef.html) summarises to
  posterior means.

- trend_effects:

  Logical. [`coef()`](https://rdrr.io/r/stats/coef.html),
  [`fixef()`](https://rdrr.io/pkg/nlme/man/fixed.effects.html) and
  [`vcov()`](https://rdrr.io/r/stats/vcov.html) report the coefficients
  of the observation formula by default. `TRUE` reports those of the
  `trend_formula` instead. Their names end in `_trend`.

- ...:

  Further arguments.
  [`nuts_params()`](https://mc-stan.org/bayesplot/reference/bayesplot-extractors.html)
  and
  [`log_posterior()`](https://mc-stan.org/bayesplot/reference/bayesplot-extractors.html)
  pass them to bayesplot.
  [`bayes_R2()`](https://mc-stan.org/rstantools/reference/bayes_R2.html)
  passes them to
  [`posterior_epred.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_epred.mvgam.md).
  The other methods refuse them.

- pars:

  Optional character vector of parameter names, or of sampler parameters
  such as `"divergent__"` for
  [`nuts_params()`](https://mc-stan.org/bayesplot/reference/bayesplot-extractors.html).
  `NULL` takes all of them.

- robust:

  Logical. `TRUE` summarises by the median and the median absolute
  deviation. Defaults to `FALSE`, the mean and the standard deviation.

- probs:

  Numeric vector of the two interval probabilities, `c(0.025, 0.975)` by
  default.

- resp:

  Optional name of one response of a multivariate model.

- variable:

  Parameter names for
  [`posterior_summary()`](https://paulbuerkner.com/brms/reference/posterior_summary.html),
  the brms name of the argument. There `pars` is its deprecated alias.

- correlation:

  Logical. `TRUE` returns the correlation matrix of the coefficients.
  Defaults to `FALSE`, the covariance matrix.

## Value

[`fixef()`](https://rdrr.io/pkg/nlme/man/fixed.effects.html),
[`posterior_summary()`](https://paulbuerkner.com/brms/reference/posterior_summary.html)
and
[`bayes_R2()`](https://mc-stan.org/rstantools/reference/bayes_R2.html)
return a matrix with columns `Estimate`, `Est.Error` and one per
quantile, or the draws with `summary = FALSE`.
[`coef()`](https://rdrr.io/r/stats/coef.html) returns the posterior
means. [`vcov()`](https://rdrr.io/r/stats/vcov.html) returns the
covariance matrix of the coefficients.
[`rhat()`](https://mc-stan.org/posterior/reference/rhat.html) and
[`neff_ratio()`](https://mc-stan.org/bayesplot/reference/bayesplot-extractors.html)
return a named vector with one value per parameter.
[`nuts_params()`](https://mc-stan.org/bayesplot/reference/bayesplot-extractors.html)
and
[`log_posterior()`](https://mc-stan.org/bayesplot/reference/bayesplot-extractors.html)
return the long data frames of bayesplot. The counting methods return an
integer.

## References

Vehtari, A., Gelman, A., Simpson, D., Carpenter, B. and Burkner, P.-C.
(2021). Rank-normalization, folding, and localization: An improved Rhat
for assessing convergence of MCMC. *Bayesian Analysis*, 16(2):667-718.
[doi:10.1214/20-BA1221](https://doi.org/10.1214/20-BA1221)

Gelman, A., Goodrich, B., Gabry, J. and Vehtari, A. (2019). R-squared
for Bayesian regression models. *The American Statistician*,
73(3):307-309.
[doi:10.1080/00031305.2018.1549100](https://doi.org/10.1080/00031305.2018.1549100)

## See also

[`summary.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/summary.mvgam.md)
prints these summaries for every parameter.
[`mcmc_plot.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mcmc_plot.mvgam.md)
plots the draws.
[mvgam_draws](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_draws.md)
extracts them.
[`loo_R2.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_loo_extras.md)
is the leave-one-out R^2.

Other model checking:
[`plot.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam.md),
[`pp_check.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/pp_check.mvgam.md),
[`residuals.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/residuals.mvgam.md)

## Author

Nicholas J Clark

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

# The population coefficients with 95% intervals
fixef(mod)
#>           Estimate Est.Error      Q2.5     Q97.5
#> Intercept 1.631420 0.1431782  1.358022  1.920492
#> sx_1      7.695626 4.9158900 -1.807508 17.733728

# Rhat near 1 and effective sample ratios above 0.1 indicate mixing
rhat(mod, pars = c("ar1_trend", "sigma_trend"))
#>   ar1_trend[series_1] sigma_trend[series_1] 
#>              1.006388              1.002338 
neff_ratio(mod, pars = c("ar1_trend", "sigma_trend"))
#>   ar1_trend[series_1] sigma_trend[series_1] 
#>             0.2036302             0.2875883 

# The share of variance in the counts the model explains
bayes_R2(mod)
#>     Estimate Est.Error     Q2.5     Q97.5
#> R2 0.8345563 0.0372526 0.749718 0.8951963

# Next, mcmc_plot() draws the posterior of these parameters
# }
```
