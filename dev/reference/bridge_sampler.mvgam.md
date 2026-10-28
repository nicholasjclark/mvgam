# Bridge sampling and Bayes factors for `mvgam` fits

Estimates the log marginal likelihood of a fit by bridge sampling
(Gronau, Singmann and Wagenmakers 2020) and compares two fits by their
Bayes factor. The marginal likelihood weighs how well a model explains
the data under its prior.
[`loo.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/loo.mvgam.md)
and
[`lfo_cv.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/lfo_cv.mvgam.md)
score predictions of held-out data instead. A Bayes factor sets no
forecast horizon, which makes it a complement to leave-future-out scores
for state-space models. It also depends on the priors, which predictive
scores barely do.

## Usage

``` r
# S3 method for class 'mvgam'
bridge_sampler(samples, recompile = FALSE, ...)

# S3 method for class 'mvgam'
bayes_factor(x1, x2, log = FALSE, ...)
```

## Arguments

- samples:

  An `mvgam` fit. The name follows the bridgesampling generic.

- recompile:

  Logical. Bridge sampling evaluates the density through a compiled
  rstan model. A fit without one, sampled by cmdstanr or loaded with
  [`readRDS()`](https://rdrr.io/r/base/readRDS.html) in a new session,
  has one compiled, with its draws kept. `TRUE` compiles one for any
  fit. Defaults to `FALSE`.

- ...:

  Arguments of
  [`bridgesampling::bridge_sampler()`](https://rdrr.io/pkg/bridgesampling/man/bridge_sampler.html),
  such as `method`, `repetitions` or `cores`.

- x1, x2:

  `mvgam` fits, or `bridge` objects returned by
  [`bridge_sampler()`](https://rdrr.io/pkg/bridgesampling/man/bridge_sampler.html),
  to compare.

- log:

  Logical. `TRUE` returns the log Bayes factor.

## Value

[`bridge_sampler()`](https://rdrr.io/pkg/bridgesampling/man/bridge_sampler.html)
returns a `bridge` object holding the log marginal likelihood in
`logml`.
[`bayes_factor()`](https://rdrr.io/pkg/bridgesampling/man/bf.html)
returns a `bayes_factor` object.

## Details

The Stan program must keep the normalising constants of its densities.
mvgam builds it that way by default. A fit built with
`normalize = FALSE` is refused. Bridge sampling also needs the draws of
every parameter, which a fit keeps under
`save_pars = save_pars(all = TRUE)`.

## Multivariate fits

The Stan program of an `mvbf()` fit has one joint log posterior over
every response.
[`bridge_sampler()`](https://rdrr.io/pkg/bridgesampling/man/bridge_sampler.html)
then estimates the joint log marginal likelihood \\\log p(y\_{1},
\ldots, y\_{K} \mid \mathrm{model})\\. The variance of the estimate
grows with the number of parameters. For multi-response, factor or VAR
fits, pass `repetitions` above 1 or `method = "warp3"` when the default
method reports a high `re2` (relative squared error).

## References

Gronau QF, Singmann H and Wagenmakers E-J (2020). bridgesampling: An R
package for estimating normalizing constants. *Journal of Statistical
Software*. 92(10).

## See also

[`loo.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/loo.mvgam.md),
[`lfo_cv.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/lfo_cv.mvgam.md).
The [forecast evaluation
article](https://nicholasjclark.github.io/mvgam/articles/forecast_evaluation.html)
compares models by their forecasts.

Other model comparison:
[`compare_elpds()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_elpds.md),
[`kfold.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/kfold.mvgam.md),
[`lfo_cv.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/lfo_cv.mvgam.md),
[`log_lik.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/log_lik.mvgam.md),
[`loo.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/loo.mvgam.md),
[`loo_compare.mvgam_lfo()`](https://nicholasjclark.github.io/mvgam/dev/reference/loo_compare.mvgam_lfo.md),
[`mvgam_loo_extras`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_loo_extras.md),
[`posterior_average.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_average.mvgam.md),
[`waic.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/waic.mvgam.md)

## Author

Nicholas J Clark

## Examples

``` r
# \dontrun{
set.seed(1)
simdat <- sim_mvgam(family = poisson(), n_series = 1L,
                    n_timepoints = 80L, trend_model = AR())

# Bridge sampling needs many draws and every parameter's draws. It
# evaluates the density through rstan. A fit sampled by rstan needs
# no second compile.
mod_ar <- mvgam(y ~ s(x, k = 5), trend_formula = ~ AR(p = 1),
                data = simdat$data_train, family = poisson(),
                save_pars = save_pars(all = TRUE), backend = "rstan",
                chains = 2, iter = 3000, silent = 2)
#> Warning: There were 18 divergent transitions after warmup. See
#> https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#> to find out why this is a problem and how to eliminate them.
#> Warning: Examine the pairs() plot to diagnose sampling problems
mod_rw <- update(mod_ar, trend_formula = ~ RW())
#> Warning: There were 2 divergent transitions after warmup. See
#> https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#> to find out why this is a problem and how to eliminate them.
#> Warning: Examine the pairs() plot to diagnose sampling problems

# The log marginal likelihood of each model
bs_ar <- bridge_sampler(mod_ar, silent = TRUE)
bs_rw <- bridge_sampler(mod_rw, silent = TRUE)
bs_ar
#> Bridge sampling estimate of the log marginal likelihood: -166.114
#> Estimate obtained in 9 iteration(s) via method "normal".

# A Bayes factor above 1 favours the AR(1) trend. Passing the
# bridge objects reuses their estimates.
bayes_factor(bs_ar, bs_rw)
#> Estimated Bayes factor in favor of x1 over x2: 8.97178

# Next, compare their forecasts with loo() or lfo_cv()
# }
```
