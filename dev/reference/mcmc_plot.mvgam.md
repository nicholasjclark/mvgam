# MCMC plots of mvgam parameters with bayesplot

Draws any bayesplot MCMC plot of the posterior, as
[`brms::mcmc_plot()`](https://paulbuerkner.com/brms/reference/mcmc_plot.brmsfit.html)
does. Without `variable`, the plot shows the parameters
[`pairs.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/pairs.mvgam.md)
shows: the population coefficients, the trend parameters and the family
parameters.

## Usage

``` r
# S3 method for class 'mvgam'
mcmc_plot(object, type = "intervals", variable = NULL, regex = FALSE, ...)
```

## Arguments

- object:

  An R object typically of class `brmsfit`

- type:

  The plot type, the name of a bayesplot `mcmc_*()` function without its
  prefix.
  [`bayesplot::available_mcmc()`](https://mc-stan.org/bayesplot/reference/available_ppc.html)
  lists them. The default `"intervals"` shows posterior intervals.
  `"trace"` and `"rank_overlay"` check mixing. `"nuts_energy"` and the
  other `"nuts_*"` types diagnose the sampler.

- variable:

  Names of the variables (parameters) to plot, as given by a character
  vector or a regular expression (if `regex = TRUE`). By default, a
  hopefully not too large selection of variables is plotted.

- regex:

  Logical; Indicates whether `variable` should be treated as regular
  expressions. Defaults to `FALSE`.

- ...:

  Further arguments to the bayesplot function, such as `prob` for
  `"intervals"` or `true` for the `"recover_*"` types.

## Value

A `ggplot` object, or a `bayesplot_grid` for `"pairs"` and `"combo"`.

## See also

[mvgam_draws](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_draws.md)
lists the shortcuts `variable` accepts, such as `"trend_params"`.
[`pairs.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/pairs.mvgam.md)
plots the joint posterior. See the [bayesplot MCMC
overview](https://mc-stan.org/bayesplot/reference/MCMC-overview.html).

Other posterior summaries:
[`VarCorr.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/VarCorr.mvgam.md),
[`bibtex()`](https://nicholasjclark.github.io/mvgam/dev/reference/bibtex.md),
[`how_to_cite.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/how_to_cite.mvgam.md),
[`hypothesis.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/hypothesis.mvgam.md),
[`index-mvgam`](https://nicholasjclark.github.io/mvgam/dev/reference/index-mvgam.md),
[`methods_md()`](https://nicholasjclark.github.io/mvgam/dev/reference/methods_md.md),
[`mvgam_draws`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_draws.md),
[`pairs.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/pairs.mvgam.md),
[`posterior_interval.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_interval.mvgam.md),
[`ranef.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/ranef.mvgam.md),
[`smooths.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/smooths.mvgam.md),
[`summary.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/summary.mvgam.md)

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

# Posterior intervals of the trend parameters
mcmc_plot(mod, variable = "trend_params", type = "intervals")


# Rank histograms of the same parameters check that the chains mix
mcmc_plot(mod, variable = "trend_params", type = "rank_overlay")


# Next, pairs() shows how the parameters covary
# }
```
