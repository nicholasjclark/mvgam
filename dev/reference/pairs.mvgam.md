# Pairs plot of mvgam parameters

Scatters the posterior draws of each pair of parameters, with each
parameter's marginal posterior on the diagonal, as
[`bayesplot::mcmc_pairs()`](https://mc-stan.org/bayesplot/reference/MCMC-scatterplots.html)
draws them. Divergent transitions are marked. A ridge or a funnel
between two parameters shows that the data separate them poorly.

## Usage

``` r
# S3 method for class 'mvgam'
pairs(x, variable = NULL, regex = FALSE, ...)
```

## Arguments

- x:

  A fitted `mvgam` or `jsdgam` object

- variable:

  Names of the variables (parameters) to plot, as given by a character
  vector or a regular expression (if `regex = TRUE`). By default, a
  hopefully not too large selection of variables is plotted.

- regex:

  Logical; Indicates whether `variable` should be treated as regular
  expressions. Defaults to `FALSE`.

- ...:

  Further arguments to
  [`bayesplot::mcmc_pairs()`](https://mc-stan.org/bayesplot/reference/MCMC-scatterplots.html),
  such as `off_diag_args`.

## Value

A `bayesplot_grid` object

## Details

Without `variable`, the plot shows the parameters
[`mcmc_plot.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mcmc_plot.mvgam.md)
shows: the population coefficients, the family parameters, the variance
components and the trend's dynamics and loadings. The per-basis smooth
coefficients (`s_*`) and the group-level deviations (`r_*`) run to
hundreds on a spline or hierarchical fit. They are drawn when `variable`
names them.

## See also

[`mcmc_plot.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mcmc_plot.mvgam.md)
draws the other bayesplot MCMC plots.
[mvgam_draws](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_draws.md)
lists the shortcuts `variable` accepts.

Other posterior summaries:
[`VarCorr.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/VarCorr.mvgam.md),
[`bibtex()`](https://nicholasjclark.github.io/mvgam/dev/reference/bibtex.md),
[`how_to_cite.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/how_to_cite.mvgam.md),
[`hypothesis.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/hypothesis.mvgam.md),
[`index-mvgam`](https://nicholasjclark.github.io/mvgam/dev/reference/index-mvgam.md),
[`mcmc_plot.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mcmc_plot.mvgam.md),
[`methods_md()`](https://nicholasjclark.github.io/mvgam/dev/reference/methods_md.md),
[`mvgam_draws`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_draws.md),
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

# The trend's autoregression against its innovation scale
pairs(mod, variable = "trend_params")


# Next, mcmc_plot(mod, type = "rank_overlay") checks that the chains
# mix
# }
```
