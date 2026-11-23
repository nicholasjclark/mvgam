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

  Either a keyword shortcut, a character vector of parameter names, or
  (when `regex = TRUE`) a vector of regular expressions. Recognised
  keywords:

  - `"betas"`: population-level coefficients, the set
    [`brms::fixef()`](https://paulbuerkner.com/brms/reference/fixef.brmsfit.html)
    reports (`b_*`, `bs_*`, `bsp_*` and `bcs_*`).

  - `"obs_params"`: observation-family distributional parameters
    (`sigma`, `phi`, `shape`, ...).

  - `"smooth_params"`: smooth-term standard deviations (`sds_*`).

  - `"trend_betas"`: the trend formula's population-level coefficients
    (`b_*_trend`, `bs_*_trend` and `bsp_*_trend`).

  - `"trend_params"`: trend-dynamics parameters (autoregressive
    coefficients, innovation SDs, ...).

  - `"trend_smooth_params"`: trend-side smooth SDs.

  `NULL` (the default) returns every parameter. A bare vector name, such
  as `"sigma_trend"`, selects every element. Group-level effects, factor
  loadings and latent states have no keyword. Select them by name.
  [`variables.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/index-mvgam.md)
  lists the names a fit holds.

- regex:

  Logical. When `TRUE`, each entry of `variable` is treated as a regular
  expression and any parameter matching at least one pattern is
  selected. Keyword shortcuts are still honoured. Defaults to `FALSE`.

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
simdat <- sim_mvgam(family = gaussian(), n_series = 1L,
                     n_timepoints = 120L, trend_model = AR(),
                     prop_trend = 0.5)
mod <- mvgam(y ~ s(x, k = 5), trend_formula = ~ AR(p = 1),
              data    = simdat$data_train,
              family  = gaussian(),
              chains  = 2, silent = 2)
#> Warning: 3 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
#> Warning: 2 of 2 chains had an E-BFMI less than 0.3.
#> See https://mc-stan.org/misc/warnings for details.

# A Gaussian response adds white noise to the latent AR(1) process.
# The data split the total variance between the observation noise
# `sigma` and the process noise `sigma_trend` through the
# autocorrelation alone. A larger `sigma` leaves less variance for
# `sigma_trend`. The pair then forms a negative ridge. Divergences
# gather where `sigma` nears zero.
pairs(mod, variable = c("obs_params", "trend_params"))


# Next, mcmc_plot(mod, type = "rank_overlay") checks that the chains
# mix
# }
```
