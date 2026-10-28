# Posterior credible intervals on a fitted mvgam model

Central quantile intervals of the posterior draws of parameters. The
parameters are chosen as in
[`as.matrix.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_draws.md).

## Usage

``` r
# S3 method for class 'mvgam'
posterior_interval(object, pars = NULL, variable = NULL, prob = 0.95, ...)
```

## Arguments

- object:

  A fitted `mvgam` object.

- pars:

  Deprecated alias for `variable`.

- variable:

  Either a keyword shortcut, a character vector of parameter names, or
  (when `regex = TRUE`) a vector of regular expressions. Recognised
  keywords:

  - `"betas"`: population-level coefficients, the set
    [`brms::fixef()`](https://paulbuerkner.com/brms/reference/fixef.brmsfit.html)
    reports (`b_*`, `bs_*` and `bsp_*`).

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

- prob:

  Numeric in `(0, 1)`. The probability mass each interval holds.
  Defaults to `0.95`.

- ...:

  Further arguments of
  [`as.matrix.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_draws.md),
  such as `regex`.

## Value

A numeric matrix with one row per parameter and two columns, the lower
and upper bounds.

## See also

[`predictive_interval.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/predictive_interval.mvgam.md)
gives intervals of predictive draws.
[`summary.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/summary.mvgam.md)
reports intervals with convergence diagnostics.

Other posterior summaries:
[`VarCorr.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/VarCorr.mvgam.md),
[`bibtex()`](https://nicholasjclark.github.io/mvgam/dev/reference/bibtex.md),
[`how_to_cite.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/how_to_cite.mvgam.md),
[`hypothesis.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/hypothesis.mvgam.md),
[`index-mvgam`](https://nicholasjclark.github.io/mvgam/dev/reference/index-mvgam.md),
[`mcmc_plot.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mcmc_plot.mvgam.md),
[`methods_md()`](https://nicholasjclark.github.io/mvgam/dev/reference/methods_md.md),
[`mvgam_draws`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_draws.md),
[`pairs.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/pairs.mvgam.md),
[`ranef.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/ranef.mvgam.md),
[`smooths.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/smooths.mvgam.md),
[`summary.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/summary.mvgam.md)

## Author

Nicholas J Clark

## Examples

``` r
# \dontrun{
set.seed(13)
simdat <- sim_mvgam(family = poisson(), n_series = 1L,
                    n_timepoints = 120L, trend_model = AR())
mod <- mvgam(y ~ s(x), trend_formula = ~ AR(p = 1),
             data = simdat$data_train, family = poisson(),
             chains = 2, silent = 2)
#> Warning: 9 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.

# 90 percent intervals of the autoregression and its innovation SD
posterior_interval(mod, variable = c("ar1_trend", "sigma_trend"),
                   prob = 0.9)
#>                              5%       95%
#> ar1_trend[series_1]   0.3443696 0.8156445
#> sigma_trend[series_1] 0.3228598 0.5584980

# Next: summary() reports these with convergence diagnostics
# }
```
