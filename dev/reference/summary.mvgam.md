# Summary of mvgam model fits

Provides posterior summary statistics for mvgam model parameters,
organised by category (fixed effects, smooth terms, family parameters,
trend parameters and factor loadings).

## Usage

``` r
# S3 method for class 'mvgam'
summary(object, probs = c(0.025, 0.975), robust = FALSE, matrices = FALSE, ...)
```

## Arguments

- object:

  An object of class `mvgam`.

- probs:

  Numeric vector of probabilities for quantile computation. Default is
  `c(0.025, 0.975)` for 95% credible intervals. Can be a single value or
  multiple values.

- robust:

  Logical. If `FALSE` (the default), uses mean and standard deviation
  for summaries. If `TRUE`, uses median and median absolute deviation
  (MAD).

- matrices:

  Logical; if `TRUE`, also print every cell of the matrices a
  correlated, VAR or hierarchical trend derives from its sampled
  parameters (`Phi_trend`, `Sigma_trend`, `Omega_trend`,
  `Phi_group_trend`, `Sigma_group_trend`). The default `FALSE` prints
  the sampled parameters alone, among them `sigma_trend`, `Amu_trend`,
  `Aomega_trend` and `alpha_cor_trend`.
  [`posterior_transition_matrix()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_transition_matrix.md)
  and
  [`residual_cor()`](https://nicholasjclark.github.io/mvgam/dev/reference/residual_cor.md)
  summarise the matrices a cell at a time.

- ...:

  Unused. Anything passed here is refused.

## Value

An object of class `mvgam_summary`. Each block is a data frame of the
columns below, one row per parameter, named as
[`print()`](https://rdrr.io/r/base/print.html) shows it. A block is
absent when the model has no such parameter. The observation model's
blocks are:

- `smooth`:

  Smoothing standard deviations, one row per penalty of each smooth. The
  basis coefficients are available by name from
  `as_draws(object, variable = "^s_", regex = TRUE)`.

- `gp`:

  Gaussian-process marginal standard deviations and length-scales.

- `random`:

  Group-level standard deviations and correlations, as a list of tables
  named by grouping factor. `ngrps` gives each factor's number of
  levels. [`ranef()`](https://rdrr.io/pkg/nlme/man/random.effects.html)
  gives the per-level effects.

- `fixed`:

  Regression coefficients, those of every distributional parameter given
  a formula among them.

- `mo`:

  The simplex of each monotonic effect.

- `spec`:

  Family parameters without a formula of their own, such as `sigma` or
  `shape`.

The trend model's blocks take the same names with the prefix `trend_`,
joined by `trend_spec` for the trend's dynamics, `loadings` for the
factor loadings and `loadings_prior` for the hyperparameters of their
prior. The formula, family, draw counts and data dimensions are stored
with them.

## Details

Each block has the columns:

- `Estimate`:

  Posterior mean, or median with `robust = TRUE`.

- `Est.Error`:

  Posterior standard deviation, or MAD with `robust = TRUE`.

- `l-95% CI`, `u-95% CI`:

  Credible interval bounds at `probs`, labelled by its coverage.

- `Rhat`:

  Potential scale reduction factor on split chains, 1 at convergence.

- `Bulk_ESS`, `Tail_ESS`:

  Bulk and tail effective sample sizes.

## See also

[`how_to_cite()`](https://nicholasjclark.github.io/mvgam/dev/reference/how_to_cite.mvgam.md)
drafts a methods paragraph for the model.
[`methods_md()`](https://nicholasjclark.github.io/mvgam/dev/reference/methods_md.md)
writes its equations, glossary and priors.
[`mcmc_plot.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mcmc_plot.mvgam.md)
plots the draws of these parameters.

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
[`posterior_interval.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_interval.mvgam.md),
[`ranef.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/ranef.mvgam.md),
[`smooths.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/smooths.mvgam.md)

## Examples

``` r
# \dontrun{
set.seed(13)
simdat <- sim_mvgam(family = poisson(), n_series = 1L,
                     n_timepoints = 120L, trend_model = AR())
mod <- mvgam(y ~ s(x), trend_formula = ~ AR(p = 1),
              data = simdat$data_train,
              family = poisson(),
              chains = 2, silent = 2)
#> Warning: 9 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.

summary(mod)
#>  Family: poisson 
#>   Links: mu = log 
#> Formula: y ~ s(x) 
#>    Data: simdat$data_train (Number of observations: 90) 
#>  Trends: AR(1) 
#>   Draws: 2 chains, each with iter = 2000; warmup = 1000; thin = 1; 
#>          total post-warmup draws = 2000
#> 
#> == Observation Model ==
#> Smoothing Spline Hyperparameters:
#>           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sds(sx_1)     4.32      1.65     1.93     8.28 1.00      583      999
#> 
#> Regression Coefficients:
#>           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept     1.63      0.14     1.36     1.92 1.01      791      571
#> sx_1          7.70      4.92    -1.81    17.73 1.00      785     1102
#> 
#> == Trend Model ==
#> Trend Specific Parameters:
#>                       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
#> sigma_trend[series_1]     0.44      0.07     0.30     0.59 1.00      575
#> ar1_trend[series_1]       0.59      0.15     0.28     0.85 1.01      407
#>                       Tail_ESS
#> sigma_trend[series_1]     1100
#> ar1_trend[series_1]        955
#> 
#> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).
#> 
#> Next steps:
#>   - `pp_check(fit)`: posterior predictive checks
#>   - `forecast(fit, newdata = ...)`: out-of-sample forecasts
#>   - `lfo_cv(fit)`: leave-future-out model comparison
#>   - `conditional_effects(fit)`: covariate effects
#> Use `how_to_cite(fit)` for a methods paragraph and `methods_md(fit)` for its equations.

# Each block is a data frame, such as the trend's dynamics
round(summary(mod)$trend_spec, 2)
#>                       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
#> sigma_trend[series_1]     0.44      0.07     0.30     0.59 1.00   575.18
#> ar1_trend[series_1]       0.59      0.15     0.28     0.85 1.01   407.26
#>                       Tail_ESS
#> sigma_trend[series_1]  1099.50
#> ar1_trend[series_1]     954.86

# Next: how_to_cite() drafts the methods paragraph
# }
```
