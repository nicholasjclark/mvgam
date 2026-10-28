# Extract posterior draws from fitted mvgam objects

Extract posterior draws as a data frame, matrix or array, or as one of
the `posterior::draws_*` classes. Parameters take their brms names, such
as `b_x` and `sd_grp__Intercept`. A trend parameter indexed by series
takes the series name, as in `sigma_trend[series_1]`. The [integrated
data model
article](https://nicholasjclark.github.io/mvgam/dev/articles/idm.html)
contrasts intercepts drawn with
[`as_draws_df()`](https://mc-stan.org/posterior/reference/draws_df.html).

## Usage

``` r
# S3 method for class 'mvgam'
as.data.frame(
  x,
  row.names = NULL,
  optional = TRUE,
  variable = NULL,
  draw = NULL,
  regex = FALSE,
  ...
)

# S3 method for class 'mvgam'
as.matrix(x, variable = NULL, draw = NULL, regex = FALSE, ...)

# S3 method for class 'mvgam'
as.array(x, variable = NULL, draw = NULL, regex = FALSE, ...)

# S3 method for class 'mvgam'
as_draws(x, variable = NULL, regex = FALSE, inc_warmup = FALSE, ...)

# S3 method for class 'mvgam'
as_draws_matrix(x, variable = NULL, regex = FALSE, inc_warmup = FALSE, ...)

# S3 method for class 'mvgam'
as_draws_df(x, variable = NULL, regex = FALSE, inc_warmup = FALSE, ...)

# S3 method for class 'mvgam'
as_draws_array(x, variable = NULL, regex = FALSE, inc_warmup = FALSE, ...)

# S3 method for class 'mvgam'
as_draws_list(x, variable = NULL, regex = FALSE, inc_warmup = FALSE, ...)

# S3 method for class 'mvgam'
as_draws_rvars(x, variable = NULL, regex = FALSE, inc_warmup = FALSE, ...)
```

## Arguments

- x:

  A fitted `mvgam` object.

- row.names, optional:

  Ignored.

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

- draw:

  Optional positive integer indices of the draws to keep, counted across
  chains. `NULL` (the default) keeps every draw.

- regex:

  Logical. When `TRUE`, each entry of `variable` is treated as a regular
  expression and any parameter matching at least one pattern is
  selected. Keyword shortcuts are still honoured. Defaults to `FALSE`.

- ...:

  Unused. Anything passed here is refused.

- inc_warmup:

  Logical. Include warmup draws? Defaults to `FALSE`. A fit holds them
  only when sampled with `save_warmup = TRUE`.

## Value

A `data.frame`, `matrix`, `array`, or one of the `posterior::draws_*`
classes (`draws_matrix`, `draws_array`, `draws_df`, `draws_list`,
`draws_rvars`) depending on the method dispatched.

## See also

[mvgam_diagnostics](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_diagnostics.md)
for higher-level summaries derived from these draws (`rhat`,
`neff_ratio`, `fixef`, `bayes_R2`, `posterior_summary`, ...),
[`variables.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/index-mvgam.md)
to list every parameter name in the fit,
[`mcmc_plot.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mcmc_plot.mvgam.md)
for diagnostic visualisations,
[`summary.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/summary.mvgam.md)
for a printed model summary,
[`tidy.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/tidy.mvgam.md)
for a tibble of parameter summaries,
[`posterior_epred.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_epred.mvgam.md),
[`posterior_linpred.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_linpred.mvgam.md),
[`posterior_predict.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_predict.mvgam.md)
for prediction-scale outputs,
[posterior::draws](https://mc-stan.org/posterior/reference/draws.html)
for the underlying draws classes.

Other posterior summaries:
[`VarCorr.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/VarCorr.mvgam.md),
[`bibtex()`](https://nicholasjclark.github.io/mvgam/dev/reference/bibtex.md),
[`how_to_cite.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/how_to_cite.mvgam.md),
[`hypothesis.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/hypothesis.mvgam.md),
[`index-mvgam`](https://nicholasjclark.github.io/mvgam/dev/reference/index-mvgam.md),
[`mcmc_plot.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mcmc_plot.mvgam.md),
[`methods_md()`](https://nicholasjclark.github.io/mvgam/dev/reference/methods_md.md),
[`pairs.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/pairs.mvgam.md),
[`posterior_interval.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_interval.mvgam.md),
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
mod <- mvgam(y ~ s(x, k = 5), trend_formula = ~ AR(p = 1),
              data    = simdat$data_train,
              family  = poisson(),
              chains  = 2, silent = 2)
#> Warning: 5 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.

# Trend-dynamics parameters as a data frame of draws
head(as.data.frame(mod, variable = "trend_params"))
#>   sigma_trend[series_1] ar1_trend[series_1] .chain .iteration .draw
#> 1             0.4747979           0.6952397      1          1     1
#> 2             0.4098684           0.7876823      1          2     2
#> 3             0.4974144           0.6125546      1          3     3
#> 4             0.4301547           0.6917517      1          4     4
#> 5             0.4835361           0.3383189      1          5     5
#> 6             0.4086803           0.5631561      1          6     6

# Regular expressions select by pattern, here the smooth's basis
# coefficients and its smoothing SD
colnames(as.matrix(mod, variable = c("^s_", "^sds_"), regex = TRUE))
#> [1] "s_sx_1[1]" "s_sx_1[2]" "s_sx_1[3]" "sds_sx_1" 

# One random variable per parameter, printed as mean and SD
as_draws_rvars(mod, variable = "trend_params")
#> # A draws_rvars: 1000 iterations, 2 chains, and 2 variables
#> $sigma_trend: rvar<1000,2>[1] mean ± sd:
#>      series_1 
#> 0.47 ± 0.072  
#> 
#> $ar1_trend: rvar<1000,2>[1] mean ± sd:
#>     series_1 
#> 0.56 ± 0.14  
#> 

# Next, mcmc_plot(mod) plots the same draws
# }
```
