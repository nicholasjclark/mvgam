# K-fold cross-validation for `mvgam` fits

Measures how well a model predicts groups of data it was not fitted to,
such as sites or species. Each fold of groups is held out in turn and
scored by its expected log predictive density (ELPD). Higher is better.
A fold that importance sampling approximates well takes the PSIS
estimate of [`loo::loo()`](https://mc-stan.org/loo/reference/loo.html)
on the fold's summed log likelihood. The rest are refitted without the
fold, as
[`lfo_cv.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/lfo_cv.mvgam.md)
refits the time blocks it cannot approximate. Three settings cover the
range:

- `pareto_k_threshold = Inf` refits nothing. Pareto-k warnings mark
  folds the approximation fits poorly.

- The default refits the folds whose Pareto-k exceeds
  `pareto_k_threshold`, which is often a handful.

- `exact = TRUE` refits every fold, as
  [`brms::kfold()`](https://mc-stan.org/loo/reference/kfold-generic.html)
  does.

`group` takes several columns, such as `c("site", "season")`. Every row
of one combination stays in the same fold.

## Usage

``` r
# S3 method for class 'mvgam'
kfold(
  x,
  K = NULL,
  group = NULL,
  folds = NULL,
  exact = FALSE,
  pareto_k_threshold = NULL,
  fold_split = c("grouped", "stratified", "random"),
  seed = NULL,
  silent = 1L,
  ...
)

# S3 method for class 'mvgam_kfold'
print(x, digits = 2L, ...)

# S3 method for class 'mvgam_kfold'
summary(object, ...)

# S3 method for class 'mvgam_kfold'
plot(x, ...)
```

## Arguments

- x:

  A fitted `mvgam` object, or for the methods an `mvgam_kfold` object.

- K:

  Integer number of folds. With `group` and `K = NULL` (the default),
  each group is a fold. A `K` below the number of groups bins the groups
  into `K` folds with
  [`loo::kfold_split_grouped()`](https://mc-stan.org/loo/reference/kfold-helpers.html).
  Without `group`, `K` defaults to 10.

- group:

  Optional character vector of training-data columns whose combinations
  define the groups. `NULL` (the default) takes the closure unit of an
  [`occ()`](https://nicholasjclark.github.io/mvgam/reference/occ.md) or
  [`nmix()`](https://nicholasjclark.github.io/mvgam/reference/nmix.md)
  model and the row otherwise.

- folds:

  Optional integer vector of one fold per training row, replacing `K`
  and `fold_split`. Each fold is then held out and scored as one group.
  A fold never splits a group, which is a closure unit by default.

- exact:

  Logical. `TRUE` refits every fold. Defaults to `FALSE`.

- pareto_k_threshold:

  Numeric. A fold whose Pareto-k exceeds it is refitted. `NULL` (the
  default) takes `min(1 - 1 / log10(S), 0.7)` for `S` posterior draws.
  `Inf` refits nothing.

- fold_split:

  `"grouped"` (the default), `"stratified"` or `"random"`: the
  `loo::kfold_split_*()` function binning groups into `K` folds when
  there are more groups than folds.

- seed:

  Optional integer seed for the fold split and the refits. The session's
  random state is restored when the call returns.

- silent:

  Integer in `\{0, 1, 2\}`. `0` prints each refit and its sampler start
  and finish lines. `1` (default) prints the refit count. `2` prints
  nothing.

- ...:

  Unused. Anything passed here is refused.

- digits:

  Integer number of decimal places. Defaults to 2.

- object:

  An `mvgam_kfold` object.

## Value

A `c("mvgam_kfold", "kfold", "loo")` object, which
[`loo::loo_compare()`](https://mc-stan.org/loo/reference/loo_compare.html)
takes as it takes
[`loo.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/loo.mvgam.md)
results. It holds:

- `estimates`: `elpd_kfold`, `p_kfold` and `kfoldic` with their standard
  errors.

- `pointwise`: the ELPD of each group. `pointwise_psis` holds the PSIS
  estimates before any refit.

- `pareto_k`: the Pareto-k of each group's PSIS estimate, all `NA` under
  `exact = TRUE`.

- `n_refits` and `refit_groups`: the number of refitted folds and the
  groups they hold.

- `K` and `exact`: the number of folds and the mode.

- `group`: the grouping columns, `"fold"` when `folds` was given, or
  `NULL` for single rows.

- `pareto_k_threshold`: the threshold applied, with
  `pareto_k_threshold_adaptive` `TRUE` when the default rule set it.

[`print()`](https://rdrr.io/r/base/print.html) shows the ELPD with its
standard error, the folds and refits and the Pareto k of the PSIS
estimates. It then names the groups with the lowest ELPD and the
refitted groups whose ELPD moved most from the PSIS estimate.
[`summary()`](https://rdrr.io/r/base/summary.html) returns a tibble with
one row per held-out group: `group`, its `elpd` and `elpd_share` of the
total, for a refitted group `elpd_psis` and the `lift` of the refit over
it, `pareto_k` and `refit`.
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) draws the ELPD
of each group and its Pareto k, as
[`plot.mvgam_lfo()`](https://nicholasjclark.github.io/mvgam/reference/lfo_cv.mvgam.md)
does for time blocks. Dark red points mark a Pareto k above the dashed
threshold, where PSIS is unreliable. In the ELPD panel they mark the
groups below the 15% quantile.

## References

Burkner, P.-C., Gabry, J. and Vehtari, A. (2020). Approximate
leave-future-out cross-validation for Bayesian time series models.
*Journal of Statistical Computation and Simulation*, 90(14):2499-2523.
[doi:10.1080/00949655.2020.1783262](https://doi.org/10.1080/00949655.2020.1783262)

Vehtari, A., Gelman, A. and Gabry, J. (2017). Practical Bayesian model
evaluation using leave-one-out cross-validation and WAIC. *Statistics
and Computing*, 27:1413-1432.
[doi:10.1007/s11222-016-9696-4](https://doi.org/10.1007/s11222-016-9696-4)

## See also

[`loo.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/loo.mvgam.md)
approximates leave-one-out cross-validation.
[`lfo_cv.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/lfo_cv.mvgam.md)
scores forecasts from rolling origins. `summary.mvgam_kfold()` and
`plot.mvgam_kfold()` report each group. The [forecast evaluation
article](https://nicholasjclark.github.io/mvgam/articles/forecast_evaluation.html)
compares models by cross-validation.

Other model comparison:
[`bridge_sampler.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/bridge_sampler.mvgam.md),
[`compare_elpds()`](https://nicholasjclark.github.io/mvgam/reference/compare_elpds.md),
[`lfo_cv.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/lfo_cv.mvgam.md),
[`log_lik.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/log_lik.mvgam.md),
[`loo.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/loo.mvgam.md),
[`loo_compare.mvgam_lfo()`](https://nicholasjclark.github.io/mvgam/reference/loo_compare.mvgam_lfo.md),
[`mvgam_loo_extras`](https://nicholasjclark.github.io/mvgam/reference/mvgam_loo_extras.md),
[`posterior_average.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/posterior_average.mvgam.md),
[`waic.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/waic.mvgam.md)

## Author

Nicholas J Clark

## Examples

``` r
# \dontrun{
# Overdispersed counts: a negative binomial size of 2
set.seed(13)
simdat <- sim_mvgam(family = negbinomial(), n_series = 1L,
                     n_timepoints = 120L, type = 6L,
                     family_pars = list(size = 2))

mod <- mvgam(y ~ s(season, bs = "cc", k = 6),
              trend_formula = ~ CAR(),
              data    = simdat$data_train,
              family  = negbinomial(),
              chains  = 2, seed = 1, silent = 2)
#> Warning: 5 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.

# Neighbouring times share information through the trend. Folds of
# contiguous time blocks hold each block out whole. Folds that PSIS
# approximates poorly are refitted.
blocks <- cut(simdat$data_train$time, breaks = 4L, labels = FALSE)
k <- kfold(mod, folds = blocks)
#> kfold: refitting 4 fold(s) ...
#> Warning: 17 of 2000 (1.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
#> Warning: 10 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
#> Warning: 2 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
#> Warning: 1 of 2 chains had an E-BFMI less than 0.3.
#> See https://mc-stan.org/misc/warnings for details.
#> Warning: 6 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
k
#> mvgam k-fold cross-validation
#>   elpd_kfold : -275.39 (SE 14.90)
#>   kfoldic    : 550.78 (SE 29.80)
#>   groups     : 4 (fold)
#>   refits     : 4 of 4 folds
#>   Pareto k   : max 0.91, median 0.88, threshold 0.70 (adaptive)
#> 
#> Lowest ELPD (share of the total):
#>   4                    -77.29 (28.1%)
#>   2                    -70.75 (25.7%)
#>   1                    -68.05 (24.7%)
#> 
#> Largest change from PSIS to refit:
#>   4                    -75.42 -> -77.29
#>   2                    -69.58 -> -70.75
#>   1                    -67.11 -> -68.05

# The ELPD and Pareto k of each block
plot(k)


# Next, lfo_cv() scores forecasts made from rolling origins
# }
```
