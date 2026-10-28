# K-fold cross-validation for `mvgam` fits

Measures how well a model predicts groups of data it was not fitted to,
such as sites or species. Each fold of groups is held out in turn and
scored by its expected log predictive density (ELPD). Higher is better.
A fold that importance sampling approximates well takes the PSIS
estimate of [`loo::loo()`](https://mc-stan.org/loo/reference/loo.html)
on the fold's summed log likelihood. The rest are refitted without the
fold, as
[`lfo_cv.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/lfo_cv.mvgam.md)
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
```

## Arguments

- x:

  A fitted `mvgam` object.

- K:

  Integer number of folds. With `group` and `K = NULL` (the default),
  each group is a fold. A `K` below the number of groups bins the groups
  into `K` folds with
  [`loo::kfold_split_grouped()`](https://mc-stan.org/loo/reference/kfold-helpers.html).
  Without `group`, `K` defaults to 10.

- group:

  Optional character vector of training-data columns whose combinations
  define the groups. `NULL` (the default) takes the closure unit of an
  [`occ()`](https://nicholasjclark.github.io/mvgam/dev/reference/occ.md)
  or
  [`nmix()`](https://nicholasjclark.github.io/mvgam/dev/reference/nmix.md)
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

## Value

A `c("mvgam_kfold", "kfold", "loo")` object, which
[`loo::loo_compare()`](https://mc-stan.org/loo/reference/loo_compare.html)
takes as it takes
[`loo.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/loo.mvgam.md)
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

[`loo.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/loo.mvgam.md)
approximates leave-one-out cross-validation.
[`lfo_cv.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/lfo_cv.mvgam.md)
scores forecasts from rolling origins.
[`summary.mvgam_kfold()`](https://nicholasjclark.github.io/mvgam/dev/reference/summary.mvgam_kfold.md)
and
[`plot.mvgam_kfold()`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_kfold.md)
report each group. The [forecast evaluation
article](https://nicholasjclark.github.io/mvgam/dev/articles/forecast_evaluation.html)
compares models by cross-validation.

Other model comparison:
[`bridge_sampler.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/bridge_sampler.mvgam.md),
[`compare_elpds()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_elpds.md),
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
set.seed(13)
simdat <- sim_mvgam(family = gaussian(), n_series = 1L,
                     n_timepoints = 120L, type = 6L)

mod <- mvgam(y ~ s(season, bs = "cc"),
              trend_formula = ~ CAR(),
              data    = simdat$data_train,
              family  = gaussian(),
              chains  = 2, silent = 2)
#> Warning: 67 of 2000 (3.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.

# Neighbouring times share information through the trend. Folds of
# contiguous time blocks hold each block out whole. Folds that PSIS
# approximates poorly are refitted.
blocks <- cut(simdat$data_train$time, breaks = 4L, labels = FALSE)
k <- kfold(mod, folds = blocks)
#> kfold: refitting 4 fold(s) ...
#> Warning: 7 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
#> Warning: 20 of 2000 (1.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
#> Warning: 2 of 2 chains had an E-BFMI less than 0.3.
#> See https://mc-stan.org/misc/warnings for details.
#> Warning: 7 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
#> Warning: 5 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
k
#> mvgam k-fold cross-validation
#>   elpd_kfold : -135.38 (SE 9.61)
#>   kfoldic    : 270.76 (SE 19.21)
#>   groups     : 4 (fold)
#>   refits     : 4 of 4 folds
#>   Pareto k   : max 1.76, median 1.18, threshold 0.70 (adaptive)
#> 
#> Lowest ELPD (share of the total):
#>   2                    -39.64 (29.3%)
#>   1                    -35.49 (26.2%)
#>   4                    -31.75 (23.4%)
#> 
#> Largest change from PSIS to refit:
#>   1                    -31.38 -> -35.49
#>   2                    -36.32 -> -39.64
#>   4                    -29.31 -> -31.75

# The ELPD and Pareto k of each block
plot(k)


# Next, lfo_cv() scores forecasts made from rolling origins
# }
```
