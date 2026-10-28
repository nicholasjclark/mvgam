# Compare leave-future-out evaluations across models

Ranks two or more
[`lfo_cv()`](https://nicholasjclark.github.io/mvgam/dev/reference/lfo_cv.mvgam.md)
results by their summed ELPD, in the layout of
[`loo::loo_compare()`](https://mc-stan.org/loo/reference/loo_compare.html).
The standard error of each difference comes from the paired per-fold
differences with the best model. The results must share one evaluation
grid, with the same `min_t`, `fc_horizon` and data.

## Usage

``` r
# S3 method for class 'mvgam_lfo'
loo_compare(x, ..., model_names = NULL)
```

## Arguments

- x:

  An `mvgam_lfo` object.

- ...:

  Further `mvgam_lfo` objects.

- model_names:

  Optional character vector of model labels; overrides the names
  inferred from the call.

## Value

A `compare.loo` table in the layout of
[`loo::loo_compare()`](https://mc-stan.org/loo/reference/loo_compare.html),
one row per model, best first.

- `model`:

  The model's name.

- `elpd_diff`:

  Summed ELPD difference from the best model, zero for the best.

- `se_diff`:

  Standard error of `elpd_diff`, `sqrt(N) * sd(fold_diff)` over the `N`
  folds.

- `p_worse`:

  The normal probability that the model forecasts worse than the best
  one (Sivula et al. 2025). `NA` for the best model and for a difference
  without spread.

- `diag_diff`:

  `"N < 100"` for fewer than 100 folds, else `"|elpd_diff| < 4"` for a
  difference too small for its SE.

- `diag_elpd`:

  The number of folds whose PSIS estimate was kept with a Pareto k above
  the threshold.

- `elpd_lfo`:

  Summed per-fold ELPD.

- `se_elpd_lfo`:

  Standard error of `elpd_lfo`.

## References

Sivula, T., Magnusson, M., Matamoros, A. A. and Vehtari, A. (2025).
Uncertainty in Bayesian leave-one-out cross-validation based model
comparison. *Bayesian Analysis*.
[doi:10.1214/25-BA1569](https://doi.org/10.1214/25-BA1569)

## See also

[`lfo_cv()`](https://nicholasjclark.github.io/mvgam/dev/reference/lfo_cv.mvgam.md),
[`loo_model_weights.mvgam_lfo()`](https://nicholasjclark.github.io/mvgam/dev/reference/loo_model_weights.mvgam_lfo.md),
[`compare_elpds()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_elpds.md),
[`plot.mvgam_compare_elpds()`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_compare_elpds.md)

Other model comparison:
[`bridge_sampler.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/bridge_sampler.mvgam.md),
[`compare_elpds()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_elpds.md),
[`kfold.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/kfold.mvgam.md),
[`lfo_cv.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/lfo_cv.mvgam.md),
[`log_lik.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/log_lik.mvgam.md),
[`loo.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/loo.mvgam.md),
[`mvgam_loo_extras`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_loo_extras.md),
[`posterior_average.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_average.mvgam.md),
[`waic.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/waic.mvgam.md)

## Examples

``` r
# \dontrun{
set.seed(1)
simdat <- sim_mvgam(family = poisson(), n_series = 1L,
                    n_timepoints = 120L, trend_model = AR())
# Two models of one count series: a smooth of x with and without
# an AR(1) trend
mod_gam <- mvgam(y ~ s(x), data = simdat$data_train,
                 family = poisson(), chains = 2, silent = 2)
#> Warning: 25 of 2000 (1.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
mod_ar <- mvgam(y ~ s(x), trend_formula = ~ AR(p = 1),
                data = simdat$data_train, family = poisson(),
                chains = 2, silent = 2)
#> Warning: 5 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
# }
# \dontrun{
# Each origin after t = 70 forecasts the next time step. The
# pointwise log densities feed stacking weights
lfo_gam <- lfo_cv(mod_gam, min_t = 70L, save_log_lik = TRUE)
#> Warning: 4 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
#> Warning: 6 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
#> Warning: 9 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
lfo_ar <- lfo_cv(mod_ar, min_t = 70L, save_log_lik = TRUE)
#> Warning: 9 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
#> Warning: 11 of 2000 (1.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
#> Warning: 13 of 2000 (1.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
#> Warning: 37 of 2000 (2.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.

# The model with the higher summed ELPD comes first
loo_compare(lfo_gam, lfo_ar, model_names = c("GAM", "AR"))
#>  model elpd_diff se_diff p_worse diag_diff diag_elpd
#>     AR       0.0     0.0      NA                    
#>    GAM     -32.7     9.8    1.00   N < 100          
#> 
#> Diagnostic flags present.
#> See ?`loo-glossary` (sections `diag_diff` and `diag_elpd`)
#> or https://mc-stan.org/loo/reference/loo-glossary.html.

# Next, compare_elpds() shows where along the series the models
# part ways
# }
```
