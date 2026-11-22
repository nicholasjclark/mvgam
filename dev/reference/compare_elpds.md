# Tidy comparison of leave-future-out ELPDs across mvgam_lfo runs

Stacks the per-step ELPDs from several `mvgam_lfo` objects into one
long-format `tibble`, with one row per model and evaluation time.

## Usage

``` r
compare_elpds(..., model_names = NULL)

# S3 method for class 'mvgam_compare_elpds'
plot(x, relative = NULL, cumulative = FALSE, ...)
```

## Arguments

- ...:

  Two or more `mvgam_lfo` objects from
  [`lfo_cv()`](https://nicholasjclark.github.io/mvgam/dev/reference/lfo_cv.mvgam.md).
  Argument names are taken from the call.

- model_names:

  Optional character vector of model names, one per forecast. It
  replaces the names taken from the call.

- x:

  An `mvgam_compare_elpds` object.

- relative:

  Optional name of a model in `x$model`. Each ELPD is then drawn less
  that model's at the same origin, with a dashed zero line. Positive
  values favour the other model. The baseline itself is left out.

- cumulative:

  Logical. `TRUE` draws the running sum of the ELPD over the origins,
  the quantity
  [`loo_compare()`](https://mc-stan.org/loo/reference/loo_compare.html)
  sums. Defaults to `FALSE`.

## Value

A `tibble` with columns:

- `model` – factor with one level per supplied LFO run.

- `eval_time` – the rolling-origin evaluation timepoint.

- `elpd` – per-step expected log predictive density.

- `pareto_k` – the Pareto-k diagnostic at that step.

- `refit_here` – logical, TRUE when the step triggered a fresh refit
  (Pareto-k above threshold or initial fit).

[`plot()`](https://rdrr.io/r/graphics/plot.default.html) draws each
model's ELPD by origin. Hollow points mark the origins where a Pareto k
above the threshold forced a refit.

## See also

[`lfo_cv()`](https://nicholasjclark.github.io/mvgam/dev/reference/lfo_cv.mvgam.md),
[`loo_compare.mvgam_lfo()`](https://nicholasjclark.github.io/mvgam/dev/reference/loo_compare.mvgam_lfo.md),
[`loo_model_weights.mvgam_lfo()`](https://nicholasjclark.github.io/mvgam/dev/reference/loo_model_weights.mvgam_lfo.md),
[`compare_scores()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_scores.md)

Other model comparison:
[`bridge_sampler.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/bridge_sampler.mvgam.md),
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
                    n_timepoints = 120L, trend_model = AR())
# Two models of one count series: a smooth of x with and without
# an AR(1) trend. The trend model samples with smaller steps
# (adapt_delta = 0.95). That avoids divergent transitions when
# lfo_cv() refits it to shorter stretches of the series
mod_gam <- mvgam(y ~ s(x, k = 5), data = simdat$data_train,
                 family = poisson(), chains = 2, silent = 2)
#> Warning: 12 of 2000 (1.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
mod_ar <- mvgam(y ~ s(x, k = 5), trend_formula = ~ AR(p = 1),
                data = simdat$data_train, family = poisson(),
                chains = 2, silent = 2,
                control = list(adapt_delta = 0.95))
#> Warning: 2 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
# }
# \dontrun{
# Each origin after t = 70 forecasts the next time step. The
# pointwise log densities feed stacking weights
lfo_gam <- lfo_cv(mod_gam, min_t = 70L, save_log_lik = TRUE)
#> Warning: 21 of 2000 (1.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
#> Warning: 6 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
#> Warning: 7 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
lfo_ar <- lfo_cv(mod_ar, min_t = 70L, save_log_lik = TRUE)
#> Warning: 2 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
#> Warning: 11 of 2000 (1.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
#> Warning: 9 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
#> Warning: 3 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.

# One row per model and origin
elpds <- compare_elpds(lfo_gam, lfo_ar, model_names = c("GAM", "AR"))
head(elpds)
#> # A tibble: 6 × 5
#>   model eval_time  elpd  pareto_k refit_here
#>   <fct>     <int> <dbl>     <dbl> <lgl>     
#> 1 GAM          71 -1.77 NA        TRUE      
#> 2 GAM          72 -3.44 -0.0655   FALSE     
#> 3 GAM          73 -4.09  0.0328   FALSE     
#> 4 GAM          74 -1.52 -0.168    FALSE     
#> 5 GAM          75 -2.07 -0.188    FALSE     
#> 6 GAM          76 -3.94 -0.000701 FALSE     

# The running sum of the AR model's ELPD minus the GAM's. Hollow
# points mark the origins where the model was refitted
plot(elpds, relative = "GAM", cumulative = TRUE)


# Next, loo_compare() sums the ELPDs with a standard error
# }
```
