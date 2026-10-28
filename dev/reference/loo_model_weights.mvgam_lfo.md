# Model weights from `mvgam_lfo` objects

Weights for combining models evaluated by
[`lfo_cv()`](https://nicholasjclark.github.io/mvgam/dev/reference/lfo_cv.mvgam.md)
on one rolling-origin grid, for example in
[`ensemble.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/ensemble.mvgam_forecast.md).
Yao et al. (2018) describe both methods.

- `"pseudo-BMA"`: the softmax of each model's summed ELPD. A gap of more
  than about 10 ELPD units gives the better model nearly all the weight.

- `"stacking"`: weights that maximise the log score of the combined
  predictive over the held-out observations. They spread weight more
  evenly. Each result needs the pointwise matrix that
  `lfo_cv(save_log_lik = TRUE)` stores.

## Usage

``` r
# S3 method for class 'mvgam_lfo'
loo_model_weights(x, ..., method = "pseudo-BMA", model_names = NULL)
```

## Arguments

- x:

  An `mvgam_lfo` object.

- ...:

  Further `mvgam_lfo` objects, weighted in the order given.

- method:

  `"pseudo-BMA"` (the default) or `"stacking"`.

- model_names:

  Optional character vector of model names, one per forecast. It
  replaces the names taken from the call.

## Value

A named numeric vector of weights summing to 1, of class
`"pseudobma_weights"` or `"stacking_weights"`.

## References

Yao, Y., Vehtari, A., Simpson, D., Gelman, A. (2018). Using stacking to
average Bayesian predictive distributions. *Bayesian Analysis* 13(3):
917-1003. [doi:10.1214/17-BA1091](https://doi.org/10.1214/17-BA1091)

## See also

[`loo_compare.mvgam_lfo()`](https://nicholasjclark.github.io/mvgam/dev/reference/loo_compare.mvgam_lfo.md),
[`lfo_cv()`](https://nicholasjclark.github.io/mvgam/dev/reference/lfo_cv.mvgam.md),
[`ensemble.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/ensemble.mvgam_forecast.md),
[`compare_elpds()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_elpds.md),
[`plot.mvgam_compare_elpds()`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_compare_elpds.md)

## Examples

``` r
# \dontrun{
set.seed(2)
simdat <- sim_mvgam(family = poisson(), n_series = 1L,
                    n_timepoints = 120L, trend_model = AR())
# Two models of one count series: a smooth of x with and without
# an AR(1) trend. The trend model samples with smaller steps
# (adapt_delta = 0.95). That avoids divergent transitions when
# lfo_cv() refits it to shorter stretches of the series
mod_gam <- mvgam(y ~ s(x, k = 5), data = simdat$data_train,
                 family = poisson(), chains = 2, silent = 2)
#> Warning: 29 of 2000 (1.0%) transitions ended with a divergence.
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
lfo_ar <- lfo_cv(mod_ar, min_t = 70L, save_log_lik = TRUE)
#> Warning: 1 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
#> Warning: 6 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
#> Warning: 5 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.

# Pseudo-BMA weights come from the summed ELPDs
loo_model_weights(lfo_gam, lfo_ar, model_names = c("GAM", "AR"))
#> Method: pseudo-BMA
#> ------
#>     weight
#> GAM 0.000 
#> AR  1.000 

# Stacking weights come from the pointwise log densities
w <- loo_model_weights(lfo_gam, lfo_ar, method = "stacking",
                       model_names = c("GAM", "AR"))
w
#> Method: stacking
#> ------
#>     weight
#> GAM 0.177 
#> AR  0.823 

# The stacking weights combine the two forecasts
fc <- ensemble(forecast(mod_gam, newdata = simdat$data_test),
               forecast(mod_ar, newdata = simdat$data_test),
               weights = w)
plot(fc)

# }
```
