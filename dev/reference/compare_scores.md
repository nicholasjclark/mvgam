# Tidy comparison of forecast scores across mvgam models

Bundles per-series-per-horizon scores from several `mvgam_forecast`
objects into one long-format `tibble`, with one row per model x series x
horizon combination. For joint scores (`"energy"`, `"variogram"`,
`"twenergy"`) the per-series scores are `NA` and the joint score is the
row with `series == "all_series"`.

## Usage

``` r
compare_scores(..., score = "crps", model_names = NULL, score_args = list())
```

## Arguments

- ...:

  Two or more `mvgam_forecast` objects (the same shape
  [`score.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/score.mvgam_forecast.md)
  consumes). Argument names are taken from the call and used as the
  `model` column. Bare expressions like `compare_scores(fc1, fc2)`
  produce `model` levels `"fc1"` and `"fc2"`.

- score:

  Character. Which proper scoring rule to evaluate. Forwarded to
  [`score.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/score.mvgam_forecast.md);
  see its documentation for the supported values.

- model_names:

  Optional character vector of model labels; overrides the names
  inferred from the call.

- score_args:

  Optional named list of extra arguments passed to
  [`score.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/score.mvgam_forecast.md)
  (for example `score_args = list(interval_width = 0.6)`).

## Value

A `tibble` with columns:

- `model` – factor with one level per supplied forecast.

- `series` – factor of series names (plus an `"all_series"` level when a
  joint score is requested).

- `eval_horizon` – integer horizon within the test window (1, 2, ...).

- `score` – the requested score value.

- `in_interval` – coverage indicator carried through from
  [`score.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/score.mvgam_forecast.md).

## See also

[`score.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/score.mvgam_forecast.md),
[`plot.mvgam_compare_scores()`](https://nicholasjclark.github.io/mvgam/dev/reference/plot.mvgam_compare_scores.md),
[`compare_elpds()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_elpds.md),
[`forecast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/forecast.mvgam.md),
[`ensemble.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/ensemble.mvgam_forecast.md)

Other forecasting:
[`ensemble.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/ensemble.mvgam_forecast.md),
[`forecast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/forecast.mvgam.md),
[`hindcast.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/hindcast.mvgam.md),
[`lfo_cv.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/lfo_cv.mvgam.md),
[`score.mvgam_forecast()`](https://nicholasjclark.github.io/mvgam/dev/reference/score.mvgam_forecast.md)

## Author

Nicholas J Clark

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
fc_gam <- forecast(mod_gam, newdata = simdat$data_test)
fc_ar <- forecast(mod_ar, newdata = simdat$data_test)

# One row per model and horizon, scored by the discrete ranked
# probability score (lower is better)
scores <- compare_scores(fc_gam, fc_ar, score = "drps",
                         model_names = c("GAM", "AR"))
summary(scores)
#> # A tibble: 30 × 4
#>    series   eval_horizon   GAM    AR
#>    <fct>           <int> <dbl> <dbl>
#>  1 series_1            1  1.62  1.09
#>  2 series_1            2  3.20  1.71
#>  3 series_1            3  7.72  5.58
#>  4 series_1            4  7.15  6.36
#>  5 series_1            5 11.6   5.82
#>  6 series_1            6  6.38  3.04
#>  7 series_1            7  1.32  1.08
#>  8 series_1            8  1.81  1.32
#>  9 series_1            9  4.29  2.47
#> 10 series_1           10  2.11  1.70
#> # ℹ 20 more rows

# The AR model's DRPS minus the GAM's. Negative values favour the
# AR model
plot(scores, relative = "GAM")


# Next, ensemble() combines the two forecasts
# }
```
