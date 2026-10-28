# Tidy an `mvgam` object's parameter posteriors

Posterior summaries of the parameters of a fitted `mvgam` model, as a
tibble with the columns of broom.mixed: `term`, `estimate`, `std.error`,
`conf.low` and `conf.high`. A `type` column assigns each parameter to
the observation model or the trend.

## Usage

``` r
# S3 method for class 'mvgam'
tidy(
  x,
  effects = "all",
  robust = FALSE,
  conf.int = TRUE,
  conf.level = 0.95,
  rhat = FALSE,
  ess = FALSE,
  ...
)
```

## Arguments

- x:

  A fitted `mvgam` object.

- effects:

  `"all"` (the default) for every parameter, or one of `"fixed"`,
  `"ran_pars"` and `"ran_vals"` for the categories above, as in
  `broom.mixed::tidy.brmsfit()`.

- robust:

  Logical. If `FALSE` (the default), uses mean and standard deviation
  for summaries. If `TRUE`, uses median and median absolute deviation
  (MAD).

- conf.int:

  Logical. If `TRUE` (the default), include `conf.low` and `conf.high`
  posterior quantile columns.

- conf.level:

  Numeric. Probability covered by the credible interval reported in
  `conf.low` / `conf.high`. Defaults to `0.95`.

- rhat:

  Logical. If `TRUE`, add a `rhat` column with posterior::rhat() values.

- ess:

  Logical. If `TRUE`, add an `ess_bulk` column with
  posterior::ess_bulk() values.

- ...:

  Unused. Anything passed here is refused.

## Value

A tibble with one row per parameter and the columns `term`, `type`,
`estimate` and `std.error`. `conf.int = TRUE` adds `conf.low` and
`conf.high`. `rhat` and `ess` add `rhat` and `ess_bulk`.

## Details

The `type` column places every parameter into one of the following
categories, each with the `effects` class it belongs to:

- `"observation_family_extra_param"` (`ran_pars`) – auxiliary
  observation-family parameters such as `sigma`, `shape`, `nu`, `phi`,
  `zi`, `hu`.

- `"observation_beta"` (`fixed`) – population-level coefficients of the
  observation formula, with the intercept and the fixed parts of its
  smooths and monotonic terms.

- `"observation_smooth_param"` (`ran_pars`) – the penalty of each smooth
  (`sds_`) and the marginal deviation and length scale of each Gaussian
  process.

- `"observation_smooth_coef"` (`ran_vals`) – the penalised basis
  coefficients of the smooths and Gaussian processes.

- `"random_effect_group_level"` (`ran_pars`) – the standard deviations
  and correlations of the observation formula's group-level terms.

- `"random_effect_beta"` (`ran_vals`) – the group-level coefficients
  themselves, one per level and term.

- `"trend_model_param"` (`ran_pars`) – parameters of the trend dynamics
  (`ar1_trend`, `theta1_trend`, `Sigma_trend`, `k_trend`, `m_trend`,
  `delta_trend`, etc.) and the factor loadings.

- `"trend_beta"`, `"trend_smooth_param"`, `"trend_smooth_coef"`,
  `"trend_random_effect_group_level"` and `"trend_random_effect_beta"` –
  the same five blocks of the trend formula.

Fixed-effect terms take the names
[`fixef.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_diagnostics.md)
gives them, as `broom.mixed::tidy.brmsfit()` names them: `b_x` is `x`
and `b_sigma_x` is `sigma_x`. Every other term keeps the name
[`variables.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/index-mvgam.md)
lists. brms samples a centred intercept and transforms it to the
intercept on the data's scale. The table reports the coefficient once,
as `Intercept`.

## See also

Other tidiers:
[`augment.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/augment.mvgam.md),
[`glance.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/glance.mvgam.md)

## Examples

``` r
# \dontrun{
set.seed(13)
simdat <- sim_mvgam(family = poisson(), n_series = 1L,
                     n_timepoints = 120L, trend_model = AR())
mod <- mvgam(y ~ s(x), trend_formula = ~ AR(p = 1),
              data    = simdat$data_train,
              family  = poisson(),
              chains  = 2, silent = 2)
#> Warning: 9 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.

# One row per parameter, with broom's column names
td <- tidy(mod)
td
#> # A tibble: 13 × 6
#>    term                  type              estimate std.error conf.low conf.high
#>    <chr>                 <chr>                <dbl>     <dbl>    <dbl>     <dbl>
#>  1 Intercept             observation_beta     1.63     0.143     1.36      1.92 
#>  2 sx_1                  observation_beta     7.70     4.92     -1.81     17.7  
#>  3 sds_sx_1              observation_smoo…    4.32     1.65      1.93      8.28 
#>  4 s_sx_1[1]             observation_smoo…   -2.23     4.46    -12.6       5.10 
#>  5 s_sx_1[2]             observation_smoo…   -2.07     2.23     -6.83      2.20 
#>  6 s_sx_1[3]             observation_smoo…   -5.21     1.26     -7.68     -2.87 
#>  7 s_sx_1[4]             observation_smoo…    0.164    1.20     -2.24      2.49 
#>  8 s_sx_1[5]             observation_smoo…    1.01     2.36     -3.49      5.83 
#>  9 s_sx_1[6]             observation_smoo…   -2.60     2.91     -8.56      2.81 
#> 10 s_sx_1[7]             observation_smoo…    6.62     4.15     -0.160    15.5  
#> 11 s_sx_1[8]             observation_smoo…    0.765    3.64     -6.41      8.12 
#> 12 sigma_trend[series_1] trend_model_param    0.435    0.0730    0.302     0.589
#> 13 ar1_trend[series_1]   trend_model_param    0.589    0.145     0.284     0.855

# The `type` column separates the trend's parameters from the
# observation model's
td[td$type == "trend_model_param", ]
#> # A tibble: 2 × 6
#>   term                  type              estimate std.error conf.low conf.high
#>   <chr>                 <chr>                <dbl>     <dbl>    <dbl>     <dbl>
#> 1 sigma_trend[series_1] trend_model_param    0.435    0.0730    0.302     0.589
#> 2 ar1_trend[series_1]   trend_model_param    0.589    0.145     0.284     0.855

# The fixed effects alone, with convergence diagnostics
tidy(mod, effects = "fixed", rhat = TRUE, ess = TRUE)
#> # A tibble: 2 × 8
#>   term      type            estimate std.error conf.low conf.high  rhat ess_bulk
#>   <chr>     <chr>              <dbl>     <dbl>    <dbl>     <dbl> <dbl>    <dbl>
#> 1 Intercept observation_be…     1.63     0.143     1.36      1.92  1.01     791.
#> 2 sx_1      observation_be…     7.70     4.92     -1.81     17.7   1.00     785.

# Next, augment() adds fitted values and residuals to the data
# }
```
