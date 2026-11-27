# Vary a trend effect along the trend's axis

Use `lv_axis()` in a `trend_formula` in any position that takes a factor
variable, to give each latent factor its own effect. In a model with no
latent factors it gives each series its own effect. Use it when a
covariate should act differently on different series, such as
temperature raising one species and lowering another.

## Usage

``` r
lv_axis()
```

## Value

`NULL` (invisibly). The function only marks a term in a formula.

## Details

For example:


    trend_formula = ~ s(elev, by = lv_axis()) + AR(n_lv = 2)
    trend_formula = ~ temp:lv_axis() + AR(n_lv = 2)
    trend_formula = ~ (1 + temp | lv_axis()) + AR()
    trend_formula = ~ s(temp, lv_axis(), bs = "fs") + AR()

With `n_lv` set on the trend constructor, each factor `k = 1, ..., n_lv`
takes its own effect. The effect of a covariate on series `s` is the sum
over factors of the loading `Z[s, k]` times the effect on factor `k`.
This is constrained ordination, the model
[`jsdgam()`](https://nicholasjclark.github.io/mvgam/reference/jsdgam.md)
fits. Every term of such a `trend_formula` is then evaluated once per
factor and time. Each covariate must take one value per time.

With no `n_lv`, each series takes its own effect on the latent state.

The series column itself is not supported in a `trend_formula` term.
Write `lv_axis()` for an effect that differs along the trend's axis. An
effect of the series on the observations belongs in `formula`.
`by = trend` is deprecated and is treated as `by = lv_axis()`.

## What each form estimates

With no latent factors, `lv_axis()` is each series:

- `~ x:lv_axis()` gives a slope of `x` per series.

- `~ lv_axis()` gives a level per series.

- `~ (1 + x | lv_axis())` gives levels and slopes that are partially
  pooled across series.

- `~ s(x, by = lv_axis())` gives a smooth of `x` per series, and
  `~ s(x, lv_axis(), bs = "fs")` gives smooths that share a smoothness
  penalty.

With `n_lv` latent factors, each of these is estimated once per factor.
The coefficient of one factor has no scale of its own. The loadings and
the factor effects trade scale. The data identify their product.
[`conditional_effects.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/conditional_effects.mvgam.md)
and
[`predict.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/predict.mvgam.md)
report that product, the effect of the covariate on each series.
[`conditional_effects.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/conditional_effects.mvgam.md)
draws a panel per series. When every `lv_axis()` term is a slope or a
level,
[`summary.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/summary.mvgam.md)
reports the per-factor coefficients in the basis of the identified
loadings `Z_tilde`.

## See also

[`jsdgam()`](https://nicholasjclark.github.io/mvgam/reference/jsdgam.md)
fits per-factor effects across species.
[`conditional_effects.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/conditional_effects.mvgam.md)
plots the effect on each series. The [dynamic factor
vignette](https://nicholasjclark.github.io/mvgam/articles/dfm.html) fits
factor models. The [JSDM
article](https://nicholasjclark.github.io/mvgam/articles/jsdgam.html)
fits per-factor trait and covariate effects.

Other trend models:
[`LLT()`](https://nicholasjclark.github.io/mvgam/reference/LLT.md),
[`PW()`](https://nicholasjclark.github.io/mvgam/reference/piecewise_trends.md),
[`ZMVN()`](https://nicholasjclark.github.io/mvgam/reference/ZMVN.md),
[`list_trend_types()`](https://nicholasjclark.github.io/mvgam/reference/list_trend_types.md),
[`print.mvgam_trend()`](https://nicholasjclark.github.io/mvgam/reference/trend_constructors.md)

## Examples

``` r
# `lv_axis()` marks a term in a formula. Called by itself it
# returns NULL.
lv_axis()

# \dontrun{
# Proportions in three series whose seasonal effect on the logit
# scale differs: 1, 0.5 and -1
set.seed(1)
dat <- sim_mvgam(family = Beta(), n_series = 3L,
                 n_timepoints = 60L)$data_train
dat$season <- sin(2 * pi * dat$time / 12)
dat$y <- plogis(qlogis(dat$y) +
                  c(1, 0.5, -1)[as.integer(dat$series)] * dat$season)

# One seasonal slope per series
mod <- mvgam(
  formula       = y ~ 1,
  trend_formula = ~ season:lv_axis() + AR(p = 1),
  data          = dat,
  family        = Beta(),
  chains        = 2,
  silent        = 2
)
summary(mod)
#>  Family: beta 
#>   Links: mu = logit; phi = log 
#> Formula: y ~ 1 
#>    Data: dat (Number of observations: 135) 
#>  Series: 3 
#>  Trends: AR(1); formula: ~season:lv_axis() 
#>   Draws: 2 chains, each with iter = 2000; warmup = 1000; thin = 1; 
#>          total post-warmup draws = 2000
#> 
#> == Observation Model ==
#> Regression Coefficients:
#>           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept     0.23      0.20    -0.23     0.59 1.00      607      419
#> 
#> Further Distributional Parameters:
#>     Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> phi     3.22      0.54     2.33     4.45 1.00      667      991
#> 
#> == Trend Model ==
#> Regression Coefficients:
#>                       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
#> season:seriesseries_1     0.90      0.25     0.42     1.40 1.00     1961
#> season:seriesseries_2     0.45      0.28    -0.09     1.01 1.00     1612
#> season:seriesseries_3    -0.77      0.28    -1.33    -0.20 1.00     1466
#>                       Tail_ESS
#> season:seriesseries_1     1293
#> season:seriesseries_2     1357
#> season:seriesseries_3      976
#> 
#> Trend Specific Parameters:
#>                       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
#> sigma_trend[series_1]     0.45      0.20     0.16     0.91 1.00      502
#> sigma_trend[series_2]     0.46      0.17     0.22     0.87 1.00      834
#> sigma_trend[series_3]     0.42      0.14     0.21     0.76 1.00      787
#> ar1_trend[series_1]       0.01      0.44    -0.77     0.88 1.00      447
#> ar1_trend[series_2]       0.83      0.17     0.36     0.98 1.00      549
#> ar1_trend[series_3]       0.90      0.08     0.68     0.99 1.00      963
#>                       Tail_ESS
#> sigma_trend[series_1]     1266
#> sigma_trend[series_2]     1215
#> sigma_trend[series_3]     1283
#> ar1_trend[series_1]        328
#> ar1_trend[series_2]        628
#> ar1_trend[series_3]       1148
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
conditional_effects(mod)


# With two latent factors, each factor takes a seasonal slope. The
# loadings carry the two slopes to the three series. The prior
# table lists one coefficient per factor. A smooth takes the same
# marker: `s(season, by = lv_axis())`
get_prior(
  mvgam_formula(y ~ 1,
                trend_formula = ~ season:lv_axis() + AR(p = 1, n_lv = 2)),
  data   = dat,
  family = Beta()
)
#>                 prior     class           coef group resp dpar nlpar lb ub tag
#>  student_t(3, 0, 2.5) Intercept                                               
#>     gamma(0.01, 0.01)       phi                                       0       
#>        normal(0, 0.5) ar1_trend                                      -1  1    
#>  student_t(3, 0, 0.5)         Z                                               
#>                (flat)   b_trend                                               
#>                (flat)   b_trend season:.trend1                                
#>                (flat)   b_trend season:.trend2                                
#>        source
#>       default
#>       default
#>       default
#>       default
#>       default
#>  (vectorized)
#>  (vectorized)
# }
```
