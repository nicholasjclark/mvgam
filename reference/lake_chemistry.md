# Simulated lake phosphorus with correlated random-walk state

A state-space dataset for five lakes sampled monthly across 60 months.
Each sample records total phosphorus in micrograms per litre, drawn from
a Gamma distribution with a log link. The latent process is a correlated
random walk across lakes. Two regional clusters share positive
innovation correlation. A fifth lake is independent. The observation
equation adds a binary treatment indicator that switches on at month 31,
with heterogeneous slopes across lakes. Phosphorus rises in two lakes
and falls in three.

## Usage

``` r
lake_chemistry
```

## Format

A `data.frame` with 300 rows and the following fields:

- series:

  factor identifying the lake (`alpine`, `boreal`, `coastal`, `delta`,
  `estuary`); duplicated as `lake`.

- time:

  integer month index from 1 to 60.

- y:

  positive total phosphorus in micrograms per litre; duplicated as
  `chemistry`.

- lake:

  factor, alias of `series`.

- month:

  integer month index, alias of `time`.

- treated:

  integer (0/1) treatment indicator; switches on from month 31 onwards
  in all lakes.

- chemistry:

  positive total phosphorus, alias of `y`.

## Details

A pooled fit with one treatment slope for all lakes reports an effect
near zero with a wide credible interval. The positive and negative
responses cancel. A state-space fit (`trend_formula = ~ RW(cor = TRUE)`)
with a treatment slope for each lake recovers the response of every
lake.

True generative components, on the log scale:

- An intercept of `log(20)`, a typical 20 micrograms per litre. Each
  lake adds its own centred intercept.

- Latent random walk with cross-lake innovation correlation `0.6`
  between `(alpine, boreal)` and `(coastal, delta)`, independent for
  `estuary`. Innovation standard deviation `0.1`.

- Heterogeneous treatment slopes:
  `(alpine = +0.8, boreal = +0.6, coastal = -0.5, delta = -0.7, estuary = -0.6)`.

- Gamma observations with shape `10`, a coefficient of variation near
  0.32.

## See also

[`RW()`](https://nicholasjclark.github.io/mvgam/reference/trend_constructors.md)
for the trend,
[`residual_cor()`](https://nicholasjclark.github.io/mvgam/reference/residual_cor.md)
to extract the fitted correlations between lakes,
[`ranef.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/ranef.mvgam.md)
for the lake slopes,
[`mvgam_data()`](https://nicholasjclark.github.io/mvgam/reference/mvgam_data.md)
to check the data.

## Examples

``` r
data(lake_chemistry)
mvgam_data(lake_chemistry, family = Gamma(link = "log"))
#> ✔ Data check passed for family 'gamma (link = log)'.
#> • series: 5 levels
#> • time:   1 to 60
#> • n obs:  300 (0 NA)


# \dontrun{
# A model with the structure the data were simulated from
mod <- mvgam(
  formula = y ~ treated + (treated || lake),
  trend_formula = ~ RW(cor = TRUE),
  data = lake_chemistry,
  family = Gamma(link = "log"),
  chains = 2, silent = 2
)
#> Warning: 1 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
summary(mod)
#>  Family: gamma 
#>   Links: mu = log; shape = log 
#> Formula: y ~ treated + (treated || lake) 
#>    Data: lake_chemistry (Number of observations: 300) 
#>  Series: 5 
#>  Trends: RW 
#>   Draws: 2 chains, each with iter = 2000; warmup = 1000; thin = 1; 
#>          total post-warmup draws = 2000
#> 
#> == Observation Model ==
#> Multilevel Hyperparameters:
#> ~lake (Number of levels: 5)
#>               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sd(Intercept)     1.14      0.53     0.49     2.47 1.00     1538     1511
#> sd(treated)       1.11      0.61     0.38     2.76 1.00     1453     1181
#> 
#> Regression Coefficients:
#>           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept     2.95      0.55     1.82     4.05 1.00     1067     1189
#> treated      -0.10      0.58    -1.30     1.11 1.00      763      877
#> 
#> Further Distributional Parameters:
#>       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> shape    10.55      1.11     8.55    12.85 1.00     1064     1335
#> 
#> == Trend Model ==
#> Trend Specific Parameters:
#>                      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
#> sigma_trend[alpine]      0.17      0.04     0.11     0.26 1.00     1520
#> sigma_trend[boreal]      0.16      0.04     0.10     0.25 1.00     1368
#> sigma_trend[coastal]     0.14      0.03     0.08     0.21 1.00     1941
#> sigma_trend[delta]       0.17      0.04     0.10     0.26 1.00     1664
#> sigma_trend[estuary]     0.21      0.05     0.13     0.32 1.00     1336
#>                      Tail_ESS
#> sigma_trend[alpine]      1667
#> sigma_trend[boreal]      1578
#> sigma_trend[coastal]     1723
#> sigma_trend[delta]       1631
#> sigma_trend[estuary]     1643
#> 
#> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).
#> 
#> Next steps:
#>   - `pp_check(fit)`: posterior predictive checks
#>   - `residual_cor(fit)`: implied correlations among series
#>   - `forecast(fit, newdata = ...)`: out-of-sample forecasts
#>   - `lfo_cv(fit)`: leave-future-out model comparison
#>   - `conditional_effects(fit)`: covariate effects
#> Use `how_to_cite(fit)` for a methods paragraph and `methods_md(fit)` for its equations.
# }
```
