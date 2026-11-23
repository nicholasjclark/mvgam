# Simulated weekly bird song counts with hierarchically correlated AR(1) trends

A multivariate count time series for four bird species observed weekly
across 80 weeks. Each species has its own AR(1) latent trend; the four
series are coupled through a contemporaneous innovation correlation
matrix. Two species (warbler, thrush) rise together; two (wren, finch)
move opposite to them. A shared cyclic seasonal smooth is added on
week-within-year.

## Usage

``` r
birdsong
```

## Format

A `data.frame` with 320 rows and the following fields:

- series:

  factor identifying the species (`warbler`, `thrush`, `wren`, `finch`);
  duplicated as `species` for readability.

- time:

  integer week index from 1 to 80.

- y:

  integer weekly count (Poisson sampled); duplicated as `count`.

- species:

  factor, alias of `series`.

- week:

  integer week index, alias of `time`.

- week_in_year:

  integer week within the annual cycle (1..52), used for the shared
  seasonal smooth.

- count:

  integer weekly count, alias of `y`.

## Details

An independent-AR fit (`trend_formula = ~ AR(p = 1)`) treats the four
trajectories as unrelated. `trend_formula = ~ AR(p = 1, cor = TRUE)`
recovers the cross-species correlation that couples them.

True generative components:

- Species random intercepts (centred) on the log scale.

- Shared cyclic seasonal smooth (mgcv `bs = "cc"`) on week-within-year.

- Hierarchically correlated AR(1) trends with `phi = 0.7` and innovation
  standard deviation 0.35. The 4 x 4 innovation correlation matrix sets
  `(warbler, thrush)` against `(wren, finch)`.

## See also

[`AR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
for the trend,
[`residual_cor()`](https://nicholasjclark.github.io/mvgam/dev/reference/residual_cor.md)
to extract the fitted correlations between species,
[`mvgam_data()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_data.md)
to check the data.

## Examples

``` r
data(birdsong)
mvgam_data(birdsong, family = poisson())
#> ✔ Data check passed for family 'poisson (link = log)'.
#> • series: 4 levels
#> • time:   1 to 80
#> • n obs:  320 (0 NA)


# \dontrun{
# A model with the structure the data were simulated from
mod <- mvgam(
  formula = y ~ s(week_in_year, bs = "cc", k = 8) +
                s(species, bs = "re"),
  trend_formula = ~ AR(p = 1, cor = TRUE),
  data = birdsong,
  family = poisson(),
  chains = 2, silent = 2
)
#> Warning: 11 of 2000 (1.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
summary(mod)
#>  Family: poisson 
#>   Links: mu = log 
#> Formula: y ~ s(week_in_year, bs = "cc", k = 8) + s(species, bs = "re") 
#>    Data: birdsong (Number of observations: 320) 
#>  Series: 4 
#>  Trends: AR(1) 
#>   Draws: 2 chains, each with iter = 2000; warmup = 1000; thin = 1; 
#>          total post-warmup draws = 2000
#> 
#> == Observation Model ==
#> Smoothing Spline Hyperparameters:
#>                      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
#> sds(sweek_in_year_1)     0.52      0.23     0.24     1.13 1.00      255
#> sds(sspecies_1)          0.56      0.37     0.18     1.56 1.00      454
#>                      Tail_ESS
#> sds(sweek_in_year_1)      330
#> sds(sspecies_1)           542
#> 
#> Regression Coefficients:
#>           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept     1.45      0.31     0.85     2.13 1.01      353      389
#> 
#> == Trend Model ==
#> Trend Specific Parameters:
#>                      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
#> sigma_trend[warbler]     0.32      0.06     0.22     0.45 1.01      754
#> sigma_trend[thrush]      0.44      0.10     0.26     0.64 1.01      801
#> sigma_trend[wren]        0.38      0.07     0.26     0.52 1.00      655
#> sigma_trend[finch]       0.32      0.07     0.19     0.46 1.00      933
#> ar1_trend[warbler]       0.59      0.12     0.33     0.79 1.00      762
#> ar1_trend[thrush]        0.38      0.23    -0.09     0.79 1.00      540
#> ar1_trend[wren]          0.50      0.15     0.18     0.77 1.00      457
#> ar1_trend[finch]         0.48      0.21     0.01     0.84 1.01      441
#>                      Tail_ESS
#> sigma_trend[warbler]     1352
#> sigma_trend[thrush]      1235
#> sigma_trend[wren]         863
#> sigma_trend[finch]        971
#> ar1_trend[warbler]       1179
#> ar1_trend[thrush]         950
#> ar1_trend[wren]           514
#> ar1_trend[finch]          583
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
