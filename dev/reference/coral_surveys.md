# Simulated coral bleaching index with irregular CAR(1) state

A state-space dataset for three coral reefs surveyed at irregular
monthly intervals (gap distribution `Uniform(1, 6)` months, mean
approximately 3.5). The latent state is a continuous-time AR(1) (CAR(1))
trend per reef with `phi = 0.7` and `sigma = 0.5`. The observation
equation adds an observed sea-surface temperature (SST) driver via a
smooth nonlinear effect.

## Usage

``` r
coral_surveys
```

## Format

A `data.frame` with 150 rows and the following fields:

- series:

  factor identifying the reef (`flynn`, `myrmidon`, `pixie`); duplicated
  as `reef`.

- time:

  integer month index (irregularly spaced within each reef).

- y:

  numeric bleaching index (Gaussian, scaled); duplicated as `bleaching`.

- reef:

  factor, alias of `series`.

- sst:

  numeric observed sea-surface temperature in degrees Celsius.

- bleaching:

  numeric bleaching index, alias of `y`.

## Details

Naive AR(1) treats every consecutive observation as a unit gap and
biases `phi` downward toward `phi^mean(dt)` (roughly 0.29 for this
dataset), leaving the SST smooth to absorb persistence that belongs to
the latent state. `trend_formula = ~ CAR()` with the supplied `time`
column measures the damping over the median gap between surveys, three
months here. Its `ar1_trend` is then near `0.7^3 = 0.34`, the monthly
`phi` of `0.7` cubed. With persistence assigned to the latent state, the
SST smooth is identifiable.

True generative components:

- Per-reef intercepts (centred).

- Latent CAR(1) state per reef with `phi = 0.7` and `sigma = 0.5`,
  propagated through
  `trend[t] = phi^dt * trend[t-1] + N(0, sigma * sqrt((1 - phi^(2*dt)) / (1 - phi^2)))`.

- Shared smooth nonlinear obs-side effect of SST, evaluated via a
  thin-plate basis.

- Observation noise `sigma_obs = 0.3`.

## See also

[`CAR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
for the trend,
[`AR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
for evenly spaced surveys,
[`mvgam_data()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_data.md)
to check the data.

## Examples

``` r
data(coral_surveys)
mvgam_data(coral_surveys, family = gaussian())
#> ✔ Data check passed for family 'gaussian (link = identity)'.
#> • series: 3 levels
#> • time:   1 to 178
#> • n obs:  150 (0 NA)


# \dontrun{
# A model with the structure the data were simulated from
mod <- mvgam(
  formula = y ~ s(sst, k = 8),
  trend_formula = ~ CAR(),
  data = coral_surveys,
  family = gaussian(),
  chains = 2, silent = 2
)
#> Warning: 22 of 2000 (1.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
#> Warning: 2 of 2 chains had an E-BFMI less than 0.3.
#> See https://mc-stan.org/misc/warnings for details.
summary(mod)
#>  Family: gaussian 
#>   Links: mu = identity; sigma = log 
#> Formula: y ~ s(sst, k = 8) 
#>    Data: coral_surveys (Number of observations: 150) 
#>  Series: 3 
#>  Trends: CAR 
#>   Draws: 2 chains, each with iter = 2000; warmup = 1000; thin = 1; 
#>          total post-warmup draws = 2000
#> 
#> == Observation Model ==
#> Smoothing Spline Hyperparameters:
#>             Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sds(ssst_1)     1.80      1.05     0.37     4.42 1.00      511      342
#> 
#> Regression Coefficients:
#>           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept     0.05      0.13    -0.20     0.31 1.01      449     1039
#> ssst_1       10.58      3.15     5.46    17.32 1.00      660      834
#> 
#> Further Distributional Parameters:
#>       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sigma     0.50      0.10     0.30     0.69 1.01       75      121
#> 
#> == Trend Model ==
#> Trend Specific Parameters:
#>                       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
#> sigma_trend[flynn]        0.59      0.17     0.16     0.88 1.01      124
#> sigma_trend[myrmidon]     0.57      0.15     0.26     0.88 1.00      127
#> sigma_trend[pixie]        0.32      0.12     0.10     0.55 1.00      126
#> ar1_trend[flynn]          0.27      0.19     0.02     0.74 1.01      498
#> ar1_trend[myrmidon]       0.83      0.10     0.60     0.97 1.00      212
#> ar1_trend[pixie]          0.59      0.22     0.11     0.94 1.01      348
#>                       Tail_ESS
#> sigma_trend[flynn]         283
#> sigma_trend[myrmidon]      451
#> sigma_trend[pixie]         562
#> ar1_trend[flynn]           609
#> ar1_trend[myrmidon]        708
#> ar1_trend[pixie]          1032
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
# }
```
