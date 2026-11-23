# Simulated coral bleaching surveys with an irregular CAR(1) state

A state-space dataset for three coral reefs surveyed at irregular
monthly intervals (gap distribution `Uniform(1, 6)` months, mean
approximately 3.5). Each survey inspects between 20 and 40 colonies and
counts those that have bleached. The latent state is a continuous-time
AR(1) (CAR(1)) trend per reef with `phi = 0.9` and `sigma = 0.3` on the
logit scale. The observation equation adds an observed sea-surface
temperature (SST) driver through a smooth nonlinear effect.

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

  integer count of bleached colonies; duplicated as `bleaching`.

- reef:

  factor, alias of `series`.

- sst:

  numeric observed sea-surface temperature in degrees Celsius.

- bleaching:

  integer count of bleached colonies, alias of `y`.

- colonies:

  integer number of colonies inspected, the binomial trials.

## Details

A naive AR(1) treats every consecutive survey as one step. It gives a
one-month gap and a six-month gap the same persistence.
`trend_formula = ~ CAR()` with the supplied `time` column decays the
state by the length of each gap. Its `ar1_trend` measures the
persistence over the median gap between surveys, three months here. The
simulated value is `0.9^3 = 0.73`.

True generative components, on the logit scale:

- An intercept of -0.85, which puts a typical survey near 30% bleached.
  Each reef adds its own centred intercept.

- Latent CAR(1) state per reef with `phi = 0.9` and `sigma = 0.3`,
  propagated through
  `trend[t] = phi^dt * trend[t-1] + N(0, sigma * sqrt((1 - phi^(2*dt)) / (1 - phi^2)))`.

- Shared smooth nonlinear effect of SST, evaluated through a thin-plate
  basis.

- Binomial observations: `bleaching ~ Binomial(colonies, p)`.

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
mvgam_data(coral_surveys, formula = y | trials(colonies) ~ s(sst),
           family = binomial())
#> ✔ Data check passed for family 'binomial (link = logit)'.
#> • series: 3 levels
#> • time:   1 to 178
#> • n obs:  150 (0 NA)


# \dontrun{
# A model with the structure the data were simulated from
mod <- mvgam(
  formula = y | trials(colonies) ~ series + s(sst, k = 8),
  trend_formula = ~ CAR(),
  data = coral_surveys,
  family = binomial(),
  chains = 2, silent = 2
)
#> Warning: 29 of 2000 (1.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
summary(mod)
#>  Family: binomial 
#>   Links: mu = logit 
#> Formula: y | trials(colonies) ~ series + s(sst, k = 8) 
#>    Data: coral_surveys (Number of observations: 150) 
#>  Series: 3 
#>  Trends: CAR 
#>   Draws: 2 chains, each with iter = 2000; warmup = 1000; thin = 1; 
#>          total post-warmup draws = 2000
#> 
#> == Observation Model ==
#> Smoothing Spline Hyperparameters:
#>             Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sds(ssst_1)     2.33      1.25     0.68     5.48 1.00      949     1296
#> 
#> Regression Coefficients:
#>                Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept         -0.64      0.19    -1.01    -0.29 1.00     1550     1580
#> seriesmyrmidon    -0.83      0.55    -1.79     0.47 1.01      869      621
#> seriespixie       -0.61      0.43    -1.48     0.19 1.00     1055      907
#> ssst_1            12.80      4.39     5.87    23.20 1.00     1209     1037
#> 
#> == Trend Model ==
#> Trend Specific Parameters:
#>                       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
#> sigma_trend[flynn]        0.54      0.11     0.35     0.78 1.00      740
#> sigma_trend[myrmidon]     0.44      0.11     0.26     0.71 1.00      958
#> sigma_trend[pixie]        0.47      0.10     0.31     0.68 1.00     1076
#> ar1_trend[flynn]          0.48      0.20     0.08     0.85 1.00      675
#> ar1_trend[myrmidon]       0.82      0.12     0.53     0.98 1.00      806
#> ar1_trend[pixie]          0.76      0.12     0.49     0.96 1.00     1003
#>                       Tail_ESS
#> sigma_trend[flynn]         945
#> sigma_trend[myrmidon]     1234
#> sigma_trend[pixie]        1178
#> ar1_trend[flynn]           987
#> ar1_trend[myrmidon]       1019
#> ar1_trend[pixie]          1039
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
