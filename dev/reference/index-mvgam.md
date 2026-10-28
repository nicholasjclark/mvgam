# Parameter names of a fitted mvgam model

`variables()` lists the name of every quantity in a fitted model's
draws.
[`hypothesis.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/hypothesis.mvgam.md),
[`as_draws()`](https://mc-stan.org/posterior/reference/draws.html),
[`as.data.frame.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_draws.md),
[`mcmc_plot.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mcmc_plot.mvgam.md)
and
[`posterior_average.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_average.mvgam.md)
accept these names.
[`summary.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/summary.mvgam.md)
and
[`posterior_summary()`](https://paulbuerkner.com/brms/reference/posterior_summary.html)
label their rows with them.

## Usage

``` r
# S3 method for class 'mvgam'
variables(x, ...)
```

## Arguments

- x:

  A fitted `mvgam` object.

- ...:

  Unused. Anything passed here is refused.

## Value

A character vector of parameter names.

## How parameters are named

Observation parameters take their brms names, such as `b_x`, `sds_sx_1`,
`sd_site__Intercept` and `r_site[a,Intercept]`. The trend model's
parameters take the same names with a `_trend` suffix, such as
`b_x_trend` or `sds_sx_1_trend`.

An index that runs over the series takes the series' name, as brms
labels a grouping factor's levels. A model with series `DM` and `DO` has
`sigma_trend[DM]`, `ar1_trend[DM]` and the innovation covariance
`Sigma_trend[DM,DO]`. A VAR's coefficients keep the lag as a number, as
in `Phi_trend[1,DM,DO]`, as do the changepoints of
[`PW()`](https://nicholasjclark.github.io/mvgam/dev/reference/piecewise_trends.md)
in `delta_trend[3,DM]`. Factor loadings are `Z_tilde[DM,1]`, with the
factor as the second index. In a factor model the dynamics belong to the
factors, as in `ar1_trend[1]`. The latent state matrices keep numeric
indices, as in `trend[12,1]` for time 12 of the first series.

A bare name such as `variable = "sigma_trend"` selects every element of
a vector, as in posterior. Because `variables()` includes the latent
states, its output runs to hundreds of names on most fits.
[`grep()`](https://rdrr.io/r/base/grep.html) narrows it, as in
`grep("^(sigma|ar1)_trend", variables(fit), value = TRUE)`.

## See also

[mvgam_draws](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_draws.md)
to extract draws by name,
[`hypothesis.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/hypothesis.mvgam.md)
to test them,
[`summary.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/summary.mvgam.md)
and
[`posterior_summary()`](https://paulbuerkner.com/brms/reference/posterior_summary.html)
to summarise them.

Other posterior summaries:
[`VarCorr.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/VarCorr.mvgam.md),
[`bibtex()`](https://nicholasjclark.github.io/mvgam/dev/reference/bibtex.md),
[`how_to_cite.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/how_to_cite.mvgam.md),
[`hypothesis.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/hypothesis.mvgam.md),
[`mcmc_plot.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mcmc_plot.mvgam.md),
[`methods_md()`](https://nicholasjclark.github.io/mvgam/dev/reference/methods_md.md),
[`mvgam_draws`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_draws.md),
[`pairs.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/pairs.mvgam.md),
[`posterior_interval.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_interval.mvgam.md),
[`ranef.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/ranef.mvgam.md),
[`smooths.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/smooths.mvgam.md),
[`summary.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/summary.mvgam.md)

## Author

Nicholas J Clark

## Examples

``` r
# \dontrun{
set.seed(13)
simdat <- sim_mvgam(n_series = 2L, trend_model = AR())
mod <- mvgam(y ~ x,
             trend_formula = ~ AR(),
             data = simdat$data_train,
             chains = 2, silent = 2)
#> Warning: 8 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
#> Warning: 2 of 2 chains had an E-BFMI less than 0.3.
#> See https://mc-stan.org/misc/warnings for details.

# The trend dynamics, indexed by series
grep("^(sigma|ar1)_trend", variables(mod), value = TRUE)
#> [1] "sigma_trend[series_1]" "sigma_trend[series_2]" "ar1_trend[series_1]"  
#> [4] "ar1_trend[series_2]"  

# Every element of a vector by its bare name
as_draws_df(mod, variable = "sigma_trend")
#> # A draws_df: 1000 iterations, 2 chains, and 2 variables
#>    sigma_trend[series_1] sigma_trend[series_2]
#> 1                  0.272                  0.82
#> 2                  0.097                  0.63
#> 3                  0.196                  0.42
#> 4                  0.108                  0.87
#> 5                  0.253                  0.77
#> 6                  0.314                  0.55
#> 7                  0.250                  0.50
#> 8                  0.252                  0.51
#> 9                  0.487                  0.55
#> 10                 0.129                  0.73
#> # ... with 1990 more draws
#> # ... hidden reserved variables {'.chain', '.iteration', '.draw'}

# Two series' autoregressive coefficients compared
hypothesis(mod, "ar1_trend[series_1] > ar1_trend[series_2]",
           class = NULL)
#> Hypothesis Tests for class :
#>                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
#> 1 (ar1_trend[series... > 0    -0.26      0.41    -1.04     0.31       0.39
#>   Post.Prob Star
#> 1      0.28     
#> ---
#> 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
#> '*': For one-sided hypotheses, the posterior probability exceeds 95%;
#> for two-sided hypotheses, the value tested against lies outside the 95%-CI.
#> Posterior probabilities of point hypotheses assume equal prior probabilities.

# Next, plot chosen parameters with mcmc_plot(mod, variable = ...)
# }
```
