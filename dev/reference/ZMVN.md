# Correlated latent effects with no time dependence

A latent effect for each unit, such as a site, plot or time point, drawn
from a multivariate normal that correlates the series. Unlike the other
trends, the effect at one unit does not depend on the previous one. The
constructor records its arguments. The model evaluates them against the
data.

## Usage

``` r
ZMVN(
  time = NA,
  series = NA,
  gr = NA,
  subgr = NA,
  n_lv = NULL,
  cor = TRUE,
  trend_map = NULL,
  df = Inf
)
```

## Arguments

- time:

  The unquoted name of the `numeric` or `integer` variable in `data`
  that indexes the units, such as sites, plots or times. Defaults to
  `time`, as for the other trends.

  The values may have any spacing. Each unit's effects follow \\x\_{u,
  :} \sim MVN(0, \Sigma)\\, with \\\Sigma\\ indexed by series alone. The
  gap between two units plays no part. That suits sites, dropped units,
  k-fold refits and irregular sampling.

- series:

  The unquoted name of the `factor` variable in `data` that names each
  row's series. Its levels fix the order of the series. A trend needs
  this column even for a single series. Defaults to `series`.

- gr:

  An optional grouping `factor` in `data`. Each of its levels takes its
  own correlation matrix among the latent effects. Each matrix shrinks
  toward a global one by the weight `alpha_cor_trend`, as
  [trend_constructors](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
  sets out. Without `subgr`, the series column names the series within
  each level.

- subgr:

  A subgrouping `factor` in `data` naming the units within each level of
  `gr`, from which the series are built as
  [trend_constructors](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
  describes. For species (`species`) counted at sites (`site`) in
  regions (`region`), `time = site`, `gr = region` and `subgr = species`
  correlate the species' latent effects within each region.

- n_lv:

  The number of latent factors to estimate for dynamic factor models.
  When `n_lv` is smaller than the number of series, the latent processes
  are modelled as `n_lv` factors with estimated loadings onto the
  series. Defaults to `NULL`, in which case one latent process is used
  per series.

- cor:

  `Logical`. Correlate the innovations of different series? `TRUE`
  estimates a full covariance matrix of the innovations when the data
  hold more than one series.
  [`AR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
  and
  [`RW()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
  default to `NULL`. That means `FALSE` for an ungrouped trend and
  `TRUE` when `gr` names a grouping variable.
  [`VAR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md),
  `ZMVN()` and grouped
  [`AR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
  and
  [`RW()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
  trends always estimate correlations. They refuse `cor = FALSE`.
  [`CAR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
  defaults to `FALSE`.

- trend_map:

  Optional specification of which latent process each series maps onto.
  It fixes the loadings of a dynamic factor model. See
  [`mvgam`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md)
  for the accepted formats. Defaults to `NULL`.

- df:

  Degrees of freedom for the latent process innovations. Defaults to
  `Inf`, which gives Gaussian innovations: a t with infinite degrees of
  freedom is a normal. Set `df = NA` to estimate them, or supply a
  number above `2` to fix them.

  Finite degrees of freedom let the latent process absorb an occasional
  large shock while `sigma_trend` stays at the scale of ordinary
  variation. That suits boom-and-bust population series and outbreak
  dynamics. The innovations follow a multivariate t. Because it shares
  one innovation *scale* across the series at each time point, large
  innovations tend to occur together. Each series keeps its own
  direction and magnitude. A shock in one series says nothing about the
  sign of the others. One series can also stay unmoved by an event that
  moves its neighbours. `cor` sets the correlation between series
  exactly as it does for Gaussian innovations. The degrees of freedom
  set how heavy the tails of the innovations are.

  When `df = NA` the degrees of freedom are estimated as `nu_trend`,
  with a default `gamma(4, 0.3)` prior. The `prior` argument replaces
  it, for example `prior(gamma(2, 0.1), class = "nu_trend")`.
  [`get_prior`](https://nicholasjclark.github.io/mvgam/dev/reference/get_prior.md)
  lists `nu_trend` with the other trend classes.

  Three caveats apply:

  - A fixed value must exceed `2`. Below it the innovations have no
    finite variance. An autoregressive trend then has no stationary
    covariance to start from.

  - The first states of an autoregressive trend take the stationary mean
    and covariance. Under t innovations the true stationary distribution
    has heavier tails than that.

  - Only the tails of the latent process inform the degrees of freedom.
    Below roughly 200 time points their posterior stays close to the
    prior.

  [`VAR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
  samples its states directly and
  [`PW()`](https://nicholasjclark.github.io/mvgam/dev/reference/piecewise_trends.md)
  has no innovations. Neither takes `df`.

## Value

An object of class `mvgam_trend`.

## Identification

A factor model with sampled loadings fits equally well under any
reordering or sign change of its factors. Some models also fit equally
well under any rotation. mvgam reports one version, chosen by the
dynamics of the factors:

- [`AR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
  factors take their own coefficients, as do
  [`RW()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
  and
  [`AR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
  factors with `ma = TRUE`. A rotation would mix those coefficients.
  After sampling, the factors of every draw are reordered and signed to
  agree with one reference. The loadings `Z`, the factor paths
  `lv_trend` and the coefficients `ar1_trend` or `theta1_trend` move
  together. Factors are numbered by the variance they explain, largest
  first.

- [`RW()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md),
  [`VAR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
  and `ZMVN()` factors without a moving-average term are unchanged by
  rotation. So are
  [`AR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
  factors with `coef_sharing = "shared"` and no moving-average term.
  Following Heaps & Jermyn (2024), generated quantities rotate each draw
  to lower-triangular loadings `Z_tilde` with a non-negative diagonal.
  The factor paths rotate to `lv_trend_tilde`. A VAR factor model
  rotates its coefficients to `Phi_trend_tilde`.

Post-fit methods use the identified loadings and factor paths.
[`shared_variation()`](https://nicholasjclark.github.io/mvgam/dev/reference/shared_variation.md)
and
[`residual_cor()`](https://nicholasjclark.github.io/mvgam/dev/reference/residual_cor.md)
are the same under every version.

A `trend_map` keeps its fixed entries on `Z` exactly. A `by = lv_axis()`
smooth ties each factor to its own covariate effect. Neither takes a
rotation. For a model with every loading sampled, the `loadings_prior`
argument of
[`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md)
replaces the default iid Student-t prior on `Z` with a structured
matrix-normal prior. That prior is built from per-series features,
pairwise distance matrices or both. See
[`mvgam`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md)
for its arguments.

Setting `loadings_prior = "mgp"` (or
`loadings_prior = list(column_shrinkage = "mgp")`) switches the column
scaling to the multiplicative gamma process prior of Bhattacharya &
Dunson (2011), which shrinks later columns of `Z` toward zero with
increasing strength. `n_lv` is then a truncation ceiling on the factor
count.
[`active_factors`](https://nicholasjclark.github.io/mvgam/dev/reference/active_factors.md)
reports the posterior distribution of the number of active factors.

## References

Heaps, S. E. and Jermyn, I. H. (2024). Structured prior distributions
for the covariance matrix in latent factor models. *Statistics and
Computing*, 34:143.
[doi:10.1007/s11222-024-10454-0](https://doi.org/10.1007/s11222-024-10454-0)

## See also

[`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md)
fits `ZMVN()` factors across species.
[`residual_cor()`](https://nicholasjclark.github.io/mvgam/dev/reference/residual_cor.md)
and
[`ordinate()`](https://nicholasjclark.github.io/mvgam/dev/reference/ordinate.md)
interpret the fitted correlations. The [dynamic factor
vignette](https://nicholasjclark.github.io/mvgam/dev/articles/dfm.html)
builds factor models with a partial `trend_map`. The [integrated
distribution model
article](https://nicholasjclark.github.io/mvgam/dev/articles/idm.html)
shares one latent process across several observation families.

Other trend models:
[`LLT()`](https://nicholasjclark.github.io/mvgam/dev/reference/LLT.md),
[`PW()`](https://nicholasjclark.github.io/mvgam/dev/reference/piecewise_trends.md),
[`RW()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md),
[`list_trend_types()`](https://nicholasjclark.github.io/mvgam/dev/reference/list_trend_types.md),
[`lv_axis()`](https://nicholasjclark.github.io/mvgam/dev/reference/lv_axis.md)

## Examples

``` r
# \dontrun{
# Simulate counts of four series whose latent effects are correlated
# at each time point but independent over time.
set.seed(2)
simdat <- sim_mvgam(
  family       = poisson(),
  n_series     = 4L,
  n_timepoints = 120L,
  trend_model  = ZMVN(),
  prop_trend   = 0.9
)

mod <- mvgam(
  y ~ 1,
  trend_formula = ~ ZMVN(),
  data          = simdat$data_train,
  family        = poisson(),
  chains        = 2,
  silent        = 2
)
summary(mod)
#>  Family: poisson 
#>   Links: mu = log 
#> Formula: y ~ 1 
#>    Data: simdat$data_train (Number of observations: 360) 
#>  Series: 4 
#>  Trends: ZMVN 
#>   Draws: 2 chains, each with iter = 2000; warmup = 1000; thin = 1; 
#>          total post-warmup draws = 2000
#> 
#> == Observation Model ==
#> Regression Coefficients:
#>           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept     1.68      0.06     1.55     1.81 1.00     1300     1263
#> 
#> == Trend Model ==
#> Trend Specific Parameters:
#>                       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
#> sigma_trend[series_1]     0.75      0.08     0.60     0.92 1.00      928
#> sigma_trend[series_2]     0.69      0.07     0.55     0.84 1.00      960
#> sigma_trend[series_3]     0.82      0.09     0.68     1.02 1.00     1063
#> sigma_trend[series_4]     0.76      0.08     0.62     0.93 1.00     1000
#>                       Tail_ESS
#> sigma_trend[series_1]     1355
#> sigma_trend[series_2]     1336
#> sigma_trend[series_3]     1396
#> sigma_trend[series_4]     1435
#> 
#> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).
#> 
#> Next steps:
#>   - `pp_check(fit)`: posterior predictive checks
#>   - `residual_cor(fit)`: implied cross-series correlations
#>   - `loo(fit)` / `loo_compare(...)`: model fit + comparison
#> Use `how_to_cite(fit)` for a methods paragraph and `methods_md(fit)` for its equations.

# The correlations among the series that ZMVN() estimates
rc <- residual_cor(mod)
rc
#> Residual correlations from an mvgam fit
#>   Source     : correlated innovations
#>   Covariance : stationary
#>   Series     : 4
#>   Mean |r_ij| (i != j): 0.35 [0.22, 0.48]
#>   Pairs with prob_nonzero > 0.95: 6 / 6
plot(rc)


# Next: jsdgam() fits ZMVN() factors across many species
# }
```
