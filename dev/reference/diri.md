# Dirichlet family for compositions

For proportions measured together on each unit that sum to one, such as
the cover shares of several species at a site. The data hold one row per
component at each unit, whose responses sum to one within the unit. Each
unit's vector of `K` shares is Dirichlet with concentrations
`softmax(mu) * phi`. `mu` holds the components' linear predictors. `phi`
sets how tightly the shares gather around `softmax(mu)` (Aitchison
1982).

## Usage

``` r
diri()
```

## Value

A family object to pass to
[`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md)
or
[`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md).

## Details

[`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md)
fits the family, taking the unit and component columns and the number of
latent factors `n_lv`. The factors give the components correlated
departures from their expected shares (Warton et al. 2015).

## Identification

The softmax is unchanged when the same constant is added to every
component. The data then identify neither a shift of the linear
predictors that every component shares nor a shift of the loadings that
every component shares. mvgam removes both:

- The first component is the reference. Its linear predictor is
  subtracted from every component's before the softmax, as in brms's
  `dirichlet()` family. A term that acts on every component alike, such
  as `Intercept` or `env` in `y ~ env * series`, then cancels. Its
  posterior is its prior, `student_t(3, 0, 2.5)` by default. The
  `series` terms are contrasts with the first component.
  [`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md)
  warns when the formula has no term that varies by component.

- Each column of the loadings `Z` sums to zero, through Stan's
  `sum_to_zero_vector` type.

A `loadings_prior` combines with that constraint.

## Regression on `phi`

`bf(y ~ env * series, phi ~ env)` models the concentration. A unit's
density takes one `phi`. Its covariates must take one value per unit.
mvgam refuses a covariate that varies within a unit. The coefficients of
`phi` take a flat prior by default.
`prior(student_t(3, 0, 2.5), class = b, dpar = phi)` regularises them.

## Post-fit conventions

The density is joint over a unit's components, which makes the unit the
grain of
[`log_lik()`](https://mc-stan.org/rstantools/reference/log_lik.html),
[`loo()`](https://mc-stan.org/loo/reference/loo.html) and
[`waic()`](https://mc-stan.org/loo/reference/waic.html) (Vehtari et al.
2017).
[`log_lik()`](https://mc-stan.org/rstantools/reference/log_lik.html)
gives each unit's log density on its first row and `NA` on the others.
[`loo()`](https://mc-stan.org/loo/reference/loo.html) then counts each
unit once.
[`loo_compare()`](https://mc-stan.org/loo/reference/loo_compare.html)
needs every model it compares scored at that grain.

`posterior_linpred()` gives `mu` per row before the reference
subtraction. `posterior_epred()` gives the expected shares, which sum to
one within each unit.

## References

Aitchison, J. (1982). The statistical analysis of compositional data.
*Journal of the Royal Statistical Society Series B*, 44(2):139-177.

Vehtari, A., Gelman, A. and Gabry, J. (2017). Practical Bayesian model
evaluation using leave-one-out cross-validation and WAIC. *Statistics
and Computing*, 27:1413-1432.
[doi:10.1007/s11222-016-9696-4](https://doi.org/10.1007/s11222-016-9696-4)

Warton, D. I., Blanchet, F. G., O'Hara, R. B., Ovaskainen, O., Taskinen,
S., Walker, S. C. and Hui, F. K. C. (2015). So many variables: joint
modeling in community ecology. *Trends in Ecology and Evolution*,
30(12):766-779.
[doi:10.1016/j.tree.2015.09.007](https://doi.org/10.1016/j.tree.2015.09.007)

Heaps, S. E. and Jermyn, I. H. (2024). Structured prior distributions
for the covariance matrix in latent factor models. *Statistics and
Computing*, 34:143.
[doi:10.1007/s11222-024-10454-0](https://doi.org/10.1007/s11222-024-10454-0)

## See also

Other observation families:
[`beta_nb()`](https://nicholasjclark.github.io/mvgam/dev/reference/beta_nb.md),
[`categ()`](https://nicholasjclark.github.io/mvgam/dev/reference/categ.md),
[`com_binomial()`](https://nicholasjclark.github.io/mvgam/dev/reference/com_binomial.md),
[`multi()`](https://nicholasjclark.github.io/mvgam/dev/reference/multi.md),
[`mvgam_families`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_families.md),
[`mvn()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvn.md),
[`mvt()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvt.md),
[`nmix()`](https://nicholasjclark.github.io/mvgam/dev/reference/nmix.md),
[`occ()`](https://nicholasjclark.github.io/mvgam/dev/reference/occ.md),
[`tweedie()`](https://nicholasjclark.github.io/mvgam/dev/reference/tweedie.md)

## Examples

``` r
# \dontrun{
# Shares of three categories at 60 sites along an environmental
# gradient
simdat <- sim_jsdgam(family = diri(), n_species = 3L, n_sites = 60L,
                     seed = 2L)
head(simdat$data_train)
#>   site        env series          y time
#> 1    1  0.7079547   sp_1 0.01630229    1
#> 2    1  0.7079547   sp_2 0.14446708    1
#> 3    1  0.7079547   sp_3 0.83923063    1
#> 4    2 -0.2396980   sp_1 0.10325166    2
#> 5    2 -0.2396980   sp_2 0.10469336    2
#> 6    2 -0.2396980   sp_3 0.79205498    2

# `env * series` gives each category its own slope
fit <- jsdgam(
  formula        = y ~ env * series,
  factor_formula = ~ -1,
  data           = simdat$data_train,
  unit           = time, species = series,
  family         = diri(),
  n_lv           = 2L,
  chains         = 2L,
  silent         = 2
)

# The species terms are contrasts with the first species. `Intercept`
# and `env` act on every species alike. The softmax cancels them. Their
# posteriors repeat the prior.
summary(fit)
#>  Family: diri 
#>   Links: mu = identity; phi = log 
#> Formula: y ~ env * series 
#>    Data: simdat$data_train (Number of observations: 180) 
#>  Series: 3 
#>  Trends: ZMVN; formula: ~-1 
#>   Draws: 2 chains, each with iter = 2000; warmup = 1000; thin = 1; 
#>          total post-warmup draws = 2000
#> 
#> == Observation Model ==
#> Regression Coefficients:
#>                Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept         -1.09      5.80    -8.79     7.48 1.01     1679      561
#> env               -0.01      3.98    -8.39     8.06 1.00     3147      870
#> seriessp_2         1.74      0.18     1.39     2.10 1.00     1246     1501
#> seriessp_3         2.40      0.27     1.86     2.92 1.00     1151     1448
#> env:seriessp_2    -0.05      0.14    -0.31     0.25 1.00      937     1019
#> env:seriessp_3     1.23      0.22     0.82     1.66 1.00      842      982
#> 
#> Further Distributional Parameters:
#>     Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> phi    31.71     12.03    16.58    61.61 1.02      203      280
#> 
#> == Trend Model ==
#> Factor Loadings:
#>                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Z_tilde[sp_1,1]     0.95      0.12     0.73     1.20 1.00      471      988
#> Z_tilde[sp_2,1]    -0.05      0.07    -0.19     0.07 1.00      744     1159
#> Z_tilde[sp_3,1]    -0.89      0.10    -1.12    -0.71 1.00      733     1020
#> Z_tilde[sp_2,2]     0.18      0.10     0.01     0.36 1.02      212      584
#> Z_tilde[sp_3,2]    -0.18      0.10    -0.36    -0.01 1.02      212      584
#> 
#> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).
#> 
#> Next steps:
#>   - `pp_check(fit)`: posterior predictive checks
#>   - `shared_variation(fit)`: factor-implied Delta = Z Sigma Z' (rotation-invariant)
#>   - `residual_cor(fit)`: implied cross-series correlations
#>   - `loo(fit)` / `loo_compare(...)`: model fit + comparison
#>   - `conditional_effects(fit)`: covariate effects
#> Use `how_to_cite(fit)` for a methods paragraph and `methods_md(fit)` for its equations.

# The expected shares move along the gradient
conditional_effects(fit, effects = "env:series")


# Each site's expected shares sum to one
pe <- posterior_epred(fit, ndraws = 50L)
first_site <- which(simdat$data_train$time == 1L)
rowSums(pe[1:5, first_site])
#> [1] 1 1 1 1 1
# }
```
