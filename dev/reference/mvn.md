# Multivariate normal family for several continuous responses

For continuous responses measured together on each unit, such as the
biomass of several species at a site. The data hold one row per response
component at each unit. Latent factors shared by a unit's components
give their residuals the covariance `Z Z' + diag(Psi^2)`, where `Z`
holds the factor loadings and `Psi` each component's residual standard
deviation (Hui et al. 2015; Niku et al. 2019). Given the factors, each
row is normal with mean `mu` and standard deviation `Psi`.

## Usage

``` r
mvn()
```

## Value

A family object to pass to
[`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md)
or
[`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md).

## Details

[`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md)
fits the family, taking the unit and component columns and the number of
factors `n_lv`.
[`residual_cor()`](https://nicholasjclark.github.io/mvgam/dev/reference/residual_cor.md)
summarises the residual correlations and
[`shared_variation()`](https://nicholasjclark.github.io/mvgam/dev/reference/shared_variation.md)
the part the factors share. `Psi` takes the prior `exponential(1)`,
which suits responses near unit scale. `prior(..., class = Psi)` sets
another.

With `K` components the data separate the factor part from the residual
part only when `(K - n_lv)^2 >= K + n_lv`. Fewer factors or more
components meet the bound.
[`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md)
warns when it fails.

## References

Hui, F. K. C., Taskinen, S., Pledger, S., Foster, S. D. and Warton, D.
I. (2015). Model-based approaches to unconstrained ordination. *Methods
in Ecology and Evolution*, 6(4):399-411.
[doi:10.1111/2041-210X.12236](https://doi.org/10.1111/2041-210X.12236)

Niku, J., Brooks, W., Herliansyah, R., Hui, F. K. C., Taskinen, S. and
Warton, D. I. (2019). Efficient estimation of generalized linear latent
variable models. *PLOS ONE*, 14(5):e0216129.
[doi:10.1371/journal.pone.0216129](https://doi.org/10.1371/journal.pone.0216129)

Heaps, S. E. and Jermyn, I. H. (2024). Structured prior distributions
for the covariance matrix in latent factor models. *Statistics and
Computing*, 34:143.
[doi:10.1007/s11222-024-10454-0](https://doi.org/10.1007/s11222-024-10454-0)

## See also

Other observation families:
[`beta_nb()`](https://nicholasjclark.github.io/mvgam/dev/reference/beta_nb.md),
[`categ()`](https://nicholasjclark.github.io/mvgam/dev/reference/categ.md),
[`com_binomial()`](https://nicholasjclark.github.io/mvgam/dev/reference/com_binomial.md),
[`diri()`](https://nicholasjclark.github.io/mvgam/dev/reference/diri.md),
[`multi()`](https://nicholasjclark.github.io/mvgam/dev/reference/multi.md),
[`mvgam_families`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_families.md),
[`mvt()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvt.md),
[`nmix()`](https://nicholasjclark.github.io/mvgam/dev/reference/nmix.md),
[`occ()`](https://nicholasjclark.github.io/mvgam/dev/reference/occ.md),
[`tweedie()`](https://nicholasjclark.github.io/mvgam/dev/reference/tweedie.md)

## Examples

``` r
# \dontrun{
# Five species at 80 sites whose residuals share two latent
# factors
simdat <- sim_jsdgam(family = mvn(), n_species = 5L, n_sites = 80L,
                     n_lv = 2L, seed = 1L)

fit <- jsdgam(
  formula        = y ~ env * series,
  factor_formula = ~ -1,
  data           = simdat$data_train,
  unit           = time, species = series,
  family         = mvn(),
  n_lv           = 2L,
  chains         = 2L,
  silent         = 2
)
#> Warning: 38 of 2000 (2.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
#> Warning: 1 of 2000 (0.0%) transitions hit the maximum treedepth limit of 10.
#> See https://mc-stan.org/misc/warnings for details.
#> Warning: 2 of 2 chains had an E-BFMI less than 0.3.
#> See https://mc-stan.org/misc/warnings for details.
summary(fit)
#>  Family: mvn 
#>   Links: mu = identity 
#> Formula: y ~ env * series 
#>    Data: simdat$data_train (Number of observations: 400) 
#>  Series: 5 
#>  Trends: ZMVN; formula: ~-1 
#>   Draws: 2 chains, each with iter = 2000; warmup = 1000; thin = 1; 
#>          total post-warmup draws = 2000
#> 
#> == Observation Model ==
#> Regression Coefficients:
#>                Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept         -0.42      0.11    -0.64    -0.21 1.01      258      604
#> env                0.34      0.12     0.11     0.55 1.00      261      515
#> seriessp_2         1.49      0.16     1.19     1.80 1.01      270      552
#> seriessp_3         1.35      0.16     1.03     1.68 1.00      326      528
#> seriessp_4         1.18      0.25     0.70     1.68 1.01      267      507
#> seriessp_5         2.09      0.13     1.84     2.36 1.01      289      518
#> env:seriessp_2    -1.59      0.16    -1.89    -1.27 1.00      281      488
#> env:seriessp_3    -0.76      0.18    -1.09    -0.41 1.00      360      602
#> env:seriessp_4    -1.45      0.26    -1.95    -0.95 1.00      255      633
#> env:seriessp_5    -0.78      0.14    -1.05    -0.50 1.00      301      841
#> 
#> Further Distributional Parameters:
#>        Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Psi[1]     0.42      0.15     0.13     0.66 1.13       12       97
#> Psi[2]     0.49      0.04     0.42     0.58 1.00      597     1209
#> Psi[3]     0.39      0.16     0.10     0.72 1.10       14       24
#> Psi[4]     0.63      0.19     0.22     0.93 1.15       11      107
#> Psi[5]     0.56      0.05     0.46     0.66 1.00      861      877
#> 
#> == Trend Model ==
#> Factor Loadings:
#>                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Z_tilde[sp_1,1]     0.89      0.11     0.68     1.10 1.05       34      315
#> Z_tilde[sp_2,1]    -0.28      0.07    -0.42    -0.16 1.00     1178     1400
#> Z_tilde[sp_3,1]    -0.08      0.12    -0.32     0.16 1.00     1281     1328
#> Z_tilde[sp_4,1]    -1.03      0.16    -1.35    -0.73 1.03      110      821
#> Z_tilde[sp_5,1]     0.02      0.08    -0.13     0.18 1.01     1369     1172
#> Z_tilde[sp_2,2]     0.07      0.05     0.00     0.19 1.00     1046     1624
#> Z_tilde[sp_3,2]     0.54      0.70    -1.01     1.06 1.01      554      897
#> Z_tilde[sp_4,2]    -0.42      0.56    -0.93     0.85 1.01     1502     1835
#> Z_tilde[sp_5,2]    -0.20      0.25    -0.46     0.38 1.00     1422     1842
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

# The species-by-species residual correlations, against the values
# the simulation used
round(residual_cor(fit)$cor, 2)
#>       sp_1  sp_2  sp_3  sp_4  sp_5
#> sp_1  1.00 -0.44 -0.07 -0.65  0.03
#> sp_2 -0.44  1.00  0.13  0.31 -0.07
#> sp_3 -0.07  0.13  1.00 -0.38 -0.44
#> sp_4 -0.65  0.31 -0.38  1.00  0.21
#> sp_5  0.03 -0.07 -0.44  0.21  1.00
round(simdat$truth$residual_cor, 2)
#>       sp_1  sp_2  sp_3  sp_4  sp_5
#> sp_1  1.00 -0.46 -0.05 -0.64  0.04
#> sp_2 -0.46  1.00  0.18  0.35 -0.12
#> sp_3 -0.05  0.18  1.00 -0.37 -0.45
#> sp_4 -0.64  0.35 -0.37  1.00  0.23
#> sp_5  0.04 -0.12 -0.45  0.23  1.00

# Next, shared_variation(fit) gives the part the factors share
# }
```
