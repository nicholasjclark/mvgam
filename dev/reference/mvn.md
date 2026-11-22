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
#> Warning: 855 of 2000 (43.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
#> Warning: 2 of 2 chains had an E-BFMI less than 0.3.
#> See https://mc-stan.org/misc/warnings for details.
summary(fit)
#>  Family: mvn 
#>   Links: mu = identity 
#> Formula: y ~ env * series 
#>    Data: simdat$data_train (Number of observations: 400) 
#> Species: 5 
#>  Trends: 2 latent factors 
#>   Draws: 2 chains, each with iter = 2000; warmup = 1000; thin = 1; 
#>          total post-warmup draws = 2000
#> 
#> == Observation Model ==
#> Regression Coefficients:
#>                Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept         -0.30      0.16    -0.60    -0.11 1.44        4       28
#> env                0.30      0.10     0.14     0.54 1.39       27      188
#> seriessp_2         1.31      0.24     1.02     1.72 1.44        4       68
#> seriessp_3         1.19      0.22     0.91     1.61 1.40        4       46
#> seriessp_4         1.00      0.29     0.67     1.57 1.52        4       52
#> seriessp_5         1.99      0.15     1.80     2.29 1.35        5       78
#> env:seriessp_2    -1.54      0.14    -1.87    -1.32 1.36       17      288
#> env:seriessp_3    -0.74      0.15    -1.07    -0.42 1.13      131      149
#> env:seriessp_4    -1.38      0.21    -1.88    -1.04 1.50       16      234
#> env:seriessp_5    -0.76      0.11    -1.01    -0.54 1.14      151      284
#> 
#> Further Distributional Parameters:
#>        Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Psi[1]     0.52      0.12     0.21     0.68 1.28        6       56
#> Psi[2]     0.48      0.04     0.42     0.57 1.18       16      104
#> Psi[3]     0.51      0.19     0.11     0.78 1.53        4       18
#> Psi[4]     0.38      0.28     0.10     0.86 1.94        3       14
#> Psi[5]     0.56      0.04     0.48     0.64 1.27      217      393
#> 
#> == Trend Model ==
#> Factor Loadings:
#>                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Z_tilde[sp_1,1]     0.85      0.10     0.66     1.05 1.04       63      144
#> Z_tilde[sp_2,1]    -0.31      0.06    -0.41    -0.18 1.13       11      141
#> Z_tilde[sp_3,1]    -0.07      0.11    -0.30     0.15 1.04       63      159
#> Z_tilde[sp_4,1]    -1.11      0.15    -1.35    -0.77 1.17       11      176
#> Z_tilde[sp_5,1]     0.05      0.08    -0.11     0.21 1.11       12       46
#> Z_tilde[sp_2,2]     0.08      0.05     0.00     0.18 1.10       15      234
#> Z_tilde[sp_3,2]     0.59      0.53    -0.96     1.04 1.30       17       46
#> Z_tilde[sp_4,2]    -0.62      0.49    -1.01     0.84 1.34        5      118
#> Z_tilde[sp_5,2]    -0.23      0.19    -0.43     0.36 1.12      225      409
#> 
#> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).
#> 
#> Next steps:
#>   - `pp_check(fit)`: posterior predictive checks
#>   - `shared_variation(fit)`: covariance the factors imply, unchanged by rotation
#>   - `residual_cor(fit)`: implied correlations among species
#>   - `ordinate(fit)`: biplot of the factor scores and loadings
#>   - `loo(fit)` / `loo_compare(...)`: model fit + comparison
#>   - `conditional_effects(fit)`: covariate effects
#> Use `how_to_cite(fit)` for a methods paragraph and `methods_md(fit)` for its equations.

# The species-by-species residual correlations, against the values
# the simulation used
round(residual_cor(fit)$cor, 2)
#>       sp_1  sp_2  sp_3  sp_4  sp_5
#> sp_1  1.00 -0.46 -0.06 -0.65  0.07
#> sp_2 -0.46  1.00  0.13  0.35 -0.10
#> sp_3 -0.06  0.13  1.00 -0.38 -0.38
#> sp_4 -0.65  0.35 -0.38  1.00  0.18
#> sp_5  0.07 -0.10 -0.38  0.18  1.00
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
