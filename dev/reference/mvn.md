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
#> Warning: 143 of 2000 (7.0%) transitions ended with a divergence.
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
#> Intercept         -0.41      0.12    -0.63    -0.17 1.01      252      357
#> env                0.34      0.12     0.11     0.60 1.03       83      160
#> seriessp_2         1.47      0.16     1.15     1.79 1.01      268      355
#> seriessp_3         1.36      0.16     1.01     1.67 1.01      310      472
#> seriessp_4         1.15      0.26     0.64     1.65 1.02      131      398
#> seriessp_5         2.07      0.14     1.80     2.34 1.01      222      447
#> env:seriessp_2    -1.59      0.17    -1.93    -1.27 1.03       89      189
#> env:seriessp_3    -0.76      0.17    -1.09    -0.44 1.02      272      681
#> env:seriessp_4    -1.46      0.27    -2.00    -0.97 1.04       65      325
#> env:seriessp_5    -0.78      0.15    -1.08    -0.52 1.02      116      196
#> 
#> Further Distributional Parameters:
#>        Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Psi[1]     0.44      0.15     0.13     0.68 1.06       30       61
#> Psi[2]     0.49      0.04     0.42     0.59 1.00      402      495
#> Psi[3]     0.38      0.17     0.09     0.70 1.02       30       48
#> Psi[4]     0.61      0.20     0.13     0.90 1.12       15       22
#> Psi[5]     0.55      0.05     0.46     0.66 1.01      324      259
#> 
#> == Trend Model ==
#> Factor Loadings:
#>                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Z_tilde[sp_1,1]     0.88      0.11     0.66     1.10 1.03       73      326
#> Z_tilde[sp_2,1]    -0.28      0.07    -0.42    -0.16 1.01      535      533
#> Z_tilde[sp_3,1]    -0.09      0.13    -0.35     0.15 1.01      199      471
#> Z_tilde[sp_4,1]    -1.04      0.15    -1.35    -0.75 1.01      255      949
#> Z_tilde[sp_5,1]     0.03      0.08    -0.13     0.18 1.01      403      939
#> Z_tilde[sp_2,2]     0.07      0.05     0.00     0.18 1.00      675     1190
#> Z_tilde[sp_3,2]     0.52      0.73    -1.02     1.08 1.00      393      212
#> Z_tilde[sp_4,2]    -0.40      0.57    -0.94     0.85 1.00      741      899
#> Z_tilde[sp_5,2]    -0.19      0.26    -0.45     0.38 1.00      673      967
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
#> sp_1  1.00 -0.43 -0.08 -0.65  0.04
#> sp_2 -0.43  1.00  0.12  0.32 -0.07
#> sp_3 -0.08  0.12  1.00 -0.37 -0.44
#> sp_4 -0.65  0.32 -0.37  1.00  0.20
#> sp_5  0.04 -0.07 -0.44  0.20  1.00
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
