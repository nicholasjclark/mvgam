# Multivariate Student-t family for several continuous responses

[`mvn()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvn.md)
with heavy-tailed residuals. Given the latent factors, each row follows
a Student-t distribution with location `mu`, scale `Psi` and degrees of
freedom `nu`, which every component shares. An outlying row then pulls
less on the fit. The residual covariance is
`Z Z' + diag(Psi^2 * nu / (nu - 2))`. A unit's responses do not follow a
multivariate Student-t law. Their factors stay normal.

## Usage

``` r
mvt()
```

## Value

A family object to pass to
[`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md)
or
[`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md).

## Details

`nu` is bounded below by 2, which keeps the residual variance finite.
Its prior is `gamma(2, 0.1)`, as brms gives a Student-t `nu`. Above the
bound it places the median of `nu` at 17 and 95% of its mass between 3.2
and 56. `prior(..., class = nu)` and `prior(..., class = Psi)` set other
priors. The data layout, the fit and the bound on `n_lv` follow
[`mvn()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvn.md).

## References

Hui, F. K. C., Taskinen, S., Pledger, S., Foster, S. D. and Warton, D.
I. (2015). Model-based approaches to unconstrained ordination. *Methods
in Ecology and Evolution*, 6(4):399-411.
[doi:10.1111/2041-210X.12236](https://doi.org/10.1111/2041-210X.12236)

## See also

Other observation families:
[`beta_nb()`](https://nicholasjclark.github.io/mvgam/dev/reference/beta_nb.md),
[`categ()`](https://nicholasjclark.github.io/mvgam/dev/reference/categ.md),
[`com_binomial()`](https://nicholasjclark.github.io/mvgam/dev/reference/com_binomial.md),
[`diri()`](https://nicholasjclark.github.io/mvgam/dev/reference/diri.md),
[`multi()`](https://nicholasjclark.github.io/mvgam/dev/reference/multi.md),
[`mvgam_families`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_families.md),
[`mvn()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvn.md),
[`nmix()`](https://nicholasjclark.github.io/mvgam/dev/reference/nmix.md),
[`occ()`](https://nicholasjclark.github.io/mvgam/dev/reference/occ.md),
[`tweedie()`](https://nicholasjclark.github.io/mvgam/dev/reference/tweedie.md)

## Examples

``` r
# \dontrun{
simdat <- sim_jsdgam(family = mvt(), n_species = 5L, n_sites = 80L,
                     n_lv = 2L, seed = 1L)
fit <- jsdgam(
  formula        = y ~ env * series,
  factor_formula = ~ -1,
  data           = simdat$data_train,
  unit           = time, species = series,
  family         = mvt(),
  n_lv           = 2L,
  chains         = 2L,
  silent         = 2
)
#> Warning: 14 of 2000 (1.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
#> Warning: 2 of 2 chains had an E-BFMI less than 0.3.
#> See https://mc-stan.org/misc/warnings for details.

# The degrees of freedom, against 5 in the simulation
posterior_summary(fit, variable = "nu")
#>    Estimate Est.Error     Q2.5    Q97.5
#> nu 8.921448   5.83514 3.262335 23.14878
# }
```
