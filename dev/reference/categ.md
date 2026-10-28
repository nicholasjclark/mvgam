# Categorical family for one category observed per unit

For a single category recorded on each unit, such as the dominant
species at a site. The data hold one row per category at each unit, with
response 1 on the observed category and 0 on the others. The observed
category is categorical with probabilities `softmax(mu)`, where `mu`
holds the categories' linear predictors.

## Usage

``` r
categ()
```

## Value

A family object to pass to
[`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md)
or
[`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md).

## Details

[`jsdgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/jsdgam.md)
fits the family, taking the unit and category columns and the number of
latent factors `n_lv`. The constraints that identify the model are those
of
[`diri()`](https://nicholasjclark.github.io/mvgam/dev/reference/diri.md).
A unit holds one trial. It informs the factor loadings less than a
[`multi()`](https://nicholasjclark.github.io/mvgam/dev/reference/multi.md)
unit of many counts does. Recovering the residual correlations needs
more units.

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

Warton, D. I., Blanchet, F. G., O'Hara, R. B., Ovaskainen, O., Taskinen,
S., Walker, S. C. and Hui, F. K. C. (2015). So many variables: joint
modeling in community ecology. *Trends in Ecology and Evolution*,
30(12):766-779.
[doi:10.1016/j.tree.2015.09.007](https://doi.org/10.1016/j.tree.2015.09.007)

## See also

Other observation families:
[`beta_nb()`](https://nicholasjclark.github.io/mvgam/dev/reference/beta_nb.md),
[`com_binomial()`](https://nicholasjclark.github.io/mvgam/dev/reference/com_binomial.md),
[`diri()`](https://nicholasjclark.github.io/mvgam/dev/reference/diri.md),
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
# The one category observed at each of 150 sites
simdat <- sim_jsdgam(family = categ(), n_species = 3L,
                     n_sites = 150L, seed = 4L)

fit <- jsdgam(
  formula        = y ~ env * series,
  factor_formula = ~ -1,
  data           = simdat$data_train,
  unit           = time, species = series,
  family         = categ(),
  n_lv           = 1L,
  chains         = 2L,
  silent         = 2
)

# The probability of each category along the gradient
conditional_effects(fit, effects = "env:series")


# Next, pp_check(fit) compares observed and replicated categories
# }
```
