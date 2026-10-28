# Compare posterior loadings between two latent-factor fits

Side-by-side comparison of the posterior-median species loadings from
two latent-factor `mvgam` / `jsdgam` fits on the same set of species.
Two fits need not order or orient their factors alike, and a direct
overlay of their loadings would mislead. `compare_loadings()` applies
the orthogonal Procrustes solution (Schoenemann 1966) that rotates the
posterior-median loadings of `fit_b` to best match those of `fit_a` in
the least-squares sense, then either overlays both onto a single biplot
with displacement segments (default) or splits them into two facets
sharing axes.

Useful when teaching the effect of a structured loadings prior: fit one
model with `loadings_prior = NULL` and a second with `traits =` or
`phylo =`, then plot the comparison to show how the trait or phylogeny
similarity reshapes the latent positions.

Procrustes alignment is orthogonal: it rotates and reflects the loadings
with their scale unchanged. A factor the model has shrunk towards zero
has short loadings, close to the origin. The panel shows which species
cluster together and how that arrangement moves between fits.
[`active_factors()`](https://nicholasjclark.github.io/mvgam/dev/reference/active_factors.md)
reports how many factors the data support.

## Usage

``` r
compare_loadings(
  fit_a,
  fit_b,
  labels = c("fit_a", "fit_b"),
  which_lvs = c(1L, 2L),
  align = TRUE,
  facet = FALSE,
  label_species = c("all", "shift", "none"),
  ...
)
```

## Arguments

- fit_a, fit_b:

  Two fitted `mvgam` / `jsdgam` objects, each with `n_lv >= 2` and the
  same species set in the same factor order.

- labels:

  Optional length-2 character vector of fit labels shown in the legend /
  facet strips. Defaults to `c("fit_a", "fit_b")`.

- which_lvs:

  Integer vector of length 2 indicating which two latent factors to
  plot. Defaults to `c(1, 2)`.

- align:

  Logical. When `TRUE` (default) the posterior-median loadings of
  `fit_b` are rotated onto those of `fit_a` by orthogonal Procrustes
  before plotting. `FALSE` plots each fit with the factors it reports.

- facet:

  Logical. When `FALSE` (default) both fits are overlaid on the same
  axes in different colours, with segments connecting the same species
  across fits. When `TRUE`, fits are drawn as side-by-side facets on
  shared axes.

- label_species:

  Character. Controls which species are labelled. `"all"` (default)
  labels every species; `"shift"` labels the top 10 species by
  post-alignment displacement between fits; `"none"` draws unlabelled
  points only.

- ...:

  Unused. Anything passed here is refused.

## Value

A `ggplot` object.

## Details

The orthogonal Procrustes rotation is the closed-form solution \$\$R = U
V^T\$\$ where \\U \Sigma V^T = SVD(Z_a^T Z_b)\\ and \\Z_a, Z_b\\ are the
posterior-median loading matrices restricted to `which_lvs`. The aligned
`Z_b` is `Z_b %*% t(R)`. Alignment is a sufficient but not unique
solution to factor rotation indeterminacy; alternative alignments
(varimax on the joint matrix, sign-flip only) are possible but typically
less interpretable for the prior-comparison use case here.

Both fits must report the same species in the same factor order; the
function errors otherwise. To compare fits with different species sets,
subset before fitting.

## References

Schoenemann PH (1966). A generalized solution of the orthogonal
Procrustes problem. *Psychometrika* 31, 1-10.
[doi:10.1007/BF02289451](https://doi.org/10.1007/BF02289451)

Heaps SE and Jermyn IH (2024). Structured prior distributions for the
covariance matrix in latent factor models. *Statistics and Computing*
34, 143.
[doi:10.1007/s11222-024-10454-0](https://doi.org/10.1007/s11222-024-10454-0)

## See also

[`ordinate()`](https://nicholasjclark.github.io/mvgam/dev/reference/ordinate.md),
[`residual_cor()`](https://nicholasjclark.github.io/mvgam/dev/reference/residual_cor.md),
[`active_factors()`](https://nicholasjclark.github.io/mvgam/dev/reference/active_factors.md)

Other latent dynamics:
[`active_factors()`](https://nicholasjclark.github.io/mvgam/dev/reference/active_factors.md),
[`fevd.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/fevd.mvgam.md),
[`irf.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/irf.mvgam.md),
[`ordinate()`](https://nicholasjclark.github.io/mvgam/dev/reference/ordinate.md),
[`posterior_transition_matrix()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_transition_matrix.md),
[`residual_cor()`](https://nicholasjclark.github.io/mvgam/dev/reference/residual_cor.md),
[`shared_variation()`](https://nicholasjclark.github.io/mvgam/dev/reference/shared_variation.md),
[`stability.mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/stability.mvgam.md)

## Author

Nicholas J Clark

## Examples

``` r
# \dontrun{
simdat <- sim_jsdgam(family = gaussian(), n_species = 10L,
                     n_sites = 60L, n_lv = 2L, seed = 3L)
mod <- jsdgam(y ~ env * series, data = simdat$data_train,
              family = gaussian(), n_lv = 2L, chains = 2, silent = 2)

# A trait that tracks each species' true loading on the first factor
set.seed(3)
traits <- data.frame(
  size = simdat$truth$loadings[, 1] + rnorm(10, sd = 0.2),
  row.names = levels(simdat$data_train$series)
)
mod_traits <- jsdgam(y ~ env * series, data = simdat$data_train,
                     family = gaussian(), n_lv = 2L, traits = traits,
                     chains = 2, silent = 2)

# Grey segments join each species' loadings under the two priors
compare_loadings(mod, mod_traits,
                 labels = c("Default prior", "Trait prior"))

# }
```
