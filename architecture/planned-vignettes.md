# Planned vignettes

Online-only HTML articles (pkgdown `articles/`), not shipped with
the source tarball. Each is a focused recipe end-users can follow
without prior mvgam exposure. Roxygen across the package
cross-references these as `\href{...}{mvgam vignette: <slug>}`
links so users land on the right one from any entry point.

Authoring conventions:

- Build at `vignettes/online/<slug>.Rmd` so they render under
  `pkgdown` but are excluded from CRAN via `_pkgdown.yml`
  config (similar to brms' "online-only" vignette pattern).
- Each vignette compiles a real fit using `cmdstanr` against a
  stock dataset (`portal_data`, `lynx_full`, or simulated data
  inline); cache fits with `knitr::opts_chunk$set(cache = TRUE)`.
- Cite the corresponding roxygen entry points at the top of
  each vignette so a reader bouncing in from `?mvgam` can find
  their way back.
- Keep each vignette under 1500 lines of rendered HTML; split
  if it grows past that. Avoid the "everything-and-the-kitchen-
  sink" anti-pattern.

## Vignette list

### 1. `multivariate-mvgam.Rmd`

Multivariate response models: multiple series with shared or
distinct observation families. Demonstrates:

- One trend formula spanning all responses (the default
  multivariate setup: shared trend dynamics, response-specific
  observation models).
- Mixed observation families (`bf(count ~ x, family = poisson())
  + bf(presence ~ x, family = bernoulli())`).
- Series-level vs response-level covariates.
- When to use `series = ...` vs separate response columns.

Roxygen cross-links from: `mvgam()`, `mvgam_formula()`,
`posterior_predict.mvgam`.

### 2. `factor-models.Rmd`

Latent factor trends: dimensionality reduction for many series.
Two halves:

- **Sampled loadings** (`AR(n_lv = 2)`, `RW(n_lv = 3)` etc.).
  The default factor model with PLT identification, including
  `plot_factors()`, `residual_cor()`, `ordinate.jsdgam()`,
  `factor_contributions()` (when shipped).
- **Fixed loadings via `trend_map`** (this release, N3). Three
  input shapes (numeric matrix for dense weights, data.frame for
  binary sharing, character codes `"identity"` / `"shared"`).
  When the user wants MARSS-style structural constraints. Show
  the worked example: 4 species, 2 latent factors, dense
  loadings, verify the fit recovers the latent dynamics.

Roxygen cross-links from: `AR()`, `RW()`, `VAR()`, `ZMVN()`,
`mvgam(trend_map = ...)`, `plot_factors()`, `residual_cor()`.

### 3. `forecast-evaluation.Rmd`

Out-of-sample forecast workflow. Covers:

- Splitting data via `newdata` on `mvgam()` so the fit persists
  the test arm.
- `forecast()` vs `posterior_predict()` semantics
  (state-propagated vs in-sample replication).
- Proper scoring rules from `score()`: CRPS, log-score, energy,
  variogram.
- Sliding-window evaluation via `lfo_cv()`.
- Comparing competing models with `loo_compare()` /
  `loo_model_weights()`.

Roxygen cross-links from: `forecast.mvgam`, `score.mvgam_forecast`,
`lfo_cv.mvgam`, `loo.mvgam`, `mvgam(newdata = ...)`.

### 4. `jsdgam-informed-loadings.Rmd`

Joint species distribution model (jsdgam) with informed factor-
loading priors following Heaps (2024). Not shippable in this
release because the informed-prior surface is queued as task
#166. Vignette skeleton lands now so:

- The `jsdgam()` roxygen can already link to the placeholder.
- When task #166 lands, the vignette already has a home and a
  citation skeleton.

Once #166 ships, the vignette covers:

- The Heaps (2024) auxiliary-info prior structure.
- How `loadings_prior = ...` composes with `trend_map`
  (constraints vs priors).
- A worked species-by-traits example showing posterior
  contraction relative to the default factor prior.

Roxygen cross-links from: `jsdgam()`, `residual_cor()`,
`ordinate.jsdgam`, and the `factor-models` vignette's
"informed priors" section.

## Cross-referencing pattern

Every roxygen entry that supports one of these topics should
include a `\seealso` block of the form:

```r
#' @seealso
#'   \href{https://nicholasjclark.github.io/mvgam/articles/factor-models.html}{
#'     mvgam vignette: factor models with sampled or fixed loadings
#'   }
```

Use the slug (`factor-models`, `multivariate-mvgam`,
`forecast-evaluation`, `jsdgam-informed-loadings`) as the article
basename so the URL stays stable as content drifts.

## Build / ship order

1. `multivariate-mvgam.Rmd` and `forecast-evaluation.Rmd` first,
   covering surfaces that already work and have stable APIs.
2. `factor-models.Rmd` once N3 lands (the fixed-loadings half
   needs the `trend_map` API to be present).
3. `jsdgam-informed-loadings.Rmd` once task #166 ships the
   informed-prior surface.

Drafts live under `vignettes/online/` even before they're cited
from roxygen, so reviewers can iterate on prose before the
final cross-link sweep.
