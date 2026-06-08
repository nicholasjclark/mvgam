# Heaps 2024 factor-model architecture (task #166)

## Context

mvgam's current factor-model architecture is PLT-at-sampling
(lower-triangular Z constructed from a `Z_raw` vector in
transformed parameters, independent student-t prior per entry).
This gives a clean implementation but two real limitations:

1. **No mechanism to incorporate domain structure into the prior.**
   Analysts with phylogenetic trees, distance matrices, or
   per-row trait information cannot use them to shape the prior
   on Z. Every entry is iid up to the PLT constraint.
2. **The 2^k sign-mode equivalence survives at sampling time.**
   The recent `sign_canonicalise_factors` post-processor patches
   this for the saved draws but does not change the geometry
   that the sampler explores.

Heaps and Jermyn (2024, *Stat. Comput.*) propose a cleaner
parameterisation that addresses both concerns at once: sample Λ
unconstrained under a STRUCTURED prior whose row scale matrix Φ
encodes domain knowledge, then identify the lower-triangular Λ̃
post-hoc via QR decomposition in generated quantities. The key
insight is `E(Δ) = tr(Ψ²)Φ` where `Δ = ΛΛ'` is the shared
variation matrix. So Φ IS proportional to the prior expectation
of the (interpretable, observable) shared variation, not the
(uninterpretable, latent) factor loadings.

Heaps' Section 6.2 (Finnish bird co-occurrence) shows the user
surface in action. Side information consists of:
- Per-species trait matrix containing both continuous columns
  (log body mass) and categorical columns (migrant status as
  three binary indicators)
- Pre-computed phylogenetic distance matrix from a tree

All side information feeds an ARD Euclidean kernel on the
encoded feature matrix plus an exponential decay on the
phylogenetic distance. Length-scales per dimension are sampled.

## Reference materials to bring into the repo

Stage the Heaps supplementary materials in `tasks/heaps-2024/`
so they are version-controlled and accessible during
implementation review. The supps include working R and Stan
reference code that the new mvgam machinery should match in
structure (with appropriate generalisation).

Files to add (sources in `~/Downloads/`):

- `tasks/heaps-2024/paper.pdf` from
  `~/Downloads/s11222-024-10454-0.pdf` (the journal article,
  18 pages)
- `tasks/heaps-2024/supplementary.pdf` from
  `~/Downloads/11222_2024_10454_MOESM1_ESM.zip ->
  supplMat.pdf` (33 pages of proofs, derivations, application
  detail, and trace-plot diagnostics)
- `tasks/heaps-2024/code/hourlygasdemand.stan` and
  `hourlygasdemand.R` (their dynamic-factor reference
  implementation, the closest analogue to mvgam's setup)
- `tasks/heaps-2024/code/multiprobitregr.stan` and
  `multiprobitregr.R` (their JSDM reference implementation,
  closest analogue to a future `jsdgam`)
- `tasks/heaps-2024/code/commonfunctions.R` (shared utilities)
- `tasks/heaps-2024/code/README` (the supps README explaining
  what each file does)

Reference points to verify against during implementation:

- The Stan `gp_exponential_cov(x, sigma, theta)` call in
  `multiprobitregr.stan` for the ARD Euclidean kernel on the
  encoded feature matrix (Section 6.2.1 of the paper, equation
  giving `d_C,ij`)
- The `qr_R(Lambda')'` post-hoc identification in
  `generated quantities` of both reference Stan files (we use
  `qr_thin_R` for positive-diagonal guarantee)
- The multiplicative gamma process `varrho_inv` block in both
  reference Stan files (Section 3.5 of the paper)
- The matrix-normal prior on Λ split per column:
  `Lambda[, i] ~ multi_normal_prec(zero_vec, S / Psi_diag[i])`
  in `hourlygasdemand.stan` (Section 3.2 of the paper)

Phase 0 of the implementation copies these files in as a single
prep commit before any phase-1 code change. This gives reviewers
the reference implementations to check our generalisation
against.

## What changes

Three coupled changes, landing as one feature branch:

1. **Stan-side architecture switch.** Replace PLT-at-sampling
   with sample-free-Λ + post-hoc QR via `qr_thin_R` (positive
   diagonal by construction, subsumes the sign-flip equivalence
   class).
2. **`loadings_prior` argument added to `mvgam()`.** Generic
   user surface for structured priors. Accepts per-row feature
   matrices and pre-computed pairwise distance matrices via
   `data2` lookup. Builds the projected-exponential covariance
   internally.
3. **Documentation in neutral language.** `mvgam()` itself is
   domain-neutral and is the general entry point for analysts
   from any field. Ecologist-friendly terminology (`traits`,
   `phylo`) is reserved for the future `jsdgam()` wrapper and
   the planned `jsdgam-informed-loadings.Rmd` vignette.

## API surface

### `mvgam()` gains a `loadings_prior` argument

```r
mvgam(
  formula        = ...,
  trend_formula  = ~ AR(p = 1, n_lv = 5),
  data           = obs_data,
  data2          = list(
    row_features = features_df_or_matrix,
    pair_dist_1  = some_pairwise_matrix,
    pair_dist_2  = another_pairwise_matrix
  ),
  loadings_prior = list(
    features         = "row_features",
    distances        = c("pair_dist_1", "pair_dist_2"),
    column_shrinkage = c("iid", "mgp"),
    mgp_a1 = 2, mgp_a2 = 3
  ),
  family = ...
)
```

| Field | Type | Meaning |
|---|---|---|
| `features` | string lookup into `data2` or a matrix / data.frame | Per-row attribute matrix walked column by column and encoded into a numeric matrix |
| `distances` | string or character vector of `data2` keys (or matrix / list of matrices passed directly) | Pre-computed pairwise distance matrices |
| `column_shrinkage` | `"iid"` (default) or `"mgp"` | Column shrinkage prior on Ψ |
| `mgp_a1`, `mgp_a2` | numeric | Multiplicative-gamma-process hyperparameters when `column_shrinkage = "mgp"` |
| `length_scale_prior` | `"default"` or a user expression | Prior on the ARD length-scales (default `log(theta) ~ N(0, 1)`) |

### Feature-encoding rules (R-side preprocessing)

The package walks `features` columns and encodes by type:

| Column type | Encoding |
|---|---|
| `numeric` / `integer` | z-score by SD |
| `ordered factor` | `as.numeric()` then z-score (equal-spacing assumption documented) |
| `factor` (unordered) | one-hot encode, all levels kept (matches Heaps' Finnish-birds setup) |
| `character` | coerce to factor, then one-hot encode |

Encoded columns stack into a `p × c` numeric matrix passed to
Stan as `array[p] vector[c] row_features`. Stan samples `c`
length-scales for the ARD Euclidean via `gp_exponential_cov`.

### Combination semantics

`Φ_ij = exp(-d_features_ij) * prod_k exp(-d_k_ij / theta_k)`

Multiplicative across information sources (Heaps' framework). The
roxygen documents the AND-logic semantic explicitly: adding more
informative dimensions tightens the prior toward identity for any
row pair that differs on one dimension.

### Validation warnings

- `p > 100`: warn about `O(p^3)` Cholesky of Φ
- Pairwise Frobenius correlation between encoded feature columns
  or supplied distance matrices > 0.9: length-scales may not be
  separately identifiable
- Imbalanced one-hot column (>95% single value): length-scale
  for that dimension may be hard to learn from data

### `jsdgam()` translation (future task)

Domain-friendly aliases for ecologists, callable through the
same Stan machinery:

```r
jsdgam(
  formula = ...,
  data    = sites_x_species,
  traits  = species_trait_df,   # rewritten to loadings_prior$features
  phylo   = phylo_tree,         # ape::cophenetic.phylo, then
                                # rewritten to loadings_prior$distances
  ...
)
```

A one-page shim. No new Stan-side code.

## Phasing

Phase 0 is a prep commit. Phases 1 to 3 are prerequisite to
anything informed-prior, so the landing order matters.

### Phase 0 (~0.1 day): Stage Heaps reference materials

Copy the paper PDF, supps PDF, and supps `code/` directory into
`tasks/heaps-2024/` (paths listed in the "Reference materials"
section above). Commit alone before any Stan or R change so the
references are stable and reviewable. Also delete the leftover
draft at `architecture/heaps-factor-architecture-plan.md` (this
plan moves to `tasks/heaps-factor-architecture.md` at Phase 0
commit time).

### Phase 1 (~1 day): Stan refactor for the default factor model

Touch `R/stan_assembly.R::generate_factor_model` and matrix-Z
helpers (`generate_matrix_z_parameters`, `generate_matrix_z_tdata`,
`generate_matrix_z_multiblock_stanvars`).

- Drop the lower-triangular fill from `Z_raw`. Sample
  `matrix[N_series_trend, N_lv_trend] Z` directly in `parameters`.
- Default prior: `to_vector(Z) ~ student_t(3, 0, 1)` matches the
  Φ = I_p, Ψ = ψI_k case in Heaps' framework. Marginal density on
  Z̃ is identical to the current per-entry prior.
- Add a `generated quantities` emission of
  `Z_tilde = qr_thin_R(Z')'`, `Q = qr_thin_Q(Z')'`, rotated
  `lv_trend_tilde = lv_trend * Q'`. For VAR trends also rotate
  the AR coefficient matrices as `A_trend_tilde = Q * A_trend * Q'`.

Test regen: factor-model contract tests in
`tests/testthat/test-stancode-standata.R` currently match
`Z_raw`, lower-triangular fill, and the `Z_raw ~ student_t`
prior. None of those patterns survive. Replace with contracts on
the new shape: full Z sampled, `qr_thin_R` in generated
quantities, `Z_tilde` and `lv_trend_tilde` saved.

Risk: free-Λ has more parameters than PLT (`p·k` vs
`p·k − k(k−1)/2`). MCMC speed could differ. Verify on the
existing 4-series × 2-factor local fixture before committing.

### Phase 2 (~0.5 day): R-side rewires for the new variable names

- `resolve_factor_loadings` reads `Z_tilde[i, j]` instead of
  `Z[i, j]`.
- `extract_lv_trend_matrices` reads `lv_trend_tilde[t, k]`.
- `sign_canonicalise_factors` becomes a no-op for sampled-Z fits
  (the `qr_thin_R` positive-diagonal guarantee covers it). Keep
  the function as a defensive belt.
- Fully-fixed-Z fits (`trend_map` with no NAs) are unchanged: Z
  is in the data block, no QR rotation.
- Backward-compat path: resolver falls back to `Z[i, j]` for
  pre-Heaps cached fits and warns about the deprecated names.

### Phase 3 (~0.5 day): Partial Z under free-Λ

`trend_map` with NAs (partial Z) semantics shift. The user
currently thinks of `trend_map` as fixing entries of identified
Z̃. Under Heaps, the saved Z̃ is a QR rotation of unconstrained
Λ; hard-fixing entries of Λ does not preserve those entries in
Z̃.

Decision: `trend_map` applies to Λ in the new architecture. The
user encodes structural hypotheses on unconstrained loadings;
the QR rotation produces a canonical Z̃ that may differ. Store
`Q` on the fit as `attr(fit, "loadings_rotation")` so users who
care can map back via `Z̃ Q'`.

Update `test-trend-map.R` partial-Z tests to reflect the new
semantics. The user-supplied pattern is preserved on Λ
parameter draws.

### Phase 4 (~1.5 days): `loadings_prior` plumbing

- New `R/validations.R::normalise_loadings_prior(spec, data2,
  n_series, n_lv)` resolves names against `data2`, validates
  dimensions, encodes feature columns, packages the result.
- `R/mvgam_core.R::mvgam()` gains the `loadings_prior` arg and
  plumbs it through.
- `R/make_stan.R::generate_stan_components_mvgam_formula` picks
  up the normalised spec.
- New `R/stan_assembly.R::make_loadings_prior_stanvars(spec)`
  emits:
  - `data` block: `row_features` array, one or more pairwise
    distance matrices, `n_features` int
  - `parameters` block: `theta_features[n_features]`, one
    `theta_dist_k` per supplied distance, `varrho_inv[H]` if
    MGP is selected
  - `transformed parameters` block: assemble
    `Phi_inv = exp(log(gp_exponential_cov(row_features, 1.0,
    theta_features)) + sum_k log(exp(-pair_dist_k /
    theta_dist_k)))`, assemble `Psi_diag` via cumulative product
    for MGP
  - `model` block: per-column loop
    `Z[, i] ~ multi_normal_prec(zero_vec, Phi_inv / Psi_diag[i])`
    replacing the default
    `to_vector(Z) ~ student_t(3, 0, 1)`.

MVP restrictions:
- `loadings_prior` cannot combine with `trend_map` containing
  NAs (conditional-prior-on-free-given-fixed via Schur is a
  follow-up).
- `loadings_prior` with fully-fixed `trend_map` is incoherent
  (fixed Z has no prior). Error.

New tests:
- `tests/testthat/test-loadings-prior.R`: normaliser validation
  (name missing in `data2`, non-square matrix, non-PD matrix,
  all-NA feature column, collinearity warning).
- Stancode round-trip tests per prior shape: default, features
  only, distances only, features plus distances, MGP shrinkage.

### Phase 5 (~1 day): Encoding and validation helpers

`R/loadings_prior_helpers.R` (new file):

- `encode_loadings_features(features_df_or_matrix, series_levels)`:
  walks columns by type, encodes, returns a `p × c` matrix
  ordered by series.
- `validate_pairwise_distance(mat, n_series, name)`: dimension,
  symmetry, non-negativity, diagonal-zero.
- `length_scale_collinearity_warning(distance_matrices)`:
  flattens upper triangles, Pearson correlations, warns at
  threshold 0.9.
- `imbalance_warning(features_matrix)`: per-column modal-value
  proportion, warns at 95%.

`tests/testthat/test-loadings-prior-helpers.R` covers each
helper on small synthetic inputs.

### Phase 6 (~0.5 day): Local end-to-end fit fixture

Touch `tests/local/build_fixtures.R` and add
`tests/local/test-loadings-prior-fit.R`.

A small structured-prior factor fixture: six rows, three
factors, one continuous feature, one binary categorical
(one-hot to two indicators), one pairwise distance from a hand-
built hierarchy. Verify:
- stancode emits ARD kernel plus pairwise-distance kernel.
- standata carries the encoded feature matrix and the distance
  matrix.
- Fit converges (Rhat below 1.1 on identified Z̃ entries).
- Rows similar on features and pairwise-close show positively-
  correlated residuals via `residual_cor(fit)`.

### Phase 7 (~0.5 day): Documentation

- `architecture/architecture-decisions.md`: extend Section 2
  with three short paragraphs on post-QR identification, the
  `loadings_prior` surface, and how Φ shapes E(Δ).
- Roxygen on `mvgam()` extended with `@param loadings_prior`
  and `@param data2` in neutral language. One numeric and one
  categorical feature in the example.
- `?AR`, `?VAR`, `?RW`, `?ZMVN`: `@section Identification:`
  paragraph noting post-QR convention and pointing at
  `?loadings_prior`.
- `vignettes/online/jsdgam-informed-loadings.Rmd` becomes a
  promoted draft (was placeholder). Translates the generic
  surface into traits + phylogeny language for ecologists.

## Files to be modified

### New files

- `R/loadings_prior_helpers.R`
- `tests/testthat/test-loadings-prior.R`
- `tests/testthat/test-loadings-prior-helpers.R`
- `tests/local/test-loadings-prior-fit.R`
- `tasks/heaps-factor-architecture.md` (this plan, moved from
  `architecture/` after exit)
- `tasks/heaps-2024/paper.pdf` (Phase 0 import)
- `tasks/heaps-2024/supplementary.pdf` (Phase 0 import)
- `tasks/heaps-2024/code/` (Phase 0 import, R and Stan reference
  implementations from the supps zip)

### Modified

- `R/stan_assembly.R` (Phase 1, 4): factor-model emission and
  new prior stanvars
- `R/plot_helpers.R` (Phase 2): `resolve_factor_loadings` reads
  `Z_tilde`
- `R/plot_factors.R` (Phase 2): `extract_lv_trend_matrices`
  reads `lv_trend_tilde`
- `R/sign_canonical.R` (Phase 2): becomes a no-op for sampled-Z
  fits; defensive belt only
- `R/validations.R` (Phase 4): new normaliser
- `R/mvgam_core.R` (Phase 4): `loadings_prior` signature plumb
- `R/make_stan.R` (Phase 4): pick up normalised spec
- `tests/testthat/test-stancode-standata.R` (Phase 1): factor-
  model contracts regenerated
- `tests/testthat/test-plot-factors.R`,
  `tests/testthat/test-residual-cor.R`,
  `tests/testthat/test-trend-map.R` (Phase 2, 3): variable-name
  swaps and partial-Z semantics
- `tests/local/build_fixtures.R` (Phase 6): new fixture builder
- `architecture/architecture-decisions.md` (Phase 7)
- `architecture/heaps-factor-architecture-plan.md`: DELETE after
  plan moves to `tasks/`

## Reusable functions and patterns already in mvgam

- `resolve_factor_loadings()` in `R/plot_helpers.R`: keep the
  branching pattern (fixed-Z broadcast vs posterior parse).
  Only the parameter name changes (`Z[i, j]` to
  `Z_tilde[i, j]`).
- `make_fixed_z_stanvars()`, `make_partial_z_stanvars()` in
  `R/stan_assembly.R`: stay as-is. They emit Z in the data
  block, no QR involved.
- `normalise_trend_map()`, `normalise_trend_map_on_specs()` in
  `R/validations.R`: pattern to follow for
  `normalise_loadings_prior()`.
- `apply_trend_map_alias()` in `R/validations.R`: dual-entry-
  point pattern (trend_map can come from constructor OR
  top-level) is not needed for `loadings_prior` (top-level only).
- `extract_indexed_array_2d()` in `R/sample_innovations.R`: used
  for column-major parsing of 2D Stan parameters. Reuse for
  `Z_tilde` parsing.
- `combine_stanvars()` in `R/stan_assembly.R`: pattern to follow
  for combining the `loadings_prior` stanvars.

## Review gates per phase

Code review and stats review are mandatory at the gates below.
Each gate must produce APPROVED status before the next phase
begins.

| Phase | Code review | Stats review |
|---|---|---|
| 0 (staging refs) | not needed | not needed |
| 1 (Stan refactor) | YES, on the Stan emission diff and the test-regen pass | YES, **stringent**: web-search the Stan reference manual and Stan forums for `qr_thin_R` performance characteristics, free-Λ vs PLT sampling efficiency on factor models, and the Heaps reparameterisation specifically; verify the chosen idioms are the most efficient available |
| 2 (R-side rewires) | YES | not needed |
| 3 (partial Z under free Λ) | YES, on the semantic shift + tests | YES, on whether the user-facing semantic of `trend_map` (constraints on Λ vs Z̃) makes statistical sense |
| 4 (`loadings_prior` plumbing) | YES, focus on DRY against existing resolver / normaliser patterns | YES, **stringent**: web-search-supported review covering (a) the Stan idioms used to assemble Φ_inv (Heaps uses `multi_normal_prec`, but `multi_normal_cholesky` with a cholesky factor of the structured matrix is typically faster), (b) numerical stability under near-collinear distance matrices, (c) the multiplicative-vs-additive kernel-combination choice |
| 5 (encoding helpers) | YES, focus on edge cases (NA in features, single-level factors, etc.) | YES, on the encoding choices (one-hot keeping all levels per Heaps, ordered-factor z-scoring equal-spacing assumption) |
| 6 (local fixture) | YES, focus on whether the fixture genuinely exercises the new emission paths | YES, on whether the recovery test validates the prior structure faithfully (does the simulated truth match the prior, can the fit recover it) |
| 7 (documentation) | YES, focus on neutrality (no domain-loaded terms in `mvgam()` itself) | not needed |

### Stringent stats review prompts

The Phase 1 and Phase 4 stats reviews are flagged stringent. The
agent prompt for each must:

- Instruct web-search use to verify Stan code is the most
  efficient AND mathematically correct rendering of the math in
  the Heaps paper
- Reference the staged Heaps supps Stan files
  (`tasks/heaps-2024/code/*.stan`) as the canonical comparison
- Check specific idioms: `cholesky_decompose` vs
  `multiply_lower_tri_self_transpose`, `multi_normal_cholesky`
  vs `multi_normal_prec`, `qr_thin_R` vs `qr_R`,
  `gp_exponential_cov` for the ARD kernel, and any cached
  intermediates the reviewer can identify as recomputed
  per-iteration unnecessarily
- Demand specific recommendations with reference URLs (Stan
  manual sections, Stan forum posts, published benchmarks)
  rather than vague directives
- Require the reviewer to flag any place where the mvgam
  emission deviates from Heaps' reference Stan in a way that
  could change inferential properties

The output must be actionable: file path, line number, what to
change, why, and a reference link.

## End-of-merge verification

Before merge to `feature/brms-integration`:

1. `Rscript -e "devtools::load_all(); testthat::test_dir('tests/testthat')"`
   green. Zero new failures or warnings against the current
   baseline.
2. `tests/local/test-trend-map-fit.R` passes against a freshly
   rebuilt cached fixture. The saved variable names change, so
   the fixture must be regenerated.
3. New `tests/local/test-loadings-prior-fit.R` passes against
   the structured-prior fixture.
4. Convergence comparison on the existing 4-series × 2-factor
   AR(1) recovery fixture: Rhat and ESS on identified Z̃ entries
   should be equal or better than the pre-refactor saved
   posterior.
5. All per-phase code reviews and stats reviews APPROVED.

## Backout strategy

If Phase 1 verification surfaces a meaningful convergence
regression, revert at the commit level. The PLT machinery is
small enough to restore in one commit. Phases 2 through 7 layer
on top of Phase 1 and cannot ship without it.

## Effort estimate

- Phase 0: 0.1 day
- Phase 1: 1 day
- Phase 2: 0.5 day
- Phase 3: 0.5 day
- Phase 4: 1.5 days
- Phase 5: 1 day
- Phase 6: 0.5 day
- Phase 7: 0.5 day

Total: 5.6 days focused work plus review and convergence
verification time.

## Open design choices to confirm before Phase 4

1. **Direct passing of features and distances inline.** Should
   `loadings_prior = list(features = my_matrix)` (matrix passed
   directly with no `data2` indirection) be supported, or always
   require name lookup into `data2`? Recommend support both;
   document the indirection as the standard pattern.
2. **Combination rule exposure.** Should
   `combination = c("product", "sum")` be exposed as a user knob
   in `loadings_prior`? The product form is Heaps' framework
   verbatim; the sum form is a kernel-mixture alternative
   weakening the AND-logic concern but not what the paper does.
   Recommend: do not expose for the MVP; document the
   multiplicative form clearly; file a follow-up if a real user
   case for sum-of-kernels surfaces.
3. **`ordinate()` behaviour under the new architecture.** The
   saved Z̃ is already QR-canonical, so the SVD rotation inside
   `ordinate.mvgam` operates on top of that. Document in the
   `ordinate.mvgam` roxygen that the function performs a second
   rotation for biplot orientation purposes.
