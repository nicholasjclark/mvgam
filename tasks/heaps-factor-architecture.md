# Heaps 2024 factor-model architecture (task #166)

## Status

| Phase | State | Commit |
|---|---|---|
| 0 — Stage Heaps reference materials | DONE | `61a2c143` |
| 1 — Stan refactor for default factor model | DONE | `d80eab98` |
| 2 — R-side rewires for the new variable names | DONE | `290d29b1` |
| 3 — `trend_map` semantics under free Λ | DONE | `5d70d90c` |
| 4 — `loadings_prior` plumbing | PENDING | — |
| 5 — Encoding + validation helpers | PENDING | — |
| 6 — Local end-to-end fit fixture | PENDING | — |
| 7 — Documentation sweep | PENDING | — |

Per-phase implementation notes appear inside each Phase section
below. Decisions that diverged from the original plan are
flagged explicitly.

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

### Phase 0 (~0.1 day): Stage Heaps reference materials — DONE

Commit `61a2c143`. Paper PDF, supplementary PDF, and supps
`code/` directory staged under `tasks/heaps-2024/`. The plan
moved into `tasks/heaps-factor-architecture.md` at this point;
the architecture-side draft was deleted.

### Phase 1 (~1 day): Stan refactor for the default factor model — DONE

Commit `d80eab98`. `R/stan_assembly.R::generate_factor_model`
and matrix-Z helpers rewritten so factor models sample
`matrix[N_series_trend, N_lv_trend] Z` directly in `parameters`
under `to_vector(Z) ~ student_t(3, 0, 1)`, then identify
`Z_tilde = qr_thin_R(Z')'`, `Q_tilde = qr_thin_Q(Z')'`,
`lv_trend_tilde = lv_trend * Q_tilde'` in generated quantities.

Implementation notes that diverged from / refined the original
plan, after stringent code review + stats review + Opus
deep-verify:

- **`qr_thin_R` confirmed on wide matrices.** The Stan
  Functions Reference documents `qr_thin_R` as requiring
  `rows >= cols`, but the underlying Eigen `HouseholderQR`
  handles wide input correctly and Stan applies the positive-
  diagonal normalisation to the first `min(rows, cols)`
  diagonal entries. Verified empirically against Stan 2.38 on
  a 3×5 input (reconstruction error 1e-16, positive diagonal
  by construction).
- **Sign-fix loop dropped.** Because `qr_thin_R` guarantees
  diag(R) ≥ 0 and `qr_thin_Q` applies the matching column-
  sign flips so `Z = Z_tilde * Q_tilde` is preserved, the
  inline sign-fix loop that an earlier draft of this phase
  included is dead code under the thin variants. Heaps' own
  reference code does not include one either.
- **VAR rotation wired in.** `generate_factor_model` accepts
  an optional `trend_type` argument; when set to `"VAR"` the
  generated-quantities block also emits
  `array[size(A_trend)] matrix[N_lv, N_lv] A_trend_tilde` with
  `A_trend_tilde[lag] = Q_tilde * A_trend[lag] * Q_tilde'`
  (the analogue of Heaps' `Gammatilde = Q Gamma Q'` in
  `hourlygasdemand.stan`).
- **Identification scope documented in roxygen.** Per-factor
  scalar nuisance parameters (`ar1_trend`, `ar{p}_trend`,
  `theta1_trend`, `sigma_trend`, `L_Omega_trend`,
  `Sigma_trend`) remain in the unrotated `Z` basis. They are
  meaningful per-factor only on the sampled `Z`, not on the
  identified `Z_tilde`. Rotating them into `K × K` matrices
  destroys the per-factor interpretation, so they are NOT
  emitted in rotated form. The function roxygen calls this
  out explicitly.
- **Student-t prior kept for the MVP.** The plan flagged a
  Gaussian (`multi_normal_prec`) prior as the canonical
  Heaps form, which preserves the gamma-on-diagonal / normal-
  off-diagonal closed-form marginal on `Z_tilde` (Corollary
  S1). The Phase 1 commit retains `to_vector(Z) ~
  student_t(3, 0, 1)` for compatibility with the existing
  prior baseline. The induced marginal on `Z_tilde` is
  heavier-tailed than the Gaussian case and not the closed-
  form Corollary S1 density; this is documented in the
  function comment. Phase 4 introduces the structured Gaussian
  prior via `loadings_prior`.

Test contracts in `tests/testthat/test-stancode-standata.R`
updated to assert: `matrix[N_series_trend, N_lv_trend] Z` in
parameters, `to_vector(Z) ~ student_t(3, 0, 1)`, and the QR
block (`qr_thin_R(Z')'`, `qr_thin_Q(Z')'`, `lv_trend_tilde =
lv_trend * Q_tilde'`) in generated quantities. Full testthat
green (4076 PASS, 0 FAIL, 0 WARN) before merge.

### Phase 2 (~0.5 day): R-side rewires for the new variable names — DONE

Commit `290d29b1`. Resolvers, summary classifier, and tidy
classifier now read the QR-identified parameter names with a
prefer-`Z_tilde`-then-fall-back-to-`Z` rule. Implementation
notes:

- **Shared helpers in `R/sample_innovations.R`.** Three small
  pattern selectors centralise the routing so the resolver,
  the summary classifier, and the tidy classifier stay in
  lockstep:
  - `factor_loading_param_pattern(pars)`: returns
    `"^Z_tilde\\["` if any `Z_tilde[...]` column is in the
    posterior, else `"^Z\\["`.
  - `factor_state_param_pattern(pars)`: same shape, picking
    `lv_trend_tilde` over `lv_trend`.
  - `hidden_unrotated_factor_pars(pars)`: returns
    `"^A_trend\\["` when `A_trend_tilde` is in the posterior,
    so VAR factor summaries do not display both bases.
- **Resolvers.** `extract_Z_loadings`,
  `extract_lv_trend_matrices`, and
  `extract_factor_loadings_array` delegate to the helpers.
  Free-Z factor fits surface identified loadings / paths;
  partial-Z fits surface the user-encoded `Z` / `lv_trend`
  unchanged.
- **`sign_canonicalise_factors`.** Short-circuits to a no-op
  whenever `Z_tilde` is in the posterior (positive diagonal
  guaranteed by `qr_thin_R`). Also short-circuits for ANY
  user-supplied loadings (fully fixed OR partial) because
  flipping a column would corrupt the user's encoded pattern.
- **Classifiers.** `match_z_loadings`, `match_trend_pars`,
  `is_latent_state_param`, `match_trend_specific_pars`
  (`R/summary.mvgam.R`) and `categorize_mvgam_parameters`
  (`R/index-mvgam.R`) use the helpers. Default summary print,
  tidy, `mcmc_plot`, `as.data.frame` all see identified
  loadings as canonical and hide the unrotated `A_trend`
  draws when `A_trend_tilde` is present.
- **Identification footnote.** `compute_all_summaries` stores
  a `loadings_identified` flag (TRUE when extracted from
  `Z_tilde` draws). `print.summary.mvgam` emits a scope
  paragraph on identified fits stating that per-factor trend
  dynamics parameters remain in the unrotated factor basis.
- **Roxygen sweep.** User-facing roxygen on
  `resolve_factor_loadings` (`R/plot_helpers.R`),
  `ordinate.jsdgam`, and `plot_factors` describes the new
  routing. The `sign_canonicalise_factors` heading and the
  call-site comment in `R/mvgam_core.R` reflect the no-op
  semantic on Phase 1+ fits.

Pre-Heaps cached fits would surface as a draws-array without
`Z_tilde` columns; the helpers fall through to `Z` cleanly so
no deprecation warning was needed.

### Phase 3 (~0.5 day): `trend_map` semantics under free Λ — DONE

Commit `5d70d90c`. Doc-only phase; the routing already
implemented in Phase 1 separated user-supplied loadings from
the QR identification path so no code change was required.

**Semantic chosen (diverges from the original plan).** The
original plan proposed that `trend_map` should apply to the
unconstrained Λ in the new architecture, with the QR rotation
producing a canonical Z̃ that may differ from the user's
encoded pattern; the rotation Q was to be stashed on the fit
so users could map back. The implementation instead chose a
cleaner semantic: **any non-NULL `trend_map` bypasses the QR
identification entirely**. The user's encoded entries are
preserved exactly on `Z` in the posterior, no `Z_tilde` is
emitted for those fits, and no rotation map needs to be
stored. The routing in
`generate_matrix_z_multiblock_stanvars` dispatches user-
supplied loadings to `make_partial_z_stanvars` /
`make_fixed_z_stanvars`, neither of which calls
`generate_factor_model`.

Why this is the right call:

- A user who fixes `Z[2, 1] = 0.5` expects 0.5 in the saved
  draws of `Z[2, 1]`. Rotating Λ into a canonical Z̃ would
  destroy that.
- For partial Z, any fixed non-zero entry in column k
  anchors the sign of that column, so the sign-mode
  equivalence the QR removes does not exist for those
  columns. Rotation would solve a non-problem.
- Users who want the structured-prior surface should reach
  for Phase 4's `loadings_prior`, which encodes belief
  through Φ on the unrotated Λ rather than hard constraints
  on Z.

Phase 3 work shipped:

- `@param trend_map` (`R/mvgam_core.R`) restated to make the
  partial-Z surface (NA = sampled, finite = preserved) and
  the QR bypass explicit.
- `normalise_trend_map()` accepted-shapes roxygen extended
  with the partial-Z entry plus a paragraph stating that
  any non-NULL `trend_map` bypasses the QR identification.
- Two invariant tests added to
  `tests/testthat/test-stancode-standata.R`: the partial-Z
  and the fully-fixed-Z stancode test bodies now assert that
  `Z_tilde`, `Q_tilde`, and `qr_thin_R` do NOT appear.

Existing `test-trend-map.R` partial-Z assertions were left in
place — they continue to describe the user-preserved-on-`Z`
semantic accurately because nothing changed about how partial
Z is sampled.

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
  Reads identified loadings via `extract_Z_loadings`, which
  in turn uses `factor_loading_param_pattern()`.
- `make_fixed_z_stanvars()`, `make_partial_z_stanvars()` in
  `R/stan_assembly.R`: stay as-is. They emit Z in the data /
  transformed-parameters block, no QR involved.
- `factor_loading_param_pattern()`,
  `factor_state_param_pattern()`,
  `hidden_unrotated_factor_pars()` in
  `R/sample_innovations.R`: the shared selectors added in
  Phase 2. Any new accessor that needs to read identified
  loadings, factor paths, or hide unrotated VAR coefficients
  should delegate to these helpers rather than rolling its
  own regex.
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
3. **`ordinate()` behaviour under the new architecture** —
   RESOLVED in Phase 2. The saved Z̃ is QR-canonical via
   `qr_thin_R`, so `ordinate.mvgam` operates on identified
   loadings; `extract_factor_loadings_array` reads `Z_tilde`
   when present and `Z` otherwise. The `ordinate` roxygen
   describes this routing.

## Deferred follow-ups

Items considered for the Heaps architecture work but scoped
out of Phases 1–3 (and not yet implemented). Each is small
enough to land as an isolated follow-up.

- **Shared variation matrix accessor.** Heaps emphasises
  `Δ = Z Z'` (equivalently `Z̃ Z̃'`) as the
  rotation-invariant interpretable quantity. A dedicated
  `shared_variation(object)` accessor returning a per-pair
  posterior summary of the off-diagonal entries would surface
  this without cluttering default `summary()` print. Out of
  Phase 2 scope because the per-pair output design needs its
  own pass (full `p × p` table vs top-K-by-magnitude).
- **Per-factor variance contribution table in `summary()`.**
  `plot_factors()` already computes per-factor variance shares
  via `lv_contribution_table(per_lv, Z_arr)`. Mirroring the
  table inside `summary.mvgam` would give a non-graphical
  caller the same information. Deferred for the same scoping
  reason as Δ.
- **`Sigma_trend_tilde` in generated quantities.** The
  innovation covariance for VAR factor dynamics rotates as
  `Q_tilde Sigma_trend Q_tilde'`. Phase 1 rotates the VAR
  coefficient array but not the innovation covariance, so the
  saved `Sigma_trend` is in the unrotated basis. Adding the
  rotated form is a 4-line genquant addition; deferred
  because it duplicates information already implicit in
  `A_trend_tilde` and the unrotated `Sigma_trend`, and most
  users will not consult it directly.
- **Rotated per-factor scalars.** `ar1_trend`, `sigma_trend`,
  etc. cannot meaningfully be rotated into per-factor
  scalars on `Z_tilde`; rotating gives `K × K` matrices that
  break the per-factor interpretation. Phase 1 documents
  this scope; users who need the rotated dynamics can
  multiply the saved draws manually via `Q_tilde`.
