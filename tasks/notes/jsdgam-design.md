# `jsdgam()` ecologist-facing wrapper — design note

Scope: pin the design of the `jsdgam()` user-facing entry point
(Step 5c of the v2.0 plan) so the implementation pass that follows
this note can land without re-litigating API decisions. The
foundation pieces are already shipped — Heaps & Jermyn (2024)
`loadings_prior` factor architecture, the closure-unit data
scaffold, and all four detection-error families (`nmix()` with
`type = "poisson_binomial" | "royle_nichols" | "poisson_poisson"`
and `occ()`). What is missing is the ecologist-friendly wrapper
that composes those pieces into a single call resembling
`Hmsc()` / `boral()` / `jSDM_*()` / `gjam()`.

References on the mvgam side:

- `/home/nicholas-clark/Desktop/mvgam/tasks/v2.0-remaining.md` §5
  (lines 398-823) — the existing plan skeleton.
- `/home/nicholas-clark/Desktop/mvgam/R/mvgam_core.R` —
  `mvgam()` / `mvgam_single()` entry points; class assignment
  `c("mvgam", "brmsfit")` at line 528.
- `/home/nicholas-clark/Desktop/mvgam/R/families.R` — closure-unit
  scaffold (lines 460-540), `nmix()` / `occ()` constructors,
  `prepare_closure_unit_family()` (line 1753),
  `dispatch_closure_unit_method()` (line 1942).
- `/home/nicholas-clark/Desktop/mvgam/R/validations.R` —
  `normalise_trend_map()` (line 726), `normalise_loadings_prior()`
  (line 4947), `validate_closure_unit_data()` (line 314).
- `/home/nicholas-clark/Desktop/mvgam/R/loadings_prior_helpers.R`
  — feature encoding (z-score, one-hot, ordered factor),
  distance-matrix validation, ARD length-scale book-keeping.
- `/home/nicholas-clark/Desktop/mvgam/R/ordinate.jsdgam.R` —
  forward-compatible `ordinate.jsdgam` method, currently fails
  with a clear `stop()` when called on plain mvgam fits.
- `/home/nicholas-clark/Desktop/mvgam/R/residual_cor.R:128` —
  `residual_cor.jsdgam` placeholder; switches to the real
  trait-corrected residual correlation when the wrapper lands.
- `/home/nicholas-clark/Desktop/mvgam/R/print.mvgam.R:111-138` —
  `print.mvgam` already branches on `model_spec$is_jsdgam` so
  per-species / per-site headers replace the time-series header.

External references (read for this note):

- HMSC source on GitHub
  (`hmsc-r/HMSC/R/Hmsc.R`) — full constructor with 20+ arguments;
  groups: response (`Y`, `YScale`), fixed effects (`XFormula`,
  `XData`, `X`, `XScale`, `XSelect`), reduced-rank regression
  (`XRRRData`, `XRRRFormula`, `ncRRR`), study design
  (`studyDesign`, `ranLevels`), traits (`TrFormula`, `TrData`,
  `Tr`, `TrScale`), phylogeny (`phyloTree`, `C`, `covRhoGroup`,
  `phyloFast`), distribution (`distr`), factor count truncation
  (`truncateNumberOfFactors`).
- boral CRAN documentation — `boral(y, X, traits, lv.control,
  family, formula, method, ...)`; trait-on-loadings via the
  `traits` argument; mixed `family` per response column.
- gjam vignette — `gjam(formula, xdata, ydata, modelList)` with
  per-column `typeNames` in `modelList` switching on data type
  (`"CON"`, `"CA"`, `"DA"`, `"PA"`, `"OC"`, `"CC"`, `"FC"`,
  `"CAT"`). Mixed-type responses are first-class. No phylogeny.
- sjSDM GitHub — `sjSDM(Y, env, biotic, spatial, family,
  iter, learning_rate, device, ...)` with PyTorch backend, neural
  network or linear env terms, MVN species covariance. No traits
  or phylogeny in the base call.
- jSDM reference index — one function per family
  (`jSDM_binomial_probit`, `jSDM_binomial_logit`,
  `jSDM_poisson_log`, `jSDM_gaussian`, plus long-format and
  species-constrained variants). MCMC backend. No phylogeny;
  traits not surfaced at the call.
- flocker README + tutorial — `make_flocker_data()` →
  `flock(f_occ, f_det, flocker_data, ...)`; wraps brms via
  `custom_family`; supports rep-constant / rep-varying single-
  season, dynamic colext, multi-species data-augmented. Long-
  format observation matrix with trailing-NA padding for ragged
  visits — the architectural ancestor of mvgam's closure-unit
  scaffold.
- ubms reference — `stan_occu`, `stan_occuRN`, `stan_colext`,
  `stan_pcount`, `stan_distsamp`, `stan_multinomPois`,
  `stan_occuTTD`. Formula-based unmarked-compatible interface;
  the coverage target for which detection-error families to
  ship under `jsdgam()`.

---

## 1. One-paragraph framing

`jsdgam()` is the user-facing entry point for Joint Species
Distribution Generalised Additive Mixed Models. Ecologists arrive
with a `sites × visits × species` array of detections (or
abundances), a per-species trait table, possibly a phylogenetic
tree, and one or two model formulae for environmental drivers.
What they want is a single Bayesian fit that decomposes species
distributions into shared environmental responses, residual
co-occurrence captured by latent factors, and (when replicate
visits are available) imperfect detection. Plain `mvgam()` can
fit each of those pieces — its `loadings_prior` argument already
implements the Heaps & Jermyn (2024) trait-on-loadings factor
prior, and `family = nmix()` / `occ()` handle detection-error —
but assembling them takes care: long-format reshape from the
species array, `trend_map` construction to share latent state
across visits within a closure unit, formula plumbing for
detection covariates, and translation of the `phylo` object into
a pairwise distance matrix `loadings_prior` accepts. `jsdgam()`
encapsulates that assembly behind an `Hmsc()`-shaped signature so
users coming from HMSC, boral, jSDM, gjam or sjSDM can translate
their existing workflow one argument at a time without learning
the underlying mvgam machinery first.

---

## 2. HMSC translation table

HMSC `Hmsc()` constructor signature (verbatim from
`hmsc-r/HMSC/R/Hmsc.R`):

```r
Hmsc(Y, XFormula = ~., XData = NULL, X = NULL, XScale = TRUE,
     XSelect = NULL, XRRRData = NULL, XRRRFormula = ~. - 1,
     XRRR = NULL, ncRRR = 2, XRRRScale = TRUE,
     YScale = FALSE, Loff = NULL,
     studyDesign = NULL, ranLevels = NULL,
     ranLevelsUsed = names(ranLevels),
     TrFormula = NULL, TrData = NULL, Tr = NULL, TrScale = TRUE,
     phyloTree = NULL, C = NULL, covRhoGroup = NULL,
     phyloFast = FALSE,
     distr = "normal", truncateNumberOfFactors = TRUE)
```

| HMSC argument          | mvgam `jsdgam()` equivalent                                  | Notes / mapping rule |
|------------------------|--------------------------------------------------------------|---------------------|
| `Y`                    | `data` (long-format) + `species = <col>`                     | Wide-to-long reshape is done inside `jsdgam()`; users can also pass a `n_sites × n_species` matrix and the wrapper pivots it. |
| `XFormula`, `XData`    | `formula = response ~ env_covariates, data = ...`            | Formula RHS supplies the ecology covariates; LHS supplies the species column. |
| `X`                    | not applicable                                               | mvgam composes design matrices from the formula; passing a raw `X` matrix is not supported. |
| `XScale`               | not applicable                                               | mvgam does not auto-scale covariates; users centre / scale themselves (matches brms convention). |
| `XSelect`              | not applicable                                               | mvgam has no built-in variable selection; users supply a smoothed / penalised RHS via `s()` / `t2()` for shrinkage. |
| `XRRR*`, `ncRRR`       | absorbed into `factor_formula = ~ env_covariates`            | Reduced-rank regression in HMSC = latent factors driven by an env LHS. In mvgam this is the concurrent-GLLVM pattern: env covariates appear in `factor_formula` rather than `formula =`. |
| `YScale`               | not applicable                                               | brms families fix the response scale by link; gaussian responses use the data scale directly. |
| `Loff`                 | `offset(...)` term in `formula`                              | Standard brms / mgcv offset syntax. |
| `studyDesign`          | `data` columns referenced by random-effects terms            | mvgam reads study design from the data frame; nested random effects spelled `(1 | site / plot)` in the formula. |
| `ranLevels`            | brms random-effects syntax in `formula` and `factor_formula` | HMSC's structured random level (e.g. spatial RL) maps to `gp(lon, lat)` in `factor_formula`. |
| `TrFormula`, `TrData`  | `loadings_prior = list(features = trait_df)`                 | mvgam encodes trait columns via `encode_loadings_features()`: numeric → z-score, factor → one-hot, ordered → numeric z-score. |
| `Tr`                   | `loadings_prior = list(features = <numeric matrix>)`         | Pre-encoded numeric matrix; rownames must match species levels or carry a `species` column. |
| `TrScale`              | always on (z-score)                                          | Matches Heaps & Jermyn (2024) Sect. 6.2 convention. |
| `phyloTree`            | `phylo = <ape::phylo>`                                       | Wrapper validates ultrametric via `ape::is.ultrametric()`, derives pairwise distances via `ape::cophenetic.phylo()`, rescales to `max(d) = 1`, attaches as `loadings_prior$distances$phylo`. |
| `C`                    | `loadings_prior = list(distances = list(phylo = C))`         | Users with a pre-computed correlation / distance matrix bypass `phylo` and pass `C` directly. |
| `covRhoGroup`          | not applicable                                               | mvgam's loadings prior uses a single phylo prior per column; per-covariate weighting on phylo would be a follow-up (see Q5). |
| `phyloFast`            | not applicable                                               | mvgam's loadings prior already uses the Heaps multiplicative-Cholesky form; no fast / slow toggle needed. |
| `distr`                | `family = bernoulli() / poisson() / ...`                     | Per-response distribution. HMSC's `distr` matrix shape (one per species) collapses to a single `family` because jsdgam fits all species under the same family. Mixed-family workflows route to gjam-style cases (Q4). |
| `truncateNumberOfFactors` | `n_lv = <int>`                                          | User-supplied factor count; default `min(5, n_species - 1)` matches the Heaps & Jermyn (2024) bird-case starting point. |

Translation summary: an HMSC user who reads this table top to
bottom should be able to translate any single-distribution HMSC
fit into a `jsdgam()` call without consulting any other doc. The
two genuine mismatches are (i) `distr` (HMSC allows mixed
distributions across species, mvgam ships one family per fit),
and (ii) `XSelect` (no built-in variable selection — use
smooths / brms priors instead).

---

## 3. Cross-package comparison grid

| Package / call         | Response shape           | Family options                                                  | Traits | Phylo | LV factors | Multithread | Detection-error families                            | Hierarchical / spatial    |
|------------------------|--------------------------|-----------------------------------------------------------------|--------|-------|------------|-------------|------------------------------------------------------|---------------------------|
| HMSC `Hmsc()`          | wide `n_site × n_sp` Y   | normal, probit, logit, lognormal-Poisson, Poisson (4 distr)     | yes    | yes   | yes        | no          | none (cannot model imperfect detection)              | `ranLevels` structured RE |
| boral `boral()`        | wide Y                   | binomial, poisson, negbin, normal, tweedie, ordinal             | yes    | no    | yes        | no          | none                                                  | site effects only         |
| gjam `gjam()`          | wide Y + `typeNames`     | mixed per column: `CON / CA / DA / PA / OC / CC / FC / CAT`     | partial (slope-trait interactions) | no | yes | no | none | random groups               |
| sjSDM `sjSDM()`        | wide Y                   | binomial (probit/logit), poisson, negbin, gaussian              | no     | no    | implicit MVN species cov | optional GPU | none | spatial DNN block         |
| jSDM `jSDM_*()`        | wide or long Y           | binomial probit/logit, poisson, gaussian (one per fn)            | no     | no    | yes        | no          | none                                                  | site effects              |
| **mvgam `jsdgam()`**   | long-format data frame   | bernoulli, binomial, poisson, negbinomial, nmix(3 variants), occ | yes    | yes   | yes (Heaps) | pending #229-#232 | nmix("poisson_binomial" / "royle_nichols" / "poisson_poisson"), occ() | brms RE / `gp()` / GAM smooths in both `formula` and `factor_formula` |

Headline read: `jsdgam()` is the only package in the row that
combines (Heaps-style structured loadings prior with traits + phylo)
× (closure-unit detection error families) × (full brms RE / GP /
smooth flexibility on both occupancy / abundance and detection
formulae). HMSC has the trait+phylo machinery but no imperfect-
detection support; ubms has occupancy + N-mixture but no LV factor
JSDM machinery. jsdgam closes that gap by composing the two on top
of mvgam's existing two-formula architecture.

---

## 4. Proposed `jsdgam()` signature

```r
jsdgam(
  formula,                # one-sided env covariates: response ~ covs
  factor_formula = ~ 1,   # alias for trend_formula in factor mode
  data,                   # long-format data frame
  species,                # name of the column holding the species id
  unit,                   # name of the column identifying a site / sampling unit
  visit          = NULL,  # optional visit / replicate id; required for closure-unit families
  family         = bernoulli(),
  traits         = NULL,  # data.frame, matrix, or string lookup into data2
  phylo          = NULL,  # ape::phylo, or pre-computed distance matrix
  data2          = NULL,  # passthrough for matrices referenced by traits / phylo
  n_lv           = NULL,  # default min(5, n_species - 1L)
  loadings_prior = NULL,  # override the trait/phylo prior; otherwise auto-built
  trend_map      = NULL,  # auto-built for closure-unit families; pass explicitly to override
  prior          = NULL,
  threads        = NULL,
  backend        = getOption("brms.backend", "cmdstanr"),
  ...                      # forwarded to mvgam() / Stan sampler
)
```

Argument-by-argument:

- `formula`: standard brms-style formula. LHS names the response
  column (counts or 0/1); RHS the env-covariate part of the
  occupancy / abundance linpred. For closure-unit families the
  detection sub-formula rides on `bf(response ~ env, p ~ det)`.
- `factor_formula`: alias for `mvgam()`'s `trend_formula`. Carries
  the latent-factor structure: `~ 1` (default; pure residual JSDM
  with iid factors), `~ gp(lon, lat)` (spatial GLLVM), `~ env`
  (concurrent-GLLVM / constrained-ordination). Renamed from
  `trend_formula` because in the JSDM context there is no temporal
  trend — the factors are over species, not over time.
- `data`: long-format data frame, one row per (unit, visit,
  species) observation for closure-unit families, or one row per
  (unit, species) for plain JSDM families. The wrapper accepts a
  wide `n_unit × n_species` matrix at the top level and pivots
  internally; the long-format path is the canonical one for users
  who have repeat visits.
- `species`, `unit`: bare or quoted column names. The wrapper
  renames internally to `series` and (with closure units) `time` so
  the underlying mvgam machinery sees its expected names.
- `visit`: required for closure-unit families; identifies the
  replicate-visit within each (unit, species) closure unit. Plain
  JSDM families omit `visit`.
- `family`: any closure-unit family or plain count / binary
  family (see §6 for the coverage matrix).
- `traits`, `phylo`: ecologist-friendly aliases. Compiled into a
  `loadings_prior` spec before being handed to `normalise_loadings_prior()`.
- `n_lv`: factor count. Default `min(5, n_species - 1L)` matches
  the Heaps & Jermyn (2024) bird-case starting point.
- `loadings_prior`: escape hatch. If the user passes this directly,
  the wrapper checks that `traits` / `phylo` were NOT also passed
  (error on conflict) and forwards the spec as-is.
- `trend_map`: auto-built for closure-unit families (see §5 for
  the construction rule). Users may override; conflict with
  `loadings_prior` is detected and rejected per `mvgam()`'s
  existing rule.

Recommended workflow (4-6 lines):

1. Build a long-format `data.frame` with one row per
   (unit, visit, species, response) observation.
2. Assemble a `traits` data.frame (one row per species; species
   column or row names) and a `phylo` `ape::phylo` object.
3. Call `jsdgam(formula = y ~ env_covariates,
   factor_formula = ~ gp(lon, lat), data = ..., species, unit,
   visit, family = nmix(), traits, phylo, n_lv = 4)`.
4. Inspect with `summary(fit)`, `loo(fit)`, `pp_check(fit)`.
5. Ordinate with `ordinate(fit)`; visualise residual species
   correlation with `plot(residual_cor(fit))`.
6. Forecast / predict at new sites via `predict(fit, newdata)` —
   inherited from `mvgam()`.

Three usage examples:

```r
# (1) Single-visit JSDM: presence / absence + traits + phylo + 4 latent factors
fit_pa <- jsdgam(
  formula        = present ~ s(elev) + s(precip),
  factor_formula = ~ gp(lon, lat, n_lv = 4),
  data           = sites_x_species_long,
  species        = species,
  unit           = site,
  family         = bernoulli(),
  traits         = species_traits,
  phylo          = bird_phylo
)

# (2) N-mixture jsdgam: replicated counts, per-visit detection,
#     per-species traits driving the residual factor loadings
fit_nmix <- jsdgam(
  formula        = bf(count ~ s(elev) + s(precip),
                       p ~ s(tod) + (1 | observer)),
  factor_formula = ~ gp(lon, lat, n_lv = 3),
  data           = camera_traps_long,        # one row per (site, visit, species)
  species        = species,
  unit           = site,
  visit          = visit_id,
  family         = nmix("poisson_binomial"),
  traits         = species_body_mass_diet,
  phylo          = mammal_phylo,
  threads        = brms::threading(4)
)

# (3) Single-season occupancy jsdgam: per-visit detection,
#     trait + phylo shrinkage on factor loadings
fit_occ <- jsdgam(
  formula        = bf(detected ~ s(habitat),
                       p ~ s(date_of_year)),
  factor_formula = ~ gp(lon, lat, n_lv = 3),
  data           = bird_surveys_long,
  species        = species,
  unit           = transect,
  visit          = visit_id,
  family         = occ(),
  traits         = avonet_traits,
  phylo          = passerine_phylo
)
```

---

## 5. DRY discipline: what is reused, what is new

The single most consequential decision in this design: every
piece of `jsdgam()` should be a thin reshape / alias layer on top
of the existing `mvgam()` infrastructure. New code lives in one
file (`R/jsdgam.R`) and consists almost entirely of (a) input
reshape, (b) `loadings_prior` and `trend_map` construction from
ecologist-friendly aliases, (c) dispatch into `mvgam()`. No
parallel pipeline. No parallel Stan code.

### 5.1 Response-shape validation and naming

**Reuse, with one new helper.** Plain mvgam already validates the
long-format frame via `validate_required_variables()`
(R/validations.R). The jsdgam-specific check is "for closure-unit
families, every (unit, species) pair has the same number of
visits, or NA-padded ragged visits with no internal gaps." Add:

- `validate_jsdgam_response_shape(data, species, unit, visit,
  family, response_var)` in `R/jsdgam.R`. Returns `invisible(TRUE)`
  or stops with the standard `insight::format_error(c(..., x = ...,
  i = ...))` shape. Checks: species column non-empty, unit column
  non-empty, visit column required for closure-unit families,
  response column numeric and family-compatible (delegate the
  family check to `validate_closure_unit_data()` for closure-unit
  families).

Do NOT add this to `R/validations.R` — it is jsdgam-specific
reshape logic. Keep it next to the wrapper that calls it.

### 5.2 Long-format reshape

**New, single helper.** Plain mvgam accepts only long format. The
wrapper is the one place that does the wide-to-long pivot when a
user passes a matrix or wide data frame. Add:

- `pivot_jsdgam_data(data, species, unit, visit, response_var)` in
  `R/jsdgam.R`. Three input shapes accepted:
  - long data.frame with explicit `species` / `unit` / `visit`
    columns (passthrough).
  - wide matrix or data.frame with one row per unit and one
    column per species (pivot via `tidyr::pivot_longer()` or
    base `stack()`; rename to `species` / `series` and `unit` /
    `time`).
  - 3-D array `[unit, visit, species]` for closure-unit families
    (flatten to long with one row per cell, NA cells dropped
    after a trailing-NA-only check matching the flocker
    convention).

The output is always a long-format data.frame with columns
`series` (species), `time` (unit), and (closure-unit only) a
visit column. mvgam's existing closure-unit data prep then takes
over.

Closest existing analogue: there is no wide-to-long helper in
mvgam today; every example assumes the user supplies long format.
The pivot helper is genuinely new but small (~40 lines).

### 5.3 `trend_map` auto-build for closure-unit families

**Extend `normalise_trend_map()` minimally; add one helper in jsdgam.R.**

The plain mvgam closure-unit families fit one response per call.
A multi-species closure-unit fit shares the latent state across
visits within (species, unit) but each (species, unit) is its
own closure unit. That collapses to "the species column is the
series, the unit column is the time, and the visit column carries
the visit-replicate within the closure unit." The existing
`build_closure_unit_arrays()` already groups by (series, time), so
the trend_map for the latent-factor part of a closure-unit
jsdgam fit is exactly what plain mvgam's factor models use: one
free latent factor per species shared by some number `n_lv` of
factors.

Pseudocode for the auto-build:

```r
# Inside jsdgam(): after the long-format reshape and after
# n_lv is resolved.
if (is.null(trend_map)) {
  if (is_closure_unit_family(family)) {
    # Closure-unit auto-build: each species gets one latent
    # abundance / occupancy state; the visits within a (species,
    # unit) closure unit are aggregated by the Stan lpmf, not by
    # trend_map. The factor part is sampled directly by mvgam's
    # default n_lv-factor machinery, so we pass the "free Z"
    # signal via trend_map = NULL and let normalise_trend_map()
    # fall through to the default sampled-Z path.
    trend_map <- NULL
  } else {
    # Plain JSDM (no detection error): same default; mvgam's
    # default sampled-Z path produces the n_lv-factor JSDM.
    trend_map <- NULL
  }
}
```

So in fact `trend_map` does NOT need any closure-unit-aware
construction — the closure-unit grouping is handled by the family
scaffold and the JSDM factor structure is handled by mvgam's
existing default sampled-Z path. The auto-build is a no-op; the
helper exists only as a forward extensibility hook for users who
want to pin some loadings (e.g. fixing the diagonal to anchor
identification).

`normalise_trend_map()` requires no change. The data.frame branch
already handles "one row per series mapping to a trend integer",
which is the only sensible jsdgam-side input.

### 5.4 `loadings_prior` construction from `traits` + `phylo`

**Wrapper-side compilation; reuse `normalise_loadings_prior()`.**

```r
# In jsdgam(): translate ecologist aliases to a loadings_prior spec.
build_jsdgam_loadings_prior <- function(traits, phylo,
                                          loadings_prior, data2) {
  # Conflict check: explicit loadings_prior overrides traits / phylo,
  # and supplying both is ambiguous.
  if (!is.null(loadings_prior) &&
      (!is.null(traits) || !is.null(phylo))) {
    stop(insight::format_error(c(
      "Pass either `traits` / `phylo` aliases OR `loadings_prior`, not both.",
      i = "Compose them yourself when you need the full surface."
    )))
  }
  if (!is.null(loadings_prior)) return(loadings_prior)
  if (is.null(traits) && is.null(phylo)) return(NULL)
  spec <- list()
  if (!is.null(traits)) {
    spec$features <- traits
  }
  if (!is.null(phylo)) {
    # ape integration: validate phylo, derive cophenetic distance,
    # standardise to max(d) = 1 (matches the loadings_prior internal
    # default). Lift these three calls into one helper so the
    # error messages are uniform.
    phylo_dist <- jsdgam_phylo_to_dist(phylo)
    spec$distances <- list(phylo = phylo_dist)
  }
  spec
}

jsdgam_phylo_to_dist <- function(phylo) {
  insight::check_if_installed("ape")
  checkmate::assert_class(phylo, "phylo")
  if (!ape::is.ultrametric(phylo)) {
    stop(insight::format_error(c(
      "'phylo' must be an ultrametric phylogeny.",
      i = paste0(
        "Convert via ape::chronos() or ape::compute.brlen() ",
        "before passing to jsdgam()."
      )
    )))
  }
  d <- ape::cophenetic.phylo(phylo)
  d <- d / max(d)                       # max(d) = 1; matches Heaps
  d
}
```

Note: the `max(d) = 1` rescaling is also performed by
`validate_pairwise_distance()` inside `normalise_loadings_prior()`.
Performing it once on the wrapper side is harmless (the second
rescale is a no-op) but documents the convention at the user-
facing surface; pre-rescaling also lets `jsdgam_phylo_to_dist()`
warn cleanly if the cophenetic computation produces a degenerate
distance matrix (all zeros — happens with a single-tip pruned
tree).

`ape` joins the Suggests list. Already implied by HMSC-style
workflows; no new heavy dependencies.

### 5.5 Family routing

```r
# Inside jsdgam(): once data is long-format and family is resolved,
# the call is a single forward into mvgam(). No special-case branches.
mvgam_call <- list(
  formula        = formula,
  trend_formula  = factor_formula,        # alias swap
  data           = long_data,
  data2          = data2,
  family         = family,
  trend_map      = trend_map,
  loadings_prior = loadings_prior_spec,
  prior          = prior,
  threads        = threads,
  backend        = backend
)
mvgam_call <- c(mvgam_call, list(...))
fit <- do.call(mvgam, mvgam_call)
class(fit) <- c("jsdgam", class(fit))   # prepend; preserve mvgam + brmsfit
attr(fit, "jsdgam_meta") <- list(
  species_col = species,
  unit_col    = unit,
  visit_col   = visit,
  n_species   = ...,
  n_unit      = ...,
  family_name = resolve_family_name(family)
)
fit
```

The whole point of routing every family through `mvgam()`
unchanged is to inherit closure-unit dispatch
(`prepare_closure_unit_family()`,
`dispatch_closure_unit_method()`) for free. No closure-unit
family logic appears in `R/jsdgam.R`.

### 5.6 `ape` integration

The three `ape` calls (`is.ultrametric`, `cophenetic.phylo`,
`max(d) = 1` scaling) live inside `jsdgam_phylo_to_dist()` in
`R/jsdgam.R`. They are the only place mvgam touches `ape`. There
is no compelling reason to lift them into
`R/loadings_prior_helpers.R` today — every other distance source
in `loadings_prior` is a user-supplied pre-built matrix.
Promotion to a shared helper becomes worthwhile only when a
second ecology-flavoured wrapper (e.g. a future `phylogam()` for
single-species phylo-GAMs) needs the same lift.

### 5.7 S3 method inheritance via `class = c("jsdgam", "mvgam")`

**Confirmed adequate after audit.** Every existing S3 method
dispatches against the `mvgam` class:

- `predict.mvgam`, `posterior_predict.mvgam`,
  `posterior_epred.mvgam`, `log_lik.mvgam` — all signature
  `function(object, ...)` and the body reads
  `is_closure_unit_family(object$family)`. No explicit class check
  beyond `checkmate::assert_class(object, "mvgam")`. A `jsdgam`
  object passes that assertion.
- `summary.mvgam`, `print.mvgam`, `pp_check.mvgam`,
  `conditional_effects.mvgam`, `residuals.mvgam`, `augment.mvgam`,
  `tidy.mvgam` — same story. `print.mvgam` already branches on
  `model_spec$is_jsdgam`, so the print-time per-species / per-site
  headers are wired in; the wrapper just has to set that flag.
- `residual_cor.jsdgam` and `ordinate.jsdgam` already exist as
  forward-compat methods; the wrapper unblocks them by emitting
  fits that satisfy their slot-access expectations
  (`object$obs_data`, `attr(object$model_data,
  "prepped_trend_model")$unit`).

The only place jsdgam needs a new S3 method override is when the
inherited behaviour is wrong in the multi-species context — see
§8.

---

## 6. Family coverage decisions

The §5 coverage table in the planning doc commits to bernoulli /
binomial / poisson / negbinomial / nmix(3 variants) / occ(single-
season). Pin those for v2.0:

| Family                            | Ship in v2.0? | Notes |
|-----------------------------------|---------------|-------|
| `bernoulli()`                     | yes           | HMSC `distr = "probit"` equivalent. Default for first-pass JSDMs. |
| `binomial()`                      | yes           | For aggregated detection histories or "N out of K visits detected" data. |
| `poisson()`                       | yes           | Plain count JSDM. boral / gjam parity. |
| `negbinomial()`                   | yes           | Over-dispersed counts. gjam `"DA"` analogue. |
| `nmix("poisson_binomial")`        | yes (shipped) | Royle (2004) N-mixture. |
| `nmix("royle_nichols")`           | yes (shipped) | Binary detection / non-detection N-mixture. |
| `nmix("poisson_poisson")`         | yes (shipped) | Neyman Type A camera-trap encounter rate. |
| `occ()`                           | yes (shipped) | MacKenzie 2002 single-season occupancy. |
| `gaussian()`                      | optional      | Continuous response JSDM (rare in ecology; biomass, plant cover). Low cost; ships already through plain mvgam. Recommend exposing in jsdgam if test workload allows. |
| `Gamma()`                         | follow-up     | Strictly-positive continuous (biomass without zeros). Not a v2.0 blocker. |
| `tweedie()`                       | follow-up     | Compound Poisson-Gamma (zero-inflated biomass / cover). Ships through plain mvgam already; jsdgam-side smoke test only. |
| Multi-state occupancy (`occ_ms()`)| no (v2.1+)    | Not in mvgam's family scaffold. Cost: a fourth closure-unit family with the same scaffold pattern as `occ()`. Track as task #23X. |
| Dynamic colonisation / extinction (`stan_colext` equivalent) | no (v2.1+) | Requires time-varying latent occupancy — outside the closure-unit grain assumption. Track separately. |
| Distance sampling                 | no            | ubms `stan_distsamp` parity; out of scope and not requested. |

**Recommended ship list for v2.0:** bernoulli, binomial, poisson,
negbinomial, gaussian (already in mvgam, ~zero extra cost),
nmix(all 3), occ. Eight families, all under the same scaffold;
nothing new in mvgam's family set is required.

---

## 7. Multithreading interaction

Multithreading (#229-#232) is pending; the brms-threading
composition note pins the design (post-process the brms-emitted
stancode + `force = TRUE` to suppress the auto-thread branch +
inject our own `partial_sum_<family>_lpmf` via stanvars). The
question is whether jsdgam needs to be threading-aware.

**Recommendation: jsdgam ships first, threading follows as a
mechanical layer.** Justification:

- `jsdgam()` does not generate Stan code. Every fit routes through
  `mvgam()` → `prepare_closure_unit_family()` → `make_*_stanvars()`.
  Threading lives in those primitives, not in the wrapper.
- The Stan-code threading hook (Option ii in
  `brms-threading-composition.md`) is independent of how the family
  was constructed at the R level. A jsdgam-built `nmix()` fit and
  a plain-mvgam-built `nmix()` fit share the same Stan code, the
  same closure-unit arrays, the same `partial_sum`.
- jsdgam fits ARE the high-value workload for threading: `N_unit ×
  N_species` is exactly the chunk count `reduce_sum` parallelises.
  A 100-site × 50-species jsdgam fit becomes 5000 closure units,
  which gives meaningful per-chunk parallelism on 4-8 threads.
- The performance warn ("`N_unit < 4 * threads`") in the threading
  composition note will rarely trigger for jsdgam fits; it is the
  small-camera-trap nmix() workload that risks under-utilisation.
  Add a complementary warn to jsdgam: "`n_species == 1L` —
  consider `mvgam()` directly" (Q4).

**Ship order:** Step 5c (jsdgam wrapper) first, then Step 5d
(threading). Users running jsdgam without `threads` get the same
performance as today; users running jsdgam with `threads` get
nothing until 5d lands but no breakage. This avoids interleaving
two large changes.

---

## 8. Post-fit method coverage

What HMSC users expect, mapped to mvgam's existing surface:

| HMSC method                       | mvgam equivalent                              | Gap? |
|-----------------------------------|-----------------------------------------------|------|
| `sampleMcmc()`                    | `mvgam()` internal Stan sampler call          | none |
| `predict()`                       | `predict.mvgam()`, `posterior_predict.mvgam()` | none |
| `evaluateModelFit()`              | `loo.mvgam()`, `pp_check.mvgam()`, `bayes_R2.mvgam()` | none |
| `convertToCodaObject()`           | `as.mcmc.mvgam()` / `as_draws_*.mvgam()`      | none |
| `computeVariancePartitioning()`   | `bayes_R2()` partition; no direct equivalent  | gap: variance partition per fixed-effect group, traits, factors. Follow-up task #238. |
| `getPostEstimate()` (Beta, Gamma, V) | per-parameter `as.data.frame.mvgam()`     | none |
| `plotBeta()`                      | `conditional_effects.mvgam()`                 | none |
| `plotGamma()` (trait-on-loadings) | new: requires extracting the `theta_features` posterior and plotting trait length-scales. Follow-up task #239. | gap |
| `biPlot()` (ordination)           | `ordinate.jsdgam()` (already exists)          | none |
| `computeAssociations()` (residual cor) | `residual_cor()` + `plot()` (already exists as placeholder; unblocks under jsdgam) | none, once unblocked |
| `predictLatentFactor()` for new units | `predict.mvgam(newdata, type = "linpred")` with factor draws | partial — works for new sites; new species would need a forecasting branch (follow-up). |
| Cross-validation by site / species | `loo.mvgam()` + tail-of-site / leave-species-out scoring | gap: leave-one-species-out CV would need a per-response loo split. Follow-up task #240. |

**Two genuine gaps that the wrapper unblocks but does not fill:**

1. `computeVariancePartitioning()` — a per-formula-term variance
   contribution decomposition. Reasonable to defer to v2.1; track
   as task **#238 (Variance partitioning for `jsdgam`)**.
2. `plotGamma()` — a trait-on-loadings effect plot. The
   `loadings_prior$features` matrix carries one ARD length-scale
   per encoded column; surfacing those posteriors with a plot
   method is mechanical. Track as task **#239 (Trait
   length-scale visualisation)**.
3. Per-species cross-validation. Track as task
   **#240 (Leave-one-species-out CV for `jsdgam`)**.

**Two methods need jsdgam-specific overrides:**

- `residual_cor.jsdgam`: already a placeholder; replace the
  `stop()` body with the trait-corrected residual correlation that
  uses the Heaps Cholesky decomposition of `Z`.
- `ordinate.jsdgam`: forward-compatible code already exists;
  unblocks once jsdgam emits fits that satisfy the slot-access
  expectations (`object$obs_data`, `attr(object$model_data,
  "prepped_trend_model")$unit`).

Everything else inherits unchanged from mvgam through
`class = c("jsdgam", "mvgam", "brmsfit")`.

---

## 9. Open questions

**Q1. Long-format vs wide-format default input.**
Should `data` accept a wide `n_unit × n_species` matrix at the
top level (with the wrapper pivoting), or insist on long format?
Options:
- (A) accept both; pivot wide → long internally.
- (B) accept only long; document the pivot in the vignette.
- (C) accept only wide for plain JSDM families; only long for
  closure-unit families (which need a per-visit row).
Recommendation: **(A)**. Users coming from HMSC / boral think in
wide; ecologists coming from camera-trap workflows think in long.
Supporting both has a small wrapper-side cost (~20 lines) and
removes the friction. Risk: pivot semantics confuse users who
pass a non-canonical wide format. Mitigation: error fast on
ambiguous shapes (no rownames, no species column, etc.) with
explicit examples.

**Q2. `factor_formula` vs `trend_formula` naming.**
The planning doc proposes renaming `trend_formula` to
`factor_formula` inside the jsdgam call. Options:
- (A) `factor_formula` alias (rename at wrapper edge).
- (B) keep `trend_formula`; document in the help that "trend" is
  read as "factor" in the JSDM context.
- (C) accept both.
Recommendation: **(A)**. "Trend" is misleading in a JSDM with no
temporal axis. The aliasing is mechanical (one rename at the top
of the wrapper). Risk: HMSC users may still pass `trend_formula`
because they see it in mvgam vignettes. Mitigation: hard-error
on `trend_formula` with a one-line fix-it message.

**Q3. `n_lv` default.**
Options:
- (A) `min(5, n_species - 1L)` (matches Heaps 2024 bird case).
- (B) `2L` (matches boral default).
- (C) require user to set it; error if NULL.
Recommendation: **(A)**. Boral's `n_lv = 2` is too small for any
non-trivial fit; HMSC defaults to `truncateNumberOfFactors = TRUE`
which auto-truncates by spike-and-slab. `min(5, n_species - 1L)`
gives sensible behaviour on small species sets while not over-
parameterising tiny fits. Risk: 5 factors is computationally
heavier than 2; warn at fit time if `n_species < 5` and the
default kicked in.

**Q4. Single-species `jsdgam()` calls.**
Options:
- (A) hard-error: "use `mvgam()` directly for single-species fits".
- (B) warn and proceed: the JSDM reduces to a plain mvgam.
- (C) silently delegate to `mvgam()`.
Recommendation: **(B)**. The user may have written the call as
part of a wider workflow that iterates over species sets;
hard-erroring breaks that. The warning points at `mvgam()` for
single-species. Risk: the latent-factor part of the model
collapses to a single trend with no shared structure; the
posterior on `Z` is uninformative. Mitigation: the warn explains
this explicitly.

**Q5. Per-covariate phylo weighting (HMSC `covRhoGroup`).**
HMSC allows a per-covariate weighting on the phylogenetic prior
(some env covariates are more phylogenetically conserved than
others). Options:
- (A) skip in v2.0 — `loadings_prior$distances$phylo` applies the
  same prior to every loading column.
- (B) extend `loadings_prior` with a `column_phylo_weight`
  argument and propagate to the Stan code.
- (C) document the workaround (fit separate jsdgam models per
  covariate group).
Recommendation: **(A)** for v2.0; **(B)** as a follow-up
(task #241). The Heaps prior structure already allows per-column
shrinkage via the MGP option; per-column phylo weighting is a
natural extension but requires Stan changes.

**Q6. Mixed-distribution responses (gjam parity).**
HMSC accepts a per-species `distr` matrix; gjam accepts per-
column `typeNames`. mvgam's brms-derived family system is one
family per fit. Options:
- (A) skip: jsdgam users with mixed-type responses fit one
  jsdgam per family, then combine post-hoc.
- (B) write a mixed-family wrapper that splits the response into
  per-family blocks and refits each separately, recombining the
  factor draws.
- (C) wait for brms / mvgam multivariate response infrastructure
  to extend to mixed families.
Recommendation: **(A)** for v2.0; flag (C) as the long-term
direction. (B) is a hack that breaks the joint-factor sharing.

**Q7. Spatial factors vs temporal factors.**
The factor_formula can carry `gp(lon, lat)` for spatial factors
or `AR(p = 1)` for temporal. Can it carry both? Options:
- (A) only one factor type per fit (gp OR AR).
- (B) gp + AR additive: `gp(lon, lat) + AR(p = 1)`.
- (C) test both and document.
Recommendation: **(C)**. mvgam's trend constructor scaffold
already supports additive combinations on `trend_formula`; this
should "just work" in jsdgam. Risk: identifiability between
spatial and temporal factors when both are sparsely sampled.
Mitigation: a docs example only; do not advertise the
combination on the help page until the recovery test exists.

**Q8. Visit-aggregated input.**
Some users have detection / non-detection data already
aggregated to "K out of N visits detected per (unit, species)".
Options:
- (A) hard-require disaggregated visit rows for closure-unit
  families.
- (B) accept aggregated input and expand internally (each row
  becomes K detection rows + (N - K) non-detection rows).
- (C) provide a separate `binomial()` family path for the
  aggregated form (no closure-unit machinery).
Recommendation: **(C)**. binomial() with trials = N and
y = K is the natural model for aggregated detection histories
when within-visit detection variation is not of interest. Direct
the user to it on the jsdgam help page.

**Q9. Returning the raw `n_species × n_lv` Z draws vs the
identified `Z_tilde`.**
mvgam's default sampled-Z path applies a thin-QR post-hoc
rotation to produce `Z_tilde` with positive diagonal (Heaps
& Jermyn 2024). jsdgam needs to surface those identified
loadings for trait-on-loadings inference. Options:
- (A) inherit the mvgam default; surface `Z_tilde` everywhere.
- (B) add a jsdgam-specific override that re-rotates to a
  trait-axes basis.
- (C) leave it to `ordinate.jsdgam()` (already in place).
Recommendation: **(A)** for v2.0; (B) tracked as task #239 along
with the trait length-scale plot.

**Q10. Default prior on the per-species intercept.**
Plain mvgam inherits brms defaults. In jsdgam with `n_species` =
100 and a high-dimensional formula, the default `Student-t(3,
0, 2.5)` on per-species intercepts is wide enough to admit
poorly identified species (rare detections). Options:
- (A) inherit brms default (no change).
- (B) tighten to `Normal(0, 2)` per species — matches HMSC's
  spike-and-slab interior.
- (C) auto-tighten when `n_species > 20`.
Recommendation: **(A)** for v2.0. Custom priors are easy to
supply via the `prior` argument; auto-tightening risks silently
shifting recovery in cases users expect brms defaults. Document
the recommendation in a vignette section.

---

## 10. Implementation chunking proposal

Same pattern as the RN / PPM ports: each chunk is commit-sized
with a code-review gate at the end.

**Chunk 1 — wrapper skeleton + reshape + ape integration.**
Deliverables:
- `R/jsdgam.R`: `jsdgam()` constructor, argument validation,
  long-format pivot, ape integration (`jsdgam_phylo_to_dist()`).
- One smoke test in `tests/testthat/test-jsdgam.R` that fits a
  bernoulli jsdgam on 8 species × 30 sites simulated data with a
  trivial `phylo` and asserts the call returns a valid object of
  class `c("jsdgam", "mvgam", "brmsfit")`.
- ape promoted to Suggests in DESCRIPTION.
- `validate_jsdgam_response_shape()` + `pivot_jsdgam_data()`.

Code-review gate: confirm DRY discipline — no closure-unit
logic, no Stan code, no parallel pipeline.

**Chunk 2 — `loadings_prior` compilation from traits + phylo.**
Deliverables:
- `build_jsdgam_loadings_prior(traits, phylo, loadings_prior,
  data2)` translating ecologist aliases to a
  `normalise_loadings_prior()`-compatible spec.
- Conflict-detection between `traits`/`phylo` and explicit
  `loadings_prior`.
- Recovery test: fit jsdgam on simulated data with known
  trait-mediated loading structure; assert posterior mean Z
  reproduces the truth at `cor > 0.7`. Use the same simulation
  pattern as the existing Heaps tests in
  `tests/testthat/test-loadings-prior.R`.

Code-review gate: trait encoding matches `encode_loadings_features()`
without re-implementing it.
Stats-review gate: recovery threshold is defensible (compare against
plain `loadings_prior` recovery to ensure jsdgam adds no bias).

**Chunk 3 — `factor_formula` alias + closure-unit dispatch.**
Deliverables:
- Argument rename `factor_formula` → `trend_formula` at the
  wrapper edge.
- Hard error on bare `trend_formula` with a fix-it message.
- Smoke test: fit jsdgam with `family = nmix()`, `family =
  occ()`, and a `factor_formula = ~ gp(lon, lat)` block. Assert
  closure-unit grain dispatch reaches `posterior_predict.mvgam`
  (per-visit shape) and `log_lik.mvgam` (per-unit shape) without
  the existing multivariate hard-error firing.
- Lift the `log_lik.mvgam` multivariate-closure-unit guard
  (lines 99-111) to allow jsdgam-class fits to proceed.

Code-review gate: the multivariate guard lift does not allow
plain `mvgam()` multivariate closure-unit fits to slip through.
Detection should key off `class(object)` carrying `"jsdgam"`.

**Chunk 4 — `class` extension + S3 method audit + jsdgam-specific
overrides.**
Deliverables:
- Set `class(fit) <- c("jsdgam", "mvgam", "brmsfit")`.
- Set `attr(fit, "jsdgam_meta")` with species_col, unit_col,
  visit_col, n_species, n_unit, family_name.
- Set `model_spec$is_jsdgam = TRUE` so `print.mvgam`'s existing
  per-species / per-site branch activates.
- Confirm `predict`, `posterior_predict`, `posterior_epred`,
  `log_lik`, `summary`, `pp_check`, `conditional_effects`,
  `residuals`, `augment`, `tidy`, `loo`, `waic`, `ordinate`,
  `residual_cor` all dispatch correctly via inherited methods.
- Replace `residual_cor.jsdgam`'s `stop()` body with the actual
  trait-corrected residual correlation implementation.

Code-review gate: no jsdgam-specific S3 method exists that is not
strictly necessary. Every inherited method's test coverage
includes one jsdgam-class fixture.

**Chunk 5 — recovery + concordance tests + `how_to_cite`.**
Deliverables:
- Recovery test (`tests/local/fixtures/val_jsdgam_*.rds`): fit a
  60-site × 10-species bernoulli jsdgam with known trait /
  phylo loadings; recover within 3 joint MCMC SE.
- Concordance test against HMSC on the same simulated data;
  posterior-mean Beta agreement within 3 joint MCMC SE
  (matches the flocker concordance pattern in
  `tests/local/fixtures/val_occ_*.rds`).
- `how_to_cite.mvgam`: detect `inherits(object, "jsdgam")` and
  add Ovaskainen et al. 2017 (Hmsc), Hui 2016 (boral), Tikhonov
  et al. 2020 (MEE Hmsc paper), Heaps & Jermyn 2024.
- Documentation: `?jsdgam` man page with the three usage
  examples from §4; cross-reference to `?nmix`, `?occ`,
  `?loadings_prior`.

Code-review gate: citation list is the bare minimum sufficient
to cite the model; no padding.
Stats-review gate: concordance threshold is defensible; the
simulated truth covers a realistic trait / phylo loading
configuration (not just intercept-only).

**Chunk 6 — vignette + final polish.**
Deliverables:
- One vignette `vignettes/jsdgam.Rmd` walking through the three
  §4 examples end-to-end, with `ordinate()` and `residual_cor()`
  plots.
- README section pointing at jsdgam for ecologists arriving
  from HMSC / boral / gjam.
- `NEWS.md` entry for v2.0 release.

Code-review gate: vignette runs end-to-end inside the CI time
budget on the cached cmdstanr install.

Six chunks, each commit-sized, each independently reviewable.
Total LOC budget: ~600 lines in `R/jsdgam.R` plus ~400 lines of
tests, plus the vignette. No new Stan code. No parallel pipeline.

---

## §5b — Factor-side environmental structure (latent-factor-varying effects)

**Headline.** Constrained / concurrent ordination — where each latent
factor `k` is itself a smooth function of a *different* environmental
gradient (factor 1 ~ s(elevation), factor 2 ~ s(precip)) — is the
JSDM pattern an HMSC user reaches for when they want a-priori
interpretable axes rather than a free rotation. The existing §4 sketch
of `factor_formula = ~ env` papers over a genuine ambiguity: does the
env smooth load on all factors equally, or does each factor get its
own smooth? Auditing the source shows mvgam currently supports
neither cleanly. `mu_trend` is sized per `(time, series)`, not per
factor, so `s(x, by = trend)` has nothing to bind to on the
trend-side data and silently fails. The trend linpred can shape a
shared response curve that all factors load on (option a below), but
the per-factor (`by = trend`) machinery is absent and the roxygen
example at `R/trend_system.R:2451` is aspirational rather than wired
up.

### Current mvgam state

- **`trend_data` carries `(time, series, covariates...)`, no `trend`
  column.** `R/validations.R:4776–4805` builds the trend-side data by
  `group_by(time, series) %>% summarise(across(trend_variables,
  first))`. There is no `mutate(trend = ...)` step, no aliasing of
  `series` to `trend`, anywhere along the path from `mvgam()` →
  `extract_trend_data()` → `parse_multivariate_trends()` →
  `brms::brm(formula = trend_formula, data = trend_data)`.

- **`mu_trend` is sized per observation, not per factor.**
  `R/stan_assembly.R:2516` declares
  `vector[N_trend] mu_trend = ...`, and the assembly comment at
  L2447–2448 spells it out: "`mu_trend` can carry per-(time, series)
  values." `N_lv_trend` (L2439) sizes only `lv_trend` and `Z`. The
  trend linpred and the factor axis live in different dimensions and
  the Stan code has no path to pipe a smooth basis through the
  `N_lv_trend` axis.

- **`extract_trend_linpred()` returns `[n_time, n_series]`.**
  `R/extract_trend_linpred.R:23` documents the shape; the reshape at
  L98–135 (`reshape_linpred_to_grid`) is explicit:
  `out <- matrix(0, nrow = n_time, ncol = n_series)`. There is no
  factor axis anywhere in the post-processing path either, which
  matches the Stan side.

- **`s(x, by = trend)` in `trend_formula` is silently broken.**
  The roxygen example at `R/trend_system.R:2451` writes
  `trend_formula = ~ s(season, bs = 'cc', k = 5, by = trend)`. The
  trend-side data has no `trend` column, so brms's smooth machinery
  either errors with `object 'trend' not found` at design-matrix
  construction time or, worse, picks up an unrelated symbol from the
  formula environment. No test exercises this path (`grep "by = trend"
  tests/` returns nothing), so the breakage is invisible. `by =
  series` is not tried either.

- **What the trend formula `can` do today.** Plain smooths and
  parametric terms (`~ s(season) + temp`) work: they emit a shared
  `mu_trend[t, s]` that is added to *every* series's
  `dot_product(Z[s, :], lv_trend[t, :])`. Read as a factor model, this
  is "one shared env response across all series, plus a free
  rotation of `n_lv` residual factors". It does NOT give different
  factors different env structure.

### HMSC / gllvm / boral comparison

| Package | What `factor ~ env` actually does | User-facing syntax |
|---------|-----------------------------------|---------------------|
| HMSC    | XRRR is *not* on the factor axis. `X_RRR (S × p_RRR) %*% t(wRRR) (ncRRR × p_RRR)` produces `ncRRR` synthetic columns that are *cbind*-ed onto X and then enter the species-specific `Beta` matrix. `Eta * Lambda` (the latent factors) remain a separate residual block. Verified from `R/updatewRRR.R`: `XB = XRRR %*% t(wRRR); X = cbind(X, XB)`. The "rank reduction" is on the species-coefficient matrix, not on the latent ordination. | `Hmsc(..., XRRRData = df, XRRRFormula = ~ env1 + env2, ncRRR = 2)` |
| gllvm   | True constrained / concurrent ordination. Each latent variable is `z_i = B' x_lv,i + eps_i`; `num.RR` zeros the residual (pure constrained), `num.lv.c` keeps it (concurrent). Verified from `gllvm::gllvm` docs: `num.lv.c`, `num.RR`, `lv.formula`. This is the per-factor-env-gradient pattern. | `gllvm(y, X, lv.formula = ~ env1 + env2, num.RR = 2)` |
| boral   | `boral(..., lv.control = ...)` exposes constrained latent variables analogous to gllvm's `num.RR` (older RJAGS implementation; documented as constrained ordination). Effectively the same semantics as gllvm but slower and JAGS-bound. | `boral(y, X.lv = env, lv.control = list(num.lv = 2, type = "independent"))` |

The user's mental model of XRRR ("HMSC's constrained-ordination
pattern") is closer to gllvm's `num.RR` than to what HMSC's XRRR
actually does. An ecologist coming from HMSC asking for
"factor-varying env effects" almost certainly wants gllvm's
constrained-ordination semantics: factor 1 loads on elev, factor 2
loads on precip, species then load on those interpretable axes.

### Identifiability tradeoffs

- **(a) Shared env smooth, free factors (today's `factor_formula = ~
  s(env)`).** The smooth adds a single `mu_trend[t, s]` that all
  series see; the `n_lv` factors remain rotation-free residual
  structure. The env response is identified at the species mean
  level, but the factors themselves carry no env interpretation; any
  post-hoc `ordinate()` / varimax rotation can reassign which factor
  "looks like" the env axis. An ecologist who wants a JSDM with an
  env covariate adjustment plus residual co-occurrence picks this.

- **(b) Per-factor env smooths (`s(env, by = trend)`).** Each
  factor `k` gets its own env response `f_k(env)`; the env
  constraints break rotation invariance and the factor axes become
  a-priori interpretable. This is gllvm's `num.lv.c`. An ecologist who
  wants "factor 1 = elevation axis, factor 2 = precip axis, species
  load on those axes" picks this. The price is that the factors are
  no longer pure residual; the env block competes with `Beta` on X
  for the same variance.

- **(c) HMSC XRRR.** Reduced-rank species-coefficient matrix on env
  covariates; nothing to do with the latent factors. An HMSC user who
  has hundreds of env covariates and wants regularisation on `Beta`
  picks this. mvgam users would get the same effect from
  penalised smooths (`s(env, bs = "ts")`) in `formula =`. Not the
  thing the user is asking for under "factor-varying effects".

The clean recommendation: jsdgam should expose (b) under
`factor_formula = ~ s(env, by = trend)`, document (a) as the default
(`factor_formula = ~ env` — no `by` — adds env to all factors
uniformly), and ignore (c) since penalised smooths already cover
that ground.

### Proposed jsdgam API

Default: `factor_formula = ~ 1` → iid factors (current behaviour,
matches the §4 sketch). Constrained-ordination opt-in via
`factor_formula = ~ s(env, by = trend)`:

```r
# Factor 1 tracks elevation, factor 2 tracks precipitation;
# species load on those constrained axes.
jsdgam(
  formula        = abundance ~ 1,
  factor_formula = ~ s(elev, by = trend, k = 5) +
                    s(precip, by = trend, k = 5) +
                    AR(n_lv = 2),
  data           = comm_df,
  species        = "species",
  unit           = "site",
  family         = poisson()
)
```

The wrapper passes `factor_formula` through as `trend_formula` and
relies on per-factor smooth machinery (does not yet exist — see
implementation cost below). Each `s(env, by = trend)` term expands
to `n_lv` smooths, one per factor; the smooth basis is evaluated at
each unit's env value and added to `lv_trend[t, k]` *before* `Z`
maps it onto species. With `n_lv = 2` and two `by = trend` smooths,
factor 1's env response is `s(elev) + s(precip)` evaluated at
`trend == 1`, factor 2's at `trend == 2`.

### Implementation cost if `by = trend` is missing

The audit confirms it IS missing. To wire it up:

- **`R/validations.R:4776` (`extract_trend_data`):** when any term in
  the parsed trend formula references `trend` as a `by` variable,
  the helper must replicate `trend_data` `n_lv` times and add a
  `trend = factor(1:n_lv)` column (analogous to brms long-format
  expansion for `by = ` smooths). Cost: ~30 lines plus a branch in
  the `summarise` path.

- **`R/stan_assembly.R` `mu_trend` block (L2515–2517):** today
  `mu_trend` is `vector[N_trend]` indexed by `(time, series)`. To
  carry per-factor smooths we either (i) introduce a parallel
  `matrix[N_time_trend, N_lv_trend] mu_lv_trend` populated from the
  expanded design matrix and folded into `lv_trend` before `Z`, or
  (ii) refactor `mu_trend` itself to `[N_time_trend, N_lv_trend]`
  whenever the formula contains `by = trend`. Option (i) is
  additive and leaves the existing per-`(t,s)` path untouched for
  PB / RN / PPM / occ; option (ii) is cleaner but rewrites the
  injection points at L2528 (`mu_<resp>[n] += trend[…]`). Cost: ~80
  lines + Stan-template changes; risk: medium because the
  `lv_trend` / `Z` decomposition is shared with all factor-based
  trends.

- **`R/extract_trend_linpred.R` and `reshape_linpred_to_grid`:**
  reshape must learn to return a per-factor matrix when the formula
  carries `by = trend`. Cost: ~40 lines plus a metadata flag on the
  fit object.

- **`extract_component_linpred(component = "trend")`:** must apply
  the brms newdata expansion across the `trend = factor(1:n_lv)`
  axis. Cost: ~20 lines.

- **Risk to PB / RN / PPM / occ:** all four use `mu_trend[N_trend]`
  with no `by = trend` term. If option (i) above is chosen, the
  existing path is untouched and these families keep working with
  no behaviour change. Option (ii) would force a regression sweep
  across `tests/testthat/test-nmix.R`, `test-occ.R`, and the
  shared-trend tests.

Total cost: ~170 LOC plus tests, gated on choosing option (i) for
backwards compatibility. The cleanest delivery is a separate
follow-up after jsdgam v2.0 ships with shared env smooths only;
factor-varying env can land in v2.1 once the `by = trend` machinery
is in place.

### Open questions

- **Q1.** Should `by = trend` expansion happen inside `mvgam()` /
  `extract_trend_data()` (so plain mvgam users also get it) or
  inside the `jsdgam()` wrapper alone? The mvgam-wide path is
  cleaner but exposes a new failure mode for trend-only users who
  wrote `by = trend` not knowing it would now do something. Pin the
  scope before implementing.

- **Q2.** With per-factor env smooths the factors gain a-priori
  identifiability and the post-hoc `ordinate()` / QR + sign-fix step
  in `generate_factor_model()` (`R/stan_assembly.R:2939`) may become
  redundant or actively harmful (rotating away the env constraint).
  Confirm the identification block can be skipped when any `by =
  trend` term is present.

- **Q3.** Does the env-constrained factor compete with a Heaps
  trait-loaded `Z` prior on the same variance? If `loadings_prior$
  features = trait_df` is also specified, do we double-shrink
  toward both env-driven `lv_trend` and trait-driven `Z`? Needs a
  stats-reviewer look before exposing both knobs in the same call.

- **Q4.** Roxygen example at `R/trend_system.R:2451` references `by
  = trend` but the path does not work. Cheapest fix is a doc patch
  (drop the example or rewrite to `by = series`); long-term fix is
  to actually wire `by = trend`. Confirm which we pin for the next
  CRAN cut.
