# Downstream Consumer Rebuild — Phase 3.1 Audit

## Context

The brms-integration refactor stripped most downstream consumers (marginaleffects
hooks, `conditional_effects`, `logLik`, etc.) when their dependencies on the
old prediction stack (`mvgam_predict`, raw `model_output` trend-draws layout,
`mgcv_model`-based linear-predictor reconstruction) were removed. The new
prediction primitives — `posterior_linpred.mvgam`, `posterior_epred.mvgam`,
`posterior_predict.mvgam`, with `predict.mvgam` / `fitted.mvgam` as thin
wrappers — now provide a brms-shaped API on top of the new Stan output. The
consumers must be rebuilt against these primitives. The user has explicitly
flagged that the master implementations carried significant complexity that the
new primitives now handle for free, so the rebuild target is **brms-parity
simplicity**, not a 1:1 restoration. State-space context (process-error
toggle, time-marginalisation, hierarchical/grouped trends) is the only place
deviation from brms is justified.

## Architectural constraint

The new primitives work in exactly two modes: **predicting at observed time
points** (when `newdata` rows match the in-sample time grid) and
**time-marginalised** prediction (when `newdata` averages over the in-sample
time range, the natural setting for marginaleffects-style partial effects).
They do **not** extrapolate to new out-of-sample time points — that path
requires a C++ trend extrapolator that has not yet been written. Anything
that needs forward-in-time extrapolation (forecast + forecast scoring) is
**out of scope for this audit** and will be revisited once the extrapolator
exists. `process_error` is the user-facing knob that chooses whether to
propagate latent-state uncertainty (`TRUE`, default) or fix the trend at its
posterior mean (`FALSE`, faster, marginaleffects-friendly).

## Currently-wired downstream surface (state-of-the-world)

### pp_check.mvgam
- File / line: `R/ppc.mvgam.R:1095`
- Approach: dispatches to bayesplot `ppc_*` / `ppd_*` by `type=`, builds
  `yrep` via `posterior_predict.mvgam` (default) or `posterior_epred.mvgam`
  (for `type = "error_binned"`); plots DS residuals for `type` matching
  `resid_*`.
- Live `type=` branches: every bayesplot ppc that does not need a
  `psis_object` / `lw` argument — distribution overlays, intervals,
  scatter, ribbon, stat, ECDF, error histograms, and the mvgam-specific
  `resid_hist` / `resid_ribbon` (and `_grouped` variants).
- Stubbed / NOT working: LOO-PIT family (`loo_pit`, `loo_pit_overlay`,
  `loo_pit_qq`, `loo_intervals`, `loo_ribbon`). The stub sets
  `ppc_args$psis_object <- NULL` and `ppc_args$lw <- NULL` at lines
  1307–1320 with the comment `# compute_loo() not available yet for
  mvgam`. These `type=` values fall through to bayesplot with `NULL`
  weights and will error or render uninformative plots.
- Verdict: healthy for non-LOO `type=` values; PSIS path is the only
  failing branch.

### as_draws_*.mvgam family
- File / line: `R/as.data.frame.mvgam.R:166` (`as_draws.mvgam`); sibling
  methods at lines 206 (`as_draws_matrix.mvgam`), 245 (`as_draws_df.mvgam`),
  284 (`as_draws_array.mvgam`), 323 (`as_draws_list.mvgam`); plus
  `as.data.frame.mvgam` (line 88), `as.matrix.mvgam` (113), `as.array.mvgam`
  (139).
- Mock-brmsfit pathway: intact. Every method builds
  `dummy <- structure(list(fit = x$model_output), class = 'brmsfit')`,
  delegates to the brms / posterior generic on `dummy`, then renames via
  `validate_variables(x, variable = ...)$newnames`.
- Free-rider integrations: tidybayes / ggdist / posterior / bayesplot
  functions that call `as_draws*()` or `as.array()` on an mvgam fit work
  via this pathway.
- Verdict: healthy, no rebuild required, no stale dependencies.

### loo.mvgam / loo_compare.mvgam
- File / line: `R/loo.mvgam.R:121` (`loo.mvgam`), `R/loo.mvgam.R:182`
  (`loo_compare.mvgam`); helper `clean_ll` at line 250.
- Current log-lik extraction path: calls **`logLik(x, ...)`** at lines
  138 and 141, with `extract_family_pars(x)` and a `type = 'link'` call
  to `predict()` to build the link-scale linear predictor.
- **Stale dependency alert**: `logLik.mvgam`, `extract_family_pars`, and
  `mvgam_predict` are **not defined anywhere on the branch**. `loo.mvgam`
  will throw `could not find function "logLik.mvgam"` (or fall through
  to `stats::logLik.default`, which will fail differently) at the
  first call. `loo.mvgam` is in the wired surface in the sense that it
  is exported, but it is **broken at runtime** until `log_lik.mvgam` is
  rebuilt.
- Implication for rebuilt `log_lik.mvgam`: this is the single critical
  blocker for the LOO path (loo, loo_compare, LFO, PSIS in `pp_check`).
  Rebuild first. `loo.mvgam` should be retargeted from `logLik(x, ...)`
  to `log_lik(x, ...)` (the brms / loo standard verb), and the
  `incl_dynamics` branch — which currently stages a pre-built
  `predict(type='link', process_error=FALSE)` link matrix and feeds it
  to `logLik` — should be replaced by a `process_error` argument on
  `log_lik.mvgam` directly. `clean_ll` (R/loo.mvgam.R:250) is dependency-
  free and stays.

## Removed consumers to rebuild

### get_predict.mvgam, get_coef.mvgam, get_vcov.mvgam (marginaleffects)

- Source on master:
  - `get_coef.mvgam` — `R/marginaleffects.mvgam.R:83`
  - `set_coef.mvgam` — `R/marginaleffects.mvgam.R:101`
  - `get_vcov.mvgam` — `R/marginaleffects.mvgam.R:121`
  - `get_predict.mvgam` — `R/marginaleffects.mvgam.R:132`
  - `get_data.mvgam` and `get_data.mvgam_prefit` — `R/marginaleffects.mvgam.R:170`
    and onwards
  - `find_predictors.mvgam` and `find_predictors.mvgam_prefit` —
    `R/marginaleffects.mvgam.R:405` and onwards
- Old public API (signatures on master):
  - `get_coef.mvgam(model, trend_effects = FALSE, ...)`
  - `set_coef.mvgam(model, coefs, trend_effects = FALSE, ...)`
  - `get_vcov.mvgam(model, vcov = NULL, ...)`
  - `get_predict.mvgam(model, newdata, type = 'response', mfx, newparams,
    ndraws, se.fit, process_error = FALSE, ...)`
- Old dependencies (what each method pulled on):
  - `get_coef` / `set_coef` — reads/writes `model$mgcv_model$coefficients`
    (or `model$trend_mgcv_model$coefficients` for `trend_effects = TRUE`).
    Because `get_vcov` returns `NULL`, marginaleffects skips the
    delta-method path and falls back to the `posterior_draws` attribute,
    so the mgcv coefficients returned here are interface-satisfying and
    never actually used downstream. The `mgcv_model` slot still exists
    on the branch.
  - `get_vcov` — returns `NULL` unconditionally. Already minimal.
  - `get_predict` — calls the old `predict.mvgam(object, newdata, type,
    process_error, summary = FALSE, ...)` and packages the result as
    `data.frame(rowid, estimate = apply(preds, 2, median))` with
    `attr(out, "posterior_draws") <- t(preds)`. The `type` argument
    (`response / link / expected / variance / terms / latent_N /
    detection`) was the single most complex piece of the old
    `predict.mvgam` and the entire reason `get_predict` had a `type=`
    parameter.
  - `get_data.mvgam` — pulls `model.frame(x, trend_effects = TRUE/FALSE)`
    and reconstructs a joint observation+trend-level data frame (trend_map
    dedup, binomial `cbind` artifact, `series` join, `trend_y` drop).
    **Depends on `model.frame.mvgam`, which has also been removed from
    the branch** (master had `R/model.frame.mvgam.R`).
  - `find_predictors.mvgam` — walks `mgcv_model` and `trend_mgcv_model`
    smooths to collect predictor names; appends `trend_model` `unit / gr /
    subgr` for grouped trends; appends `cap` for nmix; appends `offset`
    names parsed from `attr(terms(x$call), 'offset')`.
- brms parity reference: marginaleffects' brmsfit dispatch works by
  calling `posterior_epred` / `posterior_predict` on the model with the
  user's `type=`, packing the per-row posterior median into a `data.frame`,
  and stuffing the full draws matrix into `attr(out, "posterior_draws")`.
  marginaleffects then computes summaries / contrasts / slopes from the
  draws attribute and ignores `get_coef` / `get_vcov` for Bayesian models.
- mvgam state-space concerns:
  - **process_error toggle**: marginaleffects users want partial effects
    averaged over the *covariate* of interest, not over latent-state
    realisations they didn't condition on. Default should be
    `process_error = FALSE` (master's choice) so AMEs and slopes do not
    bleed trend uncertainty into covariate effects. Document it as the
    deviation from brms-parity and explain why.
  - **time-marginalisation**: average partial effects naturally
    time-marginalise — marginaleffects builds a counterfactual grid that
    spans the in-sample time range, and the primitives' time-marginalised
    mode handles this without special treatment.
  - **hierarchical / grouped trends**: when `trend_map` collapses
    multiple series onto a shared trend, the rowid mapping in
    `get_predict`'s output frame must align with the *observation* frame
    rows the user passed, not the trend frame. This is the
    biggest mvgam-specific quirk and the master `get_data.mvgam` body
    (R/marginaleffects.mvgam.R:170–280) reflects it.
- Target primitive: `posterior_epred.mvgam` for `type = 'response'` (the
  marginaleffects default, since marginaleffects wants `E[Y|X]` for
  averaging), `posterior_linpred.mvgam` for `type = 'link'`,
  `posterior_predict.mvgam` for `type = 'prediction'` (note: brms
  uses `'response'` here, not `'prediction'` — match brms verbatim).
  The N-mixture special cases (`type = 'latent_N'`, `'detection'`) are
  a true mvgam-specific deviation and should be handled by a thin switch
  inside `get_predict`.
- Simplification opportunity: master's `get_predict.mvgam` body is
  ~25 lines but rides on a `predict.mvgam` that on master was ~600 lines
  of bespoke link-scale reconstruction, family inverse-link, and trend
  addition. The new primitives already do all of that, so the new
  `get_predict.mvgam` collapses to: switch on `type=`, dispatch to the
  right primitive, build the rowid data.frame, attach `posterior_draws`.
  ~15 lines, no `mgcv_model` access, no family inverse-link branch, no
  trend-add branch. `get_coef` and `get_vcov` stay as thin shims.
- Rebuild notes: thin shim for the trio (`get_coef`, `get_vcov`,
  `get_predict`); `get_data.mvgam` and `find_predictors.mvgam` are
  separable and probably need their own task because they currently
  depend on a removed `model.frame.mvgam`. **Open question for the TRD**:
  do we rebuild `model.frame.mvgam` to support `get_data` /
  `find_predictors`, or do we ship marginaleffects support that bypasses
  `get_data` by piggybacking on the brms parity (marginaleffects can
  reach into a brmsfit-like object through `as_draws*`)?

### conditional_effects.mvgam

- Source on master: `R/conditional_effects.R:127`, with the
  `mvgam_conditional_effects` plot/print methods at lines 269 and 314.
- Old public API (signature on master): `conditional_effects.mvgam(x,
  effects = NULL, type = 'expected', points = FALSE, rug = FALSE, ...)`.
  Returns an object of class `mvgam_conditional_effects` (named list of
  ggplots, one per effect).
- Old dependencies:
  - Calls `marginaleffects::plot_predictions(x, condition = ..., draw =
    TRUE, type = type, points = points, rug = rug, ...)` for each
    derived condition set — so under the hood it depends on
    `get_predict.mvgam` and `get_data.mvgam` working.
  - Pulls term labels via `terms(formula(x))` and
    `terms(formula(x, trend_effects = TRUE))`. The trend-formula path
    requires `formula.mvgam` with a `trend_effects` argument — master
    has `R/formula.mvgam.R`, branch does not.
  - `split_termlabs` (R/conditional_effects.R:329) parses smooth /
    `dynamic()` / interaction strings to expand into up-to-3-way
    condition tuples for plot_predictions. This helper is self-contained
    (only depends on `rlang::parse_expr`) and can move forward unchanged.
- brms parity reference: brms's `conditional_effects.brmsfit` builds a
  condition grid by enumerating `terms(formula(x))`, calls `posterior_epred`
  on that grid, summarises into a data frame, and returns one ggplot per
  conditioning set. The master mvgam version effectively reproduces this
  via `plot_predictions`.
- mvgam state-space concerns:
  - **`process_error` toggle**: same logic as for marginaleffects —
    default should be `FALSE` so conditional plots do not double-count
    trend uncertainty. The current master version inherits the
    marginaleffects default through `get_predict`.
  - **time-marginalisation**: `plot_predictions` builds a grid over the
    *condition* covariate(s) and lets all other covariates take their
    `datagrid()` default (mean for numeric, reference for factor). Time
    will be one of those "all other" covariates and will be marginalised
    naturally.
  - **planned series-aware enhancement**: the user has flagged that the
    rebuild should add a series-specific argument so users can plot
    conditional effects per series (e.g. `conditional_effects(mod,
    series = 1)` or `series = "all"`). This is a brms deviation, but the
    `plot_predictions` `condition` argument supports adding `series` as a
    secondary condition, so the enhancement is a small wrapper around
    that pathway, not a new primitive.
- Target primitive: indirectly `posterior_epred.mvgam` (via marginaleffects
  `plot_predictions` -> `get_predict` -> primitive). Direct dispatch is
  also possible if we want to bypass marginaleffects for plotting and
  do it natively, but that throws away the marginaleffects ecosystem.
- Simplification opportunity: the master body is ~140 lines of
  `if (length(cond_labs[[i]]) == 1) ... 2 ... 3` repetition with
  near-identical `plot_predictions` calls. This collapses to a single
  loop using `do.call(plot_predictions, c(list(x, condition =
  cond_labs[[i]]), shared_args))`. Saves ~80 lines and a maintenance
  hazard.
- Rebuild notes: needs `get_predict.mvgam` working first. Then a thin
  refactor of the master body plus the series-aware argument. If we
  decide `formula.mvgam(x, trend_effects = TRUE)` is too much to rebuild
  in this phase, the trend-formula term enumeration can be punted and
  the function will simply not auto-enumerate trend-level effects (user
  can still pass them explicitly via `effects=`).

### log_lik.mvgam

- Source on master: `R/logLik.mvgam.R:56`. Master used the `stats::logLik`
  generic, not `brms::log_lik`; for brms parity the new method should be
  `log_lik.mvgam`.
- Old public API (signature on master): `logLik.mvgam(object, linpreds,
  newdata, family_pars, include_forecast = TRUE, ...)`. Returns an
  `n_samples x n_observations` matrix.
- Old dependencies:
  - `mcmc_chains(object$model_output, 'mus')` — pulled the `mus` block
    from the old Stan output. Replacement: `posterior_linpred` plus
    inverse-link (or `posterior_epred` directly).
  - `extract_family_pars(object)` — pulled family-specific parameters
    (shape, scale, overdispersion). Not on the branch. Replacement:
    `as_draws_matrix(object, variable = ...)` with brms-style parameter
    names.
  - `mvgam_predict(..., density = TRUE)` — returned per-row log densities
    instead of samples. Not on the branch. Replacement: per-family
    log-density dispatcher keyed off `object$family`.
  - `attr(object$mgcv_model, 'trials')` for binomial models — still on
    the branch.
  - Special-cases nmix by extracting `detprob` and `trend` blocks from
    `model_output`; computes log-lik on the latent abundance scale.
- brms parity reference: `brms::log_lik(object, newdata = NULL,
  re_formula = NULL, resp = NULL, ndraws = NULL, draw_ids = NULL,
  ...)`. Returns an `S x N` matrix of pointwise log densities. The mvgam
  method should match this signature and add `process_error = TRUE`
  (the state-space deviation).
- mvgam state-space concerns:
  - **`process_error` toggle**: when `TRUE` (the brms-equivalent
    behaviour for a state-space model), each draw's log density at
    observation `t` is computed at *that draw's* sampled trend value.
    When `FALSE`, the trend is fixed at its posterior mean. Per the
    architectural framing, `process_error = TRUE` is the default and the
    only path that matches brms's expectation.
  - **time-marginalisation**: not relevant for `log_lik` — the function
    is evaluated at the actual observation time points, not a marginal
    grid.
  - **Open question (note in audit)**: should `log_lik` return a single
    `S x N` matrix where each row is one *posterior draw* of the linear
    predictor (brms convention), or an `S x N` matrix where each row is
    one (linpred-draw, trend-draw) pair (per-process-realisation, the
    natural state-space convention)? The two coincide when
    `process_error = FALSE`. The right call is probably the brms
    convention: paired draws, one per posterior draw, with the trend
    realisation embedded in `posterior_linpred(process_error = TRUE)`.
- Target primitive: `posterior_linpred.mvgam(process_error = TRUE)`
  for the linear-predictor draws, plus a per-family log-density helper.
- Simplification opportunity: master's body is ~165 lines, of which ~40
  lines are series/test_data assembly that is now obsolete (the
  primitives handle `newdata = NULL` via `object$data`). The new body
  should be ~50 lines: get linpred, get family-par draws via
  `as_draws_matrix`, dispatch to per-family log-density, return matrix.
  The per-family helper has a natural sibling in `R/posterior_epred.R`
  where the `posterior_epred_<family>` dispatchers already live —
  symmetry suggests adding `log_lik_<family>` helpers in the same file.
- Rebuild notes: highest priority — `loo.mvgam`, `loo_compare.mvgam`,
  `lfo_cv.mvgam`, and the PSIS path in `pp_check.mvgam` all depend on
  this. After it lands, the `incl_dynamics` codepath in `loo.mvgam` (R/loo.mvgam.R:125–154)
  should be replaced by `log_lik(x, process_error = TRUE)` and the
  explicit `predict(type='link', process_error=FALSE) + extract_family_pars`
  staging deleted.

### waic.mvgam

- Source on master: **no `waic.mvgam` method exists on master**. The
  master package does not export a WAIC method; users compute WAIC via
  `loo::waic(log_lik(model))` after first computing log-lik themselves.
- Implication: a "rebuild" of `waic.mvgam` is actually a *new* method,
  not a recovery. The motivating reason to add it is brms parity —
  `brms::waic.brmsfit` exists.
- Old public API: not applicable.
- Old dependencies: not applicable.
- brms parity reference: `brms::waic.brmsfit(x, ..., compare = TRUE,
  resp = NULL, pointwise = FALSE, model_names = NULL)`. Returns a
  `loo::waic` object. Almost the entire body delegates to
  `loo::waic.matrix(log_lik(x))`.
- mvgam state-space concerns: same as `log_lik` — `process_error` should
  default to `TRUE` and be passed through.
- Target primitive: `log_lik.mvgam` (via `loo::waic`).
- Simplification opportunity: if `log_lik.mvgam` exists, `waic.mvgam`
  is a 5-line wrapper: `loo::waic(log_lik(x, ...))`. Less code than the
  decision to ship it requires.
- Rebuild notes: **flag for TRD**: do we ship `waic.mvgam` as a one-line
  brms-parity convenience, or document `loo.mvgam` as the canonical
  information-criterion entry point (since LOO is generally preferred
  over WAIC for posterior model comparison anyway, and the loo
  documentation already says so)? Argument for shipping: matches brms;
  no maintenance cost. Argument against: signals that WAIC is on equal
  footing with LOO when it isn't, and adds a re-export of `loo::waic`
  for one line of dispatch code.

### PSIS path inside pp_check.mvgam

- File / line of stub: `R/ppc.mvgam.R:1307–1320`. Both `psis_object`
  (line 1307) and `lw` (1314) are set to `NULL` with a comment that
  reads `# compute_loo() not available yet for mvgam`.
- Affected `type=` values: `loo_pit`, `loo_pit_overlay`, `loo_pit_qq`,
  `loo_intervals`, `loo_ribbon`. These are listed in the
  `aps_types` block (R/ppc.mvgam.R:1197–1219) but are currently
  unsupported because they need PSIS weights.
- Dependency on `log_lik.mvgam`: PSIS weights come from
  `loo::psis(-log_lik(object, newdata = newdata))`, with `r_eff`
  computed from `loo::relative_eff(exp(log_lik))`. Same machinery
  `loo.mvgam` is supposed to use; once `log_lik.mvgam` exists, both
  `loo.mvgam` and this stub become one-line fixes.
- Old behaviour on master: did not exist as a stub — master
  `pp_check.mvgam` either lived in a similar place or supported PSIS
  via `compute_loo`, but the branch comment shows a `compute_loo`
  helper was assumed and never implemented. This is a leftover from
  the refactor, not a regression.
- Target primitive: `log_lik.mvgam` -> `loo::psis` -> `weights()`.
- Simplification opportunity: the stub is already minimal; the rebuild
  is replacing `NULL` with a `compute_psis(object, newdata)` helper that
  wraps the loo call. ~10 lines, lives wherever the rebuilt `log_lik`
  lives.
- Rebuild notes: path inside an existing wired file, not a fresh
  method. Order it after `log_lik.mvgam` lands.

## Suggested rebuild order (forward-pointer for the TRD)

1. **`log_lik.mvgam`** — everything downstream depends on it; also
   un-breaks the currently-stale `loo.mvgam`, `loo_compare.mvgam`, and
   `lfo_cv.mvgam`.
2. **marginaleffects S3 trio** (`get_predict`, `get_coef`, `get_vcov`)
   — small surface, biggest ecosystem unlock. Decide as part of this
   whether to also rebuild `get_data.mvgam` / `find_predictors.mvgam`
   (depends on whether `model.frame.mvgam` is rebuilt).
3. **`conditional_effects.mvgam`** — depends on the marginaleffects
   trio; add series-aware argument here.
4. **PSIS path in `pp_check.mvgam`** — one-line fix once `log_lik`
   lands; closes the loo-pit bayesplot types.
5. **`waic.mvgam`** (or punt to `loo`) — decision item; if shipped, is
   a 5-line wrapper.

Out of scope (defer until C++ extrapolator): `forecast.mvgam`,
`score.mvgam_forecast`, and any consumer that depends on forward-in-time
extrapolation.

## Open questions for the TRD

- Should `waic.mvgam` ship as a brms-parity one-liner, or do we document
  `loo.mvgam` as the canonical information-criterion entry point and
  skip WAIC entirely?
- For the marginaleffects S3 trio, do we need a custom `get_vcov`, or
  does the mock-brmsfit pathway (via `as_draws*`) give marginaleffects
  the posterior covariance it needs for free?
- `conditional_effects.mvgam`: add the planned series-aware argument
  during this rebuild, or restore brms-parity first and enhance in a
  follow-up?
- `log_lik.mvgam`: per-observation across the response scale (brms
  convention, one log density per draw) or per-observation per
  (linpred-draw, trend-draw) pair (the natural state-space convention
  when `process_error = TRUE`)? Default to brms convention with
  `process_error = TRUE` baked into each linpred draw.
- For `get_data.mvgam` and `find_predictors.mvgam`: the master
  implementations depend on a `model.frame.mvgam` and a
  `formula.mvgam(x, trend_effects = TRUE)` that have both been removed
  from the branch. Rebuild those helpers as part of the marginaleffects
  task, or skip `get_data` / `find_predictors` for now and rely on
  marginaleffects' brms-fallback path?
- The old `predict.mvgam` had a `type=` argument (`link / expected /
  response / variance / terms / latent_N / detection`); the new
  `predict.mvgam` does not. Should the rebuilt `get_predict.mvgam`
  dispatch on `type=` itself (switching to `posterior_linpred` /
  `posterior_epred` / `posterior_predict`), or should we add a `type=`
  argument back onto `predict.mvgam` so the consumer can stay a thin
  wrapper?
- N-mixture-only types (`latent_N`, `detection`) — keep them
  consumer-side (a switch in `get_predict.mvgam` / `conditional_effects`)
  or expose them on the primitives directly?
