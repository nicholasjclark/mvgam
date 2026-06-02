# Task Requirements Document: Downstream Consumer Rebuild

## 1. Overview

The brms-integration refactor stripped most downstream prediction consumers
(marginaleffects hooks, `conditional_effects`, log-likelihood, PSIS-LOO in
`pp_check`). It also silently broke `loo.mvgam`, `loo_compare.mvgam` and
`lfo_cv.mvgam`, which still call removed helpers and fall through to
`stats::logLik.default`. This TRD covers the rebuild against the new
prediction primitives (`posterior_linpred`, `posterior_epred`,
`posterior_predict`).

**Design target**: brms-parity. The same verbs, the same signatures, the
same return shapes. Deviate only where the state-space context demands it
(the `process_error` toggle, time-marginalisation, hierarchical / grouped
trends, and a per-series option on `conditional_effects`). The master
implementations carried complexity that the new primitives now handle for
free — this is a rebuild, not a port.

**In scope**:
- `log_lik.mvgam` (highest priority; unblocks everything else)
- marginaleffects S3 trio: `get_predict.mvgam`, `get_coef.mvgam`,
  `get_vcov.mvgam`, plus `get_data.mvgam` / `find_predictors.mvgam` and a
  minimal `model.frame.mvgam`
- `conditional_effects.mvgam` with a new `series` argument
- The PSIS path inside `pp_check.mvgam`
- `waic.mvgam` (net-new; brms-parity wrapper)
- Patching `loo.mvgam`, `loo_compare.mvgam`, `lfo_cv.mvgam` to use the
  rebuilt `log_lik`

**Out of scope** (waiting on the C++ trend extrapolator):
`forecast.mvgam`, `score.mvgam_forecast`, hindcast extrapolation, anything
needing forward-in-time prediction.

**Audit source**: `tasks/dev-tasks-downstream-consumers.md`.

---

## 2. User Journey

The whole rebuild succeeds when these snippets all work without
configuration on any fitted mvgam:

```r
# LOO / WAIC — currently silently broken on the branch
loo(fit); waic(fit); loo_compare(loo_a, loo_b)

# marginaleffects ecosystem
predictions(fit); avg_slopes(fit, variables = "x")
plot_predictions(fit, condition = "x")

# Conditional effects (brms-parity + per-series extension)
conditional_effects(fit)
conditional_effects(fit, series = "all")
conditional_effects(fit, series = 1)

# Posterior-predictive checks with PSIS weights
pp_check(fit, type = "loo_pit_overlay")
```

---

## 3. Function Specifications

Each spec gives the signature, the brms-parity behaviour, and the
state-space deviation. Implementation details (line-by-line code, exact
file layout, per-family branch lists) are left to the implementer.

### 3.1 `log_lik.mvgam`

**Generic**: `loo::log_lik` (also re-exported by brms). Master shipped
`logLik.mvgam` on `stats::logLik`; the rebuild moves to brms-parity. The
old `logLik` generic stays as a deprecated wrapper.

**Signature** (matches `brms::log_lik.brmsfit`, with one addition):
```r
log_lik(object, newdata = NULL, re_formula = NULL, resp = NULL,
        ndraws = NULL, draw_ids = NULL, process_error = TRUE, ...)
```

**Returns**: numeric matrix `[ndraws × nobs]`. Row `s` is one posterior
draw; column `n` is one observation. With `process_error = TRUE` the
trend realisation is sampled per draw and embedded into the linear
predictor before evaluating the family log-density.

**State-space deviation**: `process_error`, default `TRUE`. Matches brms's
implicit behaviour for a model with stochastic dynamics — `loo`, `waic`
and PSIS-PIT all need draws that propagate latent-state uncertainty.

**Internal path**: `posterior_linpred(process_error = ...) → family
log-density helper → matrix`. No `mgcv_model` access.

### 3.2 marginaleffects S3 trio + supporting methods

**`get_predict.mvgam`** — the main entry point.

**Reference**: `marginaleffects:::get_predict.brmsfit` (about 30 lines).
Mirror its dispatch:
- `type = "response"` → `posterior_epred`
- `type = "link"` → `posterior_linpred`
- `type = "prediction"` → `posterior_predict`
- Build `data.table(group, estimate = colMedian(draws))`; stuff
  `t(draws)` into `attr(out, "posterior_draws")`; handle 3D draws for
  ordinal / categorical responses.

**Signature**:
```r
get_predict(model, newdata = insight::get_data(model),
            type = "response", process_error = FALSE, ...)
```

**State-space deviations**:
- `process_error` default `FALSE`. Marginaleffects averages over
  covariates, not over latent-state realisations the user didn't
  condition on — bleeding trend uncertainty into a covariate slope is
  the wrong default.
- Adds two mvgam-only `type` values for nmix: `"latent_N"` and
  `"detection"`. Handle with a switch inside `get_predict`; do not push
  nmix-specific dispatch into the primitives.

**`get_coef.mvgam`**: 4-line wrapper returning posterior medians via
`as_draws_matrix` + median collapse. Interface satisfaction only —
marginaleffects ignores it for Bayesian models once
`posterior_draws` is on the `get_predict` output.

**`get_vcov.mvgam`**: returns `NULL`, matching `get_vcov.brmsfit`.
marginaleffects falls back to the draws attribute. See §4.2.

**`get_data.mvgam` and `find_predictors.mvgam`**: depend on a minimal
`model.frame.mvgam` rebuilt as part of this work. See §4.5.

### 3.3 `conditional_effects.mvgam`

**Reference**: `brms:::conditional_effects.brmsfit`. Same signature, two
additions:

```r
conditional_effects(x, ..., series = NULL, process_error = FALSE)
```

- `series = NULL` (default) matches brms — marginal over series.
- `series = "all"` facets by series.
- `series = <int>` or `<character>` plots one series.

**Internal path**: rides on `marginaleffects::plot_predictions(x,
condition = ..., draw = TRUE, ...)`, looping over the term-label list
with `do.call`. Returns `mvgam_conditional_effects` (a named list of
ggplots); existing `plot` / `print` methods for that class on master
are dependency-free and port unchanged.

**Trend-side term auto-enumeration**: deferred. See §4.5.

### 3.4 PSIS path in `pp_check.mvgam`

The stub at `R/ppc.mvgam.R` sets `psis_object` and `lw` to `NULL` for the
LOO-PIT bayesplot types. Replace with `loo::psis(-log_lik(object))` and
`weights(psis_object)`. Inherits the dispatch glue that already exists in
`pp_check.mvgam`. ~10 lines.

### 3.5 `waic.mvgam`

**Reference**: `brms::waic.brmsfit`. Same signature; body is
`loo::waic(log_lik(x, ...))`. Net-new — never existed on master. Adds
brms-parity at no maintenance cost beyond the dispatch.

---

## 4. Architectural Decisions

These resolve the open questions from the audit.

**4.1 Ship `waic.mvgam`** — yes. brms-parity at no cost. Docs still
prefer LOO for posterior model comparison.

**4.2 No custom `get_vcov`** — return `NULL`. marginaleffects detects
the `posterior_draws` attribute and computes variances from the draws,
ignoring `get_vcov` entirely for Bayesian models. Implementing it would
solve a non-problem.

**4.3 Ship the `series` argument on `conditional_effects` now** — not
later. Default behaviour is brms-parity (marginal over series); the
state-space extension is opt-in. Shipping with the rebuild avoids a
churn-y follow-up commit.

**4.4 `log_lik` returns brms-shape; `process_error` defaults to TRUE**
— one `[ndraws × nobs]` matrix, no separate trend-draw axis. The "paired
draws" question collapses when the trend realisation is sampled inside
`posterior_linpred(process_error = TRUE)`.

**4.5 Rebuild `model.frame.mvgam`; defer `formula.mvgam(trend_effects =
TRUE)`** — `model.frame.mvgam` is needed so `marginaleffects::datagrid`
sees trend covariates in the default grid. The trend-side
auto-enumeration in `conditional_effects` is lower value; users can pass
trend effects explicitly via `effects =`. Add the formula method only
if user demand emerges.

**4.6 Do not put `type=` back on `predict.mvgam`** — keep it as a thin
brms-parity wrapper. `get_predict.mvgam` switches on `type` itself,
matching the marginaleffects pattern. Adding `type` to the primitive
defeats having three separate primitives.

**4.7 N-mixture types stay consumer-side** — `"latent_N"` and
`"detection"` are switched inside `get_predict.mvgam`. The primitives
stay family-agnostic.

---

## 5. Rebuild Order

1. **`log_lik.mvgam`** (highest priority). Unblocks `loo`, `loo_compare`,
   `lfo_cv`, PSIS in `pp_check`, and `waic`. Patch `loo.mvgam`,
   `loo_compare.mvgam`, `lfo_cv.mvgam`, and the PSIS stub at the same
   time — they are one-line fixes once `log_lik` exists. Ship `waic.mvgam`
   in the same task (5-line wrapper).
2. **marginaleffects S3 trio** + `model.frame.mvgam` + `get_data` /
   `find_predictors`. Self-contained after Phase 1.
3. **`conditional_effects.mvgam`** with the `series` argument. Depends on
   the marginaleffects trio.

Each phase ships as one commit, with code-reviewer agent before the
commit and brms-concordance tests passing for the touched method.

---

## 6. Test Strategy

**brms-concordance in `tests/local/`**: re-use the 17 fixtures from the
prediction TRD. For each:
- `log_lik(mvgam_fit)` matches `log_lik(brms_fit)` on per-observation
  median to a threshold consistent with the existing concordance tests
  (tighter for non-state-space families, looser where the state-space
  AR vs brms residual-AR divergence applies — see prediction TRD §7.6).
- `loo(mvgam_fit)$estimates` matches `loo(brms_fit)$estimates` within
  the brms-reported se_diff.
- `marginaleffects::avg_slopes` matches across packages within MC noise.
- `waic` matches `loo::waic(log_lik(brms_fit))`.

**testthat unit tests**: signature compatibility (every brms-style arg
accepted; unsupported args fail-fast via the §7.9 error format), shape
(matrix / data.table with `posterior_draws` attr / `mvgam_conditional_effects`
list / `loo::waic` class), `process_error` semantics (deterministic when
FALSE under fixed seed, varies when TRUE).

**Smoke**: each common marginaleffects entry point (`predictions`,
`comparisons`, `slopes`, the `avg_*` variants, the `plot_*` variants,
`inferences`) runs on at least one fixture without error.

CI runs the testthat suite; the brms-concordance suite is manual.

---

## 7. Success Criteria

- `loo(fit)`, `loo_compare(...)`, `waic(fit)`, `lfo_cv(fit)` all run
  without falling through to `stats::logLik.default`.
- `pp_check(fit, type = "loo_pit_overlay")` renders.
- `marginaleffects::predictions(fit)` and `avg_slopes(fit, variables =
  "<cov>")` return the brms-shape data.table with `posterior_draws`
  populated.
- `conditional_effects(fit)` returns a named list of ggplots;
  `conditional_effects(fit, series = "all")` facets by series.
- Full `devtools::test()` clean (matching the current 2683 PASS / 0 FAIL
  / 1 pre-existing SKIP baseline, plus the new unit tests).
- No `mgcv_model` access in any new consumer code — everything routes
  through the primitives.

---

## 8. Out of Scope / Deferred

- Forecast + scoring (waiting on C++ trend extrapolator).
- `pointwise = TRUE` streaming mode on `log_lik` (brms supports it; v1
  errors). Add if needed for very large fits.
- Trend-side term auto-enumeration in `conditional_effects` (needs
  `formula.mvgam(trend_effects = TRUE)`). Users can pass `effects =`
  explicitly in v1.
- nmix log-likelihood on the observation scale rather than latent
  abundance scale — expensive marginalisation, defer.
- Full multivariate per-response `resp` handling on `waic` — pass-through
  to `log_lik` in v1; expand if a user asks.


## 9. Bayesplot-ecosystem methods

The diagnostic batch (task #74) shipped: `coef`, `fixef`, `rhat`,
`neff_ratio`, `nuts_params`, `log_posterior`, `bayes_R2`,
`prior_summary`, `ndraws`, `nchains`, `niterations`, `nvariables`,
`posterior_summary`, `getCall`.

The Tier-3 batch (this branch) shipped: `vcov`, `LOO`, `WAIC`,
`loo_R2`, `loo_predict`, `loo_model_weights`, `add_criterion`. Two
LOO-extras error informatively because their underlying machinery
requires infrastructure mvgam has not yet built:

- `loo_subsample.mvgam`: requires `log_lik(pointwise = TRUE)`
  (function-based interface) which mvgam defers per §8.
- `loo_moment_match.mvgam`: requires `unconstrain_pars` / `log_prob`
  on the stanfit, plus a refit path.

The Tier-3 batch also closed a beta-aliasing gap: positional Stan
parameter names (`b[k]`, `b_trend[k]`, `b_<resp>[k]`,
`b_<dpar>[k]`) are now projected to brms-native aliases
(`b_<term>`, `b_<term>_trend`, `b_<resp>_<term>`,
`b_<dpar>_<term>`) at the user-facing layer
(`extract_mvgam_draws`, `variables.mvgam`). The internal prediction
pipeline still subsets the raw stanfit by positional name.

The Tier-4 batch (this branch) shipped `update.mvgam`: brms-parity
refit method with `formula.`, `newdata`, and `recompile` args.
Recompilation is auto-detected by byte-for-byte stancode
comparison; `recompile = FALSE` errors informatively if the new
model would emit different Stan code. Trend-formula round-trip is
backed by a new `$trend_call` slot populated in
`create_mvgam_from_combined_fit()` so the user-supplied trend
constructor (e.g. `~ AR(p = 1)`) is preserved verbatim.

One brms-parity gap remains:

- **`ranef.mvgam`** and **`VarCorr.mvgam`**: brms shapes are per-group
  named lists of 3D arrays / variance matrices. mvgam exposes
  random-effect coefficients as positional `r_<id>_<coef>[<level>]`,
  `sd_<id>[<coef>]`, `cor_<id>[<i>,<j>]` and the
  group / coefficient mapping brms applies via its
  `rename_pars` cascade has not been re-implemented for these
  blocks. Users can still access raw draws via
  `as.matrix(fit, variable = "^(sd|cor|r)_", regex = TRUE)`. Land
  alongside future random-effect aliasing work.

The following brms `.brmsfit` methods remain out of scope. Each is
its own decision thread, not a gap from the rebuild:

| Method | Why deferred |
|---|---|
| `autocor.mvgam` | mvgam autocorrelation lives in the trend submodel, not residual: semantic shift, not a method gap |
| `bridge_sampler.mvgam` / `bayes_factor.mvgam` / `post_prob.mvgam` | Requires `bridgesampling` integration and Stan code surgery |
| `hypothesis.mvgam` | Reachable via `marginaleffects::hypotheses()` |
| `kfold.mvgam` | Heavy refit machinery; revisit with the forecast extrapolator |
| `expose_functions.mvgam` / `launch_shinystan.mvgam` / `getRefmodel.mvgam` | Niche tooling integrations |
| `posterior_smooths.mvgam` / `conditional_smooths.mvgam` | Smooth-term-specific predictions; reachable via marginaleffects with effect labels |
| `update.jsdgam` | `jsdgam()` does not exist on this branch; ship alongside the constructor when added |
| Diagnostic-flag methods (`control_params`, `inits`, `default_prior`) | Internal-facing in brms; defer until a user need surfaces |
