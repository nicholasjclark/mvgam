# Forecasting support roadmap

## Context

`feature/brms-integration` already ships the trend-extrapolation
kernels and an R-side dispatcher built explicitly for
`forecast.mvgam`. `R/trend_propagation.R::propagate_trend()`
collapses master's per-trend helpers (`forecast_trend`,
`sim_corcar1`, `sim_var`, …) into one call that takes a
`last_state = list(trends, errors, linpreds, time)`. The C++
kernels (`trend_arma_recursC`, `car1_recursC`) use the brms-
centred convention `(trend − linpred)`, so they plug straight
into the per-(time, series) `mu_trend` slots this branch emits.

What is missing is the user-facing surface: no `forecast.mvgam`,
no `hindcast.mvgam`, no `score.mvgam_forecast`, no
`log_lik.mvgam`. `lfo_cv.mvgam` exists but is a non-functional
stub still wired to master-era `index..time..index` plumbing.
The `mvgam_forecast` S3 class, `summary.mvgam_forecast`,
`plot.mvgam_forecast`, and `ensemble.mvgam_forecast` are in
place. Most of the work is wrapping `propagate_trend()` in a
draw loop and re-assembling the `mvgam_forecast` payload using
the branch's `posterior_linpred.mvgam` / `posterior_epred.mvgam`
/ `posterior_predict.mvgam` / `extract_trend_latent_states`
(in `R/predictions.R`).

## Port-status inventory (cross-checked against git log + user)

**Ported and usable:**
- `posterior_linpred.mvgam`, `posterior_epred.mvgam`,
  `posterior_predict.mvgam`, `predict.mvgam`, `fitted.mvgam`,
  `predictive_interval.mvgam`, `predictive_error.mvgam`.
- `log_lik.mvgam`, `loo.mvgam`, `loo_compare.mvgam`,
  `loo_predict / loo_epred / loo_linpred /
  loo_predictive_interval / loo_subsample / loo_moment_match /
  loo_model_weights / add_criterion`.
- `pp_average.mvgam`, `posterior_average.mvgam`.
- `conditional_effects.mvgam` (obs-only fits; trips on
  state-space — the trend constructor leaks into
  detect_conditional_effects as a phantom predictor).
- `conditional_smooths.mvgam`, `posterior_smooths.mvgam`.
- `as_draws_*.mvgam` family.
- `coef / fixef / ranef / vcov / VarCorr / as.mcmc.mvgam`.
- `summary.mvgam`, `update.mvgam`, `get_mvgam_priors`,
  `plot_mvgam_series` (EDA only, no fit needed),
  `pp_check.mvgam`.
- `sim_mvgam`, `propagate_trend`, the two C++ kernels.

**Not ported (file untouched on `feature/brms-integration`
since branching from master):**
- `forecast.mvgam`, `hindcast.mvgam`, `score.mvgam_forecast`
  (file deleted on this branch).
- `ensemble.R` / `ensemble.mvgam_forecast`.
- `irf.mvgam`, `fevd.mvgam`.
- `pairs.mvgam`.
- `residuals.mvgam`.
- `lv_correlations`, `stability`,
  `series_to_mvgam`, `ordinate.jsdgam`, `tidier_methods`
  (broom-style tidy / augment / glance).

**Queued for removal (not ports — feature retired):**
- The full `plot_mvgam_*` helper family:
  `plot_mvgam_trend`, `plot_mvgam_smooth`,
  `plot_mvgam_resids`, `plot_mvgam_factors`,
  `plot_mvgam_fc` (which holds `plot.mvgam_forecast`),
  `plot_mvgam_uncertainty`, `plot_mvgam_pterms`,
  `plot_mvgam_randomeffects`. **Keep** `plot_mvgam_series`
  (EDA helper, still usable without a fit).
- `dynamic` (the `dynamic(covariate, rho=...)` GP-basis
  time-varying-effect wrapper). Users get the same result
  via `s(time, by = covariate, bs = "gp")` directly, so the
  helper has no remaining job. `time_varying_effects.Rmd`
  loses its main API and is in line for either deletion
  or a full rewrite around the bare smooth.

**Deferred decision (revisit later):**
- `monotonic` smooths — port status undetermined; user
  flagged for later thought rather than retirement.
- `mvgam_fevd-class`, `mvgam_forecast-class`,
  `mvgam_irf-class`, `mvgam_residcor-class`,
  `mvgam-class`, `mvgam-package` (class definitions present
  but not validated on this branch).

**Touched but only partially ported:**
- `lfo_cv.mvgam.R` — last commit added brms-parity
  signatures only; body still relies on the unported
  `forecast / score / log_lik` surface in its master form.
- `mcmc_plot.mvgam.R` — "Restore downstream diagnostic
  surface" partial; not trusted end-to-end.
- `add_residuals`, `residual_cor` — touched on this
  branch but port status uncertain; treat as not-ported
  until verified.

## Current state (feature/brms-integration)

- `R/mvgam_forecast-class.R` (181 LOC) — working. Class docs +
  `summary.mvgam_forecast`. Field contract matches master.
- `R/plot_mvgam_fc.R` (988 LOC) — **out of date.** Treat as
  part of the broader plotting overhaul (`plot.mvgam`,
  `plot_mvgam_*` family). Not relied on by Phase F.
- `R/ensemble.R` — **not ported.** File present but not
  trusted; must be reworked alongside `forecast.mvgam` (or
  in a follow-up batch that owns ensembling).
- `R/lfo_cv.mvgam.R` (465 LOC) — **not ported.** Calls
  `forecast()`, `score()`, `log_lik()`, uses
  `index..time..index` and master `validate_series_time(…,
  trend_model = …)`. Treat as a fresh build, not a rewire.
- `R/pp_average.mvgam.R` (318 LOC) — working for the
  `posterior_*` trio; not yet `mvgam_forecast`-aware.
- `R/trend_propagation.R` (451 LOC) — working dispatcher
  covering RW / AR / ARMA / VAR / VARMA / CAR / ZMVN / None.
- `R/sample_innovations.R` — `get_observation_structure()` +
  covariance-pattern helpers; reusable by the forecast loop.
- `R/predictions.R::extract_trend_latent_states`
  (lines 2189–2282) — pulls `trend[t, s]` posterior draws;
  for unseen `t` it falls back to the per-series posterior
  mean and prints a one-time note pointing at the future
  `forecast()`.
- `R/posterior_linpred.R` / `R/posterior_epred.R` /
  `R/posterior_predict.R` — working brms-style methods.
- `R/forecast.mvgam.R`, `R/hindcast.mvgam.R`,
  `R/score.mvgam_forecast.R`, `R/log_lik.R` — **absent**.

## Master implementation we'd port from

- `R/forecast.mvgam.R` (1299 LOC). Surface:
  `forecast.mvgam(object, newdata, type = c("response",
  "link", "expected", "trend", "detection", "latent_N"), …)
  → mvgam_forecast`. Two arms: (a) extract pre-baked
  forecasts when `object$test_data` was supplied at fit time,
  (b) compute them via internal `forecast_draws()` (lines
  813–1299, ~480 LOC of building obs/trend `Xp`, calling
  `extract_trend_pars`, per-draw `forecast_trend()`, then
  `mvgam_predict()`). **`propagate_trend()` replaces the
  per-draw `forecast_trend()` body in master `R/trends.R`
  lines 1346–1806 (~460 LOC of per-trend branches) plus
  `extract_general_trend_pars` / `extract_series_trend_pars`
  on master `R/trends.R` lines 1240–1346.** Arm (a) is not
  ported (see Open question 4).
- `R/hindcast.mvgam.R` (246 LOC).
  `hindcast.mvgam(object, type = …) → mvgam_forecast`,
  extracts in-sample `mus`/`ypred`/`trend`/`detprob` via
  `mcmc_chains()`. On this branch the body becomes a thin
  call to `extract_trend_latent_states` +
  `posterior_predict.mvgam`.
- `R/mvgam_forecast-class.R` (181 LOC) — already on branch.
- `R/score.mvgam_forecast.R` (385 LOC).
  `score(object, score = c("crps", "drps", "brier", "elpd",
  "sis", "energy", "variogram"), log, weights,
  interval_width = 0.9, n_cores = 1, …)`. The `elpd` branch
  calls `logLik.mvgam(object, linpreds = …, family_pars =
  …)` which does not exist on this branch — needs a
  `log_lik.mvgam` replacement. Numeric scoring kernels live
  in master `R/evaluate_mvgams.R` lines 892–1020 (~130 LOC)
  and port directly.
- `R/lfo_cv.mvgam.R` (447 LOC). Functionally compatible
  once `forecast`, `score`, and `log_lik` exist on this
  branch; the branch already carries the body, so the port
  is just re-wiring `update()` + `log_lik()` + dropping
  `index..time..index` for `get_time_for_grouping()`.

## Architecture-doc directives

- **Decision 10 ("Ultra-Efficient Forecasting System")** —
  fitted object already carries `forecast_dispatch =
  list(function_name, required_params, time_info)` via
  `generate_forecast_metadata()` (`R/trend_system.R` line
  820+). Per-type `forecast_<x>_rcpp` functions are no
  longer needed — `propagate_trend()` is the single
  dispatch surface. Keep `required_params` and `time_info`
  for parameter filtering and horizon validation; replace
  the `do.call(function_name, …)` step with a
  `propagate_trend()` call.
- **Decision 20 ("Dual-Context Function Architecture")** —
  `extract_trend_data()` and `get_observation_structure()`
  already accept `(mvgam_object, newdata)` and consult
  `mvgam_fit$trend_metadata$variables` (stored at fit time
  in `mvgam_core.R` line 208). Forecasting reuses these
  accessors and so inherits multivariate, hierarchical,
  and CAR irregular-time handling without forking
  validation logic.
- **Decision 16 ("Trend Model Distribution Constraints")** —
  trends are always Gaussian, so the only stochastic step
  to sample is innovations. Already what `propagate_trend()`
  does; master's `b_uncertainty` / `trend_uncertainty` /
  `obs_uncertainty` toggles become simple "draw 1 vs draw
  i" switches in the loop.
- **`stan-data-flow-pipeline.md` Observation-to-Trend
  Mapping** — `obs_trend_time[N]` / `obs_trend_series[N]`
  in `standata` provide the in-sample (t, s) lookup. The
  forecast loop reuses these for the hindcast slice and
  extends them via `get_observation_structure(object,
  newdata)` for the out-of-sample horizon.

## Gap analysis

### To build (new files)

- `R/forecast.mvgam.R` (~350–450 LOC, down from 1299).
  `forecast.mvgam(object, newdata = NULL, type = c(…),
  ndraws = NULL, b_uncertainty = TRUE, trend_uncertainty
  = TRUE, obs_uncertainty = TRUE, …) → mvgam_forecast`.
  Re-exports `generics::forecast`. Internal
  `forecast_draws_mvgam()` (~150 LOC) replaces master's
  `forecast_draws()`: per-draw extract trend pars via
  `forecast_dispatch$required_params`, build `last_state`
  from the final `max_lag` columns of
  `extract_trend_latent_states` at training times, call
  `propagate_trend()`, then map to response scale via
  per-draw subsetting of `posterior_*`. Internal helper
  populates `last_state$linpreds` from
  `posterior_linpred.mvgam(object, newdata, process_error
  = FALSE)` with the trend contribution zeroed (matching
  the centred Stan convention).
- `R/hindcast.mvgam.R` (~120 LOC).
  `hindcast.mvgam(object, type = c(…), ndraws = NULL, …)
  → mvgam_forecast`. Body: trend draws via
  `extract_trend_latent_states`, scale via
  `posterior_predict.mvgam` / `posterior_epred.mvgam` on
  training data, empty `forecasts`/`test_*`.
- `R/score.mvgam_forecast.R` (~400 LOC). Port master
  dispatch verbatim; rewire `elpd` to use `log_lik.mvgam`.
- `R/scoring_kernels.R` (~150 LOC). Port
  `drps_mcmc_object`, `crps_mcmc_object`, `sis_mcmc_object`,
  `brier_mcmc_object`, `energy_mcmc_object`,
  `variogram_mcmc_object` from master
  `R/evaluate_mvgams.R` lines 892–1020.
- `R/log_lik.R` (~60 LOC).
  `log_lik.mvgam(object, newdata = NULL, ndraws = NULL,
  …) → [ndraws, nobs] matrix`. Built on
  `posterior_predict.mvgam`. Required by `elpd` scoring
  and by `lfo_cv`.

### Already covered by `propagate_trend()`

- All per-trend-type forecast branches in master's
  `forecast_trend()` (`R/trends.R` lines 1346–1806),
  except GP and PW (see Open Q3).
- `extract_general_trend_pars` /
  `extract_series_trend_pars` — replaced by direct
  `posterior::subset_draws()` plus the flat `params`
  list `propagate_trend()` takes.
- `prep_varma_params` and `sim_corcar1` — subsumed by
  the C++ kernels.

### Downstream consumers to touch

- `R/lfo_cv.mvgam.R`: swap `validate_series_time(…,
  trend_model = …)` for `validate_time_series_for_trends()`;
  swap `log_lik(fit_past)` for new `log_lik.mvgam`; drop
  `index..time..index` for `get_time_for_grouping()`. ~80
  LOC of edits.
- `R/predictions.R::extract_trend_latent_states`
  (lines 2230–2260): replace the per-series-mean fallback
  for unseen times with a `forecast.mvgam` call and remove
  the one-time `inform()`.
- `R/pp_average.mvgam.R`: working; no edits if the
  `mvgam_forecast` slot contract is preserved.
- `R/ensemble.R`: not ported. Full rebuild required;
  signature stays `ensemble(...) -> mvgam_forecast` so
  downstream consumers don't change.
- `R/lfo_cv.mvgam.R`: not ported. Full rebuild on top of
  the new `forecast.mvgam`, `score.mvgam_forecast`,
  `log_lik.mvgam`. Not just a rewire of the existing body.
- `R/plot_mvgam_fc.R`: out of scope for Phase F. Handled by
  the separate plotting overhaul that also covers
  `plot.mvgam` and the `plot_mvgam_*` family.

### Open design questions

1. **`newdata` time alignment.** Rule we propose:
   forecast horizon = newdata times not in training;
   obs-side linpreds for those times come from
   `posterior_linpred.mvgam(object, newdata,
   process_error = FALSE)` with the trend contribution
   stripped. Alternative: require `newdata` to contain
   ONLY future times.
2. **CAR1 time gaps for `newdata`.** Master's
   `add_corcar()` is gone. Need a small helper that, given
   training `time` and `newdata`, returns per-step gaps
   `c(diff(c(last_train_time, newdata_times)))` per
   series. ~25 LOC.
3. **GP and PW forecasting.** Neither covered by
   `propagate_trend()`. Recommend deferring; raise a
   targeted error in `forecast.mvgam` pointing at the
   deferred-feature note.
4. **Pre-baked forecasts (master arm a).** This branch
   has no `object$test_data` slot. Recommend not
   resurrecting it; every `forecast()` call recomputes
   via `propagate_trend()`. Saves ~340 LOC of port.
5. **`nmix` / `latent_N` / `detection` types.** No
   n-mixture observation family on this branch. Validate
   against `object$family` and error cleanly.

## Pre-build cleanup (independent of forecasting)

Before (or alongside) the forecasting work, retire surfaces
that are not coming back:

- Delete `R/plot_mvgam_trend.R`, `R/plot_mvgam_smooth.R`,
  `R/plot_mvgam_resids.R`, `R/plot_mvgam_factors.R`,
  `R/plot_mvgam_fc.R`, `R/plot_mvgam_uncertainty.R`,
  `R/plot_mvgam_pterms.R`, `R/plot_mvgam_randomeffects.R`.
  Keep `R/plot_mvgam_series.R`. Update `NAMESPACE` and any
  roxygen `@seealso` cross-references that still mention
  the deleted helpers.
- Delete `R/dynamic.R` and the `dynamic()` wrapper export.
  Audit roxygen examples and vignettes for `dynamic()`
  references; replace with `s(time, by = covar, bs = "gp")`
  or remove the section.
- Decide later: `monotonic` smooths (kept for now).

`R/plot.mvgam.R`, `R/mcmc_plot.mvgam.R`, `R/pairs.mvgam.R`
stay in place (stubbed where the body referenced deleted
helpers); they belong to the future plot-overhaul batch and
Phase F should not depend on them.

**Direction for the future plot batch.** The user wants a
ggplot-based latent-trend plot (a `plot_trend` or
`autoplot.mvgam` style helper) since that's the one thing
that doesn't have an obvious brms-native equivalent.
`pp_check.mvgam` and `conditional_effects.mvgam` cover the
posterior-predictive and smooth-effect plotting jobs, so
the plot batch can focus on (a) trend rendering, (b) the
`plot.mvgam_forecast` ggplot replacement, (c) any wrapper
around bayesplot we want to surface natively.

## Execution sequence (phases)

- **F1 — `log_lik.mvgam` + scoring kernels.** Dep: existing
  `posterior_predict.mvgam`. Est. ~1 day. Checkpoint:
  drps/crps/brier match master on a fixed reference draw
  matrix.
- **F2 — `hindcast.mvgam`.** Dep: branch only. Est. ~1 day.
  Checkpoint: hindcast plot on poisson AR(1) matches
  `posterior_predict()` draw-wise.
- **F3 — `forecast.mvgam` (univariate AR / RW / ZMVN).**
  Dep: F2. Est. ~2–3 days. Checkpoint: sim-recover test
  against `sim_mvgam` ground truth; 90% PI coverage ≈
  nominal.
- **F4 — VAR / CAR / hierarchical / multivariate.** Dep:
  F3. Est. ~2 days. Checkpoint: per-trend-type
  recoverability tests using `tests/testthat` fixtures.
- **F5 — `score.mvgam_forecast` + `lfo_cv.mvgam` rewire.**
  Dep: F1, F3. Est. ~1–2 days. Checkpoint: `?lfo_cv`
  example runs end-to-end.
- **F6 — `extract_trend_latent_states` unseen-time
  hookup.** Dep: F3. Est. ~0.5 day. Checkpoint:
  `posterior_predict.mvgam(fit, newdata = future_data)`
  returns extrapolated states, not flat means.

## Out of scope (deferred)

- GP-trend and PW-trend forecasting.
- `nmix` / `latent_N` / `detection` types.
- Pre-baked-forecast extraction arm (master
  `forecast.mvgam` lines 461–800).
- Parallel `forecast_draws` via `cores > 1`.
- `mvgam_forecast`-aware `pp_average` extension.
- Trend-side dpar forecasting (heteroscedastic
  state-space variance — already gated by the
  architecture decisions on the fit path).
