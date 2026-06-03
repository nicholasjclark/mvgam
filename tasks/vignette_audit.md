# Vignette audit — feel + structure + references

Pre-overhaul characterisation of the 7 vignettes at `vignettes/*.Rmd`.
Observation only; overhaul recommendations come in the next step.

---

## House voice (cross-cutting)

**Tone.** First-person plural throughout ("we will fit a model"), conversational but precise. Reads like a tutorial-driven walkthrough, not a reference manual. Sentences are direct and declarative; rhetorical questions appear at section transitions ("So what happens when X?").

**Pedagogy.** Consistent arc: motivation (ecology context) → simulate or load real data → naive fit → inspect residuals/forecasts → add the feature → recover the truth → contrast scores. Strongly favours "show, then explain" over "explain, then show". Each new feature is introduced inside a fit call and unpacked afterwards with `summary()`, `conditional_effects()`, or `mcmc_plot()`.

**Density.** Heavy LaTeX for model equations (state-space stack, AR(1) recursion, VAR(1) matrix). Code chunks alternate with 1-3 paragraphs of prose. Comments inside code chunks carry pedagogical weight (e.g. `# need to use 'trend' rather than series here`).

**Formatting.** Subsections via `##` / `###`. Bold for emphasis on requirements (e.g. **tidy format**), italic for variable / parameter names. No section numbering. No call-out boxes. Code chunks fenced as ```{r chunk_name, opts}```; many fits use `include = FALSE, results='hide'` for the actual run and a visible `eval=FALSE` block for documentation.

**House quirks.**
- The author writes a hidden / visible fit-call pair: one runs (hidden), one displays (eval=FALSE) — readers see the call without waiting.
- Heavy use of `silent = 2` to suppress sampling noise.
- `summary(mod, include_betas = FALSE)` is the default summary in every vignette with smooths — author hates dense spline tables.
- `noncentred = TRUE` appears whenever there are latent dynamics, framed as an efficiency win.
- Final section in every vignette is **Further reading** — a flat list of papers as hyperlinks. No integration into the narrative; pure pointer list.

---

## Per-vignette summary

### `data_in_mvgam.Rmd` (400 lines)

**Showcases:** the *shape* requirements for input data (long format, factor `series`, integer `time`, NAs in y, no NAs in covariates).

**Narrative.**
1. Why long format and how series-factor levels matter.
2. Validate series with `levels()` / `droplevels()`.
3. Time-variable requirements (mostly regular intervals; CAR for irregular).
4. `get_mvgam_priors()` as a validation pass.
5. Covariate completeness (no NAs in predictors).
6. `plot_mvgam_series()` for EDA.
7. End-to-end example (birdsong — recently swapped in for the removed neon ticks).

**Datasets:** `sim_mvgam()`, **`birdsong`** (newly added, replacing `all_neon_tick_data`).

**Style markers:** the most cookbook-style of the 7. Heavy use of bold for "must have this column". Less mathematical, more diagnostic-pattern oriented.

**Stale API footprint:** clean (I just rewrote the NEON section).

**Worth keeping:** lines 42-48 (long-format motivation), 115-148 (time-variable requirements), 241-283 (covariate NA diagnostic patterns).

**Pruning candidates:** lines 150-151 (cursory CAR mention); the prose still says `trend_model = 'None'` in describing the default — needs correction.

---

### `mvgam_overview.Rmd` (610 lines)

**Showcases:** the package's full feature surface — observation families, every supported trend model, regression formulae, Portal rodent walkthrough.

**Narrative.**
1. State-space motivation + math (lines 41-48).
2. Observation families reference table (lines 52-68).
3. Trend model catalogue with equations: ZMVN → RW → AR → VAR → GP → PW → CAR (lines 70-167).
4. Regression formulae and mgcv integration (lines 168-256).
5. Portal data walkthrough: load → GLM baseline → spline smooths → overfitting → AR(1) → forecast eval.
6. Predictor inspection (`conditional_effects`, `mcmc_plot`).
7. Forecast scoring + LOO comparison.

**Datasets:** `sim_mvgam()`, **`portal_data`**.

**Style markers:** the most reference-manual-flavoured. Long sections of equations + parameter prose. Two fit calls are the worked example (`model4` with `trend_model = AR()` — broken on this branch).

**Stale API footprint:**
- Lines 535, 546: `trend_model = AR()` legacy arg in `model4` — silently dropped; prose claims AR is fit but it isn't.
- Lines 70-167: ~10 prose mentions of `trend_model = 'X'` / `trend_model = X()` describing the API as a separate argument — wrong on this branch.
- Lines 305, 311, 325, 342: `plot.mvgam()` references in prose (`plot(mod, type='re' / 'forecast' / 'residuals')`) — no S3 method on this branch.
- Line 515: `plot_mvgam_smooth(...)` — not ported.

**Worth keeping:** lines 41-48 (state-space intro), 168-172 (mgcv integration paragraph), 240-256 (partial pooling motivation for random effects), 456-487 (spline basis + penalty explanation), 506-527 (why splines fail at forecasting → AR(1) is the fix — strong rhetorical payoff).

**Pruning candidates:** lines 52-68 (families table is dense and not used downstream), 298-301 (long `stancode` dump with no commentary), much of 70-167 (the trend-model section reads as legacy-API reference).

---

### `trend_formulas.Rmd` (546 lines)

**Showcases:** state-space modelling with a separate `trend_formula`, VAR(1) dynamics, IRF/FEVD, scoring rules.

**Narrative.**
1. State-space conceptual diagram + math.
2. Lake Washington plankton data prep (z-score, filter, time index).
3. Observation-only model with tensor `te(temp, month)` smooths.
4. Inspect smooths + residuals (model is incomplete).
5. Add VAR(1) trend with shared seasonality + per-series dynamics.
6. Set process / observation error priors.
7. Fit uncorrelated VAR.
8. Interpret A matrix (lagged effects) and Σ (process error).
9. Fit correlated-process-error VAR.
10. IRF + FEVD.
11. Forecast scoring.

**Datasets:** `lakeWAplankton` (from `MARSS`).

**Style markers:** the most technical vignette. Heavy on matrix interpretation and inspection of fitted objects. Strongest "interpret the posterior" content in the suite.

**Stale API footprint:**
- Lines 160, 176: `trend_model = "None"` → on this branch, just omit `trend_formula` entirely.
- Lines 242, 279, 296: `trend_model = VAR()` legacy arg.
- Lines 372, 389: `trend_model = VAR(cor = TRUE)` legacy arg.
- Lines 182, 187, 191: `plot_mvgam_smooth()` calls — not ported.
- Lines 196-213: `plot(mod, type = 'forecast' | 'residuals', series = N)` — no `plot.mvgam` on this branch.
- Line 315: `plot(var_mod, "smooths", trend_effects = TRUE)` — same.

**Worth keeping:** lines 41-47 (state-space motivation), 68-96 (data prep with z-scoring rationale), 217-241 (VAR motivation + maths), 307-347 (A-matrix interpretation), 431-453 (IRF / FEVD intuition), 528-541 (further reading is unusually well integrated here).

**Pruning candidates:** lines 97-145 (residual section has no interpretive payoff), 349-410 (the correlated-error fit is largely mechanical relative to the uncorrelated one).

---

### `shared_states.Rmd` (375 lines)

**Showcases:** `trend_map` — forcing multiple observed series to share a single latent state, in the MARSS Z-matrix style.

**Narrative.**
1. `trend_map` syntax and the Z-matrix analogy.
2. Worked example: 3 series, 2 latent processes, shared seasonal smooth + different AR dynamics. Setup with `run_model = FALSE` to inspect Stan / Z.
3. Fit the model, summarise.
4. Signal-detection example: 3 noisy sensors tracking 1 hidden signal with smooth productivity effect.
5. Recover the hidden signal and overlay simulated truth.

**Datasets:** `sim_mvgam()` for the 3-series example; custom-simulated sensor data (`mgcv::gamSim` + `arima.sim` for the signal-detection example).

**Style markers:** strong narrative payoff in the recovery overlay plot. Explicit MARSS analogies throughout. Pedagogically valuable explanations of the Z matrix.

**Stale API footprint (mostly already fixed by me this session):**
- Was: 6 fit calls with `trend_model = AR()` legacy arg — now folded into `trend_formula`.
- Was: lines 138-140 `plot(full_mod, type = "trend", series = N)` — replaced with prose pointer to `summary()`.
- Was: line 355 `plot(mod, type = "trend")` overlay — rewritten to use `as_draws_df(... "^trend\\[", regex = TRUE)`.

**Worth keeping:** lines 41-56 (trend_map syntax + factor-level requirement), 64-99 (run_model=FALSE workflow — valuable inspection technique), 134-198 (signal-detection narrative), 333-354 (signal-recovery overlay).

**Pruning candidates:** lines 200-234 (sensor obs plots have no interpretation).

---

### `forecast_evaluation.Rmd` (341 lines)

**Showcases:** probabilistic forecasting (`forecast()`, `hindcast()`) and scoring rules (`score(fc, score = "...")`).

**Narrative.**
1. Simulate 3-series Poisson data with GP-driven trend.
2. Fit spline-only baseline (no trend).
3. Fit AR(1) trend version.
4. Generate forecasts (`forecast(mod, newdata = ...)` vs auto-forecast via `newdata = ` at fit time).
5. Score with CRPS, ELPD, energy, variogram rules.
6. Compare the two models quantitatively.

**Datasets:** `sim_mvgam(trend_model = GP(), prop_trend = 0.75, family = poisson())`.

**Style markers:** equations for the scoring rules (CRPS formula), grounded in intuition. Score arithmetic shown explicitly (sums of differences across series) — strongest "look at the numbers" content.

**Stale API footprint:**
- Lines 49, 85, 96, 118, 129, 187, 199: 6 fits with `trend_model = X` legacy arg.
- Line 49: `sim_mvgam(trend_model = GP())` — `sim_mvgam` actually accepts `trend_model` as a real arg (Phase D), but the docs in Phase D excluded `GP()` from the simulator. **This call would error on the current branch.**

**Worth keeping:** lines 217-228 (CRPS + interval coverage explanation), 231-237 (transition to multivariate scoring), 248-327 (model comparison with explicit score differences).

**Pruning candidates:** lines 39-40 (boilerplate intro), 157-177 (hidden trend-model fit code chunk).

---

### `time_varying_effects.Rmd` (358 lines)

**Showcases:** `dynamic()` wrapper for GP-basis time-varying regression coefficients; comparison via LOO and LFO-CV.

**Narrative.**
1. Time-varying effects motivation.
2. Simulate a GP-driven time-varying coefficient (`mvgam:::sim_gp`).
3. Simulate outcome as covariate × time-varying coefficient.
4. Show `dynamic()` wrapper API.
5. Fit with fixed rho, plot recovered smooth vs truth.
6. Estimate rho via `gp()` instead of fixing it.
7. Salmon survival example (Beta regression on proportions, with informative priors).
8. State-space model: AR(1) + dynamic upwelling effect.
9. Compare models via `loo` and `lfo_cv`.

**Datasets:** simulated GP data; **`SalmonSurvCUI`** from `MARSS`.

**Style markers:** strong simulation-first pedagogy; truth is always shown alongside the recovered estimate. Clear comparison payoff via `lfo_cv`.

**Stale API footprint:**
- Lines 212, 224, 249, 263: 4 fits with `trend_model = AR()` legacy arg.
- Lines 118, 170: `plot_mvgam_smooth(mod, smooth = 1, newdata = data)` — not ported.
- Uses `mvgam:::sim_gp` (triple-colon to a non-exported function) — CRAN-policy concern.

**Worth keeping:** lines 41-42 (time-varying motivation), 94-109 (`dynamic()` formula expansion), 176-207 (salmon setup with prior knowledge), 243-270 (Beta + informative priors), 307-342 (LFO-CV comparison interpretation).

**Pruning candidates:** lines 140-174 (mechanical `plot_predictions` output descriptions).

---

### `nmixtures.Rmd` (590 lines) — **DEFER**

> `nmix()` family is not ready on `feature/brms-integration`. This vignette
> stays out of the overhaul; revisit when the N-mixture surface lands.

**Showcases:** N-mixture models (latent abundance × imperfect detection) via marginalisation, using the `nmix()` family + `trend_map`.

**Narrative.**
1. N-mixture motivation: detection vs abundance, marginalising over discrete latent N.
2. Maths: Binomial × Poisson stack.
3. Data shape for replicates (series indicator, `trend_map`).
4. Example 1: 2-species, 6 years, 5 replicates/year, nonlinear trends.
5. Set up `trend_map` to share latent N across replicates.
6. Fit `nmix()` family model.
7. Inspect Stan code (marginalisation trick visible there).
8. Extract latent N posterior and plot vs truth.
9. Example 2: 225-site survey with detection + abundance covariates.
10. Fit with splines on both detection and abundance scales.

**Datasets:** custom simulated 2-species data; **`spAbundance`** example data (225 sites × 3 replicates).

**Style markers:** the longest vignette. Strongest pedagogical commentary on the marginalisation trick. Heavy emphasis on how `trend_map` connects replicates to latent processes.

**Stale API footprint:**
- Line 270: `plot(mod, type = "smooths", trend_effects = TRUE)` — no `plot.mvgam` on this branch.
- No legacy `trend_model = X` arg (good).
- The `trend_formula` calls (lines 207-209, 233-235, 441, 477) have no trend constructor (no AR/RW/CAR) — default ZMVN per the parser. **This needs verification**: did the author intend ZMVN, or is the AR/RW silently missing?

**Worth keeping:** lines 65-82 (N-mixture + marginalisation strategy), 84-164 (Example 1 data setup with explicit truth), 183-195 (trend_map mapping commentary), 288-355 (latent N recovery with truth overlay).

**Pruning candidates:** lines 359-381 (Example 2 data loading is mechanical), 431-502 (variational inference mention is brief and underdeveloped), 504-576 (verbose `conditional_effects` output descriptions).

---

## Cross-cutting observations

**Datasets used.**
| Vignette | Built-in / shipped | External |
|---|---|---|
| data_in_mvgam | `sim_mvgam`, `birdsong` | — |
| mvgam_overview | `sim_mvgam`, `portal_data` | — |
| trend_formulas | — | `lakeWAplankton` (MARSS) |
| shared_states | `sim_mvgam` | custom-simulated signal |
| forecast_evaluation | `sim_mvgam(GP())` (currently broken) | — |
| time_varying_effects | `mvgam:::sim_gp` (`:::` use) | `SalmonSurvCUI` (MARSS) |
| nmixtures | — | `spAbundance` example data |

Three new datasets (`birdsong`, `lake_chemistry`, `coral_surveys`) are now available but only `birdsong` has landed in a vignette. `lake_chemistry` and `coral_surveys` have no vignette home yet.

**Reference style.**
Papers consistently cited as Author + year + journal with hyperlink to DOI / journal page. Recurring citations across vignettes:
- Clark & Wells 2023 *Methods in Ecology and Evolution* (the package methods paper)
- Clark et al. 2025 *PeerJ* (multivariate forecasts)
- Auger-Méthé et al. 2021 *Ecological Monographs* (state-space guide)
- Holmes et al. 2012 *R Journal* (MARSS reference)
- Heaps 2023 *JCGS* (VAR stationarity prior)
- Hannaford et al. 2023 *CSDA* (sparse hierarchical VAR)
- Karunarathna et al. 2024 *Ecological Modelling*
- Royle 2004 *Biometrics* (N-mixture origin)
- Gneiting & Raftery 2007 *JASA* (scoring rules)

Package citations: `brms`, `mgcv`, `MARSS`, `marginaleffects`, `bayesplot`, `posterior`, `loo`, `spAbundance`.

External URLs: NEON, Portal Project, mc-stan.org, [ecogambler blog](https://ecogambler.netlify.app/).

**Recurring code patterns (the "API surface" the vignettes teach).**
| Pattern | Status on this branch |
|---|---|
| `mvgam(formula, trend_formula = ~ X(), family, data)` | canonical ✓ |
| `mvgam(formula, trend_model = X(), ...)` | broken (silently dropped) ✗ |
| `get_mvgam_priors(formula, trend_formula, data, family)` | ✓ |
| `summary(mod, include_betas = FALSE)` | ✓ |
| `stancode(mod)`, `standata(mod)` | ✓ |
| `conditional_effects(mod, type = "link"/"response")` | ✓ for obs-only fits; trips on state-space (see Phase E notes) |
| `marginaleffects::plot_predictions(mod, condition = ...)` | ✓ |
| `mcmc_plot(mod, variable = "X", regex = TRUE, type = "hist")` | ✓ |
| `plot_mvgam_series(data = ..., series = "all")` | ✓ (EDA only, no fit) |
| `forecast(mod, newdata = ...)`, `score(fc, score = "...")` | ✓ |
| `loo_compare()`, `lfo_cv(mod, min_t = ...)` | ✓ |
| `plot.mvgam(mod, type = "X")` | ✗ no S3 method |
| `plot_mvgam_smooth/trend/resids/factors/fc` | ✗ not ported |
| `run_model = FALSE` for setup-only inspection | ✓ |
| `mvgam:::sim_gp` (`:::` to non-exported) | ✗ CRAN policy |

**Voice + structure to preserve in any overhaul.**
- First-person plural; ecology-grounded motivation.
- Hidden-fit / visible-eval=FALSE pair.
- `summary(mod, include_betas = FALSE)` as the default.
- LaTeX equations for each new model.
- "Show, then explain" rhythm.
- Comments inside code chunks carrying pedagogical weight.
- Closing **Further reading** list, structured the same way (Author Year *Journal* DOI).

**What's NOT in the suite that probably should be.**
- No vignette currently uses `birdsong`, `lake_chemistry`, or `coral_surveys` as the worked example (other than the `data_in_mvgam` swap I just did for `birdsong`).
- No vignette shows the canonical `pp_check(mod)` posterior-predictive workflow as a primary diagnostic.
- No vignette demonstrates the new `posterior_smooths()` / `conditional_smooths()` API.
- No standalone CAR vignette — CAR appears once as a passing mention.
