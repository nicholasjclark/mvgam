# Trend-model gaps: positions and order

Source: an analysis of monthly referral counts from two sources, which
fitted `RW()` with drift, `PW()` and a Gaussian process and found each
wrong in one respect. Correlated `CAR()` and `LLT()` are built.

## 0. Shared foundations

Each is built once, then every consumer reads it, including existing
methods.

**F3. Term-level decomposition of the linear predictor.** One
function returns named `[draws, obs]` matrices whose sum is the link
predictor:
- observation fixed effects, each smooth, each random-effect group
  and each `gp()`, from `linpred_composer.R`;
- trend-formula terms;
- the latent trend, split by `trend_components()` into level, slope
  and season where declared;
- latent factors, one per factor.

`get_combined_linpred()` adds these parts and must agree with the
sum exactly. A part's response-scale contribution is the expected
value with the full predictor less the expected value without that
part, through the fit's own inverse link and expectation, for every
family. Consumers: variance partitioning for every fit (not only
JSDMs), seasonal strength, the season on the response scale.

Build, one step at a time, each checked before the next.
`get_linpred_parts()` gives the observation terms, the trend-formula
terms and the latent state (by component and factor) at fitted times.
`get_epred_parts()` gives each part's share of the expected value.
4. Every term brms fits, in the composer (`linpred_composer.R`), which
   every prediction method calls. Each refusal below blocks
   `posterior_epred()`, `predict()`, `log_lik()` and `forecast()` on a
   fit `mvgam()` builds:
   - multi-membership groups, `(1 | mm(g1, g2))`;
   - grouping levels the fit never saw, under `allow_new_levels`
     and `sample_new_levels` as brms draws them;
   - an `re_formula` naming some of the group-level terms.
   Check: brms parity on a local fixture per item.

   In progress, uncommitted: `mo()`/`me()`/`mi()` in the composer,
   checked by `tests/local/test-obs-special-terms.R` (fits A, B, C).
   Playing with the cached fits found these defects:
   - Latent matching keys on every brms column (`fitted_row_index()`
     in `prepare_linpred_data()`). A counterfactual row that changes
     only `z` loses its imputed `xm` (C: `avg_slopes()` errors) or
     redraws `Xme` (B). Fix: match on the row's unit plus the term's
     own columns (`all.vars()` of the `me()` call; the `mi()`
     response's left side). Units come from
     `get_observation_structure()`, never literal `time`/`series`
     columns, so jsdgam, hierarchical, single-series and wide `mvbf()`
     fits all resolve. brms data lacks the axis columns. Obs rows are
     `fit$data[response_rows(), ]`; trend rows are the time-major
     cell grid (`trend_cell_frame()`), `(time, .trend)` under
     `by = lv_axis()`. A grouped `me(gr = )` keys on `(gr, x, sdx)`
     alone, shared per level. Settle how a wide fit's trend `me()`
     takes cell `(time, resp)`.
   - B: `conditional_effects()` on `go` and `avg_slopes()` fail with
     brms's "Measured values and measurement error should be unique
     for each group" (`data_Xme()`). Harmonising the frame is wrong,
     since brms accepts `me(x, sdx, gr = g) + x`. Find a lever that
     leaves the other terms' data exact.
   - C: `residuals()` errors "incorrect number of dimensions".
   - `how_to_cite()` writes "a identity link".
   - Then: rerun the special-terms file and `test-linpred-parity.R`;
     in `tests/local/README.md` drop the special-fit text from the
     parity entry and add the new file; delete
     `fixtures/val_linpred_special.rds`. Scratch exploration script:
     `play.R` (prints every surface on fits A, B and C).
   - Gap: a new or future row with a missing `mi()` covariate should
     draw it from that variable's own model.
5. The forecast horizon. The same parts on future rows: trend-formula
   parts at the future covariates, latent parts from the propagated
   state (`propagate_*()` already returns `components`), each per
   draw. Check: the parts sum to `forecast(type = "link")` per draw.

**F4. Derived community quantities per draw.** One function maps
predictive draws keyed by series (from `posterior_epred()`,
`predict()`, `hindcast()` or `forecast()`) to a quantity computed
across series within each draw:
- richness, the expected number of species present;
- community-weighted mean traits;
- pairwise co-occurrence;
- Hill numbers.

The same function serves a covariate gradient (a `datagrid()` frame)
and the forecast horizon. Consumers: community gradients, community
forecasts.

Posterior support comes from `sign_probabilities()` and summaries from
`median_interval_cols()`. Matrix plots go through `matrix_heatmap()`.

## 1. Seasonal forecasting

`seasonality()` gives the profile and per-position plots and the
amplitude and peak of each cycle, with their change. Remaining:

- **1c. Seasonal strength (F3).** The season's share of variance,
  per series and period, added to `seasonality()`.
- **1d. Response scale (F3).** The season's contribution to the
  expected value, for every family. Under a log link the ratio of the
  two expectations is the percent change.
- **1e. Correlated seasonal shocks under `cor`.** They take the level's
  correlation matrix with their own scales, as the slope shocks do.
  `residual_cor()` reports which series shift seasons together.
- **1f. Dummy seasonal form.** `s - 1` states tied to calendar
  positions, behind an argument whose default stays trigonometric.
- **1g. Real-data check.** Forecasts against `s(month, bs = "cc") +
  ti(month, time)`. Check that the time-variable warning in
  `validate_data_llt()` fits the `ti()` route.
- **1h. Discoverability.** `LLT()` and the overview article point to
  `seasonality()`, with a worked example on real data from 1g.
- **1i. Forecast uncertainty by source (F3 step 5).** Per series and
  horizon, the share of forecast variance from each source, for every
  fit. The law of total variance splits parameter uncertainty,
  `Var(E[y_h | theta])`, from process error, `E(Var[y_h | theta])`,
  with the second from propagation with and without fresh
  innovations. The parameter term splits by part (each covariate
  term, the latent dynamics), with the covariances between parts
  reported, not discarded. Motivation: the 1.x `gamvar_unc()` scripts
  (Portal analysis), which nested four scenarios in a fixed order,
  zeroed coefficients, fixed the VAR parameters at one draw and
  clipped negative increments. A stacked-area plot by horizon.

## 4. Joint species distribution models

Other packages: gllvm `VP()`, `predictSR()` richness,
`predictPairwise()` co-occurrence, `coefplot()`, phylogenetic plots,
constrained ordination and quadratic optima. Hmsc
`computeVariancePartitioning()` (with the share of variation in
species' responses that traits explain), `computeAssociations()` with
support levels, `plotGradient()` for richness, species and CWM traits
with posterior support, `plotBeta()` and `plotGamma()` sign-support
heatmaps, and `evaluateModelFit()` per species. sjSDM splits variation
into environment, space and associations, with ternary plots of sites
and species. Only mvgam has dynamic factors, detection-corrected
occupancy and abundance, VAR interactions and community forecasts.

- **4a. Variance partitioning (F3).** Per series: environment,
  latent associations, trend dynamics, season and, where present,
  detection. It works for every mvgam fit. A stacked bar per species
  and a community summary.
- **4b. Community gradients and forecasts (F4).** Richness, CWM
  traits and co-occurrence along a covariate gradient and over the
  forecast horizon, with posterior support for change.
- **4c. Sign-support heatmaps.** Species by
  covariate effects, and trait by environment effects for
  `trait_slopes` fits.
- **4d. Support levels on `residual_cor()`.** The support threshold
  is fixed at 0.95. Let the user choose it, and let the plot show only
  associations above it.
- **4e. Per-species fit.** Tjur R2 and AUC for binary responses, R2
  for counts, from the `bayes_R2()` path and `per_obs_series_labels()`.
- **4f. Associations net of detection.** For `occ()` and `nmix()` fits,
  latent-state associations set against naive co-occurrence, showing
  what detection error alone would have suggested.

## 2. Sparse level shifts

**2a. Heavy tails exist.** `RW(df = NA)` and `AR(df = NA)` give
multivariate t innovations. Work: one vignette example. The t shares
its scale across series at each time, and a jump in one series loosens
the others at that time. The example should say so.

**2b. Horseshoe step process. Parked.** It is one more law in
`innovation_vector_prior()`, with a local scale per time and series and
a global scale per series. Those scales funnel under HMC, and the
explicit scale mixture already cost 3 to 14 times the effective sample
size for `nu_trend`. Revisit with a
regularised horseshoe and a recovery study on dated shifts.

## 3. Combining trends. Not planned.

One `lv_trend` matrix and one block of dynamics parameters hold the
state, and `validate_trend_formula()` refuses a second constructor
(architecture decisions, section 2). A sum of constructors needs
parallel state, parameter and extraction paths in every post-fit
method.

The request was `PW() + AR(1)`: deterministic growth with persistent
deviations. The local linear trend gives a stochastic slope with level
noise, the same model class. One check is worth making: whether `PW()`
with an observation-side brms `ar()` term already fits, and if it does,
document it.

## 5. Centred parameterisation

Every trend is written non-centred. Counts informative enough to pin
each state gave `RW()` maximum treedepth on 24% of transitions. The
`noncentred` argument was removed on purpose. Two routes: an automatic
rule, or a restored switch. Prototype a centred `RW()` on the referral
data, and choose with the result in hand.

## 6. Calendar effects

Data helpers, with no model change: a per-series period indicator
(such as January) and a working-day count for `offset(log(.))`, built
from a holiday vector the user supplies. Regional calendars would add a
dependency and stay out.

## Order

1. F3, then 1d, 1c and variance partitioning 4a, then 1i
2. F4, then community gradients and forecasts 4b
3. 4c, 4d, 4e, 1e, 4f
4. Dummy seasonal form 1f
5. Real-data seasonal check 1g, then 1h
6. Centred prototype and decision (5)
7. Calendar helpers (6), heavy-tail example (2a)

Parked: horseshoe steps (2b). Not planned: combining trends (3).
