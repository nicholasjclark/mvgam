# Trend-model gaps: positions and order

Source: an analysis of monthly referral counts from two sources, which
fitted `RW()` with drift, `PW()` and a Gaussian process and found each
wrong in one respect. Correlated `CAR()` and `LLT()` are built.

## 0. Shared foundations

Each is built once, then every consumer reads it, including existing
methods.

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

1. F4, then community gradients and forecasts 4b
2. 4c, 4d, 4e, 1e, 4f
3. Dummy seasonal form 1f
4. Real-data seasonal check 1g, then 1h
5. Centred prototype and decision (5)
6. Calendar helpers (6), heavy-tail example (2a)

Parked: horseshoe steps (2b). Not planned: combining trends (3).
