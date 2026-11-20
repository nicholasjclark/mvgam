# Trend-model gaps: positions and order

Source: an analysis of monthly referral counts from two sources, which
fitted `RW()` with drift, `PW()` and a Gaussian process and found each
wrong in one respect.

## 1. Seasonal forecasting

- **1f. Dummy seasonal form.** `s - 1` states tied to calendar
  positions, behind an argument whose default stays trigonometric.
- **1g. Real-data check.** Forecasts against `s(month, bs = "cc") +
  ti(month, time)`. Check that the time-variable warning in
  `validate_data_llt()` fits the `ti()` route.
- **1h. Discoverability.** `LLT()` and the overview article point to
  `seasonality()`, with a worked example on real data from 1g.

## 4. Joint species distribution models

Reference points: gllvm `coefplot()`, and Hmsc `plotBeta()` and
`plotGamma()` sign-support heatmaps, `computeAssociations()` support
levels and `evaluateModelFit()` per species.

- **4f. Associations net of detection.** For `occ()` and `nmix()` fits,
  latent-state associations set against naive co-occurrence, showing
  what detection error alone would have suggested.

## 2. Sparse level shifts

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
method. The local linear trend covers the requested `PW() + AR(1)`
model class. An observation-side brms `ar()` is refused: no prediction,
forecast or log-likelihood carried it.

## 6. Calendar effects

Data helpers, with no model change: a per-series period indicator
(such as January) and a working-day count for `offset(log(.))`, built
from a holiday vector the user supplies. Regional calendars would add a
dependency and stay out.

## Order

1. 4f
2. Dummy seasonal form 1f
3. Real-data seasonal check 1g, then 1h
4. Calendar helpers (6)

Parked: horseshoe steps (2b). Not planned: combining trends (3).
