# Trend-model gaps: positions and order

Source: an analysis of monthly referral counts from two sources, which
fitted `RW()` with drift, `PW()` and a Gaussian process and found each
wrong in one respect. Correlated `CAR()` and `LLT()` are built.

## 1. Seasonal `LLT()`: the data side. Build next.

`LLT(season, harmonics)` fits the trigonometric season. What remains:

- Component plots label the season by calendar position from the
  `season` column `series_to_mvgam()` writes, where it exists. The
  period stays in the formula: nothing is read from the data at fit
  time.
- The dummy form, with `s - 1` states, anchored the same way.
- Correlated seasonal shocks under `cor`.
- Compare forecasts on real data against `s(month, bs = "cc") +
  ti(month, time)`, and check that the time-variable warning fits the
  `ti()` alternative.

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

## 4. Piecewise trend details

- **`changepoint_range`.** Document that 0.8 leaves the last fifth of
  the series without a changepoint.
- **Random-walk prior on the deltas.** Skip. Its limit is the local
  linear trend.

## 5. Sampler and comparison

- **5a. Gaussian process length-scale prior.** The pasted item is cut
  off above "overridden by the per-term brms defaults". Reproduce
  first. The likely fix refuses or warns on a prior that no term reads.
- **5b. Centred parameterisation.** Every trend is written non-centred.
  Counts informative enough to pin each state gave `RW()` maximum
  treedepth on 24% of transitions. The `noncentred` argument was
  removed on purpose. Two routes: an automatic rule, or a restored
  switch. Prototype a centred `RW()` on the referral data, and choose
  with the result in hand.
- **5c. LOO.** PSIS-LOO failed on 21 to 30 of 76 points. Section 10 of
  the architecture decisions already says `lfo_cv()` is the comparison
  for a state-space fit. Add an `inform_once()` in `loo()` for a fit
  with a trend.

## 6. Calendar effects

Data helpers, with no model change: a per-series period indicator
(such as January) and a working-day count for `offset(log(.))`, built
from a holiday vector the user supplies. Regional calendars would add a
dependency and stay out.

## Order

1. Seasonal `LLT()`, the data side (1)
2. `changepoint_range` documentation and the `loo()` pointer (4, 5c)
3. Centred prototype and decision (5b)
4. Calendar helpers (6), heavy-tail example (2a), length-scale prior (5a)

Parked: horseshoe steps (2b). Not planned: combining trends (3).
