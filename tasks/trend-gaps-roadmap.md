# Trend-model gaps: positions and order

Source: an analysis of monthly referral counts from two sources, which
fitted `RW()` with drift, `PW()` and a Gaussian process and found each
wrong in one respect. Correlated `CAR()` is planned separately in
`correlated-car.md`.

## 1. Local linear trend, with damping. Build next.

    level_t = level_{t-1} + slope_{t-1} + eta_t
    slope_t = phi * slope_{t-1} + zeta_t

`phi = 1` is the local linear trend and `phi < 1` the damped trend of
Gardner and McKenzie. No existing trend carries a stochastic slope:
`RW()` with drift fixes it, `PW()` moves it at fixed dates only, and a
Gaussian process returns to its mean.

Design:

- A new registered trend, `LLT(damped = FALSE, cor, gr, subgr, n_lv,
  trend_map, df)`. The registry maps one name to one generator, and a
  flag on `RW()` would put two state layouts behind one name.
- `lv_trend` holds the level. The level innovations come from the
  shared innovation system, which gives `cor`, `gr`, `df` and the
  factor forms without new code.
- The slope is a second matrix `slope_trend[N_time_trend, N_lv_trend]`
  with its own `innovations_slope_trend`, scale `sigma_slope_trend`
  and, when damped, `ar1_slope_trend` on (0, 1).
- With `n_lv`, each factor is a local linear trend and series share
  slope shocks through `Z`. The referral sources' shocks correlated at
  about 0.26, which this covers.
- `extract_last_state()` returns the last slope with the last level,
  and `propagate_trend()` gains a branch.
- `requires_regular_intervals = TRUE`, `stationary_source = "none"`.
  Damped factors each take a coefficient, and `ar1_slope_trend` joins
  `factor_indexed_pars` for relabelling.

Risk: `sigma_slope_trend` is weakly identified in short series and its
prior decides the forecast interval's growth. The default needs a prior
predictive check on horizon width before it is fixed.

## 2. Sparse level shifts

**2a. Heavy tails exist.** `RW(df = NA)` and `AR(df = NA)` give
multivariate t innovations. Work: one vignette example. The t shares
its scale across series at each time, and a jump in one series loosens
the others at that time. The example should say so.

**2b. Horseshoe step process. Parked.** It is one more law in
`innovation_vector_prior()`, with a local scale per time and series and
a global scale per series. Those scales funnel under HMC, and the
explicit scale mixture already cost 3 to 14 times the effective sample
size for `nu_trend`. Revisit after the local linear trend, with a
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

- **Time units. Do before 2.0.** Time is not rescaled, and `k_trend`
  and `changepoint_scale` are per raw time step. Their priors change
  meaning with series length. Rescale time to [0, 1] as Prophet does.
  This changes the meaning of existing priors and belongs in the
  release that already breaks them.
- **`changepoint_range`.** Allow 1 and document that 0.8 leaves the
  last fifth of the series without a changepoint.
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

1. Correlated `CAR()` (in progress)
2. Local linear trend (1)
3. Piecewise time units and `loo()` pointer (4, 5c)
4. Centred prototype and decision (5b)
5. Calendar helpers (6), heavy-tail example (2a), length-scale prior (5a)

Parked: horseshoe steps (2b). Not planned: combining trends (3).
