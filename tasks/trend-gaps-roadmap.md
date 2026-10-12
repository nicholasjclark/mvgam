# Trend-model gaps: positions and order

Source: an analysis of monthly referral counts from two sources, which
fitted `RW()` with drift, `PW()` and a Gaussian process and found each
wrong in one respect. Correlated `CAR()` and `LLT()` are built.

## 1. Seasonal state for `LLT()`. Build next.

A seasonal state turns `LLT()` into the basic structural model of
Harvey (1989): level, slope and season. The seasonal shape then evolves
at an estimated rate, where a cyclic smooth fixes it and a
`ti(month, time)` term sets its drift through a penalty.

Design:

- `LLT(season = s, harmonics = K)`. The trigonometric form comes first:
  K harmonic pairs, each a rotating cos/sin state with its own shock
  scale, 2K states per series. It covers short periods and long ones
  (52 weeks, 365 days), and K below s/2 gives smooth shapes. The dummy
  form, with s - 1 states, can follow.
- The trend is level plus season. The seasonal states sum to zero over
  a period, which separates them from the level and the intercept.
- A shock scale near zero recovers fixed seasonality. Its default prior
  is in units of the data, as the slope priors are.
- State components first: `hindcast()`, `forecast()` and the trend
  plot expose only the summed trend, though `slope_trend` is in the
  draws and `propagate_trend()` steps it. Users need the level, slope
  and season over the fitted times and the horizon, through a
  `component` argument or one extraction helper.
- `extract_last_state()` returns the seasonal states,
  `propagate_trend()` steps them, and `summary()`, `methods_md()` and
  `get_prior()` report the seasonal scales.
- Regular grids only, as for `LLT()`. Seasonal shocks are independent
  per series at first; `cor` and factors follow.

Season specification, decided:

- forecast takes the `frequency` of a `ts`, and `msts()` takes several
  `seasonal.periods`. fable takes `season(period = "year")` or
  `fourier(period, K)`, resolving a string against the tsibble index.
  Prophet takes `period` in days with a `fourier.order`. bsts takes
  `nseasons` and `season.duration`, or `AddTrig(period, frequencies)`.
- Proposal: the period in time steps, as forecast and bsts state it,
  with non-integer and several periods, `season = c(7, 365.25)`, and
  `harmonics` per period. `series_to_mvgam()` takes the same periods
  through `freq`, reads them from a `ts`, `msts` or `tsibble`, and
  writes the position in each period from the calendar origin.
- Phase: anchored at `time = 1`, with no origin argument. Shifting the
  origin rotates the estimated starting state of each harmonic, which
  leaves the likelihood, the seasonal shape and the forecasts unchanged.
  Forecasts align through the rotation by elapsed steps, gaps included.
- The prior on the starting seasonal state is the same for every
  harmonic and for its cos and sin parts. Rotation leaves that prior
  unchanged.
- Component plots label the season by calendar position from the
  `season` column of `series_to_mvgam()` output, where it exists. The
  dummy form needs the same anchor, `time %% s` offset by that column.
- The user states the period in the formula, `LLT(season = 12)`, and
  nothing is read from the data. Attributes on a data frame are lost
  through `dplyr` verbs, `rbind()` and subsetting, and a period taken
  from the data would make one `trend_formula` fit different models.
  fable, bsts and Prophet also take the period in the model.
- The validator refuses a period of 2 or less, `harmonics` above
  `floor(season / 2)` and a period longer than the fitted series.
  `?LLT` carries a table of common periods (12 monthly, 52.18 weekly,
  7 daily) beside the forecast, fable and bsts equivalents.
- String periods such as `"year"` need a Date time column and stay out
  of scope. `season.duration` waits for a case harmonics do not cover.

Risk: more weakly identified scales. Test the geometry on fixtures with
known changing seasonality, and compare forecasts on real data against
`s(month, bs = "cc") + ti(month, time)`. Check that the `LLT()` warning
on a time variable in a formula fits the `ti()` alternative.

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

1. Seasonal state for `LLT()` (1)
2. `changepoint_range` documentation and the `loo()` pointer (4, 5c)
3. Centred prototype and decision (5b)
4. Calendar helpers (6), heavy-tail example (2a), length-scale prior (5a)

Parked: horseshoe steps (2b). Not planned: combining trends (3).
