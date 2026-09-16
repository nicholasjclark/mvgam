# Open defects

Each entry is a task, deleted once its fix is verified.

## mvn()

**5. `Psi` has no prior class.**

`mvn()` and `mvt()` estimate one residual scale per component, and
its prior is written into a stanvar. A user who knows their response
scale cannot set the one thing that moves the posterior. Measured
against a truth of 2.0: `gamma(4, 2)` gives 34 divergences at Psi
rhat 1.009, `exponential(1)` gives 225 at 1.084 and `gamma(4, 8)`
gives 335 at 1.396, with two species pulled to 0.9 by a prior centred
on 0.5. No fixed constant suits every response scale. A new default
needs calibrating over a grid of true `Psi` and factor share.

## com_binomial's nu bound differs by spelling

**128. A modelled `nu` carries no bound.**

`com_binomial()` declares `lb = c(NA, -5)` (`families.R:1019`), and
brms turns that bound into `real<lower=-5> nu;` together with a
truncated prior, `normal_lpdf(nu | 1, 1) - normal_lccdf(-5 | 1, 1)`.
A modelled `nu` builds a linear predictor instead, where the bound
applies to no declared parameter: the program holds a plain
`normal_lpdf(Intercept_nu | 1, 1)` and `nu[n]` is free. Truncating
the intercept would constrain the intercept and leave `nu[n]` free,
which reconciles nothing. Settle what `com_binomial_lpmf()` requires
of `nu` at the bottom of its range, then either constrain the linear
predictor or state the asymmetry on the page.

## Prediction accepts frames the axis layer refuses

**92. `newdata` needs no time column.**

`predictions.R:788-794` continues when the time column is absent.
`ensure_mvgam_variables()` carries the time assertion
(`validations.R:4174`), and the prediction path never calls it. An
unknown series level is now refused through
`validate_prediction_factor_levels()` (`predictions.R:481-484`). Two
conditions still bypass it: a fit whose `trend_metadata$levels` is
NULL and a frame whose series column is absent. One layer should own
what a frame must carry.

## The composition families leave their own scale

**104. `forecast(type = "expected")` departs from the simplex.**

On categ, diri and multi, `posterior_epred()` on the training grid
gives probabilities while the forecast on the extension of that grid
gave values outside `[0, 1]`, and `hindcast()` gave 1 for every
species at every site. Beta and the negative binomial stay correct on
the same paths, which places the fault at the shared normaliser.
Reproduce on the jsdgam fixtures before editing.

## Documentation that contradicts the code

**108. Four pages describe something the code does otherwise.**

`?sim_closure_unit_data` puts the Royle-Nichols detection predictor
on the log scale while the family declares `logit` and the simulator
uses `plogis()`. `?jsdgam` documents an `n_lv < n_species` bound the
validator deliberately leaves unenforced. Two published comparisons
rank trend models by `elpd_loo`, which is unreliable on a
latent-trend fit, with nothing pointing at `lfo_cv()`. `mvn()`'s
`forecast(type = "response")` refuses a family `hindcast()` draws.
No file fits `mvn()` any more, and that one needs a fixture before it
can be settled.

## One trend family, two initial distributions

**110. The stationary initialisation reaches one AR path only.**

A plain `AR(p = 1)` starts the latent state at its stationary scale:
`lv_trend[1, j] = scaled_innovations_trend[1, j] / sqrt(1 - square(ar1_trend[j]))`.
Every other AR path starts from the raw innovation instead:
correlated innovations, a lag above one and a moving-average term. At
`ar1 = 0.9` the first state is 2.3 times under-dispersed against
stationarity, and the `t = 1` likelihood absorbs that into
`sigma_trend` and `ar1_trend`. Element-wise scaling is the wrong
repair for the correlated case: the stationary covariance solves
`Sigma_x[i, j] = Sigma_eps[i, j] / (1 - phi_i phi_j)`. `VAR()`
computes this already, through `initial_joint_var()`.

## A box where the stationarity region is a triangle

**111. `AR(p >= 2)` coefficients carry independent bounds.**

`ar1_trend` and `ar2_trend` are each declared `<lower=-1, upper=1>`
with `normal(0, 0.5)`, emitted at `stan_assembly.R:4449`, `:4469`,
`:4471` and `:4484`. The prior table and the program agree on that.
The AR(2) stationarity region is a triangle bounded by three
inequalities: `|phi_2| < 1`; `phi_1 + phi_2 < 1`; `phi_2 - phi_1 < 1`.
The box admits non-stationary draws such as `phi_1 = phi_2 = 0.9`.
`VAR()` maps an unconstrained matrix through `AtoP()`
(`stan_assembly.R:4936`) and `rev_mapping()` (`:4978`, Heaps 2023),
which is stationary by construction. That mapping at dimension 1
covers `AR(p)`.

## A test whose assertion count is not fixed

**127. The suite's total moves between runs of one tree.**

Two runs of one tree gave totals ten apart, with no failures, no
warnings and no skips in either. One test's assertion count depends on
a draw, which makes the total a poor signal for a regression. Record
per-file counts on two runs to name it.

## Debt the code carries in recognisable shapes

**89. Six shapes remain, and a scan counts three of them.**

Each shape leaves a mark in the source that a scan can find.
`tests/local/debt_scan.R` counts those marks in `R/` from parse data.
Several counts include false positives, and each hit is examined
before anything is removed.

| shape | the mark it leaves | count |
|---|---|---|
| one fact, several derivers | raw `[[series_var]]` / `[[time_var]]` reads; `sort(unique(...))` axis rebuilds; `inherits(..., "mvbrmsformula")` asked in place of the question meant | 55, 40, 43 |
| a literal standing in for a missing value | `%||% "y"`, `%||% "series"`, `%||% "explicit"` | 120 |
| a missing column skipped | `intersect(x, names(data))`, `if (!col %in% names(df)) next` | 25 |
| a stored copy of a derivable fact | object slots and metadata fields written once and read in a few places | not counted |
| one condition, several refusals | the same fault refused with different wording at different layers | not counted |
| a proxy for the question meant | "the frame has no series column" standing for "the responses are the series"; `length(x) > 1` standing for "multivariate" | found by reading |

The scan's `suppress` count was examined site by site and holds no
debt: each of the six replaces a coercion warning with a refusal
naming the column, or takes the Pareto k out of the object it
suppressed and reports it. `raw_axis` concentrates in
`forecast.mvgam.R`, which carries 22 of its 55. Several of the rest
are the layers that build the axis. The `sort(unique(...))`
sites in `sample_innovations.R` are guarded last resorts, each
carrying a comment naming the order it falls back to.

The tests carry the same debt. A stub that fakes a class with
`structure(y ~ x, class = c("brmsformula", "formula"))` lets an
assertion pass on an object no user could build. So does a stub
carrying slots a real fit no longer has. Assertions that compare
counts or use
`expect_setequal()` pass where the claim being tested is an order or
a value.

The scans are cheap, and a count falls only when code is deleted.
Each remaining shape gets one pass. A pass removes the rival, the
fallback or the proxy, adds an assertion that fails before the change
and records the count before and after.
