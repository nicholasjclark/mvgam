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

## com_binomial and the trials aterm

**A difference worth recording, for the family work to settle.** The
lower bound on `nu` differs between the two spellings: scalar `nu` is
truncated, reaching Stan as
`normal_lpdf(nu | 1, 1) - normal_lccdf(-5 | 1, 1)`, while a modelled
`nu` gets a plain `normal_lpdf(Intercept_nu | 1, 1)`. Whether an
intercept on the identity scale should carry the scalar's bound is a
question for the family rather than for the axis work.

## An exported constructor with three inert fields

**91. `custom_trend()` stores three function names nothing fetches.**

`custom_trend()` (`R/trend_system.R:1384`) is exported and asserts
`forecast_fun`, `stancode_fun` and `standata_fun` as non-empty
strings. No body in `R/` fetches any of the three, and the function
has no caller. `register_custom_trend()` covers the same ground.
Removing an exported function needs an API decision.

## Prediction accepts frames the axis layer refuses

**92. `newdata` needs no time column and no known series.**

`posterior_epred()` and `predict()` reach neither the time assertion
at `validations.R:4235` nor the unknown-series refusal.
`predictions.R:788` continues when the time column is absent, and a
hierarchical fit accepts a series level it never had. One layer
should own what a frame must carry.

## One missing time, three different refusals

**93. An NA time in `newdata` is dropped before a guard names it.**

`horizon_beyond()` (`R/forecast.mvgam.R:498`) ends in `sort()`, which
drops `NA`, and the remaining grid then looks discontinuous. The
refusal at `validations.R:4247` stays out of reach because
`forecast()` calls `ensure_mvgam_variables()` nowhere. Four fixtures
give three different messages, none naming the missing value.

## One axis name, several resolvers

**94. The time and series column names carry a literal fallback at
six sites.**

`make_stan.R:320-327` resolves both names in two branches that differ
only in whether the spec list is nested, each falling through
`$time_var %||% $time %||% "time"`. The same idiom appears at
`make_stan.R:482-483`, `axes.R:269`, `forecast.mvgam.R:411-412`,
`hindcast.mvgam.R:221-222` and `lfo_cv.mvgam.R:212`. Two field
spellings and six defaults for one pair of names. Put the pair on the
axes record with one resolver and point the six at it.

## The trend's covariates, derived twice

**95. Two routes write `metadata$covariates`.**

`validations.R:4938` takes a `brms::brmsterms()` walk, which adds
random-effect grouping factors and strips `"1"`.
`validations.R:5171-5180` builds the same field from
`extract_predictor_vars()`, which does neither. Both are taken back at
`validations.R:5183`, where they drive the covariate-invariance check,
the collapse to trend grain and what `newdata` must carry.

## One condition, two refusals

**97. More than one trend constructor is refused twice.**

`parse_trend_formula()` (`trend_system.R`) refuses a second
constructor counted from the parsed term labels, while the
`multiple_constructors` restriction in
`validate_trend_formula_restrictions()` (`validations.R`) refuses it
from the formula string at the user boundary, in different wording.
That boundary validates first for every `R/` caller of
`parse_trend_formula()`. The four assertions at
`test-trend-dispatcher.R:322`, `:330`, `:440` and `:783` call the
parser directly, which is the one route reaching its copy. Keep the
boundary, delete the parser's refusal and point those four assertions
at the boundary.

## A response can lose its trend without a word

**98. A discarded warning leaves the program unchanged.**

`stan_assembly.R:1357-1363` calls `insight::format_warning()`, which
only formats a string, discards the value and returns `code_lines`
untouched. Its single caller is `stan_assembly.R:1841`. A response
whose `mu_<resp>` assignment escapes the pattern is fitted with no
trend in its linear predictor, and the program still compiles.

## Two spellings of the training frame

**99. Three kernels take the raw slot.**

`families.R:6927`, `:7947` and `:8154` take `object$data` while the
unit arrays they index come from `mvgam_training_data()`
(`brms_wrappers.mvgam.R:484`), which exists because the frame has two
spellings. The two coincide on every cached fixture, which makes this
a latent hazard and not a measured defect. Point the three at the
accessor.

`per_obs_series_labels()` (`loo.mvgam.R:456`) is a fourth site, and
it carries the axis debt as well: it takes `x$data` and then tests
for a literal `"series"` column at `:457`, which is the question
`axis_row_series()` owns. This is the `loo(by_series = TRUE)` path.

## Copy-paste twins

**100. Two pairs differ in one field.**

`make_stan.R:688-727` and `:781-833` share a 14-parameter signature,
their defaults and their roxygen. They differ in which component they
extract. `make_stan.R:578-584` and `:587-593` test the same four
conditions and return opposite verdicts.

## Nothing checks whether a model is identified

**103. The stacked design's rank is never computed.**

The matrix deciding identification is the observation and trend
designs stacked, mapped through
`times_trend[obs_trend_time, obs_trend_series]`. No body computes it.
Four of seven ordinary pairings measured rank deficient, including
`y ~ 1` with `~ series + AR(p = 1)`, which is the plain way to ask
for a per-series latent level. Reproduce at prefit on those seven
pairings, then decide between a refusal naming the pairing and a
notice that names the confounding.

## The composition families leave their own scale

**104. `forecast(type = "expected")` departs from the simplex.**

On categ, diri and multi, `posterior_epred()` on the training grid
gives probabilities while the forecast on the extension of that grid
gave values outside `[0, 1]`, and `hindcast()` gave 1 for every
species at every site. Beta and the negative binomial stay correct on
the same paths, which places the fault at the shared normaliser.
Reproduce on the jsdgam fixtures before editing.

## One series order, except in one drawn surface

**105. `plot(type = "trend")` sorts its panels.**

Every other per-series surface takes the model's own order, so two
pictures put different series in the same position, and on the
hierarchical fit all six positions differ. Reproduce against
`axes$series$levels`, which is the order the trend matrix numbers its
columns.

## A prediction returns missing cells instead of refusing

**106. A missing covariate value gives `NA` back.**

`posterior_epred()` returns a matrix carrying `NA` cells for a
covariate value `newdata` omits. `posterior_epred.R:1470` carries no
`any.missing = FALSE`. Name the column, as the pre-fit covariate
guard does.

## No guard that an argument reaches the model

**107. The prediction surface has no exhaustiveness test.**

`tests/testthat/test-update.R:401-424` diffs `formals(mvgam)` against
`update_inheritance_table()` and `mvgam_update_uninherited`, and
requires a written reason for every exclusion. That test exists
because a dropped `loadings_prior` had gone unnoticed. The prediction
methods have no equivalent, which is how 101 went unnoticed. One test
over their formals, naming each argument as forwarded or excused.

## Documentation that contradicts the code

**108. Four pages describe something the code does otherwise.**

`?sim_closure_unit_data` puts the Royle-Nichols detection predictor
on the log scale while the family declares `logit` and the simulator
uses `plogis()`. `?jsdgam` documents an `n_lv < n_species` bound the
validator deliberately leaves unenforced. Two published comparisons
rank trend models by `elpd_loo`, which is unreliable on a
latent-trend fit, with nothing pointing at `lfo_cv()`. `mvn()`'s
`forecast(type = "response")` refuses a family `hindcast()` draws,
and no file fits `mvn()` any more, so that one needs a fixture before
it can be settled.

## A trend formula accepted and discarded

**109. `get_prior()` on a bare formula drops `trend_formula`.**

`get_prior.formula()` (`priors.R:1655`) delegates to
`brms::get_prior(object, ...)`. A `trend_formula` lands in brms's dots
and is discarded. Measured on `y ~ x` with `trend_formula = ~ AR()`: the
bare call returns 4 rows, none naming a trend, while
`get_prior(mvgam_formula(y ~ x, trend_formula = ~ AR()), data)` returns
6 rows, two of which name a trend. `get_prior.brmsformula()` (`:1665`)
and `get_prior.default()` (`:1645`) carry the same delegation. Refuse a
`trend_formula` at a method that discards it.

## Debt the code carries in recognisable shapes

**89. Six shapes remain, and a scan counts three of them.**

Each shape leaves a mark in the source that a scan can find.
`tests/local/debt_scan.R` reads parse data to count the marks in
`R/`. Several counts include false positives, and each hit is
examined before anything is removed.

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


