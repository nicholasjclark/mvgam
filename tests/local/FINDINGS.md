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

## A response can lose its trend without a word

**98. A discarded warning leaves the program unchanged.**

`handle_response_trend_injection()` (`stan_assembly.R:1281-1288`)
calls `insight::format_warning()`, which only formats a string,
discards the value and returns `code_lines` untouched. No `warning()`
wraps it. The user is told nothing at all. Its single caller is
`stan_assembly.R:1705`. A response whose `mu_<resp>` assignment
escapes the pattern is fitted with no trend in its linear predictor,
and the program still compiles.

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

**100. One pair differs in one field.**

`stancode.mvgam_formula()` (`make_stan.R:704`) and
`standata.mvgam_formula()` (`:856`) share a 14-parameter signature,
their defaults and their roxygen. They differ in which component they
extract. Each repeats the generator call as well as the
missing-component refusal. The repeated signature follows from S3
dispatch and roxygen usage. The repeated body is removable.

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
covariate value `newdata` omits. `posterior_epred.mvgam()`
(`posterior_epred.R:449-525`) asserts nothing about the matrix it
returns, and `ordinal_probs()` (`posterior_epred.R:1490`) asserts
`eta` numeric with no `any.missing = FALSE`. Name the column the way
the pre-fit covariate guard names it.

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
with `normal(0, 0.5)`. The prior table and the program agree on that.
The AR(2) stationarity region is a triangle bounded by three
inequalities: `|phi_2| < 1`; `phi_1 + phi_2 < 1`; `phi_2 - phi_1 < 1`.
The box admits non-stationary draws such as `phi_1 = phi_2 = 0.9`.
`VAR()` maps an unconstrained matrix through `AtoP()` and
`rev_mapping()` (Heaps 2023), which is stationary by construction.
That mapping at dimension 1 covers `AR(p)`.

## One block body, two bounds

**120. `extract_stan_block_content()` ends a block at the next
header.**

`extract_stan_block_content()` (`stan_assembly.R:7460`) takes a
block's end from the following block's header and then strips one
trailing `}`. `stan_block_body()` (`stan_source.R:103`) matches the
brace the header opened. A blank line or a comment between `}` and
the next header leaves the block's own closing brace in the returned
body: a `data` block comes back as
`"  int<lower=1> N;\n  vector[N] Y;\n}\n"`. Seven call sites build six
`brms::stanvar()` objects from that value, and a stray `}` unbalances
the assembled program. Point them at `stan_block_body()`.

## Comment stripping that cannot see a string

**121. Three strippers split on `//` inside a string literal.**

`strip_stan_comments()` (`stan_assembly.R:539`) and the inline
`gsub("//.*$", "", ...)` at `stan_assembly.R:593`, `:7799` and `:8553`
cut a line at its first `//`, string literals included.
`stan_drop_line_comment()` (`stan_source.R:158`) keeps the literal.
The `:7799` site is `extract_stan_identifiers()`, where
`print("a // b"); real keepme = 1;` yields `print` and `a` alone.
`keepme` then never takes its `_trend` suffix and collides with the
observation-side name of that spelling. `strip_stan_comments()` also
handles `/* */`, which neither shared helper does; fold that in.

## Two answers for what the functions block declares

**122. Called functions are reported as declared.**

`extract_declared_functions()` (`mu_expression_analysis.R:231`)
matches `identifier identifier(` with one regex, which `return
log1p(x);` satisfies. On a body calling `log1p()` and `sqrt()` it
returns `myfun`, `log1p` and `sqrt`. `parse_stan_functions()`
(`stan_assembly.R:8382`) returns `myfun` alone. The first fills
`context$all_functions`, the list against which a token in a `mu`
expression is classified as a call. The second fills
`mapping$custom_functions`, which keeps a name from `_trend`
renaming. One token, two classifications.

## Two rules for whether brms wrote a GLM likelihood

**123. A closed list and an open pattern disagree.**

`glm_calls_present()` (`glm_analysis.R:172`) tests the five names in
`mvgam_glm_families`. `map_responses_to_glm()` (`:259`) tests
`stan_density_call_pattern("_glm")`. `detect_glm_usage()`
(`stan_assembly.R:994`) dispatches to the first when `response_names`
is NULL and to the second otherwise. On
`target += categorical_logit_glm_lpmf(Y_cat | Xc, Intercept, b);` the
first reports no GLM and the second reports one. The quiet path
returns before `glm_family_of_line()`'s refusal is reached, and the
trend is computed without reaching the linear predictor.

## Three routes to the factor count

**124. `n_lv` is derived once and stored twice.**

`spec_n_lv()` (`axes.R:92`) takes both spec depths through
`spec_field()`. `trend_propagation.R:671` writes
`trend_metadata$n_lv` from a bare `spec$n_lv`, and
`validations.R:4725` writes `n_lv_for_grain` from `parsed_trend$n_lv`.
On a nested spec, `list(trend_model = list(trend = "AR", n_lv = 2))`,
`spec_n_lv()` gives 2. The bare `$` access gives NULL. Readers split
three ways: `plot_helpers.R:797`, `predictions.R:592` and
`diagnostics.mvgam.R:488`.

## Three rules for one spec or a list of them

**125. `first_trend_spec()` takes the first element of a bare spec.**

`trend_spec_head()` (`axes.R:69`) requires every entry to be a list.
`first_trend_spec()` (`multivariate_helpers.R:24`) tests
`inherits(ts, "mvgam_trend")`. `validations.R:3369` tests
`is_multivariate_trend_specs()` alone. On an unclassed univariate spec
`list(trend = "AR", n_lv = 2L, gr = NA)`, `first_trend_spec()` returns
the string `"AR"`. `detect_factor_n_lv()` composes the two and stops
on a checkmate assertion.

## Four counts of the series axis

**126. `series_info$n_series` is set only for a literal column.**

`extract_series_information()` (`mvgam_core.R:1445-1457`) sets
`n_series` when a column named `series` is present. A hierarchical fit
and an `mvbind()` fit carry none. The field stays NULL while
`mvgam_axes(object)$series$n` holds the count. `plot_helpers.R:801`,
`sample_innovations.R:713` and `print.mvgam.R:142` take the bare
field. `sign_canonical.R:87-93` records this having already returned
an object untouched with nothing said.

## The suite's assertion count moves between runs

**127. Two runs of one tree report different totals.**

`devtools::test()` reported 10417, then 10427 on a tree whose only
change was in `R/`, then 10427 again once ten assertions were added to
`test-stancode-standata.R`. That file's own count moved 1476 to 1486,
measured on its own. Ten assertions ran and ten others did not. Every
run reported no failures, no warnings and no skips. What moves is the
number of assertions a test runs, which makes the total a poor signal
for a regression. Record per-file counts on two runs of one tree to
find the test whose count depends on a draw.

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


