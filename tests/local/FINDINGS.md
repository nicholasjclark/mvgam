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
at `validations.R:4202` nor the unknown-series refusal.
`predictions.R:788` continues when the time column is absent, and a
hierarchical fit accepts a series level it never had. One layer
should own what a frame must carry.

## One missing time, three different refusals

**93. An NA time in `newdata` is dropped before a guard names it.**

`horizon_beyond()` (`R/forecast.mvgam.R:498`) ends in `sort()`, which
drops `NA`, and the remaining grid then looks discontinuous. The
refusal at `validations.R:4214` stays out of reach because
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

## Guards that cannot fire

**96. Three dead branches.**

`validations.R:2993` and `:3006` need more than one trend component,
which `validations.R:2982` has already refused.
`stan_assembly.R:1699-1754` needs `resp_name` outside `glm_responses`,
which the loop at `stan_assembly.R:1664` draws it from.
`make_stan.R:432` tests `exists("trend_metadata")` where both branches
above it assign that name. Delete all three.

## One condition, two refusals

**97. More than one trend constructor is refused twice.**

`validations.R:2985` and `validations.R:2854` refuse it with different
wording, and the detection loop is copy-pasted at
`validations.R:2540-2545` and `:2844-2849`. One detection, one
message.

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

## An argument honoured on one path and dropped on another

**101. `re_formula` reaches `mu` and never reaches a dpar.**

`posterior_linpred.R:531-534` and `posterior_predict.R:1055-1061`
call `extract_component_linpred()` with five arguments. The three it
leaves out are `re_formula`, `allow_new_levels` and
`sample_new_levels`, which then take the defaults declared at
`predictions.R:466-468`, while the mean path forwards them. Measured on a fit carrying `(1 | g)` on both
sides, `re_formula = NA` moves `mu` by 2.19 and moves `sigma` by 0.
`predicted_dpar_draws()` is reached from `resolve_family_pars()`,
which puts `posterior_epred()` and `posterior_predict()` on the same
footing. Forward the three.

## A name looked up outside the call that should have set it

**102. `exists("trend_metadata")` searches enclosing environments.**

`brms_integration.R:371` takes `trend_metadata` when
`exists("trend_metadata")` holds. That name is assigned at `:202`,
inside the branch `:195` opens for
`is_trend_setup && !is.null(trend_formula)`. The observation-side
call takes neither. On that path the name is never assigned locally,
which leaves the bare `exists()` reaching the enclosing environments,
the global one included. The usual value is `NULL` by
luck. `exists(..., inherits = FALSE)`, or an explicit `NULL` set
before the branch runs, states what is meant. This one is reachable
on the common path, which separates it from the dead guard 96 records
at `make_stan.R:432`, where both branches assign the name.

## Debt the code carries in recognisable shapes

**89. Six shapes remain, and a scan counts three of them.**

Each shape leaves a mark in the source that a scan can find.
`tests/local/debt_scan.R` reads parse data to count the marks in
`R/`. Several counts include false positives, and each hit is
examined before anything is removed.

| shape | the mark it leaves | count |
|---|---|---|
| one fact, several derivers | raw `[[series_var]]` / `[[time_var]]` reads; `sort(unique(...))` axis rebuilds; `inherits(..., "mvbrmsformula")` asked in place of the question meant | 53, 40, 43 |
| a literal standing in for a missing value | `%||% "y"`, `%||% "series"`, `%||% "explicit"` | 118 |
| a missing column skipped | `intersect(x, names(data))`, `if (!col %in% names(df)) next` | 25 |
| a stored copy of a derivable fact | object slots and metadata fields written once and read in a few places | not counted |
| one condition, several refusals | the same fault refused with different wording at different layers | not counted |
| a proxy for the question meant | "the frame has no series column" standing for "the responses are the series"; `length(x) > 1` standing for "multivariate" | found by reading |

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


