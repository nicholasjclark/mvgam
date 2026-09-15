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


