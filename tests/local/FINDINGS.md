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

## Factor arguments a trendless model discards

**90. `trend_map` and `loadings_prior` are taken and dropped.**

Both are applied onto the trend specs in `make_stan.R` at `:176`
and `:192`. A model written with no `trend_formula` has
`mv_spec$trend_specs = NULL`, and both helpers open by returning
that: `apply_trend_map_alias()` at `validations.R:3447` and
`attach_loadings_prior_spec()` at `:3504`. The argument reaches
neither the Stan data nor the program, and nothing is raised.

Measured on three series mapped onto two trends,
`run_model = FALSE`:

| call | `Z` | `N_lv_trend` | warned |
|---|---|---|---|
| `trend_map` + `AR(p = 1)` | 3x2 | 2 | no |
| `trend_map`, no `trend_formula` | absent | absent | no |
| `loadings_prior` + `AR(n_lv = 2)` | - | 2 | no |
| `loadings_prior`, no `trend_formula` | - | absent | no |

The last row also puts the structured kernel outside the program,
so a user asking for feature-based loadings is handed an ordinary
GAM. `loadings_prior` is normalised and its compatibility asserted
first, which makes the silence harder to notice.

`refuse_top_level_n_lv()` (`validations.R`) already refuses the
third factor argument, naming what was asked for, what happened
instead and where to write it. The same refusal belongs at both
sites above, owned by the layer that drops them.

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


