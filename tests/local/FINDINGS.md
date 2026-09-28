# Open defects

Each entry is a task, deleted once its fix is verified.

## Debt the code carries in recognisable shapes

**89. Six shapes remain, and a scan counts three of them.**

Each shape leaves a mark in the source that a scan can find.
`tests/local/debt_scan.R` counts those marks in `R/` from parse data.
Several counts include false positives, and each hit is examined
before anything is removed.

| shape | the mark it leaves | count |
|---|---|---|
| one fact, several derivers | raw `[[series_var]]` / `[[time_var]]` reads; `sort(unique(...))` axis rebuilds; `inherits(..., "mvbrmsformula")` asked in place of the question meant | 57, 40, 43 |
| a literal standing in for a missing value | `%||% "y"`, `%||% "series"`, `%||% "explicit"` | 98 |
| a missing column skipped | `intersect(x, names(data))`, `if (!col %in% names(df)) next` | 25 |
| a stored copy of a derivable fact | object slots and metadata fields written once and read in a few places | not counted |
| one condition, several refusals | the same fault refused with different wording at different layers | not counted |
| a proxy for the question meant | "the frame has no series column" standing for "the responses are the series"; `length(x) > 1` standing for "multivariate" | found by reading |

The scan's `suppress` count was examined site by site and holds no
debt: each of the six replaces a coercion warning with a refusal
naming the column, or takes the Pareto k out of the object it
suppressed and reports it. All 24 `raw_axis` hits in
`forecast.mvgam.R` were examined line by line. Every one takes a
time column whose name already comes from `axis_vars()`. These
functions subset rows, order them, drop duplicates and type an
empty result against the frame in hand. The values they need are
the frame's. Lines 467, 686 and 1707 each guard a past regression.
The scan matches a raw `[[` of either axis and reports both alike.
The count overstates the debt by those 24. The series axis in that
file comes from the record at every site.

All 43 `mv_class` hits were examined too. Twenty-six check the
input's type before the formula reaches brms, or rebuild a brms
object in place. Ten ask the class
where the count of responses is the question meant, and the two
agree on every formula shape mvgam accepts: `bf(mvbind(y1, y2) ~ x)`
already takes the class `mvbrmsformula`, which makes `inherits()`
and `length(response_columns()) > 1L` equivalent. Seven
re-implement `response_formulas()`'s body, each with a stated
obstacle to calling it: three run before validation, one tolerates
junk input by contract and one needs the univariate case spelled
`""` for the prior table.

Two of the three counted shapes carry false positives in the main.
A pass on either moves code and leaves what a user meets unchanged.
The counts locate the sites. Examining each site is what finds a
defect. The `sort(unique(...))` sites in `sample_innovations.R`
are guarded last resorts, each
carrying a comment naming the order it falls back to.

The tests carry the same debt. A stub that fakes a class with
`structure(y ~ x, class = c("brmsformula", "formula"))` lets an
assertion pass on an object no user could build. So does a stub
carrying slots absent from a real fit. Assertions that compare
counts or use `expect_setequal()` pass where the claim being tested
is an order or a value.

The scans are cheap, and a count falls only when code is deleted.
Each remaining shape gets one pass. A pass removes the rival, the
fallback or the proxy, adds an assertion that fails before the change
and records the count before and after.

The PW growth form took one such pass. Seven sites each spelled
`$growth %||% "linear"` for themselves, each free to disagree with
the others. `pw_growth()` and `pw_is_logistic()` resolve that fact
once, in `R/trend_system.R`. The pass also deleted a dead `growth`
formal on the PW stanvar generator, a `trend_specs$type` fallback
the constructor leaves empty and a third copy of the refusal that
checks the growth value. Count before: 7. Count after: 1.
