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

## The gate that proves an assertion can fail

**115. The axis mutation gate is absent from the test suite.**

The axis work names a mutation gate as its safeguard: reintroduce
each historical axis defect one at a time and require the suite to
fail on each. Five were listed, and three of them passed silently
before the matrix was written.

A search of the repository comes back empty. `assignInNamespace`
is absent everywhere. `local_mocked_bindings()` appears in
`test-trend-map.R`, `test-update.R`, `test-fitted.R` and
`test-plot-factors.R`, each time for a different purpose. Every
occurrence of `mutation` inside `test-axis-ordering.R` is the word
`permutation`.

The gate ran once and stayed uncommitted. A failure demonstrated
once outside the suite leaves the next change unguarded. A pass is
defined by an assertion that fails before the change. Each
remaining axis pass in entry 89 needs the gate to show that.

The five mutations to restore: a row block cut into one stretch per
response, an alphabetical series axis, a time index numbered by
first appearance, a transposed `times_trend` and `group_inds_trend`
built in row order. A permuted `trend_map` is the sixth.

## Assertions that state one claim several times

**118. The CI suite repeats 562 assertions verbatim.**

`tests/testthat` contains 6345 `expect_*` calls across 83 files.
After normalising whitespace, 55 files contain assertions appearing
more than once within the same file and 562 of those occurrences are
repeats beyond the first. `test-stancode-standata.R` contributes 278
of them against 1374 assertions in that file.

One habit produces the count: a test written for a single trend shape
asserts the whole generated program again, including the lines every
program shares. `matrix[N_time_trend, N_lv_trend] lv_trend;` is
asserted in seven tests, `vector<lower=0>[N_lv_trend] sigma_trend;`
in six, `target += lprior;` in five and `int<lower=1> N_lv_trend;` in
four. Each of these is identical in every program mvgam emits. A
change to the shared skeleton fails five tests. Each failure names a
test for one trend shape.

One pass already ran on the correlated AR(1) init. Three tests
asserted it. Two of them had the factor structure as their subject
and took the init assertions verbatim from the third. Those copies
are deleted. The claim belongs to `"a correlated AR(1) starts at the
joint stationary covariance"`.

A second pass ran on the stationary-start work. Counting whole
`expect_*` call bodies with whitespace normalised, `tests/testthat`
holds 6147 assertions across 83 files. An assertion repeats inside a
single file in 62 of them, giving 639 occurrences beyond the first.
`test-stancode-standata.R` contributes 293 of those against 1330. That
normalisation differs from the one giving 562 above. The two counts
measure the same habit by different rules and do not compare.

The pass deleted seven copies. A new correlated AR(2) local file
restated `AR(p = 2)`'s two parameterisation checks and its
stationarity triangle. Those five assertions belong to
`test-trend-ar-multilag.R`, and the fit's own existence proves the
triangle: a draw off the stationary region has no Cholesky factor and
never enters the posterior. Two more restated the recursion on the
moving average and the raw start. The sparse-lag ARMA block in
`test-stancode-standata.R` owns both.

The shared post-fit battery is a separate matter and holds no debt.
`expect_identical(dim(ep), c(20L, n_obs))` appears in nine local files
and `expect_true(all(is.finite(ep)))` in twelve, each against a
different fit. The README states why: every file drives the same
questions because most defects found here belonged to a method, with
the trend that exposed them incidental.

The count locates sites. Examining each one separates a repeat that
states an invariant twice from a claim about one shape that shares
its text. The split to aim for: one test owning the shared skeleton,
with each shape test asserting what distinguishes it.
