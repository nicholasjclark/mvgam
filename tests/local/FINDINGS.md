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

The prior is written at `families.R:4428`, inside
`make_psi_stanvars()`, whose signature takes no prior argument. A
prior passed with `class = "Psi"` is dropped without a refusal:
`is_mvgam_managed_class()` files it on the mvgam side
(`brms_integration.R:728-733`) where nothing consumes it. That same
`Psi ~ exponential(1)` is listed in `prior_summary()`
(`brms_integration.R:1024-1035`). The table advertises a prior the
user cannot set.

## Prediction accepts frames the axis layer refuses

**92. Two conditions bypass the level check.**

Two frames reach prediction with no level check at all: a fit whose
`trend_metadata$levels` is NULL returns early (`predictions.R:482`,
`validations.R:3849-3851`) and a frame with no series column never
reaches the comparison (`validations.R:3889`).
`validate_newdata_complete()` exempts that column by design
(`validations.R:3798-3805`).

The time column is held only while `mvgam_term_list()` keeps it.
`varying_meta_vars()` drops a meta var that is constant in the
training data, leaving a fit whose time column never varies outside
the check.

`ensure_mvgam_variables()` carries a second copy of the time
assertion (`validations.R:4211`) and the prediction path never calls
it. Two layers assert one fact. One layer should own what a frame
must carry.

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

## A test whose assertion count is not fixed

**127. The suite's total moves between runs of one tree.**

Two runs of one tree gave totals ten apart, with no failures, no
warnings and no skips in either. One test's assertion count depends on
a draw, which makes the total a poor signal for a regression. Record
per-file counts on two runs to name it.

## One condition, several spellings

**129. Warnings and messages carry six idioms.**

`c4d77c95` rewrote 121 condition messages and the prose linter reports
the whole set clean, which settled the wording. The syntax settled on
the error path alone: 633 of 643 `stop()` sites format through
`insight::format_error()` and 10 pass a bare string.

Warnings and messages keep six spellings. Counted from parse data in
`R/`: `rlang::warn()` at 33 sites, bare `message()` at 11,
`insight::format_warning()` as the raiser at 8, `cli::cli_inform()` at
3, `rlang::inform()` at 2, `warning()` at 2. `call. = FALSE` reaches
125 of the 643 `stop()` calls. Six files mix two or more idioms and
`backends.R` holds four.

The cost is on both sides of the call. A reader meets one condition
under several shapes, and a caller handling one class misses the rest,
since `rlang::warn()` and `warning()` signal different classes. Pick
one spelling per condition kind, apply it across the 59 warning and
message sites and give `debt_scan.R` a mode counting the idioms so
the count reaches one per kind. This is the same shape entry 89 lists
as "one condition, several refusals", measured.

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
suppressed and reports it. `raw_axis` concentrates in
`forecast.mvgam.R`. Several of the rest are the layers that build
the axis. The `sort(unique(...))`
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
