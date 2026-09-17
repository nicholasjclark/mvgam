# Open defects

Each entry is a task, deleted once its fix is verified.

## mvn()

**5. `Psi` has no prior class.**

`mvn()` and `mvt()` estimate one residual scale per component, and
its prior is written into a stanvar. A user who knows their response
scale cannot set the parameter that moves the posterior. Measured
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
assertion (`validations.R:4213`), a bare `checkmate::assert_names()`
whose message names no remedy. `validate_newdata_complete()` has one
call site (`predictions.R:481`, inside `extract_component_linpred()`)
and `ensure_mvgam_variables()` is reached through
`prepare_mvgam_frame()` (`sample_innovations.R:119`). The two guard
different entry points. Deleting either opens a hole on the routes
the other misses. What is duplicated is the wording: one condition
raised under two messages. Both sites should raise one refusal.

## One trend family, two initial distributions

**110. The stationary initialisation reaches one AR path only.**

A plain `AR(p = 1)` starts the latent state at its stationary scale:
`lv_trend[1, j] = scaled_innovations_trend[1, j] / sqrt(1 - square(ar1_trend[j]))`.
Every other AR path starts from the raw innovation instead:

- correlated innovations
- a lag above one
- a moving-average term

At `ar1 = 0.9` the first state is 2.3 times under-dispersed against
stationarity, and the `t = 1` likelihood absorbs that into
`sigma_trend` and `ar1_trend`. Element-wise scaling is the wrong
repair for the correlated case: the stationary covariance solves
`Sigma_x[i, j] = Sigma_eps[i, j] / (1 - phi_i phi_j)`. `VAR()`
computes this already, through `initial_joint_var()`.

## A test whose outcome depends on what ran before it

**127. Two runs of the suite in one session disagree.**

Three tests assert on a warning rlang raises once per R session
(`.frequency = "once"`). A second `devtools::test()` in that session
meets a cache already set. Measured across two runs in one session:
10537 expectations and no failures on the first, the same 10537 and
three failures on the second. The three land one apiece in:

- `test-trend-registry.R`
- `test-occ-family.R`
- `test-closure-unit-families.R`

An expectation count is blind to this. A failing `expect_warning()`
still counts one.

`register_custom_trend()` (`trend_system.R:427`) raises with no
`TESTTHAT` check and reaches the suite directly. The other two
raisers check it, and each test reaches its assertion by unsetting
that with `withr::with_envvar(c(TESTTHAT = ""))`.

Under `rlib_warning_verbosity = "verbose"` rlang ignores the
frequency cache. Nothing in `R/` resets it, and ten further sites
raise under the same idiom.

## One condition, several spellings

**129. Two condition kinds carry more than one spelling.**

`Rscript tests/local/debt_scan.R idioms` counts the spellings per
kind:

| kind | spellings | sites |
|---|---|---|
| warning_once | 2 | `rlang::warn()` 19, the same wrapped in `insight::format_message()` 8 |
| message | 3 | `message()` 11, `cli::cli_inform()` 3, `rlang::inform()` 2 |

`rlang::warn()` renders a named `c()` vector as bullets on its own.
The eight sites wrapped in `insight::format_message()` reach the
same result by a second route.

Some bare `message()` calls print progress, such as "Compiling Stan
program...". Others carry a condition a user acts on.

The cost is on both sides of the call. A reader meets one condition
under several shapes. A caller handling one class misses the rest,
since `rlang::warn()` and `message()` signal different classes. Pick
one formatter for the `warning_once` kind and one spelling per
message kind, until `idioms` reports one spelling for every kind.

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
the axis. The `sort(unique(...))` sites in `sample_innovations.R`
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

## A saved fit embeds the frame that called it

**130. `formula` and `trend_call` keep their calling environment.**

An mvgam fit stores `formula` and `trend_call` as formulas, and a
formula carries the environment it was written in. R serialises a
named environment by reference and a local frame by value. A
`jsdgam()` call at top level captures the global environment and
costs nothing. The same call inside a function captures that
function's frame and writes every local of it into the file.

One small fit, everything held constant apart from where the
formula was written: 14.85 MB written inside the calling function,
0.19 MB written at top level. The objects serialised are that
function's locals exactly, the fit among them.

`update.mvgam()` rebuilds `environment(trend_call)`
(`update.mvgam.R:573-589`). The bindings it re-evaluates have to
survive. The frame they came from does not.

## An expectation that runs only sometimes

**131. Assertion counts vary with the data in two files.**

An expectation under an `if` or inside a handler counts once when
its branch runs and not at all otherwise. A green total then covers
assertions nothing reached. `test-axis-ordering.R` holds about
thirty of these, driven per cell from its shared helpers. Two
helpers return early with no expectation registered:
`test-axis-ordering.R:864` and `test-stancode-standata.R:5824`. The
comment at the first records that a quiet return looks the same in
the output as a pass.
