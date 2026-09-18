# Open defects

Each entry is a task, deleted once its fix is verified.

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

A grouped `AR(p = 1, gr = ...)` takes the stationary branch and is
wrong in the other direction. `trend_specs$cor` is FALSE there,
which applies the per-series divisor, while the innovations are
correlated within group through `L_group_trend`
(`stan_assembly.R:3917`). The diagonal is right. The covariance
across series inside a group is wrong. That makes four paths in all.

`L_Sigma_trend` is declared inside an anonymous block
(`stan_assembly.R:2197-2200`). A correlated-case repair builds its
covariance from `sigma_trend` and `L_Omega_trend` instead.

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


