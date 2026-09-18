# Open defects

Each entry is a task, deleted once its fix is verified.

## One trend family, two initial distributions

**110. Two AR paths still start off their stationary distribution.**

Three paths now start at the stationary distribution. A plain
`AR(p = 1)` divides the first innovation by `sqrt(1 - ar1^2)`. A
correlated `AR(p = 1, cor = TRUE)` and a grouped
`AR(p = 1, gr = ...)` scale the first innovation row by the Cholesky
factor of `Gamma[a, b] = Sigma[a, b] / (1 - ar1[a] * ar1[b])`, taken
over the series of one group in the grouped case.

Two paths still start from the raw innovation:

- a lag above one
- a moving-average term

At `ar1 = 0.9` the first state is 2.3 times under-dispersed against
stationarity, and the `t = 1` likelihood absorbs that into
`sigma_trend` and `ar1_trend`.

`AR(p > 1)` needs the Yule-Walker solve on the companion form, which
`initial_joint_var()` supplies in the VAR generator today. That
function shares one `functions` stanvar with `sqrtm`, `AtoP`,
`kronecker_prod` and `rev_mapping`. Reuse means splitting it.

`ARMA(1, 1)` needs the pair `(lv_0, eps_0)`. Scaling the first
innovation row alone matches the marginal variance and makes `lv[1]`
and `eps[1]` perfectly correlated, where the stationary process has
`cov(lv_1, eps_1) = sigma^2`. `lv[2]` takes `eps[1]` again, which
makes the joint distribution the quantity to match. A new
`init_innovations_trend` parameter supplies `eps_0`.

A sparse lag set keeps the raw start. `ar1_trend` and `ar3_trend`
are declared as bounded parameters with no stationarity guarantee,
and `cholesky_decompose` would reject such a draw.
`ar_lags_stationary()` is the gate.

Loop indices in a generated block take an `_init` suffix. A brms
observation formula with predictors declares `b` for the
population-level coefficients, and Stan refuses a shadowing loop
variable.

## One grouped process, two stationary scales

**113. The marginal surface scales a grouped trend per series.**

`stationary_correlated_params()` (`sample_innovations.R:2371`)
computes `Gamma0[i, j] = Sigma[i, j] / (1 - ar_i * ar_j)` for a
correlated `AR(1)`, which is what the Stan program now starts at.
The grouped branch (`:2318-2344`) multiplies `sigma_group_trend`
by `sqrt(mult)` per series, where `mult` holds the per-series
`1 / (1 - ar^2)`. Stan's grouped program starts at the joint form
taken over each group's member series.

The comment above the correlated branch (`:2302-2307`) sets out
why a factor applied series by series differs from the joint form.
It measures the gap at a fifth of the cross-covariance on a fitted
pair. That reasoning covers the grouped case. The grouped branch
uses the form it argues against.

`ar_stationary_multiplier()` supplies a per-series vector. A
grouped repair needs each group's `Sigma_group` with the `ar1` of
its member series, which `group_inds_trend` already maps. A
grouped factor model is refused ("Hierarchical AR models cannot
use factor models"), which fixes each group's member count at its
subgroup count.

## One sampled scalar, three printed rows

**112. `coef_sharing = "shared"` reports the coefficient three
times.**

`summary()` on a two-series `AR(p = 1, coef_sharing = "shared")` fit
prints three rows holding one number:

| parameter | Estimate | Est.Error | Rhat | Bulk_ESS |
|---|---|---|---|---|
| `shared_ar1_trend[1]` | 0.68 | 0.09 | 1.04 | 30.02 |
| `ar1_trend[1]` | 0.68 | 0.09 | 1.04 | 30.02 |
| `ar1_trend[2]` | 0.68 | 0.09 | 1.04 | 30.02 |

`rep_vector` copies the sampled scalar into every series. The three
rows hold one quantity. `variables()`, `tidy()` and
`posterior_summary()` list all three as well. A reader meets three
parameters where the model samples one.

`is_hidden_unrotated()` is the existing mechanism for this shape. It
hides the rotation-indeterminate `Z` while `Z_tilde` is present.
`mvgam_user_pars(all = TRUE)` still reaches the hidden block. The
same treatment would leave `shared_ar{lag}_trend` in the default
view and keep `ar{lag}_trend` reachable for the consumers that take
it, `forecast()` and the trend recursion among them.

Under `"hierarchical"` each per-series coefficient is its own draw
from `normal(mu_ar{lag}_trend, sigma_ar{lag}_trend)`, which leaves
that mode unaffected.

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


