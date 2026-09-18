# Open defects

Each entry is a task, deleted once its fix is verified.

## One trend family, two initial distributions

**110. Three AR shapes still start off their stationary distribution.**

Four paths now start at the stationary distribution. A plain
`AR(p = 1)` divides the first innovation by `sqrt(1 - ar1^2)`. A
correlated `AR(p = 1, cor = TRUE)` and a grouped
`AR(p = 1, gr = ...)` scale the first innovation row by the Cholesky
factor of `Gamma[a, b] = Sigma[a, b] / (1 - ar1[a] * ar1[b])`, taken
over the series of one group in the grouped case. A contiguous
`AR(p > 1)` with independent innovations draws its first `p` states
through `ar_stationary_init()`, built from the partial
autocorrelations. The marginal variance is
`sigma^2 / prod(1 - pacf^2)`. Conditioning on `m` earlier states
multiplies it by `prod_{k<=m}(1 - pacf_k^2)`. Measured against the
companion-form Lyapunov solution, the two agree to 4.4e-11 relative
through `p = 7`.

Three shapes still start from the raw innovation:

- a correlated `AR(p > 1)`
- a grouped `AR(p > 1)`
- a moving-average term

At `ar1 = 0.9` the first state is 2.3 times under-dispersed against
stationarity, and the `t = 1` likelihood absorbs that into
`sigma_trend` and `ar1_trend`.

The R marginal path lifts these same shapes.
`ar_stationary_multiplier()` solves the companion form and scales the
innovation standard deviations. On a grouped fit the scales move by
20.9 for `AR(p = 2)` and by 6.38 for `AR(p = 1, ma = TRUE)` against
their posterior values, while the program starts both from the raw
innovation. Starting the program at stationarity settles each.

The two multivariate shapes need the Yule-Walker solve on the
companion form, which `initial_joint_var()` supplies in the VAR
generator today. That function shares one `functions` stanvar with
`sqrtm`, `AtoP`, `kronecker_prod` and `rev_mapping`. The split is
clean: `initial_joint_var()` calls `kronecker_prod`, while `AtoP`
and `rev_mapping` call `sqrtm` and form a closed group. The size of
the solve is the thing to weigh. It allocates a square matrix of
side `((p + q) * m)^2`, which reaches 144 on six correlated series
at `p = 2` and 3600 on twenty series at `p = 3`.

`ARMA(1, 1)` needs the pair `(lv_0, eps_0)`. Scaling just the first
innovation row matches the marginal variance and makes `lv[1]`
and `eps[1]` perfectly correlated, where the stationary process has
`cov(lv_1, eps_1) = sigma^2`. `lv[2]` takes `eps[1]` again, which
makes the joint distribution the quantity to match. A new
`init_innovations_trend` parameter supplies `eps_0`.

A sparse lag set keeps the raw start by design. `ar1_trend` and
`ar3_trend` are declared as bounded parameters whose stationarity is
unchecked. `cholesky_decompose` would reject such a draw.
`ar_lags_stationary()` is the gate. It admits contiguous lag sets
from `p = 2` upward.

Loop indices in a generated block take an `_init` suffix. A brms
observation formula with predictors declares `b` for the
population-level coefficients, and Stan refuses a shadowing loop
variable.

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

## Generated Stan the user reads

**114. Every comment mvgam writes into a trend stanvar is dropped.**

`stancode()` on `AR(p = 1)`, `AR(p = 2)` and
`AR(p = 1, cor = TRUE)` returns a program where each mvgam-authored
comment is absent:

- the initialisation comment on all three branches
- the `Latent states with AR dynamics` header
- the `Partial autocorrelations to AR coefficients` note in the
  functions block

Seven `//` lines survive inside the same transformed parameters
block, and each one is brms's own.

A reader of the generated program sees brms's account of its blocks
while the trend's account goes missing. The polish step reorganises
statements, and whether the stripping guards against a comment
landing away from the line it describes is unverified.
`polish_generated_stan_code()` in `R/stan_polish.R` is where to
look.


