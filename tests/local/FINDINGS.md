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

## Arguments a plot accepts and drops

**116. A multivariate `conditional_effects()` leaves out its
points.**

`points = TRUE` overlays the observations on a univariate fit at
every type sharing the observations' scale. On the wide `mvbf` fit
the rug appears and the points stay absent, across all three
responses.

`insight::find_response()` on that fit gives `count, seen, mass`.
The overlay inside `marginaleffects::plot_predictions()` needs one
observation column and meets three. The rug needs predictor
values, which every row supplies. It appears on the same panel.

Where to look: `response_column(x, resp)` names the column for the
response in scope, and mvgam could add the layer to the panel
`plot_predictions()` returns.
`tests/local/test-grain-mvbf-wide.R` has three failing assertions.

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


