# Local linear trend: design

Reviewed 2026-10-04 by a statistical review and a code review. The
decisions below record what each changed.

## Model

For latent process `j` (a series, or a factor under `n_lv`):

    level[t, j] = level[t-1, j] + slope[t-1, j] + eta[t, j]
    slope[t, j] = phi_j * slope[t-1, j] + zeta[t, j]

`phi = 1` is the local linear trend (Harvey 1989). `LLT(damped = TRUE)`
samples `phi` on (0, 1). This writes the damping in the slope
equation; Gardner and McKenzie (1985) put it in the level equation
(`level + phi * slope`). The two are the same model with
`slope' = phi * slope`, and the write-up states which form it uses.

- Level innovations `eta` come from the shared innovation system:
  `sigma_trend`, `cor`, `gr`/`subgr`, `df`, the factor forms.
- Slope innovations `zeta` are Gaussian. Under `cor = TRUE` they take
  the level's correlation factor (`L_Omega_trend`, or each group's
  factor under `gr`) with their own scales `sigma_slope_trend`. Harvey's
  homogeneous SUTSE models borrow the level structure the same way,
  and a separate slope correlation would be nearly all prior.
- `df` applies to the level alone. A t level shock absorbs a level
  shift without bending the slope.

## Start

- `level[1] = eta[1]`, as `RW()`. The observation intercept carries the
  level.
- `slope[1] = init_slope_trend[j]`, a free parameter, damped or not. A
  start at one slope innovation would shrink the initial growth toward
  zero.
- Revised 2026-10-04: the damped slope first started at its stationary
  law. A series that grows fast and slows (slope 0.25 decaying at
  `phi = 0.93`, shock SD 0.004, 45 Poisson points) put its true start
  23 stationary SDs out. Fitted, `sigma_slope` rose to 0.027 to reach
  it, and with `inv_gamma` on `sigma_trend` one chain sat at
  `phi = 0.99` (601 divergences, R-hat 2.2). Under a free start the
  same data gave four chains at `phi` 0.914 to 0.916, start 0.26 (truth
  0.25) and `sigma_slope` 0.010. `forecast::ets()` (damped trend,
  `phi` in [0.8, 0.98]) and bsts's semilocal trend
  (`initial.slope.prior = NormalPrior(0, sdy)`) also start the slope
  free.

## Factor models

With every loading sampled the level scale is fixed at 1. Per-factor
`sigma_slope` would leave a continuous rotation ridge that the
permutation and sign relabelling cannot pin. Under sampled loadings
`sigma_slope_trend` and `ar1_slope_trend` are one value shared by the
factors, and `init_slope_trend` takes a spherical normal prior. The
model is then invariant under rotation, and the QR step identifies it
as it does `RW()`. `per_factor_coefficients = FALSE`.

Limitation: with the level scale fixed, the pure smooth trend
(`sigma_trend = 0`) lies at infinite `sigma_slope`. A factor LLT keeps
some level noise. A series LLT has no such limit.

With a fixed or partial `trend_map` the factors keep their scale, and
the slope parameters are per factor.

## Priors

Defaults scale with the number of time points `N` (the brms practice of
data-dependent defaults, printed resolved in `get_prior()`).

| Class | Default | Reason |
|---|---|---|
| `sigma_slope_trend` | `student_t(3, 0, 1.3 / sqrt(v))`, lb 0 | SD of the slope-driven movement over the series has prior median 1 link unit; the movement itself q50 0.55, q90 2.8, q99 8.4, at every N |
| `init_slope_trend` | `normal(0, 2 / r)` | 2 link units of growth over the series at one SD; `r = 1 + phi + ... + phi^(N-2)`, `N - 1` undamped |
| `ar1_slope_trend` | `beta(18, 2)` | median 0.91; 8% below 0.8, 5.5% above 0.98 |

`v = llt_excursion_var(N, phi)` sums the squared weights a slope shock
carries to the level at time N: `w_k = 1 + phi + ... + phi^(k-1)` for
the shocks at times 2 to N - 1. Undamped `v = sum k^2 ~ N^3/3`;
damped takes `phi = 0.913`, the beta(18, 2) median. Factors with
sampled loadings have unit level shocks and measure both priors in
units of the level's spread, `sqrt(N)`.

Revised 2026-10-04 after fitting. The first default,
`exponential(N^1.5 sqrt(3) / 2)` (95% of excursions below 2 units,
undamped calibration), dominated three fixtures: posterior
`sigma_slope` 0.002 against truths 0.02 to 0.03, `ar1_slope` at its
prior, `sigma_trend` inflated to absorb the slope. Under a wide user
prior the same fits recovered 0.021 to 0.032 (90% intervals covering
truth); LOO did not separate the two priors (-3.0 +- 2.1, -0.8 +- 1.3).
The undamped calibration was also about 5x too tight for a damped
slope, whose stationary excursion per unit shock is smaller.

Half-t, not inverse gamma: `sigma_slope = 0` is the random walk with
drift, a nested model the data should be able to reach. The heavy
tail lets an informative series move the posterior.

Both slope parameters are sampled in units of these prior scales:
`sigma_slope_trend = slope_scale_trend * sigma_slope_raw_trend` (a fixed
linear map, no Jacobian) and `init_slope_trend` with Stan's
`multiplier`. Sampled on their natural scale, a random start in
(-2, 2) put `sigma_slope` near 7, which integrated twice over 60
steps sent a Poisson predictor to exp(200): a factor fixture's chains
stayed at lp__ near -1e98 throughout.

Geometry, 2026-10-04, on the damped growth fixture with a free start:
about 1.5% divergences under the default `adapt_delta = 0.8` (23 to 35
in 2000 draws over three seeds), 4 to 12 at 0.95, with the same
estimates at every seed. They sit at larger `sigma_slope`, not near
zero: the slope shocks reach the level twice integrated. A centred
level, tried by hand on the same data, fell into the `sigma_trend -> 0`
funnel (497 divergences in one chain, R-hat 1.59). Non-centred stays.
Inverse gamma on `sigma_trend` removed none of the divergences.

Open: a Gaussian series with informative data and a wide slope scale
hit maximum treedepth on 96% of transitions (non-centred level and
slope shocks). That fit was undamped, which the start change leaves
alone.

## Identifiability notes

- A linear or smooth time term in either formula is confounded with
  the slope. LLT warns when the time column appears in a formula.
- `sigma_slope` is the spectral density of the second differences at
  zero frequency, weakly identified on short series. Warn below 20
  time points; refuse below 5.

## Constructor and registry

    LLT(time = NA, series = NA, damped = FALSE, cor = NULL, gr = NA,
        subgr = NA, n_lv = NULL, trend_map = NULL, df = Inf)

`supports_factors = TRUE`, `per_factor_coefficients = FALSE`,
`covariance_pattern = "cholesky_scaled"`, `stationary_source = "none"`,
`requires_regular_intervals = TRUE`, `completes_time_grid = FALSE`,
`shared_innovations = TRUE`.

## Forecast

The stacked state `(level, slope)` is a VAR(1) with
`A = [[I, I], [0, diag(phi)]]`. `propagate_llt()` draws the level block
with `df` and the slope block Gaussian, separately, then runs
`trend_arma_recursC()` on the 2K state. With no fitted state (the
simulation path) the level starts at 0 and the slope at its prior
start, never a burn-in. Closed forms for the exact check: mean
`level_T + slope_T (1 + phi + ... + phi^(h-1))`; slope variance
contribution `sigma_slope^2 sum_{i=1}^{h-1} ((1 - phi^(h-i)) / (1 - phi))^2`.

## Dispatch (debt removed on the way)

- Registry property `shared_innovations` replaces the name lists in
  `generate_trend_specific_stanvars()`,
  `generate_hierarchical_correlation_parameters()` and
  `mgp_capable_trends`.
- `trend_method(type, template)` finds a trend's function by name for
  each task in `trend_method_templates`, each task with one signature.
  `register_core_trends()` requires all of them. Replaces the
  `switch()` blocks in `generate_monitor_params()`,
  `extract_last_state()`, `propagate_trend()` and the write-up.

## Verification

Exact recomputation of `lv_trend` and `slope_trend` from raw draws;
forecast moments against the closed forms; cell mapping. Fixtures:
Gaussian one series undamped, Poisson three series with `cor`, damped
negative binomial, a factor model, `gr`, and an `N = 200` Poisson fit
reporting treedepth and the ESS of `sigma_slope_trend`.
