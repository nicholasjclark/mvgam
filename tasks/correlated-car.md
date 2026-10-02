# Correlated CAR for several unevenly spaced series

## Problem

`CAR()` steps a continuous-time AR(1) over the gaps the data records.
With several series it has two limits:

1. Innovations are independent. `CAR()` hard-codes `cor = FALSE` and
   registers `covariance_pattern = "diagonal"`.
2. Every series needs a row at every time.
   `refuse_ragged_trend_grid()` refuses a frame where one series lacks
   a time another has, and the user pads with `NA` responses by hand.

The work has two stages. Stage 1 adds `CAR(cor = TRUE)` on the shared
grid. Stage 2 lets each series carry its own times.

## Model

`K` series, damping `phi_j = ar1_trend[j]` in (0, 1), rate
`lambda_j = -log(phi_j)`, unit-gap innovation covariance

    Sigma = (diag(sigma_trend) * Omega * diag(sigma_trend)) .* C
    C[a, b] = g_a g_b (1 - phi_a phi_b) / (lambda_a + lambda_b)
    g_j = sqrt(2 lambda_j / (1 - phi_j^2))

with `Omega = L_Omega_trend * L_Omega_trend'` the correlation of the
diffusion (shocks over an instant) and `C` one on the diagonal.
`sigma_trend` stays the unit-gap innovation scale of each series.

An LKJ prior on the unit-gap innovation correlation directly was the
first design. It admits correlations no continuous-time process gives
when the `phi_j` differ: `phi = (0.9, 0.1)` with correlation 0.9 gives
`Q(0.5)` a negative eigenvalue. The diffusion form is positive definite
at every gap.

Across a gap `d`:

    x_i = phi^d .* x_{i-1} + e_i,    e_i ~ MVN(0, Q(d))
    Gamma[a, b] = Sigma[a, b] / (1 - phi_a * phi_b)
    Q(d)[a, b]  = Gamma[a, b] * (1 - (phi_a * phi_b)^d)
    x_1 ~ MVN(0, Gamma)

This is the exact discretisation of the Ornstein-Uhlenbeck process with
drift `diag(-log(phi))`, parameterised by what one unit of time does:
`Q(1) = Sigma`. Three properties follow.

- The diagonal of `Q(d)` is `sigma_j^2 (1 - phi_j^(2d)) / (1 - phi_j^2)`,
  the formula the program already uses. `cor = FALSE` is the special
  case `Omega = I`.
- `Gamma` is the covariance at every occasion of any grid.
- `Q(d1 + d2) = Phi^d2 Q(d1) Phi^d2 + Q(d2)` with `Phi = diag(phi)`.
  Inserting a time point between two others leaves the joint law of
  the remaining states unchanged. Stage 2 builds a union grid and
  relies on this: one series' sampling times do not alter another
  series' model.

A cheaper alternative scales each series by its own gap factor and
multiplies by `L_Omega_trend`. It holds the innovation correlation at
`Omega` for every gap, which fails the third property when the `phi_j`
differ, and it is not used.

On a regular unit grid the model equals `AR(p = 1, cor = TRUE)` with
positive coefficients and the joint stationary start.

## Stan program under `cor = TRUE`

Data, from `calculate_car_time_distances()`:

```stan
int<lower=1> N_gaps_trend;                       // unique gaps
vector<lower=0>[N_gaps_trend] gap_trend;
array[N_time_trend] int<lower=1> gap_index_trend;
```

Parameters: `ar1_trend`, `sigma_trend`, `L_Omega_trend`,
`innovations_trend` (`[N_time_trend, N_lv_trend]`, identity scale).

Transformed parameters:

```stan
cov_matrix[N_lv_trend] Sigma_trend = multiply_lower_tri_self_transpose(
  diag_pre_multiply(sigma_trend, L_Omega_trend))
  .* car_unit_coherence(ar1_trend);
{
  matrix[N_lv_trend, N_lv_trend] log_ar_cross = log(ar1_trend * ar1_trend');
  matrix[N_lv_trend, N_lv_trend] Gamma = Sigma_trend ./ -expm1(log_ar_cross);
  array[N_gaps_trend] matrix[N_lv_trend, N_lv_trend] L_Q;
  for (g in 1:N_gaps_trend) {
    L_Q[g] = cholesky_decompose(Gamma .* -expm1(gap_trend[g] * log_ar_cross));
  }
  lv_trend[1] = (cholesky_decompose(Gamma) * innovations_trend[1]')';
  for (i in 2:N_time_trend) {
    lv_trend[i] = (pow(ar1_trend, gap_trend[gap_index_trend[i]])
                     .* lv_trend[i - 1]'
                   + L_Q[gap_index_trend[i]] * innovations_trend[i]')';
  }
}
```

One `K x K` Cholesky per unique gap. A regular grid with missing
responses has one. `innovations_trend` keeps
`innovation_sampling_code()`, and `df` works unchanged: a linear map of
a multivariate t is a multivariate t with the mapped scale.

The `cor = FALSE` program does not change.

## Stage 1 changes

| File | Change |
|---|---|
| `R/trend_system.R` | `CAR()` takes `cor = FALSE`. `car_trend_properties()` registers `covariance_pattern = "cholesky_scaled"` and `stationary_source = "lift"`. Roxygen for `cor` and the CAR paragraph of `?trend_constructors`. |
| `R/stan_assembly.R` | `generate_car_trend_stanvars()` branches on `cor`. The `L_Omega_trend` and `Sigma_trend` declarations move from `generate_shared_innovation_stanvars()` into a helper both call. `calculate_car_time_distances()` returns the unique gaps and their index. |
| `R/extract_last_state.R` | `extract_car_state()` returns `Sigma` when the fit has correlations. |
| `R/trend_propagation.R` | `propagate_car()` with `Sigma` draws each step from `Q(d)`. Without it the `car1_recursC()` path stands. |
| `R/sim_mvgam.R` | Recipe 6 accepts `CAR(cor = TRUE)`. |
| `R/methods_md.R` | `render_latent_car()` writes the correlated law. |
| `architecture/architecture-decisions.md` | Sections 3 and 10. |

`generate_monitor_params()` already lists `L_Omega_trend` for a spec
with `cor = TRUE`, and `common_trend_priors` already holds its default.
`gr`, `subgr` and factors stay refused.

### Marginal surface

`CAR()` registers `stationary_source = "none"`, and the marginal
prediction surface then samples the unit-gap innovation covariance.
The Stan program starts the state at `sigma^2 / (1 - phi^2)` and holds
that variance at every occasion. `"lift"` gives the surface the same
law through `ar_stationary_factor()`, whose factor for one lag is
`1 / (1 - phi_a phi_b)`. This changes `posterior_predict()` and
`posterior_epred()` under `process_error = TRUE` for existing
independent CAR fits.

## Time unit (decided 2026-10-02)

`ar1_trend` is damping per unit of time, and its prior and its
(0.001, 0.999) bounds then depend on whether time is in days or years.
CAR measures gaps in units of the median gap of the fitted grid:
`gap = diff(time) / median(diff(time))`. `ar1_trend` and `sigma_trend`
are then the damping and innovation scale over a typical step, and a
regular grid gives gaps of one whatever the unit. The scale is stored
with the trend metadata, and `forecast()`, `propagate_car()` and the
write-up divide by the same value. This changes existing `CAR()` fits
whose median gap is not one (NEWS).

Times closer than `1e-6` median gaps merge into one occasion. This
replaces the absolute `1e-3` floor, which changed the process and broke
the composition identity.

## Stage 2: series on their own times

CAR builds the union of all series' times. A cell with no data row is a
latent state with no likelihood term.

- `df` is refused when the frame is ragged (decided 2026-10-02). The
  mixing scale of a multivariate t is drawn per occasion, and one
  series' times would then change another series' law. A padded shared
  grid keeps `df`.
- `residual_cor()` and the summary report the stationary correlation of
  the trends, which does not depend on the time unit.
- `lfo_cv()` reports the observation count per fold.
- Registry property `completes_time_grid`, `TRUE` for CAR.
  `extract_and_validate_trend_components()` skips
  `refuse_ragged_trend_grid()` for such a trend, and
  `trend_cell_frame()` completes the `(time, series)` grid. A CAR on
  several series refuses trend covariates, and the completed cells need
  none.
- `extract_time_series_dimensions()`: confirm the observation mapping
  arrays index the union grid, and record the grid end as each series'
  last time. Every series has a state there.
- `forecast()`: `compute_car_forecast_time()` requires every series to
  share the forecast times. It becomes the union of `newdata` times
  beyond the grid, stepped from the grid end, with each `newdata` row
  mapped to its cell.
- `lfo_cv()` and `kfold()` admit a ragged frame for a trend that
  completes its grid. A series absent at a fold's horizon scores
  nothing there.
- `mvgam_data()` and the series plots draw each series at its own
  occasions.
- The ragged-grid refusal loses its CAR hint.

## Tests

Without Stan, in existing files under `tests/testthat/`:

- `Q(d)` composition identity and long-run covariance of
  `propagate_car()` with `Sigma` (`test-trend-propagation.R`).
- `CAR(cor = TRUE)` declares `L_Omega_trend`, `Sigma_trend` and the gap
  data, and `CAR()` emits the program it emits today
  (`test-stancode-standata.R`).
- Registry, last-state and prior-table rows (`test-trend-registry.R`,
  `test-extract-last-state.R`, `test-priors.R`).

Fits, in `tests/local/test-trend-car-irregular.R`, cached:

1. Three Gaussian series on a shared irregular grid, known `Omega`.
2. Poisson with missing responses.
3. `df = NA`.
4. Regular grid, against `AR(cor = TRUE)`.
5. Fit 1 with extra all-`NA` times: same posterior.
6. Stage 2: three series on their own times, against the `NA`-padded
   frame.

Each fit runs through `summary()`, `residual_cor()`, `hindcast()`,
`forecast()`, `posterior_predict()`, `lfo_cv()`, `plot()` and
`conditional_effects()`.
