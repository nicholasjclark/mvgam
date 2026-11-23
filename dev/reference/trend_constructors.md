# Latent trend models for mvgam

Specify the latent trend of a state-space model. Each constructor goes
inside `trend_formula`, as in `trend_formula = ~ AR(p = 1)`. A
constructor records its arguments. The model evaluates them against the
data. The trends suit different series:

- `RW()`:

  A level that wanders with no pull back to a mean.

- `AR()`:

  Movement around a mean, with memory of the previous `p` times. It is a
  common first choice.

- `VAR()`:

  Series whose past values drive one another, such as predators and
  prey.

- `CAR()`:

  An AR(1) for series observed at irregular times.

- [`ZMVN()`](https://nicholasjclark.github.io/mvgam/dev/reference/ZMVN.md):

  Correlated effects with no dependence on time, such as among sites or
  species.

- [`LLT()`](https://nicholasjclark.github.io/mvgam/dev/reference/LLT.md):

  A level with a slope that changes over time, with an optional season.

- [`PW()`](https://nicholasjclark.github.io/mvgam/dev/reference/piecewise_trends.md):

  Linear or logistic growth with changepoints.

Each trend moves by a random innovation at every time step. Other texts
call it a process error or a shock. The innovations are independent
across series unless `cor = TRUE`.

## Usage

``` r
# S3 method for class 'mvgam_trend'
print(x, ...)

RW(
  time = NA,
  series = NA,
  ma = FALSE,
  cor = NULL,
  gr = NA,
  subgr = NA,
  n_lv = NULL,
  trend_map = NULL,
  df = Inf,
  centred = FALSE
)

AR(
  time = NA,
  series = NA,
  p = 1,
  ma = FALSE,
  cor = NULL,
  gr = NA,
  subgr = NA,
  n_lv = NULL,
  trend_map = NULL,
  coef_sharing = c("none", "shared", "hierarchical"),
  df = Inf,
  centred = FALSE
)

CAR(
  time = NA,
  series = NA,
  cor = FALSE,
  n_lv = NULL,
  trend_map = NULL,
  df = Inf,
  centred = FALSE
)

VAR(
  time = NA,
  series = NA,
  p = 1,
  ma = FALSE,
  cor = TRUE,
  gr = NA,
  subgr = NA,
  n_lv = NULL,
  trend_map = NULL
)
```

## Arguments

- x:

  An `mvgam_trend` object.

- ...:

  Unused.

- time:

  The unquoted name of the `numeric` or `integer` variable in `data`
  that holds each row's time. Defaults to `time`.

- series:

  The unquoted name of the `factor` variable in `data` that names each
  row's series. Its levels fix the order of the series. A trend needs
  this column even for a single series. Defaults to `series`.

- ma:

  `Logical`. Include moving average terms of order `1`? Default is
  `FALSE`.

- cor:

  `Logical`. Correlate the innovations of different series? `TRUE`
  estimates a full covariance matrix of the innovations when the data
  hold more than one series. `AR()` and `RW()` default to `NULL`. That
  means `FALSE` for an ungrouped trend and `TRUE` when `gr` names a
  grouping variable. `VAR()`,
  [`ZMVN()`](https://nicholasjclark.github.io/mvgam/dev/reference/ZMVN.md)
  and grouped `AR()` and `RW()` trends always estimate correlations.
  They refuse `cor = FALSE`. `CAR()` defaults to `FALSE`.

  In a factor model with every loading sampled, the factors take
  independent shocks. The loadings carry the correlation among series,
  which
  [`residual_cor()`](https://nicholasjclark.github.io/mvgam/dev/reference/residual_cor.md)
  reports. A correlation among the factors would trade off against the
  loadings. `AR()`, `RW()` and
  [`LLT()`](https://nicholasjclark.github.io/mvgam/dev/reference/LLT.md)
  refuse `cor = TRUE` there. A `trend_map` that fixes the loadings keeps
  the factors apart. `cor = TRUE` then estimates their correlation.

- gr:

  An optional `factor` in `data` that groups the series, such as region.
  Each level of `gr` takes its own correlation matrix among the
  innovations of its series. The matrices are pooled toward one global
  matrix:

  \\\Omega\_{group} = \alpha\_{cor}\Omega\_{global} + (1 -
  \alpha\_{cor})\Omega\_{group, local}\\

  Here \\\Omega\_{global}\\ is shared by every group, \\\Omega\_{group,
  local}\\ is the deviation of one group and \\\alpha\_{cor}\\ sets the
  weight on the global matrix. An \\\alpha\_{cor}\\ near 1 pools the
  groups strongly. Without `subgr`, the series column names the series
  within each group.

  Within a `VAR()` it sets up a hierarchical panel vector
  autoregression. Each group learns its autoregressive, moving-average
  and correlation matrices from shared hyperparameters. The groups do
  not interact.

  Two constraints apply to hierarchical trend models:

  - Each series belongs to one group.
    [`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md)
    refuses a `gr` that varies within a series.

  - Every group must hold the same number of series. The Stan program
    sizes each group's correlation block from one shared count.
    Validation refuses unbalanced groups before fitting.

- subgr:

  A subgrouping `factor` variable naming the time series within each
  level of `gr`. Defaults to `series`. A model with hierarchical
  correlations builds its series from `gr` and `subgr`, and `data` then
  needs no `series` column.

  For example, to model counts of several species (`species` in `data`)
  across three regions (`region`) with innovations correlated within
  regions, specify `gr = region` and `subgr = species`.
  [`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md)
  then builds the series as

  `series = interaction(gr, subgr, drop = TRUE, sep = "_",`
  `lex.order = TRUE)`

  A region `r1` and a species `sp1` give the series `"r1_sp1"`. The
  levels sort lexically.
  [`summary()`](https://rdrr.io/r/base/summary.html),
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) and
  [`forecast()`](https://generics.r-lib.org/reference/forecast.html) all
  label that series `"r1_sp1"`. A `series` column that disagrees with
  the derived series is replaced by it, with a warning.

- n_lv:

  The number of latent factors to estimate for dynamic factor models.
  When `n_lv` is smaller than the number of series, the latent processes
  are modelled as `n_lv` factors with estimated loadings onto the
  series. Defaults to `NULL`, in which case one latent process is used
  per series.

- trend_map:

  Optional map of the series onto the latent processes, which fixes some
  or all loadings of a dynamic factor model. One of three forms:

  - a numeric `n_series x n_lv` matrix of loadings. `NA` entries are
    sampled and finite entries stay fixed. An all-`NA` matrix, such as
    `matrix(NA, n_series, 2)`, fixes nothing and sets two factors.

  - a `data.frame(series, trend)` placing each series on one process,
    with a loading of one.

  - `"identity"`, one process per series, or `"shared"`, one process for
    every series.

  Its columns set the number of factors, which an `n_lv` given too must
  match. Without a `trend_map` every loading is sampled and identified
  after sampling by a thin QR rotation (Heaps and Jermyn 2024). A map
  that fixes any loading takes no rotation. The `loadings_prior` of
  [`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md)
  accepts a map with no fixed entries. Defaults to `NULL`.

- df:

  Degrees of freedom for the latent process innovations. Defaults to
  `Inf`, which gives Gaussian innovations: a t with infinite degrees of
  freedom is a normal. Set `df = NA` to estimate them, or supply a
  number above `2` to fix them.

  Finite degrees of freedom let the latent process absorb an occasional
  large shock while `sigma_trend` stays at the scale of ordinary
  variation. That suits boom-and-bust population series and outbreak
  dynamics. The innovations follow a multivariate t. Because it shares
  one innovation *scale* across the series at each time point, large
  innovations tend to occur together. Each series keeps its own
  direction and magnitude. A shock in one series says nothing about the
  sign of the others. One series can also stay unmoved by an event that
  moves its neighbours. `cor` sets the correlation between series
  exactly as it does for Gaussian innovations. The degrees of freedom
  set how heavy the tails of the innovations are.

  When `df = NA` the degrees of freedom are estimated as `nu_trend`,
  with a default `gamma(4, 0.3)` prior. The `prior` argument replaces
  it, for example `prior(gamma(2, 0.1), class = "nu_trend")`.
  [`get_prior`](https://nicholasjclark.github.io/mvgam/dev/reference/get_prior.md)
  lists `nu_trend` with the other trend classes.

  Three caveats apply:

  - A fixed value must exceed `2`. Below it the innovations have no
    finite variance. An autoregressive trend then has no stationary
    covariance to start from.

  - The first states of an autoregressive trend take the stationary mean
    and covariance. Under t innovations the true stationary distribution
    has heavier tails than that.

  - Only the tails of the latent process inform the degrees of freedom.
    Below roughly 200 time points their posterior stays close to the
    prior.

  `VAR()` samples its states directly and
  [`PW()`](https://nicholasjclark.github.io/mvgam/dev/reference/piecewise_trends.md)
  has no innovations. Neither takes `df`.

- centred:

  Logical. Sample the latent states directly rather than their
  innovations? Default `FALSE`. The model and its posterior are the
  same. See *Centred states* below.

- p:

  The autoregressive lags. The accepted form depends on the trend type:

  - For `AR()` models: a positive integer or a positive integer vector.
    A scalar `p = k` is the textbook AR(k) and expands to the
    consecutive lags `1:k`. The fit then estimates `ar1_trend` through
    `ark_trend`. A vector `p = c(...)` selects a sparse lag set.
    `p = c(1, 12)` declares only `ar1_trend` and `ar12_trend`, a
    seasonal AR with no intermediate lags. `p = c(2, 4)` declares only
    `ar2_trend` and `ar4_trend`.

  - For `VAR()` models: a positive integer. `p = k` is the VAR(k), with
    one coefficient matrix for each of the lags `1:k`. The stationarity
    prior of Heaps (2023) needs consecutive lags. A sparse lag vector is
    not supported. Use `AR(p = c(...))` for sparse lags.

- coef_sharing:

  How the coefficients of `AR()` vary across the latent series. `"none"`
  (the default) estimates each series' coefficients independently.
  `"shared"` estimates one coefficient per lag for every series.
  `"hierarchical"` pools each series' coefficient toward a mean for its
  lag, as
  `ar{lag}_trend[j] ~ normal(mu_ar{lag}_trend, sigma_ar{lag}_trend)`.
  The section on AR coefficient sharing lists the parameters each
  setting samples.

## Value

An object of class `mvgam_trend` holding the constructor's arguments.
[`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md)
builds the trend from it. [`print()`](https://rdrr.io/r/base/print.html)
shows the label a fit would print, such as `AR({1, 12})`, with the
columns and the settings that differ from their defaults.

## Details

A `trend_formula` takes one trend constructor. The arguments of that
constructor express richer dynamics:

- For seasonal patterns: `AR(p = c(1, 12))`. A sum such as
  `RW() + AR(p = 12)` is refused

- For several time scales: `AR(p = c(1, 7, 30))` for daily, weekly and
  monthly lags

- For cross-series dynamics: `VAR(p = 2)`

**Centred states**: by default the sampler draws standard innovations
and builds the states from them. This suits data that say little about
each state, such as small counts, presence-absence or short series. When
the data pin the states down, as large counts do, every innovation
constrains every later state. The sampler then takes long trajectories
and can hit the maximum tree depth. `centred = TRUE` samples each state
as its distance from the first and recovers the innovations from them.
On large counts it ran two to five times faster per effective draw. On
small counts it ran ten to a hundred times slower.
[`ZMVN()`](https://nicholasjclark.github.io/mvgam/dev/reference/ZMVN.md)
factors with sampled loadings draw their states directly either way.
`VAR()` always does.

**Stationarity of `AR(p)`**: a consecutive lag set with `p >= 2` is
sampled as partial autocorrelations, `ar1_pacf_trend` through
`ark_pacf_trend`, each declared on `(-1, 1)`. The Levinson-Durbin
recursion derives `ar1_trend` through `ark_trend` from them. Every
summary, forecast and hypothesis test uses those coefficients. Any
vector of partial autocorrelations in `(-1, 1)` gives a stationary
process. Bounding the coefficients directly admits explosive draws. With
`ar1_trend` and `ar2_trend` both at 0.9 the declared bounds hold, yet
that process grows without limit. Priors for these models name the
`ar{k}_pacf_trend` classes.

`AR(p = 1)` samples `ar1_trend` directly. At one lag the declared
interval is already the stationary region. A sparse lag set such as
`AR(p = c(1, 12))` also samples `ar{k}_trend` directly. It fixes its
intermediate coefficients at zero, a constraint the recursion cannot
express. It carries no stationarity guarantee. Both take priors on the
`ar{k}_trend` classes.

`CAR()` steps a continuous-time AR(1) over the gaps the data records. A
gap `dt` is measured in units of the median gap between two consecutive
observations of one series. `ar1_trend` and `sigma_trend` then describe
a typical step. The model is the same whatever unit the time column
records. Across a gap `dt` the decay is `ar1_trend^dt` and the
innovation standard deviation is
`sigma_trend * sqrt((1 - ar1_trend^(2 dt)) / (1 - ar1_trend^2))`. The
decay and the innovation scale together. The marginal variance is then
`sigma_trend^2 / (1 - ar1_trend^2)` at every occasion of an irregular
grid. The first state comes from that marginal.

`CAR(cor = TRUE)` correlates the innovations of several series.
`Sigma_trend` is their covariance over a gap of one. Series `a` and `b`
have stationary covariance
`Sigma_trend[a, b] / (1 - ar1_trend[a] * ar1_trend[b])`. The innovations
across a gap `dt` have that covariance times
`1 - (ar1_trend[a] * ar1_trend[b])^dt`. This is the exact transition of
the continuous-time process. A time point added between two others
leaves the model for the remaining times unchanged. `L_Omega_trend` is
the Cholesky factor of the correlation between the series' shocks over
an instant. Its prior is placed there. The correlation of `Sigma_trend`
is closer to zero than that correlation when the series damp at
different rates. On a regular grid the likelihood is that of
`AR(p = 1, cor = TRUE)` with positive coefficients.

The series of a `CAR()` trend may be observed at their own times. The
trend has a state for every series at each time any series was observed.
A series contributes to the likelihood at the times it has a row. This
is the model a frame gives when every unobserved time of a series has a
row with a missing response.
[`forecast()`](https://generics.r-lib.org/reference/forecast.html) takes
each series at its own future times. `df` needs the series on shared
times. With one mixing scale per time, a multivariate t would let the
times of one series change the trend of another.

## Note

**VAR fits and `init = 0`**: the stationary map of Heaps (2023) takes
matrix square roots by eigendecomposition. The gradient of an
eigendecomposition divides by differences between eigenvalues.
`init = 0` in the call to
[`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md)
sets every raw coefficient to zero and every innovation scale to one.
Each matrix the map decomposes is then the identity, with repeated
eigenvalues, and the first gradient is not finite. Leave `init` at its
default random draws for `VAR()` fits. `AR()`, `RW()`, `CAR()` and
[`ZMVN()`](https://nicholasjclark.github.io/mvgam/dev/reference/ZMVN.md)
are unaffected.

## AR coefficient sharing

Each `coef_sharing` setting samples its own Stan parameters:

|                  |                                                       |
|------------------|-------------------------------------------------------|
| **coef_sharing** | **Stan parameters with priors**                       |
| `"none"`         | `ar{lag}{stem}_trend` per series                      |
| `"shared"`       | `shared_ar{lag}{stem}_trend` (one per lag)            |
| `"hierarchical"` | `mu_ar{lag}{stem}_trend`, `sigma_ar{lag}{stem}_trend` |

`{stem}` is `_pacf` for a contiguous lag set of two or more lags, where
the sampled quantity is a partial autocorrelation, and empty for `p = 1`
and for a sparse lag set such as `p = c(1, 3)`. `AR(p = 2)` samples
`ar1_pacf_trend` and `AR(p = 1)` samples `ar1_trend`. Every setting
defines the same `ar{lag}_trend[j]` in `transformed parameters`.
Forecasting, IRF, FEVD and summaries use that quantity whatever the
setting. A prior on any sampled parameter is set with
`brms::set_prior(class = "<name>")`. `get_prior(mvgam_formula(...))`
lists the parameters the chosen `coef_sharing` samples.

## Parameter Naming Convention

Every trend parameter carries a `_trend` suffix, which keeps it distinct
from an observation model parameter of the same name. The innovation
scale is `sigma_trend`, the moving-average coefficient `theta1_trend`,
the autoregressive coefficients `ar1_trend` to `ark_trend` and the VAR
coefficient matrices `Phi_trend`. Priors take these class names, as in
`prior(inv_gamma(5, 1), class = sigma_trend)`. Parameter extraction
takes the same names.

## Identification

A factor model with sampled loadings fits equally well under any
reordering or sign change of its factors. Some models also fit equally
well under any rotation. mvgam reports one version, chosen by the
dynamics of the factors:

- `AR()` factors take their own coefficients, as do `RW()` and `AR()`
  factors with `ma = TRUE`. A rotation would mix those coefficients.
  After sampling, the factors of every draw are reordered and signed to
  agree with one reference. The loadings `Z`, the factor paths
  `lv_trend` and the coefficients `ar1_trend` or `theta1_trend` move
  together. Factors are numbered by the variance they explain, largest
  first.

- `RW()`, `VAR()` and
  [`ZMVN()`](https://nicholasjclark.github.io/mvgam/dev/reference/ZMVN.md)
  factors without a moving-average term are unchanged by rotation. So
  are `AR()` factors with `coef_sharing = "shared"` and no
  moving-average term. Following Heaps & Jermyn (2024), generated
  quantities rotate each draw to lower-triangular loadings `Z_tilde`
  with a non-negative diagonal. The factor paths rotate to
  `lv_trend_tilde`. A VAR factor model rotates its coefficients to
  `Phi_trend_tilde`.

Post-fit methods use the identified loadings and factor paths.
[`shared_variation()`](https://nicholasjclark.github.io/mvgam/dev/reference/shared_variation.md)
and
[`residual_cor()`](https://nicholasjclark.github.io/mvgam/dev/reference/residual_cor.md)
are the same under every version.

A `trend_map` keeps its fixed entries on `Z` exactly. A `by = lv_axis()`
smooth ties each factor to its own covariate effect. Neither takes a
rotation. For a model with every loading sampled, the `loadings_prior`
argument of
[`mvgam()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md)
replaces the default iid Student-t prior on `Z` with a structured
matrix-normal prior. That prior is built from per-series features,
pairwise distance matrices or both. See
[`mvgam`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam.md)
for its arguments.

Setting `loadings_prior = "mgp"` (or
`loadings_prior = list(column_shrinkage = "mgp")`) switches the column
scaling to the multiplicative gamma process prior of Bhattacharya &
Dunson (2011), which shrinks later columns of `Z` toward zero with
increasing strength. `n_lv` is then a truncation ceiling on the factor
count.
[`active_factors`](https://nicholasjclark.github.io/mvgam/dev/reference/active_factors.md)
reports the posterior distribution of the number of active factors.

## References

Heaps, S. E. and Jermyn, I. H. (2024). Structured prior distributions
for the covariance matrix in latent factor models. *Statistics and
Computing*, 34:143.
[doi:10.1007/s11222-024-10454-0](https://doi.org/10.1007/s11222-024-10454-0)

Heaps, S. E. (2023). Enforcing stationarity through the prior in vector
autoregressions. *Journal of Computational and Graphical Statistics*,
32(1):74-83.
[doi:10.1080/10618600.2022.2079648](https://doi.org/10.1080/10618600.2022.2079648)

Clark NJ, Ernest SKM, Senyondo H, Simonis J, White EP, Yenni GM and
Karunarathna KANK (2025). Beyond single-species models: leveraging
multispecies forecasts to navigate the dynamics of ecological
predictability. PeerJ, 13, e18929. https://doi.org/10.7717/peerj.18929

## See also

[`get_prior()`](https://nicholasjclark.github.io/mvgam/dev/reference/get_prior.md)
lists the parameters a trend adds and their default priors.
[`residual_cor()`](https://nicholasjclark.github.io/mvgam/dev/reference/residual_cor.md),
[`irf()`](https://nicholasjclark.github.io/mvgam/dev/reference/irf.mvgam.md),
[`fevd()`](https://nicholasjclark.github.io/mvgam/dev/reference/fevd.mvgam.md)
and
[`stability()`](https://nicholasjclark.github.io/mvgam/dev/reference/stability.mvgam.md)
interpret fitted latent dynamics.
[`forecast()`](https://generics.r-lib.org/reference/forecast.html)
propagates them forward. The [overview
vignette](https://nicholasjclark.github.io/mvgam/dev/articles/mvgam_overview.html)
compares the trend types. The [VAR
article](https://nicholasjclark.github.io/mvgam/dev/articles/var.html)
interprets a fitted `VAR()` with
[`irf()`](https://nicholasjclark.github.io/mvgam/dev/reference/irf.mvgam.md),
[`fevd()`](https://nicholasjclark.github.io/mvgam/dev/reference/fevd.mvgam.md)
and
[`stability()`](https://nicholasjclark.github.io/mvgam/dev/reference/stability.mvgam.md).
The [hierarchical VAR
article](https://nicholasjclark.github.io/mvgam/dev/articles/hierarchical_var.html)
fits `gr` and `subgr`. The [dynamic factor
vignette](https://nicholasjclark.github.io/mvgam/dev/articles/dfm.html)
fits `n_lv` and `trend_map`.

Other trend models:
[`LLT()`](https://nicholasjclark.github.io/mvgam/dev/reference/LLT.md),
[`PW()`](https://nicholasjclark.github.io/mvgam/dev/reference/piecewise_trends.md),
[`ZMVN()`](https://nicholasjclark.github.io/mvgam/dev/reference/ZMVN.md),
[`list_trend_types()`](https://nicholasjclark.github.io/mvgam/dev/reference/list_trend_types.md),
[`lv_axis()`](https://nicholasjclark.github.io/mvgam/dev/reference/lv_axis.md)

## Author

Nicholas J Clark

## Examples

``` r
# \dontrun{
# A constructor records a trend before any data are seen. Printing
# one shows the settings the model will use
AR(p = c(1, 12))
#> mvgam trend specification:
#>   Type: AR({1, 12})
#>   Time variable: time
#>   Series variable: series
VAR(p = 2, ma = TRUE)
#> mvgam trend specification:
#>   Type: VARMA(2, 1)
#>   Time variable: time
#>   Series variable: series
#>   Correlated innovations
RW(gr = region, subgr = species)
#> mvgam trend specification:
#>   Type: RW
#>   Time variable: time
#>   Series variable: series
#>   Correlated innovations
#>   Hierarchical grouping: region
#>   Subgroups: species

# A regularly spaced series usually starts with AR(p = 1). ?mvgam
# fits one. The rest of this example handles irregular sampling.

# Three Poisson series with a seasonal cycle, each surveyed at its
# own irregular times. sim_mvgam(type = 6L) draws the gaps between
# occasions. Every series keeps about 60 percent of the occasions
set.seed(11)
simdat <- sim_mvgam(type = 6L, family = poisson(), n_series = 3L,
                    n_timepoints = 80L)
keep_some <- function(d) d[runif(nrow(d)) < 0.6, ]
own_times <- keep_some(simdat$data_train)

# The parameters a trend adds, with their default priors, before
# any fitting
get_prior(
  mvgam_formula(y ~ s(season, bs = "cc", k = 6),
                trend_formula = ~ CAR(cor = TRUE)),
  data   = own_times,
  family = poisson()
)
#>                   prior         class                        coef group resp
#>  student_t(3, 1.6, 2.5)     Intercept                                       
#>    student_t(3, 0, 2.5)           sds                                       
#>    student_t(3, 0, 2.5)           sds s(season, bs = "cc", k = 6)           
#>      inv_gamma(3, 1.07)   sigma_trend                                       
#>          normal(0, 0.5)     ar1_trend                                       
#>    lkj_corr_cholesky(2) L_Omega_trend                                       
#>  dpar nlpar    lb    ub tag       source
#>                                  default
#>                 0                default
#>                 0           (vectorized)
#>                 0                default
#>             0.001 0.999          default
#>                                  default

# A continuous-time AR(1) steps each series over its own gaps
mod <- mvgam(
  y ~ s(season, bs = "cc", k = 6),
  trend_formula = ~ CAR(cor = TRUE),
  data          = own_times,
  family        = poisson(),
  chains        = 2,
  silent        = 2
)
#> Warning: 2 of 2000 (0.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
summary(mod)
#>  Family: poisson 
#>   Links: mu = log 
#> Formula: y ~ s(season, bs = "cc", k = 6) 
#>    Data: own_times (Number of observations: 109) 
#>  Series: 3 
#>  Trends: CAR 
#>   Draws: 2 chains, each with iter = 2000; warmup = 1000; thin = 1; 
#>          total post-warmup draws = 2000
#> 
#> == Observation Model ==
#> Smoothing Spline Hyperparameters:
#>                Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sds(sseason_1)     1.52      0.68     0.69     3.44 1.00      923     1338
#> 
#> Regression Coefficients:
#>           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept     1.58      0.09     1.40     1.74 1.00     1739     1588
#> 
#> == Trend Model ==
#> Trend Specific Parameters:
#>                       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
#> sigma_trend[series_1]     0.32      0.09     0.17     0.51 1.00      981
#> sigma_trend[series_2]     0.47      0.12     0.26     0.73 1.00     1097
#> sigma_trend[series_3]     0.57      0.12     0.38     0.83 1.00     1405
#> ar1_trend[series_1]       0.60      0.21     0.12     0.92 1.00      681
#> ar1_trend[series_2]       0.41      0.22     0.04     0.83 1.00      929
#> ar1_trend[series_3]       0.18      0.15     0.01     0.54 1.00     1621
#>                       Tail_ESS
#> sigma_trend[series_1]     1175
#> sigma_trend[series_2]     1416
#> sigma_trend[series_3]     1526
#> ar1_trend[series_1]       1144
#> ar1_trend[series_2]       1248
#> ar1_trend[series_3]       1219
#> 
#> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).
#> 
#> Next steps:
#>   - `pp_check(fit)`: posterior predictive checks
#>   - `residual_cor(fit)`: implied correlations among series
#>   - `forecast(fit, newdata = ...)`: out-of-sample forecasts
#>   - `lfo_cv(fit)`: leave-future-out model comparison
#>   - `conditional_effects(fit)`: covariate effects
#> Use `how_to_cite(fit)` for a methods paragraph and `methods_md(fit)` for its equations.

# Correlations among the series' latent states
residual_cor(mod)
#> Residual correlations from an mvgam fit
#>   Source     : correlated innovations
#>   Covariance : stationary
#>   Series     : 3
#>   Mean |r_ij| (i != j): 0.21 [0.06, 0.41]
#>   Pairs with prob_nonzero > 0.95: 0 / 3

# Residual checks. Each series has gaps of differing length. A
# semivariogram by time lag replaces the ACF
plot(mod, type = "residuals", series = 2)


# Each series is forecast at its own future times
plot(forecast(mod, newdata = keep_some(simdat$data_test)),
     series = 2)


# Counts near 150 pin each latent state down. Sampled centred, this
# random walk ran about five times faster per effective draw
n_t <- 60
big <- data.frame(time = 1:n_t, series = factor("s1"),
                  y = rpois(n_t, exp(5 + cumsum(rnorm(n_t, 0, 0.1)))))
mod_big <- mvgam(
  y ~ 1,
  trend_formula = ~ RW(centred = TRUE),
  data          = big,
  family        = poisson(),
  chains        = 2,
  silent        = 2
)
summary(mod_big)
#>  Family: poisson 
#>   Links: mu = log 
#> Formula: y ~ 1 
#>    Data: big (Number of observations: 60) 
#>  Trends: RW 
#>   Draws: 2 chains, each with iter = 2000; warmup = 1000; thin = 1; 
#>          total post-warmup draws = 2000
#> 
#> == Observation Model ==
#> Regression Coefficients:
#>           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept     4.88      0.14     4.59     5.15 1.01      226      487
#> 
#> == Trend Model ==
#> Trend Specific Parameters:
#>                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sigma_trend[s1]     0.12      0.02     0.09     0.16 1.00      537      858
#> 
#> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).
#> 
#> Next steps:
#>   - `pp_check(fit)`: posterior predictive checks
#>   - `forecast(fit, newdata = ...)`: out-of-sample forecasts
#>   - `lfo_cv(fit)`: leave-future-out model comparison
#> Use `how_to_cite(fit)` for a methods paragraph and `methods_md(fit)` for its equations.

# Next: hindcast() returns the fitted latent trends. The articles
# under 'See Also' fit VAR, hierarchical and factor trends
# }
```
