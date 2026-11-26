# Local linear trends, with optional damping and seasons

A latent level that moves with a latent growth rate, the local linear
trend of Harvey (1989). Each latent process carries a level and a slope:

\$\$\mu_t = \mu\_{t-1} + \beta\_{t-1} + \eta_t\$\$ \$\$\beta_t = \phi
\beta\_{t-1} + \zeta_t\$\$

The level innovations \\\eta_t\\ have scale `sigma_trend` and the slope
innovations \\\zeta_t\\ scale `sigma_slope_trend`. A small
`sigma_slope_trend` gives a trend that keeps its direction. A zero scale
gives a random walk with a fixed drift. `damped = TRUE` estimates
\\\phi\\ on (0, 1) as `ar1_slope_trend`, which pulls the growth rate
back toward zero over the horizon (Gardner and McKenzie 1985). The
undamped trend has \\\phi = 1\\. Its forecasts extend the current growth
rate.

## Usage

``` r
LLT(
  time = NA,
  series = NA,
  damped = FALSE,
  cor = NULL,
  gr = NA,
  subgr = NA,
  n_lv = NULL,
  trend_map = NULL,
  df = Inf,
  season = NULL,
  harmonics = NULL,
  centred = FALSE
)
```

## Arguments

- time:

  The unquoted name of the `numeric` or `integer` variable in `data`
  that holds each row's time. Defaults to `time`.

- series:

  The unquoted name of the `factor` variable in `data` that names each
  row's series. Its levels fix the order of the series. A trend needs
  this column even for a single series. Defaults to `series`.

- damped:

  Logical. Estimate a damping coefficient for the slope? Default
  `FALSE`.

- cor:

  `Logical`. Correlate the innovations of different series? `TRUE`
  estimates a full covariance matrix of the innovations when the data
  hold more than one series.
  [`AR()`](https://nicholasjclark.github.io/mvgam/reference/trend_constructors.md)
  and
  [`RW()`](https://nicholasjclark.github.io/mvgam/reference/trend_constructors.md)
  default to `NULL`. That means `FALSE` for an ungrouped trend and
  `TRUE` when `gr` names a grouping variable.
  [`VAR()`](https://nicholasjclark.github.io/mvgam/reference/trend_constructors.md),
  [`ZMVN()`](https://nicholasjclark.github.io/mvgam/reference/ZMVN.md)
  and grouped
  [`AR()`](https://nicholasjclark.github.io/mvgam/reference/trend_constructors.md)
  and
  [`RW()`](https://nicholasjclark.github.io/mvgam/reference/trend_constructors.md)
  trends always estimate correlations. They refuse `cor = FALSE`.
  [`CAR()`](https://nicholasjclark.github.io/mvgam/reference/trend_constructors.md)
  defaults to `FALSE`.

  In a factor model with every loading sampled, the factors take
  independent shocks. The loadings carry the correlation among series,
  which
  [`residual_cor()`](https://nicholasjclark.github.io/mvgam/reference/residual_cor.md)
  reports. A correlation among the factors would trade off against the
  loadings.
  [`AR()`](https://nicholasjclark.github.io/mvgam/reference/trend_constructors.md),
  [`RW()`](https://nicholasjclark.github.io/mvgam/reference/trend_constructors.md)
  and `LLT()` refuse `cor = TRUE` there. A `trend_map` that fixes the
  loadings keeps the factors apart. `cor = TRUE` then estimates their
  correlation.

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

  Within a
  [`VAR()`](https://nicholasjclark.github.io/mvgam/reference/trend_constructors.md)
  it sets up a hierarchical panel vector autoregression. Each group
  learns its autoregressive, moving-average and correlation matrices
  from shared hyperparameters. The groups do not interact.

  Two constraints apply to hierarchical trend models:

  - Each series belongs to one group.
    [`mvgam()`](https://nicholasjclark.github.io/mvgam/reference/mvgam.md)
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
  [`mvgam()`](https://nicholasjclark.github.io/mvgam/reference/mvgam.md)
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
  [`mvgam()`](https://nicholasjclark.github.io/mvgam/reference/mvgam.md)
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
  [`get_prior`](https://nicholasjclark.github.io/mvgam/reference/get_prior.md)
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

  [`VAR()`](https://nicholasjclark.github.io/mvgam/reference/trend_constructors.md)
  samples its states directly and
  [`PW()`](https://nicholasjclark.github.io/mvgam/reference/piecewise_trends.md)
  has no innovations. Neither takes `df`.

- season:

  Optional numeric vector of seasonal periods in time steps, each above
  2, such as `12` for monthly data or `c(7, 365.25)` for daily data.
  Periods need not be whole. Default `NULL` fits no season.

- harmonics:

  Optional integer vector with the number of trigonometric harmonics of
  each period, each below half its period. Fewer harmonics give a
  smoother seasonal shape. Defaults to the most each period allows, up
  to 10.

- centred:

  Logical. Sample the latent states directly rather than their
  innovations? Default `FALSE`. The model and its posterior are the
  same. See *Centred states* below.

## Value

An object of class `mvgam_trend`.

## Details

The level shocks take the innovation structure of
[`RW()`](https://nicholasjclark.github.io/mvgam/reference/trend_constructors.md):
`cor` correlates them across series, `gr` and `subgr` pool their
correlations hierarchically and `df` gives them a multivariate t. The
slope and seasonal shocks are Gaussian and take the level's correlation
matrix with scales of their own.
[`residual_cor()`](https://nicholasjclark.github.io/mvgam/reference/residual_cor.md)
reports that one matrix.

The slope starts at a free value, `init_slope_trend`, damped or not, as
in the damped trends of exponential smoothing. Damping serves a series
that starts with fast growth and slows. The initial slope of such a
series can lie far outside the slope's stationary distribution. The
level starts at its first shock. The observation intercept carries its
mean.

Default priors scale with the number of time points \\N\\ and with the
factor \\s\\ that brms uses to widen its own default scales, as
[`get_prior()`](https://nicholasjclark.github.io/mvgam/reference/get_prior.md)
describes:

- `sigma_trend` and `sigma_slope_trend` take `gamma(2, .)` priors. At
  its prior median, each kind of shock alone moves the trend by a
  standard deviation of \\s\\ over the series. The slope prior allows
  for damping at the prior median of `ar1_slope_trend`. A `gamma(2, .)`
  density is zero at zero. It allows a nearly fixed level or slope and
  keeps the sampler off that boundary.

- `init_slope_trend` takes `normal(0, 2 / r)`. Here \\r = 1 + \phi +
  \dots + \phi^{N - 2}\\ is how far a unit initial slope moves the level
  by the end of the series. It is \\N - 1\\ without damping. One prior
  standard deviation then gives growth of \\2 s\\ over the series.

- `ar1_slope_trend` takes `beta(18, 2)`, with median 0.91.

A factor model with every loading sampled measures these scales in units
of \\\sqrt{N}\\, the spread of its level over the series. Its loadings
carry \\s\\.
[`get_prior()`](https://nicholasjclark.github.io/mvgam/reference/get_prior.md)
prints the resolved values.

A smooth or linear function of time in either formula competes with the
slope for the same variation.
[`mvgam()`](https://nicholasjclark.github.io/mvgam/reference/mvgam.md)
warns when the time variable appears in one. The slope variance is
weakly identified on short series: `LLT()` warns below 20 time points
and refuses fewer than 5. On informative data, such as large counts, a
chain started at random can stall far from the posterior.
`init = "pathfinder"` in
[`mvgam()`](https://nicholasjclark.github.io/mvgam/reference/mvgam.md)
starts every chain from Stan's Pathfinder approximation.

In a factor model with every loading sampled, the loadings set each
factor's scale. The level shocks take a fixed scale of one. Every factor
keeps its level noise. The factors share one `sigma_slope_trend` and one
`ar1_slope_trend`. A rotation of the factors then leaves the model
unchanged. The QR rotation of
[`RW()`](https://nicholasjclark.github.io/mvgam/reference/trend_constructors.md)
factors identifies the loadings. A `trend_map` that fixes loadings keeps
one slope scale per factor. The factors' shocks are independent, as
`cor` describes.

*Seasonality*: `season` adds a stochastic season \\\gamma_t\\ to the
level, the trigonometric form of Harvey (1989) with the non-integer and
multiple periods of De Livera et al. (2011). The latent state is
\\\mu_t + \gamma_t\\. Harmonic \\j\\ of period \\m\\ holds a state pair
that turns by \\\lambda_j = 2 \pi j / m\\ at each step and takes
Gaussian shocks of scale `sigma_season_trend`:

\$\$\gamma\_{j,t} = \cos\lambda_j \gamma\_{j,t-1} + \sin\lambda_j
\gamma^\*\_{j,t-1} + \omega\_{j,t}\$\$ \$\$\gamma^\*\_{j,t} =
\cos\lambda_j \gamma^\*\_{j,t-1} - \sin\lambda_j \gamma\_{j,t-1} +
\omega^\*\_{j,t}\$\$

Common periods are 4 for quarterly data, 12 for monthly data, 52.18 for
weekly data over years, 7 and 365.25 for daily data and 24 and 168 for
hourly data. `LLT(season = 12, harmonics = K)` fits the season that
`AddTrig(period = 12, frequencies = 1:K)` specifies in bsts.

The season is the sum of the first states over the harmonics. A small
`sigma_season_trend` keeps the seasonal shape nearly fixed. A zero scale
fixes it, as a cyclic smooth does. A larger scale lets the shape drift.
Every harmonic of a process shares one scale.

Each harmonic averages to zero over its period, which separates the
season from the level and the observation intercept. Two periods sharing
a harmonic, such as 6 and 12 with three or more harmonics, are refused.
A period longer than the data overlaps the slope and draws a warning.
The starting pairs `init_season_trend` are estimated under one prior for
both states of every harmonic. That prior treats every phase at the
first time point alike. Under `cor` or `gr` each harmonic's shocks are
correlated across series, as the level shocks are. Series correlated in
their shocks shift their seasons together. In a factor model each factor
has its own season, mapped to the series through the loadings.

`sigma_season_trend` takes a `gamma(2, .)` prior whose median puts the
season's drift over the series at \\s / 4\\. A season that drifts by its
own size over the series would barely repeat. Each starting state takes
`normal(0, s / sqrt(K))` over \\K\\ harmonics, which gives the starting
seasonal pattern a root mean square of \\s\\.
[`get_prior()`](https://nicholasjclark.github.io/mvgam/reference/get_prior.md)
prints the resolved values.

*Components*:
[`hindcast.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/hindcast.mvgam.md),
[`forecast.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/forecast.mvgam.md)
and `plot(type = "trend")` report the parts of the trend through
`component`. `"level"` is the stochastic level and `"slope"` its growth
per time step. `"season"` is the season. `"predictor"` holds the effects
of the trend formula's covariates. Several names add their components:
`component = c("level", "predictor")` gives the level with the covariate
effects it moves with. Level, season and predictor sum to the trend. A
factor model reports each component at the series grain, through its
loadings. A rotation of the factors leaves them unchanged.
[`seasonality()`](https://nicholasjclark.github.io/mvgam/reference/seasonality.md)
summarises the swing and peak of the season in each cycle. It also
reports how both changed over the series.

## References

Harvey, A. C. (1989). *Forecasting, Structural Time Series Models and
the Kalman Filter*. Cambridge University Press.

Gardner, E. S. and McKenzie, E. (1985). Forecasting trends in time
series. *Management Science*, 31:1237-1246.

De Livera, A. M., Hyndman, R. J. and Snyder, R. D. (2011). Forecasting
time series with complex seasonal patterns using exponential smoothing.
*Journal of the American Statistical Association*, 106:1513-1527.

## See also

[`RW()`](https://nicholasjclark.github.io/mvgam/reference/trend_constructors.md),
[`PW()`](https://nicholasjclark.github.io/mvgam/reference/piecewise_trends.md),
[trend_constructors](https://nicholasjclark.github.io/mvgam/reference/trend_constructors.md),
[`get_prior()`](https://nicholasjclark.github.io/mvgam/reference/get_prior.md)
for the scale of the default priors,
[`hindcast.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/hindcast.mvgam.md)
and
[`forecast.mvgam()`](https://nicholasjclark.github.io/mvgam/reference/forecast.mvgam.md)
for the level, the slope and the season,
[`seasonality()`](https://nicholasjclark.github.io/mvgam/reference/seasonality.md)
for the swing and peak of each seasonal cycle,
[`series_to_mvgam()`](https://nicholasjclark.github.io/mvgam/reference/series_to_mvgam.md)
for the periods a `ts`, `msts` or `tsibble` carries

Other trend models:
[`PW()`](https://nicholasjclark.github.io/mvgam/reference/piecewise_trends.md),
[`ZMVN()`](https://nicholasjclark.github.io/mvgam/reference/ZMVN.md),
[`list_trend_types()`](https://nicholasjclark.github.io/mvgam/reference/list_trend_types.md),
[`lv_axis()`](https://nicholasjclark.github.io/mvgam/reference/lv_axis.md),
[`print.mvgam_trend()`](https://nicholasjclark.github.io/mvgam/reference/trend_constructors.md)

## Examples

``` r
# \dontrun{
# A series that grows fast, then slows: its slope starts at 0.25
# and shrinks by a factor of 0.93 at each step
set.seed(2)
n_t <- 85
slope <- 0.25 * 0.93^(0:(n_t - 1))
dat <- data.frame(
  time = 1:n_t,
  series = factor("s1"),
  y = rpois(n_t, exp(1 + cumsum(slope) + cumsum(rnorm(n_t, 0, 0.03))))
)

# Fit the first 45 points, while the series is still growing
mod <- mvgam(
  y ~ 1,
  trend_formula = ~ LLT(damped = TRUE),
  data = dat[1:45, ],
  family = poisson(),
  chains = 2,
  silent = 2
)
#> Warning: 47 of 2000 (2.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.

# The posterior for `ar1_slope_trend` covers the simulated 0.93
summary(mod)
#>  Family: poisson 
#>   Links: mu = log 
#> Formula: y ~ 1 
#>    Data: dat[1:45, ] (Number of observations: 45) 
#>  Trends: LLT 
#>   Draws: 2 chains, each with iter = 2000; warmup = 1000; thin = 1; 
#>          total post-warmup draws = 2000
#> 
#> == Observation Model ==
#> Regression Coefficients:
#>           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept     1.46      0.28     0.90     1.99 1.00     1274      845
#> 
#> == Trend Model ==
#> Trend Specific Parameters:
#>                       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
#> sigma_trend[s1]           0.08      0.03     0.02     0.14 1.00      594
#> ar1_slope_trend[s1]       0.93      0.03     0.85     0.98 1.00     1004
#> init_slope_trend[s1]      0.21      0.07     0.08     0.35 1.00      926
#> sigma_slope_trend[s1]     0.01      0.01     0.00     0.03 1.00      796
#>                       Tail_ESS
#> sigma_trend[s1]            703
#> ar1_slope_trend[s1]        632
#> init_slope_trend[s1]       812
#> sigma_slope_trend[s1]      778
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

# The slope decays toward zero over the fitted times and the horizon
plot(forecast(mod, newdata = dat[46:85, ], type = "trend",
              component = "slope"))


# On the link scale the damped forecast levels off. An undamped slope
# carries the last growth rate forward.
plot(forecast(mod, newdata = dat[46:85, ], type = "link"))

mod_undamped <- update(mod, trend_formula = ~ LLT())
#> Warning: 37 of 2000 (2.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
plot(forecast(mod_undamped, newdata = dat[46:85, ], type = "link"))


# Ten years of monthly counts: a slow rise, a seasonal swing that
# widens from 0.3 to 0.9 on the log scale and a rainfall effect
set.seed(3)
n_t <- 120
season <- seq(0.3, 0.9, length.out = n_t) * sin(2 * pi * (1:n_t) / 12)
dat_s <- data.frame(time = 1:n_t, series = factor("s1"),
                    rain = rnorm(n_t))
dat_s$y <- rpois(n_t, exp(2.5 + 0.008 * dat_s$time + season +
                            0.3 * dat_s$rain))

# Two harmonics of a twelve-month season, with rainfall acting on the
# latent state. The last year is held out.
mod_s <- mvgam(
  y ~ 1,
  trend_formula = ~ rain + LLT(season = 12, harmonics = 2),
  data = dat_s[1:108, ],
  family = poisson(),
  chains = 2,
  silent = 2
)
#> Warning: 26 of 2000 (1.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.

# A small `sigma_season_trend` lets the seasonal shape drift slowly
summary(mod_s)
#>  Family: poisson 
#>   Links: mu = log 
#> Formula: y ~ 1 
#>    Data: dat_s[1:108, ] (Number of observations: 108) 
#>  Trends: LLT; formula: ~rain 
#>   Draws: 2 chains, each with iter = 2000; warmup = 1000; thin = 1; 
#>          total post-warmup draws = 2000
#> 
#> == Observation Model ==
#> Regression Coefficients:
#>           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept     2.42      0.11     2.19     2.63 1.00      864      983
#> 
#> == Trend Model ==
#> Regression Coefficients:
#>      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> rain     0.29      0.03     0.24     0.35 1.00     1771     1240
#> 
#> Trend Specific Parameters:
#>                         Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
#> sigma_trend[s1]             0.02      0.01     0.00     0.05 1.00      785
#> init_slope_trend[s1]        0.01      0.01     0.00     0.02 1.00      728
#> init_season_trend[1,s1]     0.07      0.10    -0.17     0.26 1.00     1031
#> init_season_trend[2,s1]    -0.03      0.10    -0.23     0.14 1.00      910
#> init_season_trend[3,s1]     0.26      0.11     0.02     0.46 1.00      640
#> init_season_trend[4,s1]     0.03      0.09    -0.16     0.22 1.01     1042
#> sigma_slope_trend[s1]       0.00      0.00     0.00     0.00 1.00      880
#> sigma_season_trend[s1]      0.02      0.01     0.01     0.04 1.00      566
#>                         Tail_ESS
#> sigma_trend[s1]             1087
#> init_slope_trend[s1]         760
#> init_season_trend[1,s1]     1283
#> init_season_trend[2,s1]      994
#> init_season_trend[3,s1]      893
#> init_season_trend[4,s1]     1372
#> sigma_slope_trend[s1]       1010
#> sigma_season_trend[s1]      1041
#> 
#> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).
#> 
#> Next steps:
#>   - `pp_check(fit)`: posterior predictive checks
#>   - `seasonality(fit)`: the season's swing and peak, cycle by cycle
#>   - `forecast(fit, newdata = ...)`: out-of-sample forecasts
#>   - `lfo_cv(fit)`: leave-future-out model comparison
#>   - `conditional_effects(fit)`: covariate effects
#> Use `how_to_cite(fit)` for a methods paragraph and `methods_md(fit)` for its equations.

# Simulated counts match the distribution of the observed ones
pp_check(mod_s, type = "ecdf_overlay")


# The rainfall effect on the expected count
conditional_effects(mod_s, type = "expected")


# The fitted season widens over the years and carries its shape into
# the held-out year
fc_s <- forecast(mod_s, newdata = dat_s[109:120, ], type = "trend",
                 component = "season")
plot(fc_s)


# The swing of each year's season and its peak month. Comparing the
# first and last years gives the widening and its probability
s <- seasonality(mod_s, newdata = dat_s[109:120, ])
s
#> Seasonal pattern of the latent trend (link scale), 2000 draws
#> First and last complete cycles. summary() gives every cycle.
#> 
#> Period 12 (season)
#>   s1
#>     amplitude 0.35 -> 0.73, P(larger) = 1.00
#>     peak 3 (0.45) -> 3 (0.78), P(earlier) = 0.38, P(later) = 0.20
#>     strength 0.46 -> 0.82
plot(s)

summary(s)$change
#> # A tibble: 1 × 13
#>   series period from_cycle to_cycle ratio_Q50 ratio_Q2.5 ratio_Q97.5 prob_larger
#>   <chr>  <chr>       <int>    <int>     <dbl>      <dbl>       <dbl>       <dbl>
#> 1 s1     season          1       10      2.13       1.30        4.36       0.998
#> # ℹ 5 more variables: shift_Q50 <dbl>, shift_Q2.5 <dbl>, shift_Q97.5 <dbl>,
#> #   prob_earlier <dbl>, prob_later <dbl>

# Draws of the seasonal shock scale
head(as.data.frame(mod_s, variable = "sigma_season_trend"))
#>   sigma_season_trend[s1] .chain .iteration .draw
#> 1             0.02414748      1          1     1
#> 2             0.02664308      1          2     2
#> 3             0.02803200      1          3     3
#> 4             0.02484199      1          4     4
#> 5             0.03152859      1          5     5
#> 6             0.02168667      1          6     6

# Two regions whose levels move together, with shocks correlated at
# 0.7. Counts near 20 pin the states down, which the centred form
# samples fastest
set.seed(4)
n_t <- 60
z <- rnorm(n_t)
shock <- 0.25 * cbind(z, 0.7 * z + sqrt(1 - 0.7^2) * rnorm(n_t))
dat_r <- data.frame(
  time = rep(1:n_t, 2),
  series = factor(rep(c("north", "south"), each = n_t))
)
dat_r$y <- rpois(2 * n_t, exp(3 + c(apply(shock, 2, cumsum)) +
                                0.5 * sin(2 * pi * dat_r$time / 12)))

# `cor = TRUE` correlates the level, slope and seasonal shocks
mod_r <- mvgam(
  y ~ 1,
  trend_formula = ~ LLT(season = 12, harmonics = 1, cor = TRUE,
                        centred = TRUE),
  data = dat_r,
  family = poisson(),
  chains = 2,
  silent = 2
)

# The one correlation matrix the three kinds of shock share
residual_cor(mod_r)
#> Residual correlations from an mvgam fit
#>   Source     : correlated innovations
#>   Covariance : stationary
#>   Series     : 2
#>   Mean |r_ij| (i != j): 0.65 [0.41, 0.82]
#>   Pairs with prob_nonzero > 0.95: 1 / 1
# }
```
