# mvgam

> **M**ulti**V**ariate (Dynamic) **G**eneralized **A**dditive **M**odels

[![R-CMD-check](https://github.com/nicholasjclark/mvgam/workflows/R-CMD-check/badge.svg)](https://github.com/nicholasjclark/mvgam/actions/)
[![Coverage
status](https://codecov.io/gh/nicholasjclark/mvgam/graph/badge.svg?token=RCJ2B7S0BL)](https://app.codecov.io/gh/nicholasjclark/mvgam)
[![Documentation](https://img.shields.io/badge/documentation-mvgam-orange.svg?colorB=brightgreen)](https://nicholasjclark.github.io/mvgam/)
[![Methods in Ecology &
Evolution](https://img.shields.io/badge/Methods%20in%20Ecology%20&%20Evolution-14,%20771%E2%80%93784-blue.svg)](https://doi.org/10.1111/2041-210X.13974)
[![CRAN
Version](https://www.r-pkg.org/badges/version/mvgam)](https://cran.r-project.org/package=mvgam)
[![CRAN
Downloads](https://cranlogs.r-pkg.org/badges/grand-total/mvgam?color=brightgreen)](https://cran.r-project.org/package=mvgam)

The `mvgam` 📦 fits Bayesian Dynamic Generalized Additive Models
(DGAMs). A DGAM combines nonlinear predictor effects, latent variables
and multivariate time series models in one state-space model. `mvgam`
builds each model from [`brms`](https://paulbuerkner.com/brms/) and
[`mgcv`](https://cran.r-project.org/package=mgcv) components and
estimates its parameters in [`Stan`](https://mc-stan.org/). The models
it fits include:

- [Multivariate state-space time series
  models](https://nicholasjclark.github.io/mvgam/reference/mvgam.html)
- [Vector
  autoregressions](https://nicholasjclark.github.io/mvgam/articles/var.html)
- [Continuous-time autoregressions for irregularly sampled
  series](https://nicholasjclark.github.io/mvgam/reference/trend_constructors.html)
- [Dynamic factor
  models](https://nicholasjclark.github.io/mvgam/articles/dfm.html)
- [Hierarchical occupancy
  models](https://nicholasjclark.github.io/mvgam/articles/occupancy.html)
- [Hierarchical N-mixture
  models](https://nicholasjclark.github.io/mvgam/articles/nmix.html)
- [Hierarchical generalized additive
  models](https://www.youtube.com/watch?v=2POK_FVwCHk)
- [Joint species distribution
  models](https://nicholasjclark.github.io/mvgam/articles/jsdgam.html)

To choose a model,
[`?mvgam_families`](https://nicholasjclark.github.io/mvgam/reference/mvgam_families.html)
matches each response type to an observation family.
[`?trend_constructors`](https://nicholasjclark.github.io/mvgam/reference/trend_constructors.html)
describes each latent process and when it suits.

## Installation

Install the released version from `CRAN` with
`install.packages("mvgam")`, or the development version with
`devtools::install_github("nicholasjclark/mvgam")`. `mvgam` also needs a
working installation of `Stan` through
[`rstan`](https://mc-stan.org/users/interfaces/rstan) or
[`cmdstanr`](https://mc-stan.org/cmdstanr/).

## Introductory seminar

# An error occurred.

Unable to execute JavaScript.

[![Introductory seminar on Ecological forecasting with Dynamic
Generalized Additive
Models](https://img.youtube.com/vi/0zZopLlomsQ/hqdefault.jpg)](https://www.youtube.com/watch?v=0zZopLlomsQ)

## Cheatsheet

[![mvgam usage
cheatsheet](https://nicholasjclark.github.io/mvgam/cheatsheets/mvgam_cheatsheet.png)](https://nicholasjclark.github.io/mvgam/cheatsheets/mvgam_cheatsheet.pdf)

[![jsdgam cheatsheet for joint species distribution
models](https://nicholasjclark.github.io/mvgam/cheatsheets/jsdgam_cheatsheet.png)](https://nicholasjclark.github.io/mvgam/cheatsheets/jsdgam_cheatsheet.pdf)

## A state-space model of rodent captures

The `portal_data` from [the Portal
Project](https://portal.weecology.org/) hold monthly counts of baited
captures for four desert rodent species, with minimum temperature and a
12-month moving average of NDVI (see
[`?portal_data`](https://nicholasjclark.github.io/mvgam/reference/portal_data.md)).
[`mvgam_data()`](https://nicholasjclark.github.io/mvgam/reference/mvgam_data.md)
checks that the data suit an observation family and plots each series:

``` r

data(portal_data)
mvgam_data(portal_data, y = "captures", family = poisson())
#> ✔ Data check passed for family 'poisson (link = log)'.
#> • series: 4 levels
#> • time:   1 to 80
#> • n obs:  320 (68 NA)
```

![Exploratory plots of four rodent capture
series](reference/figures/README-portal-data-1.png)

The counts have missing values, many zeros, seasonality and
autocorrelation. To test forecasts against data the model has not seen,
we held out months 61 to 65:

``` r

data_train <- subset(portal_data, time <= 60)
data_test <- subset(portal_data, time > 60 & time <= 65)
```

To estimate how temperature and vegetation drive each species’
abundance, we fitted a state-space GAM. Each species takes its own
intercept in the observation model. Each species’ latent abundance takes
its own smooth of minimum temperature, a smooth of NDVI shared by all
four species and first-order autoregressive dynamics. `AR(cor = TRUE)`
lets the four processes share correlated shocks:

``` r

mod <- mvgam(
  captures ~ series,
  trend_formula = ~ s(mintemp, k = 5, by = lv_axis()) +
    s(ndvi_ma12, k = 5) +
    AR(cor = TRUE),
  data = data_train,
  family = poisson(),
  chains = 4, cores = 4, silent = 2
)
#> Warning: 125 of 4000 (3.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
summary(mod)
#>  Family: poisson 
#>   Links: mu = log 
#> Formula: captures ~ series 
#>    Data: data_train (Number of observations: 240) 
#>  Series: 4 
#>  Trends: AR(1); formula: ~s(mintemp, k = 5, by = lv_axis()) + s(ndvi_ma12, k = 5) 
#>   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1; 
#>          total post-warmup draws = 4000
#> 
#> == Observation Model ==
#> Regression Coefficients:
#>           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept     2.86      0.28     2.28     3.45 1.00     2393     1869
#> seriesDO     -2.44      0.56    -3.64    -1.41 1.00     1886     1278
#> seriesPB     -3.07      1.15    -5.68    -0.99 1.00      903      358
#> seriesPP     -2.15      0.49    -3.17    -1.18 1.00     2200     1474
#> 
#> == Trend Model ==
#> Smoothing Spline Hyperparameters:
#>                         Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
#> sds(smintempseriesDM_1)     0.67      0.72     0.02     2.64 1.00     2697
#> sds(smintempseriesDO_1)     1.23      1.15     0.05     4.31 1.00     2605
#> sds(smintempseriesPB_1)     2.81      1.95     0.19     7.67 1.00     2514
#> sds(smintempseriesPP_1)     1.54      1.37     0.06     5.04 1.00     2867
#> sds(sndvi_ma12_1)           0.92      0.88     0.03     3.14 1.00     2757
#>                         Tail_ESS
#> sds(smintempseriesDM_1)     2193
#> sds(smintempseriesDO_1)     2408
#> sds(smintempseriesPB_1)     1945
#> sds(smintempseriesPP_1)     2679
#> sds(sndvi_ma12_1)           2470
#> 
#> Regression Coefficients:
#>                     Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> smintemp:seriesDM_1    -0.81      0.98    -2.86     1.32 1.00     3235     2463
#> smintemp:seriesDO_1    -0.27      2.28    -5.33     4.12 1.00     2816     2193
#> smintemp:seriesPB_1     1.37      4.00    -7.81     7.99 1.00     3325     2923
#> smintemp:seriesPP_1     5.47      2.92     0.83    12.20 1.00     2837     2347
#> sndvi_ma12_1            0.99      1.18    -1.37     3.48 1.00     2986     2583
#> 
#> Trend Specific Parameters:
#>                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> sigma_trend[DM]     0.20      0.04     0.13     0.30 1.00     2018     1154
#> sigma_trend[DO]     0.34      0.12     0.16     0.62 1.00     2674     3208
#> sigma_trend[PB]     0.51      0.16     0.26     0.86 1.00     2372     3217
#> sigma_trend[PP]     0.74      0.16     0.48     1.09 1.00     1777     3036
#> ar1_trend[DM]       0.86      0.08     0.66     0.98 1.00     1612     1688
#> ar1_trend[DO]       0.73      0.27    -0.05     0.99 1.00     1350     1732
#> ar1_trend[PB]       0.90      0.07     0.74     0.99 1.00      908      261
#> ar1_trend[PP]       0.68      0.12     0.42     0.88 1.00     2010     1809
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
```

[`conditional_effects()`](https://paulbuerkner.com/brms/reference/conditional_effects.brmsfit.html)
plots the smooths on the scale of the latent process:

``` r

conditional_effects(mod)
```

![Smooth effects of minimum temperature and NDVI on each species' latent
abundance](reference/figures/README-portal-effects-1.png)![Smooth
effects of minimum temperature and NDVI on each species' latent
abundance](reference/figures/README-portal-effects-2.png)![Smooth
effects of minimum temperature and NDVI on each species' latent
abundance](reference/figures/README-portal-effects-3.png)

[`forecast()`](https://generics.r-lib.org/reference/forecast.html)
continues each latent process through the five held-out months. The plot
sets the forecasts against the held-out counts:

``` r

fc <- forecast(mod, newdata = data_test)
plot(fc)
```

![Hindcasts and forecasts of rodent captures with the held-out
observations](reference/figures/README-portal-forecast-1.png)

[`score()`](https://nicholasjclark.github.io/mvgam/reference/score.mvgam_forecast.md)
and
[`compare_scores()`](https://nicholasjclark.github.io/mvgam/reference/compare_scores.md)
evaluate forecasts with proper scoring rules. The [forecast evaluation
article](https://nicholasjclark.github.io/mvgam/articles/forecast_evaluation.html)
compares models by their forecasts.

Randomised quantile residuals plotted over time show any structure the
model missed:

``` r

pp_check(mod, type = "resid_ribbon_grouped", group = "series",
         x = "time", ndraws = 200)
#> Observations with a missing response are omitted from the plot.
#> This message is displayed once per session.
```

![Randomised quantile residuals over time for each rodent
species](reference/figures/README-portal-resid-1.png)

## Vector autoregressions

A vector autoregression (VAR) lets each latent process depend on the
lagged values of every process. To show what `mvgam` recovers from such
a system, we simulated monthly counts of a prey species and its predator
over ten years. Predators suppress the prey a month later. Prey in turn
raise the predators a month later:

``` r

set.seed(2025)
n_months <- 120
# Rows hold this month's processes, columns last month's
A <- rbind(prey     = c(0.6, -0.5),
           predator = c(0.4,  0.5))
z <- matrix(0, n_months, 2)
for (t in 2:n_months) {
  z[t, ] <- A %*% z[t - 1, ] + rnorm(2, sd = 0.35)
}
predator_prey <- data.frame(
  time = rep(seq_len(n_months), 2),
  series = factor(rep(c("prey", "predator"), each = n_months),
                  levels = c("prey", "predator")),
  y = rpois(2 * n_months, exp(c(2.5 + z[, 1], 1.5 + z[, 2])))
)
```

``` r

mod_var <- mvgam(
  y ~ series,
  trend_formula = ~ VAR(),
  data = predator_prey,
  family = poisson(),
  chains = 4, cores = 4, silent = 2
)
```

[`posterior_transition_matrix()`](https://nicholasjclark.github.io/mvgam/reference/posterior_transition_matrix.md)
summarises the estimated coefficient matrix. Each cell gives the effect
of a process last month (column) on a process this month (row):

``` r

A_post <- posterior_transition_matrix(mod_var)
plot(A_post)
```

![Posterior transition matrix of the predator-prey
VAR](reference/figures/README-var-matrix-1.png)

The model recovered the simulated interactions. The predators’ effect on
the prey has a posterior mean of -0.58 against a true value of -0.50.
The prey’s effect on the predators has a mean of 0.47 against 0.40.

[`irf()`](https://nicholasjclark.github.io/mvgam/reference/irf.mvgam.md)
traces how a shock to one process spreads through the system. Each panel
follows one process after a shock of one standard deviation:

``` r

irfs <- irf(mod_var, h = 12)
plot(irfs, series = 1)
```

![Impulse responses of both species to a shock to the
prey](reference/figures/README-var-irf-1.png)

A shock to the prey raises the predators, whose response peaks 2 months
later at 0.17 on the log scale. The same shock to the predators lowers
the prey, reaching -0.18 after 3 months:

``` r

plot(irfs, series = 2)
```

![Impulse responses of both species to a shock to the
predator](reference/figures/README-var-irf-predator-1.png)

[`fevd()`](https://nicholasjclark.github.io/mvgam/reference/fevd.mvgam.md)
splits the forecast variance of each process among the shocks that
produce it:

``` r

fevds <- fevd(mod_var, h = 12)
plot(fevds)
```

![Forecast error variance decompositions of the predator-prey
VAR](reference/figures/README-var-fevd-1.png)

At a 12-month horizon, shocks to the predators account for 35% of the
prey’s forecast variance. Shocks to the prey account for 29% of the
predators’ forecast variance.

The [VAR
article](https://nicholasjclark.github.io/mvgam/articles/var.html)
covers higher-order and hierarchical VARs.

## Describing a fitted model

A fitted model combines published methods: an observation family, a
trend model, priors and a sampler.
[`how_to_cite()`](https://nicholasjclark.github.io/mvgam/reference/how_to_cite.mvgam.md)
drafts a methods paragraph for the model and lists the references for
each part:

The methods paragraph and references `how_to_cite(mod)` drafts

``` r

how_to_cite(mod)
#> Methods text skeleton
#> We used the R package mvgam (version 2.0.0; Clark & Wells, 2023) to
#>   construct, fit and interrogate the model. mvgam fits Bayesian
#>   state-space models that combine flexible predictor effects in both the
#>   process and observation components, using functionality from the brms
#>   (Burkner 2017) and mgcv (Wood 2017) packages. Observations followed the
#>   poisson family with the log link. The latent process followed AR(1)
#>   dynamics with correlated innovations. The mvgam-constructed model and
#>   data were passed to Stan (Carpenter et al. 2017) via the cmdstanr
#>   interface (Gabry et al. 2024). We ran 4 Hamiltonian Monte Carlo chains
#>   for 1000 warmup iterations and 1000 sampling iterations. Rank-normalised
#>   split Rhat and effective sample sizes (Vehtari et al. 2021) were used to
#>   monitor convergence.
#> 
#> Primary references
#> Burkner PC (2017). brms: An R Package for Bayesian Multilevel Models
#>   Using Stan. Journal of Statistical Software, 80(1), 1-28.
#>   https://doi.org/10.18637/jss.v080.i01
#> Carpenter B, Gelman A, Hoffman MD, Lee D, Goodrich B, Betancourt M,
#>   Brubaker M, Guo J, Li P and Riddell A (2017). Stan: A probabilistic
#>   programming language. Journal of Statistical Software, 76(1), 1-32.
#>   https://doi.org/10.18637/jss.v076.i01
#> Clark NJ and Wells K (2023). Dynamic Generalized Additive Models (DGAMs)
#>   for forecasting discrete ecological time series. Methods in Ecology and
#>   Evolution, 14(3), 771-784. https://doi.org/10.1111/2041-210X.13974
#> Gabry J, Cesnovar R, Johnson A and Bronder S (2024). cmdstanr: R
#>   Interface to 'CmdStan'. https://mc-stan.org/cmdstanr/
#> Vehtari A, Gelman A, Simpson D, Carpenter B and Burkner P (2021).
#>   Rank-normalization, folding, and localization: An improved Rhat for
#>   assessing convergence of MCMC. Bayesian Analysis, 16(2), 667-718.
#>   https://doi.org/10.1214/20-BA1221
#> Wood SN (2017). Generalized Additive Models: An Introduction with R (2nd
#>   edition). Chapman and Hall/CRC.
#> 
#> Other useful references
#> Arel-Bundock V, Greifer N and Heiss A (2024). How to interpret
#>   statistical models using marginaleffects for R and Python. Journal of
#>   Statistical Software, 111(9), 1-32.
#>   https://doi.org/10.18637/jss.v111.i09
#> Burkner PC, Gabry J and Vehtari A (2020). Approximate leave-future-out
#>   cross-validation for Bayesian time series models. Journal of Statistical
#>   Computation and Simulation, 90(14), 2499-2523.
#>   https://doi.org/10.1080/00949655.2020.1783262
#> Gabry J, Simpson D, Vehtari A, Betancourt M and Gelman A (2019).
#>   Visualization in Bayesian workflow. Journal of the Royal Statistical
#>   Society: Series A (Statistics in Society), 182(2), 389-402.
#>   https://doi.org/10.1111/rssa.12378
#> Vehtari A, Gelman A and Gabry J (2017). Practical Bayesian model
#>   evaluation using leave-one-out cross-validation and WAIC. Statistics and
#>   Computing, 27(5), 1413-1432. https://doi.org/10.1007/s11222-016-9696-4
#> 
#> Use `bibtex(x)` for a BibTeX block ready for your .bib file.
```

[`bibtex()`](https://nicholasjclark.github.io/mvgam/reference/bibtex.md)
writes those references to a `.bib` file.
[`methods_md()`](https://nicholasjclark.github.io/mvgam/reference/methods_md.md)
writes the model as Markdown equations with a glossary of symbols and
the priors, ready for a methods section or an appendix:

The equations `methods_md(mod)` writes for the rodent model

#### Data

$`\mathrm{captures}`$: non-negative integer counts

$`N = 240`$ observations, $`S = 4`$ series, $`T = 60`$ time points

Predictors:

- $`\mathrm{ndvi\_ma12}`$: continuous covariate
- $`\mathrm{mintemp}`$: continuous covariate

#### Model

``` math
\begin{aligned}
i &\in \{1, \ldots, 4\} \\
t &\in \{1, \ldots, 60\} \\
\mathrm{captures}_{i,t} &\sim \text{Poisson}(\mu_{i,t}) \\
\log \mu_{i,t} &= \alpha + \beta_{\mathrm{series}[\mathrm{DO}]} \mathbb{1}[i = \mathrm{DO}] + \beta_{\mathrm{series}[\mathrm{PB}]} \mathbb{1}[i = \mathrm{PB}] + \beta_{\mathrm{series}[\mathrm{PP}]} \mathbb{1}[i = \mathrm{PP}] + \mu^{(\eta)}_{i,t} + \eta_{i,t} \\
\mu^{(\eta)}_{i,t} &= \beta^{(\eta)}_{\mathrm{smintemp}:\mathrm{seriesDM\_1}} \mathrm{smintemp}_{i,t} \, \mathrm{seriesDM\_1}_{i,t} + \beta^{(\eta)}_{\mathrm{smintemp}:\mathrm{seriesDO\_1}} \mathrm{smintemp}_{i,t} \, \mathrm{seriesDO\_1}_{i,t} + \beta^{(\eta)}_{\mathrm{smintemp}:\mathrm{seriesPB\_1}} \mathrm{smintemp}_{i,t} \, \mathrm{seriesPB\_1}_{i,t} + \beta^{(\eta)}_{\mathrm{smintemp}:\mathrm{seriesPP\_1}} \mathrm{smintemp}_{i,t} \, \mathrm{seriesPP\_1}_{i,t} + \beta^{(\eta)}_{\mathrm{sndvi\_ma12\_1}} \mathrm{sndvi\_ma12\_1}_{i,t} + f^{(\eta)}_{\mathrm{mintemp} \mid \mathrm{series}}(\mathrm{mintemp}_{i,t}) + f^{(\eta)}_{\mathrm{ndvi\_ma12}}(\mathrm{ndvi\_ma12}_{i,t}) \\
f^{(\eta)}_{\mathrm{mintemp} \mid \mathrm{series}}(\mathrm{mintemp}) &= \sum_{m=1}^{K_{\mathrm{mintemp}}} \beta^{(\mathrm{mintemp}, \eta)}_m B_m(\mathrm{mintemp}) \\
f^{(\eta)}_{\mathrm{ndvi\_ma12}}(\mathrm{ndvi\_ma12}) &= \sum_{m=1}^{K_{\mathrm{ndvi\_ma12}}} \beta^{(\mathrm{ndvi\_ma12}, \eta)}_m B_m(\mathrm{ndvi\_ma12}) \\
\eta_{i,t} &= \phi_{i,1} \eta_{i,t-1} + \epsilon^{(\eta)}_{i,t} \\
\boldsymbol{\epsilon}_{t} &\sim \text{MVNormal}(\mathbf{0}, \boldsymbol{\Sigma}) \\
\boldsymbol{\Sigma} &= \text{diag}(\boldsymbol{\sigma}_\eta) \, \boldsymbol{\Omega} \, \text{diag}(\boldsymbol{\sigma}_\eta)
\end{aligned}
```

where:

- $`i`$ indexes series, $`t`$ indexes time
- $`\mu_{i,t}`$: conditional mean of $`\mathrm{captures}_{i,t}`$
- $`\alpha`$: population intercept
- $`\beta_{\mathrm{series}[\mathrm{DO}]}`$: difference between level
  $`\mathrm{DO}`$ and reference level $`\mathrm{DM}`$ of
  $`\mathrm{series}`$
- $`\beta_{\mathrm{series}[\mathrm{PB}]}`$: difference between level
  $`\mathrm{PB}`$ and reference level $`\mathrm{DM}`$ of
  $`\mathrm{series}`$
- $`\beta_{\mathrm{series}[\mathrm{PP}]}`$: difference between level
  $`\mathrm{PP}`$ and reference level $`\mathrm{DM}`$ of
  $`\mathrm{series}`$
- $`\eta_{i,t}`$: latent state of series $`i`$ at time $`t`$
- $`\mu^{(\eta)}_{i,t}`$: trend predictor from ‘trend_formula’
- $`\beta^{(\eta)}_{\mathrm{smintemp}:\mathrm{seriesDM\_1}}`$:
  population effect of $`\mathrm{smintemp} \times \mathrm{seriesDM\_1}`$
- $`\beta^{(\eta)}_{\mathrm{smintemp}:\mathrm{seriesDO\_1}}`$:
  population effect of $`\mathrm{smintemp} \times \mathrm{seriesDO\_1}`$
- $`\beta^{(\eta)}_{\mathrm{smintemp}:\mathrm{seriesPB\_1}}`$:
  population effect of $`\mathrm{smintemp} \times \mathrm{seriesPB\_1}`$
- $`\beta^{(\eta)}_{\mathrm{smintemp}:\mathrm{seriesPP\_1}}`$:
  population effect of $`\mathrm{smintemp} \times \mathrm{seriesPP\_1}`$
- $`\beta^{(\eta)}_{\mathrm{sndvi\_ma12\_1}}`$: population effect of
  $`\mathrm{sndvi\_ma12\_1}`$
- $`f^{(\eta)}_{\mathrm{mintemp} \mid \mathrm{series}}`$: thin plate
  regression spline in $`\mathrm{mintemp}`$, stratified by
  $`\mathrm{series}`$, basis size $`K_{\mathrm{mintemp}} = 5`$,
  smoothing SD $`\sigma^{(s, \eta)}_{\mathrm{mintemp}}`$
- $`f^{(\eta)}_{\mathrm{ndvi\_ma12}}`$: thin plate regression spline in
  $`\mathrm{ndvi\_ma12}`$, basis size $`K_{\mathrm{ndvi\_ma12}} = 5`$,
  smoothing SD $`\sigma^{(s, \eta)}_{\mathrm{ndvi\_ma12}}`$
- $`\boldsymbol{\epsilon}_{t}`$: process innovations at time $`t`$,
  covariance $`\boldsymbol{\Sigma}`$, with $`\boldsymbol{\sigma}_\eta`$
  the vector of SDs $`\sigma_{\eta,i}`$
- $`\boldsymbol{\Omega}`$: correlation matrix of the innovations
- $`\phi_{i,l}`$: autoregressive coefficient at lag $`l`$

#### Priors

``` math
\begin{aligned}
\alpha &\sim \text{StudentT}(3, 0.9, 2.5) \\
\sigma^{(s, \eta)}_{\mathrm{mintemp}} &\sim \text{StudentT}(3, 0, 2.5) \\
\sigma^{(s, \eta)}_{\mathrm{ndvi\_ma12}} &\sim \text{StudentT}(3, 0, 2.5) \\
\sigma_{\eta,i} &\sim \text{InvGamma}(3, 1.07) \\
\boldsymbol{\Omega} &\sim \text{LKJCorr}(2) \\
\phi_{i,1} &\sim \text{Normal}(0, 0.5)
\end{aligned}
```

Every other parameter takes an improper flat prior on its support.

#### Implementation

Fitted with mvgam 2.0.0 (brms 2.23.0; Stan 2.38.0 via cmdstanr).
Hamiltonian Monte Carlo: 4 chains, 1000 warmup + 1000 sampling
iterations per chain.

``` R
mvgam(
  formula       = captures ~ series,
  trend_formula = ~s(mintemp, k = 5, by = lv_axis()) + s(ndvi_ma12, k = 5) + AR(cor = TRUE),
  data          = data_train,
  family        = poisson(),
  backend       = "cmdstanr",
  chains        = 4,
  iter          = 2000,
  warmup        = 1000
)
```

## Observation families

`mvgam` began as a tool for forecasting ecological counts. Its
observation families now span most of the response types met in
ecological and environmental monitoring.

For continuous responses:

- [`gaussian()`](https://rdrr.io/r/stats/family.html) for real-valued
  data
- [`student()`](https://paulbuerkner.com/brms/reference/brmsfamily.html)
  for real-valued data with heavy tails
- [`lognormal()`](https://paulbuerkner.com/brms/reference/brmsfamily.html)
  and [`Gamma()`](https://rdrr.io/r/stats/family.html) for strictly
  positive data
- [`exponential()`](https://paulbuerkner.com/brms/reference/brmsfamily.html)
  for waiting times
- [`Beta()`](https://paulbuerkner.com/brms/reference/brmsfamily.html)
  for proportions on `(0, 1)`
- [`tweedie()`](https://nicholasjclark.github.io/mvgam/reference/tweedie.md)
  for positive continuous data carrying an exact mass at zero, such as
  catch per unit effort or rainfall

For counts and binary outcomes:

- [`poisson()`](https://rdrr.io/r/stats/family.html) for equidispersed
  counts
- [`negbinomial()`](https://paulbuerkner.com/brms/reference/brmsfamily.html)
  for overdispersed counts
- [`beta_nb()`](https://nicholasjclark.github.io/mvgam/reference/beta_nb.md)
  for counts whose tails run heavier than a negative binomial can reach
- [`bernoulli()`](https://paulbuerkner.com/brms/reference/brmsfamily.html)
  for binary data
- [`binomial()`](https://rdrr.io/r/stats/family.html) and
  [`beta_binomial()`](https://paulbuerkner.com/brms/reference/brmsfamily.html)
  for counts with a known number of trials
- [`com_binomial()`](https://nicholasjclark.github.io/mvgam/reference/com_binomial.md)
  for bounded counts that are under-, over- or super-dispersed relative
  to a binomial

A further group of families operates on *closure units*, where several
rows of the data share a single latent state. `mvgam` groups the rows of
a unit by their `series` and `time` values. Replicate visits to a site
and the species of one assemblage then need no reshaping:

- [`nmix()`](https://nicholasjclark.github.io/mvgam/reference/nmix.md)
  for repeat counts of an unknown abundance seen with imperfect
  detection
- [`occ()`](https://nicholasjclark.github.io/mvgam/reference/occ.md) for
  repeat detection and non-detection visits to a site of unknown
  occupancy
- [`multi()`](https://nicholasjclark.github.io/mvgam/reference/multi.md)
  and
  [`diri()`](https://nicholasjclark.github.io/mvgam/reference/diri.md)
  for compositional counts and proportions that sum within a unit
- [`categ()`](https://nicholasjclark.github.io/mvgam/reference/categ.md)
  for a single categorical outcome per unit
- [`mvn()`](https://nicholasjclark.github.io/mvgam/reference/mvn.md) and
  [`mvt()`](https://nicholasjclark.github.io/mvgam/reference/mvt.md) for
  continuous multi-species responses with a low-rank residual
  covariance, the latter permitting heavier tails

[`?mvgam_families`](https://nicholasjclark.github.io/mvgam/reference/mvgam_families.md)
lists the full set, grouped by the kind of response each one suits. To
show a `Beta` model, we simulated proportions for three series. Each
series has a smooth covariate effect and its own autoregressive trend:

``` r

set.seed(100)
beta_data <- sim_mvgam(
  family = Beta(),
  n_series = 3L,
  n_timepoints = 80L,
  trend_model = AR(),
  prop_trend = 0.5
)
mvgam_data(beta_data$data_train, y = "y", family = Beta())
#> ✔ Data check passed for family 'beta (link = logit)'.
#> • series: 3 levels
#> • time:   1 to 60
#> • n obs:  180 (0 NA)
```

![Exploratory plots of three simulated series of
proportions](reference/figures/README-beta-sim-1.png)

``` r

mod_beta <- mvgam(
  y ~ s(x, by = series, k = 6),
  trend_formula = ~ AR(p = 1),
  data = beta_data$data_train,
  newdata = beta_data$data_test,
  family = Beta(),
  chains = 4, cores = 4, silent = 2
)
#> Warning: 32 of 4000 (1.0%) transitions ended with a divergence.
#> See https://mc-stan.org/misc/warnings for details.
#> Warning: 4 of 4 chains had an E-BFMI less than 0.3.
#> See https://mc-stan.org/misc/warnings for details.
```

The three series share one `Beta` precision parameter $`\phi`$. The
summary reports it with the smooths and the trend parameters:

``` r

summary(mod_beta)
#>  Family: beta 
#>   Links: mu = logit; phi = log 
#> Formula: y ~ s(x, by = series, k = 6) 
#>    Data: beta_data$data_train (Number of observations: 180) 
#>  Series: 3 
#>  Trends: AR(1) 
#>   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1; 
#>          total post-warmup draws = 4000
#> 
#> == Observation Model ==
#> Smoothing Spline Hyperparameters:
#>                         Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
#> sds(sxseriesseries_1_1)     4.81      2.31     1.73    10.50 1.04       91
#> sds(sxseriesseries_2_1)     6.03      2.48     2.56    11.88 1.02      514
#> sds(sxseriesseries_3_1)     8.88      3.43     3.99    17.29 1.07     1330
#>                         Tail_ESS
#> sds(sxseriesseries_1_1)     2473
#> sds(sxseriesseries_2_1)     2357
#> sds(sxseriesseries_3_1)     2012
#> 
#> Regression Coefficients:
#>                     Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> Intercept               0.19      0.16    -0.13     0.51 1.01      878     1997
#> sx:seriesseries_1_1    10.67      7.38    -1.09    25.07 1.07       44      161
#> sx:seriesseries_2_1    14.80      6.18     3.90    26.87 1.07       41       71
#> sx:seriesseries_3_1    23.44      7.20    10.27    37.19 1.08       36       78
#> 
#> Further Distributional Parameters:
#>     Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#> phi    11.86      7.84     5.27    35.93 1.15       19       17
#> 
#> == Trend Model ==
#> Trend Specific Parameters:
#>                       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
#> sigma_trend[series_1]     0.74      0.22     0.34     1.15 1.09       34
#> sigma_trend[series_2]     0.81      0.21     0.45     1.22 1.09       34
#> sigma_trend[series_3]     0.47      0.17     0.18     0.80 1.09       32
#> ar1_trend[series_1]       0.57      0.20     0.17     0.91 1.05       86
#> ar1_trend[series_2]       0.72      0.11     0.47     0.91 1.01      695
#> ar1_trend[series_3]       0.55      0.26    -0.10     0.92 1.03      251
#>                       Tail_ESS
#> sigma_trend[series_1]      437
#> sigma_trend[series_2]      461
#> sigma_trend[series_3]      469
#> ar1_trend[series_1]       1029
#> ar1_trend[series_2]       1834
#> ar1_trend[series_3]       1243
#> 
#> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
#> and Tail_ESS are effective sample size measures, and Rhat is the potential
#> scale reduction factor on split chains (at convergence, Rhat = 1).
#> 
#> Next steps:
#>   - `pp_check(fit)`: posterior predictive checks
#>   - `forecast(fit, newdata = ...)`: out-of-sample forecasts
#>   - `lfo_cv(fit)`: leave-future-out model comparison
#>   - `conditional_effects(fit)`: covariate effects
#> Use `how_to_cite(fit)` for a methods paragraph and `methods_md(fit)` for its equations.
```

The hindcasts and forecasts of each series, against the held-out
observations:

``` r

plot(forecast(mod_beta))
```

![Hindcasts and forecasts of three series of
proportions](reference/figures/README-beta-forecast-1.png)

The [package documentation](https://nicholasjclark.github.io/mvgam/)
covers the other uses of `mvgam`, among them dynamic and spatially
varying coefficients, dynamic factors and joint species distribution
models.
[`stancode()`](https://paulbuerkner.com/brms/reference/stancode.html)
and
[`standata()`](https://paulbuerkner.com/brms/reference/standata.html)
return the `Stan` program and data of any model for users who wish to
adapt it. The [`Stan` Discourse](https://discourse.mc-stan.org/) is a
helpful place to troubleshoot.

## Citing `mvgam` and related software

Research software is written and maintained by people whose only
currency is citation. If `mvgam` contributed to an analysis, please cite
it and the packages it depends on. A ⭐ on the repository is welcome
too.

When using `mvgam`, please cite the following:

> Clark, N.J. and Wells, K. (2023). Dynamic Generalized Additive Models
> (DGAMs) for forecasting discrete ecological time series. *Methods in
> Ecology and Evolution*. DOI: <https://doi.org/10.1111/2041-210X.13974>

As `mvgam` acts as an interface to `Stan`, please additionally cite:

> Carpenter B., Gelman A., Hoffman M. D., Lee D., Goodrich B.,
> Betancourt M., Brubaker M., Guo J., Li P., and Riddell A. (2017).
> Stan: A probabilistic programming language. *Journal of Statistical
> Software*. 76(1). DOI: <https://doi.org/10.18637/jss.v076.i01>

`how_to_cite(mod)` lists the further references for the family, trend
model and sampler of a fitted model.

## Getting help

Bug reports belong on the [issue
tracker](https://github.com/nicholasjclark/mvgam/issues). A minimal
reproducible example makes a report fixable. Questions about model
syntax, priors or interpretation are better suited to the [`mvgam`
Discussion Board](https://github.com/nicholasjclark/mvgam/discussions),
where an earlier thread has often covered the same ground already. The
[Changelog](https://nicholasjclark.github.io/mvgam/news/index.html)
records what changed in each release.

## Other resources

The [package website](https://nicholasjclark.github.io/mvgam/) holds
articles on data formatting, forecasting and several extended case
studies. Further examples, including step-by-step introductory webinars:

- [Time series in R and Stan using the `mvgam`
  package](https://www.youtube.com/playlist?list=PLzFHNoUxkCvsFIg6zqogylUfPpaxau_a3)
- [Ecological Forecasting with Dynamic Generalized Additive
  Models](https://www.youtube.com/watch?v=0zZopLlomsQ)
- [Distributed lags (and hierarchical distributed lags) using `mgcv` and
  `mvgam`](https://ecogambler.netlify.app/blog/distributed-lags-mgcv/)
- [State-Space Vector Autoregressions in
  `mvgam`](https://ecogambler.netlify.app/blog/vector-autoregressions/)
- [Ecological Forecasting with Dynamic GAMs; a tutorial and detailed
  case study](https://www.youtube.com/watch?v=RwllLjgPUmM)
- [Incorporating time-varying seasonality in forecast
  models](https://ecogambler.netlify.app/blog/time-varying-seasonality/)

## Interested in contributing?

I’m actively seeking PhD students and other researchers to work in the
areas of ecological forecasting, multivariate model evaluation and
development of `mvgam`. Please reach out if you are interested
(n.clark’at’uq.edu.au). Other contributions are also very welcome. [The
Contributor
Instructions](https://github.com/nicholasjclark/mvgam/blob/master/.github/CONTRIBUTING.md)
give general guidelines. By participating in this project you agree to
abide by the terms of its [Contributor Code of
Conduct](https://github.com/nicholasjclark/mvgam/blob/master/.github/CODE_OF_CONDUCT.md).

## License

The `mvgam` project is licensed under an `MIT` open source license
