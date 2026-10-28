# State-space vector autoregressions with mvgam

Do the plankton of a lake regulate one another from month to month, or
do they rise and fall with the same seasons and nutrients? A vector
autoregression (VAR) separates the two. It lets the state of each taxon
this month depend on the state of every taxon last month. The
coefficients of that dependence form a matrix $`A`$. Its off-diagonal
cells are the lagged interactions between taxa (Lütkepohl
[2005](https://doi.org/10.1007/978-3-540-27752-1)).

mvgam fits the VAR to latent states. Because each measurement carries
observation error of its own (Auger-Méthé et al.
[2021](https://doi.org/10.1002/ecm.1470)), a separate observation model
links those states to the data. This article fits that model to five
plankton taxa from Lake Washington, then works through what a fitted VAR
offers: the interactions in $`A`$, the correlation of the taxa’s shocks,
impulse responses, variance decompositions, stability metrics and
forecasts.

## The data

The MARSS package (Holmes et al.
[2012](https://doi.org/10.32614/RJ-2012-002)) distributes monthly
plankton densities and water conditions for Lake Washington from 1962 to
1994, drawn from the long-term monitoring of W. T. Edmondson. In the
transformed version each density is log-transformed and then z-scored
over the whole record, with zero counts recorded as missing.

We use the fifteen years from 1980 to 1994. That window is long enough
to estimate monthly interactions and short enough to fit in minutes. We
use five taxa. Four are groups of phytoplankton: diatoms, green algae,
unicellular algae and the cryptophyte *Cryptomonas*. The fifth is
*Daphnia*, a grazing zooplankter that eats them. Two water conditions
enter as covariates. Temperature has a strong seasonal cycle. Because a
smooth of month describes that cycle, the covariate is the temperature
anomaly, each month’s temperature less the mean for its calendar month.
Phosphorus income controlled the growth of algae in Lake Washington
(Edmondson and Lehman
[1981](https://doi.org/10.4319/lo.1981.26.1.0001)). Total phosphorus
averaged over the twelve months to each month measures that nutrient
loading.

``` r

load(url("https://github.com/atsa-es/MARSS/raw/master/data/lakeWAplankton.rda"))
lake <- as.data.frame(lakeWAplanktonTrans)
lake$TP_annual <- as.numeric(
  stats::filter(lake$TP, rep(1 / 12, 12), sides = 1)
)
lake <- lake[lake$Year >= 1980, ]
lake$time <- seq_len(nrow(lake))
lake$temp_anom <- lake$Temp - ave(lake$Temp, lake$Month)

taxa <- c("Diatoms", "Greens", "Unicells", "Cryptomonas", "Daphnia")
plankton <- do.call(rbind, lapply(taxa, function(taxon) {
  data.frame(
    y = lake[[taxon]], time = lake$time, year = lake$Year,
    month = lake$Month, temp_anom = lake$temp_anom,
    TP_annual = lake$TP_annual, series = taxon
  )
}))
plankton$series <- factor(plankton$series, levels = taxa)
```

The data are in long format, one row per taxon and month. `series` names
the taxon and `time` counts the months. The order of the levels of
`series` fixes the order of the taxa in every output below. We hold 1994
out to score forecasts against:

``` r

train <- plankton[plankton$year < 1994, ]
test <- plankton[plankton$year == 1994, ]
```

[`mvgam_data()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_data.md)
checks the data against the model’s requirements and plots each series,
with the held-out months marked:

``` r

mvgam_data(train, family = gaussian(), newdata = test)
```

![Monthly log densities of the five taxa, z-scored over 1962 to 1994.
The final year is held out.](var_files/figure-html/plot-data-1.png)

Monthly log densities of the five taxa, z-scored over 1962 to 1994. The
final year is held out.

Each taxon cycles through the seasons. The unicellular algae also
decline from 1980 until about 1988 and then level off, with two brief
collapses near the end of the decline. A seasonal model alone would miss
that decline.

## The model

The model has two layers. The observation layer says that each log
density $`y_{s,t}`$ measures a latent state $`x_{s,t}`$ with observation
error, whose standard deviation $`\sigma_s`$ differs by taxon:

``` math
y_{s,t} \sim \mathrm{Normal}(x_{s,t},\ \sigma_s).
```

The latent state is a mean set by the covariates plus a deviation
$`z_{s,t}`$ that follows the VAR:

``` math
\begin{aligned}
x_{s,t} &= \alpha_s + \beta_s\,\mathrm{TP}_t
  + \gamma_s\,\mathrm{temp}_t + f_s(\mathrm{month}_t) + z_{s,t}, \\
\mathbf{z}_t &\sim \mathrm{MVN}(A\,\mathbf{z}_{t-1},\ \Sigma).
\end{aligned}
```

Each taxon has its own level $`\alpha_s`$ and seasonal smooth $`f_s`$,
with its own slopes on phosphorus loading and on the temperature
anomaly. $`A`$ holds the lagged effects among the deviations. $`\Sigma`$
holds the covariance of the monthly shocks. The VAR describes how taxa
push one another away from their seasonal and nutrient-driven means.
Seasonality itself stays out of $`A`$.

mvgam’s
[`VAR()`](https://nicholasjclark.github.io/mvgam/dev/reference/trend_constructors.md)
keeps the process stationary: after a disturbance, every deviation
returns towards zero. The prior on $`A`$ enforces this (Heaps
[2023](https://doi.org/10.1080/10618600.2022.2079648)). A stationary VAR
cannot absorb a slow drift in a taxon’s mean. When we fitted the model
without phosphorus, the four chains disagreed about the level of the
unicellular algae, whose decline nothing in that model explained. The
phosphorus slope gives the drift a cause outside the VAR.

The observation and process variances both describe scatter around the
same latent state. Fourteen years of data from one lake separate them
poorly: a month’s departure could be observation error or real change.
The prior on $`\log\sigma_s`$ supplies the missing information. We
centred it on $`\log 0.25`$ with a standard deviation of $`0.2`$, which
places observation error near a quarter of a standard deviation of log
density, below the change in the populations from month to month. The
data still move each taxon’s observation standard deviation away from
that prior.

In mvgam the trend formula holds the latent model.
[`lv_axis()`](https://nicholasjclark.github.io/mvgam/dev/reference/lv_axis.md)
gives a term a separate effect for each taxon.
[`bf()`](https://paulbuerkner.com/brms/reference/brmsformula.html) gives
the observation standard deviation a formula of its own. With
`sigma ~ 0 + series`, it takes one value per taxon. The observation
formula `y ~ -1` adds nothing to the latent state.

``` r

fit <- mvgam(
  formula = bf(y ~ -1, sigma ~ 0 + series),
  trend_formula = ~ lv_axis() + TP_annual:lv_axis() +
    temp_anom:lv_axis() + s(month, by = lv_axis(), bs = "cc", k = 5) +
    VAR(cor = TRUE),
  family = gaussian(),
  data = train,
  newdata = test,
  knots = list(month = c(0.5, 12.5)),
  priors = prior(normal(-1.4, 0.2), class = b, dpar = sigma),
  chains = 4,
  iter = 1500,
  warmup = 750,
  control = list(adapt_delta = 0.95),
  seed = 1980,
  silent = 2
)
```

`bs = "cc"` makes each seasonal smooth cyclic. December then joins
January, at the boundary that `knots` places between them. `cor = TRUE`
estimates the correlations among the shocks in $`\Sigma`$. Passing
`newdata` stores the held-out year on the model for forecasting. `seed`
makes the sampling reproducible.

## Checking the fit

We first checked that the model reproduces the data.
[`pp_check()`](https://nicholasjclark.github.io/mvgam/dev/reference/pp_check.mvgam.md)
draws replicate data sets from the posterior and overlays their
densities on the observed one, here separately for each taxon:

``` r

pp_check(fit, type = "dens_overlay_grouped", group = "series",
         ndraws = 50)
```

![Density of the observed values (dark line) and of 50 replicate data
sets drawn from the posterior (light lines), one panel per
taxon.](var_files/figure-html/ppc-1.png)

Density of the observed values (dark line) and of 50 replicate data sets
drawn from the posterior (light lines), one panel per taxon.

The simulated densities bracket the observed density of every taxon.
They reproduce the two peaks of *Daphnia*, which spends winter scarce
and summer abundant.

## What sets the means

The covariates and seasonal smooths give each taxon’s expected log
density when no taxon deviates from its mean.
[`fixef()`](https://rdrr.io/pkg/nlme/man/fixed.effects.html) with
`trend_effects = TRUE` reports the coefficients of the trend formula:

``` r

round(fixef(fit, trend_effects = TRUE), 2)
#>                                   Estimate Est.Error  Q2.5 Q97.5
#> seriesDiatoms_trend                   0.40      0.26 -0.14  0.93
#> seriesGreens_trend                    0.24      0.39 -0.54  0.97
#> seriesUnicells_trend                  1.03      0.36  0.31  1.78
#> seriesCryptomonas_trend               0.53      0.21  0.12  0.95
#> seriesDaphnia_trend                   0.05      0.20 -0.35  0.45
#> seriesDiatoms:TP_annual_trend         0.54      0.57 -0.61  1.66
#> seriesGreens:TP_annual_trend          0.42      0.82 -1.29  1.97
#> seriesUnicells:TP_annual_trend        2.88      0.76  1.31  4.35
#> seriesCryptomonas:TP_annual_trend     0.21      0.44 -0.66  1.11
#> seriesDaphnia:TP_annual_trend        -0.29      0.44 -1.16  0.60
#> seriesDiatoms:temp_anom_trend        -0.57      0.30 -1.17  0.02
#> seriesGreens:temp_anom_trend          0.23      0.43 -0.60  1.08
#> seriesUnicells:temp_anom_trend       -0.07      0.32 -0.66  0.56
#> seriesCryptomonas:temp_anom_trend     0.33      0.26 -0.19  0.84
#> seriesDaphnia:temp_anom_trend         0.20      0.24 -0.26  0.67
```

Because total phosphorus fell across the record, the positive slope of
the unicellular algae on phosphorus predicts the decline the first
figure shows. Their slope is 2.88 \[1.31, 4.35\] standard deviations of
log density per unit of phosphorus. The intervals of the other four
phosphorus slopes span zero.

mvgam computes effects on the outcome scale through the marginaleffects
package (Arel-Bundock et al.
[2024](https://doi.org/10.18637/jss.v111.i09)). Every call here passes
`type = "expected"`, which predicts the expected log density $`E[y]`$
and leaves observation error out of the interval drawn around it. The
marginaleffects default, `type = "response"`, draws new observations
with their observation error. A contrast between two such predictions
carries that error twice. Slopes and comparisons come from the expected
log density under either setting.
[`avg_slopes()`](https://rdrr.io/pkg/marginaleffects/man/slopes.html)
averages the slope of the expected log density over the data, here
separately for each taxon:

``` r

marginaleffects::avg_slopes(fit, variables = "TP_annual",
                            by = "series", type = "expected")
#> 
#>       series Estimate  2.5 % 97.5 %
#>  Diatoms        0.542 -0.607  1.655
#>  Greens         0.438 -1.286  1.971
#>  Unicells       2.896  1.309  4.346
#>  Cryptomonas    0.218 -0.658  1.109
#>  Daphnia       -0.285 -1.155  0.602
#> 
#> Term: TP_annual
#> Type: expected
#> Comparison: dY/dX
```

In a Gaussian model with an identity link, each slope has the posterior
of its coefficient. The table reports medians, where
[`fixef()`](https://rdrr.io/pkg/nlme/man/fixed.effects.html) reports
means. Under a nonlinear link they differ.
[`avg_slopes()`](https://rdrr.io/pkg/marginaleffects/man/slopes.html)
then reports the change on the outcome scale, which the coefficients do
not give directly.
[`conditional_effects()`](https://paulbuerkner.com/brms/reference/conditional_effects.brmsfit.html)
draws each effect with the other covariates held at their means:

``` r

conditional_effects(fit, effects = "TP_annual", type = "expected")
```

![Expected log density against twelve-month total phosphorus for each
taxon, with 95% credible intervals.](var_files/figure-html/ce-tp-1.png)

Expected log density against twelve-month total phosphorus for each
taxon, with 95% credible intervals.

The seasonal smooths carry the cycle that dominated the data plot:

``` r

conditional_effects(fit, effects = "month", type = "expected")
```

![Seasonal smooths of expected log density by calendar month, with 95%
credible intervals.](var_files/figure-html/ce-month-1.png)

Seasonal smooths of expected log density by calendar month, with 95%
credible intervals.

Diatoms peak in spring and reach their low in autumn. Green algae and
*Daphnia* peak in mid-summer. *Cryptomonas* peaks in April. The
temperature anomaly adds little once season is in the model. Its largest
slope, for the diatoms, is -0.57 \[-1.17, 0.02\].

## Interactions among the taxa

[`posterior_transition_matrix()`](https://nicholasjclark.github.io/mvgam/dev/reference/posterior_transition_matrix.md)
summarises the transition matrix cell by cell. Row $`i`$, column $`j`$
is the effect of taxon $`j`$’s deviation last month on taxon $`i`$’s
deviation this month:

``` r

A <- posterior_transition_matrix(fit)
A
#> Lag-1 VAR transition matrix from an mvgam fit
#>   Group      : single-panel
#>   Dimension  : 5 x 5
#>   Mean |A_ij| (i != j): 0.10
#>   Point estimate:
#>             Diatoms Greens Unicells Cryptomonas Daphnia
#> Diatoms        0.37  -0.03    -0.03        0.32   -0.19
#> Greens        -0.09   0.40     0.13        0.07    0.14
#> Unicells      -0.08   0.02     0.65       -0.14   -0.15
#> Cryptomonas   -0.06   0.00     0.06        0.50   -0.24
#> Daphnia        0.03  -0.02     0.07        0.18    0.47
#>   95% CI on each cell available in `A_lower` / `A_upper`.
```

The diagonal describes persistence. Each taxon’s deviation carries a
coefficient between 0.37 and 0.65 into the next month. The heatmap
outlines each cell whose 95% interval excludes zero:

``` r

plot(A)
```

![Posterior mean of the transition matrix. Rows are the taxa at month t
and columns the taxa at month t - 1. Outlined cells have 95% credible
intervals that exclude zero.](var_files/figure-html/plot-A-1.png)

Posterior mean of the transition matrix. Rows are the taxa at month t
and columns the taxa at month t - 1. Outlined cells have 95% credible
intervals that exclude zero.

In this fit, 2 of the twenty off-diagonal cells clear zero. Two cells
have an ecological interpretation. The first is grazing. A month with
more *Daphnia* than expected is followed by fewer *Cryptomonas*, with a
coefficient of -0.24 \[-0.43, -0.04\]. The effect of *Cryptomonas* on
*Daphnia* is 0.18 \[-0.09, 0.67\]. It is positive in 90% of the
posterior draws, which suggests that more food in one month brings more
grazers in the next. Fourteen years of monthly data do not settle that
effect.

The second cell links *Cryptomonas* to the diatoms. A month with more
*Cryptomonas* than expected is followed by more diatoms, with a
coefficient of 0.32 \[0.01, 0.84\]. Because no feeding link runs between
two phytoplankton groups, the likelier source is a driver the model
omits, such as a change in mixing that favours one group before the
other. A coefficient in $`A`$ records that one deviation predicts
another without naming a mechanism.

## Shocks and states

The transition matrix describes how taxa push one another over time. The
innovation covariance $`\Sigma`$ describes which taxa receive the same
unmodelled push in the same month. A change in light or mixing that the
covariates miss would appear as a correlation between the shocks of the
taxa it affects.
[`residual_cor()`](https://nicholasjclark.github.io/mvgam/dev/reference/residual_cor.md)
with `covariance = "innovation"` reports these correlations:

``` r

rc_innov <- residual_cor(fit, covariance = "innovation")
summary(rc_innov)
#> # A tibble: 10 × 11
#>    series_1    series_2   Estimate Est.Error Q_lower Q_upper   ESS prob_positive
#>    <chr>       <chr>         <dbl>     <dbl>   <dbl>   <dbl> <dbl>         <dbl>
#>  1 Unicells    Cryptomon…   0.476     0.143   0.261   0.691   169.         1    
#>  2 Diatoms     Cryptomon…   0.267     0.129   0.0385  0.494   797.         0.988
#>  3 Cryptomonas Daphnia      0.244     0.113   0.0235  0.439  1434.         0.986
#>  4 Diatoms     Unicells     0.176     0.0976 -0.0119  0.355  1672.         0.967
#>  5 Unicells    Daphnia     -0.142     0.0986 -0.320   0.0514 1668.         0.074
#>  6 Diatoms     Greens       0.123     0.0956 -0.0671  0.303  1736.         0.906
#>  7 Greens      Cryptomon…   0.109     0.112  -0.106   0.329  1371.         0.841
#>  8 Diatoms     Daphnia      0.103     0.107  -0.107   0.298   629.         0.836
#>  9 Greens      Daphnia      0.0334    0.0969 -0.157   0.219  1358.         0.634
#> 10 Greens      Unicells    -0.0144    0.0978 -0.208   0.169  1406.         0.443
#> # ℹ 3 more variables: prob_negative <dbl>, prob_nonzero <dbl>, sig <lgl>
```

The shocks of the unicellular algae and *Cryptomonas* correlate most
strongly, at 0.48 \[0.26, 0.69\]. The model holds no covariate that
would explain the shared rise.

The default, `covariance = "stationary"`, correlates the deviations
themselves. Each deviation accumulates the shocks of many months, passed
through $`A`$:

``` r

rc <- residual_cor(fit)
plot(rc)
```

![Posterior mean correlations among the deviations of the five taxa.
Outlined pairs have 95% credible intervals that exclude
zero.](var_files/figure-html/rc-stat-1.png)

Posterior mean correlations among the deviations of the five taxa.
Outlined pairs have 95% credible intervals that exclude zero.

The two correlations differ where $`A`$ couples the taxa. The shocks of
*Cryptomonas* and *Daphnia* correlate at 0.24 \[0.02, 0.44\]. Their
deviations correlate at 0.21 \[-0.02, 0.42\]. Grazing passes each shock
to the other taxon over the following months, which changes the
correlation the shared shocks create. $`A`$ barely couples the
unicellular algae and *Cryptomonas*. Their correlations are 0.48 \[0.26,
0.69\] for the shocks and 0.50 \[0.26, 0.70\] for the deviations.

## Impulse responses

An impulse response gives the expected path of every taxon after a
single shock to one of them (Lütkepohl
[2005](https://doi.org/10.1007/978-3-540-27752-1)).
[`irf()`](https://nicholasjclark.github.io/mvgam/dev/reference/irf.mvgam.md)
computes the generalised impulse response by default (Pesaran and Shin
[1998](https://doi.org/10.1016/S0165-1765(97)00214-0)). The shock to one
taxon brings with it the shocks that its correlated taxa receive in the
same month, in the proportions $`\Sigma`$ implies. Horizon 1 is the
month of the shock.

``` r

irfs <- irf(fit, h = 12)
```

[`plot()`](https://rdrr.io/r/graphics/plot.default.html) with
`series = 5` draws the responses to a shock in the fifth taxon,
*Daphnia*:

``` r

plot(irfs, series = 5)
```

![Generalised impulse responses of each taxon to a one standard
deviation shock in Daphnia, with 95% credible intervals. Horizon 1 is
the month of the shock.](var_files/figure-html/plot-irf-1.png)

Generalised impulse responses of each taxon to a one standard deviation
shock in Daphnia, with 95% credible intervals. Horizon 1 is the month of
the shock.

A shock raises *Daphnia* by 0.42 \[0.35, 0.50\] in its first month. The
rise falls below 0.05 by month 4. Because the shocks of *Daphnia* and
*Cryptomonas* are correlated, *Cryptomonas* moves with *Daphnia* in the
first month, by 0.10 \[0.01, 0.19\]. Grazing then acts through $`A`$. In
the third month *Cryptomonas* is at -0.09 \[-0.17, 0.00\] relative to
its expected level. The unicellular algae change by -0.13 \[-0.24,
-0.03\] in the second month. Every response stays within 0.01 of zero
from month 7.

## Variance decompositions

A forecast error variance decomposition (FEVD) splits the variance of
each taxon’s forecast among the taxa whose shocks generate it, at each
horizon.
[`fevd()`](https://nicholasjclark.github.io/mvgam/dev/reference/fevd.mvgam.md)
splits the variance with the Cholesky factor of $`\Sigma`$, taken in the
order of the levels of `series`. Where the shocks of two taxa correlate,
the factor assigns the shared part to whichever taxon comes first. The
diatoms come first and keep all of their own shock.

``` r

fevds <- fevd(fit, h = 12)
plot(fevds)
```

![Median share of each taxon's forecast error variance that each source
of shocks generates, by forecast horizon. Panels are the taxa forecast.
Colours are the taxa whose shocks generate the
variance.](var_files/figure-html/fevd-1.png)

Median share of each taxon’s forecast error variance that each source of
shocks generates, by forecast horizon. Panels are the taxa forecast.
Colours are the taxa whose shocks generate the variance.

For every taxon but *Cryptomonas*, its own shocks generate most of its
forecast variance. At twelve months those own shares run from 68% to
91%. For *Cryptomonas*, the unicellular algae generate 18% of its
forecast variance in the first month and 23% by month twelve. Most of
that share comes from the correlation of the two taxa’s shocks. The
ordering assigns it to the unicellular algae because they come first.
The coupling of the two taxa appears in the *Daphnia* panel, where
*Cryptomonas* shocks generate 16% of the twelve-month forecast variance.
Reordering the levels of `series` and refitting would move the shared
part of each correlated pair to the other taxon. The generalised impulse
responses above are the same in any order.

## Stability of the community

[`stability()`](https://nicholasjclark.github.io/mvgam/dev/reference/stability.mvgam.md)
summarises the dynamics in $`A`$ as community stability metrics (Ives et
al.
[2003](https://doi.org/10.1890/0012-9615(2003)073%5B0301:ECSAEI%5D2.0.CO;2)).
Reactivity is the log of the largest singular value of $`A`$. Above
zero, some perturbation grows in the month after it occurs before the
community returns towards its means (Neubert et al.
[2009](https://doi.org/10.1890/08-2014.1)). The mean return rate is the
largest modulus of the eigenvalues of $`A`$. Once the slowest mode
dominates, the distance from the means shrinks by about this factor each
month. The variance return rate is its square. It describes how fast the
forecast variance settles.

``` r

stab <- stability(fit)
plot(stab)
```

![Posterior distributions of reactivity and of the mean and variance
return rates. The dashed line marks zero
reactivity.](var_files/figure-html/stability-1.png)

Posterior distributions of reactivity and of the mean and variance
return rates. The dashed line marks zero reactivity.

Reactivity is -0.15 \[-0.38, 0.43\], with 81% of the posterior below
zero. The data favour a community that damps every perturbation from the
first month. They do not exclude one that briefly amplifies some
perturbations. The mean return rate is 0.65 \[0.54, 0.77\]. At the
median, a deviation halves in about 1.6 months. The plankton community
returns to its seasonal and nutrient-driven means within a season.

## Forecasting the held-out year

Passing `newdata` at fitting stored 1994 on the model.
[`forecast()`](https://generics.r-lib.org/reference/forecast.html)
projects the fitted states forward through those twelve months and adds
observation error:

``` r

fc <- forecast(fit)
plot(fc)
```

![Fitted values to the end of 1993 and forecasts for 1994 (right of the
dashed line), with observations as
points.](var_files/figure-html/forecast-1.png)

Fitted values to the end of 1993 and forecasts for 1994 (right of the
dashed line), with observations as points.

[`score()`](https://nicholasjclark.github.io/mvgam/dev/reference/score.md)
compares the forecast distributions with the 1994 observations. The
continuous ranked probability score (CRPS) measures the distance between
a forecast distribution and the observation, in the units of the data.
Lower is better. `in_interval` records whether the observation fell
inside the 90% forecast interval:

``` r

sc <- score(fc, score = "crps")
data.frame(
  taxon = taxa,
  mean_crps = vapply(taxa, function(t) mean(sc[[t]]$score, na.rm = TRUE),
                     numeric(1)),
  coverage = vapply(taxa, function(t) mean(sc[[t]]$in_interval,
                                           na.rm = TRUE), numeric(1)),
  row.names = NULL
)
#>         taxon mean_crps  coverage
#> 1     Diatoms 0.3845181 0.9166667
#> 2      Greens 0.4629722 1.0000000
#> 3    Unicells 0.3105877 1.0000000
#> 4 Cryptomonas 0.3413625 0.8333333
#> 5     Daphnia 0.2364582 1.0000000
```

Of the 60 observations in 1994, 57 fell inside their 90% intervals,
close to the coverage the intervals claim. The CRPS compares models
scored on the same data, such as this VAR against an independent
autoregression for each taxon. The [forecast evaluation
article](https://nicholasjclark.github.io/mvgam/dev/articles/forecast_evaluation.md)
works through that comparison with leave-future-out cross-validation.

The plankton of Lake Washington rise and fall mostly with the seasons
and with phosphorus. In this fit, 2 of the twenty lagged interactions
among the taxa separate from zero. The community damps a perturbation
within a season. The forecasts for the held-out year keep close to their
stated coverage.

## Where to next

Each function used here has a help page, such as
[`?irf`](https://nicholasjclark.github.io/mvgam/dev/reference/irf.mvgam.md)
or
[`?residual_cor`](https://nicholasjclark.github.io/mvgam/dev/reference/residual_cor.md),
with its arguments and output.
[`how_to_cite()`](https://nicholasjclark.github.io/mvgam/dev/reference/how_to_cite.mvgam.md)
writes a methods paragraph for a fitted model, with its references.

Related articles:

- [Hierarchical
  VARs](https://nicholasjclark.github.io/mvgam/dev/articles/hierarchical_var.md)
  fit one VAR per group of series, such as one per lake. Each group’s
  $`A`$ and $`\Sigma`$ are drawn towards a shared mean.
- [Dynamic factor
  models](https://nicholasjclark.github.io/mvgam/dev/articles/dfm.md)
  describe many series with fewer latent processes than series.
- [Forecast
  evaluation](https://nicholasjclark.github.io/mvgam/dev/articles/forecast_evaluation.md)
  covers scoring rules and leave-future-out cross-validation.
- [mvgam
  overview](https://nicholasjclark.github.io/mvgam/dev/articles/mvgam_overview.md)
  introduces the observation and trend formulas.
- [Formatting
  data](https://nicholasjclark.github.io/mvgam/dev/articles/data.md)
  covers the long data format and
  [`mvgam_data()`](https://nicholasjclark.github.io/mvgam/dev/reference/mvgam_data.md).

Chapter 8 of the [Applied Time Series
Analysis](https://atsa-es.github.io/atsa-labs/chap-msscov.html) lab book
fits multivariate autoregressive state-space models to the same Lake
Washington plankton with MARSS.

## References

Arel-Bundock V, Greifer N and Heiss A (2024). How to interpret
statistical models using marginaleffects for R and Python. *Journal of
Statistical Software* 111(9), 1-32.
[doi:10.18637/jss.v111.i09](https://doi.org/10.18637/jss.v111.i09)

Auger-Méthé M, Newman K, Cole D, Empacher F, Gryba R, King AA,
Leos-Barajas V, Mills Flemming J, Nielsen A, Petris G and Thomas L
(2021). A guide to state-space modeling of ecological time series.
*Ecological Monographs* 91(4), e01470.
[doi:10.1002/ecm.1470](https://doi.org/10.1002/ecm.1470)

Edmondson WT and Lehman JT (1981). The effect of changes in the nutrient
income on the condition of Lake Washington. *Limnology and Oceanography*
26(1), 1-29.
[doi:10.4319/lo.1981.26.1.0001](https://doi.org/10.4319/lo.1981.26.1.0001)

Heaps SE (2023). Enforcing stationarity through the prior in vector
autoregressions. *Journal of Computational and Graphical Statistics*
32(1), 74-83.
[doi:10.1080/10618600.2022.2079648](https://doi.org/10.1080/10618600.2022.2079648)

Holmes EE, Ward EJ and Wills K (2012). MARSS: Multivariate
autoregressive state-space models for analyzing time-series data. *The R
Journal* 4(1), 11-19.
[doi:10.32614/RJ-2012-002](https://doi.org/10.32614/RJ-2012-002)

Ives AR, Dennis B, Cottingham KL and Carpenter SR (2003). Estimating
community stability and ecological interactions from time-series data.
*Ecological Monographs* 73(2), 301-330.
[doi:10.1890/0012-9615(2003)073\[0301:ECSAEI\]2.0.CO;2](https://doi.org/10.1890/0012-9615(2003)073%5B0301:ECSAEI%5D2.0.CO;2)

Lütkepohl H (2005). *New Introduction to Multiple Time Series Analysis*.
Springer, Berlin.
[doi:10.1007/978-3-540-27752-1](https://doi.org/10.1007/978-3-540-27752-1)

Neubert MG, Caswell H and Solow AR (2009). Detecting reactivity.
*Ecology* 90(10), 2683-2688.
[doi:10.1890/08-2014.1](https://doi.org/10.1890/08-2014.1)

Pesaran HH and Shin Y (1998). Generalized impulse response analysis in
linear multivariate models. *Economics Letters* 58(1), 17-29.
[doi:10.1016/S0165-1765(97)00214-0](https://doi.org/10.1016/S0165-1765(97)00214-0)
