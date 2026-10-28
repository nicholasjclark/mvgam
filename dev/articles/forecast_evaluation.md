# Forecast evaluation in mvgam

A model that describes its training data well can still forecast poorly.
To find the model that forecasts best, mvgam scores each candidate’s
predictive distribution against observations the model did not see.
Every scoring rule used here is proper, in the sense that the true
predictive distribution minimises its expected value (Gneiting and
Raftery [2007](https://doi.org/10.1198/016214506000001437)). Lower
scores are better.

This article compares three models of monthly rodent captures, each with
a different trend. We score their forecasts of a twelve-month holdout,
repeat the comparison over rolling forecast origins and combine the
models into a weighted ensemble.

## The data

`portal_data` holds captures of four desert rodents at the Portal
Project site in southeastern Arizona, summed over the control plots in
each lunar month ([Portal Project
methods](https://github.com/weecology/PortalData/blob/main/SiteandMethods/Methods.md)).
The series are *Dipodomys merriami* (DM), *Dipodomys ordii* (DO),
*Chaetodipus baileyi* (PB) and *Chaetodipus penicillatus* (PP). Two
covariates accompany them: the monthly mean of minimum temperature
(`mintemp`) and a 12-month moving average of NDVI (`ndvi_ma12`). We hold
out the last 12 of the 80 months:

``` r

data(portal_data)
train <- subset(portal_data, time <= 68)
test <- subset(portal_data, time > 68)
```

## Three models

To isolate how the trend shapes a forecast, the three models share a
Poisson observation model, the two covariates and the sampler settings.
Each fit receives the holdout as `newdata`, from which
[`forecast()`](https://generics.r-lib.org/reference/forecast.html) later
takes its forecast times:

``` r

fit_portal <- function(formula, trend_formula = NULL) {
  mvgam(formula, trend_formula = trend_formula, family = poisson(),
        data = train, newdata = test, chains = 4, seed = 2026,
        silent = 2)
}
```

The first model is a generalised additive model with a smooth of time
for each species. Its forecast extends those smooths past the last
training month:

``` r

mod_gam <- fit_portal(
  captures ~ series + s(mintemp, k = 6) + s(ndvi_ma12, k = 6) +
    s(time, by = series, k = 8)
)
```

The second model gives each species a latent AR(1) state. Its covariates
move into `trend_formula`, where `by = lv_axis()` gives each species its
own smooth of temperature. `AR(cor = TRUE)` correlates the innovations
of the four states:

``` r

mod_ar <- fit_portal(
  captures ~ series,
  trend_formula = ~ s(mintemp, by = lv_axis(), k = 6) +
    s(ndvi_ma12, k = 6) + AR(cor = TRUE)
)
```

The third model replaces the AR(1) state with a piecewise linear trend.
Up to six changepoints per species fall in the first 80% of the training
months. As in Prophet (Taylor and Letham
[2018](https://doi.org/10.1080/00031305.2017.1380080)), the forecast
extends the final segment and adds new changepoints at the rate seen in
training:

``` r

mod_pw <- fit_portal(
  captures ~ series,
  trend_formula = ~ s(mintemp, by = lv_axis(), k = 6) +
    s(ndvi_ma12, k = 6) + PW(n_changepoints = 6)
)
```

## Forecasts

[`forecast()`](https://generics.r-lib.org/reference/forecast.html)
returns the predictive distribution of the captures in each held-out
month. Its [`plot()`](https://rdrr.io/r/graphics/plot.default.html)
method draws the forecast after the hindcast of the training months:

``` r

set.seed(2026)
fc_gam <- forecast(mod_gam)
fc_ar <- forecast(mod_ar)
fc_pw <- forecast(mod_pw)
```

``` r

plot(fc_gam)
```

![GAM hindcasts and forecasts of each species' captures, with 50%, 80%
and 95% credible intervals on the forecast. Points are the observed
captures. The dashed line marks the end of the training
data.](forecast_evaluation_files/figure-html/plot-gam-1.png)

GAM hindcasts and forecasts of each species’ captures, with 50%, 80% and
95% credible intervals on the forecast. Points are the observed
captures. The dashed line marks the end of the training data.

Each smooth of time extrapolates the slope at the end of its series. For
DO, whose captures rose over the final training months, the median
forecast reaches 126 captures by month 12. The held-out months recorded
at most 14. The AR forecast carries its latent state forward instead:

``` r

plot(fc_ar)
```

![AR hindcasts and forecasts of each species'
captures.](forecast_evaluation_files/figure-html/plot-ar-1.png)

AR hindcasts and forecasts of each species’ captures.

## Scoring the holdout

[`compare_scores()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_scores.md)
scores several forecasts with one rule and returns one row per model,
series and horizon. The continuous ranked probability score (CRPS)
generalises the absolute error to a predictive distribution. With
`relative`, [`plot()`](https://rdrr.io/r/graphics/plot.default.html)
draws each model’s score minus that of a baseline:

``` r

crps <- compare_scores(fc_gam, fc_ar, fc_pw, score = "crps",
                       model_names = c("GAM", "AR", "PW"))
plot(crps, relative = "GAM")
```

![CRPS of the AR and PW forecasts minus that of the GAM at each horizon.
Negative values favour the AR or PW model. No species was observed at
horizon 3.](forecast_evaluation_files/figure-html/crps-1.png)

CRPS of the AR and PW forecasts minus that of the GAM at each horizon.
Negative values favour the AR or PW model. No species was observed at
horizon 3.

Between AR and the GAM, the lower mean CRPS goes to AR on DM, AR on DO,
the GAM on PB and the GAM on PP. On DO the mean CRPS is 3.32 for AR and
33.70 for the GAM. PB and PP recorded few or no captures over long
stretches. Averaged over the species, AR scores 3.94 against 3.87 for
PW.

The energy score grades the four forecasts at each horizon as one
multivariate distribution (Gneiting et al.
[2008](https://doi.org/10.1007/s11749-008-0114-x)).
[`compare_scores()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_scores.md)
holds a joint score in the rows whose series is `all_series`:

``` r

energy <- compare_scores(fc_gam, fc_ar, fc_pw, score = "energy",
                         model_names = c("GAM", "AR", "PW"))
plot(energy)
```

![Energy score of the joint forecast of the four species at each
horizon.](forecast_evaluation_files/figure-html/energy-1.png)

Energy score of the joint forecast of the four species at each horizon.

The GAM’s energy score runs from 7 to 93 over the twelve months. AR and
PW stay below 13.
[`score()`](https://nicholasjclark.github.io/mvgam/dev/reference/score.md)
offers nine more rules, among them the variogram score for the
dependence between series and threshold-weighted scores for a range of
the response.

## Rolling forecast origins

The holdout scores one twelve-month forecast from one origin. To test
whether the ranking holds across origins,
[`lfo_cv()`](https://nicholasjclark.github.io/mvgam/dev/reference/lfo_cv.mvgam.md)
runs approximate leave-future-out cross-validation (Bürkner et al.
[2020](https://doi.org/10.1080/00949655.2020.1783262)). Starting from
the first `min_t` months, it forecasts the `fc_horizon` months that
follow each origin in turn. It scores each forecast by its expected log
predictive density (ELPD). Pareto-smoothed importance sampling reweights
the existing fit to each new origin. The model is refit only where the
Pareto $`k`$ diagnostic crosses its threshold. Leave-one-out
cross-validation with
[`loo()`](https://mc-stan.org/loo/reference/loo.html) suits these models
poorly. A latent state that the neighbouring months still inform
predicts each left-out month, which turns the comparison into one of
interpolation.

`save_log_lik = TRUE` keeps the pointwise log densities that the
stacking weights below require:

``` r

run_lfo <- function(mod) {
  lfo_cv(mod, min_t = 56, fc_horizon = 4, save_log_lik = TRUE,
         silent = 2)
}
lfo_gam <- run_lfo(mod_gam)
lfo_ar <- run_lfo(mod_ar)
lfo_pw <- run_lfo(mod_pw)
loo_compare(lfo_gam, lfo_ar, lfo_pw, model_names = c("GAM", "AR", "PW"))
#>  model elpd_diff se_diff p_worse diag_diff diag_elpd
#>     AR       0.0     0.0      NA                    
#>     PW     -41.4    10.4    1.00   N < 100          
#>    GAM     -75.2    12.7    1.00   N < 100
```

AR leads. PW trails it by 41 (SE 10) and GAM by 75 (SE 13). The log
score penalises an observation in the tail of a forecast far more
heavily than the CRPS does.
[`compare_elpds()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_elpds.md)
tabulates the ELPD at each origin:

``` r

elpds <- compare_elpds(lfo_gam, lfo_ar, lfo_pw,
                       model_names = c("GAM", "AR", "PW"))
plot(elpds, cumulative = TRUE)
```

![Cumulative ELPD over the forecast origins. Open circles mark the
origins at which the model was
refit.](forecast_evaluation_files/figure-html/lfo-plot-1.png)

Cumulative ELPD over the forecast origins. Open circles mark the origins
at which the model was refit.

## A weighted ensemble

Stacking chooses the weights that maximise the log score of the mixture
of the models’ predictive distributions (Yao et al.
[2018](https://doi.org/10.1214/17-BA1091)).
[`loo_model_weights()`](https://mc-stan.org/loo/reference/loo_model_weights.html)
computes them from the leave-future-out densities.
[`ensemble()`](https://nicholasjclark.github.io/mvgam/dev/reference/ensemble.mvgam_forecast.md)
combines the forecasts in those proportions.
[`compare_scores()`](https://nicholasjclark.github.io/mvgam/dev/reference/compare_scores.md)
then scores the ensemble with its components:

``` r

weights <- loo_model_weights(lfo_gam, lfo_ar, lfo_pw, method = "stacking",
                             model_names = c("GAM", "AR", "PW"))
weights
#> Method: stacking
#> ------
#>     weight
#> GAM 0.000 
#> AR  0.694 
#> PW  0.306
fc_ens <- ensemble(fc_gam, fc_ar, fc_pw, weights = as.numeric(weights),
                   seed = 2026)
crps_ens <- compare_scores(fc_gam, fc_ar, fc_pw, fc_ens, score = "crps",
                           model_names = c("GAM", "AR", "PW", "Ensemble"))
aggregate(score ~ model, FUN = mean,
          data = subset(crps_ens, series != "all_series"))
#>      model     score
#> 1      GAM 11.372691
#> 2       AR  3.939303
#> 3       PW  3.868367
#> 4 Ensemble  3.791555
```

Stacking gives 0 to the GAM, 0.69 to AR and 0.31 to PW. The ensemble’s
mean CRPS on the holdout is 3.79, the lowest of the four.

## Summary

The CRPS and the energy score compared the three forecasts on the
holdout. Under leave-future-out cross-validation AR led in ELPD. The
GAM’s smooths of time extrapolated the end of each series. Stacking
combined the three forecasts into an ensemble with a mean CRPS of 3.79.
Each step took one or two function calls on the fitted models. The CRPS
and the log score can rank models differently. Choose the scoring rule
before scoring any forecast.

## Where to next

Each function used here has a help page, such as
[`?score`](https://nicholasjclark.github.io/mvgam/dev/reference/score.md)
or
[`?lfo_cv`](https://nicholasjclark.github.io/mvgam/dev/reference/lfo_cv.mvgam.md),
with its arguments and output.
[`how_to_cite()`](https://nicholasjclark.github.io/mvgam/dev/reference/how_to_cite.mvgam.md)
writes a methods paragraph for each fitted model.
[`methods_md()`](https://nicholasjclark.github.io/mvgam/dev/reference/methods_md.md)
writes out its equations.

Related articles:

- [Vector
  autoregressions](https://nicholasjclark.github.io/mvgam/dev/articles/var.md)
  model the lagged effects of one species on another.
- [Hierarchical vector
  autoregressions](https://nicholasjclark.github.io/mvgam/dev/articles/hierarchical_var.md)
  pool dynamics across groups of series.
- [N-mixture
  models](https://nicholasjclark.github.io/mvgam/dev/articles/nmix.md)
  forecast abundance from counts with imperfect detection.

## References

Bürkner P-C, Gabry J and Vehtari A (2020). Approximate leave-future-out
cross-validation for Bayesian time series models. *Journal of
Statistical Computation and Simulation* 90(14), 2499-2523.
[doi:10.1080/00949655.2020.1783262](https://doi.org/10.1080/00949655.2020.1783262)

Gneiting T and Raftery AE (2007). Strictly proper scoring rules,
prediction, and estimation. *Journal of the American Statistical
Association* 102(477), 359-378.
[doi:10.1198/016214506000001437](https://doi.org/10.1198/016214506000001437)

Gneiting T, Stanberry LI, Grimit EP, Held L and Johnson NA (2008).
Assessing probabilistic forecasts of multivariate quantities, with an
application to ensemble predictions of surface winds. *TEST* 17(2),
211-235.
[doi:10.1007/s11749-008-0114-x](https://doi.org/10.1007/s11749-008-0114-x)

Taylor SJ and Letham B (2018). Forecasting at scale. *The American
Statistician* 72(1), 37-45.
[doi:10.1080/00031305.2017.1380080](https://doi.org/10.1080/00031305.2017.1380080)

Yao Y, Vehtari A, Simpson D and Gelman A (2018). Using stacking to
average Bayesian predictive distributions. *Bayesian Analysis* 13(3),
917-1003. [doi:10.1214/17-BA1091](https://doi.org/10.1214/17-BA1091)
