
<!-- README.md is generated from README.Rmd. Please edit that file -->
# *mvgam*

The goal of `mvgam` is to use a Bayesian framework to estimate parameters of Generalised Additive Models for discrete time series with dynamic trend components.

## Installation

Install the development version from `GitHub` using:

``` r
# install.packages("devtools")
devtools::install_github("nicholasjclark/mvgam")
```

## Brief Overview

We can explore the model’s primary functions using a test dataset that is available with all `R` installations. We introduce dynamic Generalised Additive Models and some of the key utility functions provided in `mvgam`. First, load the `lynx` data and plot the series as well as its estimated autocorrelation function

``` r
library(mvgam)
#> Loading required package: mgcv
#> Loading required package: nlme
#> This is mgcv 1.8-33. For overview type 'help("mgcv-package")'.
#> Loading required package: rjags
#> Loading required package: coda
#> Linked to JAGS 4.3.0
#> Loaded modules: basemod,bugs
#> Loading required package: parallel
data(lynx)
lynx_full = data.frame(year = 1821:1934, 
                       population = as.numeric(lynx))
plot(lynx_full$population, type = 'l', ylab = 'Lynx trappings',
     xlab = 'Time')
```

<img src="README-unnamed-chunk-2-1.png" style="display: block; margin: auto;" />

``` r
acf(lynx_full$population, main = '')
```

<img src="README-unnamed-chunk-2-2.png" style="display: block; margin: auto;" />

Along with serial autocorrelation, there is a clear ~19-year cyclic pattern to the data. Create a `season` term that can be used to model this effect and give a better representation of the data generating process than we would likely get with a linear model

``` r
plot(stl(ts(lynx_full$population, frequency = 19), s.window = 'periodic'))
```

<img src="README-unnamed-chunk-3-1.png" style="display: block; margin: auto;" />

``` r
lynx_full$season <- (lynx_full$year %%19) + 1
```

For `mvgam` models, the response needs to be labelled `y` and we also need an indicator of the series name as a `factor` variable

``` r
lynx_full$y <- lynx_full$population
lynx_full$series <- factor('series1')
```

Split the data into training (first 50 years) and testing (next 10 years of data) to evaluate multi-step ahead forecasts

``` r
lynx_train = lynx_full[1:50, ]
lynx_test = lynx_full[51:60, ]
```

Now fit an `mvgam` model; it fits a GAM in which a smooth function for `season` is estimated jointly with a full time series model for the errors (in this case an `AR3` process with drift), rather than relying on smoothing splines that do not incorporate a concept of the future. We estimate the model in `JAGS` using MCMC sampling (Note that `JAGS` 4.3.0 is required; installation links are found [here](https://sourceforge.net/projects/mcmc-jags/files/))

``` r
lynx_mvgam <- mvjagam(data_train = lynx_train,
               data_test = lynx_test,
               formula = y ~ s(season, bs = 'cc', k = 19),
               knots = list(season = c(0.5, 19.5)),
               family = 'poisson',
               trend_model = 'AR3',
               n.burnin = 20000,
               n.iter = 10000,
               thin = 10,
               auto_update = F)
#> module glm loaded
#> Compiling model graph
#>    Resolving undeclared variables
#>    Allocating nodes
#> Graph information:
#>    Observed stochastic nodes: 50
#>    Unobserved stochastic nodes: 138
#>    Total graph size: 2306
#> 
#> Initializing model
```

Calculate the out of sample forecast from the fitted `mvgam` model and visualise 95% and 68% credible intervals

``` r
fits <- MCMCvis::MCMCchains(lynx_mvgam$jags_output, 'ypred')
fits <- fits[,(NROW(lynx_mvgam$obs_data)+1):(NROW(lynx_mvgam$obs_data)+10)]
cred_ints <- apply(fits, 2, function(x) hpd(x, 0.95))
plot(cred_ints[3,] ~ seq(1:NCOL(cred_ints)), type = 'l',
     col = rgb(1,0,0, alpha = 0),
     ylim = c(0, max(lynx_full$population) * 1.25),
     ylab = '',
     xlab = 'Forecast horizon',
     main = 'mvgam')
polygon(c(seq(1:(NCOL(cred_ints))), rev(seq(1:NCOL(cred_ints)))),
        c(cred_ints[1,],rev(cred_ints[3,])),
        col = rgb(150, 0, 0, max = 255, alpha = 100), border = NA)
cred_ints <- apply(fits, 2, function(x) hpd(x, 0.68))
polygon(c(seq(1:(NCOL(cred_ints))), rev(seq(1:NCOL(cred_ints)))),
        c(cred_ints[1,],rev(cred_ints[3,])),
        col = rgb(150, 0, 0, max = 255, alpha = 180), border = NA)
lines(cred_ints[2,], col = rgb(150, 0, 0, max = 255), lwd = 2, lty = 'dashed')
points(lynx_test$population[1:10], pch = 16)
lines(lynx_test$population[1:10])
```

<img src="README-unnamed-chunk-7-1.png" style="display: block; margin: auto;" />

All out of sample observations falling within the model's 95% credible intervals. Have a look at this model's summary to see what is being estimated (note that longer MCMC runs would probably be needed to increase effective sample sizes)

``` r
summary_mvgam(lynx_mvgam)
#> GAM formula:
#> y ~ s(season, bs = "cc", k = 19)
#> 
#> Family:
#> Poisson
#> 
#> N series:
#> 1
#> 
#> N observations per series:
#> 50
#> 
#> GAM smooth term approximate significances:
#>             edf Ref.df Chi.sq p-value    
#> s(season) 14.65  17.00  34368  <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> GAM coefficient (beta) estimates:
#>                     2.5%         50%       97.5% Rhat n.eff
#> (Intercept)   4.71271818  5.38977865  5.71438601 1.34     8
#> s(season).1  -1.24893078 -0.70017392 -0.22140756 1.09    28
#> s(season).2  -0.68512250  0.05263288  0.53077006 1.56    32
#> s(season).3   0.15632523  0.76576091  1.32760063 2.40    30
#> s(season).4   0.51088995  1.46413594  2.10737993 2.19    16
#> s(season).5   0.66740000  1.72432191  2.25195823 1.47    12
#> s(season).6   0.42944644  0.98731419  1.45050480 1.01    21
#> s(season).7  -0.66336762 -0.13979425  0.42460534 1.44    47
#> s(season).8  -1.23574086 -0.70391390  0.12491007 1.63    50
#> s(season).9  -1.43320276 -0.87985732  0.06163018 1.64    67
#> s(season).10 -1.15580242 -0.52037596  0.21473580 1.18    58
#> s(season).11 -0.39583137  0.19095742  0.80166516 1.14    39
#> s(season).12 -0.02984552  1.13138753  1.89694238 1.84    11
#> s(season).13  0.01371487  1.41492001  2.24143676 2.00     8
#> s(season).14  0.20480910  1.25175497  2.03664358 1.89    13
#> s(season).15 -0.06548466  0.47659385  0.97812105 1.11    44
#> s(season).16 -0.89001125 -0.36838619  0.17516767 1.35    96
#> s(season).17 -1.46751003 -0.86631819 -0.26038164 1.93    48
#> 
#> GAM smoothing parameter (rho) estimates:
#>              2.5%      50%    97.5% Rhat n.eff
#> s(season) 3.85257 4.847141 6.274806 1.41   222
#> 
#> Latent trend drift and AR parameter estimates:
#>           2.5%         50%     97.5% Rhat n.eff
#> phi  0.1806876  0.47239893 0.8349388 1.25   400
#> ar1  0.5049776  0.80949145 1.1002874 1.21   711
#> ar2 -0.4467396 -0.12484886 0.2106545 1.00  1706
#> ar3 -0.3831465 -0.04621891 0.2270324 1.41   362
#> 
```

Inspect the model's estimated smooth for the 19-year cyclic pattern. Note that the `mvgam` smooth plot is on a different scale compared to `mgcv` plots, but interpretation is similar

``` r
plot_mvgam_smooth(lynx_mvgam, 1, 'season')
```

<img src="README-unnamed-chunk-9-1.png" style="display: block; margin: auto;" />

We can also view the mvgam's posterior predictions for the entire series (testing and training)

``` r
plot_mvgam_fc(lynx_mvgam, data_test = lynx_test)
```

<img src="README-unnamed-chunk-10-1.png" style="display: block; margin: auto;" />

And the estimated latent trend component

``` r
plot_mvgam_trend(lynx_mvgam, data_test = lynx_test)
```

<img src="README-unnamed-chunk-11-1.png" style="display: block; margin: auto;" />

A key aspect of ecological forecasting is to understand [how different components of a model contribute to forecast uncertainty](https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/eap.1589). We can estimate contributions to forecast uncertainty for the GAM smooth functions and the latent trend using `mvgam`

``` r
plot_mvgam_uncertainty(lynx_mvgam, data_test = lynx_test)
```

<img src="README-unnamed-chunk-12-1.png" style="display: block; margin: auto;" />

Both components contribute to forecast uncertainty, suggesting we would still need some more work to learn about factors driving the dynamics of the system. But we will leave the model as-is for this example. Diagnostics of the model can also be performed using `mvgam`. Have a look at the model's residuals, which are posterior medians of Dunn-Smyth randomised quantile residuals so should follow approximate normality. We are primarily looking for a lack of autocorrelation, which would suggest our AR2 model is appropriate for the latent trend

``` r
plot_mvgam_resids(lynx_mvgam)
```

<img src="README-unnamed-chunk-13-1.png" style="display: block; margin: auto;" />

Another useful utility of `mvgam` is the ability to use rolling window forecasts to evaluate competing models that may represent different hypotheses about the series dynamics. Here we will fit a poorly specified model to showcase how this evaluation works

``` r
lynx_mvgam_poor <- mvjagam(data_train = lynx_train,
               data_test = lynx_test,
               formula = y ~ s(season, bs = 'gp', k = 3),
               family = 'poisson',
               trend_model = 'RW',
               n.burnin = 10000,
               n.iter = 5000,
               thin = 5,
               auto_update = F)
#> Compiling model graph
#>    Resolving undeclared variables
#>    Allocating nodes
#> Graph information:
#>    Observed stochastic nodes: 50
#>    Unobserved stochastic nodes: 136
#>    Total graph size: 1055
#> 
#> Initializing model
```

We choose a set of timepoints within the training data to forecast from, allowing us to simulate a situation where the model's parameters had already been estimated but we have only observed data up to the evaluation timepoint and would like to generate forecasts from the latent trends. Here we use year 10 as our last observation and forecast ahead for the next 10 years.

``` r
mod1_eval <- eval_mvgam(lynx_mvgam, eval_timepoint = 10, fc_horizon = 10)
mod2_eval <- eval_mvgam(lynx_mvgam_poor, eval_timepoint = 10, fc_horizon = 10)
```

Summary statistics of the two models' out of sample Discrete Rank Probability Score (DRPS) indicate that the well-specified model performs markedly better (far lower DRPS) for this evaluation timepoint

``` r
summary(mod1_eval$series1$drps)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   5.384  18.755  74.924  92.254 144.125 221.072
summary(mod2_eval$series1$drps)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   3.605 102.387 352.623 396.140 671.536 902.525
```

Nominal coverages for both models' 90% prediction intervals

``` r
mean(mod1_eval$series1$in_interval)
#> [1] 1
mean(mod2_eval$series1$in_interval)
#> [1] 0.7
```

The `compare_mvgams` function automates this process by rolling along a set of timepoints for each model, ensuring a more in-depth evaluation of each competing model at the same set of timepoints. There are many more extended uses for `mvgam` models, including the ability to fit dynamic factor processes for analysing and forecasting sets of multivariate discrete time series
