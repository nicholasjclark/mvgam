
<!-- README.md is generated from README.Rmd. Please edit that file -->
# *mvgam*

The goal of `mvgam` is to use a Bayesian framework to estimate parameters of Generalised Additive Models for discrete time series with dynamic trend components.

## Installation

Install the development version from `GitHub` using: `devtools::install_github("nicholasjclark/mvgam")`

## Brief introduction to the package

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

Now fit an `mvgam` model; it fits a GAM in which a cyclic smooth function for `season` is estimated jointly with a full time series model for the errors (in this case an `AR3` process with drift), rather than relying on smoothing splines that do not incorporate a concept of the future. We assume the outcome follows a Poisson distribution and estimate the model in `JAGS` using MCMC sampling (Note that `JAGS` 4.3.0 is required; installation links are found [here](https://sourceforge.net/projects/mcmc-jags/files/))

``` r
lynx_mvgam <- mvjagam(data_train = lynx_train,
               data_test = lynx_test,
               formula = y ~ s(season, bs = 'cc', k = 19),
               knots = list(season = c(0.5, 19.5)),
               family = 'poisson',
               trend_model = 'AR3',
               n.burnin = 5000,
               n.iter = 5000,
               thin = 5,
               auto_update = F)
#> module glm loaded
#> Compiling model graph
#>    Resolving undeclared variables
#>    Allocating nodes
#> Graph information:
#>    Observed stochastic nodes: 50
#>    Unobserved stochastic nodes: 137
#>    Total graph size: 2304
#> 
#> Initializing model
```

Perform a series of posterior predictive checks to see if the model is able to simulate data for the training period that looks realistic and unbiased. First, examine simulated kernel densities for posterior predictions (`yhat`) and compare to the density of the observations (`y`)

``` r
plot_mvgam_ppc(lynx_mvgam, series = 1, type = 'density')
```

<img src="README-unnamed-chunk-7-1.png" style="display: block; margin: auto;" />

Now plot the distribution of predicted means compared to the observed mean

``` r
plot_mvgam_ppc(lynx_mvgam, series = 1, type = 'mean')
```

<img src="README-unnamed-chunk-8-1.png" style="display: block; margin: auto;" />

Next examine simulated empirical Cumulative Distribution Functions (CDF) for posterior predictions (`yhat`) and compare to the CDF of the observations (`y`)

``` r
plot_mvgam_ppc(lynx_mvgam, series = 1, type = 'cdf')
```

<img src="README-unnamed-chunk-9-1.png" style="display: block; margin: auto;" />

Finally look for any biases in predictions by examining a Probability Integral Transform (PIT) histogram. If our predictions are not biased one way or another (i.e. not consistently under- or over-predicting), this histogram should look roughly uniform

``` r
plot_mvgam_ppc(lynx_mvgam, series = 1, type = 'pit')
```

<img src="README-unnamed-chunk-10-1.png" style="display: block; margin: auto;" />

All of these plots indicate the model is well calibrated against the training data, with no apparent pathological behaviors exhibited. Have a look at this model's summary to see what is being estimated (note that longer MCMC runs would probably be needed to increase effective sample sizes)

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
#> s(season) 16.18  17.00  121.3  <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> GAM coefficient (beta) estimates:
#>                    2.5%        50%      97.5% Rhat n.eff
#> (Intercept)   6.3449002  6.7480489  7.0482336 3.95    13
#> s(season).1  -1.1789197 -0.7566967 -0.2386666 1.18    37
#> s(season).2  -0.2493787  0.1381873  0.5732121 1.16    44
#> s(season).3   0.3905161  0.8945659  1.3764916 1.32    20
#> s(season).4   0.7251234  1.5566877  2.3799051 1.78    12
#> s(season).5   1.3983219  1.9341548  2.5964346 1.94    15
#> s(season).6   0.7235234  1.2986331  1.7332518 1.31    35
#> s(season).7  -0.4036282  0.1331384  0.7543562 1.22    31
#> s(season).8  -0.9916748 -0.4196393  0.3429022 1.77    35
#> s(season).9  -1.3992386 -0.6173552  0.5370080 1.46    18
#> s(season).10 -1.0809559 -0.2660351  0.9562298 1.36    10
#> s(season).11 -0.2506481  0.4547052  1.5527995 1.69    19
#> s(season).12  0.7779441  1.2005663  2.1971282 1.99    24
#> s(season).13  0.8842195  1.2713717  1.8881489 1.47    30
#> s(season).14  0.3210921  0.8836766  1.3980610 1.11    14
#> s(season).15 -0.9001341 -0.1549425  0.6075921 1.03    13
#> s(season).16 -1.6947464 -0.8471082 -0.2808164 1.04    17
#> s(season).17 -1.8245422 -1.1256415 -0.7124639 1.12    32
#> 
#> GAM smoothing parameter (rho) estimates:
#>               2.5%      50%    97.5% Rhat n.eff
#> s(season) 3.409171 4.221702 4.951053    1   787
#> 
#> Latent trend drift (phi) and AR parameter estimates:
#>           2.5%           50%    97.5% Rhat n.eff
#> phi  0.0000000  0.0000000000 0.000000  NaN     0
#> ar1  0.4573075  0.7888400570 1.122021 1.05   147
#> ar2 -0.4877526 -0.1318654485 0.209148 1.00  1175
#> ar3 -0.3136362 -0.0001508825 0.322628 1.07   114
#> 
```

Inspect the model's estimated smooth for the 19-year cyclic pattern. Note that the `mvgam` smooth plot is on a different scale compared to `mgcv` plots, but interpretation is similar

``` r
plot_mvgam_smooth(lynx_mvgam, 1, 'season')
```

<img src="README-unnamed-chunk-12-1.png" style="display: block; margin: auto;" />

We can also view the mvgam's posterior retrodictions and predictions for the entire series (testing and training)

``` r
plot_mvgam_fc(lynx_mvgam, data_test = lynx_test)
```

<img src="README-unnamed-chunk-13-1.png" style="display: block; margin: auto;" />

And the estimated latent trend component

``` r
plot_mvgam_trend(lynx_mvgam, data_test = lynx_test)
```

<img src="README-unnamed-chunk-14-1.png" style="display: block; margin: auto;" />

We can also re-do the posterior predictive checks, but this time focusing only on the out of sample period. This will give us better insight into how the model is performing and whether it is able to simulate realistic and unbiased future values

``` r
plot_mvgam_ppc(lynx_mvgam, series = 1, type = 'density', data_test = lynx_test)
```

<img src="README-unnamed-chunk-15-1.png" style="display: block; margin: auto;" />

``` r
plot_mvgam_ppc(lynx_mvgam, series = 1, type = 'mean', data_test = lynx_test)
```

<img src="README-unnamed-chunk-16-1.png" style="display: block; margin: auto;" />

``` r
plot_mvgam_ppc(lynx_mvgam, series = 1, type = 'cdf', data_test = lynx_test)
```

<img src="README-unnamed-chunk-17-1.png" style="display: block; margin: auto;" />

``` r
plot_mvgam_ppc(lynx_mvgam, series = 1, type = 'pit', data_test = lynx_test)
```

<img src="README-unnamed-chunk-18-1.png" style="display: block; margin: auto;" />

A key aspect of ecological forecasting is to understand [how different components of a model contribute to forecast uncertainty](https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/eap.1589). We can estimate relative contributions to forecast uncertainty for the GAM component and the latent trend component using `mvgam`

``` r
plot_mvgam_uncertainty(lynx_mvgam, data_test = lynx_test)
```

<img src="README-unnamed-chunk-19-1.png" style="display: block; margin: auto;" />

Both components contribute to forecast uncertainty, suggesting we would still need some more work to learn about factors driving the dynamics of the system. But we will leave the model as-is for this example. Diagnostics of the model can also be performed using `mvgam`. Have a look at the model's residuals, which are posterior medians of Dunn-Smyth randomised quantile residuals so should follow approximate normality. We are primarily looking for a lack of autocorrelation, which would suggest our AR3 model is appropriate for the latent trend

``` r
plot_mvgam_resids(lynx_mvgam)
```

<img src="README-unnamed-chunk-20-1.png" style="display: block; margin: auto;" />

Another useful utility of `mvgam` is the ability to use rolling window forecasts to evaluate competing models that may represent different hypotheses about the series dynamics. Here we will fit a poorly specified model to showcase how this evaluation works. In this model, we ignore the cyclic pattern of seasonality and force it to be fairly non-wiggly. We also use a random walk process for the trend

``` r
lynx_mvgam_poor <- mvjagam(data_train = lynx_train,
               data_test = lynx_test,
               formula = y ~ s(season, bs = 'gp', k = 3),
               family = 'poisson',
               trend_model = 'RW',
               n.burnin = 5000,
               n.iter = 5000,
               thin = 5,
               auto_update = F)
#> Compiling model graph
#>    Resolving undeclared variables
#>    Allocating nodes
#> Graph information:
#>    Observed stochastic nodes: 50
#>    Unobserved stochastic nodes: 135
#>    Total graph size: 1053
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
#>   2.634  22.887 113.332 113.968 168.403 284.457
summary(mod2_eval$series1$drps)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   54.54   68.04  301.74  300.64  435.01  723.71
```

Nominal coverages for both models' 90% prediction intervals

``` r
mean(mod1_eval$series1$in_interval)
#> [1] 1
mean(mod2_eval$series1$in_interval)
#> [1] 1
```

The `compare_mvgams` function automates this process by rolling along a set of timepoints for each model, ensuring a more in-depth evaluation of each competing model at the same set of timepoints. There are many more extended uses for `mvgam` models, including the ability to fit dynamic factor processes for analysing and forecasting sets of multivariate discrete time series
