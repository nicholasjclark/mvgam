
<!-- README.md is generated from README.Rmd. Please edit that file -->
# *mvgam*

The goal of `mvgam` is to use a Bayesian framework to estimate parameters of Generalised Additive Models for discrete time series with dynamic trend components. The motivation for the package and some of its primary objectives are described in detail by [Clark & Wells 2022](https://www.biorxiv.org/content/10.1101/2022.02.22.481550v1), with additional inspiration on the use of Bayesian probabilistic modelling to quantify uncertainty and advise principled decision making coming from [Michael Betancourt](https://betanalpha.github.io/writing/), [Michael Dietze](https://www.bu.edu/earth/profiles/michael-dietze/) and [Emily Fox](https://emilybfox.su.domains/), among many others.

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
#> Loading required package: runjags
#> Warning: package 'runjags' was built under R version 4.0.5
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

For `mvgam` models, the response needs to be labelled `y` and we also need an indicator of the series name as a `factor` variable (if the column `series` is missing, this will be added automatically by assuming that all observations are from a single time series). Finally, a `time` column is needed to index time

``` r
lynx_full$y <- lynx_full$population
lynx_full$time <- 1:NROW(lynx_full)
lynx_full$series <- factor('series1')
```

Split the data into training (first 50 years) and testing (next 10 years of data) to evaluate multi-step ahead forecasts

``` r
lynx_train = lynx_full[1:50, ]
lynx_test = lynx_full[51:60, ]
```

Now fit an `mvgam` model; it fits a GAM in which a cyclic smooth function for `season` is estimated jointly with a full time series model for the errors (in this case an `AR2` process), rather than relying on smoothing splines that do not incorporate a concept of the future. We assume the outcome follows a Poisson distribution and estimate the model in `JAGS` using MCMC sampling (installation links are found [here](https://sourceforge.net/projects/mcmc-jags/files/))

``` r
lynx_mvgam <- mvjagam(data_train = lynx_train,
               data_test = lynx_test,
               formula = y ~ s(season, bs = 'cc', k = 19),
               knots = list(season = c(0.5, 19.5)),
               family = 'poisson',
               trend_model = 'AR2',
               drift = F,
               burnin = 20000,
               chains = 4)
#> NOTE: Stopping adaptation
```

Perform a series of posterior predictive checks to see if the model is able to simulate data for the training period that looks realistic and unbiased. First, examine simulated kernel densities for posterior predictions (`yhat`) and compare to the density of the observations (`y`)

``` r
ppc(lynx_mvgam, series = 1, type = 'density')
```

<img src="README-unnamed-chunk-7-1.png" style="display: block; margin: auto;" />

Now plot the distribution of predicted means compared to the observed mean

``` r
ppc(lynx_mvgam, series = 1, type = 'mean')
```

<img src="README-unnamed-chunk-8-1.png" style="display: block; margin: auto;" />

Next examine simulated empirical Cumulative Distribution Functions (CDF) for posterior predictions (`yhat`) and compare to the CDF of the observations (`y`)

``` r
ppc(lynx_mvgam, series = 1, type = 'cdf')
```

<img src="README-unnamed-chunk-9-1.png" style="display: block; margin: auto;" />

Rootograms are becoming [popular graphical tools for checking a discrete model's ability to capture dispersion properties of the response variable](https://arxiv.org/pdf/1605.01311.pdf). Posterior predictive hanging rootograms can be displayed using the `ppc()` function in `mvgam`. In the plot below, we bin the unique observed values into `25` bins to prevent overplotting and help with interpretation. This plot compares the frequencies of observed vs predicted values for each bin, which can help to identify aspects of poor model fit. For example, if the gray bars (representing observed frequencies) tend to stretch below zero, this suggests the model's simulations predict the values in that particular bin less frequently than they are observed in the data. A well-fitting model that can generate realistic simulated data will provide a rootogram in which the lower boundaries of the grey bars are generally near zero

``` r
ppc(lynx_mvgam, series = 1, type = 'rootogram', n_bins = 25)
```

<img src="README-unnamed-chunk-10-1.png" style="display: block; margin: auto;" />

Finally look for any biases in predictions by examining a Probability Integral Transform (PIT) histogram. If our predictions are not biased one way or another (i.e. not consistently under- or over-predicting), this histogram should look roughly uniform

``` r
ppc(lynx_mvgam, series = 1, type = 'pit')
```

<img src="README-unnamed-chunk-11-1.png" style="display: block; margin: auto;" />

All of these plots indicate the model is well calibrated against the training data, with no apparent pathological behaviors exhibited. Have a look at this model's summary to see what is being estimated (note that longer MCMC runs would probably be needed to increase effective sample sizes)

``` r
summary(lynx_mvgam)
#> GAM formula:
#> y ~ s(season, bs = "cc", k = 19)
#> 
#> Family:
#> Poisson
#> 
#> Link function:
#> log
#> 
#> Trend model:
#> AR2
#> 
#> N series:
#> 1
#> 
#> N observations per series:
#> 50
#> 
#> Status:
#> Fitted using runjags::run.jags()
#> 
#> GAM smooth term approximate significances:
#>             edf Ref.df Chi.sq p-value    
#> s(season) 16.35  17.00  433.4  <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> GAM coefficient (beta) estimates:
#>                    2.5%         50%       97.5% Rhat n.eff
#> (Intercept)   6.6499895  6.77282057  6.90965261 1.08   110
#> s(season).1  -1.1945043 -0.70316805 -0.19353191 1.02    96
#> s(season).2  -0.3763193  0.23635487  0.77710126 1.04    75
#> s(season).3   0.4134563  1.07370823  1.77251522 1.09    28
#> s(season).4   1.0802667  1.71896480  2.35842720 1.26    35
#> s(season).5   1.2126016  1.95225696  2.61451653 1.42    36
#> s(season).6   0.3503449  1.03612011  1.76100663 1.12    34
#> s(season).7  -0.8934572 -0.25798609  0.53776811 1.02    57
#> s(season).8  -1.4306093 -0.76108780  0.01181555 1.02    71
#> s(season).9  -1.6207012 -0.90227747 -0.14843757 1.03   117
#> s(season).10 -1.1686178 -0.51409174  0.18910762 1.05   114
#> s(season).11 -0.3763644  0.31820721  1.06359377 1.18    78
#> s(season).12  0.6732042  1.39332811  2.13882995 1.41    43
#> s(season).13  0.8827504  1.63372452  2.31486153 1.38    28
#> s(season).14  0.6215632  1.24724055  1.99972057 1.07    28
#> s(season).15 -0.5936375  0.04815016  0.65978588 1.05    95
#> s(season).16 -1.3435105 -0.75309726 -0.13946592 1.12   143
#> s(season).17 -1.6023979 -1.06368932 -0.52991254 1.11   150
#> 
#> GAM smoothing parameter (rho) estimates:
#>               2.5%      50%   97.5% Rhat n.eff
#> s(season) 3.096734 3.871571 4.53649 1.01  2063
#> 
#> Latent trend drift (phi) and AR parameter estimates:
#>           2.5%         50%     97.5% Rhat n.eff
#> phi  0.0000000  0.00000000 0.0000000  NaN     0
#> ar1  0.4092519  0.72030248 1.0068403    1   906
#> ar2 -0.3893921 -0.09838423 0.2094005    1   785
#> ar3  0.0000000  0.00000000 0.0000000  NaN     0
#> 
```

The `plot_mvgam_...()` functions offer more flexibility than the generic `S3 plot.mvgam()` functions. For example, we can inpsect traceplots when sampling from a posterior with `MCMC` methods. Here for the `GAM` component (smoothing parameters).

``` r
plot_mvgam_trace(lynx_mvgam, 'rho')
```

<img src="README-unnamed-chunk-13-1.png" style="display: block; margin: auto;" />

and for the latent trend component parameters

``` r
plot_mvgam_trace(lynx_mvgam, 'trend')
```

<img src="README-unnamed-chunk-14-1.png" style="display: block; margin: auto;" /><img src="README-unnamed-chunk-14-2.png" style="display: block; margin: auto;" />

Inspect the model's estimated smooth for the 19-year cyclic pattern, which is shown as a ribbon plot of posterior empirical quantiles. We can also overlay posterior quantiles of partial residuals (shown as ribbon rectangles in red), which represent the leftover variation that the model expects would remain if this smooth term was dropped but all other parameters remained unchanged. Note that these are on a different scale to those from `mgcv::plot.gam` as these are randomised quantile residuals that are essentially standard normal in distribution. But either way, a strong pattern in the partial residuals suggests there would be strong patterns left unexplained in the model *if* we were to drop this term, giving us further confidence that this function is important in the model

``` r
plot(lynx_mvgam, type = 'smooths', residuals = T)
```

<img src="README-unnamed-chunk-15-1.png" style="display: block; margin: auto;" />

First derivatives of smooth functions can also be plotted to inspect how the slope of the function changes across its length. To plot these we use the more flexible `plot_mvgam_smooth()` function

``` r
plot_mvgam_smooth(lynx_mvgam, 1, 'season', derivatives = T)
```

<img src="README-unnamed-chunk-16-1.png" style="display: block; margin: auto;" />

We can also view the mvgam's posterior retrodictions and predictions for the entire series (testing and training)

``` r
plot(lynx_mvgam, type = 'forecast', data_test = lynx_test)
```

<img src="README-unnamed-chunk-17-1.png" style="display: block; margin: auto;" />

And the estimated latent trend component, again using the more flexible `plot_mvgam_...()` option to show first derivatives of the estimated trend

``` r
plot_mvgam_trend(lynx_mvgam, data_test = lynx_test, derivatives = T)
```

<img src="README-unnamed-chunk-18-1.png" style="display: block; margin: auto;" />

We can also re-do the posterior predictive checks, but this time focusing only on the out of sample period. This will give us better insight into how the model is performing and whether it is able to simulate realistic and unbiased future values

``` r
ppc(lynx_mvgam, series = 1, type = 'density', data_test = lynx_test)
```

<img src="README-unnamed-chunk-19-1.png" style="display: block; margin: auto;" />

``` r
ppc(lynx_mvgam, series = 1, type = 'mean', data_test = lynx_test)
```

<img src="README-unnamed-chunk-20-1.png" style="display: block; margin: auto;" />

``` r
ppc(lynx_mvgam, series = 1, type = 'cdf', data_test = lynx_test)
```

<img src="README-unnamed-chunk-21-1.png" style="display: block; margin: auto;" />

``` r
ppc(lynx_mvgam, series = 1, type = 'pit', data_test = lynx_test)
```

<img src="README-unnamed-chunk-22-1.png" style="display: block; margin: auto;" />

A key aspect of ecological forecasting is to understand [how different components of a model contribute to forecast uncertainty](https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/eap.1589). We can estimate relative contributions to forecast uncertainty for the GAM component and the latent trend component using `mvgam`

``` r
plot_mvgam_uncertainty(lynx_mvgam, data_test = lynx_test, legend_position = 'none')
text(1, 0.2, cex = 1.5, label="GAM component", 
     pos = 4, col="white", family = 'serif')
text(1, 0.8, cex = 1.5, label="Trend component", 
     pos = 4, col="#7C0000", family = 'serif')
```

<img src="README-unnamed-chunk-23-1.png" style="display: block; margin: auto;" />

Both components contribute to forecast uncertainty, suggesting we would still need some more work to learn about factors driving the dynamics of the system. But we will leave the model as-is for this example. Diagnostics of the model can also be performed using `mvgam`. Have a look at the model's residuals, which are posterior medians of Dunn-Smyth randomised quantile residuals so should follow approximate normality. We are primarily looking for a lack of autocorrelation, which would suggest our AR2 model is appropriate for the latent trend

``` r
plot(lynx_mvgam, type = 'residuals')
```

<img src="README-unnamed-chunk-24-1.png" style="display: block; margin: auto;" />

Another useful utility of `mvgam` is the ability to use rolling window forecasts to evaluate competing models that may represent different hypotheses about the series dynamics. Here we will fit a poorly specified model to showcase how this evaluation works. In this model, we ignore the cyclic pattern of seasonality and force it to be fairly non-wiggly. We also use a random walk process for the trend

``` r
lynx_mvgam_poor <- mvjagam(data_train = lynx_train,
               data_test = lynx_test,
               formula = y ~ s(season, bs = 'gp', k = 3),
               family = 'poisson',
               trend_model = 'RW',
               drift = FALSE,
               burnin = 20000,
               chains = 4)
#> NOTE: Stopping adaptation
```

We choose a set of timepoints within the training data to forecast from, allowing us to simulate a situation where the model's parameters had already been estimated but we have only observed data up to the evaluation timepoint and would like to generate forecasts from the latent trends. Here we use year 10 as our last observation and forecast ahead for the next 10 years.

``` r
mod1_eval <- eval_mvgam(lynx_mvgam, eval_timepoint = 10, fc_horizon = 10)
mod2_eval <- eval_mvgam(lynx_mvgam_poor, eval_timepoint = 10, fc_horizon = 10)
```

Summary statistics of the two models' out of sample Discrete Rank Probability Score (DRPS) indicate that the well-specified model performs markedly better (far lower DRPS) for this evaluation timepoint

``` r
summary(mod1_eval$series1$drps)
#>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#>   0.3049   5.3115  98.2153  89.0318 115.8889 226.8302
summary(mod2_eval$series1$drps)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   59.16   81.11  292.59  299.99  430.72  717.14
```

Nominal coverages for both models' 90% prediction intervals

``` r
mean(mod1_eval$series1$in_interval)
#> [1] 1
mean(mod2_eval$series1$in_interval)
#> [1] 1
```

The `compare_mvgams` function automates this process by rolling along a set of timepoints for each model, ensuring a more in-depth evaluation of each competing model at the same set of timepoints. There are many more extended uses for `mvgam` models, including the ability to fit dynamic factor processes for analysing and forecasting sets of multivariate discrete time series

# License

This project is licensed under an `MIT` open source license
