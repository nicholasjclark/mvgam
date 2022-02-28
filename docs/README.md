
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

For `mvgam` models, the response needs to be labelled `y` and we also need an indicator of the series name as a `factor` variable (if the column `series` is missing, this will be added automatically by assuming that all observations are from a single time series)

``` r
lynx_full$y <- lynx_full$population
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
#> Compiling rjags model...
#> Starting 4 rjags simulations using a PSOCK cluster with 4 nodes on host
#> 'localhost'
#> Simulation complete
#> Note: Summary statistics were not produced as there are >50 monitored
#> variables
#> [To override this behaviour see ?add.summary and ?runjags.options]
#> FALSEFinished running the simulation
#> NOTE: Stopping adaptation
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
#> s(season) 16.26  17.00  895.9  <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> GAM coefficient (beta) estimates:
#>                    2.5%         50%      97.5% Rhat n.eff
#> (Intercept)   6.6576074  6.77853485  6.9212594 1.05    88
#> s(season).1  -1.2290241 -0.68734717 -0.1875609 1.39   133
#> s(season).2  -0.3578916  0.25965317  0.8365117 1.23    76
#> s(season).3   0.5779850  1.09406507  1.6499922 1.18    57
#> s(season).4   1.2898583  1.81650320  2.2637867 1.40    53
#> s(season).5   1.4563061  2.20871777  2.7775186 1.83    45
#> s(season).6   0.5633056  1.31944290  1.8870897 1.50    70
#> s(season).7  -0.6637729 -0.02632234  0.6960057 1.24    89
#> s(season).8  -1.2093072 -0.57996535  0.2733595 1.19   109
#> s(season).9  -1.3993486 -0.64497557  0.2077909 1.07   105
#> s(season).10 -1.0333690 -0.32309001  0.5706681 1.02    68
#> s(season).11 -0.4317056  0.36807347  1.0853681 1.11    53
#> s(season).12  0.2529551  1.18527258  1.7785658 1.55    42
#> s(season).13  0.1137979  1.21577858  1.8107595 2.52    63
#> s(season).14 -0.1179462  0.91735159  1.6277047 2.11    48
#> s(season).15 -0.9110310 -0.18626860  0.4264597 1.25    89
#> s(season).16 -1.4964100 -0.89794283 -0.3213307 1.03   128
#> s(season).17 -1.6772655 -1.12662302 -0.6607839 1.12   136
#> 
#> GAM smoothing parameter (rho) estimates:
#>               2.5%      50%    97.5% Rhat n.eff
#> s(season) 3.042201 3.885326 4.583159 1.03  1432
#> 
#> Latent trend drift (phi) and AR parameter estimates:
#>           2.5%        50%     97.5% Rhat n.eff
#> phi  0.0000000  0.0000000 0.0000000  NaN     0
#> ar1  0.4589342  0.7780231 1.1002675 1.16   924
#> ar2 -0.4366027 -0.1381638 0.1771885 1.11  1346
#> ar3  0.0000000  0.0000000 0.0000000  NaN     0
#> 
```

Of course we should always inpsect traceplots when sampling from a posterior with `MCMC` methods. Here for the `GAM` component (smoothing parameters)

``` r
plot_mvgam_trace(lynx_mvgam, 'rho')
```

<img src="README-unnamed-chunk-12-1.png" style="display: block; margin: auto;" />

and for the latent trend component parameters

``` r
plot_mvgam_trace(lynx_mvgam, 'trend')
```

<img src="README-unnamed-chunk-13-1.png" style="display: block; margin: auto;" /><img src="README-unnamed-chunk-13-2.png" style="display: block; margin: auto;" />

Inspect the model's estimated smooth for the 19-year cyclic pattern, which is shown as a ribbon plot of posterior empirical quantiles. We can also overlay posterior quantiles of partial residuals (shown as ribbon rectangles in red), which represent the leftover variation that the model expects would remain if this smooth term was dropped but all other parameters remained unchanged. Note that these are on a different scale to those from `mgcv::plot.gam` as these are randomised quantile residuals that are essentially standard normal in distribution. But either way, a strong pattern in the partial residuals suggests there would be strong patterns left unexplained in the model *if* we were to drop this term, giving us further confidence that this function is important in the model

``` r
plot_mvgam_smooth(lynx_mvgam, 1, 'season', residuals = T)
```

<img src="README-unnamed-chunk-14-1.png" style="display: block; margin: auto;" />

First derivatives of smooth functions can also be plotted to inspect how the slope of the function changes across its length

``` r
plot_mvgam_smooth(lynx_mvgam, 1, 'season', derivatives = T)
```

<img src="README-unnamed-chunk-15-1.png" style="display: block; margin: auto;" />

We can also view the mvgam's posterior retrodictions and predictions for the entire series (testing and training)

``` r
plot_mvgam_fc(lynx_mvgam, data_test = lynx_test)
```

<img src="README-unnamed-chunk-16-1.png" style="display: block; margin: auto;" />

And the estimated latent trend component, again with the option to show first derivatives

``` r
plot_mvgam_trend(lynx_mvgam, data_test = lynx_test, derivatives = T)
```

<img src="README-unnamed-chunk-17-1.png" style="display: block; margin: auto;" />

We can also re-do the posterior predictive checks, but this time focusing only on the out of sample period. This will give us better insight into how the model is performing and whether it is able to simulate realistic and unbiased future values

``` r
plot_mvgam_ppc(lynx_mvgam, series = 1, type = 'density', data_test = lynx_test)
```

<img src="README-unnamed-chunk-18-1.png" style="display: block; margin: auto;" />

``` r
plot_mvgam_ppc(lynx_mvgam, series = 1, type = 'mean', data_test = lynx_test)
```

<img src="README-unnamed-chunk-19-1.png" style="display: block; margin: auto;" />

``` r
plot_mvgam_ppc(lynx_mvgam, series = 1, type = 'cdf', data_test = lynx_test)
```

<img src="README-unnamed-chunk-20-1.png" style="display: block; margin: auto;" />

``` r
plot_mvgam_ppc(lynx_mvgam, series = 1, type = 'pit', data_test = lynx_test)
```

<img src="README-unnamed-chunk-21-1.png" style="display: block; margin: auto;" />

A key aspect of ecological forecasting is to understand [how different components of a model contribute to forecast uncertainty](https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/eap.1589). We can estimate relative contributions to forecast uncertainty for the GAM component and the latent trend component using `mvgam`

``` r
plot_mvgam_uncertainty(lynx_mvgam, data_test = lynx_test, legend_position = 'none')
text(1, 0.2, cex = 1.5, label="GAM component", 
     pos = 4, col="white", family = 'serif')
text(1, 0.8, cex = 1.5, label="Trend component", 
     pos = 4, col="#7C0000", family = 'serif')
```

<img src="README-unnamed-chunk-22-1.png" style="display: block; margin: auto;" />

Both components contribute to forecast uncertainty, suggesting we would still need some more work to learn about factors driving the dynamics of the system. But we will leave the model as-is for this example. Diagnostics of the model can also be performed using `mvgam`. Have a look at the model's residuals, which are posterior medians of Dunn-Smyth randomised quantile residuals so should follow approximate normality. We are primarily looking for a lack of autocorrelation, which would suggest our AR2 model is appropriate for the latent trend

``` r
plot_mvgam_resids(lynx_mvgam)
```

<img src="README-unnamed-chunk-23-1.png" style="display: block; margin: auto;" />

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
#> Compiling rjags model...
#> Starting 4 rjags simulations using a PSOCK cluster with 4 nodes on host
#> 'localhost'
#> Simulation complete
#> Note: Summary statistics were not produced as there are >50 monitored
#> variables
#> [To override this behaviour see ?add.summary and ?runjags.options]
#> FALSEFinished running the simulation
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
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   11.48   34.52   98.61   99.68  133.44  230.84
summary(mod2_eval$series1$drps)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   64.56   84.92  283.51  289.99  409.86  688.65
```

Nominal coverages for both models' 90% prediction intervals

``` r
mean(mod1_eval$series1$in_interval)
#> [1] 1
mean(mod2_eval$series1$in_interval)
#> [1] 1
```

The `compare_mvgams` function automates this process by rolling along a set of timepoints for each model, ensuring a more in-depth evaluation of each competing model at the same set of timepoints. There are many more extended uses for `mvgam` models, including the ability to fit dynamic factor processes for analysing and forecasting sets of multivariate discrete time series
