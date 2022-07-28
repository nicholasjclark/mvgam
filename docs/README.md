
<!-- README.md is generated from README.Rmd. Please edit that file -->

# *mvgam*

The goal of `mvgam` is to use a Bayesian framework to estimate
parameters of Generalized Additive Models for discrete time series with
dynamic trend components. The motivation for the package and some of its
primary objectives are described in detail by [Clark & Wells
2022](https://www.biorxiv.org/content/10.1101/2022.02.22.481550v1), with
additional inspiration on the use of Bayesian probabilistic modelling to
quantify uncertainty and advise principled decision making coming from
[Michael Betancourt](https://betanalpha.github.io/writing/), [Michael
Dietze](https://www.bu.edu/earth/profiles/michael-dietze/) and [Emily
Fox](https://emilybfox.su.domains/), among many others.

## Installation

Install the development version from `GitHub` using:
`devtools::install_github("nicholasjclark/mvgam")`

## Brief introduction to the package

We can explore the model’s primary functions using a test dataset that
is available with all `R` installations. We introduce dynamic
Generalised Additive Models and some of the key utility functions
provided in `mvgam`. First, load the `lynx` data and plot the series as
well as its estimated autocorrelation function

``` r
library(mvgam)
#> Loading required package: mgcv
#> Loading required package: nlme
#> This is mgcv 1.8-33. For overview type 'help("mgcv-package")'.
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

Along with serial autocorrelation, there is a clear \~19-year cyclic
pattern to the data. Create a `season` term that can be used to model
this effect and give a better representation of the data generating
process than we would likely get with a linear model

``` r
plot(stl(ts(lynx_full$population, frequency = 19), s.window = 'periodic'))
```

<img src="README-unnamed-chunk-3-1.png" style="display: block; margin: auto;" />

``` r
lynx_full$season <- (lynx_full$year %%19) + 1
```

For `mvgam` models, the response needs to be labelled `y` and we also
need an indicator of the series name as a `factor` variable (if the
column `series` is missing, this will be added automatically by assuming
that all observations are from a single time series). Finally, a `time`
column is needed to index time

``` r
lynx_full$y <- lynx_full$population
lynx_full$time <- 1:NROW(lynx_full)
lynx_full$series <- factor('series1')
```

Split the data into training (first 50 years) and testing (next 10 years
of data) to evaluate multi-step ahead forecasts

``` r
lynx_train = lynx_full[1:50, ]
lynx_test = lynx_full[51:60, ]
```

Inspect the series in a bit more detail using `mvgam`’s plotting utility

``` r
plot_mvgam_series(data_train = lynx_train)
```

<img src="README-unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

Now fit an `mvgam` model; it fits a GAM in which a cyclic smooth
function for `season` is estimated jointly with a full time series model
for the errors (in this case an `AR3` process), rather than relying on
smoothing splines that do not incorporate a concept of the future. We
assume the outcome follows a Poisson distribution and estimate the model
in `Stan` using MCMC sampling (installation links for `rstan` and
`cmdstanr` are found [here](https://mc-stan.org/users/interfaces/rstan)
and [here](https://mc-stan.org/cmdstanr/articles/cmdstanr.html)).

``` r
lynx_mvgam <- mvgam(data_train = lynx_train,
               data_test = lynx_test,
               formula = y ~ s(season, bs = 'cc', k = 19),
               knots = list(season = c(0.5, 19.5)),
               family = 'poisson',
               trend_model = 'AR3',
               use_stan = TRUE,
               burnin = 1000,
               chains = 4)
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 1 Iteration:    1 / 2000 [  0%]  (Warmup) 
#> Chain 2 Iteration:    1 / 2000 [  0%]  (Warmup) 
#> Chain 3 Iteration:    1 / 2000 [  0%]  (Warmup) 
#> Chain 4 Iteration:    1 / 2000 [  0%]  (Warmup) 
#> Chain 3 Iteration:  500 / 2000 [ 25%]  (Warmup) 
#> Chain 4 Iteration:  500 / 2000 [ 25%]  (Warmup) 
#> Chain 1 Iteration:  500 / 2000 [ 25%]  (Warmup) 
#> Chain 2 Iteration:  500 / 2000 [ 25%]  (Warmup) 
#> Chain 3 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 3 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 4 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 4 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 1 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 1 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 2 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 2 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 3 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 1 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 2 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 4 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 3 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 3 finished in 43.4 seconds.
#> Chain 1 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 1 finished in 44.5 seconds.
#> Chain 2 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 2 finished in 44.8 seconds.
#> Chain 4 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 4 finished in 45.5 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 44.5 seconds.
#> Total execution time: 45.8 seconds.
```

Inspect the resulting model file, which is written in the `Stan`
probabilistic programming language

``` r
lynx_mvgam$model_file
#>   [1] ""                                                                                                                     
#>   [2] "//Stan model code generated by package mvgam"                                                                         
#>   [3] "data {"                                                                                                               
#>   [4] "int<lower=0> total_obs; // total number of observations"                                                              
#>   [5] "int<lower=0> n; // number of timepoints per series"                                                                   
#>   [6] "int<lower=0> n_sp; // number of smoothing parameters"                                                                 
#>   [7] "int<lower=0> n_series; // number of series"                                                                           
#>   [8] "int<lower=0> num_basis; // total number of basis coefficients"                                                        
#>   [9] "vector[num_basis] zero; // prior locations for basis coefficients"                                                    
#>  [10] "real p_taus[1]; // prior precisions for parametric coefficients"                                                      
#>  [11] "real p_coefs[1]; // prior locations for parametric coefficients"                                                      
#>  [12] "matrix[num_basis, total_obs] X; // transposed mgcv GAM design matrix"                                                 
#>  [13] "int<lower=0> ytimes[n, n_series]; // time-ordered matrix (which col in X belongs to each [time, series] observation?)"
#>  [14] "matrix[17,17] S1; // mgcv smooth penalty matrix S1"                                                                   
#>  [15] "int<lower=0, upper=1> y_observed[n, n_series]; // indices of missing vs observed"                                     
#>  [16] "int<lower=-1> y[n, n_series]; // time-ordered observations, with -1 indicating missing"                               
#>  [17] "}"                                                                                                                    
#>  [18] ""                                                                                                                     
#>  [19] "parameters {"                                                                                                         
#>  [20] "// raw basis coefficients"                                                                                            
#>  [21] "row_vector[num_basis] b_raw;"                                                                                         
#>  [22] ""                                                                                                                     
#>  [23] "// latent trend AR1 terms"                                                                                            
#>  [24] "vector<lower=-1.5,upper=1.5>[n_series] ar1;"                                                                          
#>  [25] ""                                                                                                                     
#>  [26] "// latent trend AR2 terms"                                                                                            
#>  [27] "vector<lower=-1.5,upper=1.5>[n_series] ar2;"                                                                          
#>  [28] ""                                                                                                                     
#>  [29] "// latent trend AR3 terms"                                                                                            
#>  [30] "vector<lower=-1.5,upper=1.5>[n_series] ar3;"                                                                          
#>  [31] ""                                                                                                                     
#>  [32] "// latent trend variance parameters"                                                                                  
#>  [33] "vector<lower=0.05,upper=5>[n_series] sigma;"                                                                          
#>  [34] ""                                                                                                                     
#>  [35] "// latent trends"                                                                                                     
#>  [36] "matrix[n, n_series] trend;"                                                                                           
#>  [37] ""                                                                                                                     
#>  [38] "// smoothing parameters"                                                                                              
#>  [39] "vector<lower=0.0005>[n_sp] lambda;"                                                                                   
#>  [40] "}"                                                                                                                    
#>  [41] ""                                                                                                                     
#>  [42] "transformed parameters {"                                                                                             
#>  [43] "// GAM contribution to expectations (log scale)"                                                                      
#>  [44] "vector[total_obs] eta;"                                                                                               
#>  [45] ""                                                                                                                     
#>  [46] "// basis coefficients"                                                                                                
#>  [47] "row_vector[num_basis] b;"                                                                                             
#>  [48] ""                                                                                                                     
#>  [49] "for (i in 1:num_basis) {"                                                                                             
#>  [50] "b[i] = b_raw[i];"                                                                                                     
#>  [51] "}"                                                                                                                    
#>  [52] "eta = to_vector(b * X);"                                                                                              
#>  [53] "}"                                                                                                                    
#>  [54] ""                                                                                                                     
#>  [55] "model {"                                                                                                              
#>  [56] "// parametric effect priors (regularised for identifiability)"                                                        
#>  [57] "for (i in 1:1) {"                                                                                                     
#>  [58] "b_raw[i] ~ normal(p_coefs[i], 1 / p_taus[i]);"                                                                        
#>  [59] "}"                                                                                                                    
#>  [60] ""                                                                                                                     
#>  [61] "// prior for s(season)..."                                                                                            
#>  [62] "b_raw[2:18] ~ multi_normal_prec(zero[2:18],S1[1:17,1:17] * lambda[1]);"                                               
#>  [63] ""                                                                                                                     
#>  [64] "// priors for AR parameters"                                                                                          
#>  [65] "ar1 ~ normal(0, 0.5);"                                                                                                
#>  [66] "ar2 ~ normal(0, 0.5);"                                                                                                
#>  [67] "ar3 ~ normal(0, 0.5);"                                                                                                
#>  [68] ""                                                                                                                     
#>  [69] "// priors for smoothing parameters"                                                                                   
#>  [70] "lambda ~ exponential(0.05);"                                                                                          
#>  [71] ""                                                                                                                     
#>  [72] "// priors for latent trend variance parameters"                                                                       
#>  [73] "sigma ~ exponential(1);"                                                                                              
#>  [74] ""                                                                                                                     
#>  [75] "// trend estimates"                                                                                                   
#>  [76] "for (s in 1:n_series) {"                                                                                              
#>  [77] "trend[1, s] ~ normal(0, sigma[s]);"                                                                                   
#>  [78] "}"                                                                                                                    
#>  [79] ""                                                                                                                     
#>  [80] "for (s in 1:n_series) {"                                                                                              
#>  [81] "trend[2, s] ~ normal(trend[1, s] * ar1[s], sigma[s]);"                                                                
#>  [82] "}"                                                                                                                    
#>  [83] ""                                                                                                                     
#>  [84] "for (s in 1:n_series) {"                                                                                              
#>  [85] "trend[3, s] ~ normal(trend[2, s] * ar1[s] + trend[1, s] * ar2[s], sigma[s]);"                                         
#>  [86] "}"                                                                                                                    
#>  [87] ""                                                                                                                     
#>  [88] "for (i in 4:n) {"                                                                                                     
#>  [89] "for (s in 1:n_series) {"                                                                                              
#>  [90] "trend[i, s] ~ normal(ar1[s] * trend[i - 1, s] + ar2[s] * trend[i - 2, s] + ar3[s] * trend[i - 3, s], sigma[s]);"      
#>  [91] "}"                                                                                                                    
#>  [92] "}"                                                                                                                    
#>  [93] ""                                                                                                                     
#>  [94] "// likelihood functions"                                                                                              
#>  [95] "for (i in 1:n) {"                                                                                                     
#>  [96] "for (s in 1:n_series) {"                                                                                              
#>  [97] "if (y_observed[i, s])"                                                                                                
#>  [98] "y[i, s] ~ poisson_log(eta[ytimes[i, s]] + trend[i, s]);"                                                              
#>  [99] "}"                                                                                                                    
#> [100] "}"                                                                                                                    
#> [101] "}"                                                                                                                    
#> [102] ""                                                                                                                     
#> [103] "generated quantities {"                                                                                               
#> [104] "vector[n_sp] rho;"                                                                                                    
#> [105] "vector[n_series] tau;"                                                                                                
#> [106] "matrix[n, n_series] ypred;"                                                                                           
#> [107] "rho = log(lambda);"                                                                                                   
#> [108] "for (s in 1:n_series) {"                                                                                              
#> [109] "tau[s] = pow(sigma[s], -2.0);"                                                                                        
#> [110] "}"                                                                                                                    
#> [111] ""                                                                                                                     
#> [112] "// posterior predictions"                                                                                             
#> [113] "for(i in 1:n){"                                                                                                       
#> [114] "for(s in 1:n_series){"                                                                                                
#> [115] "ypred[i, s] = poisson_log_rng(eta[ytimes[i, s]] + trend[i, s]);"                                                      
#> [116] "}"                                                                                                                    
#> [117] "}"                                                                                                                    
#> [118] "}"                                                                                                                    
#> [119] ""
```

Perform a series of posterior predictive checks to see if the model is
able to simulate data for the training period that looks realistic and
unbiased. First, examine histograms for posterior predictions (`yhat`)
and compare to the histogram of the observations (`y`)

``` r
ppc(lynx_mvgam, series = 1, type = 'hist')
```

<img src="README-unnamed-chunk-9-1.png" style="display: block; margin: auto;" />

Now plot the distribution of predicted means compared to the observed
mean

``` r
ppc(lynx_mvgam, series = 1, type = 'mean')
```

<img src="README-unnamed-chunk-10-1.png" style="display: block; margin: auto;" />

Next examine simulated empirical Cumulative Distribution Functions (CDF)
for posterior predictions (`yhat`) and compare to the CDF of the
observations (`y`)

``` r
ppc(lynx_mvgam, series = 1, type = 'cdf')
```

<img src="README-unnamed-chunk-11-1.png" style="display: block; margin: auto;" />

Rootograms are becoming [popular graphical tools for checking a discrete
model’s ability to capture dispersion properties of the response
variable](https://arxiv.org/pdf/1605.01311.pdf). Posterior predictive
hanging rootograms can be displayed using the `ppc()` function in
`mvgam`. In the plot below, we bin the unique observed values into `25`
bins to prevent overplotting and help with interpretation. This plot
compares the frequencies of observed vs predicted values for each bin,
which can help to identify aspects of poor model fit. For example, if
the gray bars (representing observed frequencies) tend to stretch below
zero, this suggests the model’s simulations predict the values in that
particular bin less frequently than they are observed in the data. A
well-fitting model that can generate realistic simulated data will
provide a rootogram in which the lower boundaries of the grey bars are
generally near zero

``` r
ppc(lynx_mvgam, series = 1, type = 'rootogram', n_bins = 25)
```

<img src="README-unnamed-chunk-12-1.png" style="display: block; margin: auto;" />

Finally look for any biases in predictions by examining a Probability
Integral Transform (PIT) histogram. If our predictions are not biased
one way or another (i.e. not consistently under- or over-predicting),
this histogram should look roughly uniform

``` r
ppc(lynx_mvgam, series = 1, type = 'pit')
```

<img src="README-unnamed-chunk-13-1.png" style="display: block; margin: auto;" />

All of these plots indicate the model is well calibrated against the
training data, with no apparent pathological behaviors exhibited. Have a
look at this model’s summary to see what is being estimated (note that
longer MCMC runs would probably be needed to increase effective sample
sizes)

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
#> AR3
#> 
#> N series:
#> 1
#> 
#> N observations:
#> 50
#> 
#> Status:
#> Fitted using Stan
#> 
#> GAM smooth term estimated degrees of freedom:
#>            edf df
#> s(season) 9799 17
#> 
#> GAM coefficient (beta) estimates:
#>                     2.5%         50%       97.5% Rhat n.eff
#> (Intercept)   6.79015950  6.80218000  6.81431000    1  6808
#> s(season).1  -1.20317400 -0.68312750  0.01746606    1  1039
#> s(season).2  -0.34217647  0.25609150  0.90302028    1  1803
#> s(season).3   0.38097955  1.10338500  1.79252475    1  1762
#> s(season).4   0.80091535  1.72666500  2.44061925    1  1272
#> s(season).5   1.03276175  1.99367500  2.72996700    1  1117
#> s(season).6   0.29843772  1.17278500  1.89230325    1  1392
#> s(season).7  -0.84398567 -0.08065355  0.68787680    1  1811
#> s(season).8  -1.41318800 -0.64321500  0.20129283    1  1323
#> s(season).9  -1.56880575 -0.76334700  0.31739575    1  1094
#> s(season).10 -1.24017525 -0.42983750  0.64996255    1  1280
#> s(season).11 -0.60767142  0.24961800  1.17260975    1  1705
#> s(season).12  0.08591474  1.17380000  2.00801000    1  1208
#> s(season).13 -0.03388641  1.29483500  2.15689075    1   839
#> s(season).14 -0.33679847  0.99392500  1.82216725    1   813
#> s(season).15 -0.96419433 -0.11696200  0.54524333    1  1123
#> s(season).16 -1.42184425 -0.80551700 -0.22757412    1  1959
#> s(season).17 -1.57080350 -1.05019000 -0.41779690    1  1377
#> 
#> GAM smoothing parameter (rho) estimates:
#>               2.5%      50%    97.5% Rhat n.eff
#> s(season) 3.321265 4.190605 4.934154    1  2996
#> 
#> Latent trend parameter estimates:
#>                2.5%         50%     97.5% Rhat n.eff
#> ar1[1]    0.5430310  0.89219950 1.2556305    1  1493
#> ar2[1]   -0.6911293 -0.27678200 0.1547917    1  2967
#> ar3[1]   -0.3486109  0.05792415 0.4070008    1  1333
#> sigma[1]  0.3651319  0.45930700 0.5985543    1  1955
#> 
#> [1] "n_eff / iter looks reasonable for all parameters"
#> [1] "Rhat looks reasonable for all parameters"
#> [1] "0 of 4000 iterations ended with a divergence (0%)"
#> [1] "24 of 4000 iterations saturated the maximum tree depth of 11 (0.6%)"
#> [1] "  Run again with max_treedepth set to a larger value to avoid saturation"
#> [1] "E-FMI indicated no pathological behavior"
```

The `plot_mvgam_...()` functions offer more flexibility than the generic
`S3 plot.mvgam()` functions. For example, we can inpsect traceplots when
sampling from a posterior with `MCMC` methods. Here for the `GAM`
component (smoothing parameters).

``` r
plot_mvgam_trace(lynx_mvgam, 'rho')
```

<img src="README-unnamed-chunk-15-1.png" style="display: block; margin: auto;" />

and for the latent trend component parameters

``` r
MCMCvis::MCMCtrace(lynx_mvgam$model_output, c('ar1', 'ar2', 'sigma'), pdf = F, n.eff = T, Rhat = T)
```

<img src="README-unnamed-chunk-16-1.png" style="display: block; margin: auto;" />

Inspect the model’s estimated smooth for the 19-year cyclic pattern,
which is shown as a ribbon plot of posterior empirical quantiles. We can
also overlay posterior quantiles of partial residuals (shown as ribbon
rectangles in red), which represent the leftover variation that the
model expects would remain if this smooth term was dropped but all other
parameters remained unchanged. Note that these are on a different scale
to those from `mgcv::plot.gam` as these are randomised quantile
residuals that are essentially standard normal in distribution. But
either way, a strong pattern in the partial residuals suggests there
would be strong patterns left unexplained in the model *if* we were to
drop this term, giving us further confidence that this function is
important in the model

``` r
plot(lynx_mvgam, type = 'smooths', residuals = T)
```

<img src="README-unnamed-chunk-17-1.png" style="display: block; margin: auto;" />

First derivatives of smooth functions can also be plotted to inspect how
the slope of the function changes across its length. To plot these we
use the more flexible `plot_mvgam_smooth()` function

``` r
plot_mvgam_smooth(lynx_mvgam, 1, 'season', derivatives = T)
```

<img src="README-unnamed-chunk-18-1.png" style="display: block; margin: auto;" />

We can also view the mvgam’s posterior retrodictions and predictions for
the entire series (testing and training)

``` r
plot(lynx_mvgam, type = 'forecast', data_test = lynx_test)
#> Out of sample DRPS:
#> [1] 682.8037
#> 
```

<img src="README-unnamed-chunk-19-1.png" style="display: block; margin: auto;" />

And the estimated latent trend component, again using the more flexible
`plot_mvgam_...()` option to show first derivatives of the estimated
trend

``` r
plot_mvgam_trend(lynx_mvgam, data_test = lynx_test, derivatives = T)
```

<img src="README-unnamed-chunk-20-1.png" style="display: block; margin: auto;" />

We can also re-do the posterior predictive checks, but this time
focusing only on the out of sample period. This will give us better
insight into how the model is performing and whether it is able to
simulate realistic and unbiased future values

``` r
ppc(lynx_mvgam, series = 1, type = 'rootogram', data_test = lynx_test)
```

<img src="README-unnamed-chunk-21-1.png" style="display: block; margin: auto;" />

``` r
ppc(lynx_mvgam, series = 1, type = 'mean', data_test = lynx_test)
```

<img src="README-unnamed-chunk-22-1.png" style="display: block; margin: auto;" />

``` r
ppc(lynx_mvgam, series = 1, type = 'cdf', data_test = lynx_test)
```

<img src="README-unnamed-chunk-23-1.png" style="display: block; margin: auto;" />

``` r
ppc(lynx_mvgam, series = 1, type = 'pit', data_test = lynx_test)
```

<img src="README-unnamed-chunk-24-1.png" style="display: block; margin: auto;" />

A key aspect of ecological forecasting is to understand [how different
components of a model contribute to forecast
uncertainty](https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/eap.1589).
We can estimate relative contributions to forecast uncertainty for the
GAM component and the latent trend component using `mvgam`

``` r
plot_mvgam_uncertainty(lynx_mvgam, data_test = lynx_test, legend_position = 'none')
text(1, 0.2, cex = 1.5, label="GAM component", 
     pos = 4, col="white", family = 'serif')
text(1, 0.8, cex = 1.5, label="Trend component", 
     pos = 4, col="#7C0000", family = 'serif')
```

<img src="README-unnamed-chunk-25-1.png" style="display: block; margin: auto;" />

Both components contribute to forecast uncertainty, suggesting we would
still need some more work to learn about factors driving the dynamics of
the system. But we will leave the model as-is for this example.
Diagnostics of the model can also be performed using `mvgam`. Have a
look at the model’s residuals, which are posterior medians of Dunn-Smyth
randomised quantile residuals so should follow approximate normality. We
are primarily looking for a lack of autocorrelation, which would suggest
our AR2 model is appropriate for the latent trend

``` r
plot(lynx_mvgam, type = 'residuals')
```

<img src="README-unnamed-chunk-26-1.png" style="display: block; margin: auto;" />

Another useful utility of `mvgam` is the ability to use rolling window
forecasts to evaluate competing models that may represent different
hypotheses about the series dynamics. Here we will fit a poorly
specified model to showcase how this evaluation works. In this model, we
ignore the cyclic pattern of seasonality and force it to be fairly
non-wiggly. We also use a random walk process for the trend

``` r
lynx_mvgam_poor <- mvgam(data_train = lynx_train,
               data_test = lynx_test,
               formula = y ~ s(season, k = 3),
               family = 'poisson',
               trend_model = 'RW',
               drift = FALSE,
               use_stan = TRUE,
               burnin = 1000,
               chains = 4)
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 1 Iteration:    1 / 2000 [  0%]  (Warmup) 
#> Chain 2 Iteration:    1 / 2000 [  0%]  (Warmup) 
#> Chain 3 Iteration:    1 / 2000 [  0%]  (Warmup) 
#> Chain 4 Iteration:    1 / 2000 [  0%]  (Warmup) 
#> Chain 4 Iteration:  500 / 2000 [ 25%]  (Warmup) 
#> Chain 1 Iteration:  500 / 2000 [ 25%]  (Warmup) 
#> Chain 3 Iteration:  500 / 2000 [ 25%]  (Warmup) 
#> Chain 4 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 3 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 4 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 3 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 2 Iteration:  500 / 2000 [ 25%]  (Warmup) 
#> Chain 1 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 1 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 4 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 3 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 1 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 2 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 2 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 4 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 4 finished in 6.7 seconds.
#> Chain 3 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 3 finished in 7.1 seconds.
#> Chain 2 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 1 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 1 finished in 7.7 seconds.
#> Chain 2 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 2 finished in 8.8 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 7.6 seconds.
#> Total execution time: 8.9 seconds.
```

We choose a set of timepoints within the training data to forecast from,
allowing us to simulate a situation where the model’s parameters had
already been estimated but we have only observed data up to the
evaluation timepoint and would like to generate forecasts from the
latent trends. Here we use year 10 as our last observation and forecast
ahead for the next 10 years.

``` r
mod1_eval <- eval_mvgam(lynx_mvgam, eval_timepoint = 10, fc_horizon = 10)
mod2_eval <- eval_mvgam(lynx_mvgam_poor, eval_timepoint = 10, fc_horizon = 10)
```

Summary statistics of the two models’ out of sample Discrete Rank
Probability Score (DRPS) indicate that the well-specified model performs
markedly better (far lower DRPS) for this evaluation timepoint

``` r
summary(mod1_eval$series1$drps)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   2.815  14.735 111.162 103.530 151.525 244.586
summary(mod2_eval$series1$drps)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   34.93   41.65  311.68  289.32  456.17  675.74
```

Nominal coverages for both models’ 90% prediction intervals

``` r
mean(mod1_eval$series1$in_interval)
#> [1] 0.9
mean(mod2_eval$series1$in_interval)
#> [1] 0.7
```

The `compare_mvgams` function automates this process by rolling along a
set of timepoints for each model, ensuring a more in-depth evaluation of
each competing model at the same set of timepoints. There are many more
extended uses for `mvgam` models, including the ability to fit dynamic
factor processes for analysing and forecasting sets of multivariate
discrete time series

# License

This project is licensed under an `MIT` open source license
