
<!-- README.md is generated from README.Rmd. Please edit that file -->

# *mvgam*

The goal of `mvgam` is to use a Bayesian framework to estimate
parameters of Generalized Additive Models for discrete time series with
dynamic trend components. The motivation for the package and some of its
primary objectives are described in detail by [Clark & Wells
2022](https://www.biorxiv.org/content/10.1101/2022.02.22.481550v1) (in
press at *Methods in Ecology and Evolution*), with additional
inspiration on the use of Bayesian probabilistic modelling to quantify
uncertainty and advise principled decision making coming from [Michael
Betancourt](https://betanalpha.github.io/writing/), [Michael
Dietze](https://www.bu.edu/earth/profiles/michael-dietze/) and [Emily
Fox](https://emilybfox.su.domains/), among many others.

## Resources

A number of case studies have been compiled to highlight how DGAMs can
be estimated using MCMC sampling. These are hosted currently on `RPubs`
at the following links:

-   [mvgam case study 1: model comparison and data
    assimilation](https://rpubs.com/NickClark47/mvgam)
-   [mvgam case study 2: multivariate
    models](https://rpubs.com/NickClark47/mvgam2)
-   [mvgam case study 3: distributed lag
    models](https://rpubs.com/NickClark47/mvgam3)

The package can also be used to generate all necessary data structures,
initial value functions and modelling code necessary to fit DGAMs using
`Stan` or `JAGS`. This can be helpful if users wish to make changes to
the model to better suit their own bespoke research / analysis goals.
The following resources can be helpful to troubleshoot:

-   [Stan Discourse](https://discourse.mc-stan.org/)
-   [JAGS Discourse](https://sourceforge.net/projects/mcmc-jags/)

## Installation

Install the development version from `GitHub` using:
`devtools::install_github("nicholasjclark/mvgam")`. Note that to
actually condition models with MCMC sampling, either the `JAGS` software
must be installed (along with the `R` packages `rjags` and `runjags`) or
the `Stan` software must be installed (along with the package `rstan`
and, optionally, the `cmdstanr` package). These are not listed as
dependencies of `mvgam` to ensure that installation is less difficult.
If users wish to fit the models using `mvgam`, please refer to
installation links for `JAGS`
[here](https://sourceforge.net/projects/mcmc-jags/files/) or for `Stan`
and `rstan` [here](https://mc-stan.org/users/interfaces/rstan)).

## Citing mvgam and related software

When using open source software (or software in general), please make
sure to appropriately acknowledge the hard work that developers and
maintainers put into making these packages available. Citations are
currently the best way to formally acknowledge this work, so we highly
encourage you to cite any packages that you rely on for your research.

When using `mvgam`, please cite the following publication:

-   Clark, N.J. and Wells, K. (2022). Dynamic Generalized Additive
    Models (DGAMs) for forecasting discrete ecological time series.
    *Methods in Ecology and Evolution*. *In Press*

As `mvgam` acts as an interface to `Stan` and `JAGS`, please
additionally cite whichever software you use for parameter estimation:

-   Carpenter B., Gelman A., Hoffman M. D., Lee D., Goodrich B.,
    Betancourt M., Brubaker M., Guo J., Li P., and Riddell A. (2017).
    Stan: A probabilistic programming language. *Journal of Statistical
    Software*. 76(1). 10.18637/jss.v076.i01
-   Plummer, M. (2013). JAGS: A program for analysis of Bayesian
    graphical models using Gibbs sampling. *Proceedings of the 3rd
    International Workshop on Distributed Statistical Computing*.
    124(125.10).

Further, `mvgam` relies on several other `R` packages and, of course, on
`R` itself. To find out how to cite R and its packages, use the
`citation` function. There are some features of `mvgam` which
specifically rely on certain packages. The most important of these is
the generation of data necessary to estimate smoothing splines, which
entirely rely on `mgcv`. The `rstan` and `cmdstanr` packages together
with `Rcpp` makes `Stan` conveniently accessible in `R`, while the
`rjags` and `runjags` packages together with the `coda` package make
`JAGS` accessible in `R`. If you use some of these features, please also
consider citing the related packages.

## A brief introduction to the package

We can explore the model’s primary functions using a test dataset that
is available with all `R` installations. We introduce Dynamic
Generalized Additive Models and some of the key utility functions
provided in `mvgam`. First, load the `lynx` data and plot the series as
well as its estimated autocorrelation function

``` r
library(mvgam)
#> Loading required package: mgcv
#> Loading required package: nlme
#> This is mgcv 1.8-33. For overview type 'help("mgcv-package")'.
#> Loading required package: parallel
#> Welcome to mvgam. Please cite as: Clark, NJ, and Wells, K. 2022. Dynamic Generalized Additive Models (DGAMs) for forecasting discrete ecological time series. Methods in Ecology and Evolution IN PRESS
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
in `Stan` using MCMC sampling with the `Cmdstan` interface (installation
links for `rstan` and `cmdstanr` are found
[here](https://mc-stan.org/users/interfaces/rstan) and
[here](https://mc-stan.org/cmdstanr/articles/cmdstanr.html)).

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
#> Chain 2 Iteration:  500 / 2000 [ 25%]  (Warmup) 
#> Chain 1 Iteration:  500 / 2000 [ 25%]  (Warmup) 
#> Chain 4 Iteration:  500 / 2000 [ 25%]  (Warmup) 
#> Chain 3 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 3 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 2 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 2 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 4 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 4 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 1 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 1 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 2 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 4 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 3 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 1 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 4 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 4 finished in 40.6 seconds.
#> Chain 2 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 2 finished in 41.0 seconds.
#> Chain 3 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 3 finished in 42.6 seconds.
#> Chain 1 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 1 finished in 44.1 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 42.1 seconds.
#> Total execution time: 44.3 seconds.
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
#>             edf df
#> s(season) 10159 17
#> 
#> GAM coefficient (beta) estimates:
#>                     2.5%        50%        97.5% Rhat n.eff
#> (Intercept)   6.79137975  6.8021500  6.813060250 1.00  5835
#> s(season).1  -1.21310225 -0.6848265  0.007933415 1.01   795
#> s(season).2  -0.33875902  0.2585940  0.933929175 1.00  1687
#> s(season).3   0.37890977  1.1085050  1.780403000 1.00  1602
#> s(season).4   0.79057713  1.7351950  2.449421500 1.01   822
#> s(season).5   0.95817748  2.0073100  2.763706500 1.01   771
#> s(season).6   0.26672547  1.1824250  1.923650500 1.00  1035
#> s(season).7  -0.82938468 -0.0678411  0.696989275 1.00  1627
#> s(season).8  -1.34704850 -0.6325070  0.225866975 1.00  1413
#> s(season).9  -1.47981825 -0.7533885  0.315684225 1.00  1023
#> s(season).10 -1.21779300 -0.4211650  0.658708600 1.00  1173
#> s(season).11 -0.60858420  0.2570000  1.159796250 1.00  1786
#> s(season).12  0.08138890  1.1585000  1.979469500 1.01  1150
#> s(season).13 -0.09381449  1.2803300  2.101127750 1.01   697
#> s(season).14 -0.33100875  0.9802165  1.796408750 1.01   602
#> s(season).15 -0.93567278 -0.1239895  0.528814800 1.01   840
#> s(season).16 -1.41926150 -0.8126590 -0.239660375 1.00  1787
#> s(season).17 -1.60886300 -1.0628500 -0.398178875 1.00   935
#> 
#> GAM smoothing parameter (rho) estimates:
#>               2.5%     50% 97.5% Rhat n.eff
#> s(season) 3.365781 4.19124 4.922    1  2845
#> 
#> Latent trend parameter estimates:
#>                2.5%         50%     97.5% Rhat n.eff
#> ar1[1]    0.5416637  0.89273550 1.2541355 1.01  1158
#> ar2[1]   -0.6874001 -0.27628350 0.1291174 1.00  3114
#> ar3[1]   -0.3266922  0.04769725 0.4027873 1.00  1032
#> sigma[1]  0.3690128  0.45778400 0.5962831 1.00  2109
#> 
#> [1] "n_eff / iter looks reasonable for all parameters"
#> [1] "Rhat looks reasonable for all parameters"
#> [1] "0 of 4000 iterations ended with a divergence (0%)"
#> [1] "56 of 4000 iterations saturated the maximum tree depth of 11 (1.4%)"
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
#> [1] 665.6868
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
#> Chain 2 Iteration:  500 / 2000 [ 25%]  (Warmup) 
#> Chain 3 Iteration:  500 / 2000 [ 25%]  (Warmup) 
#> Chain 4 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 4 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 1 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 1 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 3 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 3 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 2 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 2 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 4 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 1 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 3 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 2 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 4 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 4 finished in 7.2 seconds.
#> Chain 1 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 1 finished in 7.6 seconds.
#> Chain 3 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 3 finished in 7.8 seconds.
#> Chain 2 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 2 finished in 8.1 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 7.7 seconds.
#> Total execution time: 8.3 seconds.
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
#>   3.128  16.787 113.044 104.322 153.197 245.332
summary(mod2_eval$series1$drps)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   32.43   41.85  312.08  293.78  468.14  686.28
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

## License

This project is licensed under an `MIT` open source license
