
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

- [mvgam case study 1: model comparison and data
  assimilation](https://rpubs.com/NickClark47/mvgam)
- [mvgam case study 2: multivariate
  models](https://rpubs.com/NickClark47/mvgam2)
- [mvgam case study 3: distributed lag
  models](https://rpubs.com/NickClark47/mvgam3)

The package can also be used to generate all necessary data structures,
initial value functions and modelling code necessary to fit DGAMs using
`Stan` or `JAGS`. This can be helpful if users wish to make changes to
the model to better suit their own bespoke research / analysis goals.
The following resources can be helpful to troubleshoot:

- [Stan Discourse](https://discourse.mc-stan.org/)
- [JAGS Discourse](https://sourceforge.net/projects/mcmc-jags/)

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

- Clark, N.J. and Wells, K. (2022). Dynamic Generalized Additive Models
  (DGAMs) for forecasting discrete ecological time series. *Methods in
  Ecology and Evolution*. *In Press*

As `mvgam` acts as an interface to `Stan` and `JAGS`, please
additionally cite whichever software you use for parameter estimation:

- Carpenter B., Gelman A., Hoffman M. D., Lee D., Goodrich B.,
  Betancourt M., Brubaker M., Guo J., Li P., and Riddell A. (2017).
  Stan: A probabilistic programming language. *Journal of Statistical
  Software*. 76(1). 10.18637/jss.v076.i01
- Plummer, M. (2013). JAGS: A program for analysis of Bayesian graphical
  models using Gibbs sampling. *Proceedings of the 3rd International
  Workshop on Distributed Statistical Computing*. 124(125.10).

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
#> Chain 2 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 2 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 3 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 3 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 1 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 1 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 4 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 4 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 2 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 3 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 4 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 1 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 4 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 4 finished in 39.1 seconds.
#> Chain 1 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 1 finished in 40.2 seconds.
#> Chain 2 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 2 finished in 40.3 seconds.
#> Chain 3 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 3 finished in 41.0 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 40.1 seconds.
#> Total execution time: 41.2 seconds.
```

Inspect the resulting model file, which is written in the `Stan`
probabilistic programming language

``` r
cat(c("```stan", lynx_mvgam$model_file, "```"), sep = "\n")
```

``` stan

//Stan model code generated by package mvgam
data {
int<lower=0> total_obs; // total number of observations
int<lower=0> n; // number of timepoints per series
int<lower=0> n_sp; // number of smoothing parameters
int<lower=0> n_series; // number of series
int<lower=0> num_basis; // total number of basis coefficients
vector[num_basis] zero; // prior locations for basis coefficients
real p_taus[1]; // prior precisions for parametric coefficients
real p_coefs[1]; // prior locations for parametric coefficients
matrix[num_basis, total_obs] X; // transposed mgcv GAM design matrix
int<lower=0> ytimes[n, n_series]; // time-ordered matrix (which col in X belongs to each [time, series] observation?)
matrix[17,17] S1; // mgcv smooth penalty matrix S1
int<lower=0, upper=1> y_observed[n, n_series]; // indices of missing vs observed
int<lower=-1> y[n, n_series]; // time-ordered observations, with -1 indicating missing
}

parameters {
// raw basis coefficients
row_vector[num_basis] b_raw;

// latent trend AR1 terms
vector<lower=-1.5,upper=1.5>[n_series] ar1;

// latent trend AR2 terms
vector<lower=-1.5,upper=1.5>[n_series] ar2;

// latent trend AR3 terms
vector<lower=-1.5,upper=1.5>[n_series] ar3;

// latent trend variance parameters
vector<lower=0.05,upper=5>[n_series] sigma;

// latent trends
matrix[n, n_series] trend;

// smoothing parameters
vector<lower=0>[n_sp] lambda;
}

transformed parameters {
// GAM contribution to expectations (log scale)
vector[total_obs] eta;

// basis coefficients
row_vector[num_basis] b;

for (i in 1:num_basis) {
b[i] = b_raw[i];
}
eta = to_vector(b * X);
}

model {
// parametric effect priors (regularised for identifiability)
for (i in 1:1) {
b_raw[i] ~ normal(p_coefs[i], 1 / p_taus[i]);
}

// prior for s(season)...
b_raw[2:18] ~ multi_normal_prec(zero[2:18],S1[1:17,1:17] * lambda[1]);

// priors for AR parameters
ar1 ~ normal(0, 0.5);
ar2 ~ normal(0, 0.5);
ar3 ~ normal(0, 0.5);

// priors for smoothing parameters
lambda ~ exponential(0.05);

// priors for latent trend variance parameters
sigma ~ exponential(1);

// trend estimates
for (s in 1:n_series) {
trend[1, s] ~ normal(0, sigma[s]);
}

for (s in 1:n_series) {
trend[2, s] ~ normal(trend[1, s] * ar1[s], sigma[s]);
}

for (s in 1:n_series) {
trend[3, s] ~ normal(trend[2, s] * ar1[s] + trend[1, s] * ar2[s], sigma[s]);
}

for (i in 4:n) {
for (s in 1:n_series) {
trend[i, s] ~ normal(ar1[s] * trend[i - 1, s] + ar2[s] * trend[i - 2, s] + ar3[s] * trend[i - 3, s], sigma[s]);
}
}

// likelihood functions
for (i in 1:n) {
for (s in 1:n_series) {
if (y_observed[i, s])
y[i, s] ~ poisson_log(eta[ytimes[i, s]] + trend[i, s]);
}
}
}

generated quantities {
vector[n_sp] rho;
vector[n_series] tau;
matrix[n, n_series] ypred;
rho = log(lambda);
for (s in 1:n_series) {
tau[s] = pow(sigma[s], -2.0);
}

// posterior predictions
for(i in 1:n){
for(s in 1:n_series){
ypred[i, s] = poisson_log_rng(eta[ytimes[i, s]] + trend[i, s]);
}
}
}
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
#> GAM coefficient (beta) estimates:
#>                    2.5%        50%       97.5% Rhat n.eff
#> (Intercept)   6.7847172  6.8019650  6.81853025 1.00  5995
#> s(season).1  -1.1931685 -0.6753465  0.06160255 1.00  1127
#> s(season).2  -0.3503541  0.2563230  0.98173883 1.00  1900
#> s(season).3   0.3049028  1.0984300  1.79329400 1.00  1746
#> s(season).4   0.6415440  1.7205300  2.42829650 1.00  1070
#> s(season).5   0.8288639  1.9804600  2.72707000 1.00  1041
#> s(season).6   0.1761299  1.1585300  1.91562600 1.00  1293
#> s(season).7  -0.9246409 -0.0977666  0.71098763 1.00  1700
#> s(season).8  -1.4012007 -0.6300050  0.25118925 1.00  1487
#> s(season).9  -1.5040042 -0.7400695  0.36457035 1.01  1151
#> s(season).10 -1.2027393 -0.3883940  0.68614583 1.00  1311
#> s(season).11 -0.5867134  0.2584650  1.17237575 1.00  1955
#> s(season).12  0.0374231  1.1505150  2.00861700 1.00  1159
#> s(season).13 -0.2163205  1.2619950  2.15339775 1.00   872
#> s(season).14 -0.4009086  0.9733245  1.83032950 1.01   832
#> s(season).15 -0.9661038 -0.1300905  0.51815713 1.00  1069
#> s(season).16 -1.4247428 -0.8007880 -0.21897797 1.00  1910
#> s(season).17 -1.5603102 -1.0403900 -0.35109880 1.00  1203
#> 
#> GAM smoothing parameter (rho) estimates:
#>               2.5%      50%    97.5% Rhat n.eff
#> s(season) 3.354857 4.185815 4.932806    1  2733
#> 
#> Latent trend parameter estimates:
#>                2.5%        50%     97.5% Rhat n.eff
#> ar1[1]    0.5509472  0.8986910 1.2522383    1  1196
#> ar2[1]   -0.6851573 -0.2767420 0.1177708    1  2868
#> ar3[1]   -0.3576823  0.0466283 0.4088621    1  1140
#> sigma[1]  0.3685893  0.4621530 0.5979138    1  1806
#> 
#> [1] "n_eff / iter looks reasonable for all parameters"
#> [1] "Rhat looks reasonable for all parameters"
#> [1] "0 of 4000 iterations ended with a divergence (0%)"
#> [1] "0 of 4000 iterations saturated the maximum tree depth of 12 (0%)"
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
#> [1] 686.9317
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
#> Chain 2 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 2 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 1 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 1 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 3 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
#> Chain 3 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
#> Chain 4 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 2 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 3 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 1 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
#> Chain 4 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 4 finished in 6.0 seconds.
#> Chain 3 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 3 finished in 6.4 seconds.
#> Chain 2 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 2 finished in 6.6 seconds.
#> Chain 1 Iteration: 2000 / 2000 [100%]  (Sampling) 
#> Chain 1 finished in 7.0 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 6.5 seconds.
#> Total execution time: 7.1 seconds.
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
#>   4.255  16.591 117.410 105.254 153.657 241.936
summary(mod2_eval$series1$drps)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   35.34   42.37  309.70  288.71  453.04  675.05
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
