
<!-- README.md is generated from README.Rmd. Please edit that file -->

# *mvgam*

The goal of `mvgam` is to use a Bayesian framework to estimate
parameters of Generalized Additive Models for time series with dynamic
trend components. The motivation for the package and some of its primary
objectives are described in detail by [Clark & Wells
2022](https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13974)
(published in *Methods in Ecology and Evolution*), with additional
inspiration on the use of Bayesian probabilistic modelling to quantify
uncertainty and advise principled decision making coming from [Michael
Betancourt](https://betanalpha.github.io/writing/), [Michael
Dietze](https://www.bu.edu/earth/profiles/michael-dietze/) and [Emily
Fox](https://emilybfox.su.domains/), among many others.

## Disclaimer

Please note that the `mvgam` package is under active development, and it
is likely that certain functionality will change on a regular basis. As
such, please take caution when implementing the package for automated
routines. Also note that, because of the many advantages of `Stan` over
`JAGS`, *further development of the package will only be applied to
`Stan`*. This includes the planned addition of more response
distributions, plans to handle zero-inflation, and plans to incorporate
a greater variety of trend models. Users are strongly encouraged to opt
for `Stan` over `JAGS` in any proceeding workflows.

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
the `Stan` software must be installed (along with either `rstan` and/or
`cmdstanr`). Only `rstan` is listed as a dependency of `mvgam` to ensure
that installation is less difficult. If users wish to fit the models
using `mvgam`, please refer to installation links for `JAGS`
[here](https://sourceforge.net/projects/mcmc-jags/files/), for `Stan`
with `rstan` [here](https://mc-stan.org/users/interfaces/rstan), or for
`Stan` with `cmdstandr` [here](https://mc-stan.org/cmdstanr/).

## Citing mvgam and related software

When using open source software (or software in general), please make
sure to appropriately acknowledge the hard work that developers and
maintainers put into making these packages available. Citations are
currently the best way to formally acknowledge this work, so we highly
encourage you to cite any packages that you rely on for your research.

When using `mvgam`, please cite the following publication:

- Clark, N.J. and Wells, K. (2022). Dynamic Generalized Additive Models
  (DGAMs) for forecasting discrete ecological time series. *Methods in
  Ecology and Evolution*. DOI: <https://doi.org/10.1111/2041-210X.13974>

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

## Dynamic latent temporal processes

`mvgam` is designed to propagate unobserved temporal processes to
capture autocorrelation in the observed time series. This works in a
state-space format, with the temporal *trend* evolving independently of
the observation process. Available trend models are:

- `RW` Random Walk
- `AR1` Autoregressive model with AR coefficient for lag 1
- `AR2` Autoregressive model with AR coefficients for lags 1 and 2
- `AR3` Autoregressive model with AR coefficients for lags 1, 2 and 3
- `VAR1` Vector Autoregressive model with VAR coefficients for lag 1;
  contemporaneously uncorrelated errors
- `GP` Squared exponential Gaussian Process
- `None` No latent trend is fitted

When using `Stan` as the back-end, all of these trend models (apart from
`VAR1`) can be estimated using a set of dimension-reduced dynamic
factors. Please see [mvgam case study 2: multivariate
models](https://rpubs.com/NickClark47/mvgam2) for more information

## A brief introduction to the package

We can explore the model’s primary functions using a test dataset that
is available with all `R` installations. We introduce Dynamic
Generalized Additive Models and some of the key utility functions
provided in `mvgam`. First, load the `lynx` data and plot the series as
well as its estimated autocorrelation function

``` r
library(mvgam)
#> Loading required package: mgcv
#> Warning: package 'mgcv' was built under R version 4.2.2
#> Loading required package: nlme
#> This is mgcv 1.8-41. For overview type 'help("mgcv-package")'.
#> Loading required package: parallel
#> Loading required package: rstan
#> Loading required package: StanHeaders
#> 
#> rstan version 2.26.13 (Stan version 2.26.1)
#> For execution on a local, multicore CPU with excess RAM we recommend calling
#> options(mc.cores = parallel::detectCores()).
#> To avoid recompilation of unchanged Stan programs, we recommend calling
#> rstan_options(auto_write = TRUE)
#> For within-chain threading using `reduce_sum()` or `map_rect()` Stan functions,
#> change `threads_per_chain` option:
#> rstan_options(threads_per_chain = 1)
#> Do not specify '-march=native' in 'LOCAL_CPPFLAGS' or a Makevars file
#> Welcome to mvgam. Please cite as: Clark, NJ, and Wells, K. 2022. Dynamic Generalized Additive Models (DGAMs) for forecasting discrete ecological time series. Methods in Ecology and Evolution, 2022, https://doi.org/10.1111/2041-210X.13974
data(lynx)
lynx_full = data.frame(year = 1821:1934, 
                       population = as.numeric(lynx))
plot(lynx_full$population, type = 'l', ylab = 'Lynx trappings',
     xlab = 'Time', bty = 'l', lwd = 2)
box(bty = 'l', lwd  = 2)
```

<img src="README-unnamed-chunk-2-1.png" width="60%" style="display: block; margin: auto;" />

``` r
acf(lynx_full$population, main = '', bty = 'l', lwd = 2,
    ci.col = 'darkred')
box(bty = 'l', lwd  = 2)
```

<img src="README-unnamed-chunk-2-2.png" width="60%" style="display: block; margin: auto;" />

Along with serial autocorrelation, there is a clear \~19-year cyclic
pattern to the data. Create a `season` term that can be used to model
this effect and give a better representation of the data generating
process than we would likely get with a linear model

``` r
plot(stl(ts(lynx_full$population, frequency = 19), s.window = 'periodic'),
     lwd = 2, col.range = 'darkred')
```

<img src="README-unnamed-chunk-3-1.png" width="60%" style="display: block; margin: auto;" />

``` r
lynx_full$season <- (lynx_full$year %%19) + 1
```

For `mvgam` models, we need an indicator of the series name as a
`factor` variable (if the column `series` is missing, this will be added
automatically by assuming that all observations are from a single time
series). Finally, a `time` column is needed to index time

``` r
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
plot_mvgam_series(data = lynx_train, y = 'population')
```

<img src="README-unnamed-chunk-6-1.png" width="60%" style="display: block; margin: auto;" />

Now we will formulate an `mvgam` model; this model fits a GAM in which a
cyclic smooth function for `season` is estimated jointly with a full
time series model for the temporal process (in this case an `AR3`
process), rather than relying on smoothing splines that do not
incorporate a concept of the future. We assume the outcome follows a
Poisson distribution. But before conditioning the model on observed
data, a check of prior smooth function realisations is useful to ensure
we are allowing enough flexibility to capture the types of functional
behaviours we think are reasonable without allowing outrageous
behaviours. First we follow conventional recommendations to set `k` for
the smooth term to be large, which would allow maximum flexibility in
functional behaviours

``` r
lynx_mvgam_prior <- mvgam(data = lynx_train,
               formula = population ~ s(season, bs = 'cc', k = 19),
               knots = list(season = c(0.5, 19.5)),
               family = 'poisson',
               trend_model = 'AR3',
               chains = 4,
               prior_simulation = TRUE)
```

Plot empirical quantiles of the prior seasonal smooth function

``` r
plot(lynx_mvgam_prior, type = 'smooths')
```

<img src="README-unnamed-chunk-9-1.png" width="60%" style="display: block; margin: auto;" />

Plot a set of realisations from the prior seasonal smooth function

``` r
plot(lynx_mvgam_prior, type = 'smooths', realisations = TRUE,
     n_realisations = 20)
```

<img src="README-unnamed-chunk-10-1.png" width="60%" style="display: block; margin: auto;" />

These functions are showing the marginal contribution of the seasonal
smooth function to the linear predictor (on the log scale), and they are
clearly allowed to move into ridiculous spaces that we should give very
little prior plausibility to:

``` r
exp(-15)
#> [1] 3.059023e-07
exp(15)
#> [1] 3269017
```

Setting `k` to a smaller value results in less flexibility. This is
because number of basis functions that contribute to functional
behaviour is reduced

``` r
lynx_mvgam_prior <- mvgam(data = lynx_train,
               formula = population ~ s(season, bs = 'cc', k = 12),
               knots = list(season = c(0.5, 19.5)),
               family = 'poisson',
               trend_model = 'AR3',
               chains = 4,
               prior_simulation = TRUE)
```

The resulting prior looks more reasonable given the range of the
observations, and there is clearly enough flexibility to support a wide
range of functional shapes.

``` r
plot(lynx_mvgam_prior, type = 'smooths')
```

<img src="README-unnamed-chunk-14-1.png" width="60%" style="display: block; margin: auto;" />

``` r
plot(lynx_mvgam_prior, type = 'smooths', realisations = TRUE,
     n_realisations = 20)
```

<img src="README-unnamed-chunk-15-1.png" width="60%" style="display: block; margin: auto;" />

In practice, imparting domain knowledge into prior specifications for
penalised smooth functions is challenging, as these behaviours are often
the cumulative result of multiple penalty matrices that all have their
own separate smoothing parameters. Changing the prior on the smoothing
parameters is another option (`mvgam` uses a half-normal prior by
default, which regularises functions more than the default approach used
in `mgcv::jagam`). But without running through prior visualisations (and
other prior pushforward checks) it will be more difficult to reason
about how to set `k` to respect domain knowledge. In general it is
highly recommended that users view `mvgam` and related interfaces such
as `brms` as tools for building scaffold models that can then be
modified to suit the bespoke needs of each particular analysis.

Users can also check what the default prior distributions are for given
model formulations, which can be helpful to understand how the model can
be modified but also to see any restrictions on what can be changed
within the `mvgam` framework.

``` r
test_priors <- get_mvgam_priors(population ~ s(season, bs = 'cc', k = 12),
                                family = 'poisson',
                                data = lynx_train,
                                trend_model = 'AR3',
                                use_stan = TRUE)
test_priors
#>        param_name param_length                  param_info
#> 1 lambda<lower=0>            1 s(season) smooth parameters
#> 2             ar1            1       trend AR1 coefficient
#> 3             ar2            1       trend AR2 coefficient
#> 4             ar3            1       trend AR3 coefficient
#> 5  sigma<lower=0>            1                    trend sd
#>                      prior              example_change
#> 1 lambda ~ normal(10, 25); lambda ~ exponential(0.21);
#> 2      ar1 ~ std_normal();    ar1 ~ normal(0.25, 0.8);
#> 3      ar2 ~ std_normal();  ar2 ~ normal(-0.63, 0.21);
#> 4      ar3 ~ std_normal();   ar3 ~ normal(0.39, 0.19);
#> 5  sigma ~ exponential(2);  sigma ~ exponential(0.58);
```

Any of the above priors can be changed by modifying the `prior` column
and supplying the resulting `data.frame` to the `priors` argument in
`mvgam()`. But for now, we will proceed with the defaults by
conditioning the model on observed data in `Stan` using MCMC sampling
with the `Cmdstan` interface (installation links for `rstan` and
`cmdstanr` are found [here](https://mc-stan.org/users/interfaces/rstan)
and [here](https://mc-stan.org/cmdstanr/articles/cmdstanr.html)).

``` r
lynx_mvgam <- mvgam(data = lynx_train,
               newdata = lynx_test,
               formula = population ~ s(season, bs = 'cc', k = 12),
               knots = list(season = c(0.5, 19.5)),
               family = 'poisson',
               trend_model = 'AR3',
               use_stan = TRUE)
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 1 Iteration:   1 / 1000 [  0%]  (Warmup) 
#> Chain 2 Iteration:   1 / 1000 [  0%]  (Warmup) 
#> Chain 3 Iteration:   1 / 1000 [  0%]  (Warmup) 
#> Chain 4 Iteration:   1 / 1000 [  0%]  (Warmup) 
#> Chain 4 Iteration: 100 / 1000 [ 10%]  (Warmup) 
#> Chain 2 Iteration: 100 / 1000 [ 10%]  (Warmup) 
#> Chain 1 Iteration: 100 / 1000 [ 10%]  (Warmup) 
#> Chain 3 Iteration: 100 / 1000 [ 10%]  (Warmup) 
#> Chain 4 Iteration: 200 / 1000 [ 20%]  (Warmup) 
#> Chain 1 Iteration: 200 / 1000 [ 20%]  (Warmup) 
#> Chain 2 Iteration: 200 / 1000 [ 20%]  (Warmup) 
#> Chain 3 Iteration: 200 / 1000 [ 20%]  (Warmup) 
#> Chain 4 Iteration: 300 / 1000 [ 30%]  (Warmup) 
#> Chain 1 Iteration: 300 / 1000 [ 30%]  (Warmup) 
#> Chain 2 Iteration: 300 / 1000 [ 30%]  (Warmup) 
#> Chain 3 Iteration: 300 / 1000 [ 30%]  (Warmup) 
#> Chain 4 Iteration: 400 / 1000 [ 40%]  (Warmup) 
#> Chain 1 Iteration: 400 / 1000 [ 40%]  (Warmup) 
#> Chain 2 Iteration: 400 / 1000 [ 40%]  (Warmup) 
#> Chain 3 Iteration: 400 / 1000 [ 40%]  (Warmup) 
#> Chain 4 Iteration: 500 / 1000 [ 50%]  (Warmup) 
#> Chain 4 Iteration: 501 / 1000 [ 50%]  (Sampling) 
#> Chain 1 Iteration: 500 / 1000 [ 50%]  (Warmup) 
#> Chain 1 Iteration: 501 / 1000 [ 50%]  (Sampling) 
#> Chain 2 Iteration: 500 / 1000 [ 50%]  (Warmup) 
#> Chain 2 Iteration: 501 / 1000 [ 50%]  (Sampling) 
#> Chain 3 Iteration: 500 / 1000 [ 50%]  (Warmup) 
#> Chain 4 Iteration: 600 / 1000 [ 60%]  (Sampling) 
#> Chain 3 Iteration: 501 / 1000 [ 50%]  (Sampling) 
#> Chain 2 Iteration: 600 / 1000 [ 60%]  (Sampling) 
#> Chain 1 Iteration: 600 / 1000 [ 60%]  (Sampling) 
#> Chain 4 Iteration: 700 / 1000 [ 70%]  (Sampling) 
#> Chain 3 Iteration: 600 / 1000 [ 60%]  (Sampling) 
#> Chain 2 Iteration: 700 / 1000 [ 70%]  (Sampling) 
#> Chain 1 Iteration: 700 / 1000 [ 70%]  (Sampling) 
#> Chain 3 Iteration: 700 / 1000 [ 70%]  (Sampling) 
#> Chain 4 Iteration: 800 / 1000 [ 80%]  (Sampling) 
#> Chain 2 Iteration: 800 / 1000 [ 80%]  (Sampling) 
#> Chain 1 Iteration: 800 / 1000 [ 80%]  (Sampling) 
#> Chain 3 Iteration: 800 / 1000 [ 80%]  (Sampling) 
#> Chain 4 Iteration: 900 / 1000 [ 90%]  (Sampling) 
#> Chain 2 Iteration: 900 / 1000 [ 90%]  (Sampling) 
#> Chain 3 Iteration: 900 / 1000 [ 90%]  (Sampling) 
#> Chain 1 Iteration: 900 / 1000 [ 90%]  (Sampling) 
#> Chain 4 Iteration: 1000 / 1000 [100%]  (Sampling) 
#> Chain 4 finished in 28.9 seconds.
#> Chain 2 Iteration: 1000 / 1000 [100%]  (Sampling) 
#> Chain 2 finished in 29.4 seconds.
#> Chain 3 Iteration: 1000 / 1000 [100%]  (Sampling) 
#> Chain 3 finished in 30.2 seconds.
#> Chain 1 Iteration: 1000 / 1000 [100%]  (Sampling) 
#> Chain 1 finished in 30.8 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 29.8 seconds.
#> Total execution time: 30.9 seconds.
```

Inspect the resulting model file, which is written in the `Stan`
probabilistic programming language

``` r
code(lynx_mvgam)
#> // Stan model code generated by package mvgam
#> data {
#>   int<lower=0> total_obs; // total number of observations
#>   int<lower=0> n; // number of timepoints per series
#>   int<lower=0> n_sp; // number of smoothing parameters
#>   int<lower=0> n_series; // number of series
#>   int<lower=0> num_basis; // total number of basis coefficients
#>   vector[num_basis] zero; // prior locations for basis coefficients
#>   real p_taus[1]; // prior precisions for parametric coefficients
#>   real p_coefs[1]; // prior locations for parametric coefficients
#>   matrix[total_obs, num_basis] X; // mgcv GAM design matrix
#>   int<lower=0> ytimes[n, n_series]; // time-ordered matrix (which col in X belongs to each [time, series] observation?)
#>   matrix[10,10] S1; // mgcv smooth penalty matrix S1
#>   int<lower=0> n_nonmissing; // number of nonmissing observations
#>   int<lower=0> flat_ys[n_nonmissing]; // flattened nonmissing observations
#>   matrix[n_nonmissing, num_basis] flat_xs; // X values for nonmissing observations
#>   int<lower=0> obs_ind[n_nonmissing]; // indices of nonmissing observations
#> }
#> parameters {
#>   // raw basis coefficients
#>   vector[num_basis] b_raw;
#>   // latent trend AR1 terms
#>   vector<lower=-1.5,upper=1.5>[n_series] ar1;
#>   // latent trend AR2 terms
#>   vector<lower=-1.5,upper=1.5>[n_series] ar2;
#>   // latent trend AR3 terms
#>   vector<lower=-1.5,upper=1.5>[n_series] ar3;
#>   // latent trend variance parameters
#>   vector<lower=0>[n_series] sigma;
#>   // latent trends
#>   matrix[n, n_series] trend;
#>   // smoothing parameters
#>   vector<lower=0>[n_sp] lambda;
#> }
#> transformed parameters {
#>   // basis coefficients
#>   vector[num_basis] b;
#>   b[1:num_basis] = b_raw[1:num_basis];
#> }
#> model {
#>   // parametric effect priors (regularised for identifiability)
#>   for (i in 1:1) {
#>   b_raw[i] ~ normal(p_coefs[i], sqrt(1 / p_taus[i]));
#>   }
#>   // prior for s(season)...
#>   b_raw[2:11] ~ multi_normal_prec(zero[2:11],S1[1:10,1:10] * lambda[1]);
#>   // priors for AR parameters
#>   ar1 ~ std_normal();
#>   ar2 ~ std_normal();
#>   ar3 ~ std_normal();
#>   // priors for smoothing parameters
#>   lambda ~ normal(10, 25);
#>   // priors for latent trend variance parameters
#>   sigma ~ exponential(2);
#>   // trend estimates
#>   trend[1, 1:n_series] ~ normal(0, sigma);
#>   trend[2, 1:n_series] ~ normal(trend[1, 1:n_series] * ar1, sigma);
#>   trend[3, 1:n_series] ~ normal(trend[2, 1:n_series] * ar1 + trend[1, 1:n_series] * ar2, sigma);
#>   for(s in 1:n_series){
#>   trend[4:n, s] ~ normal(ar1[s] * trend[3:(n - 1), s] + ar2[s] * trend[2:(n - 2), s] + ar3[s] * trend[1:(n - 3), s], sigma[s]);
#>   }
#>   {
#>   // likelihood functions
#>   vector[n_nonmissing] flat_trends;
#>   flat_trends = (to_vector(trend))[obs_ind];
#>   flat_ys ~ poisson_log_glm(append_col(flat_xs, flat_trends),
#>   0.0,append_row(b, 1.0));
#>   }
#> }
#> generated quantities {
#>   vector[total_obs] eta;
#>   matrix[n, n_series] mus;
#>   vector[n_sp] rho;
#>   vector[n_series] tau;
#>   array[n, n_series] int ypred;
#>   rho = log(lambda);
#>   for (s in 1:n_series) {
#>   tau[s] = pow(sigma[s], -2.0);
#>   }
#>   // posterior predictions
#>   eta = X * b;
#>   for(s in 1:n_series){ 
#>   mus[1:n, s] = eta[ytimes[1:n, s]] + trend[1:n, s];
#>   ypred[1:n, s] = poisson_log_rng(mus[1:n, s]);
#>   }
#> }
```

Perform a series of posterior retrodictive checks to see if the model is
able to simulate data for the training period that looks realistic and
unbiased. First, examine histograms for posterior retrodictions (`yhat`)
and compare to the histogram of the observations (`y`)

``` r
ppc(lynx_mvgam, series = 1, type = 'hist')
```

<img src="README-unnamed-chunk-19-1.png" width="60%" style="display: block; margin: auto;" />

Now plot the distribution of predicted means compared to the observed
mean

``` r
ppc(lynx_mvgam, series = 1, type = 'mean')
```

<img src="README-unnamed-chunk-20-1.png" width="60%" style="display: block; margin: auto;" />

Next examine simulated empirical Cumulative Distribution Functions (CDF)
for posterior retrodictions (`yhat`) and compare to the CDF of the
observations (`y`)

``` r
ppc(lynx_mvgam, series = 1, type = 'cdf')
```

<img src="README-unnamed-chunk-21-1.png" width="60%" style="display: block; margin: auto;" />

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

<img src="README-unnamed-chunk-22-1.png" width="60%" style="display: block; margin: auto;" />

Finally look for any biases in predictions by examining a Probability
Integral Transform (PIT) histogram. If our predictions are not biased
one way or another (i.e. not consistently under- or over-predicting),
this histogram should look roughly uniform

``` r
ppc(lynx_mvgam, series = 1, type = 'pit')
```

<img src="README-unnamed-chunk-23-1.png" width="60%" style="display: block; margin: auto;" />

All of these plots indicate the model is well calibrated against the
training data, with no apparent pathological behaviors exhibited. Have a
look at this model’s summary to see what is being estimated. Note that
no pathological behaviours have been detected and we achieve good
effective sample sizes / mixing for all parameters

``` r
summary(lynx_mvgam)
#> GAM formula:
#> population ~ s(season, bs = "cc", k = 12)
#> 
#> Family:
#> poisson
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
#> N timepoints:
#> 50
#> 
#> Status:
#> Fitted using Stan
#> 
#> GAM coefficient (beta) estimates:
#>                     2.5%        50%       97.5% Rhat n.eff
#> (Intercept)   6.78311725  6.8047850  6.82785150 1.00  2477
#> s(season).1  -0.56231907  0.0786684  0.76775175 1.00   814
#> s(season).2  -0.29419730  0.8139825  1.80692275 1.00   504
#> s(season).3  -0.07397281  1.1745000  2.42287425 1.00   409
#> s(season).4  -0.51154182  0.4146510  1.32839200 1.01   617
#> s(season).5  -1.18118675 -0.1136550  0.96291527 1.00   510
#> s(season).6  -1.02503175  0.0313312  1.09524375 1.00   573
#> s(season).7  -0.76636920  0.3265845  1.36367975 1.00   631
#> s(season).8  -1.09967100  0.1165245  1.74848025 1.00   407
#> s(season).9  -1.21529000 -0.3979710  0.57777195 1.00   471
#> s(season).10 -1.31417025 -0.7095250 -0.01492678 1.00   694
#> 
#> GAM smoothing parameter (rho) estimates:
#>               2.5%     50%    97.5% Rhat n.eff
#> s(season) 2.073215 3.43419 4.293916 1.01   492
#> 
#> Latent trend AR parameter estimates:
#>                2.5%        50%      97.5% Rhat n.eff
#> ar1[1]    0.7378222  1.1335600 1.44146125    1   647
#> ar2[1]   -0.8311461 -0.4141595 0.03908541    1  1226
#> ar3[1]   -0.4555851 -0.1374490 0.27090805    1   606
#> sigma[1]  0.4043893  0.4983925 0.62644357    1  1465
#> 
#> Stan MCMC diagnostics
#> n_eff / iter looks reasonable for all parameters
#> Rhat looks reasonable for all parameters
#> 0 of 2000 iterations ended with a divergence (0%)
#> 0 of 2000 iterations saturated the maximum tree depth of 12 (0%)
#> E-FMI indicated no pathological behavior
#> 
```

As with any `MCMC` based software, we can inspect traceplots. Here for
the `GAM` component smoothing parameters. There is no requirement for
`rstan` to be installed as a dependency, but we can still use it if
available to examine traceplots

``` r
rstan::stan_trace(lynx_mvgam$model_output, 'rho')
```

<img src="README-unnamed-chunk-25-1.png" width="60%" style="display: block; margin: auto;" />

and for the latent trend component parameters

``` r
rstan::stan_trace(lynx_mvgam$model_output, c('ar1', 'ar2', 'ar3', 'sigma'))
```

<img src="README-unnamed-chunk-26-1.png" width="60%" style="display: block; margin: auto;" />

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
plot(lynx_mvgam, type = 'smooths', residuals = TRUE)
```

<img src="README-unnamed-chunk-27-1.png" width="60%" style="display: block; margin: auto;" />

It is often also useful to compare prior to posterior function
realisations to understand how informative the observed data have been
for learning these functional shapes

``` r
layout(matrix(1:2, nrow = 2))
plot(lynx_mvgam_prior, type = 'smooths', realisations = TRUE,
     n_realisations = 30)
```

<img src="README-unnamed-chunk-28-1.png" width="60%" style="display: block; margin: auto;" />

``` r
plot(lynx_mvgam, type = 'smooths', realisations = TRUE,
     n_realisations = 30)
```

<img src="README-unnamed-chunk-28-2.png" width="60%" style="display: block; margin: auto;" />

``` r
layout(1)
```

First derivatives of smooth functions can be plotted to inspect how the
slope of the function changes across its length. To plot these we use
the more flexible `plot_mvgam_smooth()` function

``` r
plot_mvgam_smooth(lynx_mvgam, 1, 'season', derivatives = TRUE)
```

<img src="README-unnamed-chunk-29-1.png" width="60%" style="display: block; margin: auto;" />

We can also view the mvgam’s posterior retrodictions and predictions for
the entire series (testing and training)

``` r
plot(lynx_mvgam, type = 'forecast', newdata = lynx_test)
#> Out of sample DRPS:
#> [1] 1099.7
#> 
```

<img src="README-unnamed-chunk-30-1.png" width="60%" style="display: block; margin: auto;" />

And the estimated latent trend component, again using the more flexible
`plot_mvgam_...()` option to show first derivatives of the estimated
trend

``` r
plot_mvgam_trend(lynx_mvgam, newdata = lynx_test, derivatives = TRUE)
```

<img src="README-unnamed-chunk-31-1.png" width="60%" style="display: block; margin: auto;" />

We can also re-do the posterior predictive checks, but this time
focusing only on the out of sample period. This will give us better
insight into how the model is performing and whether it is able to
simulate realistic and unbiased future values

``` r
ppc(lynx_mvgam, series = 1, type = 'rootogram', newdata = lynx_test)
```

<img src="README-unnamed-chunk-32-1.png" width="60%" style="display: block; margin: auto;" />

``` r
ppc(lynx_mvgam, series = 1, type = 'mean', newdata = lynx_test)
```

<img src="README-unnamed-chunk-33-1.png" width="60%" style="display: block; margin: auto;" />

``` r
ppc(lynx_mvgam, series = 1, type = 'cdf', newdata = lynx_test)
```

<img src="README-unnamed-chunk-34-1.png" width="60%" style="display: block; margin: auto;" />

``` r
ppc(lynx_mvgam, series = 1, type = 'pit', newdata = lynx_test)
```

<img src="README-unnamed-chunk-35-1.png" width="60%" style="display: block; margin: auto;" />

A key aspect of ecological forecasting is to understand [how different
components of a model contribute to forecast
uncertainty](https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/eap.1589).
We can estimate relative contributions to forecast uncertainty for the
GAM component and the latent trend component using `mvgam`

``` r
plot_mvgam_uncertainty(lynx_mvgam, newdata = lynx_test, legend_position = 'none')
text(1, 0.2, cex = 1.5, label="GAM component", 
     pos = 4, col="white", family = 'serif')
text(1, 0.8, cex = 1.5, label="Trend component", 
     pos = 4, col="#7C0000", family = 'serif')
```

<img src="README-unnamed-chunk-36-1.png" width="60%" style="display: block; margin: auto;" />

Both components contribute to forecast uncertainty, suggesting we would
still need some more work to learn about factors driving the dynamics of
the system. But we will leave the model as-is for this example.
Diagnostics of the model can also be performed using `mvgam`. Have a
look at the model’s residuals, which are posterior empirical quantiles
of Dunn-Smyth randomised quantile residuals so should follow approximate
normality. We are primarily looking for a lack of autocorrelation, which
would suggest our AR3 model is appropriate for the latent trend

``` r
plot(lynx_mvgam, type = 'residuals')
```

<img src="README-unnamed-chunk-37-1.png" width="60%" style="display: block; margin: auto;" />

Another useful utility of `mvgam` is the ability to use rolling window
forecasts to evaluate competing models that may represent different
hypotheses about the series dynamics. Here we will fit a poorly
specified model to showcase how this evaluation works. In this model, we
ignore the cyclic pattern of seasonality. We also use a random walk
process for the trend

``` r
lynx_mvgam_poor <- mvgam(data = lynx_train,
               newdata = lynx_test,
               formula = population ~ 1,
               family = 'poisson',
               trend_model = 'RW',
               use_stan = TRUE,
               chains = 4)
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 1 Iteration:   1 / 1000 [  0%]  (Warmup) 
#> Chain 2 Iteration:   1 / 1000 [  0%]  (Warmup) 
#> Chain 3 Iteration:   1 / 1000 [  0%]  (Warmup) 
#> Chain 4 Iteration:   1 / 1000 [  0%]  (Warmup) 
#> Chain 1 Iteration: 100 / 1000 [ 10%]  (Warmup) 
#> Chain 2 Iteration: 100 / 1000 [ 10%]  (Warmup) 
#> Chain 4 Iteration: 100 / 1000 [ 10%]  (Warmup) 
#> Chain 1 Iteration: 200 / 1000 [ 20%]  (Warmup) 
#> Chain 1 Iteration: 300 / 1000 [ 30%]  (Warmup) 
#> Chain 1 Iteration: 400 / 1000 [ 40%]  (Warmup) 
#> Chain 1 Iteration: 500 / 1000 [ 50%]  (Warmup) 
#> Chain 1 Iteration: 501 / 1000 [ 50%]  (Sampling) 
#> Chain 2 Iteration: 200 / 1000 [ 20%]  (Warmup) 
#> Chain 2 Iteration: 300 / 1000 [ 30%]  (Warmup) 
#> Chain 2 Iteration: 400 / 1000 [ 40%]  (Warmup) 
#> Chain 2 Iteration: 500 / 1000 [ 50%]  (Warmup) 
#> Chain 2 Iteration: 501 / 1000 [ 50%]  (Sampling) 
#> Chain 3 Iteration: 100 / 1000 [ 10%]  (Warmup) 
#> Chain 4 Iteration: 200 / 1000 [ 20%]  (Warmup) 
#> Chain 4 Iteration: 300 / 1000 [ 30%]  (Warmup) 
#> Chain 4 Iteration: 400 / 1000 [ 40%]  (Warmup) 
#> Chain 4 Iteration: 500 / 1000 [ 50%]  (Warmup) 
#> Chain 4 Iteration: 501 / 1000 [ 50%]  (Sampling) 
#> Chain 1 Iteration: 600 / 1000 [ 60%]  (Sampling) 
#> Chain 1 Iteration: 700 / 1000 [ 70%]  (Sampling) 
#> Chain 1 Iteration: 800 / 1000 [ 80%]  (Sampling) 
#> Chain 2 Iteration: 600 / 1000 [ 60%]  (Sampling) 
#> Chain 2 Iteration: 700 / 1000 [ 70%]  (Sampling) 
#> Chain 3 Iteration: 200 / 1000 [ 20%]  (Warmup) 
#> Chain 3 Iteration: 300 / 1000 [ 30%]  (Warmup) 
#> Chain 3 Iteration: 400 / 1000 [ 40%]  (Warmup) 
#> Chain 3 Iteration: 500 / 1000 [ 50%]  (Warmup) 
#> Chain 3 Iteration: 501 / 1000 [ 50%]  (Sampling) 
#> Chain 3 Iteration: 600 / 1000 [ 60%]  (Sampling) 
#> Chain 4 Iteration: 600 / 1000 [ 60%]  (Sampling) 
#> Chain 4 Iteration: 700 / 1000 [ 70%]  (Sampling) 
#> Chain 1 Iteration: 900 / 1000 [ 90%]  (Sampling) 
#> Chain 1 Iteration: 1000 / 1000 [100%]  (Sampling) 
#> Chain 2 Iteration: 800 / 1000 [ 80%]  (Sampling) 
#> Chain 2 Iteration: 900 / 1000 [ 90%]  (Sampling) 
#> Chain 3 Iteration: 700 / 1000 [ 70%]  (Sampling) 
#> Chain 3 Iteration: 800 / 1000 [ 80%]  (Sampling) 
#> Chain 4 Iteration: 800 / 1000 [ 80%]  (Sampling) 
#> Chain 4 Iteration: 900 / 1000 [ 90%]  (Sampling) 
#> Chain 1 finished in 1.3 seconds.
#> Chain 2 Iteration: 1000 / 1000 [100%]  (Sampling) 
#> Chain 2 finished in 1.3 seconds.
#> Chain 4 Iteration: 1000 / 1000 [100%]  (Sampling) 
#> Chain 4 finished in 1.3 seconds.
#> Chain 3 Iteration: 900 / 1000 [ 90%]  (Sampling) 
#> Chain 3 Iteration: 1000 / 1000 [100%]  (Sampling) 
#> Chain 3 finished in 1.4 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 1.3 seconds.
#> Total execution time: 1.5 seconds.
```

We choose a set of timepoints within the training data to forecast from,
allowing us to simulate a situation where the model’s parameters had
already been estimated but we have only observed data up to the
evaluation timepoint and would like to generate forecasts from the
latent trends. Here we simulate scenarios where we forecast ahead for
the next 10 years. The `compare_mvgams` function automates this process
by rolling along a set of timepoints for each model, ensuring a more
in-depth evaluation of each competing model at the same set of
timepoints.

``` r
compare_mvgams(lynx_mvgam, lynx_mvgam_poor, fc_horizon = 10)
#> RPS summaries per model (lower is better)
#>             Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
#> Model 1 1242.028 1513.598 1612.644 1674.693 1791.964 2408.058
#> Model 2 1902.154 2000.629 2027.249 2040.265 2097.316 2188.545
#> 
#> 90% interval coverages per model (closer to 0.9 is better)
#> Model 1 0.88 
#> Model 2 0.94
```

<img src="README-unnamed-chunk-39-1.png" width="60%" style="display: block; margin: auto;" /><img src="README-unnamed-chunk-39-2.png" width="60%" style="display: block; margin: auto;" /><img src="README-unnamed-chunk-39-3.png" width="60%" style="display: block; margin: auto;" />

Summary statistics of the two models’ out of sample Discrete Rank
Probability Score (DRPS) indicate that the well-specified model performs
markedly better (lower DRPS) across most out of sample horizons. There
are many more extended uses for `mvgam` models, including the ability to
fit dynamic factor processes for analysing and forecasting sets of
multivariate time series

## Extended observation families

`mvgam` was originally designed to analyse and forecast non-negative
integer-valued data (counts). These data are traditionally challenging
to analyse with existing time-series analysis packages. But further
development of `mvgam` has resulted in support for a growing number of
observation families that extend to other types of data. Currently, the
package can handle data for the following families:

- `gaussian()` for real-valued data
- `student_t()` for heavy-tailed real-valued data
- `lognormal()` for non-negative real-valued data
- `betar()` for proportional data on `(0,1)`
- `poisson()` for count data
- `nb()` for overdispersed count data
- `tweedie()` for overdispersed count data

Note that only `poisson()`, `nb()`, and `tweedie()` are available if
using `JAGS`. All families, apart from `tweedie()`, are supported if
using `Stan`. See `??mvgam_families` for more information. A simple
example for simulating and modelling proportional data with `Beta`
observations over a set of seasonal series with independent Gaussian
Process dynamic trends:

``` r
set.seed(100)
data <- sim_mvgam(family = betar(),
                 T = 80,
                 trend_model = 'GP',
                 trend_rel = 0.5, 
                 seasonality = 'shared')
plot_mvgam_series(data = data$data_train, series = 'all')
```

<img src="README-unnamed-chunk-40-1.png" width="60%" style="display: block; margin: auto;" />

``` r
mod <- mvgam(y ~ s(season),
             trend_model = 'GP',
             data = data$data_train,
             newdata = data$data_test,
             family = betar(),
             run_model = TRUE)
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 1 Iteration:   1 / 1000 [  0%]  (Warmup) 
#> Chain 2 Iteration:   1 / 1000 [  0%]  (Warmup) 
#> Chain 3 Iteration:   1 / 1000 [  0%]  (Warmup) 
#> Chain 4 Iteration:   1 / 1000 [  0%]  (Warmup) 
#> Chain 4 Iteration: 100 / 1000 [ 10%]  (Warmup) 
#> Chain 3 Iteration: 100 / 1000 [ 10%]  (Warmup) 
#> Chain 1 Iteration: 100 / 1000 [ 10%]  (Warmup) 
#> Chain 2 Iteration: 100 / 1000 [ 10%]  (Warmup) 
#> Chain 4 Iteration: 200 / 1000 [ 20%]  (Warmup) 
#> Chain 3 Iteration: 200 / 1000 [ 20%]  (Warmup) 
#> Chain 1 Iteration: 200 / 1000 [ 20%]  (Warmup) 
#> Chain 2 Iteration: 200 / 1000 [ 20%]  (Warmup) 
#> Chain 4 Iteration: 300 / 1000 [ 30%]  (Warmup) 
#> Chain 3 Iteration: 300 / 1000 [ 30%]  (Warmup) 
#> Chain 1 Iteration: 300 / 1000 [ 30%]  (Warmup) 
#> Chain 2 Iteration: 300 / 1000 [ 30%]  (Warmup) 
#> Chain 4 Iteration: 400 / 1000 [ 40%]  (Warmup) 
#> Chain 3 Iteration: 400 / 1000 [ 40%]  (Warmup) 
#> Chain 2 Iteration: 400 / 1000 [ 40%]  (Warmup) 
#> Chain 1 Iteration: 400 / 1000 [ 40%]  (Warmup) 
#> Chain 4 Iteration: 500 / 1000 [ 50%]  (Warmup) 
#> Chain 4 Iteration: 501 / 1000 [ 50%]  (Sampling) 
#> Chain 3 Iteration: 500 / 1000 [ 50%]  (Warmup) 
#> Chain 3 Iteration: 501 / 1000 [ 50%]  (Sampling) 
#> Chain 1 Iteration: 500 / 1000 [ 50%]  (Warmup) 
#> Chain 2 Iteration: 500 / 1000 [ 50%]  (Warmup) 
#> Chain 1 Iteration: 501 / 1000 [ 50%]  (Sampling) 
#> Chain 2 Iteration: 501 / 1000 [ 50%]  (Sampling) 
#> Chain 4 Iteration: 600 / 1000 [ 60%]  (Sampling) 
#> Chain 3 Iteration: 600 / 1000 [ 60%]  (Sampling) 
#> Chain 2 Iteration: 600 / 1000 [ 60%]  (Sampling) 
#> Chain 1 Iteration: 600 / 1000 [ 60%]  (Sampling) 
#> Chain 4 Iteration: 700 / 1000 [ 70%]  (Sampling) 
#> Chain 3 Iteration: 700 / 1000 [ 70%]  (Sampling) 
#> Chain 2 Iteration: 700 / 1000 [ 70%]  (Sampling) 
#> Chain 1 Iteration: 700 / 1000 [ 70%]  (Sampling) 
#> Chain 4 Iteration: 800 / 1000 [ 80%]  (Sampling) 
#> Chain 3 Iteration: 800 / 1000 [ 80%]  (Sampling) 
#> Chain 2 Iteration: 800 / 1000 [ 80%]  (Sampling) 
#> Chain 1 Iteration: 800 / 1000 [ 80%]  (Sampling) 
#> Chain 4 Iteration: 900 / 1000 [ 90%]  (Sampling) 
#> Chain 3 Iteration: 900 / 1000 [ 90%]  (Sampling) 
#> Chain 2 Iteration: 900 / 1000 [ 90%]  (Sampling) 
#> Chain 1 Iteration: 900 / 1000 [ 90%]  (Sampling) 
#> Chain 4 Iteration: 1000 / 1000 [100%]  (Sampling) 
#> Chain 4 finished in 10.6 seconds.
#> Chain 3 Iteration: 1000 / 1000 [100%]  (Sampling) 
#> Chain 3 finished in 10.9 seconds.
#> Chain 2 Iteration: 1000 / 1000 [100%]  (Sampling) 
#> Chain 2 finished in 11.2 seconds.
#> Chain 1 Iteration: 1000 / 1000 [100%]  (Sampling) 
#> Chain 1 finished in 11.5 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 11.0 seconds.
#> Total execution time: 11.6 seconds.
```

Inspect the summary to see that the posterior now also contains
estimates for the `Beta` precision parameters

``` r
summary(mod)
#> GAM formula:
#> y ~ s(season)
#> 
#> Family:
#> beta
#> 
#> Link function:
#> logit
#> 
#> Trend model:
#> GP
#> 
#> N series:
#> 3
#> 
#> N timepoints:
#> 60
#> 
#> Status:
#> Fitted using Stan
#> 
#> Precision parameter estimates:
#>            2.5%      50%     97.5% Rhat n.eff
#> phi[1] 4.822165 7.170550 10.217747    1  1604
#> phi[2] 5.002601 7.408465 10.580300    1  1931
#> phi[3] 4.776052 6.980360  9.812386    1  2032
#> 
#> GAM coefficient (beta) estimates:
#>                    2.5%         50%     97.5% Rhat n.eff
#> (Intercept) -0.05173722  0.19748650 0.4282204    1  1167
#> s(season).1 -0.11793685  0.26103100 0.6928650    1   944
#> s(season).2 -1.14657450 -0.02756155 1.0477435    1   566
#> s(season).3 -0.39962398 -0.10377000 0.1851409    1   896
#> s(season).4 -0.43561682  0.27143750 1.0114200    1   596
#> s(season).5 -0.22351128  0.04889930 0.2840670    1   745
#> s(season).6 -0.78156598 -0.20326150 0.3368906    1   575
#> s(season).7 -0.24723035 -0.06135585 0.1479203    1   626
#> s(season).8 -2.88301175 -1.29728000 0.1681699    1   557
#> s(season).9 -0.64988235 -0.10616250 0.2520681    1   759
#> 
#> GAM smoothing parameter (rho) estimates:
#>                  2.5%       50%    97.5% Rhat n.eff
#> s(season)  -1.2235650 0.3143075 1.522726    1   815
#> s(season)2  0.5435733 3.0842000 4.145387    1   661
#> 
#> Latent trend marginal deviation (alpha) and length scale (rho) estimates:
#>                   2.5%        50%     97.5% Rhat n.eff
#> alpha_gp[1] 0.27955497  0.5864015  1.121270 1.00   851
#> alpha_gp[2] 0.36947950  0.6985970  1.257831 1.00  1385
#> alpha_gp[3] 0.08670639  0.3847510  0.899009 1.00  1112
#> rho_gp[1]   1.48049425  5.8454300 23.393262 1.00   666
#> rho_gp[2]   2.39939700 19.4051000 61.502100 1.01   458
#> rho_gp[3]   1.43602625  7.8401450 52.924342 1.00   852
#> 
#> Stan MCMC diagnostics
#> n_eff / iter looks reasonable for all parameters
#> Rhat looks reasonable for all parameters
#> 16 of 2000 iterations ended with a divergence (0.8%)
#> *Try running with larger adapt_delta to remove the divergences
#> 0 of 2000 iterations saturated the maximum tree depth of 12 (0%)
#> E-FMI indicated no pathological behavior
#> 
```

Plot the hindcast and forecast distributions for one series

``` r
plot(mod, type = 'forecast', newdata = data$data_test, series = 2)
#> Out of sample CRPS:
#> [1] 1.79123
#> 
```

<img src="README-unnamed-chunk-43-1.png" width="60%" style="display: block; margin: auto;" />

## Dynamic coefficient models

Dynamic fixed-effect coefficients (often referred to as dynamic linear
models) can also be readily incorporated into GAMs / DGAMs. In `mvgam`,
the `dynamic()` formula wrapper is used to set these up. The plan is to
incorporate a range of dynamic options (such as random walk, ar1 etc…)
but for the moment only low-rank Gaussian Process smooths are allowed
(making use of the `gp` basis in `mgcv`). An example below illustrates:

Simulate a time-varying coefficient using a squared exponential Gaussian
Process function with length scale $\rho$=10

``` r
set.seed(1111)
N = 200
beta_temp <- mvgam:::sim_gp(rnorm(1),
                            alpha_gp = 0.75,
                            rho_gp = 10,
                            h = N) + 0.5
plot(beta_temp, type = 'l', lwd = 3, 
     bty = 'l', xlab = 'Time', ylab = 'Coefficient',
     col = 'darkred')
box(bty = 'l', lwd = 2)
```

<img src="README-unnamed-chunk-44-1.png" width="60%" style="display: block; margin: auto;" />

Now simulate the outcome, which is a Gaussian observation process (with
observation error) over the time-varying effect of $temperature$

``` r
temp <- rnorm(N, sd = 1)
out <- rnorm(N, mean = 4 + beta_temp * temp,
             sd = 0.25)
time <- seq_along(temp)
plot(out,  type = 'l', lwd = 3, 
     bty = 'l', xlab = 'Time', ylab = 'Outcome',
     col = 'darkred')
box(bty = 'l', lwd = 2)
```

<img src="README-unnamed-chunk-45-1.png" width="60%" style="display: block; margin: auto;" />

Gather the data into a `data.frame` and fit a model using the
`dynamic()` formula wrapper to specify a low-rank Gaussian Process
smooth function to estimate the time-varying coefficient of
$temperature$. We will mis-specify the $\rho$ parameter here as, in
practice, it is never known

``` r
data = data.frame(out, temp, time)
data_train <- data[1:190,]
data_test <- data[191:200,]
mod <- mvgam(out ~ dynamic(temp, rho = 8, stationary = TRUE),
             family = gaussian(),
             data = data_train)
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 1 Iteration:   1 / 1000 [  0%]  (Warmup) 
#> Chain 2 Iteration:   1 / 1000 [  0%]  (Warmup) 
#> Chain 3 Iteration:   1 / 1000 [  0%]  (Warmup) 
#> Chain 4 Iteration:   1 / 1000 [  0%]  (Warmup) 
#> Chain 1 Iteration: 100 / 1000 [ 10%]  (Warmup) 
#> Chain 1 Iteration: 200 / 1000 [ 20%]  (Warmup) 
#> Chain 2 Iteration: 100 / 1000 [ 10%]  (Warmup) 
#> Chain 2 Iteration: 200 / 1000 [ 20%]  (Warmup) 
#> Chain 3 Iteration: 100 / 1000 [ 10%]  (Warmup) 
#> Chain 3 Iteration: 200 / 1000 [ 20%]  (Warmup) 
#> Chain 4 Iteration: 100 / 1000 [ 10%]  (Warmup) 
#> Chain 4 Iteration: 200 / 1000 [ 20%]  (Warmup) 
#> Chain 1 Iteration: 300 / 1000 [ 30%]  (Warmup) 
#> Chain 2 Iteration: 300 / 1000 [ 30%]  (Warmup) 
#> Chain 3 Iteration: 300 / 1000 [ 30%]  (Warmup) 
#> Chain 4 Iteration: 300 / 1000 [ 30%]  (Warmup) 
#> Chain 1 Iteration: 400 / 1000 [ 40%]  (Warmup) 
#> Chain 2 Iteration: 400 / 1000 [ 40%]  (Warmup) 
#> Chain 3 Iteration: 400 / 1000 [ 40%]  (Warmup) 
#> Chain 4 Iteration: 400 / 1000 [ 40%]  (Warmup) 
#> Chain 1 Iteration: 500 / 1000 [ 50%]  (Warmup) 
#> Chain 1 Iteration: 501 / 1000 [ 50%]  (Sampling) 
#> Chain 2 Iteration: 500 / 1000 [ 50%]  (Warmup) 
#> Chain 2 Iteration: 501 / 1000 [ 50%]  (Sampling) 
#> Chain 3 Iteration: 500 / 1000 [ 50%]  (Warmup) 
#> Chain 3 Iteration: 501 / 1000 [ 50%]  (Sampling) 
#> Chain 4 Iteration: 500 / 1000 [ 50%]  (Warmup) 
#> Chain 4 Iteration: 501 / 1000 [ 50%]  (Sampling) 
#> Chain 2 Iteration: 600 / 1000 [ 60%]  (Sampling) 
#> Chain 4 Iteration: 600 / 1000 [ 60%]  (Sampling) 
#> Chain 1 Iteration: 600 / 1000 [ 60%]  (Sampling) 
#> Chain 3 Iteration: 600 / 1000 [ 60%]  (Sampling) 
#> Chain 1 Iteration: 700 / 1000 [ 70%]  (Sampling) 
#> Chain 2 Iteration: 700 / 1000 [ 70%]  (Sampling) 
#> Chain 3 Iteration: 700 / 1000 [ 70%]  (Sampling) 
#> Chain 4 Iteration: 700 / 1000 [ 70%]  (Sampling) 
#> Chain 1 Iteration: 800 / 1000 [ 80%]  (Sampling) 
#> Chain 2 Iteration: 800 / 1000 [ 80%]  (Sampling) 
#> Chain 3 Iteration: 800 / 1000 [ 80%]  (Sampling) 
#> Chain 4 Iteration: 800 / 1000 [ 80%]  (Sampling) 
#> Chain 1 Iteration: 900 / 1000 [ 90%]  (Sampling) 
#> Chain 2 Iteration: 900 / 1000 [ 90%]  (Sampling) 
#> Chain 3 Iteration: 900 / 1000 [ 90%]  (Sampling) 
#> Chain 4 Iteration: 900 / 1000 [ 90%]  (Sampling) 
#> Chain 1 Iteration: 1000 / 1000 [100%]  (Sampling) 
#> Chain 2 Iteration: 1000 / 1000 [100%]  (Sampling) 
#> Chain 3 Iteration: 1000 / 1000 [100%]  (Sampling) 
#> Chain 4 Iteration: 1000 / 1000 [100%]  (Sampling) 
#> Chain 1 finished in 1.7 seconds.
#> Chain 2 finished in 1.6 seconds.
#> Chain 3 finished in 1.7 seconds.
#> Chain 4 finished in 1.7 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 1.7 seconds.
#> Total execution time: 1.8 seconds.
```

Inspect the model summary, which now contains estimates for the
observation errors

``` r
summary(mod)
#> GAM formula:
#> out ~ dynamic(temp, rho = 8, stationary = TRUE)
#> 
#> Family:
#> gaussian
#> 
#> Link function:
#> identity
#> 
#> Trend model:
#> None
#> 
#> N series:
#> 1
#> 
#> N timepoints:
#> 190
#> 
#> Status:
#> Fitted using Stan
#> 
#> Observation error parameter estimates:
#>                  2.5%       50%     97.5% Rhat n.eff
#> sigma_obs[1] 0.220982 0.2448605 0.2743896    1  2068
#> 
#> GAM coefficient (beta) estimates:
#>                         2.5%          50%        97.5% Rhat n.eff
#> (Intercept)      3.933876000  3.965385000  3.998452250 1.00  3404
#> s(time):temp.1  -0.731473775 -0.249871500  0.124949500 1.01   271
#> s(time):temp.2   0.048729852  0.089573650  0.135720075 1.00  1780
#> s(time):temp.3  -0.126036450  0.047858350  0.180812175 1.01   289
#> s(time):temp.4   0.135387300  0.181695500  0.226471825 1.00  2042
#> s(time):temp.5  -0.532886000 -0.433848500 -0.317227775 1.01   333
#> s(time):temp.6   0.108744950  0.157482500  0.206032525 1.00  2128
#> s(time):temp.7  -0.275140025 -0.177325500 -0.094990803 1.01   347
#> s(time):temp.8  -0.730431725 -0.680667500 -0.630035700 1.00  2479
#> s(time):temp.9   0.060638858  0.137319000  0.228954425 1.01   410
#> s(time):temp.10  0.233985825  0.292665500  0.356705300 1.00  2256
#> s(time):temp.11 -0.240652025 -0.145699000 -0.064564025 1.00   463
#> s(time):temp.12  0.176815550  0.252308000  0.328240275 1.00  2382
#> s(time):temp.13 -0.235896750 -0.138047500 -0.032274273 1.00   581
#> s(time):temp.14 -0.055483455  0.035965400  0.130127400 1.00  1847
#> s(time):temp.15 -0.280968500 -0.168804500 -0.039261478 1.00   809
#> s(time):temp.16  0.007824585  0.124738500  0.246307800 1.00  2188
#> s(time):temp.17 -0.156658125 -0.004226335  0.142031150 1.00   967
#> s(time):temp.18  0.035837915  0.167032500  0.301348575 1.00  1976
#> s(time):temp.19 -0.090682515  0.094611900  0.271414425 1.00  1126
#> s(time):temp.20 -0.131781125  0.041016250  0.233267925 1.00  2104
#> s(time):temp.21 -0.099012435  0.124226500  0.349815675 1.00  1692
#> s(time):temp.22 -0.518186375 -0.264716000 -0.008269355 1.00  2453
#> s(time):temp.23 -0.455239800 -0.141528500  0.167827975 1.00  1578
#> s(time):temp.24 -0.695031500 -0.357737500 -0.023859470 1.00  1755
#> s(time):temp.25 -0.374841450 -0.005545590  0.370185050 1.00  1870
#> s(time):temp.26 -0.502629325 -0.090190800  0.297385500 1.00  2025
#> s(time):temp.27 -0.752633825 -0.198780000  0.220772125 1.01   268
#> 
#> GAM smoothing parameter (rho) estimates:
#>                    2.5%       50%      97.5% Rhat n.eff
#> s(time):temp  -1.639277 -1.362130 -0.9917427    1  1242
#> s(time):temp2 -2.786229  1.001594  3.6922605    1  1304
#> 
#> Stan MCMC diagnostics
#> n_eff / iter looks reasonable for all parameters
#> Rhat looks reasonable for all parameters
#> 6 of 2000 iterations ended with a divergence (0.3%)
#> *Try running with larger adapt_delta to remove the divergences
#> 0 of 2000 iterations saturated the maximum tree depth of 12 (0%)
#> E-FMI indicated no pathological behavior
#> 
```

Plot the estimated time-varying coefficient for the in-sample training
period

``` r
plot(mod, type = 'smooths')
```

<img src="README-unnamed-chunk-48-1.png" width="60%" style="display: block; margin: auto;" />

Plot the estimates for the in-sample and out-of-sample periods to see
how the Gaussian Process function produces sensible smooth forecasts.
Overlay the true simulated function to see that the model has adequately
estimated it’s dynamics in both the training and testing data partitions

``` r
plot_mvgam_smooth(mod, smooth = 1, newdata = data)
abline(v = 190, lty = 'dashed', lwd = 2)
lines(beta_temp, lwd = 2.5, col = 'white')
lines(beta_temp, lwd = 2)
```

<img src="README-unnamed-chunk-49-1.png" width="60%" style="display: block; margin: auto;" />

This results in sensible forecasts of the observations as well

``` r
plot(mod, type = 'forecast', newdata = data_test)
#> Out of sample CRPS:
#> [1] 1.704194
#> 
```

<img src="README-unnamed-chunk-50-1.png" width="60%" style="display: block; margin: auto;" />

## License

This project is licensed under an `MIT` open source license
