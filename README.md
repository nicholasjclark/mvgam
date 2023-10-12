
<!-- README.md is generated from README.Rmd. Please edit that file -->

# *mvgam*

The goal of `mvgam` is to use a Bayesian framework to estimate
parameters of Generalized Additive Models (DGAMs) for time series with
dynamic trend components. The motivation for the package and some of its
primary objectives are described in detail by <a
href="https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13974"
target="_blank">Clark &amp; Wells 2022</a> (published in *Methods in
Ecology and Evolution*), with additional inspiration on the use of
Bayesian probabilistic modelling to quantify uncertainty and advise
principled decision making coming from
<a href="https://betanalpha.github.io/writing/" target="_blank">Michael
Betancourt</a>,
<a href="https://www.bu.edu/earth/profiles/michael-dietze/"
target="_blank">Michael Dietze</a> and
<a href="https://emilybfox.su.domains/" target="_blank">Emily Fox</a>,
among many others.

## Resources

A number of case studies have been compiled to highlight how DGAMs can
be estimated using MCMC sampling. These are hosted currently on `RPubs`
at the following links:

- <a href="https://rpubs.com/NickClark47/mvgam" target="_blank">mvgam case
  study 1: model comparison and data assimilation</a>
- <a href="https://rpubs.com/NickClark47/mvgam2" target="_blank">mvgam
  case study 2: multivariate models</a>
- <a href="https://rpubs.com/NickClark47/mvgam3" target="_blank">mvgam
  case study 3: distributed lag models</a>

The package can also be used to generate all necessary data structures,
initial value functions and modelling code necessary to fit DGAMs using
`Stan` or `JAGS`. This can be helpful if users wish to make changes to
the model to better suit their own bespoke research / analysis goals.
The following resources can be helpful to troubleshoot:

- <a href="https://discourse.mc-stan.org/" target="_blank">Stan
  Discourse</a>
- <a href="https://sourceforge.net/projects/mcmc-jags/"
  target="_blank">JAGS Discourse</a>

## Installation

Install the development version from `GitHub` using:
`devtools::install_github("nicholasjclark/mvgam")`. Note that to
actually condition models with MCMC sampling, either the `JAGS` software
must be installed (along with the `R` packages `rjags` and `runjags`) or
the `Stan` software must be installed (along with either `rstan` and/or
`cmdstanr`). Only `rstan` is listed as a dependency of `mvgam` to ensure
that installation is less difficult. If users wish to fit the models
using `mvgam`, please refer to installation links for `JAGS`
<a href="https://sourceforge.net/projects/mcmc-jags/files/"
target="_blank">here</a>, for `Stan` with `rstan`
<a href="https://mc-stan.org/users/interfaces/rstan"
target="_blank">here</a>, or for `Stan` with `cmdstandr`
<a href="https://mc-stan.org/cmdstanr/" target="_blank">here</a>. You
will need a fairly recent version of `Stan` to ensure all the model
syntax is recognized. If you see warnings such as
`variable "array" does not exist`, this is usually a sign that you need
to update your version of `Stan`. We highly recommend you use `Cmdstan`
through the `cmdstanr` interface as the backend. This is because
`Cmdstan` is easier to install, is more up to date with new features,
and uses less memory than `Rstan`. See <a
href="http://mc-stan.org/cmdstanr/articles/cmdstanr.html#comparison-with-rstan"
target="_blank">this documentation from the <code>Cmdstan</code> team
for more information</a>.

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

`mvgam` relies on several other `R` packages and, of course, on `R`
itself. To find out how to cite R and its packages, use the `citation`
function. There are some features of `mvgam` which specifically rely on
certain packages. The most important of these is the generation of data
necessary to estimate smoothing splines, which entirely rely on `mgcv`.
The `rstan` and `cmdstanr` packages together with `Rcpp` makes `Stan`
conveniently accessible in `R`, while the `rjags` and `runjags` packages
together with the `coda` package make `JAGS` accessible in `R`. If you
use some of these features, please also consider citing the related
packages.

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
- `VAR1cor` Vector Autoregressive model with VAR coefficients for lag 1;
  contemporaneously correlated (multivariate) errors
- `GP` Squared exponential Gaussian Process
- `None` No latent trend is fitted

When using `Stan` as the back-end, all of these trend models (apart from
`VAR` models) can be estimated using a set of dimension-reduced dynamic
factors. Please see
<a href="https://rpubs.com/NickClark47/mvgam2" target="_blank">mvgam
case study 2: multivariate models</a> for more information

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
#> Loading required package: Rcpp
#> Warning: package 'Rcpp' was built under R version 4.2.3
#> Loading required package: brms
#> Warning: package 'brms' was built under R version 4.2.3
#> Loading 'brms' package (version 2.19.0). Useful instructions
#> can be found by typing help('brms'). A more detailed introduction
#> to the package is available through vignette('brms_overview').
#> 
#> Attaching package: 'brms'
#> The following objects are masked from 'package:mgcv':
#> 
#>     s, t2
#> The following object is masked from 'package:stats':
#> 
#>     ar
#> Loading required package: marginaleffects
#> Warning: package 'marginaleffects' was built under R version 4.2.3
#> Loading required package: insight
#> Warning: package 'insight' was built under R version 4.2.3
#> Welcome to mvgam. Please cite as: Clark, NJ, and Wells, K. 2022. Dynamic Generalized Additive Models (DGAMs) for forecasting discrete ecological time series. Methods in Ecology and Evolution, 2022, https://doi.org/10.1111/2041-210X.13974
data(lynx)
lynx_full = data.frame(year = 1821:1934, 
                       population = as.numeric(lynx))
plot(lynx_full$population, type = 'l', ylab = 'Lynx trappings',
     xlab = 'Time', bty = 'l', lwd = 2)
box(bty = 'l', lwd  = 2)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="60%" style="display: block; margin: auto;" />

``` r
acf(lynx_full$population, main = '', bty = 'l', lwd = 2,
    ci.col = 'darkred')
box(bty = 'l', lwd  = 2)
```

<img src="man/figures/README-unnamed-chunk-2-2.png" width="60%" style="display: block; margin: auto;" />

Along with serial autocorrelation, there is a clear ~19-year cyclic
pattern to the data. Create a `season` term that can be used to model
this effect and give a better representation of the data generating
process than we would likely get with a linear model

``` r
plot(stl(ts(lynx_full$population, frequency = 19), s.window = 'periodic'),
     lwd = 2, col.range = 'darkred')
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="60%" style="display: block; margin: auto;" />

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

<img src="man/figures/README-unnamed-chunk-6-1.png" width="60%" style="display: block; margin: auto;" />

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

<img src="man/figures/README-unnamed-chunk-9-1.png" width="60%" style="display: block; margin: auto;" />

Plot a set of realisations from the prior seasonal smooth function

``` r
plot(lynx_mvgam_prior, type = 'smooths', realisations = TRUE,
     n_realisations = 20)
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="60%" style="display: block; margin: auto;" />

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

<img src="man/figures/README-unnamed-chunk-14-1.png" width="60%" style="display: block; margin: auto;" />

``` r
plot(lynx_mvgam_prior, type = 'smooths', realisations = TRUE,
     n_realisations = 20)
```

<img src="man/figures/README-unnamed-chunk-15-1.png" width="60%" style="display: block; margin: auto;" />

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
#>                                    param_name param_length
#> 1                                 (Intercept)            1
#> 2               vector<lower=0>[n_sp] lambda;            1
#> 3 vector<lower=-1.5,upper=1.5>[n_series] ar1;            1
#> 4 vector<lower=-1.5,upper=1.5>[n_series] ar2;            1
#> 5 vector<lower=-1.5,upper=1.5>[n_series] ar3;            1
#> 6            vector<lower=0>[n_series] sigma;            1
#>                    param_info                                 prior
#> 1                 (Intercept) (Intercept) ~ student_t(3, 6.5, 2.5);
#> 2 s(season) smooth parameters              lambda ~ normal(10, 25);
#> 3       trend AR1 coefficient                   ar1 ~ std_normal();
#> 4       trend AR2 coefficient                   ar2 ~ std_normal();
#> 5       trend AR3 coefficient                   ar3 ~ std_normal();
#> 6                    trend sd         sigma ~ student_t(3, 0, 2.5);
#>                example_change new_lowerbound new_upperbound
#> 1 (Intercept) ~ normal(0, 1);             NA             NA
#> 2 lambda ~ exponential(0.95);             NA             NA
#> 3  ar1 ~ normal(-0.69, 0.65);             NA             NA
#> 4   ar2 ~ normal(0.19, 0.53);             NA             NA
#> 5  ar3 ~ normal(-0.42, 0.61);             NA             NA
#> 6  sigma ~ exponential(0.78);             NA             NA
```

Any of the above priors can be changed by modifying the `prior` column
and supplying the resulting `data.frame` to the `priors` argument in
`mvgam()`. But for now, we will proceed with the defaults by
conditioning the model on observed data in `Stan` using MCMC sampling
with the `Cmdstan` interface (installation links for `rstan` and
`cmdstanr` are found
<a href="https://mc-stan.org/users/interfaces/rstan"
target="_blank">here</a> and
<a href="https://mc-stan.org/cmdstanr/articles/cmdstanr.html"
target="_blank">here</a>).

``` r
lynx_mvgam <- mvgam(data = lynx_train,
               newdata = lynx_test,
               formula = population ~ s(season, bs = 'cc', k = 12),
               knots = list(season = c(0.5, 19.5)),
               family = 'poisson',
               trend_model = 'AR3',
               use_stan = TRUE)
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
#>   matrix[total_obs, num_basis] X; // mgcv GAM design matrix
#>   array[n, n_series] int<lower=0> ytimes; // time-ordered matrix (which col in X belongs to each [time, series] observation?)
#>   matrix[10, 10] S1; // mgcv smooth penalty matrix S1
#>   int<lower=0> n_nonmissing; // number of nonmissing observations
#>   array[n_nonmissing] int<lower=0> flat_ys; // flattened nonmissing observations
#>   matrix[n_nonmissing, num_basis] flat_xs; // X values for nonmissing observations
#>   array[n_nonmissing] int<lower=0> obs_ind; // indices of nonmissing observations
#> }
#> parameters {
#>   // raw basis coefficients
#>   vector[num_basis] b_raw;
#>   
#>   // latent trend AR1 terms
#>   vector<lower=-1.5, upper=1.5>[n_series] ar1;
#>   
#>   // latent trend AR2 terms
#>   vector<lower=-1.5, upper=1.5>[n_series] ar2;
#>   
#>   // latent trend AR3 terms
#>   vector<lower=-1.5, upper=1.5>[n_series] ar3;
#>   
#>   // latent trend variance parameters
#>   vector<lower=0>[n_series] sigma;
#>   
#>   // latent trends
#>   matrix[n, n_series] trend;
#>   
#>   // smoothing parameters
#>   vector<lower=0>[n_sp] lambda;
#> }
#> transformed parameters {
#>   // basis coefficients
#>   vector[num_basis] b;
#>   b[1 : num_basis] = b_raw[1 : num_basis];
#> }
#> model {
#>   // prior for (Intercept)...
#>   b_raw[1] ~ student_t(3, 6.5, 2.5);
#>   
#>   // prior for s(season)...
#>   b_raw[2 : 11] ~ multi_normal_prec(zero[2 : 11],
#>                                     S1[1 : 10, 1 : 10] * lambda[1]);
#>   
#>   // priors for AR parameters
#>   ar1 ~ std_normal();
#>   ar2 ~ std_normal();
#>   ar3 ~ std_normal();
#>   
#>   // priors for smoothing parameters
#>   lambda ~ normal(10, 25);
#>   
#>   // priors for latent trend variance parameters
#>   sigma ~ student_t(3, 0, 2.5);
#>   
#>   // trend estimates
#>   trend[1, 1 : n_series] ~ normal(0, sigma);
#>   trend[2, 1 : n_series] ~ normal(trend[1, 1 : n_series] * ar1, sigma);
#>   trend[3, 1 : n_series] ~ normal(trend[2, 1 : n_series] * ar1
#>                                   + trend[1, 1 : n_series] * ar2, sigma);
#>   for (s in 1 : n_series) {
#>     trend[4 : n, s] ~ normal(ar1[s] * trend[3 : (n - 1), s]
#>                              + ar2[s] * trend[2 : (n - 2), s]
#>                              + ar3[s] * trend[1 : (n - 3), s], sigma[s]);
#>   }
#>   {
#>     // likelihood functions
#>     vector[n_nonmissing] flat_trends;
#>     flat_trends = to_vector(trend)[obs_ind];
#>     flat_ys ~ poisson_log_glm(append_col(flat_xs, flat_trends), 0.0,
#>                               append_row(b, 1.0));
#>   }
#> }
#> generated quantities {
#>   vector[total_obs] eta;
#>   matrix[n, n_series] mus;
#>   vector[n_sp] rho;
#>   vector[n_series] tau;
#>   array[n, n_series] int ypred;
#>   rho = log(lambda);
#>   for (s in 1 : n_series) {
#>     tau[s] = pow(sigma[s], -2.0);
#>   }
#>   
#>   // posterior predictions
#>   eta = X * b;
#>   for (s in 1 : n_series) {
#>     mus[1 : n, s] = eta[ytimes[1 : n, s]] + trend[1 : n, s];
#>     ypred[1 : n, s] = poisson_log_rng(mus[1 : n, s]);
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

<img src="man/figures/README-unnamed-chunk-20-1.png" width="60%" style="display: block; margin: auto;" />

Next examine simulated empirical Cumulative Distribution Functions (CDF)
for posterior retrodictions (`yhat`) and compare to the CDF of the
observations (`y`)

``` r
ppc(lynx_mvgam, series = 1, type = 'cdf')
```

<img src="man/figures/README-unnamed-chunk-21-1.png" width="60%" style="display: block; margin: auto;" />

Rootograms are becoming
<a href="https://arxiv.org/pdf/1605.01311.pdf" target="_blank">popular
graphical tools for checking a discrete model’s ability to capture
dispersion properties of the response variable</a>. Posterior predictive
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

<img src="man/figures/README-unnamed-chunk-22-1.png" width="60%" style="display: block; margin: auto;" />

Finally look for any biases in predictions by examining a Probability
Integral Transform (PIT) histogram. If our predictions are not biased
one way or another (i.e. not consistently under- or over-predicting),
this histogram should look roughly uniform

``` r
ppc(lynx_mvgam, series = 1, type = 'pit')
```

<img src="man/figures/README-unnamed-chunk-23-1.png" width="60%" style="display: block; margin: auto;" />

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
#>                2.5%    50%  97.5% Rhat n.eff
#> (Intercept)   5.900  6.600  7.000 1.01   411
#> s(season).1  -0.610  0.056  0.730 1.00   883
#> s(season).2  -0.210  0.860  1.900 1.01   391
#> s(season).3  -0.029  1.300  2.400 1.01   357
#> s(season).4  -0.460  0.440  1.300 1.01   816
#> s(season).5  -1.300 -0.180  0.830 1.00   518
#> s(season).6  -1.200 -0.025  1.000 1.01   553
#> s(season).7  -0.720  0.390  1.500 1.00   832
#> s(season).8  -0.960  0.280  1.800 1.01   376
#> s(season).9  -1.200 -0.270  0.740 1.01   510
#> s(season).10 -1.400 -0.710 -0.038 1.01   568
#> 
#> Latent trend AR parameter estimates:
#>           2.5%   50% 97.5% Rhat n.eff
#> ar1[1]    0.75  1.10 1.400 1.00   890
#> ar2[1]   -0.83 -0.40 0.041 1.00  1391
#> ar3[1]   -0.47 -0.11 0.320 1.02   415
#> sigma[1]  0.40  0.50 0.630 1.00  1113
#> 
#> Stan MCMC diagnostics:
#> n_eff / iter looks reasonable for all parameters
#> Rhat looks reasonable for all parameters
#> 0 of 2000 iterations ended with a divergence (0%)
#> 0 of 2000 iterations saturated the maximum tree depth of 12 (0%)
#> E-FMI indicated no pathological behavior
```

As with any `MCMC` based software, we can inspect traceplots. Here for
the `GAM` component smoothing parameters, using `mvgam`’s reliance on
the excellent `bayesplot` library:

``` r
mcmc_plot(lynx_mvgam, variable = 'rho', regex = TRUE, type = 'trace')
#> No divergences to plot.
```

<img src="man/figures/README-unnamed-chunk-25-1.png" width="60%" style="display: block; margin: auto;" />

and for the latent trend component parameters

``` r
mcmc_plot(lynx_mvgam, variable = 'trend_params', regex = TRUE, type = 'trace')
#> No divergences to plot.
```

<img src="man/figures/README-unnamed-chunk-26-1.png" width="60%" style="display: block; margin: auto;" />

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

<img src="man/figures/README-unnamed-chunk-27-1.png" width="60%" style="display: block; margin: auto;" />

It is often also useful to compare prior to posterior function
realisations to understand how informative the observed data have been
for learning these functional shapes

``` r
layout(matrix(1:2, nrow = 2))
plot(lynx_mvgam_prior, type = 'smooths', realisations = TRUE,
     n_realisations = 30)
```

<img src="man/figures/README-unnamed-chunk-28-1.png" width="60%" style="display: block; margin: auto;" />

``` r
plot(lynx_mvgam, type = 'smooths', realisations = TRUE,
     n_realisations = 30)
```

<img src="man/figures/README-unnamed-chunk-28-2.png" width="60%" style="display: block; margin: auto;" />

``` r
layout(1)
```

First derivatives of smooth functions can be plotted to inspect how the
slope of the function changes across its length. To plot these we use
the more flexible `plot_mvgam_smooth()` function

``` r
plot_mvgam_smooth(lynx_mvgam, series = 1, 
                  smooth = 'season', 
                  derivatives = TRUE)
```

<img src="man/figures/README-unnamed-chunk-29-1.png" width="60%" style="display: block; margin: auto;" />

As for many types of regression models, it is often more useful and
intuitive to plot model effects on the scale of the outcome. `mvgam` has
support for the wonderful `marginaleffects` package, allowing a wide
variety of posterior contrasts, averages, conditional and marginal
predictions to be seamlessly calculated and plotted. Below is the
conditional effect of season plotted on the outcome scale, for example:

``` r
require(ggplot2)
#> Loading required package: ggplot2
plot_predictions(lynx_mvgam, condition = 'season', points = 0.5) +
  theme_classic()
```

<img src="man/figures/README-unnamed-chunk-30-1.png" width="60%" style="display: block; margin: auto;" />

We can also view the `mvgam`’s posterior retrodictions and predictions
for the entire series (testing and training)

``` r
plot(lynx_mvgam, type = 'forecast', newdata = lynx_test)
```

<img src="man/figures/README-unnamed-chunk-31-1.png" width="60%" style="display: block; margin: auto;" />

    #> Out of sample DRPS:
    #> [1] 2820.858

And the estimated latent trend component, again using the more flexible
`plot_mvgam_...()` option to show first derivatives of the estimated
trend

``` r
plot_mvgam_trend(lynx_mvgam, newdata = lynx_test, derivatives = TRUE)
```

<img src="man/figures/README-unnamed-chunk-32-1.png" width="60%" style="display: block; margin: auto;" />

We can also re-do the posterior predictive checks, but this time
focusing only on the out of sample period. This will give us better
insight into how the model is performing and whether it is able to
simulate realistic and unbiased future values

``` r
ppc(lynx_mvgam, series = 1, type = 'rootogram', newdata = lynx_test)
```

<img src="man/figures/README-unnamed-chunk-33-1.png" width="60%" style="display: block; margin: auto;" />

``` r
ppc(lynx_mvgam, series = 1, type = 'cdf', newdata = lynx_test)
```

<img src="man/figures/README-unnamed-chunk-34-1.png" width="60%" style="display: block; margin: auto;" />

``` r
ppc(lynx_mvgam, series = 1, type = 'pit', newdata = lynx_test)
```

<img src="man/figures/README-unnamed-chunk-35-1.png" width="60%" style="display: block; margin: auto;" />

A key aspect of ecological forecasting is to understand <a
href="https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/eap.1589"
target="_blank">how different components of a model contribute to
forecast uncertainty</a>. We can estimate relative contributions to
forecast uncertainty for the GAM component and the latent trend
component using `mvgam`

``` r
plot_mvgam_uncertainty(lynx_mvgam, newdata = lynx_test, legend_position = 'none')
text(1, 0.2, cex = 1.5, label="GAM component", 
     pos = 4, col="white", family = 'serif')
text(1, 0.8, cex = 1.5, label="Trend component", 
     pos = 4, col="#7C0000", family = 'serif')
```

<img src="man/figures/README-unnamed-chunk-36-1.png" width="60%" style="display: block; margin: auto;" />

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

<img src="man/figures/README-unnamed-chunk-37-1.png" width="60%" style="display: block; margin: auto;" />

## Comparing models based on forecasts

Another useful utility of `mvgam` is the ability to use approximate
rolling window forecasts to evaluate competing models that may represent
different hypotheses about the series dynamics. Here we will fit a
poorly specified model to showcase how these evaluations works. In this
model, we ignore the cyclic pattern of seasonality. We also use a random
walk process for the trend

``` r
lynx_mvgam_poor <- mvgam(data = lynx_train,
               newdata = lynx_test,
               formula = population ~ 1,
               family = 'poisson',
               trend_model = 'RW',
               use_stan = TRUE,
               chains = 4)
```

The first approximator targets each model’s ability to simulate temporal
dynamics using the single model that has been fit. We choose a set of
timepoints within the training data to forecast from, allowing us to
simulate a situation where the model’s parameters had already been
estimated but we have only observed data up to the evaluation timepoint
and would like to generate forecasts from the latent trends. Here we
simulate scenarios where we forecast ahead for the next 10 years. The
`compare_mvgams` function automates this process by rolling along a set
of timepoints for each model, ensuring a more in-depth evaluation of
each competing model at the same set of timepoints.

``` r
compare_mvgams(lynx_mvgam, lynx_mvgam_poor, fc_horizon = 10)
#> RPS summaries per model (lower is better)
#>             Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
#> Model 1 3836.557  4246.513  4825.217  5228.524  6013.686  7722.069
#> Model 2 5753.894 11795.078 17950.797 15884.080 20015.136 22239.033
#> 
#> 90% interval coverages per model (closer to 0.9 is better)
#> Model 1 0.98 
#> Model 2 0.89
```

<img src="man/figures/README-unnamed-chunk-40-1.png" width="60%" style="display: block; margin: auto;" /><img src="man/figures/README-unnamed-chunk-40-2.png" width="60%" style="display: block; margin: auto;" /><img src="man/figures/README-unnamed-chunk-40-3.png" width="60%" style="display: block; margin: auto;" />

Summary statistics of the two models’ out of sample Discrete Rank
Probability Score (DRPS) indicate that the well-specified model performs
markedly better (lower DRPS) across most out of sample horizons.

The second approximator uses more conventional leave-future-out
comparisons. Time series models are often evaluated using an expanding
window training technique, where the model is initially trained on some
subset of data from `t = 1` to `t = n_train`, and then is used to
produce forecasts for the next `fc_horizon` time steps
`t = n_train + fc_horizon`. In the next iteration, the size of training
data is expanded by a single time point and the process repeated. This
is obviously computationally challenging for Bayesian time series
models, as the number of refits can be very large. `mvgam` uses an
approximation based on importance sampling. Briefly, we refit the model
using the first `min_t` observations to perform a single exact
`fc_horizon`-ahead forecast step. This forecast is evaluated against the
`min_t + fc_horizon` out of sample observations using the Expected Log
Predictive Density (ELPD). Next, we approximate each successive round of
expanding window forecasts by moving forward one step at a time
`for i in 1:N_evaluations` and re-weighting draws from the model’s
posterior predictive distribution using Pareto Smoothed Importance
Sampling (PSIS). In each iteration `i`, PSIS weights are obtained for
all observations that would have been included in the model if we had
re-fit. If these importance ratios are stable, we consider the
approximation adequate and use the re-weighted posterior’s forecast for
evaluating the next holdout set of testing observations
(`(min_t + i + 1):(min_t + i + fc_horizon)`). This is similar to the
process of particle filtering to update forecasts in light of new data
by re-weighting the posterior draws using importance weights. But at
some point the importance ratio variability will become too large and
importance sampling will be unreliable. This is indicated by the
estimated shape parameter `k` of the generalized Pareto distribution
crossing a certain threshold `pareto_k_threshold`. Only then do we refit
the model using all of the observations up to the time of the failure.
We then restart the process and iterate forward until the next refit is
triggered. The process is computationally much more efficient, as only a
fraction of the evaluations typically requires refits (the algorithm is
described in detail by Bürkner et al. 2020).

Paul-Christian Bürkner, Jonah Gabry & Aki Vehtari (2020). Approximate
leave-future-out cross-validation for Bayesian time series models.
Journal of Statistical Computation and Simulation. 90:14, 2499-2523.

For this example, we simulate a single count-valued time series of
length `T = 60`, using a latent `AR1` trend and a cyclic seasonal
pattern. Two models are fit as before, the first is a complex but more
correct model, while the second is simpler and mis-specified. We then
run approximate leave-future-out cross-validation, setting `min_t = 36`
so that the first refit uses all observations from `t = 1` to `t = 36`.
This is done for both models so that we can compare approximate ELPD
values when forecasting two time steps ahead (`fc_horizon = 2`)

``` r
set.seed(12345)
simdat <- sim_mvgam(T = 60, prop_train = 1, n_series = 1,
                    mu = 2,
                    trend_model = 'AR1', trend_rel = 0.35)
good <- mvgam(y ~ s(season, bs = 'cc', k = 8),
              trend_model = 'AR1',
              family = poisson(),
              data = simdat$data_train)
poor <- update(good, formula = y ~ 1,
               trend_model = 'RW')
lfo_good <- lfo_cv(good, min_t = 36,
                   fc_horizon = 2)
lfo_poor <- lfo_cv(poor, min_t = 36,
                   fc_horizon = 2)
```

The `S3` plotting function for these `lfo_cv` objects will show the
Pareto-k values and ELPD values over the evaluation time points. For the
Pareto-k plot, a dashed red line indicates the specified threshold
chosen for triggering model refits. For the ELPD plot, a dashed red line
indicates the bottom 10% quantile of ELPD values. Points below this
threshold may represent outliers that were more difficult to forecast

``` r
par(mar = c(4,4, 1, 1))
plot(lfo_good)
```

<img src="man/figures/README-unnamed-chunk-43-1.png" width="60%" style="display: block; margin: auto;" />

``` r
par(mar = c(4,4, 1, 1))
plot(lfo_poor)
```

<img src="man/figures/README-unnamed-chunk-44-1.png" width="60%" style="display: block; margin: auto;" />

The model with the better ELPD values (higher values are better in this
case) should be preferred. First we can calculate the proportion of
forecast time points in which the first model gives better forecasts
than the poor model

``` r
length(which((lfo_good$elpds - lfo_poor$elpds) > 0)) /
  length(lfo_good$elpds)
#> [1] 0.9545455
```

Total ELPDs per model are also a useful overall indicator of performance

``` r
lfo_good$sum_ELPD
#> [1] -141.4854
lfo_poor$sum_ELPD
#> [1] -166.5148
```

As before, these metrics all favour the more complex model over the
simpler “poor” model. This gives us confidence that the more complex
model will perform better in future forecasting exercises. The first
approximator is faster as it does not require refits, but caution is
needed as the state of the latent trend at the evaluation time point has
already been informed by both the past and future observations. The
second approximator, using PSIS for approximate leave-future-out, should
be preferred when computationally accessible.

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
- `Gamma()` for non-negative real-valued data
- `betar()` for proportional data on `(0,1)`
- `poisson()` for count data
- `nb()` for overdispersed count data
- `tweedie()` for overdispersed count data

Note that only `poisson()`, `nb()`, and `tweedie()` are available if
using `JAGS`. All families, apart from `tweedie()`, are supported if
using `Stan`. See `??mvgam_families` for more information. Below is a
simple example for simulating and modelling proportional data with
`Beta` observations over a set of seasonal series with independent
Gaussian Process dynamic trends:

``` r
set.seed(100)
data <- sim_mvgam(family = betar(),
                 T = 80,
                 trend_model = 'GP',
                 trend_rel = 0.5, 
                 seasonality = 'shared')
plot_mvgam_series(data = data$data_train, series = 'all')
```

<img src="man/figures/README-beta_sim-1.png" width="60%" style="display: block; margin: auto;" />

``` r
mod <- mvgam(y ~ s(season, bs = 'cc', k = 7) +
               s(season, by = series, m = 1, k = 5),
             trend_model = 'GP',
             data = data$data_train,
             newdata = data$data_test,
             family = betar())
```

Inspect the summary to see that the posterior now also contains
estimates for the `Beta` precision parameters $\phi$. Note that we can
also suppress a summary of the $\beta$ coefficients as an option, which
is useful when there are many spline coefficients to report:

``` r
summary(mod, include_betas = FALSE)
#> GAM formula:
#> y ~ s(season, bs = "cc", k = 7) + s(season, by = series, m = 1, 
#>     k = 5)
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
#> Observation precision parameter estimates:
#>        2.5% 50% 97.5% Rhat n.eff
#> phi[1]  5.4 8.3    12    1  1046
#> phi[2]  5.7 8.6    12    1  1610
#> phi[3]  5.7 8.4    12    1  1721
#> 
#> GAM coefficient (beta) estimates:
#>              2.5%  50% 97.5% Rhat n.eff
#> (Intercept) -0.21 0.18  0.42    1   592
#> 
#> Latent trend marginal deviation (alpha) and length scale (rho) estimates:
#>              2.5%  50% 97.5% Rhat n.eff
#> alpha_gp[1] 0.078 0.42  0.93 1.00   581
#> alpha_gp[2] 0.380 0.70  1.30 1.00  1258
#> alpha_gp[3] 0.150 0.46  0.95 1.01   725
#> rho_gp[1]   1.200 3.90 18.00 1.01   249
#> rho_gp[2]   1.800 7.60 36.00 1.00   326
#> rho_gp[3]   1.300 5.00 20.00 1.00   756
#> 
#> Stan MCMC diagnostics:
#> n_eff / iter looks reasonable for all parameters
#> Rhat looks reasonable for all parameters
#> 9 of 2000 iterations ended with a divergence (0.45%)
#> *Try running with larger adapt_delta to remove the divergences
#> 0 of 2000 iterations saturated the maximum tree depth of 12 (0%)
#> E-FMI indicated no pathological behavior
```

Plot the hindcast and forecast distributions for each series

``` r
layout(matrix(1:4, nrow = 2, byrow = TRUE))
for(i in 1:3){
  plot(mod, type = 'forecast', series = i)
}
```

    #> Out of sample CRPS:
    #> [1] 2.113229
    #> Out of sample CRPS:
    #> [1] 1.850393
    #> Out of sample CRPS:
    #> [1] 1.778506

<img src="man/figures/README-beta_fc-1.png" width="60%" style="display: block; margin: auto;" />

## Dynamic coefficient models

Dynamic fixed-effect coefficients (often referred to as dynamic linear
models) can also be readily incorporated into GAMs / DGAMs. In `mvgam`,
the `dynamic()` formula wrapper is used to set these up. The plan is to
incorporate a range of dynamic options (such as random walk, AR1 etc…)
but for the moment only low-rank Gaussian Process smooths are allowed
(making use either of the `gp` basis in `mgcv` of of Hilbert space
approximate GPs). An example below illustrates:

Simulate a time-varying coefficient using a squared exponential Gaussian
Process function with length scale $\rho$=10

``` r
set.seed(1111)
N <- 200
beta_temp <- mvgam:::sim_gp(rnorm(1),
                            alpha_gp = 0.75,
                            rho_gp = 10,
                            h = N) + 0.5
plot(beta_temp, type = 'l', lwd = 3, 
     bty = 'l', xlab = 'Time', ylab = 'Coefficient',
     col = 'darkred')
box(bty = 'l', lwd = 2)
```

<img src="man/figures/README-unnamed-chunk-51-1.png" width="60%" style="display: block; margin: auto;" />

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

<img src="man/figures/README-unnamed-chunk-52-1.png" width="60%" style="display: block; margin: auto;" />

Gather the data into a `data.frame` and fit a model using the
`dynamic()` formula wrapper to specify a low-rank Gaussian Process
smooth function to estimate the time-varying coefficient of
$temperature$. The `dynamic()` function will automatically use a spline
with the `gp` basis in `mgcv` if you supply a value for `rho`. If you do
not supply a value for this argument, it will be estimated using a
Hilbert space approximate GP following the documentation outlined in the
`gp()` function in `brms`. We will mis-specify the $\rho$ parameter here
as, in practice, it is never known

``` r
data <- data.frame(out, temp, time)
data_train <- data[1:190,]
data_test <- data[191:200,]
mod <- mvgam(out ~ dynamic(temp, rho = 8, stationary = TRUE),
             family = gaussian(),
             data = data_train)
```

Inspect the model summary, which shows how the `dynamic` wrapper was
used to construct a low-rank Gaussian Process smooth function:

``` r
summary(mod, include_betas = FALSE)
#> GAM formula:
#> out ~ s(time, by = temp, bs = "gp", m = c(-2, 8, 2), k = 27)
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
#>              2.5%  50% 97.5% Rhat n.eff
#> sigma_obs[1] 0.23 0.25  0.28    1  2428
#> 
#> GAM coefficient (beta) estimates:
#>             2.5% 50% 97.5% Rhat n.eff
#> (Intercept)    4   4   4.1    1  2056
#> 
#> Stan MCMC diagnostics:
#> n_eff / iter looks reasonable for all parameters
#> Rhat looks reasonable for all parameters
#> 0 of 2000 iterations ended with a divergence (0%)
#> 0 of 2000 iterations saturated the maximum tree depth of 12 (0%)
#> E-FMI indicated no pathological behavior
```

Because this model used a spline with a `gp` basis, it’s smooths can be
visualised just like any other `gam`. Plot the estimated time-varying
coefficient for the in-sample training period

``` r
plot(mod, type = 'smooths')
```

<img src="man/figures/README-unnamed-chunk-56-1.png" width="60%" style="display: block; margin: auto;" />

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

<img src="man/figures/README-unnamed-chunk-57-1.png" width="60%" style="display: block; margin: auto;" />

This results in sensible forecasts of the observations as well

``` r
plot(mod, type = 'forecast', newdata = data_test)
```

<img src="man/figures/README-unnamed-chunk-58-1.png" width="60%" style="display: block; margin: auto;" />

    #> Out of sample CRPS:
    #> [1] 1.281124

The syntax is very similar if we wish to estimate the parameters of the
underlying Gaussian Process, this time using a Hilbert space
approximation. We simply omit the `rho` argument in `dynamic`:

``` r
mod <- mvgam(out ~ dynamic(temp, scale = FALSE),
             family = gaussian(),
             data = data_train)
#> Using cmdstanr as the backend
#> 
#> In file included from stan/lib/stan_math/lib/boost_1.78.0/boost/multi_array/multi_array_ref.hpp:32,
#>                  from stan/lib/stan_math/lib/boost_1.78.0/boost/multi_array.hpp:34,
#>                  from stan/lib/stan_math/lib/boost_1.78.0/boost/numeric/odeint/algebra/multi_array_algebra.hpp:22,
#>                  from stan/lib/stan_math/lib/boost_1.78.0/boost/numeric/odeint.hpp:63,
#>                  from stan/lib/stan_math/stan/math/prim/functor/ode_rk45.hpp:9,
#>                  from stan/lib/stan_math/stan/math/prim/functor/integrate_ode_rk45.hpp:6,
#>                  from stan/lib/stan_math/stan/math/prim/functor.hpp:16,
#>                  from stan/lib/stan_math/stan/math/rev/fun.hpp:200,
#>                  from stan/lib/stan_math/stan/math/rev.hpp:10,
#>                  from stan/lib/stan_math/stan/math.hpp:19,
#>                  from stan/src/stan/model/model_header.hpp:4,
#>                  from C:/Users/Nick/AppData/Local/Temp/RtmpIdGf68/model-745478b56c19.hpp:2:
#> stan/lib/stan_math/lib/boost_1.78.0/boost/functional.hpp:180:45: warning: 'template<class _Arg, class _Result> struct std::unary_function' is deprecated [-Wdeprecated-declarations]
#>   180 |         : public boost::functional::detail::unary_function<typename unary_traits<Predicate>::argument_type,bool>
#>       |                                             ^~~~~~~~~~~~~~
#> In file included from C:/RBuildTools/4.2/ucrt64/include/c++/12.2.0/string:48,
#>                  from C:/RBuildTools/4.2/ucrt64/include/c++/12.2.0/bits/locale_classes.h:40,
#>                  from C:/RBuildTools/4.2/ucrt64/include/c++/12.2.0/bits/ios_base.h:41,
#>                  from C:/RBuildTools/4.2/ucrt64/include/c++/12.2.0/ios:42,
#>                  from C:/RBuildTools/4.2/ucrt64/include/c++/12.2.0/istream:38,
#>                  from C:/RBuildTools/4.2/ucrt64/include/c++/12.2.0/sstream:38,
#>                  from C:/RBuildTools/4.2/ucrt64/include/c++/12.2.0/complex:45,
#>                  from stan/lib/stan_math/lib/eigen_3.4.0/Eigen/Core:50,
#>                  from stan/lib/stan_math/lib/eigen_3.4.0/Eigen/Dense:1,
#>                  from stan/lib/stan_math/stan/math/prim/fun/Eigen.hpp:22,
#>                  from stan/lib/stan_math/stan/math/rev.hpp:4:
#> C:/RBuildTools/4.2/ucrt64/include/c++/12.2.0/bits/stl_function.h:117:12: note: declared here
#>   117 |     struct unary_function
#>       |            ^~~~~~~~~~~~~~
#> stan/lib/stan_math/lib/boost_1.78.0/boost/functional.hpp:214:45: warning: 'template<class _Arg1, class _Arg2, class _Result> struct std::binary_function' is deprecated [-Wdeprecated-declarations]
#>   214 |         : public boost::functional::detail::binary_function<
#>       |                                             ^~~~~~~~~~~~~~~
#> C:/RBuildTools/4.2/ucrt64/include/c++/12.2.0/bits/stl_function.h:131:12: note: declared here
#>   131 |     struct binary_function
#>       |            ^~~~~~~~~~~~~~~
#> stan/lib/stan_math/lib/boost_1.78.0/boost/functional.hpp:252:45: warning: 'template<class _Arg, class _Result> struct std::unary_function' is deprecated [-Wdeprecated-declarations]
#>   252 |         : public boost::functional::detail::unary_function<
#>       |                                             ^~~~~~~~~~~~~~
#> C:/RBuildTools/4.2/ucrt64/include/c++/12.2.0/bits/stl_function.h:117:12: note: declared here
#>   117 |     struct unary_function
#>       |            ^~~~~~~~~~~~~~
#> stan/lib/stan_ma
#> th/lib/boost_1.78.0/boost/functional.hpp:299:45: warning: 'template<class _Arg, class _Result> struct std::unary_function' is deprecated [-Wdeprecated-declarations]
#>   299 |         : public boost::functional::detail::unary_function<
#>       |                                             ^~~~~~~~~~~~~~
#> C:/RBuildTools/4.2/ucrt64/include/c++/12.2.0/bits/stl_function.h:117:12: note: declared here
#>   117 |     struct unary_function
#>       |            ^~~~~~~~~~~~~~
#> stan/lib/stan_math/lib/boost_1.78.0/boost/functional.hpp:345:57: warning: 'template<class _Arg, class _Result> struct std::unary_function' is deprecated [-Wdeprecated-declarations]
#>   345 |     class mem_fun_t : public boost::functional::detail::unary_function<T*, S>
#>       |                                                         ^~~~~~~~~~~~~~
#> C:/RBuildTools/4.2/ucrt64/include/c++/12.2.0/bits/stl_function.h:117:12: note: declared here
#>   117 |     struct unary_function
#>       |            ^~~~~~~~~~~~~~
#> stan/lib/stan_math/lib/boost_1.78.0/boost/functional.hpp:361:58: warning: 'template<class _Arg1, class _Arg2, class _Result> struct std::binary_function' is deprecated [-Wdeprecated-declarations]
#>   361 |     class mem_fun1_t : public boost::functional::detail::binary_function<T*, A, S>
#>       |                                                          ^~~~~~~~~~~~~~~
#> C:/RBuildTools/4.2/ucrt64/include/c++/12.2.0/bits/stl_function.h:131:12: note: declared here
#>   131 |     struct binary_function
#>       |            ^~~~~~~~~~~~~~~
#> stan/lib/stan_math/lib/boost_1.78.0/boost/functional.hpp:377:63: warning: 'template<class _Arg, class _Result> struct std::unary_function' is deprecated [-Wdeprecated-declarations]
#>   377 |     class const_mem_fun_t : public boost::functional::detail::unary_function<const T*, S>
#>       |                                                               ^~~~~~~~~~~~~~
#> C:/RBuildTools/4.2/ucrt64/include/c++/12.2.0/bits/stl_function.h:117:12: note: declared here
#>   117 |     struct unary_function
#>       |            ^~~~~~~~~~~~~~
#> stan/lib/stan_math/lib/boost_1.78.0/boost/functional.hpp:393:64: warning: 'template<class _Arg1, class _Arg2, class _Result> struct std::binary_function' is deprecated [-Wdeprecated-declarations]
#>   393 |     class const_mem_fun1_t : public boost::functional::detail::binary_function<const T*, A, S>
#>       |                                                                ^~~~~~~~~~~~~~~
#> C:/RBuildTools/4.2/ucrt64/include/c++/12.2.0/bits/stl_function.h:131:12: note: declared here
#>   131 |     struct binary_function
#>       |            ^~~~~~~~~~~~~~~
#> stan/lib/stan_math/lib/boost_1.78.0/boost/functional.hpp:438:61: warning: 'template<class _Arg, class _Result> struct std::unary_function' is deprecated [-Wdeprecated-declarations]
#>   438 |     class mem_fun_ref_t : public boost::functional::detail::unary_function<T&, S>
#>       |                                                             ^~~~~~~~~~~~~~
#> C:/RBuildTools/4.2/ucrt64/include/c++/12.2.0/bits/stl_function.h:117:12: note: declared here
#>   117 |     struct unary_function
#>       |            ^~~~~~~~~~~~~~
#> stan/lib/stan_math/lib/boost_1.78.0/boost/functional.hpp:454:62: warning: 'template<class _Arg1, class _Arg2, class _Result> struct std::binary_function' is deprecated [-Wdeprecated-declarations]
#>   454 |     class mem_fun1_ref_t : public boost::functional::detail::binary_function<T&, A, S>
#>       |                                                              ^~~~~~~~~~~~~~~
#> C:/RBuildTools/4.2/ucrt64/include/c
#> ++/12.2.0/bits/stl_function.h:131:12: note: declared here
#>   131 |     struct binary_function
#>       |            ^~~~~~~~~~~~~~~
#> stan/lib/stan_math/lib/boost_1.78.0/boost/functional.hpp:470:67: warning: 'template<class _Arg, class _Result> struct std::unary_function' is deprecated [-Wdeprecated-declarations]
#>   470 |     class const_mem_fun_ref_t : public boost::functional::detail::unary_function<const T&, S>
#>       |                                                                   ^~~~~~~~~~~~~~
#> C:/RBuildTools/4.2/ucrt64/include/c++/12.2.0/bits/stl_function.h:117:12: note: declared here
#>   117 |     struct unary_function
#>       |            ^~~~~~~~~~~~~~
#> stan/lib/stan_math/lib/boost_1.78.0/boost/functional.hpp:487:68: warning: 'template<class _Arg1, class _Arg2, class _Result> struct std::binary_function' is deprecated [-Wdeprecated-declarations]
#>   487 |     class const_mem_fun1_ref_t : public boost::functional::detail::binary_function<const T&, A, S>
#>       |                                                                    ^~~~~~~~~~~~~~~
#> C:/RBuildTools/4.2/ucrt64/include/c++/12.2.0/bits/stl_function.h:131:12: note: declared here
#>   131 |     struct binary_function
#>       |            ^~~~~~~~~~~~~~~
#> stan/lib/stan_math/lib/boost_1.78.0/boost/functional.hpp:533:73: warning: 'template<class _Arg, class _Result> struct std::unary_function' is deprecated [-Wdeprecated-declarations]
#>   533 |     class pointer_to_unary_function : public boost::functional::detail::unary_function<Arg,Result>
#>       |                                                                         ^~~~~~~~~~~~~~
#> C:/RBuildTools/4.2/ucrt64/include/c++/12.2.0/bits/stl_function.h:117:12: note: declared here
#>   117 |     struct unary_function
#>       |            ^~~~~~~~~~~~~~
#> stan/lib/stan_math/lib/boost_1.78.0/boost/functional.hpp:557:74: warning: 'template<class _Arg1, class _Arg2, class _Result> struct std::binary_function' is deprecated [-Wdeprecated-declarations]
#>   557 |     class pointer_to_binary_functi
#> on : public boost::functional::detail::binary_function<Arg1,Arg2,Result>
#>       |                                                                          ^~~~~~~~~~~~~~~
#> C:/RBuildTools/4.2/ucrt64/include/c++/12.2.0/bits/stl_function.h:131:12: note: declared here
#>   131 |     struct binary_function
#>       |            ^~~~~~~~~~~~~~~
#> Init values were only set for a subset of parameters. 
#> Missing init values for the following parameters:
#>  - chain 1: alpha_gp_time_bytemp, rho_gp_time_bytemp, z_gp_time_bytemp, sigma_obs, lambda
#>  - chain 2: alpha_gp_time_bytemp, rho_gp_time_bytemp, z_gp_time_bytemp, sigma_obs, lambda
#>  - chain 3: alpha_gp_time_bytemp, rho_gp_time_bytemp, z_gp_time_bytemp, sigma_obs, lambda
#>  - chain 4: alpha_gp_time_bytemp, rho_gp_time_bytemp, z_gp_time_bytemp, sigma_obs, lambda
#> Running MCMC with 4 parallel chains...
#> 
#> Chain 1 Iteration:   1 / 1000 [  0%]  (Warmup) 
#> Chain 1 Iteration: 100 / 1000 [ 10%]  (Warmup)
#> Chain 1 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 1 Exception: normal_id_glm_lpdf: Scale vector[1] is inf, but must be positive finite! (in 'C:/Users/Nick/AppData/Local/Temp/RtmpIdGf68/model-745478b56c19.stan', line 99, column 4 to column 61)
#> Chain 1 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 1 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 1
#> Chain 1 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 1 Exception: normal_id_glm_lpdf: Scale vector[1] is inf, but must be positive finite! (in 'C:/Users/Nick/AppData/Local/Temp/RtmpIdGf68/model-745478b56c19.stan', line 99, column 4 to column 61)
#> Chain 1 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 1 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 1
#> Chain 1 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 1 Exception: normal_id_glm_lpdf: Scale vector[1] is inf, but must be positive finite! (in 'C:/Users/Nick/AppData/Local/Temp/RtmpIdGf68/model-745478b56c19.stan', line 99, column 4 to column 61)
#> Chain 1 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 1 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 1
#> Chain 1 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 1 Exception: normal_id_glm_lpdf: Scale vector[1] is inf, but must be positive finite! (in 'C:/Users/Nick/AppData/Local/Temp/RtmpIdGf68/model-745478b56c19.stan', line 99, column 4 to column 61)
#> Chain 1 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 1 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 1
#> Chain 1 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 1 Exception: normal_id_glm_lpdf: Scale vector[1] is inf, but must be positive finite! (in 'C:/Users/Nick/AppData/Local/Temp/RtmpIdGf68/model-745478b56c19.stan', line 99, column 4 to column 61)
#> Chain 1 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 1 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 1
#> Chain 2 Iteration:   1 / 1000 [  0%]  (Warmup)
#> Chain 2 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 2 Exception: normal_id_glm_lpdf: Scale vector[1] is inf, but must be positive finite! (in 'C:/Users/Nick/AppData/Local/Temp/RtmpIdGf68/model-745478b56c19.stan', line 99, column 4 to column 61)
#> Chain 2 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 2 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 2
#> Chain 2 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 2 Exception: normal_id_glm_lpdf: Scale vector[1] is inf, but must be positive finite! (in 'C:/Users/Nick/AppData/Local/Temp/RtmpIdGf68/model-745478b56c19.stan', line 99, column 4 to column 61)
#> Chain 2 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 2 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 2
#> Chain 2 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 2 Exception: normal_id_glm_lpdf: Scale vector[1] is inf, but must be positive finite! (in 'C:/Users/Nick/AppData/Local/Temp/RtmpIdGf68/model-745478b56c19.stan', line 99, column 4 to column 61)
#> Chain 2 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 2 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 2
#> Chain 3 Iteration:   1 / 1000 [  0%]  (Warmup)
#> Chain 3 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 3 Exception: normal_id_glm_lpdf: Scale vector[1] is inf, but must be positive finite! (in 'C:/Users/Nick/AppData/Local/Temp/RtmpIdGf68/model-745478b56c19.stan', line 99, column 4 to column 61)
#> Chain 3 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 3 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 3
#> Chain 3 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 3 Exception: normal_id_glm_lpdf: Scale vector[1] is inf, but must be positive finite! (in 'C:/Users/Nick/AppData/Local/Temp/RtmpIdGf68/model-745478b56c19.stan', line 99, column 4 to column 61)
#> Chain 3 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 3 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 3
#> Chain 3 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 3 Exception: normal_id_glm_lpdf: Scale vector[1] is inf, but must be positive finite! (in 'C:/Users/Nick/AppData/Local/Temp/RtmpIdGf68/model-745478b56c19.stan', line 99, column 4 to column 61)
#> Chain 3 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 3 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 3
#> Chain 3 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 3 Exception: normal_id_glm_lpdf: Scale vector[1] is inf, but must be positive finite! (in 'C:/Users/Nick/AppData/Local/Temp/RtmpIdGf68/model-745478b56c19.stan', line 99, column 4 to column 61)
#> Chain 3 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 3 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 3
#> Chain 3 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 3 Exception: normal_id_glm_lpdf: Scale vector[1] is inf, but must be positive finite! (in 'C:/Users/Nick/AppData/Local/Temp/RtmpIdGf68/model-745478b56c19.stan', line 99, column 4 to column 61)
#> Chain 3 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 3 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 3
#> Chain 3 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 3 Exception: normal_id_glm_lpdf: Scale vector[1] is inf, but must be positive finite! (in 'C:/Users/Nick/AppData/Local/Temp/RtmpIdGf68/model-745478b56c19.stan', line 99, column 4 to column 61)
#> Chain 3 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 3 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 3
#> Chain 4 Iteration:   1 / 1000 [  0%]  (Warmup)
#> Chain 4 Informational Message: The current Metropolis proposal is about to be rejected because of the following issue:
#> Chain 4 Exception: normal_id_glm_lpdf: Scale vector[1] is inf, but must be positive finite! (in 'C:/Users/Nick/AppData/Local/Temp/RtmpIdGf68/model-745478b56c19.stan', line 99, column 4 to column 61)
#> Chain 4 If this warning occurs sporadically, such as for highly constrained variable types like covariance matrices, then the sampler is fine,
#> Chain 4 but if this warning occurs often then your model may be either severely ill-conditioned or misspecified.
#> Chain 4
#> Chain 2 Iteration: 100 / 1000 [ 10%]  (Warmup) 
#> Chain 1 Iteration: 200 / 1000 [ 20%]  (Warmup) 
#> Chain 3 Iteration: 100 / 1000 [ 10%]  (Warmup) 
#> Chain 2 Iteration: 200 / 1000 [ 20%]  (Warmup) 
#> Chain 1 Iteration: 300 / 1000 [ 30%]  (Warmup) 
#> Chain 4 Iteration: 100 / 1000 [ 10%]  (Warmup) 
#> Chain 3 Iteration: 200 / 1000 [ 20%]  (Warmup) 
#> Chain 1 Iteration: 400 / 1000 [ 40%]  (Warmup) 
#> Chain 2 Iteration: 300 / 1000 [ 30%]  (Warmup) 
#> Chain 3 Iteration: 300 / 1000 [ 30%]  (Warmup) 
#> Chain 4 Iteration: 200 / 1000 [ 20%]  (Warmup) 
#> Chain 2 Iteration: 400 / 1000 [ 40%]  (Warmup) 
#> Chain 1 Iteration: 500 / 1000 [ 50%]  (Warmup) 
#> Chain 3 Iteration: 400 / 1000 [ 40%]  (Warmup) 
#> Chain 1 Iteration: 501 / 1000 [ 50%]  (Sampling) 
#> Chain 2 Iteration: 500 / 1000 [ 50%]  (Warmup) 
#> Chain 4 Iteration: 300 / 1000 [ 30%]  (Warmup) 
#> Chain 2 Iteration: 501 / 1000 [ 50%]  (Sampling) 
#> Chain 1 Iteration: 600 / 1000 [ 60%]  (Sampling) 
#> Chain 3 Iteration: 500 / 1000 [ 50%]  (Warmup) 
#> Chain 4 Iteration: 400 / 1000 [ 40%]  (Warmup) 
#> Chain 3 Iteration: 501 / 1000 [ 50%]  (Sampling) 
#> Chain 2 Iteration: 600 / 1000 [ 60%]  (Sampling) 
#> Chain 3 Iteration: 600 / 1000 [ 60%]  (Sampling) 
#> Chain 4 Iteration: 500 / 1000 [ 50%]  (Warmup) 
#> Chain 1 Iteration: 700 / 1000 [ 70%]  (Sampling) 
#> Chain 4 Iteration: 501 / 1000 [ 50%]  (Sampling) 
#> Chain 3 Iteration: 700 / 1000 [ 70%]  (Sampling) 
#> Chain 2 Iteration: 700 / 1000 [ 70%]  (Sampling) 
#> Chain 1 Iteration: 800 / 1000 [ 80%]  (Sampling) 
#> Chain 4 Iteration: 600 / 1000 [ 60%]  (Sampling) 
#> Chain 3 Iteration: 800 / 1000 [ 80%]  (Sampling) 
#> Chain 1 Iteration: 900 / 1000 [ 90%]  (Sampling) 
#> Chain 2 Iteration: 800 / 1000 [ 80%]  (Sampling) 
#> Chain 3 Iteration: 900 / 1000 [ 90%]  (Sampling) 
#> Chain 4 Iteration: 700 / 1000 [ 70%]  (Sampling) 
#> Chain 3 Iteration: 1000 / 1000 [100%]  (Sampling) 
#> Chain 3 finished in 3.0 seconds.
#> Chain 1 Iteration: 1000 / 1000 [100%]  (Sampling) 
#> Chain 2 Iteration: 900 / 1000 [ 90%]  (Sampling) 
#> Chain 1 finished in 3.5 seconds.
#> Chain 4 Iteration: 800 / 1000 [ 80%]  (Sampling) 
#> Chain 2 Iteration: 1000 / 1000 [100%]  (Sampling) 
#> Chain 2 finished in 3.7 seconds.
#> Chain 4 Iteration: 900 / 1000 [ 90%]  (Sampling) 
#> Chain 4 Iteration: 1000 / 1000 [100%]  (Sampling) 
#> Chain 4 finished in 3.9 seconds.
#> 
#> All 4 chains finished successfully.
#> Mean chain execution time: 3.5 seconds.
#> Total execution time: 4.7 seconds.
```

This model summary now contains estimates for the marginal deviation and
length scale parameters of the underlying Gaussian Process function:

``` r
summary(mod, include_betas = FALSE)
#> GAM formula:
#> out ~ gp(time, by = temp, c = 5/4, k = 40, scale = FALSE)
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
#>              2.5%  50% 97.5% Rhat n.eff
#> sigma_obs[1] 0.24 0.26   0.3    1  2848
#> 
#> GAM coefficient (beta) estimates:
#>             2.5% 50% 97.5% Rhat n.eff
#> (Intercept)    4   4   4.1    1  3367
#> 
#> GAM gp term marginal deviation (alpha) and length scale (rho) estimates:
#>                     2.5%   50% 97.5% Rhat n.eff
#> alpha_gp(time):temp 0.62  0.87   1.4    1   762
#> rho_gp(time):temp   5.30 10.00  13.0    1   769
#> 
#> Stan MCMC diagnostics:
#> n_eff / iter looks reasonable for all parameters
#> Rhat looks reasonable for all parameters
#> 0 of 2000 iterations ended with a divergence (0%)
#> 0 of 2000 iterations saturated the maximum tree depth of 12 (0%)
#> E-FMI indicated no pathological behavior
```

We can use `plot_predictions` from the `marginaleffects` package to
visualise the time-varying coefficient for the effect of `temp`, which
is similar to what we estimated above:

``` r
plot_predictions(mod, 
                 newdata = datagrid(time = unique,
                                    temp = 1),
                 by = 'time',
                 type = 'link')
```

<img src="man/figures/README-unnamed-chunk-61-1.png" width="60%" style="display: block; margin: auto;" />

Forecasts are also similar:

``` r
plot(mod, type = 'forecast', newdata = data_test)
```

<img src="man/figures/README-unnamed-chunk-62-1.png" width="60%" style="display: block; margin: auto;" />

    #> Out of sample CRPS:
    #> [1] 1.633331

There are many more extended uses for `mvgam` models, including the
ability to fit dynamic factor processes for analysing and forecasting
sets of multivariate time series. See the package documentation for more
details.

## License

This project is licensed under an `MIT` open source license
