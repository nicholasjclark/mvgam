# Hierarchical Vector Autoregression

**Author:** Jim Savage
**Date:** 27 November 2016
**Source:** https://rpubs.com/jimsavage/hierarchical_var

Reference post for `tests/local/savage_hierarchical_var.R` and the in-progress
hierarchical-VAR vignette draft. The Savage formulation drives our `VAR(p=1,
gr=country, subgr=outcome, cor=TRUE)` validation target.

---

A few days ago, Nathaniel Bechhofer wrote to me on Twitter asking if I knew of
a good way to use repeated observations (hierarchical) data to estimate a
covariance matrix. I've done a lot of work with Bayesian Vector
Autoregressions (there are about three almost-finished posts on these coming
up), but I had never played around with VARs estimated by partial pooling.
This post illustrates one approach to the technique.

For the uninitiated, a vector autoregression relates current values of a
vector of (unconstrained) outcomes in period `t`, `Y_t`, to its own lags. We
typically assume that the residuals of a VAR process (often called
"innovations") are multivariate normal, with a big part of the literature
concerned with estimating the covariance matrix of these innovations under
various restrictions.

Explicitly, given a vector of intercepts `A` and a matrix of coefficients
`B`, a VAR(1) process is

    Y_t = A + B Y_{t-1} + eps_t

with `eps_t ~ multi_normal(0, Sigma)`. For interpretability we normally
decompose Sigma as `Sigma = diag(tau) Omega diag(tau)`, where `tau` is a
vector of scales (marginal standard deviations of the innovations) and
`Omega` is their correlation matrix. A VAR(p) process generalises this to
`p` lags.

## From VAR to Hierarchical VAR

Now what if we have outcomes for many units of observation? For instance, we
might observe a vector of outcomes for many countries over many years (the
example below). How do we approach this?

One method is to "pool" the data and estimate the model above. That assumes
every country shares the same `A`, `B`, `tau` and `Omega` — a strong
assumption. Another approach estimates the model separately for each
country (an "unpooled" estimate); this is the typical approach and is
equivalent to saying there is nothing we can learn from one country when
estimating parameters for another.

The method I favour is "partial pooling", which balances those two. Parameter
estimates are shrunk towards a common average, with the degree of shrinkage
proportional to the noisiness of the unpooled estimate. The intuition is
that information from the general behaviour across countries helps us get
better estimates for an individual country.

## An example

A simple hierarchical VAR(1). Each country `c` is observed at time `t`,
with country-specific intercepts `A_c`, slopes `B_c`, innovation scales
`tau_c`, and innovation correlations `Omega_c`. The data are generated as

    Y_{c,t} = A_c + B_c Y_{c,t-1} + eps_{c,t}

with

    eps_{c,t} ~ multi_normal(0, diag(tau_c) Omega_c diag(tau_c))

Each of the country-level parameters gets a hierarchical prior. Our
hierarchical priors are

    A_c    ~ normal(A_hat, sigma_A)
    vec(B_c) ~ normal(B_hat, sigma_B)
    tau_c  ~ normal_plus(tau_hat, sigma_tau)

It is a little difficult to give the correlation matrices `Omega_c` a
hierarchical prior because their values are constrained by other values. One
approach — at Ben Goodrich's suggestion — is to use a weighting parameter
`rho` in `(0, 1)` and shrink local correlation estimates towards a global
correlation matrix using

    Omega_c = rho * Omega_global + (1 - rho) * Omega_{c, local}

with

    Omega_global    ~ LKJ(1)
    Omega_{c,local} ~ LKJ(10)
    rho             ~ Beta(2, 2)

A big advantage of this approach is that we end up with an estimate for `rho`
as well as the correlation matrices. That tells us how important the partial
pooling is to the estimates of the `Omega_c`s.

## A worked example

The example pulls data from the World Bank's World Development Indicators
(via the WDI R API). We pull annual GDP, consumption, and gross fixed capital
formation (investment) for all countries from 1970, in constant-price local
currency units, convert to annual (continuous-compounding) growth rates, and
fit the hierarchical VAR above.

```r
# We'll use WDI data from the World Bank, dplyr and rstan
library(WDI); library(dplyr)
library(rstan)
options(mc.cores = parallel::detectCores())

# Grab gdp, consumption and investment (lcu constant prices)
gdp_cons_inv <- WDI(indicator = c("NY.GDP.MKTP.KN","NE.CON.TOTL.KN", "NE.GDI.FTOT.KN"),
                    start = 1970)
```

Convert to differenced logs, drop missing rows, and keep series with > 10
years of data. For speed restrict to English-speaking countries plus Chile;
the full 138-country fit took about an hour on a four-core laptop.

```r
gdp_cons_inv_1 <- gdp_cons_inv %>%
  filter(complete.cases(.)) %>%
  rename(GDP = NY.GDP.MKTP.KN,
         CONS = NE.CON.TOTL.KN,
         GFCF = NE.GDI.FTOT.KN) %>%
  group_by(country) %>%
  arrange(year) %>%
  mutate(dl_gdp  = c(NA, diff(log(GDP))),
         dl_cons = c(NA, diff(log(CONS))),
         dl_gfcf = c(NA, diff(log(GFCF))),
         more_than_10 = sum(!is.na(dl_gfcf)) > 10) %>%
  arrange(country, year) %>%
  ungroup() %>%
  filter(more_than_10 & is.finite(dl_gfcf))

gdp_cons_inv_1 <- gdp_cons_inv_1 %>%
  ungroup() %>%
  filter(complete.cases(.)) %>%
  group_by(country) %>%
  mutate(time = 1:n())

gdp_cons_inv_2 <- gdp_cons_inv_1 %>%
  ungroup() %>%
  filter(country %in% c("United States", "United Kingdom", "Australia",
                        "New Zealand", "Chile", "Canada", "Ireland",
                        "South Africa"))
```

## Stan model

Saved as `hierarchical_var.stan`. Non-centered parameterisation for `A_c`
and `B_c`.

```stan
data {
  int N;                              // rows in the panel
  int K;                              // outcome dimension
  int I;                              // number of individuals
  int T;                              // max time periods for any individual
  int<lower = 1, upper = I> individual[N];
  int<lower = 1, upper = T> time[N];
  matrix[N, K] Y;
}
parameters {
  corr_matrix[K] Omega_local[I];
  vector<lower = 0>[K] tau[I];
  matrix[K, K] z_beta[I];
  vector[K] z_alpha[I];

  real<lower = 0, upper = 1> rho;
  corr_matrix[K] Omega_global;
  vector[K] tau_location;
  vector<lower = 0>[K] tau_scale;
  matrix[K, K] beta_hat_location;
  matrix<lower = 0>[K, K] beta_hat_scale;
  vector[K] alpha_hat_location;
  vector<lower = 0>[K] alpha_hat_scale;
}
transformed parameters {
  matrix[K, K] beta[I];
  vector[K] alpha[I];
  corr_matrix[K] Omega[I];

  for (i in 1:I) {
    alpha[i] = alpha_hat_location + alpha_hat_scale .* z_alpha[i];
    beta[i]  = beta_hat_location  + beta_hat_scale  .* z_beta[i];
    Omega[i] = rho * Omega_global + (1 - rho) * Omega_local[i];
  }
}
model {
  // hyperpriors
  rho                    ~ beta(2, 2);
  tau_location           ~ cauchy(0, 1);
  tau_scale              ~ cauchy(0, 1);
  alpha_hat_location     ~ normal(0, 1);
  alpha_hat_scale        ~ cauchy(0, 1);
  to_vector(beta_hat_location) ~ normal(0, .5);
  to_vector(beta_hat_scale)    ~ cauchy(0, .5);
  Omega_global           ~ lkj_corr(1);

  for (i in 1:I) {
    z_alpha[i]            ~ normal(0, 1);
    to_vector(z_beta[i])  ~ normal(0, 1);
    tau[i]                ~ normal(tau_location, tau_scale);
    Omega_local[i]        ~ lkj_corr(10);
  }

  for (n in 1:N) {
    if (time[n] > 1) {
      Y[n] ~ multi_normal(alpha[individual[n]] + beta[individual[n]] * Y[n - 1]',
                          quad_form_diag(Omega[individual[n]], tau[individual[n]]));
    }
  }
}
```

## Results

Global correlation matrix (expected value for a new country):

    [1,] 1.000 0.890 0.925
    [2,] 0.890 1.000 0.738
    [3,] 0.925 0.738 1.000

Australia (index 1) country-specific correlation matrix:

    [1,] 1.000 0.690 0.781
    [2,] 0.690 1.000 0.587
    [3,] 0.781 0.587 1.000

Relevance of the global correlation matrix:

         mean se_mean   sd 2.5%  25%  50%  75% 97.5% n_eff Rhat
    rho  0.82       0 0.04 0.75 0.79 0.82 0.84   0.9   610 1.01

A value of 0.82 means there is a lot of borrowed power from the hierarchy.
At 1, all country correlation matrices would equal the global one; at 0,
they would share nothing.

## When to use a hierarchical VAR

The value of the hierarchical approach is fitting a richly parameterised
model with relatively few data points. The example uses 313 observations of
3 variables to estimate 171 parameters. That is only doable with good priors,
and we don't want to be too subjective about them; the hierarchical prior
approach lets us "learn the priors" by sharing information across units.

---

**mvgam mapping (added 2026-06-24):**

| Savage symbol      | mvgam equivalent                                        |
|--------------------|---------------------------------------------------------|
| individual `c`     | `gr = country`                                          |
| outcome dim `K`    | `subgr = outcome`                                       |
| `beta_hat_location`| `Amu_trend[1]` (diagonal) + `Amu_trend[2]` (off-diag)   |
| `beta_hat_scale`   | `1 / sqrt(Aomega_trend[1 or 2])`                        |
| `tau_c`            | `sigma_group_trend[country, outcome]`                   |
| `Omega_global`     | `L_Omega_global_trend` (Cholesky-stored)                |
| `Omega_{c,local}`  | per-country `L_Omega_group_trend[c]`                    |
| `rho`              | `alpha_cor_trend` (Beta-distributed weight)             |

The Heaps (2024) stationarity transform on the per-country `Sigma` block is
the dominant per-gradient cost; that's why standardising each outcome to
unit variance and tightening the diffuse `Amu_trend` / `L_Omega_global_trend`
priors is necessary for tractable sampling.
