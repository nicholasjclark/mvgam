library(brms)

#### Multivariate observation example ####
data("BTdata", package = "MCMCglmm")
head(BTdata)
# tarsus       back  animal     dam fosternest  hatchdate  sex
# 1 -1.89229718  1.1464212 R187142 R187557      F2102 -0.6874021  Fem
# 2  1.13610981 -0.7596521 R187154 R187559      F1902 -0.6874021 Male
# 3  0.98468946  0.1449373 R187341 R187568       A602 -0.4279814 Male
# 4  0.37900806  0.2555847 R046169 R187518      A1302 -1.4656641 Male
# 5 -0.07525299 -0.3006992 R046161 R187528      A2602 -1.4656641  Fem
# 6 -1.13519543  1.5577219 R187409 R187945      C2302  0.3502805  Fem

# Multivariate observation models generate stancode like this,
# with custom-named mu parameters
make_stancode(
  bf(
    mvbind(tarsus, back) ~ sex + hatchdate + (1|p|fosternest) + (1|q|dam)
  ) +
    set_rescor(TRUE),
  data = BTdata
)
"// generated with brms 2.22.9
functions {
 /* compute correlated group-level effects
  * Args:
  *   z: matrix of unscaled group-level effects
  *   SD: vector of standard deviation parameters
  *   L: cholesky factor correlation matrix
  * Returns:
  *   matrix of scaled group-level effects
  */
  matrix scale_r_cor(matrix z, vector SD, matrix L) {
    // r is stored in another dimension order than z
    return transpose(diag_pre_multiply(SD, L) * z);
  }

}
data {
  int<lower=1> N;  // total number of observations
  int<lower=1> N_tarsus;  // number of observations
  vector[N_tarsus] Y_tarsus;  // response variable
  int<lower=1> K_tarsus;  // number of population-level effects
  matrix[N_tarsus, K_tarsus] X_tarsus;  // population-level design matrix
  int<lower=1> Kc_tarsus;  // number of population-level effects after centering
  int<lower=1> N_back;  // number of observations
  vector[N_back] Y_back;  // response variable
  int<lower=1> K_back;  // number of population-level effects
  matrix[N_back, K_back] X_back;  // population-level design matrix
  int<lower=1> Kc_back;  // number of population-level effects after centering
  int<lower=1> nresp;  // number of responses
  int nrescor;  // number of residual correlations
  // data for group-level effects of ID 1
  int<lower=1> N_1;  // number of grouping levels
  int<lower=1> M_1;  // number of coefficients per level
  array[N_tarsus] int<lower=1> J_1_tarsus;  // grouping indicator per observation
  array[N_back] int<lower=1> J_1_back;  // grouping indicator per observation
  // group-level predictor values
  vector[N_tarsus] Z_1_tarsus_1;
  vector[N_back] Z_1_back_2;
  int<lower=1> NC_1;  // number of group-level correlations
  // data for group-level effects of ID 2
  int<lower=1> N_2;  // number of grouping levels
  int<lower=1> M_2;  // number of coefficients per level
  array[N_tarsus] int<lower=1> J_2_tarsus;  // grouping indicator per observation
  array[N_back] int<lower=1> J_2_back;  // grouping indicator per observation
  // group-level predictor values
  vector[N_tarsus] Z_2_tarsus_1;
  vector[N_back] Z_2_back_2;
  int<lower=1> NC_2;  // number of group-level correlations
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
  matrix[N_tarsus, Kc_tarsus] Xc_tarsus;  // centered version of X_tarsus without an intercept
  vector[Kc_tarsus] means_X_tarsus;  // column means of X_tarsus before centering
  matrix[N_back, Kc_back] Xc_back;  // centered version of X_back without an intercept
  vector[Kc_back] means_X_back;  // column means of X_back before centering
  array[N] vector[nresp] Y;  // response array
  for (i in 2:K_tarsus) {
    means_X_tarsus[i - 1] = mean(X_tarsus[, i]);
    Xc_tarsus[, i - 1] = X_tarsus[, i] - means_X_tarsus[i - 1];
  }
  for (i in 2:K_back) {
    means_X_back[i - 1] = mean(X_back[, i]);
    Xc_back[, i - 1] = X_back[, i] - means_X_back[i - 1];
  }
  for (n in 1:N) {
    Y[n] = transpose([Y_tarsus[n], Y_back[n]]);
  }
}
parameters {
  vector[Kc_tarsus] b_tarsus;  // regression coefficients
  real Intercept_tarsus;  // temporary intercept for centered predictors
  real<lower=0> sigma_tarsus;  // dispersion parameter
  vector[Kc_back] b_back;  // regression coefficients
  real Intercept_back;  // temporary intercept for centered predictors
  real<lower=0> sigma_back;  // dispersion parameter
  cholesky_factor_corr[nresp] Lrescor;  // parameters for multivariate linear models
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations
  matrix[M_1, N_1] z_1;  // standardized group-level effects
  cholesky_factor_corr[M_1] L_1;  // cholesky factor of correlation matrix
  vector<lower=0>[M_2] sd_2;  // group-level standard deviations
  matrix[M_2, N_2] z_2;  // standardized group-level effects
  cholesky_factor_corr[M_2] L_2;  // cholesky factor of correlation matrix
}
transformed parameters {
  matrix[N_1, M_1] r_1;  // actual group-level effects
  // using vectors speeds up indexing in loops
  vector[N_1] r_1_tarsus_1;
  vector[N_1] r_1_back_2;
  matrix[N_2, M_2] r_2;  // actual group-level effects
  // using vectors speeds up indexing in loops
  vector[N_2] r_2_tarsus_1;
  vector[N_2] r_2_back_2;
  real lprior = 0;  // prior contributions to the log posterior
  // compute actual group-level effects
  r_1 = scale_r_cor(z_1, sd_1, L_1);
  r_1_tarsus_1 = r_1[, 1];
  r_1_back_2 = r_1[, 2];
  // compute actual group-level effects
  r_2 = scale_r_cor(z_2, sd_2, L_2);
  r_2_tarsus_1 = r_2[, 1];
  r_2_back_2 = r_2[, 2];
  lprior += student_t_lpdf(Intercept_tarsus | 3, 0.1, 2.5);
  lprior += student_t_lpdf(sigma_tarsus | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += student_t_lpdf(Intercept_back | 3, -0.1, 2.5);
  lprior += student_t_lpdf(sigma_back | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += lkj_corr_cholesky_lpdf(Lrescor | 1);
  lprior += student_t_lpdf(sd_1 | 3, 0, 2.5)
    - 2 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += lkj_corr_cholesky_lpdf(L_1 | 1);
  lprior += student_t_lpdf(sd_2 | 3, 0, 2.5)
    - 2 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += lkj_corr_cholesky_lpdf(L_2 | 1);
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N_tarsus] mu_tarsus = rep_vector(0.0, N_tarsus);
    // initialize linear predictor term
    vector[N_back] mu_back = rep_vector(0.0, N_back);
    // multivariate predictor array
    array[N] vector[nresp] Mu;
    vector[nresp] sigma = transpose([sigma_tarsus, sigma_back]);
    // cholesky factor of residual covariance matrix
    matrix[nresp, nresp] LSigma = diag_pre_multiply(sigma, Lrescor);
    mu_tarsus += Intercept_tarsus + Xc_tarsus * b_tarsus;
    mu_back += Intercept_back + Xc_back * b_back;
    for (n in 1:N_tarsus) {
      // add more terms to the linear predictor
      mu_tarsus[n] += r_1_tarsus_1[J_1_tarsus[n]] * Z_1_tarsus_1[n] + r_2_tarsus_1[J_2_tarsus[n]] * Z_2_tarsus_1[n];
    }
    for (n in 1:N_back) {
      // add more terms to the linear predictor
      mu_back[n] += r_1_back_2[J_1_back[n]] * Z_1_back_2[n] + r_2_back_2[J_2_back[n]] * Z_2_back_2[n];
    }
    // combine univariate parameters
    for (n in 1:N) {
      Mu[n] = transpose([mu_tarsus[n], mu_back[n]]);
    }
    target += multi_normal_cholesky_lpdf(Y | Mu, LSigma);
  }
  // priors including constants
  target += lprior;
  target += std_normal_lpdf(to_vector(z_1));
  target += std_normal_lpdf(to_vector(z_2));
}
generated quantities {
  // actual population-level intercept
  real b_tarsus_Intercept = Intercept_tarsus - dot_product(means_X_tarsus, b_tarsus);
  // actual population-level intercept
  real b_back_Intercept = Intercept_back - dot_product(means_X_back, b_back);
  // residual correlations
  corr_matrix[nresp] Rescor = multiply_lower_tri_self_transpose(Lrescor);
  vector<lower=-1,upper=1>[nrescor] rescor;
  // compute group-level correlations
  corr_matrix[M_1] Cor_1 = multiply_lower_tri_self_transpose(L_1);
  vector<lower=-1,upper=1>[NC_1] cor_1;
  // compute group-level correlations
  corr_matrix[M_2] Cor_2 = multiply_lower_tri_self_transpose(L_2);
  vector<lower=-1,upper=1>[NC_2] cor_2;
  // extract upper diagonal of correlation matrix
  for (k in 1:nresp) {
    for (j in 1:(k - 1)) {
      rescor[choose(k - 1, 2) + j] = Rescor[j, k];
    }
  }
  // extract upper diagonal of correlation matrix
  for (k in 1:M_1) {
    for (j in 1:(k - 1)) {
      cor_1[choose(k - 1, 2) + j] = Cor_1[j, k];
    }
  }
  // extract upper diagonal of correlation matrix
  for (k in 1:M_2) {
    for (j in 1:(k - 1)) {
      cor_2[choose(k - 1, 2) + j] = Cor_2[j, k];
    }
  }
}"

#### Distributional observation model examples ####
group <- rep(c("treat", "placebo"), each = 30)
symptom_post <- c(rnorm(30, mean = 1, sd = 2), rnorm(30, mean = 0, sd = 1))
dat1 <- data.frame(group, symptom_post)
head(dat1)
# group symptom_post
# 1 treat    1.0955231
# 2 treat   -0.3114784
# 3 treat    3.5437112
# 4 treat    0.4413125
# 5 treat    3.5880087
# 6 treat    0.2369789

# mu still exists in this model
make_stancode(
  bf(symptom_post ~ group, sigma ~ group),
  data = dat1,
  family = gaussian()
)

"// generated with brms 2.22.9
functions {
}
data {
  int<lower=1> N;  // total number of observations
  vector[N] Y;  // response variable
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  int<lower=1> Kc;  // number of population-level effects after centering
  int<lower=1> K_sigma;  // number of population-level effects
  matrix[N, K_sigma] X_sigma;  // population-level design matrix
  int<lower=1> Kc_sigma;  // number of population-level effects after centering
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
  matrix[N, Kc] Xc;  // centered version of X without an intercept
  vector[Kc] means_X;  // column means of X before centering
  matrix[N, Kc_sigma] Xc_sigma;  // centered version of X_sigma without an intercept
  vector[Kc_sigma] means_X_sigma;  // column means of X_sigma before centering
  for (i in 2:K) {
    means_X[i - 1] = mean(X[, i]);
    Xc[, i - 1] = X[, i] - means_X[i - 1];
  }
  for (i in 2:K_sigma) {
    means_X_sigma[i - 1] = mean(X_sigma[, i]);
    Xc_sigma[, i - 1] = X_sigma[, i] - means_X_sigma[i - 1];
  }
}
parameters {
  vector[Kc] b;  // regression coefficients
  real Intercept;  // temporary intercept for centered predictors
  vector[Kc_sigma] b_sigma;  // regression coefficients
  real Intercept_sigma;  // temporary intercept for centered predictors
}
transformed parameters {
  real lprior = 0;  // prior contributions to the log posterior
  lprior += student_t_lpdf(Intercept | 3, 0.2, 2.5);
  lprior += student_t_lpdf(Intercept_sigma | 3, 0, 2.5);
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu = rep_vector(0.0, N);
    // initialize linear predictor term
    vector[N] sigma = rep_vector(0.0, N);
    mu += Intercept + Xc * b;
    sigma += Intercept_sigma + Xc_sigma * b_sigma;
    sigma = exp(sigma);
    target += normal_lpdf(Y | mu, sigma);
  }
  // priors including constants
  target += lprior;
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept - dot_product(means_X, b);
  // actual population-level intercept
  real b_sigma_Intercept = Intercept_sigma - dot_product(means_X_sigma, b_sigma);
}"

zinb <- read.csv("https://paul-buerkner.github.io/data/fish.csv")
head(zinb)
# nofish livebait camper persons child         xb         zg count
# 1      1        0      0       1     0 -0.8963146  3.0504048     0
# 2      0        1      1       1     0 -0.5583450  1.7461489     0
# 3      0        1      0       1     0 -0.4017310  0.2799389     0
# 4      0        1      1       2     1 -0.9562981 -0.6015257     0
# 5      0        1      0       1     0  0.4368910  0.5277091     1
# 6      0        1      1       4     2  1.3944855 -0.7075348     0

make_stancode(
  count ~ persons + child + camper,
  data = zinb,
  family = zero_inflated_poisson()
)

# mu also exists in this example
"// generated with brms 2.22.9
functions {
  /* zero-inflated poisson log-PDF of a single response
   * Args:
   *   y: the response value
   *   lambda: mean parameter of the poisson distribution
   *   zi: zero-inflation probability
   * Returns:
   *   a scalar to be added to the log posterior
   */
  real zero_inflated_poisson_lpmf(int y, real lambda, real zi) {
    if (y == 0) {
      return log_sum_exp(bernoulli_lpmf(1 | zi),
                         bernoulli_lpmf(0 | zi) +
                         poisson_lpmf(0 | lambda));
    } else {
      return bernoulli_lpmf(0 | zi) +
             poisson_lpmf(y | lambda);
    }
  }
  /* zero-inflated poisson log-PDF of a single response
   * logit parameterization of the zero-inflation part
   * Args:
   *   y: the response value
   *   lambda: mean parameter of the poisson distribution
   *   zi: linear predictor for zero-inflation part
   * Returns:
   *   a scalar to be added to the log posterior
   */
  real zero_inflated_poisson_logit_lpmf(int y, real lambda, real zi) {
    if (y == 0) {
      return log_sum_exp(bernoulli_logit_lpmf(1 | zi),
                         bernoulli_logit_lpmf(0 | zi) +
                         poisson_lpmf(0 | lambda));
    } else {
      return bernoulli_logit_lpmf(0 | zi) +
             poisson_lpmf(y | lambda);
    }
  }
  /* zero-inflated poisson log-PDF of a single response
   * log parameterization for the poisson part
   * Args:
   *   y: the response value
   *   eta: linear predictor for poisson distribution
   *   zi: zero-inflation probability
   * Returns:
   *   a scalar to be added to the log posterior
   */
  real zero_inflated_poisson_log_lpmf(int y, real eta, real zi) {
    if (y == 0) {
      return log_sum_exp(bernoulli_lpmf(1 | zi),
                         bernoulli_lpmf(0 | zi) +
                         poisson_log_lpmf(0 | eta));
    } else {
      return bernoulli_lpmf(0 | zi) +
             poisson_log_lpmf(y | eta);
    }
  }
  /* zero-inflated poisson log-PDF of a single response
   * log parameterization for the poisson part
   * logit parameterization of the zero-inflation part
   * Args:
   *   y: the response value
   *   eta: linear predictor for poisson distribution
   *   zi: linear predictor for zero-inflation part
   * Returns:
   *   a scalar to be added to the log posterior
   */
  real zero_inflated_poisson_log_logit_lpmf(int y, real eta, real zi) {
    if (y == 0) {
      return log_sum_exp(bernoulli_logit_lpmf(1 | zi),
                         bernoulli_logit_lpmf(0 | zi) +
                         poisson_log_lpmf(0 | eta));
    } else {
      return bernoulli_logit_lpmf(0 | zi) +
             poisson_log_lpmf(y | eta);
    }
  }
  // zero-inflated poisson log-CCDF and log-CDF functions
  real zero_inflated_poisson_lccdf(int y, real lambda, real zi) {
    return bernoulli_lpmf(0 | zi) + poisson_lccdf(y | lambda);
  }
  real zero_inflated_poisson_lcdf(int y, real lambda, real zi) {
    return log1m_exp(zero_inflated_poisson_lccdf(y | lambda, zi));
  }
}
data {
  int<lower=1> N;  // total number of observations
  array[N] int Y;  // response variable
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  int<lower=1> Kc;  // number of population-level effects after centering
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
  matrix[N, Kc] Xc;  // centered version of X without an intercept
  vector[Kc] means_X;  // column means of X before centering
  for (i in 2:K) {
    means_X[i - 1] = mean(X[, i]);
    Xc[, i - 1] = X[, i] - means_X[i - 1];
  }
}
parameters {
  vector[Kc] b;  // regression coefficients
  real Intercept;  // temporary intercept for centered predictors
  real<lower=0,upper=1> zi;  // zero-inflation probability
}
transformed parameters {
  real lprior = 0;  // prior contributions to the log posterior
  lprior += student_t_lpdf(Intercept | 3, -2.3, 2.5);
  lprior += beta_lpdf(zi | 1, 1);
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu = rep_vector(0.0, N);
    mu += Intercept + Xc * b;
    for (n in 1:N) {
      target += zero_inflated_poisson_log_lpmf(Y[n] | mu[n], zi);
    }
  }
  // priors including constants
  target += lprior;
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept - dot_product(means_X, b);
}"

#### Nonlinear model example ####
b <- c(2, 0.75)
x <- rnorm(100)
y <- rnorm(100, mean = b[1] * exp(b[2] * x))
dat1 <- data.frame(x, y)
head(dat1)
# x        y
# 1  0.9241089 3.156914
# 2 -0.1753145 1.936561
# 3  1.6768183 6.370550
# 4 -0.6705956 1.043834
# 5 -0.4407895 2.061754
# 6  1.7391168 7.954501

prior1 <- prior(normal(1, 2), nlpar = "b1") +
  prior(normal(0, 2), nlpar = "b2")
make_stancode(
  bf(y ~ b1 * exp(b2 * x),
     b1 + b2 ~ 1, nl = TRUE),
  data = dat1,
  prior = prior1
)

# Nonlinear model retains mu
"// generated with brms 2.22.9
functions {
}
data {
  int<lower=1> N;  // total number of observations
  vector[N] Y;  // response variable
  int<lower=1> K_b1;  // number of population-level effects
  matrix[N, K_b1] X_b1;  // population-level design matrix
  int<lower=1> K_b2;  // number of population-level effects
  matrix[N, K_b2] X_b2;  // population-level design matrix
  // covariates for non-linear functions
  vector[N] C_1;
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
}
parameters {
  vector[K_b1] b_b1;  // regression coefficients
  vector[K_b2] b_b2;  // regression coefficients
  real<lower=0> sigma;  // dispersion parameter
}
transformed parameters {
  real lprior = 0;  // prior contributions to the log posterior
  lprior += normal_lpdf(b_b1 | 1, 2);
  lprior += normal_lpdf(b_b2 | 0, 2);
  lprior += student_t_lpdf(sigma | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] nlp_b1 = rep_vector(0.0, N);
    // initialize linear predictor term
    vector[N] nlp_b2 = rep_vector(0.0, N);
    // initialize non-linear predictor term
    vector[N] mu;
    nlp_b1 += X_b1 * b_b1;
    nlp_b2 += X_b2 * b_b2;
    for (n in 1:N) {
      // compute non-linear predictor values
      mu[n] = (nlp_b1[n] * exp(nlp_b2[n] * C_1[n]));
    }
    target += normal_lpdf(Y | mu, sigma);
  }
  // priors including constants
  target += lprior;
}
generated quantities {
}"

data(loss)
head(loss)
# AY dev      cum premium
# 1 1991   6  357.848   10000
# 2 1991  18 1124.788   10000
# 3 1991  30 1735.330   10000
# 4 1991  42 2182.708   10000
# 5 1991  54 2745.596   10000
# 6 1991  66 3319.994   10000

make_stancode(
  bf(cum ~ ult * (1 - exp(-(dev/theta)^omega)),
     ult ~ 1 + (1|AY),
     omega ~ 1,
     theta ~ 1,
     nl = TRUE),
  data = loss,
  family = gaussian(),
  prior = c(
    prior(normal(5000, 1000), nlpar = "ult"),
    prior(normal(1, 2), nlpar = "omega"),
    prior(normal(45, 10), nlpar = "theta")
  )
)

# And mu retained in this example
"// generated with brms 2.22.9
functions {
}
data {
  int<lower=1> N;  // total number of observations
  vector[N] Y;  // response variable
  int<lower=1> K_ult;  // number of population-level effects
  matrix[N, K_ult] X_ult;  // population-level design matrix
  int<lower=1> K_omega;  // number of population-level effects
  matrix[N, K_omega] X_omega;  // population-level design matrix
  int<lower=1> K_theta;  // number of population-level effects
  matrix[N, K_theta] X_theta;  // population-level design matrix
  // covariates for non-linear functions
  array[N] int C_1;
  // data for group-level effects of ID 1
  int<lower=1> N_1;  // number of grouping levels
  int<lower=1> M_1;  // number of coefficients per level
  array[N] int<lower=1> J_1;  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_1_ult_1;
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
}
parameters {
  vector[K_ult] b_ult;  // regression coefficients
  vector[K_omega] b_omega;  // regression coefficients
  vector[K_theta] b_theta;  // regression coefficients
  real<lower=0> sigma;  // dispersion parameter
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations
  array[M_1] vector[N_1] z_1;  // standardized group-level effects
}
transformed parameters {
  vector[N_1] r_1_ult_1;  // actual group-level effects
  real lprior = 0;  // prior contributions to the log posterior
  r_1_ult_1 = (sd_1[1] * (z_1[1]));
  lprior += normal_lpdf(b_ult | 5000, 1000);
  lprior += normal_lpdf(b_omega | 1, 2);
  lprior += normal_lpdf(b_theta | 45, 10);
  lprior += student_t_lpdf(sigma | 3, 0, 1963.7)
    - 1 * student_t_lccdf(0 | 3, 0, 1963.7);
  lprior += student_t_lpdf(sd_1 | 3, 0, 1963.7)
    - 1 * student_t_lccdf(0 | 3, 0, 1963.7);
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] nlp_ult = rep_vector(0.0, N);
    // initialize linear predictor term
    vector[N] nlp_omega = rep_vector(0.0, N);
    // initialize linear predictor term
    vector[N] nlp_theta = rep_vector(0.0, N);
    // initialize non-linear predictor term
    vector[N] mu;
    nlp_ult += X_ult * b_ult;
    nlp_omega += X_omega * b_omega;
    nlp_theta += X_theta * b_theta;
    for (n in 1:N) {
      // add more terms to the linear predictor
      nlp_ult[n] += r_1_ult_1[J_1[n]] * Z_1_ult_1[n];
    }
    for (n in 1:N) {
      // compute non-linear predictor values
      mu[n] = (nlp_ult[n] * (1 - exp( - (C_1[n] / nlp_theta[n]) ^ nlp_omega[n])));
    }
    target += normal_lpdf(Y | mu, sigma);
  }
  // priors including constants
  target += lprior;
  target += std_normal_lpdf(z_1[1]);
}
generated quantities {

}"
