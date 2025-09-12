// generated with brms 2.22.9
functions {
}
data {
  int<lower=1> N;  // total number of observations
  array[N] int Y;  // response variable
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  int<lower=1> Kc;  // number of population-level effects after centering
  int prior_only;  // should the likelihood be ignored?
    int<lower=1> N_trend;  // total number of observations
  int<lower=1> N_series_trend;
  int<lower=1> N_lv_trend;
  array[N_trend, N_series_trend] int times_trend;
  array[N] int obs_trend_time;
  array[N] int obs_trend_series;
  vector[1] mu_ones;
}
transformed data {
  matrix[N, Kc] Xc;  // centered version of X without an intercept
  vector[Kc] means_X;  // column means of X before centering
  matrix[N_series_trend, N_lv_trend] Z = diag_matrix(rep_vector(1.0, N_lv_trend));
  for (i in 2:K) {
    means_X[i - 1] = mean(X[, i]);
    Xc[, i - 1] = X[, i] - means_X[i - 1];
  }
}
parameters {
  vector[Kc] b;  // regression coefficients
  real Intercept;  // temporary intercept for centered predictors
  vector<lower=0>[N_lv_trend] sigma_trend;
  cholesky_factor_corr[N_lv_trend] L_Omega_trend;
  matrix[N_trend, N_lv_trend] innovations_trend;
  
}
transformed parameters {
  real lprior = 0;  // prior contributions to the log posterior
  vector[N_trend] mu_trend = rep_vector(0.0, N_trend);
  int<lower=1> N_trend;  // total number of observations
  vector[N_trend] Y;  // response variable
  vector[N_trend] mu_trend = rep_vector(0.0, N_trend);
  
    // Scaled innovations after applying correlations
    matrix[N_trend, N_lv_trend] scaled_innovations_trend;

    // Apply correlation transformation using efficient non-centered parameterization
    {
      matrix[N_lv_trend, N_lv_trend] L_Sigma = diag_pre_multiply(sigma_trend, L_Omega_trend);
      scaled_innovations_trend = innovations_trend * L_Sigma';
    }
  matrix[N_trend, N_lv_trend] lv_trend;
  matrix[N_lv_trend, N_lv_trend] Sigma_trend = diag_pre_multiply(sigma_trend, L_Omega_trend);
  lv_trend = scaled_innovations_trend;
    // Derived latent trends using universal computation pattern
  matrix[N_trend, N_series_trend] trend;

  // Universal trend computation: state-space dynamics + linear predictors
  // dot_product captures dynamic component, mu_trend captures trend_formula
  for (i in 1:N_trend) {
    for (s in 1:N_series_trend) {
      trend[i, s] = dot_product(Z[s, :], lv_trend[i, :]) + mu_trend[times_trend[i, s]];
    }
  }
  lprior += student_t_lpdf(Intercept | 3, 1.1, 2.5);

  // Efficient matrix multiplication for linear predictor
  vector[N] mu = Xc * b;
  // Add intercept and trend components
  for (n in 1:N) {
    mu[n] += Intercept + trend[obs_trend_time[n], obs_trend_series[n]];
  }
}
model {
  // Shared Gaussian innovation priors
  sigma_trend ~ exponential(2);
  L_Omega_trend ~ lkj_corr_cholesky(2);
  to_vector(innovations_trend) ~ std_normal();
  
  // likelihood including constants
  if (!prior_only) {
    target += poisson_log_glm_lpmf(Y | to_matrix(mu), 0.0, mu_ones);
  }
  // priors including constants
  target += lprior;
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept - dot_product(means_X, b);
}

