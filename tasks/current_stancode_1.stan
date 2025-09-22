// generated with brms 2.22.9
functions {
  
}
data {
  int<lower=1> N; // total number of observations
  array[N] int Y; // response variable
  int<lower=1> K; // number of population-level effects
  matrix[N, K] X; // population-level design matrix
  int<lower=1> Kc; // number of population-level effects after centering
  int prior_only; // should the likelihood be ignored?
  int<lower=1> N_trend; // total number of observations
  int<lower=1> N_series_trend;
  int<lower=1> N_lv_trend;
  array[N_trend, N_series_trend] int times_trend;
  array[N] int obs_trend_time;
  array[N] int obs_trend_series;
  vector[1] mu_ones;
}
transformed data {
  matrix[N, Kc] Xc; // centered version of X without an intercept
  vector[Kc] means_X; // column means of X before centering
  matrix[N_series_trend, N_lv_trend] Z = diag_matrix(rep_vector(1.0,
                                                                N_lv_trend));
  for (i in 2 : K) {
    means_X[i - 1] = mean(X[ : , i]);
    Xc[ : , i - 1] = X[ : , i] - means_X[i - 1];
  }
}
parameters {
  vector[Kc] b; // regression coefficients
  real Intercept; // temporary intercept for centered predictors
  real Intercept_trend; // temporary intercept for centered predictors
  vector<lower=0>[N_lv_trend] sigma_trend;
  matrix[N_trend, N_lv_trend] innovations_trend;
}
transformed parameters {
  real lprior = 0; // prior contributions to the log posterior
  vector[N_trend] mu_trend = rep_vector(0.0, N_trend);
  mu_trend += Intercept_trend;
  
  // Scaled innovations (uncorrelated case)
  matrix[N_trend, N_lv_trend] scaled_innovations_trend;
  
  // Apply scaling using vectorized operations
  scaled_innovations_trend = innovations_trend * diag_matrix(sigma_trend);
  matrix[N_trend, N_lv_trend] lv_trend;
  lprior += student_t_lpdf(Intercept_trend | 3, 0, 2.5);
  
  // Latent states with RW dynamics
  // Apply RW dynamics
  lv_trend[1,  : ] = scaled_innovations_trend[1,  : ];
  for (i in 2 : N_trend) {
    lv_trend[i,  : ] = lv_trend[i - 1,  : ]
                       + scaled_innovations_trend[i,  : ];
  }
  
  // Derived latent trends using universal computation pattern
  matrix[N_trend, N_series_trend] trend;
  
  // Universal trend computation: state-space dynamics + linear predictors
  // dot_product captures dynamic component, mu_trend captures trend_formula
  for (i in 1 : N_trend) {
    for (s in 1 : N_series_trend) {
      trend[i, s] = dot_product(Z[s,  : ], lv_trend[i,  : ])
                    + mu_trend[times_trend[i, s]];
    }
  }
  lprior += student_t_lpdf(Intercept | 3, 1.8, 2.5);
}
model {
  // Shared Gaussian innovation priors
  sigma_trend ~ exponential(2);
  to_vector(innovations_trend) ~ std_normal();
  
  // likelihood including constants
  if (!prior_only) {
    // Efficient matrix multiplication for linear predictor
    vector[N] mu = Xc * b;
    
    // Add intercept and trend components
    for (n in 1 : N) {
      mu[n] += Intercept + trend[obs_trend_time[n], obs_trend_series[n]];
    }
    target += poisson_log_glm_lpmf(Y | to_matrix(mu), 0.0, mu_ones);
  }
  
  // priors including constants
  target += lprior;
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept - dot_product(means_X, b);
  
  // actual population-level intercept
  real b_Intercept_trend = Intercept_trend;
}

