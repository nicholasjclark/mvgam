// Generated with mvgam 2.0.0 using brms 2.22.9
functions {
  
}
data {
  int<lower=1> N;
  array[N] int Y;
  int<lower=1> K;
  matrix[N, K] X;
  int<lower=1> Kc;
  int prior_only;
  int<lower=1> N_trend;
  int<lower=1> N_series_trend;
  int<lower=1> N_lv_trend;
  array[N_trend, N_series_trend] int times_trend;
  array[N] int obs_trend_time;
  array[N] int obs_trend_series;
  vector[1] mu_ones;
}
transformed data {
  matrix[N, Kc] Xc;
  vector[Kc] means_X;
  // Factor loadings matrix: maps latent variables to observed series
  matrix[N_series_trend, N_lv_trend] Z = diag_matrix(rep_vector(1.0,
                                                                N_lv_trend));
  for (i in 2 : K) {
    means_X[i - 1] = mean(X[ : , i]);
    Xc[ : , i - 1] = X[ : , i] - means_X[i - 1];
  }
}
parameters {
  vector[Kc] b;
  real Intercept;
  real Intercept_trend;
  vector<lower=0>[N_lv_trend] sigma_trend;
  matrix[N_trend, N_lv_trend] innovations_trend;
}
transformed parameters {
  // Prior log-probability accumulator
  real lprior = 0;
  lprior += student_t_lpdf(Intercept_trend | 3, 0, 2.5);
  lprior += student_t_lpdf(Intercept | 3, 1.8, 2.5);
  vector[N_trend] mu_trend = rep_vector(0.0, N_trend);
  mu_trend += Intercept_trend;
  matrix[N_trend, N_lv_trend] scaled_innovations_trend;
  scaled_innovations_trend = innovations_trend * diag_matrix(sigma_trend);
  // Latent variable trajectories over time
  matrix[N_trend, N_lv_trend] lv_trend;
  lv_trend[1,  : ] = scaled_innovations_trend[1,  : ];
  for (i in 2 : N_trend) {
    lv_trend[i,  : ] = lv_trend[i - 1,  : ]
                       + scaled_innovations_trend[i,  : ];
  }
  // Final trend values for each time point and series
  matrix[N_trend, N_series_trend] trend;
  // Map latent variables to trend values via factor loadings
  for (i in 1 : N_trend) {
    for (s in 1 : N_series_trend) {
      trend[i, s] = dot_product(Z[s,  : ], lv_trend[i,  : ])
                    + mu_trend[times_trend[i, s]];
    }
  }
}
model {
  sigma_trend ~ exponential(2);
  to_vector(innovations_trend) ~ std_normal();
  // Likelihood calculation (skipped when sampling from prior only)
  if (!prior_only) {
    vector[N] mu = Xc * b;
    for (n in 1 : N) {
      mu[n] += Intercept + trend[obs_trend_time[n], obs_trend_series[n]];
    }
    target += poisson_log_glm_lpmf(Y | to_matrix(mu), 0.0, mu_ones);
  }
  target += lprior;
}
generated quantities {
  real b_Intercept = Intercept - dot_product(means_X, b);
  real b_Intercept_trend = Intercept_trend;
}

