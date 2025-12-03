// Generated with mvgam 2.0.0 using brms 2.22.9
functions {
  
}
data {
  int<lower=1> N;
  array[N] int Y;
  int prior_only;
  int<lower=1> N_trend;
  int<lower=1> N_series_trend;
  int<lower=1> N_lv_trend;
  array[N_trend, N_series_trend] int times_trend;
  vector[N_trend] Z_1_1_trend;
  array[N_trend] int<lower=1> J_1_trend;
  int<lower=1> N_1_trend;
  int<lower=1> M_1_trend;
  array[N] int obs_trend_time;
  array[N] int obs_trend_series;
}
transformed data {
  // Factor loadings matrix: maps latent variables to observed series
  matrix[N_series_trend, N_lv_trend] Z = diag_matrix(rep_vector(1.0,
                                                                N_lv_trend));
}
parameters {
  real Intercept;
  vector<lower=0>[M_1_trend] sd_1_trend;
  array[M_1_trend] vector[N_1_trend] z_1_trend;
  vector<lower=0>[N_lv_trend] sigma_trend;
  matrix[N_trend, N_lv_trend] innovations_trend;
  vector<lower=-1, upper=1>[N_lv_trend] ar1_trend;
}
transformed parameters {
  // Prior log-probability accumulator
  real lprior = 0;
  lprior += student_t_lpdf(sd_1_trend | 3, 0, 2.5)
            - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += student_t_lpdf(Intercept | 3, 1.6, 2.5);
  vector[N_1_trend] r_1_1_trend;
  r_1_1_trend = (sd_1_trend[1] * (z_1_trend[1]));
  vector[N_trend] mu_trend = rep_vector(0.0, N_trend);
  for (n in 1 : N_trend) {
    mu_trend[n] += r_1_1_trend[J_1_trend[n]] * Z_1_1_trend[n];
  }
  matrix[N_trend, N_lv_trend] scaled_innovations_trend;
  scaled_innovations_trend = innovations_trend * diag_matrix(sigma_trend);
  // Latent variable trajectories over time
  matrix[N_trend, N_lv_trend] lv_trend;
  for (i in 1 : 1) {
    lv_trend[i,  : ] = scaled_innovations_trend[i,  : ];
  }
  for (i in 2 : N_trend) {
    for (j in 1 : N_lv_trend) {
      lv_trend[i, j] = ar1_trend[j] * lv_trend[i - 1, j]
                       + scaled_innovations_trend[i, j];
    }
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
  ar1_trend ~ normal(0, 0.5);
  
  // Observation linear predictors and likelihoods (skipped when sampling from prior only)
  if (!prior_only) {
    vector[N] mu = rep_vector(0.0, N);
    mu += Intercept;
    for (n in 1 : N) {
      mu[n] += trend[obs_trend_time[n], obs_trend_series[n]];
    }
    // Likelihood calculations
    target += poisson_log_lpmf(Y | mu);
  }
  
  // Prior contributions
  target += lprior;
  target += std_normal_lpdf(z_1_trend[1]);
}
generated quantities {
  real b_Intercept = Intercept;
}

