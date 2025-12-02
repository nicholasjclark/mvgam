// Generated with mvgam 2.0.0 using brms 2.22.9
functions {
  
}
data {
  int<lower=1> N;
  vector[N] Y;
  int Ks;
  matrix[N, Ks] Xs;
  int nb_1;
  array[nb_1] int knots_1;
  matrix[N, knots_1[1]] Zs_1_1;
  int Ks_sigma;
  matrix[N, Ks_sigma] Xs_sigma;
  int nb_sigma_1;
  array[nb_sigma_1] int knots_sigma_1;
  matrix[N, knots_sigma_1[1]] Zs_sigma_1_1;
  int prior_only;
  int<lower=1> N_trend;
  int<lower=1> N_series_trend;
  int<lower=1> N_lv_trend;
  array[N_trend, N_series_trend] int times_trend;
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
  vector[Ks] bs;
  vector[knots_1[1]] zs_1_1;
  vector<lower=0>[nb_1] sds_1;
  real Intercept_sigma;
  vector[Ks_sigma] bs_sigma;
  vector[knots_sigma_1[1]] zs_sigma_1_1;
  vector<lower=0>[nb_sigma_1] sds_sigma_1;
  vector<lower=0>[N_lv_trend] sigma_trend;
  matrix[N_trend, N_lv_trend] innovations_trend;
}
transformed parameters {
  vector[knots_1[1]] s_1_1;
  vector[knots_sigma_1[1]] s_sigma_1_1;
  // Prior log-probability accumulator
  real lprior = 0;
  lprior += student_t_lpdf(Intercept | 3, 9.9, 2.5);
  lprior += student_t_lpdf(sds_1 | 3, 0, 2.5)
            - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += student_t_lpdf(Intercept_sigma | 3, 0, 2.5);
  lprior += student_t_lpdf(sds_sigma_1 | 3, 0, 2.5)
            - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  vector[N_trend] mu_trend = rep_vector(0.0, N_trend);
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
  s_1_1 = sds_1[1] * zs_1_1;
  s_sigma_1_1 = sds_sigma_1[1] * zs_sigma_1_1;
}
model {
  sigma_trend ~ exponential(2);
  to_vector(innovations_trend) ~ std_normal();
  
  // Observation linear predictors and likelihoods (skipped when sampling from prior only)
  if (!prior_only) {
    vector[N] mu = rep_vector(0.0, N);
    vector[N] sigma = rep_vector(0.0, N);
    mu += Intercept + Xs * bs + Zs_1_1 * s_1_1;
    for (n in 1 : N) {
      mu[n] += trend[obs_trend_time[n], obs_trend_series[n]];
    }
    sigma += Intercept_sigma + Xs_sigma * bs_sigma
             + Zs_sigma_1_1 * s_sigma_1_1;
    sigma = exp(sigma);
    // Likelihood calculations
    target += normal_lpdf(Y | mu, sigma);
  }
  
  // Prior contributions
  target += lprior;
  target += std_normal_lpdf(zs_1_1);
  target += std_normal_lpdf(zs_sigma_1_1);
}
generated quantities {
  real b_Intercept = Intercept;
  real b_sigma_Intercept = Intercept_sigma;
}

