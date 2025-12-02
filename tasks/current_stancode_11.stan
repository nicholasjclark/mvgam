// Generated with mvgam 2.0.0 using brms 2.22.9
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
  int<lower=1> K_trend;
  matrix[N_trend, K_trend] X_trend;
  vector[N_trend] Z_1_1_trend;
  vector[N_trend] Z_1_2_trend;
  array[N_trend] int<lower=1> J_1_trend;
  int<lower=1> N_1_trend;
  int<lower=1> M_1_trend;
  int<lower=1> NC_1_trend;
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
  vector[K_trend] b_trend;
  vector<lower=0>[M_1_trend] sd_1_trend;
  matrix[M_1_trend, N_1_trend] z_1_trend;
  cholesky_factor_corr[M_1_trend] L_1_trend;
  vector<lower=0>[N_lv_trend] sigma_trend;
  matrix[N_trend, N_lv_trend] innovations_trend;
  vector<lower=-1, upper=1>[N_lv_trend] ar1_trend;
}
transformed parameters {
  // Prior log-probability accumulator
  real lprior = 0;
  lprior += student_t_lpdf(sd_1_trend | 3, 0, 2.5)
            - 2 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += lkj_corr_cholesky_lpdf(L_1_trend | 1);
  lprior += student_t_lpdf(Intercept | 3, 1.8, 2.5);
  matrix[N_1_trend, M_1_trend] r_1_trend;
  vector[N_1_trend] r_1_1_trend;
  vector[N_1_trend] r_1_2_trend;
  r_1_trend = scale_r_cor(z_1_trend, sd_1_trend, L_1_trend);
  r_1_1_trend = r_1_trend[ : , 1];
  r_1_2_trend = r_1_trend[ : , 2];
  vector[N_trend] mu_trend = rep_vector(0.0, N_trend);
  for (n in 1 : N_trend) {
    mu_trend[n] += r_1_1_trend[J_1_trend[n]] * Z_1_1_trend[n]
                   + r_1_2_trend[J_1_trend[n]] * Z_1_2_trend[n];
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
    vector[N] mu;
    mu = rep_vector(0.0, N);
    mu += Xc * b;
    mu += Intercept;
    for (n in 1 : N) {
      mu[n] += trend[obs_trend_time[n], obs_trend_series[n]];
    }
    // Likelihood calculations
    target += poisson_log_glm_lpmf(Y | to_matrix(mu), 0.0, mu_ones);
  }
  
  // Prior contributions
  target += lprior;
  target += std_normal_lpdf(to_vector(z_1_trend));
}
generated quantities {
  real b_Intercept = Intercept - dot_product(means_X, b);
  corr_matrix[M_1_trend] Cor_1_trend = multiply_lower_tri_self_transpose(L_1_trend);
  vector<lower=-1, upper=1>[NC_1_trend] cor_1_trend;
  for (k_trend in 1 : M_1_trend) {
    for (j_trend in 1 : (k_trend - 1)) {
      cor_1_trend[choose(k_trend - 1, 2) + j_trend] = Cor_1_trend[j_trend, k_trend];
    }
  }
}

