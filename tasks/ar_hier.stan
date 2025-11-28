// Generated with mvgam 2.0.0 using brms 2.22.9
functions {
  /* Function to compute a partially pooled correlation matrix
   * Combines global correlation structure with group-specific deviations
   * alpha controls mixing: 1 = pure global, 0 = pure local
   */
  matrix combine_cholesky(matrix global_chol_cor, matrix local_chol_cor,
                          real alpha) {
    int dim = rows(local_chol_cor);
    matrix[dim, dim] global_cor = multiply_lower_tri_self_transpose(global_chol_cor);
    matrix[dim, dim] local_cor = multiply_lower_tri_self_transpose(local_chol_cor);
    matrix[dim, dim] combined_chol_cor;
    combined_chol_cor = cholesky_decompose(alpha * global_cor
                                           + (1 - alpha) * local_cor);
    return combined_chol_cor;
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
  array[N] int obs_trend_time;
  array[N] int obs_trend_series;
  int<lower=1> n_groups_trend;
  array[n_groups_trend] int<lower=1> group_inds_trend;
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
  vector<lower=-1, upper=1>[N_lv_trend] ar1_trend;
  cholesky_factor_corr[3] L_Omega_global_trend;
  array[n_groups_trend] cholesky_factor_corr[3] L_deviation_group_trend;
  real<lower=0, upper=1> alpha_cor_trend;
}
transformed parameters {
  // Prior log-probability accumulator
  real lprior = 0;
  lprior += student_t_lpdf(Intercept_trend | 3, 0, 2.5);
  lprior += student_t_lpdf(Intercept | 3, 1.4, 2.5);
  vector[N_trend] mu_trend = rep_vector(0.0, N_trend);
  mu_trend += Intercept_trend;
  matrix[N_trend, N_lv_trend] scaled_innovations_trend;
  array[n_groups_trend] cholesky_factor_corr[N_lv_trend] L_Omega_group_trend;
  for (g_idx in 1 : n_groups_trend) {
    L_Omega_group_trend[g_idx] = combine_cholesky(L_Omega_global_trend,
                                                  L_deviation_group_trend[g_idx],
                                                  alpha_cor_trend);
  }
  for (g in 1 : n_groups_trend) {
    matrix[N_lv_trend, N_lv_trend] Sigma_group = diag_pre_multiply(sigma_trend,
                                                                   L_Omega_group_trend[g]);
  }
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
  to_vector(innovations_trend) ~ std_normal();
  ar1_trend ~ normal(0, 0.5);
  alpha_cor_trend ~ beta(3, 2);
  L_Omega_global_trend ~ lkj_corr_cholesky(1);
  for (g in 1 : n_groups_trend) {
    L_deviation_group_trend[g] ~ lkj_corr_cholesky(6);
  }
  
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
}
generated quantities {
  real b_Intercept = Intercept - dot_product(means_X, b);
  real b_Intercept_trend = Intercept_trend;
}

