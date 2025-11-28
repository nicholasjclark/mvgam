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
  vector[N] Y;
  int prior_only;
  int<lower=1> N_trend;
  int<lower=1> N_series_trend;
  int<lower=1> N_lv_trend;
  array[N_trend, N_series_trend] int times_trend;
  array[N_series_trend] int<lower=1> group_inds_trend;
  int<lower=1> K_trend;
  int<lower=1> Kc_trend;
  matrix[N_trend, K_trend] X_trend;
  array[N] int obs_trend_time;
  array[N] int obs_trend_series;
  int<lower=1> N_groups_trend;
  int<lower=1> N_subgroups_trend;
}
transformed data {
  // Factor loadings matrix: maps latent variables to observed series
  matrix[N_series_trend, N_lv_trend] Z = diag_matrix(rep_vector(1.0,
                                                                N_lv_trend));
  matrix[N_trend, Kc_trend] Xc_trend;
  vector[Kc_trend] means_X_trend;
  for (i_trend in 2 : K_trend) {
    means_X_trend[i_trend - 1] = mean(X_trend[ : , i_trend]);
    Xc_trend[ : , i_trend - 1] = X_trend[ : , i_trend]
                                 - means_X_trend[i_trend - 1];
  }
}
parameters {
  real Intercept;
  real<lower=0> sigma;
  vector[Kc_trend] b_trend;
  real Intercept_trend;
  vector<lower=0>[N_lv_trend] sigma_trend;
  matrix[N_trend, N_lv_trend] innovations_trend;
  cholesky_factor_corr[3] L_Omega_global_trend;
  array[N_groups_trend] cholesky_factor_corr[3] L_deviation_group_trend;
  real<lower=0, upper=1> alpha_cor_trend;
}
transformed parameters {
  // Prior log-probability accumulator
  real lprior = 0;
  lprior += student_t_lpdf(Intercept_trend | 3, 0, 2.5);
  lprior += student_t_lpdf(Intercept | 3, 1, 2.5);
  lprior += student_t_lpdf(sigma | 3, 0, 2.5)
            - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  vector[N_trend] mu_trend = rep_vector(0.0, N_trend);
  mu_trend += Intercept_trend + Xc_trend * b_trend;
  matrix[N_trend, N_lv_trend] scaled_innovations_trend;
  // Latent variable trajectories over time
  matrix[N_trend, N_lv_trend] lv_trend;
  lv_trend = scaled_innovations_trend;
  array[N_groups_trend] cholesky_factor_corr[N_subgroups_trend] L_Omega_group_trend;
  array[N_groups_trend] cov_matrix[N_subgroups_trend] Sigma_group_trend;
  for (g_idx in 1 : N_groups_trend) {
    L_Omega_group_trend[g_idx] = combine_cholesky(L_Omega_global_trend,
                                                  L_deviation_group_trend[g_idx],
                                                  alpha_cor_trend);
    Sigma_group_trend[g_idx] = multiply_lower_tri_self_transpose(diag_pre_multiply(
                                                                 sigma_trend,
                                                                 L_Omega_group_trend[g_idx]));
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
  alpha_cor_trend ~ beta(3, 2);
  L_Omega_global_trend ~ lkj_corr_cholesky(1);
  for (g in 1 : N_groups_trend) {
    L_deviation_group_trend[g] ~ lkj_corr_cholesky(6);
  }
  
  // Observation linear predictors and likelihoods (skipped when sampling from prior only)
  if (!prior_only) {
    vector[N] mu = rep_vector(0.0, N);
    mu += Intercept;
    for (n in 1 : N) {
      mu[n] += trend[obs_trend_time[n], obs_trend_series[n]];
    }
    // Likelihood calculations
    target += lognormal_lpdf(Y | mu, sigma);
  }
  
  // Prior contributions
  target += lprior;
}
generated quantities {
  real b_Intercept = Intercept;
  real b_Intercept_trend = Intercept_trend
                           - dot_product(means_X_trend, b_trend);
}

