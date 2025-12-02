// Generated with mvgam 2.0.0 using brms 2.22.9
functions {
  
}
data {
  int<lower=1> N;
  int<lower=1> N_count;
  array[N_count] int Y_count;
  int<lower=1> K_count;
  matrix[N_count, K_count] X_count;
  int<lower=1> Kc_count;
  int<lower=1> N_presence;
  array[N_presence] int Y_presence;
  int<lower=1> K_presence;
  matrix[N_presence, K_presence] X_presence;
  int<lower=1> Kc_presence;
  int<lower=1> N_biomass;
  vector[N_biomass] Y_biomass;
  int<lower=1> K_biomass;
  matrix[N_biomass, K_biomass] X_biomass;
  int<lower=1> Kc_biomass;
  int prior_only;
  int<lower=1> N_trend;
  int<lower=1> N_series_trend;
  int<lower=1> N_lv_trend;
  array[N_trend, N_series_trend] int times_trend;
  array[N_count] int obs_trend_time_count;
  array[N_count] int obs_trend_series_count;
  array[N_presence] int obs_trend_time_presence;
  array[N_presence] int obs_trend_series_presence;
  array[N_biomass] int obs_trend_time_biomass;
  array[N_biomass] int obs_trend_series_biomass;
  vector[1] mu_ones_count;
  vector[1] mu_ones_presence;
}
transformed data {
  matrix[N_count, Kc_count] Xc_count;
  vector[Kc_count] means_X_count;
  matrix[N_presence, Kc_presence] Xc_presence;
  vector[Kc_presence] means_X_presence;
  matrix[N_biomass, Kc_biomass] Xc_biomass;
  vector[Kc_biomass] means_X_biomass;
  for (i in 2 : K_count) {
    means_X_count[i - 1] = mean(X_count[ : , i]);
    Xc_count[ : , i - 1] = X_count[ : , i] - means_X_count[i - 1];
  }
  for (i in 2 : K_presence) {
    means_X_presence[i - 1] = mean(X_presence[ : , i]);
    Xc_presence[ : , i - 1] = X_presence[ : , i] - means_X_presence[i - 1];
  }
  for (i in 2 : K_biomass) {
    means_X_biomass[i - 1] = mean(X_biomass[ : , i]);
    Xc_biomass[ : , i - 1] = X_biomass[ : , i] - means_X_biomass[i - 1];
  }
}
parameters {
  vector[Kc_count] b_count;
  real Intercept_count;
  vector[Kc_presence] b_presence;
  real Intercept_presence;
  vector[Kc_biomass] b_biomass;
  real Intercept_biomass;
  real<lower=0> shape_biomass;
  vector<lower=0>[N_lv_trend] sigma_trend;
  cholesky_factor_corr[N_lv_trend] L_Omega_trend;
  matrix[N_trend, N_lv_trend] innovations_trend;
  vector<lower=-1, upper=1>[N_lv_trend] ar1_trend;
  vector[N_series_trend * N_lv_trend] Z_raw;
}
transformed parameters {
  // Prior log-probability accumulator
  real lprior = 0;
  lprior += student_t_lpdf(Intercept_count | 3, 1.1, 2.5);
  lprior += student_t_lpdf(Intercept_presence | 3, 0, 2.5);
  lprior += student_t_lpdf(Intercept_biomass | 3, 1, 2.5);
  lprior += gamma_lpdf(shape_biomass | 0.01, 0.01);
  vector[N_trend] mu_trend = rep_vector(0.0, N_trend);
  matrix[N_trend, N_lv_trend] scaled_innovations_trend;
  {
    matrix[N_lv_trend, N_lv_trend] L_Sigma_trend = diag_pre_multiply(sigma_trend,
                                                                    L_Omega_trend);
    scaled_innovations_trend = innovations_trend * L_Sigma_trend';
  }
  // Latent variable trajectories over time
  matrix[N_trend, N_lv_trend] lv_trend;
  // Factor loadings matrix: maps latent variables to observed series
  matrix[N_series_trend, N_lv_trend] Z = rep_matrix(0, N_series_trend,
                                                    N_lv_trend);
  {
    int index = 1;
    for (j in 1 : N_lv_trend) {
      for (i in j : N_series_trend) {
        Z[i, j] = Z_raw[index];
        index += 1;
      }
    }
  }
  matrix[N_lv_trend, N_lv_trend] Sigma_trend = diag_pre_multiply(sigma_trend,
                                                                 L_Omega_trend);
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
  L_Omega_trend ~ lkj_corr_cholesky(2);
  to_vector(innovations_trend) ~ std_normal();
  ar1_trend ~ normal(0, 0.5);
  Z_raw ~ student_t(3, 0, 1);
  
  // Observation linear predictors and likelihoods (skipped when sampling from prior only)
  if (!prior_only) {
    vector[N_presence] mu_presence = Xc_presence * b_presence;
    for (n in 1 : N_presence) {
      mu_presence[n] += Intercept_presence
                        + trend[obs_trend_time_presence[n], obs_trend_series_presence[n]];
    }
    vector[N_count] mu_count = Xc_count * b_count;
    for (n in 1 : N_count) {
      mu_count[n] += Intercept_count
                     + trend[obs_trend_time_count[n], obs_trend_series_count[n]];
    }
    vector[N_biomass] mu_biomass = rep_vector(0.0, N_biomass);
    mu_biomass += Intercept_biomass + Xc_biomass * b_biomass;
    for (n in 1 : N_biomass) {
      mu_biomass[n] += trend[obs_trend_time_biomass[n], obs_trend_series_biomass[n]];
    }
    mu_biomass = exp(mu_biomass);
    // Likelihood calculations
    target += poisson_log_glm_lpmf(Y_count | to_matrix(mu_count), 0.0, mu_ones_count);
    target += bernoulli_logit_glm_lpmf(Y_presence | to_matrix(mu_presence), 0.0, mu_ones_presence);
    target += gamma_lpdf(Y_biomass | shape_biomass, shape_biomass
                                                    ./ mu_biomass);
  }
  
  // Prior contributions
  target += lprior;
}
generated quantities {
  real b_count_Intercept = Intercept_count
                           - dot_product(means_X_count, b_count);
  real b_presence_Intercept = Intercept_presence
                              - dot_product(means_X_presence, b_presence);
  real b_biomass_Intercept = Intercept_biomass
                             - dot_product(means_X_biomass, b_biomass);
}

