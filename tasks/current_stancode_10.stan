// Generated with mvgam 2.0.0 using brms 2.22.9
functions {
  
}
data {
  int<lower=1> N;
  int<lower=1> N_count;
  vector[N_count] Y_count;
  int<lower=1> K_count;
  matrix[N_count, K_count] X_count;
  int<lower=1> Kc_count;
  int<lower=1> N_biomass;
  vector[N_biomass] Y_biomass;
  int<lower=1> K_biomass;
  matrix[N_biomass, K_biomass] X_biomass;
  int<lower=1> Kc_biomass;
  int<lower=1> N_1;
  int<lower=1> M_1;
  array[N_count] int<lower=1> J_1_count;
  vector[N_count] Z_1_count_1;
  int<lower=1> N_2;
  int<lower=1> M_2;
  array[N_count] int<lower=1> J_2_count;
  vector[N_count] Z_2_count_1;
  int<lower=1> N_3;
  int<lower=1> M_3;
  array[N_biomass] int<lower=1> J_3_biomass;
  vector[N_biomass] Z_3_biomass_1;
  int<lower=1> N_4;
  int<lower=1> M_4;
  array[N_biomass] int<lower=1> J_4_biomass;
  vector[N_biomass] Z_4_biomass_1;
  int prior_only;
  int<lower=1> N_trend;
  int<lower=1> N_series_trend;
  int<lower=1> N_lv_trend;
  array[N_trend, N_series_trend] int times_trend;
  array[N_count] int obs_trend_time_count;
  array[N_count] int obs_trend_series_count;
  array[N_biomass] int obs_trend_time_biomass;
  array[N_biomass] int obs_trend_series_biomass;
  vector[1] mu_ones_count;
  vector[1] mu_ones_biomass;
}
transformed data {
  matrix[N_count, Kc_count] Xc_count;
  vector[Kc_count] means_X_count;
  matrix[N_biomass, Kc_biomass] Xc_biomass;
  vector[Kc_biomass] means_X_biomass;
  // Factor loadings matrix: maps latent variables to observed series
  matrix[N_series_trend, N_lv_trend] Z = diag_matrix(rep_vector(1.0,
                                                                N_lv_trend));
  for (i in 2 : K_count) {
    means_X_count[i - 1] = mean(X_count[ : , i]);
    Xc_count[ : , i - 1] = X_count[ : , i] - means_X_count[i - 1];
  }
  for (i in 2 : K_biomass) {
    means_X_biomass[i - 1] = mean(X_biomass[ : , i]);
    Xc_biomass[ : , i - 1] = X_biomass[ : , i] - means_X_biomass[i - 1];
  }
}
parameters {
  vector[Kc_count] b_count;
  real Intercept_count;
  real<lower=0> sigma_count;
  vector[Kc_biomass] b_biomass;
  real Intercept_biomass;
  real<lower=0> sigma_biomass;
  vector<lower=0>[M_1] sd_1;
  array[M_1] vector[N_1] z_1;
  vector<lower=0>[M_2] sd_2;
  array[M_2] vector[N_2] z_2;
  vector<lower=0>[M_3] sd_3;
  array[M_3] vector[N_3] z_3;
  vector<lower=0>[M_4] sd_4;
  array[M_4] vector[N_4] z_4;
  vector<lower=0>[N_lv_trend] sigma_trend;
  cholesky_factor_corr[N_lv_trend] L_Omega_trend;
  matrix[N_trend, N_lv_trend] innovations_trend;
}
transformed parameters {
  vector[N_1] r_1_count_1;
  vector[N_2] r_2_count_1;
  vector[N_3] r_3_biomass_1;
  vector[N_4] r_4_biomass_1;
  // Prior log-probability accumulator
  real lprior = 0;
  lprior += student_t_lpdf(Intercept_count | 3, 3, 2.5);
  lprior += student_t_lpdf(sigma_count | 3, 0, 2.5)
            - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += student_t_lpdf(Intercept_biomass | 3, 2.7, 2.5);
  lprior += student_t_lpdf(sigma_biomass | 3, 0, 2.5)
            - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += student_t_lpdf(sd_1 | 3, 0, 2.5)
            - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += student_t_lpdf(sd_2 | 3, 0, 2.5)
            - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += student_t_lpdf(sd_3 | 3, 0, 2.5)
            - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += student_t_lpdf(sd_4 | 3, 0, 2.5)
            - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  vector[N_trend] mu_trend = rep_vector(0.0, N_trend);
  matrix[N_trend, N_lv_trend] scaled_innovations_trend;
  {
    matrix[N_lv_trend, N_lv_trend] L_Sigma_trend = diag_pre_multiply(sigma_trend,
                                                                    L_Omega_trend);
    scaled_innovations_trend = innovations_trend * L_Sigma_trend';
  }
  // Latent variable trajectories over time
  matrix[N_trend, N_lv_trend] lv_trend;
  matrix[N_lv_trend, N_lv_trend] Sigma_trend = diag_pre_multiply(sigma_trend,
                                                                 L_Omega_trend);
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
  r_1_count_1 = (sd_1[1] * (z_1[1]));
  r_2_count_1 = (sd_2[1] * (z_2[1]));
  r_3_biomass_1 = (sd_3[1] * (z_3[1]));
  r_4_biomass_1 = (sd_4[1] * (z_4[1]));
}
model {
  sigma_trend ~ exponential(2);
  L_Omega_trend ~ lkj_corr_cholesky(2);
  to_vector(innovations_trend) ~ std_normal();
  
  // Observation linear predictors and likelihoods (skipped when sampling from prior only)
  if (!prior_only) {
    vector[N_count] mu_count = rep_vector(0.0, N_count);
    vector[N_biomass] mu_biomass = rep_vector(0.0, N_biomass);
    mu_count += Intercept_count;
    mu_biomass += Intercept_biomass;
    for (n in 1 : N_count) {
      mu_count[n] += r_1_count_1[J_1_count[n]] * Z_1_count_1[n]
                     + r_2_count_1[J_2_count[n]] * Z_2_count_1[n];
    }
    for (i in 1 : N_count) {
      mu_count[i] += trend[obs_trend_time_count[i], obs_trend_series_count[i]];
    }
    for (n in 1 : N_biomass) {
      mu_biomass[n] += r_3_biomass_1[J_3_biomass[n]] * Z_3_biomass_1[n]
                       + r_4_biomass_1[J_4_biomass[n]] * Z_4_biomass_1[n];
    }
    for (i in 1 : N_biomass) {
      mu_biomass[i] += trend[obs_trend_time_biomass[i], obs_trend_series_biomass[i]];
    }
    // Likelihood calculations
    target += normal_id_glm_lpdf(Y_count | to_matrix(mu_count), 0.0, mu_ones_count, sigma_count);
    target += normal_id_glm_lpdf(Y_biomass | to_matrix(mu_biomass), 0.0, mu_ones_biomass, sigma_biomass);
  }
  
  // Prior contributions
  target += lprior;
  target += std_normal_lpdf(z_1[1]);
  target += std_normal_lpdf(z_2[1]);
  target += std_normal_lpdf(z_3[1]);
  target += std_normal_lpdf(z_4[1]);
}
generated quantities {
  real b_count_Intercept = Intercept_count
                           - dot_product(means_X_count, b_count);
  real b_biomass_Intercept = Intercept_biomass
                             - dot_product(means_X_biomass, b_biomass);
}

