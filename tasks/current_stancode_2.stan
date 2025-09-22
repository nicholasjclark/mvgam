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
  real Intercept_trend;
  vector<lower=0>[N_lv_trend] sigma_trend;
  cholesky_factor_corr[N_lv_trend] L_Omega_trend;
  matrix[N_trend, N_lv_trend] innovations_trend;
}
transformed parameters {
  real lprior = 0;
  vector[N_trend] mu_trend = rep_vector(0.0, N_trend);
  mu_trend += Intercept_trend;
  matrix[N_trend, N_lv_trend] scaled_innovations_trend;
  {
    matrix[N_lv_trend, N_lv_trend] L_Sigma = diag_pre_multiply(sigma_trend,
                                                               L_Omega_trend);
    scaled_innovations_trend = innovations_trend * L_Sigma';
  }
  matrix[N_trend, N_lv_trend] lv_trend;
  lprior += student_t_lpdf(Intercept_trend | 3, -0.1, 2.5);
  matrix[N_lv_trend, N_lv_trend] Sigma_trend = diag_pre_multiply(sigma_trend,
                                                                 L_Omega_trend);
  lv_trend[1,  : ] = scaled_innovations_trend[1,  : ];
  for (i in 2 : N_trend) {
    lv_trend[i,  : ] = lv_trend[i - 1,  : ]
                       + scaled_innovations_trend[i,  : ];
  }
  matrix[N_trend, N_series_trend] trend;
  for (i in 1 : N_trend) {
    for (s in 1 : N_series_trend) {
      trend[i, s] = dot_product(Z[s,  : ], lv_trend[i,  : ])
                    + mu_trend[times_trend[i, s]];
    }
  }
  lprior += student_t_lpdf(Intercept_count | 3, 4, 2.5);
  lprior += student_t_lpdf(sigma_count | 3, 0, 2.5)
            - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += student_t_lpdf(Intercept_biomass | 3, 2.4, 2.5);
  lprior += student_t_lpdf(sigma_biomass | 3, 0, 2.5)
            - 1 * student_t_lccdf(0 | 3, 0, 2.5);
}
model {
  sigma_trend ~ exponential(2);
  L_Omega_trend ~ lkj_corr_cholesky(2);
  to_vector(innovations_trend) ~ std_normal();
  if (!prior_only) {
    vector[N_biomass] mu_biomass = Xc_biomass * b_biomass;
    for (n in 1 : N_biomass) {
      mu_biomass[n] += Intercept_biomass
                       + trend[obs_trend_time_biomass[n], obs_trend_series_biomass[n]];
    }
    vector[N_count] mu_count = Xc_count * b_count;
    for (n in 1 : N_count) {
      mu_count[n] += Intercept_count
                     + trend[obs_trend_time_count[n], obs_trend_series_count[n]];
    }
    target += normal_id_glm_lpdf(Y_count | to_matrix(mu_count), 0.0, mu_ones_count, sigma_count);
    target += normal_id_glm_lpdf(Y_biomass | to_matrix(mu_biomass), 0.0, mu_ones_biomass, sigma_biomass);
  }
  target += lprior;
}
generated quantities {
  real b_count_Intercept = Intercept_count
                           - dot_product(means_X_count, b_count);
  real b_biomass_Intercept = Intercept_biomass
                             - dot_product(means_X_biomass, b_biomass);
  real b_Intercept_trend = Intercept_trend;
}

