// Generated with mvgam 2.0.0 using brms 2.22.9
functions {
  matrix sqrtm(matrix A) {
    int m = rows(A);
    vector[m] eigenvals = eigenvalues_sym(A);
    
    // Numerical stability check for positive definiteness
    if (min(eigenvals) <= 1e-12) {
      reject("Matrix must be positive definite for square root computation");
    }
    vector[m] root_root_evals = sqrt(sqrt(eigenvals));
    matrix[m, m] evecs = eigenvectors_sym(A);
    matrix[m, m] eprod = diag_post_multiply(evecs, root_root_evals);
    return tcrossprod(eprod);
  }
  matrix AtoP(matrix P_real) {
    int m = rows(P_real);
    matrix[m, m] B = tcrossprod(P_real);
    for (i in 1 : m) {
      B[i, i] += 1.0;
    }
    return mdivide_left_spd(sqrtm(B), P_real);
  }
  matrix kronecker_prod(matrix A, matrix B) {
    int m = rows(A);
    int n = cols(A);
    int p = rows(B);
    int q = cols(B);
    matrix[m * p, n * q] C;
    for (i in 1 : m) {
      for (j in 1 : n) {
        int row_start = (i - 1) * p + 1;
        int row_end = (i - 1) * p + p;
        int col_start = (j - 1) * q + 1;
        int col_end = (j - 1) * q + q;
        C[row_start : row_end, col_start : col_end] = A[i, j] * B;
      }
    }
    return C;
  }
  array[,] matrix rev_mapping(array[] matrix P, matrix Sigma) {
    int p = size(P);
    int m = rows(Sigma);
    array[p, p] matrix[m, m] phi_for;
    array[p, p] matrix[m, m] phi_rev;
    array[p + 1] matrix[m, m] Sigma_for;
    array[p + 1] matrix[m, m] Sigma_rev;
    matrix[m, m] S_for;
    matrix[m, m] S_rev;
    array[p + 1] matrix[m, m] S_for_list;
    array[p + 1] matrix[m, m] Gamma_trans;
    array[2, p] matrix[m, m] phiGamma;
    Sigma_for[p + 1] = Sigma;
    S_for_list[p + 1] = sqrtm(Sigma);
    for (s in 1 : p) {
      S_for = -tcrossprod(P[p - s + 1]);
      for (i in 1 : m) {
        S_for[i, i] += 1.0;
      }
      S_rev = sqrtm(S_for);
      S_for_list[p - s + 1] = mdivide_right_spd(mdivide_left_spd(S_rev,
                                                                 sqrtm(
                                                                 quad_form_sym(
                                                                 Sigma_for[
                                                                 p - s + 2],
                                                                 S_rev))),
                                                S_rev);
      Sigma_for[p - s + 1] = tcrossprod(S_for_list[p - s + 1]);
    }
    Sigma_rev[1] = Sigma_for[1];
    Gamma_trans[1] = Sigma_for[1];
    for (s in 0 : (p - 1)) {
      S_for = S_for_list[s + 1];
      S_rev = sqrtm(Sigma_rev[s + 1]);
      phi_for[s + 1, s + 1] = mdivide_right_spd(S_for * P[s + 1], S_rev);
      phi_rev[s + 1, s + 1] = mdivide_right_spd(S_rev * P[s + 1]', S_for);
      Gamma_trans[s + 2] = phi_for[s + 1, s + 1] * Sigma_rev[s + 1];
      if (s >= 1) {
        for (k in 1 : s) {
          phi_for[s + 1, k] = phi_for[s, k]
                              - phi_for[s + 1, s + 1] * phi_rev[s, s - k + 1];
          phi_rev[s + 1, k] = phi_rev[s, k]
                              - phi_rev[s + 1, s + 1] * phi_for[s, s - k + 1];
        }
        for (k in 1 : s) {
          Gamma_trans[s + 2] = Gamma_trans[s + 2]
                               + phi_for[s, k] * Gamma_trans[s + 2 - k];
        }
      }
      Sigma_rev[s + 2] = Sigma_rev[s + 1]
                         - quad_form_sym(Sigma_for[s + 1],
                                         phi_rev[s + 1, s + 1]');
    }
    for (i in 1 : p) {
      phiGamma[1, i] = phi_for[p, i];
      phiGamma[2, i] = Gamma_trans[i]';
    }
    return phiGamma;
  }
  matrix initial_joint_var(matrix Sigma, array[] matrix phi,
                           array[] matrix theta) {
    int p = size(phi);
    int q = size(theta);
    int m = rows(Sigma);
    matrix[(p + q) * m, (p + q) * m] companion_mat = rep_matrix(0.0,
                                                                (p + q) * m,
                                                                (p + q) * m);
    matrix[(p + q) * m, (p + q) * m] companion_var = rep_matrix(0.0,
                                                                (p + q) * m,
                                                                (p + q) * m);
    matrix[(p + q) * m * (p + q) * m, (p + q) * m * (p + q) * m] tmp;
    matrix[(p + q) * m, (p + q) * m] Omega;
    for (i in 1 : p) {
      companion_mat[1 : m, ((i - 1) * m + 1) : (i * m)] = phi[i];
      if (i > 1) {
        for (j in 1 : m) {
          companion_mat[(i - 1) * m + j, (i - 2) * m + j] = 1.0;
        }
      }
    }
    for (i in 1 : q) {
      companion_mat[1 : m, ((p + i - 1) * m + 1) : ((p + i) * m)] = theta[i];
    }
    if (q > 1) {
      for (i in 2 : q) {
        for (j in 1 : m) {
          companion_mat[(p + i - 1) * m + j, (p + i - 2) * m + j] = 1.0;
        }
      }
    }
    companion_var[1 : m, 1 : m] = Sigma;
    companion_var[(p * m + 1) : ((p + 1) * m), (p * m + 1) : ((p + 1) * m)] = Sigma;
    companion_var[1 : m, (p * m + 1) : ((p + 1) * m)] = Sigma;
    companion_var[(p * m + 1) : ((p + 1) * m), 1 : m] = Sigma;
    tmp = diag_matrix(rep_vector(1.0, (p + q) * m * (p + q) * m))
          - kronecker_prod(companion_mat, companion_mat);
    Omega = to_matrix(tmp \ to_vector(companion_var), (p + q) * m,
                      (p + q) * m);
    for (i in 1 : (rows(Omega) - 1)) {
      for (j in (i + 1) : rows(Omega)) {
        Omega[j, i] = Omega[i, j];
      }
    }
    return Omega;
  }
}
data {
  int<lower=1> N;
  int<lower=1> N_count;
  vector[N_count] Y_count;
  int Ks_count;
  matrix[N_count, Ks_count] Xs_count;
  int nb_count_1;
  array[nb_count_1] int knots_count_1;
  matrix[N_count, knots_count_1[1]] Zs_count_1_1;
  int<lower=1> N_biomass;
  vector[N_biomass] Y_biomass;
  int Ks_biomass;
  matrix[N_biomass, Ks_biomass] Xs_biomass;
  int nb_biomass_1;
  array[nb_biomass_1] int knots_biomass_1;
  matrix[N_biomass, knots_biomass_1[1]] Zs_biomass_1_1;
  int prior_only;
  int<lower=1> N_trend;
  int<lower=1> N_series_trend;
  int<lower=1> N_lv_trend;
  array[N_trend, N_series_trend] int times_trend;
  int<lower=1> K_trend;
  matrix[N_trend, K_trend] X_trend;
  array[N_count] int obs_trend_time_count;
  array[N_count] int obs_trend_series_count;
  array[N_biomass] int obs_trend_time_biomass;
  array[N_biomass] int obs_trend_series_biomass;
  int<lower=1> N_lags_trend;
}
transformed data {
  // Factor loadings matrix: maps latent variables to observed series
  matrix[N_series_trend, N_lv_trend] Z = diag_matrix(rep_vector(1.0,
                                                                N_lv_trend));
  vector[N_lv_trend] trend_zeros = rep_vector(0.0, N_lv_trend);
}
parameters {
  real Intercept_count;
  vector[Ks_count] bs_count;
  vector[knots_count_1[1]] zs_count_1_1;
  vector<lower=0>[nb_count_1] sds_count_1;
  real<lower=0> sigma_count;
  real Intercept_biomass;
  vector[Ks_biomass] bs_biomass;
  vector[knots_biomass_1[1]] zs_biomass_1_1;
  vector<lower=0>[nb_biomass_1] sds_biomass_1;
  real<lower=0> sigma_biomass;
  // Latent variable trajectories over time
  matrix[N_trend, N_lv_trend] lv_trend;
  vector[K_trend] b_trend;
  array[2] matrix[N_lv_trend, N_lv_trend] A_raw_trend;
  vector<lower=0>[N_lv_trend] sigma_trend;
  cholesky_factor_corr[N_lv_trend] L_Omega_trend;
  array[2] vector[2] Amu_trend;
  array[2] vector<lower=0>[2] Aomega_trend;
  vector[(2 + 1) * N_lv_trend] init_trend;
  array[1] matrix[N_lv_trend, N_lv_trend] D_raw_trend;
  array[2] vector[1] Dmu_trend;
  array[2] vector<lower=0>[1] Domega_trend;
}
transformed parameters {
  vector[knots_count_1[1]] s_count_1_1;
  vector[knots_biomass_1[1]] s_biomass_1_1;
  // Prior log-probability accumulator
  real lprior = 0;
  lprior += student_t_lpdf(Intercept_count | 3, 3, 2.5);
  lprior += student_t_lpdf(sds_count_1 | 3, 0, 2.5)
            - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += student_t_lpdf(sigma_count | 3, 0, 2.5)
            - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += student_t_lpdf(Intercept_biomass | 3, 2.7, 2.5);
  lprior += student_t_lpdf(sds_biomass_1 | 3, 0, 2.5)
            - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += student_t_lpdf(sigma_biomass | 3, 0, 2.5)
            - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  vector[N_trend] mu_trend = rep_vector(0.0, N_trend);
  mu_trend += X_trend * b_trend;
  matrix[N_lv_trend, N_lv_trend] L_Sigma_trend = diag_pre_multiply(sigma_trend,
                                                                   L_Omega_trend);
  cov_matrix[N_lv_trend] Sigma_trend = multiply_lower_tri_self_transpose(L_Sigma_trend);
  array[2] matrix[N_lv_trend, N_lv_trend] A_trend;
  array[2] matrix[N_lv_trend, N_lv_trend] P_var;
  array[2, 2] matrix[N_lv_trend, N_lv_trend] result_var;
  for (i in 1 : 2) {
    P_var[i] = AtoP(A_raw_trend[i]);
  }
  result_var = rev_mapping(P_var, Sigma_trend);
  for (i in 1 : 2) {
    A_trend[i] = result_var[1, i];
  }
  array[1] matrix[N_lv_trend, N_lv_trend] D_trend;
  cov_matrix[(2 + 1) * N_lv_trend] Omega_trend;
  vector[N_lv_trend] ma_init_trend;
  array[1] matrix[N_lv_trend, N_lv_trend] P_ma;
  array[2, 1] matrix[N_lv_trend, N_lv_trend] result_ma;
  for (i in 1 : 1) {
    P_ma[i] = AtoP(D_raw_trend[i]);
  }
  result_ma = rev_mapping(P_ma, Sigma_trend);
  for (i in 1 : 1) {
    D_trend[i] = -result_ma[1, i];
  }
  Omega_trend = initial_joint_var(Sigma_trend, A_trend, D_trend);
  ma_init_trend = init_trend[(2 * N_lv_trend + 1) : (3 * N_lv_trend)];
  // Final trend values for each time point and series
  matrix[N_trend, N_series_trend] trend;
  // Map latent variables to trend values via factor loadings
  for (i in 1 : N_trend) {
    for (s in 1 : N_series_trend) {
      trend[i, s] = dot_product(Z[s,  : ], lv_trend[i,  : ])
                    + mu_trend[times_trend[i, s]];
    }
  }
  s_count_1_1 = sds_count_1[1] * zs_count_1_1;
  s_biomass_1_1 = sds_biomass_1[1] * zs_biomass_1_1;
}
model {
  vector[(2 + 1) * N_lv_trend] mu_init_trend = rep_vector(0.0,
                                                          (2 + 1)
                                                          * N_lv_trend);
  init_trend ~ multi_normal(mu_init_trend, Omega_trend);
  array[N_trend] vector[N_lv_trend] mu_t_trend;
  for (t in 1 : N_trend) {
    mu_t_trend[t] = rep_vector(0.0, N_lv_trend);
    for (i in 1 : 2) {
      if (t - i <= 0) {
        int init_idx = 2 + 1 - i;
        if (init_idx > 0 && init_idx <= 2) {
          vector[N_lv_trend] lagged_lv;
          int start_idx = (init_idx - 1) * N_lv_trend + 1;
          int end_idx = init_idx * N_lv_trend;
          lagged_lv = init_trend[start_idx : end_idx];
          mu_t_trend[t] += A_trend[i] * lagged_lv;
        }
      } else {
        mu_t_trend[t] += A_trend[i] * lv_trend[t - i,  : ]';
      }
    }
    if (t - 1 <= 0) {
      mu_t_trend[t] += D_trend[1] * ma_init_trend;
    } else {
      mu_t_trend[t] += D_trend[1]
                       * (lv_trend[t - 1,  : ]' - mu_t_trend[t - 1]);
    }
  }
  for (t in 1 : N_trend) {
    lv_trend[t,  : ]' ~ multi_normal(mu_t_trend[t], Sigma_trend);
  }
  for (lag in 1 : N_lags_trend) {
    diagonal(A_raw_trend[lag]) ~ normal(Amu_trend[1, lag],
                                        1 / sqrt(Aomega_trend[1, lag]));
    for (i in 1 : N_lv_trend) {
      for (j in 1 : N_lv_trend) {
        if (i != j) {
          A_raw_trend[lag, i, j] ~ normal(Amu_trend[2, lag],
                                          1 / sqrt(Aomega_trend[2, lag]));
        }
      }
    }
  }
  for (ma_lag in 1 : 1) {
    diagonal(D_raw_trend[ma_lag]) ~ normal(Dmu_trend[1, ma_lag],
                                           1 / sqrt(Domega_trend[1, ma_lag]));
    for (i in 1 : N_lv_trend) {
      for (j in 1 : N_lv_trend) {
        if (i != j) {
          D_raw_trend[ma_lag, i, j] ~ normal(Dmu_trend[2, ma_lag],
                                             1
                                             / sqrt(Domega_trend[2, ma_lag]));
        }
      }
    }
  }
  Dmu_trend[1, 1] ~ normal(0.0, 1.0);
  Domega_trend[1, 1] ~ gamma(2.0, 1.0);
  Dmu_trend[2, 1] ~ normal(0.0, 1.0);
  Domega_trend[2, 1] ~ gamma(2.0, 1.0);
  L_Omega_trend ~ lkj_corr_cholesky(2);
  for (lag in 1 : 2) {
    Amu_trend[lag] ~ normal(0, sqrt(0.455));
    Aomega_trend[lag] ~ gamma(1.365, 0.071175);
  }
  sigma_trend ~ exponential(2);
  
  // Observation linear predictors and likelihoods (skipped when sampling from prior only)
  if (!prior_only) {
    vector[N_count] mu_count = rep_vector(0.0, N_count);
    vector[N_biomass] mu_biomass = rep_vector(0.0, N_biomass);
    mu_count += Intercept_count + Xs_count * bs_count
                + Zs_count_1_1 * s_count_1_1;
    for (n in 1 : N_count) {
      mu_count[n] += trend[obs_trend_time_count[n], obs_trend_series_count[n]];
    }
    mu_biomass += Intercept_biomass + Xs_biomass * bs_biomass
                  + Zs_biomass_1_1 * s_biomass_1_1;
    for (n in 1 : N_biomass) {
      mu_biomass[n] += trend[obs_trend_time_biomass[n], obs_trend_series_biomass[n]];
    }
    // Likelihood calculations
    target += normal_lpdf(Y_count | mu_count, sigma_count);
    target += normal_lpdf(Y_biomass | mu_biomass, sigma_biomass);
  }
  
  // Prior contributions
  target += lprior;
  target += std_normal_lpdf(zs_count_1_1);
  target += std_normal_lpdf(zs_biomass_1_1);
}
generated quantities {
  real b_count_Intercept = Intercept_count;
  real b_biomass_Intercept = Intercept_biomass;
}

