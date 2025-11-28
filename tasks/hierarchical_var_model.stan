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
        for (i in 1:m) {
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
        for (i in 1:m) {
          for (j in 1:n) {
            int row_start = (i - 1) * p + 1;
            int row_end = (i - 1) * p + p;
            int col_start = (j - 1) * q + 1;
            int col_end = (j - 1) * q + q;
            C[row_start:row_end, col_start:col_end] = A[i, j] * B;
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
        for (s in 1:p) {
          S_for = -tcrossprod(P[p - s + 1]);
          for (i in 1:m) {
            S_for[i, i] += 1.0;
          }
          S_rev = sqrtm(S_for);
          S_for_list[p - s + 1] = mdivide_right_spd(
            mdivide_left_spd(S_rev, sqrtm(quad_form_sym(Sigma_for[p - s + 2], S_rev))),
            S_rev
          );
          Sigma_for[p - s + 1] = tcrossprod(S_for_list[p - s + 1]);
        }
        Sigma_rev[1] = Sigma_for[1];
        Gamma_trans[1] = Sigma_for[1];
        for (s in 0:(p - 1)) {
          S_for = S_for_list[s + 1];
          S_rev = sqrtm(Sigma_rev[s + 1]);
          phi_for[s + 1, s + 1] = mdivide_right_spd(S_for * P[s + 1], S_rev);
          phi_rev[s + 1, s + 1] = mdivide_right_spd(S_rev * P[s + 1]', S_for);
          Gamma_trans[s + 2] = phi_for[s + 1, s + 1] * Sigma_rev[s + 1];
          if (s >= 1) {
            for (k in 1:s) {
              phi_for[s + 1, k] = phi_for[s, k] -
                                  phi_for[s + 1, s + 1] * phi_rev[s, s - k + 1];
              phi_rev[s + 1, k] = phi_rev[s, k] -
                                  phi_rev[s + 1, s + 1] * phi_for[s, s - k + 1];
            }
            for (k in 1:s) {
              Gamma_trans[s + 2] = Gamma_trans[s + 2] +
                                   phi_for[s, k] * Gamma_trans[s + 2 - k];
            }
          }
          Sigma_rev[s + 2] = Sigma_rev[s + 1] -
                             quad_form_sym(Sigma_for[s + 1], phi_rev[s + 1, s + 1]');
        }
        for (i in 1:p) {
          phiGamma[1, i] = phi_for[p, i];
          phiGamma[2, i] = Gamma_trans[i]';
        }
        return phiGamma;
      }
      matrix initial_joint_var(matrix Sigma, array[] matrix phi, array[] matrix theta) {
        int p = size(phi);
        int q = size(theta);
        int m = rows(Sigma);
        matrix[(p + q) * m, (p + q) * m] companion_mat = rep_matrix(0.0, (p + q) * m, (p + q) * m);
        matrix[(p + q) * m, (p + q) * m] companion_var = rep_matrix(0.0, (p + q) * m, (p + q) * m);
        matrix[(p + q) * m * (p + q) * m, (p + q) * m * (p + q) * m] tmp;
        matrix[(p + q) * m, (p + q) * m] Omega;
        for (i in 1:p) {
          companion_mat[1:m, ((i - 1) * m + 1):(i * m)] = phi[i];
          if (i > 1) {
            for (j in 1:m) {
              companion_mat[(i - 1) * m + j, (i - 2) * m + j] = 1.0;
            }
          }
        }
        for (i in 1:q) {
          companion_mat[1:m, ((p + i - 1) * m + 1):((p + i) * m)] = theta[i];
        }
        if (q > 1) {
          for (i in 2:q) {
            for (j in 1:m) {
              companion_mat[(p + i - 1) * m + j, (p + i - 2) * m + j] = 1.0;
            }
          }
        }
        companion_var[1:m, 1:m] = Sigma;
        companion_var[(p * m + 1):((p + 1) * m), (p * m + 1):((p + 1) * m)] = Sigma;
        companion_var[1:m, (p * m + 1):((p + 1) * m)] = Sigma;
        companion_var[(p * m + 1):((p + 1) * m), 1:m] = Sigma;
        tmp = diag_matrix(rep_vector(1.0, (p + q) * m * (p + q) * m)) -
              kronecker_prod(companion_mat, companion_mat);
        Omega = to_matrix(tmp \ to_vector(companion_var), (p + q) * m, (p + q) * m);
        for (i in 1:(rows(Omega) - 1)) {
          for (j in (i + 1):rows(Omega)) {
            Omega[j, i] = Omega[i, j];
          }
        }
        return Omega;
      }
      matrix combine_cholesky(matrix global_chol_cor, matrix local_chol_cor,                               real alpha) {
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
  matrix[N_series_trend, N_lv_trend] Z = diag_matrix(rep_vector(1.0, N_lv_trend));
    vector[N_lv_trend] trend_zeros = rep_vector(0.0, N_lv_trend);
int<lower=0> n_groups_trend = 2;
int<lower=0> n_subgroups_trend = 3;
array[2, 3] int<lower=1> group_inds_trend;
{
  group_inds_trend[1] = {1, 2, 3};
        group_inds_trend[2] = {4, 5, 6};
}
  for (i in 2:K) {
    means_X[i - 1] = mean(X[, i]);
    Xc[, i - 1] = X[, i] - means_X[i - 1];
  }
}
parameters {
  vector[Kc] b;
  real Intercept;
  // Latent variable trajectories over time
  matrix[N_trend, N_lv_trend] lv_trend;
  real Intercept_trend;
      array[2, 1] matrix[3, 3] A_raw_group_trend;
      array[2] vector<lower=0>[3] sigma_group_trend;
  array[2] vector[1] Amu_trend;
  array[2] vector<lower=0>[1] Aomega_trend;
  vector[1 * N_lv_trend] init_trend;
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
      array[2] cov_matrix[3] Sigma_group_trend;
      array[2, 1] matrix[3, 3] A_group_trend;
      for (g in 1:2) {
        matrix[3, 3] L_Omega_group_trend =
          cholesky_compose(alpha_cor_trend * multiply_lower_tri_self_transpose(L_Omega_global_trend) +
                           (1 - alpha_cor_trend) * multiply_lower_tri_self_transpose(L_deviation_group_trend[g]));
        Sigma_group_trend[g] = multiply_lower_tri_self_transpose(
          diag_pre_multiply(sigma_group_trend[g], L_Omega_group_trend));
        for (lag in 1:1) {
          array[1] matrix[3, 3] P_group;
          P_group[1] = AtoP(A_raw_group_trend[g, lag]);
          array[2, 1] matrix[3, 3] result_group =
            rev_mapping(P_group, Sigma_group_trend[g]);
          A_group_trend[g, lag] = result_group[1, 1];
        }
      }
      cov_matrix[N_lv_trend] Sigma_trend = rep_matrix(0, N_lv_trend, N_lv_trend);
      array[1] matrix[N_lv_trend, N_lv_trend] A_trend;
      for (lag in 1:1) {
        A_trend[lag] = rep_matrix(0, N_lv_trend, N_lv_trend);
      }
      for (g in 1:2) {
        Sigma_trend[group_inds_trend[g], group_inds_trend[g]] = Sigma_group_trend[g];
        for (lag in 1:1) {
          A_trend[lag][group_inds_trend[g], group_inds_trend[g]] = A_group_trend[g, lag];
        }
      }
  cov_matrix[1 * N_lv_trend] Omega_trend;
  array[1] matrix[N_lv_trend, N_lv_trend] empty_theta; empty_theta[1] = rep_matrix(0.0, N_lv_trend, N_lv_trend); Omega_trend = initial_joint_var(Sigma_trend, A_trend, empty_theta[1:0]);
  // Final trend values for each time point and series
  matrix[N_trend, N_series_trend] trend;
  // Map latent variables to trend values via factor loadings
  for (i in 1:N_trend) {
    for (s in 1:N_series_trend) {
      trend[i, s] = dot_product(Z[s, :], lv_trend[i, :]) + mu_trend[times_trend[i, s]];
    }
  }
}
model {
  vector[1 * N_lv_trend] mu_init_trend = rep_vector(0.0, 1 * N_lv_trend);
  init_trend ~ multi_normal(mu_init_trend, Omega_trend);
  vector[N_lv_trend] mu_t_trend[N_trend];
  for (t in 1:N_trend) {
    mu_t_trend[t] = rep_vector(0.0, N_lv_trend);
    for (i in 1:1) {
      if (t - i <= 0) {
        int init_idx = 1 + 1 - i;
        if (init_idx > 0 && init_idx <= 1) {
          vector[N_lv_trend] lagged_lv;
          int start_idx = (init_idx - 1) * N_lv_trend + 1;
          int end_idx = init_idx * N_lv_trend;
          lagged_lv = init_trend[start_idx:end_idx];
          mu_t_trend[t] += A_trend[i] * lagged_lv;
        }
      } else {
        mu_t_trend[t] += A_trend[i] * lv_trend[t - i, :]';
      }
    }
  }
  for (t in 1:N_trend) {
    lv_trend[t, :]' ~ multi_normal(mu_t_trend[t], Sigma_trend);
  }
      for (g in 1:2) {
        for (lag in 1:1) {
          diagonal(A_raw_group_trend[g, lag]) ~ normal(Amu_trend[1, lag], 1 / sqrt(Aomega_trend[1, lag]));
          for (i in 1:3) {
            for (j in 1:3) {
              if (i != j) {
                A_raw_group_trend[g, lag, i, j] ~ normal(Amu_trend[2, lag], 1 / sqrt(Aomega_trend[2, lag]));
              }
            }
          }
        }
      }
  L_Omega_trend ~ lkj_corr_cholesky(2);
  for (lag in 1:2) {
    Amu_trend[lag] ~ normal(0, sqrt(0.455));
    Aomega_trend[lag] ~ gamma(1.365, 0.071175);
  }
  alpha_cor_trend ~ beta(3, 2);
  L_Omega_global_trend ~ lkj_corr_cholesky(1);
  for (g in 1:n_groups_trend) { L_deviation_group_trend[g] ~ lkj_corr_cholesky(6); }
  sigma_trend ~ exponential(2);

  // Observation linear predictors and likelihoods (skipped when sampling from prior only)
  if (!prior_only) {
  vector[N] mu;
  mu = rep_vector(0.0, N);
  mu += Xc * b;
  mu += Intercept;
  for (n in 1:N) {
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
