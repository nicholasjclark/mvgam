// Expected final model for the following:
// make_stancode(
//  bf(mvbind(count, biomass) ~ s(x)) + set_rescor(FALSE),
//  trend_formula = ~ presence + VAR(p = 2, ma = TRUE),
//  data = data
// )

functions {
  /**
  * Compute matrix square root using eigendecomposition
  * Following Heaps 2022 methodology for stationary VAR/VARMA
  * @param A Symmetric positive definite matrix (m x m)
  * @return Matrix square root of A
  */
  matrix sqrtm(matrix A) {
    int m = rows(A);
    vector[m] eigenvals = eigenvalues_sym(A);

    if (min(eigenvals) <= 1e-12) {
      reject("Matrix must be positive definite for square root computation");
    }

    matrix[m, m] eigenvecs = eigenvectors_sym(A);
    matrix[m, m] eprod = diag_post_multiply(eigenvecs, sqrt(eigenvals));
    return tcrossprod(eprod);
  }

  /**
  * Transform P_real to P matrix using partial autocorrelation approach
  * Heaps 2022 transformation for stationarity constraints
  * @param P_real Real-valued unconstrained matrix
  * @return Constrained P matrix for stationary VAR coefficients
  */
  matrix AtoP(matrix P_real) {
    int m = rows(P_real);
    matrix[m, m] B = tcrossprod(P_real);
    for (i in 1:m) {
      B[i, i] += 1.0;
    }
    return mdivide_left_spd(sqrtm(B), P_real);
  }

  /**
  * Perform reverse mapping from partial autocorrelations to stationary coefficients
  * Heaps 2022 Algorithm for computing phi coefficients from P matrices
  * @param P Array of partial autocorrelation matrices
  * @param Sigma Innovation covariance matrix
  * @return Array containing phi coefficients and Gamma matrices [2, p]
  */
  array[,] matrix rev_mapping(array[] matrix P, matrix Sigma) {
    int p = size(P);
    int m = rows(Sigma);

    if (p == 0) {
      array[2, 0] matrix[m, m] empty_result;
      return empty_result;
    }

    array[p, p] matrix[m, m] phi_for;
    array[p, p] matrix[m, m] phi_rev;
    array[p + 1] matrix[m, m] Sigma_for;
    array[p + 1] matrix[m, m] Sigma_rev;
    array[2, p] matrix[m, m] phiGamma;

    Sigma_for[1] = Sigma;
    Sigma_rev[p + 1] = Sigma;

    for (k in 1:p) {
      phi_for[k, k] = P[k];
      Sigma_for[k + 1] = Sigma_for[k] - quad_form_sym(Sigma_for[k], P[k]');

      for (j in 1:(k-1)) {
        phi_for[j, k] = phi_for[j, k-1] - phi_for[k, k] * phi_for[k-j, k-1];
      }
    }

    for (k in p:1) {
      phi_rev[k, k] = P[k];
      Sigma_rev[k] = Sigma_rev[k + 1] - quad_form_sym(Sigma_rev[k + 1], P[k]');

      for (j in 1:(k-1)) {
        phi_rev[j, k] = phi_rev[j, k+1] - phi_rev[k, k] * phi_rev[k-j, k+1];
      }
    }

    for (i in 1:p) {
      phiGamma[1, i] = phi_for[i, p];
      phiGamma[2, i] = Sigma_for[i + 1];
    }

    return phiGamma;
  }

  /* Function to compute Kronecker product */
  matrix kronecker_prod(matrix A, matrix B) {
    matrix[rows(A) * rows(B), cols(A) * cols(B)] C;
    int m = rows(A);
    int n = cols(A);
    int p = rows(B);
    int q = cols(B);
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

  /**
  * Compute joint stationary covariance for VARMA(p,q) initialization
  * Heaps 2022 companion matrix approach for stationary distribution
  * @param Sigma Innovation covariance matrix (m x m)
  * @param phi Array of stationary VAR coefficient matrices
  * @param theta Array of stationary MA coefficient matrices
  * @return Joint covariance matrix Omega for initial distribution
  */
  matrix initial_joint_var(matrix Sigma, array[] matrix phi, array[] matrix theta) {
    int p = size(phi);
    int q = size(theta);
    int m = rows(Sigma);
    matrix[(p + q) * m, (p + q) * m] companion_mat = rep_matrix(0.0, (p + q) * m,
    (p + q) * m);
    matrix[(p + q) * m, (p + q) * m] companion_var = rep_matrix(0.0, (p + q) * m,
    (p + q) * m);
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
    tmp = diag_matrix(rep_vector(1.0, (p + q) * m * (p + q) * m)) - kronecker_prod(companion_mat, companion_mat);
    Omega = to_matrix(tmp \ to_vector(companion_var), (p + q) * m, (p + q) * m);

    for (i in 1:(rows(Omega) - 1)) {
      for (j in (i + 1):rows(Omega)) {
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
  int<lower=1> K_trend;
  int<lower=1> Kc_trend;
  matrix[N_trend, K_trend] X_trend;
  matrix[N_trend, Kc_trend] Xc_trend;
  vector[Kc_trend] means_X_trend;
  array[N_count] int obs_trend_time_count;
  array[N_count] int obs_trend_series_count;
  array[N_biomass] int obs_trend_time_biomass;
  array[N_biomass] int obs_trend_series_biomass;
  array[N_trend, N_series_trend] int times_trend;
}
transformed data {
  matrix[N_trend, Kc_trend] Xc_trend;
  vector[Kc_trend] means_X_trend;
  for (i in 2:K_trend) {
    means_X_trend[i - 1] = mean(X_trend[, i]);
    Xc_trend[, i - 1] = X_trend[, i] - means_X_trend[i - 1];
  }

  vector[N_lv_trend] trend_zeros = rep_vector(0.0, N_lv_trend);
  matrix[N_series_trend, N_lv_trend] Z = diag_matrix(rep_vector(1.0, N_lv_trend));
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
  real Intercept_trend;
  vector[Kc_trend] b_trend;
  array[2] matrix[N_lv_trend, N_lv_trend] A_raw_trend;
  array[1] matrix[N_lv_trend, N_lv_trend] D_raw_trend;
  array[2] vector[2] Amu_trend;
  array[2] vector<lower=0>[2] Aomega_trend;
  array[2] vector[1] Dmu_trend;
  array[2] vector<lower=0>[1] Domega_trend;
  vector<lower=0>[N_lv_trend] sigma_trend;
  cholesky_factor_corr[N_lv_trend] L_Omega_trend;
  vector[3 * N_lv_trend] init_trend;
  matrix[N_trend, N_lv_trend] lv_trend;
}
transformed parameters {
  vector[knots_count_1[1]] s_count_1_1;
  vector[knots_biomass_1[1]] s_biomass_1_1;
  real lprior = 0;
  s_count_1_1 = sds_count_1[1] * zs_count_1_1;
  s_biomass_1_1 = sds_biomass_1[1] * zs_biomass_1_1;
  lprior += student_t_lpdf(Intercept_count | 3, 4, 2.5);
  lprior += student_t_lpdf(sds_count_1 | 3, 0, 2.5)
  - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += student_t_lpdf(sigma_count | 3, 0, 2.5)
  - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += student_t_lpdf(Intercept_biomass | 3, 2.4, 2.5);
  lprior += student_t_lpdf(sds_biomass_1 | 3, 0, 2.5)
  - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += student_t_lpdf(sigma_biomass | 3, 0, 2.5)
  - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += student_t_lpdf(Intercept_trend | 3, 0, 2.5);
  vector[N_trend] mu_trend = rep_vector(0.0, N_trend);
  mu_trend += Intercept_trend + Xc_trend * b_trend;
  matrix[N_lv_trend, N_lv_trend] L_Sigma_trend = diag_pre_multiply(sigma_trend, L_Omega_trend);
  cov_matrix[N_lv_trend] Sigma_trend = multiply_lower_tri_self_transpose(L_Sigma_trend);
  array[2] matrix[N_lv_trend, N_lv_trend] A_trend;
  array[1] matrix[N_lv_trend, N_lv_trend] D_trend;
  array[2] matrix[N_lv_trend, N_lv_trend] P_var;
  array[2, 2] matrix[N_lv_trend, N_lv_trend] result_var;

  for (i in 1:2) {
    P_var[i] = AtoP(A_raw_trend[i]);
  }

  result_var = rev_mapping(P_var, Sigma_trend);

  for (i in 1:2) {
    A_trend[i] = result_var[1, i];
  }

  array[1] matrix[N_lv_trend, N_lv_trend] P_ma;
  array[2, 1] matrix[N_lv_trend, N_lv_trend] result_ma;
  P_ma[1] = AtoP(D_raw_trend[1]);
  result_ma = rev_mapping(P_ma, Sigma_trend);
  D_trend[1] = -result_ma[1, 1];
  cov_matrix[3 * N_lv_trend] Omega_trend = initial_joint_var(Sigma_trend, A_trend, D_trend);
  vector[N_lv_trend] ma_init_trend = init_trend[(2 * N_lv_trend + 1):(3 * N_lv_trend)];
  matrix[N_trend, N_series_trend] trend;

  for (i in 1:N_trend) {
    for (s in 1:N_series_trend) {
      trend[i, s] = dot_product(Z[s, :], lv_trend[i, :]) +
      mu_trend[times_trend[i, s]];
    }
  }

  vector[N_count] mu_count = rep_vector(0.0, N_count);
  vector[N_biomass] mu_biomass = rep_vector(0.0, N_biomass);
  mu_count += Intercept_count + Xs_count * bs_count + Zs_count_1_1 * s_count_1_1;
  mu_biomass += Intercept_biomass + Xs_biomass * bs_biomass + Zs_biomass_1_1 * s_biomass_1_1;

  for (n in 1:N_count) {
    mu_count[n] += trend[obs_trend_time_count[n], obs_trend_series_count[n]];
  }

  for (n in 1:N_biomass) {
    mu_biomass[n] += trend[obs_trend_time_biomass[n], obs_trend_series_biomass[n]];
  }
}
model {
  if (!prior_only) {
    target += normal_lpdf(Y_count | mu_count, sigma_count);
    target += normal_lpdf(Y_biomass | mu_biomass, sigma_biomass);
  }

  target += lprior;
  target += std_normal_lpdf(zs_count_1_1);
  target += std_normal_lpdf(zs_biomass_1_1);
  vector[3 * N_lv_trend] mu_init_trend = rep_vector(0.0, 3 * N_lv_trend);
  init_trend ~ multi_normal(mu_init_trend, Omega_trend);
  vector[N_lv_trend] mu_t_trend[N_trend];
  vector[N_lv_trend] ma_error_trend[N_trend];

  for (t in 1:N_trend) {
    mu_t_trend[t] = rep_vector(0.0, N_lv_trend);

    for (i in 1:2) {
      if (t - i <= 0) {

        int init_idx = 2 - (t - i) + 1;
        if (init_idx > 0 && init_idx <= 2) {
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

    if (t - 1 <= 0) {
      mu_t_trend[t] += D_trend[1] * ma_init_trend;
    } else {
      mu_t_trend[t] += D_trend[1] * ma_error_trend[t - 1];
    }
  }

  for (t in 1:N_trend) {
    lv_trend[t, :]' ~ multi_normal(mu_t_trend[t], Sigma_trend);
    ma_error_trend[t] = lv_trend[t, :]' - mu_t_trend[t];
  }

  for (lag in 1:2) {
    Amu_trend[1, lag] ~ normal(0.0, 1.0);
    Aomega_trend[1, lag] ~ gamma(2.0, 1.0);
    Amu_trend[2, lag] ~ normal(0.0, 1.0);
    Aomega_trend[2, lag] ~ gamma(2.0, 1.0);
  }

  Dmu_trend[1, 1] ~ normal(0.0, 1.0);
  Domega_trend[1, 1] ~ gamma(2.0, 1.0);
  Dmu_trend[2, 1] ~ normal(0.0, 1.0);
  Domega_trend[2, 1] ~ gamma(2.0, 1.0);
  b_trend ~ normal(0, 1);
  sigma_trend ~ exponential(2);
  L_Omega_trend ~ lkj_corr_cholesky(2);

  for (i in 1:2) {
    for (j in 1:N_lv_trend) {
      for (k in 1:N_lv_trend) {
        if (j == k) {

          A_raw_trend[i, j, k] ~ normal(Amu_trend[1, i], inv_sqrt(Aomega_trend[1, i]));
        } else {

          A_raw_trend[i, j, k] ~ normal(Amu_trend[2, i], inv_sqrt(Aomega_trend[2, i]));
        }
      }
    }
  }

  for (j in 1:N_lv_trend) {
    for (k in 1:N_lv_trend) {
      if (j == k) {
        D_raw_trend[1, j, k] ~ normal(Dmu_trend[1, 1], inv_sqrt(Domega_trend[1, 1]));
      } else {
        D_raw_trend[1, j, k] ~ normal(Dmu_trend[2, 1], inv_sqrt(Domega_trend[2, 1]));
      }
    }
  }
}
generated quantities {
  real b_count_Intercept = Intercept_count;
  real b_biomass_Intercept = Intercept_biomass;
  real b_trend_Intercept = Intercept_trend - dot_product(means_X_trend, b_trend);
}
