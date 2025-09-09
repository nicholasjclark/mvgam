// generated with brms 2.22.9
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

        // Numerical stability check for positive definiteness
        if (min(eigenvals) <= 1e-12) {
          reject("Matrix must be positive definite for square root computation");
        }

        vector[m] root_root_evals = sqrt(sqrt(eigenvals));
        matrix[m, m] evecs = eigenvectors_sym(A);
        matrix[m, m] eprod = diag_post_multiply(evecs, root_root_evals);
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
       * Compute Kronecker product of two matrices
       * Used in companion matrix approach for VARMA initialization
       * @param A First matrix (m x n)
       * @param B Second matrix (p x q)
       * @return Kronecker product A ⊗ B (mp x nq)
       */
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

      /**
       * Perform reverse mapping from partial autocorrelations to stationary coefficients
       * Heaps 2022 Algorithm for computing phi coefficients from P matrices
       * @param P Array of partial autocorrelation matrices (modern syntax)
       * @param Sigma Innovation covariance matrix
       * @return Array containing phi coefficients and Gamma matrices [2, p]
       */
      array[,] matrix[,] rev_mapping(array[] matrix[,] P, matrix Sigma) {
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

        // Step 1: Forward pass - compute Sigma_for and S_for_list
        Sigma_for[p + 1] = Sigma;
        S_for_list[p + 1] = sqrtm(Sigma);
        for (s in 1:p) {
          // Compute working matrices (S_rev is B^{-1}, S_for is working matrix)
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

        // Step 2: Reverse pass - compute phi coefficients and Gamma matrices
        Sigma_rev[1] = Sigma_for[1];
        Gamma_trans[1] = Sigma_for[1];
        for (s in 0:(p - 1)) {
          S_for = S_for_list[s + 1];
          S_rev = sqrtm(Sigma_rev[s + 1]);
          phi_for[s + 1, s + 1] = mdivide_right_spd(S_for * P[s + 1], S_rev);
          phi_rev[s + 1, s + 1] = mdivide_right_spd(S_rev * P[s + 1]', S_for);
          Gamma_trans[s + 2] = phi_for[s + 1, s + 1] * Sigma_rev[s + 1];

          if (s >= 1) {
            // Update phi coefficients using recursive relations
            for (k in 1:s) {
              phi_for[s + 1, k] = phi_for[s, k] -
                                  phi_for[s + 1, s + 1] * phi_rev[s, s - k + 1];
              phi_rev[s + 1, k] = phi_rev[s, k] -
                                  phi_rev[s + 1, s + 1] * phi_for[s, s - k + 1];
            }
            // Update Gamma_trans using phi coefficients
            for (k in 1:s) {
              Gamma_trans[s + 2] = Gamma_trans[s + 2] +
                                   phi_for[s, k] * Gamma_trans[s + 2 - k];
            }
          }
          Sigma_rev[s + 2] = Sigma_rev[s + 1] -
                             quad_form_sym(Sigma_for[s + 1], phi_rev[s + 1, s + 1]');
        }

        // Pack results: phi coefficients in row 1, Gamma matrices in row 2
        for (i in 1:p) {
          phiGamma[1, i] = phi_for[p, i];
          phiGamma[2, i] = Gamma_trans[i]';
        }
        return phiGamma;
      }

      /**
       * Compute joint stationary covariance for VARMA(p,q) initialization
       * Heaps 2022 companion matrix approach for stationary distribution
       * @param Sigma Innovation covariance matrix (m x m)
       * @param phi Array of stationary VAR coefficient matrices
       * @param theta Array of stationary MA coefficient matrices
       * @return Joint covariance matrix Omega for (y_0,...,y_{1-p},eps_0,...,eps_{1-q})
       */
      matrix initial_joint_var(matrix Sigma, array[] matrix[,] phi, array[] matrix[,] theta) {
        int p = size(phi);
        int q = size(theta);
        int m = rows(Sigma);
        matrix[(p + q) * m, (p + q) * m] companion_mat = rep_matrix(0.0, (p + q) * m, (p + q) * m);
        matrix[(p + q) * m, (p + q) * m] companion_var = rep_matrix(0.0, (p + q) * m, (p + q) * m);
        matrix[(p + q) * m * (p + q) * m, (p + q) * m * (p + q) * m] tmp;
        matrix[(p + q) * m, (p + q) * m] Omega;

        // Construct companion matrix (phi_tilde) following Heaps 2022
        // VAR component: phi matrices in top-left block
        for (i in 1:p) {
          companion_mat[1:m, ((i - 1) * m + 1):(i * m)] = phi[i];
          if (i > 1) {
            // Identity blocks for VAR lags
            for (j in 1:m) {
              companion_mat[(i - 1) * m + j, (i - 2) * m + j] = 1.0;
            }
          }
        }

        // MA component: theta matrices in top-right block
        for (i in 1:q) {
          companion_mat[1:m, ((p + i - 1) * m + 1):((p + i) * m)] = theta[i];
        }

        // Identity blocks for MA lags (if q > 1)
        if (q > 1) {
          for (i in 2:q) {
            for (j in 1:m) {
              companion_mat[(p + i - 1) * m + j, (p + i - 2) * m + j] = 1.0;
            }
          }
        }

        // Construct innovation covariance matrix (Sigma_tilde)
        // Innovations affect y_t and eps_t simultaneously
        companion_var[1:m, 1:m] = Sigma;  // For y_t innovations
        companion_var[(p * m + 1):((p + 1) * m), (p * m + 1):((p + 1) * m)] = Sigma;  // For eps_t innovations
        companion_var[1:m, (p * m + 1):((p + 1) * m)] = Sigma;  // Cross-covariance
        companion_var[(p * m + 1):((p + 1) * m), 1:m] = Sigma;  // Symmetric cross-covariance

        // Solve Lyapunov equation: Omega = Sigma_tilde + Phi_tilde * Omega * Phi_tilde'
        // Vectorized form: vec(Omega) = (I - Phi_tilde ⊗ Phi_tilde)^{-1} vec(Sigma_tilde)
        tmp = diag_matrix(rep_vector(1.0, (p + q) * m * (p + q) * m)) -
              kronecker_prod(companion_mat, companion_mat);

        Omega = to_matrix(tmp \ to_vector(companion_var), (p + q) * m, (p + q) * m);

        // Ensure numerical symmetry of result
        for (i in 1:(rows(Omega) - 1)) {
          for (j in (i + 1):rows(Omega)) {
            Omega[j, i] = Omega[i, j];
          }
        }

        return Omega;
      }
    
}
data {
  int<lower=1> N;  // total number of observations
  int<lower=1> N_count;  // number of observations
  vector[N_count] Y_count;  // response variable
  // data for splines
  int Ks_count;  // number of linear effects
  matrix[N_count, Ks_count] Xs_count;  // design matrix for the linear effects
  // data for spline 1
  int nb_count_1;  // number of bases
  array[nb_count_1] int knots_count_1;  // number of knots
  // basis function matrices
  matrix[N_count, knots_count_1[1]] Zs_count_1_1;
  int<lower=1> N_biomass;  // number of observations
  vector[N_biomass] Y_biomass;  // response variable
  // data for splines
  int Ks_biomass;  // number of linear effects
  matrix[N_biomass, Ks_biomass] Xs_biomass;  // design matrix for the linear effects
  // data for spline 1
  int nb_biomass_1;  // number of bases
  array[nb_biomass_1] int knots_biomass_1;  // number of knots
  // basis function matrices
  matrix[N_biomass, knots_biomass_1[1]] Zs_biomass_1_1;
  int prior_only;  // should the likelihood be ignored?
    int<lower=1> N_trend;  // total number of_trend observations
  int<lower=1> N_series_trend;
  int<lower=1> N_lv_trend;
  array[N_trend, N_series_trend] int times_trend;
  array[N_count] int obs_trend_time_count;
  array[N_count] int obs_trend_series_count;
  array[N_biomass] int obs_trend_time_biomass;
  array[N_biomass] int obs_trend_series_biomass;
    int<lower=1> K_trend;  // number of_trend population-level effects
    int<lower=1> Kc_trend;  // number of_trend population-level effects after centering
    matrix[N_trend, K_trend] X_trend;  // population-level design matrix
}
transformed data {
    matrix[N_trend, Kc_trend] Xc_trend;  // centered version_trend of_trend X_trend without_trend an_trend intercept
  vector[Kc_trend] means_X_trend;  // column_trend means_trend of_trend X_trend before_trend centering
  for (i_trend in 2:K_trend) {
    means_X_trend[i_trend - 1] = mean(X_trend[, i_trend]);
    Xc_trend[, i_trend - 1] = X_trend[, i_trend] - means_X_trend[i_trend - 1];
    // Zero mean vector for VARMA process (following Heaps 2022)
  vector[1] trend_zeros = rep_vector(0.0, 1);

  

  // Hyperparameter constants for hierarchical priors (as constants, not user inputs)
  // For A_trend coefficient matrices: diagonal and off-diagonal elements
  array[2] vector[2] es_trend = {
    {0.0, 0.0},    // Means for diagonal elements [lag 1, lag 2, ...]
    {0.0, 0.0}     // Means for off-diagonal elements [lag 1, lag 2, ...]
  };
  array[2] vector[2] fs_trend = {
    {1.0, 1.0},    // Standard deviations for diagonal elements
    {1.0, 1.0}     // Standard deviations for off-diagonal elements
  };
  array[2] vector[2] gs_trend = {
    {2.0, 2.0},    // Gamma shape parameters for diagonal precision
    {2.0, 2.0}     // Gamma shape parameters for off-diagonal precision
  };
  array[2] vector[2] hs_trend = {
    {1.0, 1.0},    // Gamma rate parameters for diagonal precision
    {1.0, 1.0}     // Gamma rate parameters for off-diagonal precision
  };

  
  // Additional hyperparameters for D_trend (MA coefficient matrices) when ma_lags > 0
  array[2] vector[2] es_ma_trend = {{
    {{0.0, 0.0}},    // Means for MA diagonal elements
    {{0.0, 0.0}}     // Means for MA off-diagonal elements
  }};
  array[2] vector[2] fs_ma_trend = {{
    {{1.0, 1.0}},    // Standard deviations for MA diagonal elements
    {{1.0, 1.0}}     // Standard deviations for MA off-diagonal elements
  }};
  array[2] vector[2] gs_ma_trend = {{
    {{2.0, 2.0}},    // Gamma shape for MA diagonal precision
    {{2.0, 2.0}}     // Gamma shape for MA off-diagonal precision
  }};
  array[2] vector[2] hs_ma_trend = {{
    {{1.0, 1.0}},    // Gamma rate for MA diagonal precision
    {{1.0, 1.0}}     // Gamma rate for MA off-diagonal precision
  }};
  
}
parameters {
  real Intercept_count;  // temporary intercept for centered predictors
  vector[Ks_count] bs_count;  // unpenalized spline coefficients
  // parameters for spline 1
  // standardized penalized spline coefficients
  vector[knots_count_1[1]] zs_count_1_1;
  vector<lower=0>[nb_count_1] sds_count_1;  // SDs of penalized spline coefficients
  real<lower=0> sigma_count;  // dispersion parameter
  real Intercept_biomass;  // temporary intercept for centered predictors
  vector[Ks_biomass] bs_biomass;  // unpenalized spline coefficients
  // parameters for spline 1
  // standardized penalized spline coefficients
  vector[knots_biomass_1[1]] zs_biomass_1_1;
  vector<lower=0>[nb_biomass_1] sds_biomass_1;  // SDs of penalized spline coefficients
  real<lower=0> sigma_biomass;  // dispersion parameter
  vector[Kc_trend] b_trend;  // regression coefficients
real Intercept_trend;  // temporary intercept for centered predictors
  vector<lower=0>[N_lv_trend] sigma_trend;
  cholesky_factor_corr[N_lv_trend] L_Omega_trend;
  matrix[N_trend, N_lv_trend] innovations_trend;
  matrix[N_series_trend, N_lv_trend] Z;
    
  // Standard VAR: single raw matrix
  array[{lags}] matrix[N_lv_trend, N_lv_trend] A_raw_trend;

  // Standard variance and correlation parameters
  vector<lower=0>[N_lv_trend] sigma_trend;
  cholesky_factor_corr[N_lv_trend] L_Omega_trend;
  

  // Shared hierarchical hyperparameters for A_raw coefficients (across all groups)
  // [1] = diagonal elements, [2] = off-diagonal elements
  array[2] vector[1] Amu_trend;     // Shared means
  array[2] vector<lower=0>[1] Aomega_trend;  // Shared precisions

  // Joint initialization vector for stationary distribution
  vector[(1 + 1) * N_lv_trend] init_trend;

  // Standard latent variable trends - consistent with other trend generators
  matrix[N_trend, N_lv_trend] lv_trend;
  // Raw MA partial autocorrelation matrices (unconstrained for stationarity)
// D_raw_trend gets transformed to D_trend (stationary MA coefficients)
array[1] matrix[N_lv_trend, N_lv_trend] D_raw_trend;

// Hierarchical hyperparameters for D_raw_trend (MA) coefficients
// Mirrors A_raw_trend hyperparameter structure for consistency
// [1] = diagonal elements, [2] = off-diagonal elements
array[2] vector[1] Dmu_trend;           // Means for D_raw_trend elements
array[2] vector<lower=0>[1] Domega_trend;  // Precisions for D_raw_trend elements
  // Factor loading matrix (estimated for factor model)
vector[N_series_trend * N_lv_trend] Z_raw;  // raw factor loadings
}
transformed parameters {
  // penalized spline coefficients
  vector[knots_count_1[1]] s_count_1_1;
  // penalized spline coefficients
  vector[knots_biomass_1[1]] s_biomass_1_1;
  real lprior = 0;  // prior contributions to the log posterior
  lprior += student_t_lpdf(Intercept_trend | 3, -0.2, 2.5);
  vector[N_trend] mu_trend = rep_vector(Intercept_trend, N_trend);
  matrix[N_lv_trend, N_lv_trend] Sigma_trend = diag_pre_multiply(sigma_trend, L_Omega_trend);
  
    // Scaled innovations after applying correlations
    matrix[N_trend, N_lv_trend] scaled_innovations_trend;

    // Apply correlation transformation using efficient non-centered parameterization
    {
      matrix[N_lv_trend, N_lv_trend] L_Sigma = diag_pre_multiply(sigma_trend, L_Omega_trend);
      scaled_innovations_trend = innovations_trend * L_Sigma';
    }
    
  // Standard VAR: single covariance matrix and transformation
  matrix[N_lv_trend, N_lv_trend] L_Sigma_trend = diag_pre_multiply(sigma_trend, L_Omega_trend);
  cov_matrix[N_lv_trend] Sigma_trend = multiply_lower_tri_self_transpose(L_Sigma_trend);

  // Transform raw parameters to stationary coefficients
  array[{lags}] matrix[N_lv_trend, N_lv_trend] A_trend;

  // Working arrays for stationarity transformation
  array[{lags}] matrix[N_lv_trend, N_lv_trend] P_var;
  array[2, {lags}] matrix[N_lv_trend, N_lv_trend] result_var;

  for (i in 1:{lags}) {{
    P_var[i] = AtoP(A_raw_trend[i]);
  }}

  result_var = rev_mapping(P_var, Sigma_trend);

  for (i in 1:{lags}) {{
    A_trend[i] = result_var[1, i];
  }}
  

  array[1] matrix[N_lv_trend, N_lv_trend] D_trend;

  // Joint covariance matrix for stationary initialization
  cov_matrix[(1 + 1) * N_lv_trend] Omega_trend;

  vector[N_lv_trend] ma_init_trend[1];  // Initial MA errors

  // Working arrays for stationarity transformation
  array[1] matrix[N_lv_trend, N_lv_trend] P_var;
  array[2, 1] matrix[N_lv_trend, N_lv_trend] result_var;

  // Transform A_raw_trend to stationary A_trend using Heaps methodology
  for (i in 1:1) {
    P_var[i] = AtoP(A_raw_trend[i]);
  }

  // Apply reverse mapping to get stationary coefficients
  result_var = rev_mapping(P_var, Sigma_trend);

  // Extract stationary VAR coefficients (these become our final A_trend)
  for (i in 1:1) {
    A_trend[i] = result_var[1, i];
  }

  // Transform D_raw_trend to stationary D_trend (VARMA only)
array[1] matrix[N_lv_trend, N_lv_trend] P_ma;
array[2, 1] matrix[N_lv_trend, N_lv_trend] result_ma;

// Transform D_raw_trend matrices using AtoP transformation
for (i in 1:1) {
  P_ma[i] = AtoP(D_raw_trend[i]);
}

// Apply reverse mapping to get stationary MA coefficients
result_ma = rev_mapping(P_ma, Sigma_trend);

// Extract stationary MA coefficients (negative sign per Heaps 2022)
for (i in 1:1) {
  D_trend[i] = -result_ma[1, i];
}

  // Compute initial joint covariance matrix using companion matrix approach
  Omega_trend = initial_joint_var(Sigma_trend, A_trend, D_trend);

  // Initialize MA error terms from init_trend (VARMA specific)
for (i in 1:1) {
  int start_idx = 1 * N_lv_trend + (i - 1) * N_lv_trend + 1;
  int end_idx = 1 * N_lv_trend + i * N_lv_trend;
  ma_init_trend[i] = init_trend[start_idx:end_idx];
}
    // Derived latent trends using universal computation pattern
  matrix[N_trend, N_series_trend] trend;

  // Universal trend computation: state-space dynamics + linear predictors
  // dot_product captures dynamic component, mu_trend captures trend_formula
  for (i in 1:N_trend) {
    for (s in 1:N_series_trend) {
      trend[i, s] = dot_product(Z[s, :], lv_trend[i, :]) + mu_trend[times_trend[i, s]];
    }
  }
  // Factor loading matrix with identifiability constraints
  matrix[N_series_trend, N_lv_trend] Z = rep_matrix(0, N_series_trend, N_lv_trend);
  // constraints allow identifiability of loadings
  {
    int index = 1;
    for (j in 1 : N_lv_trend) {
      for (i in j : N_series_trend) {
        Z[i, j] = Z_raw[index];
        index += 1;
      }
    }
  }
  // compute penalized spline coefficients
  s_count_1_1 = sds_count_1[1] * zs_count_1_1;
  // compute penalized spline coefficients
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
}
model {
  // Shared Gaussian innovation priors
  sigma_trend ~ exponential(2);
  L_Omega_trend ~ lkj_corr_cholesky(2);
  to_vector(innovations_trend) ~ std_normal();
    // VARMA likelihood implementation following Heaps 2022 methodology

  // Initial joint distribution for stationary VARMA initialization
  vector[(1 + 1) * N_lv_trend] mu_init_trend = rep_vector(0.0, (1 + 1) * N_lv_trend);
  init_trend ~ multi_normal(mu_init_trend, Omega_trend);

  // Conditional means for VARMA dynamics
  vector[N_lv_trend] mu_t_trend[N_trend];
  vector[N_lv_trend] ma_error_trend[N_trend];  // MA error terms

  // Compute conditional means for all time points
  for (t in 1:N_trend) {
    mu_t_trend[t] = rep_vector(0.0, N_lv_trend);

    // VAR component: Add autoregressive terms
    for (i in 1:1) {
      if (t - i <= 0) {
        // Use values from earlier than series start (from init_trend)
        int init_idx = 1 - (t - i) + 1;
        if (init_idx > 0 && init_idx <= 1) {
          vector[N_lv_trend] lagged_lv;
          int start_idx = (init_idx - 1) * N_lv_trend + 1;
          int end_idx = init_idx * N_lv_trend;
          lagged_lv = init_trend[start_idx:end_idx];
          mu_t_trend[t] += A_trend[i] * lagged_lv;
        }
      } else {
        // Use regular lv_trend values
        mu_t_trend[t] += A_trend[i] * lv_trend[t - i, :];
      }
    }

    // MA component: Add moving average terms for VARMA
for (i in 1:1) {
  if (t - i <= 0) {
    // Use initial MA errors for early time points
    if (i <= 1) {
      mu_t_trend[t] += D_trend[i] * ma_init_trend[i];
    }
  } else {
    // Use computed MA errors from previous time points
    mu_t_trend[t] += D_trend[i] * ma_error_trend[t - i];
  }
}
  }

  // Observation likelihood for time series
  for (t in 1:N_trend) {
    lv_trend[t, :] ~ multi_normal(mu_t_trend[t], Sigma_trend);
    // Compute MA error for next iteration
    ma_error_trend[t] = lv_trend[t, :] - mu_t_trend[t];
  }

  
  // Standard VAR: single matrix priors
  for (lag in 1:{lags}) {{
    // Diagonal elements prior
    diagonal(A_raw_trend[lag]) ~ normal(Amu_trend[1, lag], 1 / sqrt(Aomega_trend[1, lag]));

    // Off-diagonal elements prior
    for (i in 1:N_lv_trend) {{
      for (j in 1:N_lv_trend) {{
        if (i != j) {{
          A_raw_trend[lag, i, j] ~ normal(Amu_trend[2, lag], 1 / sqrt(Aomega_trend[2, lag]));
        }}
      }}
    }}
  }}
  

  // Hierarchical priors for VARMA MA coefficient matrices (D_raw_trend)
// Following same structure as VAR coefficients but conditional on ma_lags > 0
for (ma_lag in 1:1) {
  // Diagonal elements prior for MA coefficients
  diagonal(D_raw_trend[ma_lag]) ~ normal(Dmu_trend[1, ma_lag], 1 / sqrt(Domega_trend[1, ma_lag]));

  // Off-diagonal elements prior for MA coefficients
  for (i in 1:N_lv_trend) {
    for (j in 1:N_lv_trend) {
      if (i != j) {
        D_raw_trend[ma_lag, i, j] ~ normal(Dmu_trend[2, ma_lag], 1 / sqrt(Domega_trend[2, ma_lag]));
      }
    }
  }
}

// Hyperpriors for hierarchical MA coefficient means and precisions
for (component in 1:2) {  // [1] diagonal, [2] off-diagonal
  Dmu_trend[component] ~ normal(es_ma_trend[component], fs_ma_trend[component]);
  Domega_trend[component] ~ gamma(gs_ma_trend[component], hs_ma_trend[component]);
}

  // Innovation variance and correlation priors (consistent with other trend generators)
  // Variance parameters using inverse gamma (equivalent to existing patterns)
  sigma_trend ~ inv_gamma(1.418, 0.452);

  // LKJ correlation prior on Cholesky factor
  L_Omega_trend ~ lkj_corr_cholesky(2);

  // Hyperpriors for hierarchical VAR coefficient means and precisions
  // Following Heaps 2022 exchangeable hyperprior structure
  for (component in 1:2) {  // [1] diagonal, [2] off-diagonal
    Amu_trend[component] ~ normal(es_trend[component], fs_trend[component]);
    Aomega_trend[component] ~ gamma(gs_trend[component], hs_trend[component]);
  }
  Z_raw ~ student_t(3, 0, 1);
  sigma_trend ~ exponential(2);
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N_count] mu_count = rep_vector(0.0, N_count);
    // initialize linear predictor term
  // Add trend effects for response count
  for (n in 1:N_count) {
    mu_count[n] += trend[obs_trend_time_count[n], obs_trend_series_count[n]];
  }
    vector[N_biomass] mu_biomass = rep_vector(0.0, N_biomass);
  // Add trend effects for response biomass
  for (n in 1:N_biomass) {
    mu_biomass[n] += trend[obs_trend_time_biomass[n], obs_trend_series_biomass[n]];
  }
    mu_count += Intercept_count + Xs_count * bs_count + Zs_count_1_1 * s_count_1_1;
    mu_biomass += Intercept_biomass + Xs_biomass * bs_biomass + Zs_biomass_1_1 * s_biomass_1_1;
    target += normal_lpdf(Y_count | mu_count, sigma_count);
    target += normal_lpdf(Y_biomass | mu_biomass, sigma_biomass);
  }
  // priors including constants
  target += lprior;
  target += std_normal_lpdf(zs_count_1_1);
  target += std_normal_lpdf(zs_biomass_1_1);
}
generated quantities {
  // actual population-level intercept
  real b_count_Intercept = Intercept_count;
  // actual population-level intercept
  real b_biomass_Intercept = Intercept_biomass;
}
