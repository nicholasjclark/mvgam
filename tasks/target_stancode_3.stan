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

      // Numerical stability check for positive definiteness
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

      // Initialize
      Sigma_for[1] = Sigma;
      Sigma_rev[p + 1] = Sigma;

      // Forward recursion
      for (k in 1:p) {
        phi_for[k, k] = P[k];
        Sigma_for[k + 1] = Sigma_for[k] - quad_form_sym(Sigma_for[k], P[k]');

        for (j in 1:(k-1)) {
          phi_for[j, k] = phi_for[j, k-1] - phi_for[k, k] * phi_for[k-j, k-1];
        }
      }

      // Backward recursion
      for (k in p:1) {
        phi_rev[k, k] = P[k];
        Sigma_rev[k] = Sigma_rev[k + 1] - quad_form_sym(Sigma_rev[k + 1], P[k]');

        for (j in 1:(k-1)) {
          phi_rev[j, k] = phi_rev[j, k+1] - phi_rev[k, k] * phi_rev[k-j, k+1];
        }
      }

      // Extract final coefficients
      for (i in 1:p) {
        phiGamma[1, i] = phi_for[i, p];  // Stationary coefficients
        phiGamma[2, i] = Sigma_for[i + 1];  // Covariance matrices
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
    companion_var[(p * m + 1):((p + 1) * m), (p * m + 1):((p + 1) * m)] = Sigma;
   // For eps_t innovations
    companion_var[1:m, (p * m + 1):((p + 1) * m)] = Sigma;  // Cross-covariance
    companion_var[(p * m + 1):((p + 1) * m), 1:m] = Sigma;  // Symmetric cross-covariance

    // Solve Lyapunov equation: Omega = Sigma_tilde + Phi_tilde * Omega * Phi_tilde'
    // Vectorized form: vec(Omega) = (I - Phi_tilde ⊗ Phi_tilde)^{-1}vec(Sigma_tilde)
    tmp = diag_matrix(rep_vector(1.0, (p + q) * m * (p + q) * m)) - kronecker_prod(companion_mat, companion_mat);

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

  // Trend dimensions (injected by mvgam)
  int<lower=1> N_trend;  // number of time points
  int<lower=1> N_series_trend;  // number of series
  int<lower=1> N_lv_trend;  // latent variables

  // Trend formula design matrix for covariates
  int<lower=1> K_trend;  // number of trend coefficients
  int<lower=1> Kc_trend;  // number of trend coefficients after centering
  matrix[N_trend, K_trend] X_trend;  // trend design matrix
  matrix[N_trend, Kc_trend] Xc_trend;  // centered trend design matrix
  vector[Kc_trend] means_X_trend;  // column means of X_trend

  // Observation-to-trend mappings
  array[N_count] int obs_trend_time_count;
  array[N_count] int obs_trend_series_count;
  array[N_biomass] int obs_trend_time_biomass;
  array[N_biomass] int obs_trend_series_biomass;

  // Times trend matrix
  array[N_trend, N_series_trend] int times_trend;
}
transformed data {
  matrix[N_trend, Kc_trend] Xc_trend;  // centered version of X_trend without an intercept
  vector[Kc_trend] means_X_trend;  // column means of X_trend before centering
  for (i in 2:K_trend) {
    means_X_trend[i - 1] = mean(X_trend[, i]);
    Xc_trend[, i - 1] = X_trend[, i] - means_X_trend[i - 1];
  }

  // Zero mean vector for VARMA process (following Heaps 2022)
  vector[N_lv_trend] trend_zeros = rep_vector(0.0, N_lv_trend);

  // Factor loading matrix (identity for non-factor VAR)
  matrix[N_series_trend, N_lv_trend] Z = diag_matrix(rep_vector(1.0, N_lv_trend));
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

  // Trend parameters (injected by mvgam)
  real Intercept_trend;  // trend intercept
  vector[Kc_trend] b_trend;  // trend coefficients for presence covariate

  // VAR coefficient matrices (raw/unconstrained for stationarity)
  array[2] matrix[N_lv_trend, N_lv_trend] A_raw_trend;  // lag-1, lag-2 raw coefficients

  // MA coefficient matrices (raw/unconstrained for stationarity)
  array[1] matrix[N_lv_trend, N_lv_trend] D_raw_trend;  // ma=TRUE -> 1 MA lag

  // Hierarchical hyperparameters following Heaps 2022
  array[2] vector[2] Amu_trend;     // Shared means [diag, off-diag][lag1, lag2]
  array[2] vector<lower=0>[2] Aomega_trend;  // Shared precisions [diag, off-diag][lag1, lag2]
  array[2] vector[1] Dmu_trend;     // MA means [diag, off-diag][ma1]
  array[2] vector<lower=0>[1] Domega_trend;  // MA precisions [diag, off-diag][ma1]

  // Innovation parameters
  vector<lower=0>[N_lv_trend] sigma_trend;  // innovation SDs
  cholesky_factor_corr[N_lv_trend] L_Omega_trend;  // innovation correlations

  // Joint initialization vector for stationary distribution
  vector[3 * N_lv_trend] init_trend;  // (2 VAR lags + 1 MA lag) * N_lv_trend

  // Standard latent variable trends
  matrix[N_trend, N_lv_trend] lv_trend;
}
transformed parameters {
  // penalized spline coefficients
  vector[knots_count_1[1]] s_count_1_1;
  // penalized spline coefficients
  vector[knots_biomass_1[1]] s_biomass_1_1;
  real lprior = 0;  // prior contributions to the log posterior

  // compute penalized spline coefficients
  s_count_1_1 = sds_count_1[1] * zs_count_1_1;
  // compute penalized spline coefficients
  s_biomass_1_1 = sds_biomass_1[1] * zs_biomass_1_1;

  // Original brms lprior calculations
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

  // Trend priors in lprior
  lprior += student_t_lpdf(Intercept_trend | 3, 0, 2.5);

  // Create trend linear predictor
  vector[N_trend] mu_trend = rep_vector(0.0, N_trend);
  mu_trend += Intercept_trend + Xc_trend * b_trend;

  // Innovation covariance matrix
  matrix[N_lv_trend, N_lv_trend] L_Sigma_trend = diag_pre_multiply(sigma_trend,
                                                                   L_Omega_trend);
  cov_matrix[N_lv_trend] Sigma_trend = multiply_lower_tri_self_transpose(L_Sigma_trend);

  // Transform raw parameters to stationary coefficients using Heaps 2022
  array[2] matrix[N_lv_trend, N_lv_trend] A_trend;
  array[1] matrix[N_lv_trend, N_lv_trend] D_trend;

  // Working arrays for stationarity transformation
  array[2] matrix[N_lv_trend, N_lv_trend] P_var;
  array[2, 2] matrix[N_lv_trend, N_lv_trend] result_var;

  // Transform A_raw_trend to stationary A_trend using AtoP + rev_mapping
  for (i in 1:2) {
    P_var[i] = AtoP(A_raw_trend[i]);
  }
  result_var = rev_mapping(P_var, Sigma_trend);
  for (i in 1:2) {
    A_trend[i] = result_var[1, i];
  }

  // Transform D_raw_trend to stationary D_trend (MA coefficients)
  array[1] matrix[N_lv_trend, N_lv_trend] P_ma;
  array[2, 1] matrix[N_lv_trend, N_lv_trend] result_ma;

  P_ma[1] = AtoP(D_raw_trend[1]);
  result_ma = rev_mapping(P_ma, Sigma_trend);
  D_trend[1] = -result_ma[1, 1];

  // Compute initial joint covariance matrix using companion matrix approach
  cov_matrix[3 * N_lv_trend] Omega_trend = initial_joint_var(Sigma_trend, A_trend, D_trend);

  // Initialize MA error terms from init_trend
  vector[N_lv_trend] ma_init_trend = init_trend[(2 * N_lv_trend + 1):(3 * N_lv_trend)];

  // Compute trend matrix
  matrix[N_trend, N_series_trend] trend;
  for (i in 1:N_trend) {
    for (s in 1:N_series_trend) {
      trend[i, s] = dot_product(Z[s, :], lv_trend[i, :]) +
        mu_trend[times_trend[i, s]];
    }
  }

  // Original brms linear predictors with trend injection
  // initialize linear predictor term
  vector[N_count] mu_count = rep_vector(0.0, N_count);
  // initialize linear predictor term
  vector[N_biomass] mu_biomass = rep_vector(0.0, N_biomass);
  mu_count += Intercept_count + Xs_count * bs_count + Zs_count_1_1 * s_count_1_1;
  mu_biomass += Intercept_biomass + Xs_biomass * bs_biomass + Zs_biomass_1_1 * s_biomass_1_1;

  // Inject trends into linear predictors
  for (n in 1:N_count) {
    mu_count[n] += trend[obs_trend_time_count[n], obs_trend_series_count[n]];
  }
  for (n in 1:N_biomass) {
    mu_biomass[n] += trend[obs_trend_time_biomass[n], obs_trend_series_biomass[n]];
  }
}
model {
  // likelihood including constants
  if (!prior_only) {
    target += normal_lpdf(Y_count | mu_count, sigma_count);
    target += normal_lpdf(Y_biomass | mu_biomass, sigma_biomass);
  }
  // priors including constants
  target += lprior;
  target += std_normal_lpdf(zs_count_1_1);
  target += std_normal_lpdf(zs_biomass_1_1);

  // VARMA likelihood implementation following Heaps 2022 methodology

  // Initial joint distribution for stationary VARMA initialization
  vector[3 * N_lv_trend] mu_init_trend = rep_vector(0.0, 3 * N_lv_trend);
  init_trend ~ multi_normal(mu_init_trend, Omega_trend);

  // Conditional means for VARMA dynamics
  vector[N_lv_trend] mu_t_trend[N_trend];
  vector[N_lv_trend] ma_error_trend[N_trend];  // MA error terms

  // Compute conditional means for all time points
  for (t in 1:N_trend) {
    mu_t_trend[t] = rep_vector(0.0, N_lv_trend);

    // VAR component: Add autoregressive terms
    for (i in 1:2) {  // 2 VAR lags
      if (t - i <= 0) {
        // Use values from earlier than series start (from init_trend)
        int init_idx = 2 - (t - i) + 1;
        if (init_idx > 0 && init_idx <= 2) {
          vector[N_lv_trend] lagged_lv;
          int start_idx = (init_idx - 1) * N_lv_trend + 1;
          int end_idx = init_idx * N_lv_trend;
          lagged_lv = init_trend[start_idx:end_idx];
          mu_t_trend[t] += A_trend[i] * lagged_lv;
        }
      } else {
        // Use regular lv_trend values
        mu_t_trend[t] += A_trend[i] * lv_trend[t - i, :]';
        }
      }

      // MA component: Add moving average terms for VARMA
      if (t - 1 <= 0) {
        // Use initial MA errors for early time points
        mu_t_trend[t] += D_trend[1] * ma_init_trend;
      } else {
        // Use computed MA errors from previous time points
        mu_t_trend[t] += D_trend[1] * ma_error_trend[t - 1];
      }
    }

    // Latent time series
    for (t in 1:N_trend) {
      lv_trend[t, :]' ~ multi_normal(mu_t_trend[t], Sigma_trend);
        // Compute MA error for next iteration
        ma_error_trend[t] = lv_trend[t, :]' - mu_t_trend[t];
    }

    // Hierarchical priors for VAR coefficients (Heaps 2022 methodology)
    for (lag in 1:2) {
      // Diagonal elements
      Amu_trend[1, lag] ~ normal(0.0, 1.0);
      Aomega_trend[1, lag] ~ gamma(2.0, 1.0);

      // Off-diagonal elements
      Amu_trend[2, lag] ~ normal(0.0, 1.0);
      Aomega_trend[2, lag] ~ gamma(2.0, 1.0);
    }

    // Hierarchical priors for MA coefficients
    Dmu_trend[1, 1] ~ normal(0.0, 1.0);
    Domega_trend[1, 1] ~ gamma(2.0, 1.0);
    Dmu_trend[2, 1] ~ normal(0.0, 1.0);
    Domega_trend[2, 1] ~ gamma(2.0, 1.0);

    // Trend coefficients for presence covariate
    b_trend ~ normal(0, 1);

    // Innovation priors
    sigma_trend ~ exponential(2);
    L_Omega_trend ~ lkj_corr_cholesky(2);

    // Structured priors for raw coefficient matrices
    for (i in 1:2) {
      for (j in 1:N_lv_trend) {
        for (k in 1:N_lv_trend) {
          if (j == k) {
            // Diagonal elements
            A_raw_trend[i, j, k] ~ normal(Amu_trend[1, i], inv_sqrt(Aomega_trend[1, i]));
          } else {
            // Off-diagonal elements
            A_raw_trend[i, j, k] ~ normal(Amu_trend[2, i], inv_sqrt(Aomega_trend[2, i]));
          }
        }
      }
    }

    // MA raw coefficient priors
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
    // actual population-level intercept
    real b_count_Intercept = Intercept_count;
    // actual population-level intercept
    real b_biomass_Intercept = Intercept_biomass;
    // Actual trend intercept
    real b_trend_Intercept = Intercept_trend - dot_product(means_X_trend, b_trend);
  }
