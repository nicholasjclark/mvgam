// generated with brms 2.22.9
functions {
  /* compute a latent Gaussian process with squared exponential kernel
   * Args:
   *   x: array of continuous predictor values
   *   sdgp: marginal SD parameter
   *   lscale: length-scale parameter
   *   zgp: vector of independent standard normal variables
   * Returns:
   *   a vector to be added to the linear predictor
   */
  vector gp_exp_quad(data array[] vector x, real sdgp, vector lscale, vector zgp) {
    int Dls = rows(lscale);
    int N = size(x);
    matrix[N, N] cov;
    if (Dls == 1) {
      // one dimensional or isotropic GP
      cov = gp_exp_quad_cov(x, sdgp, lscale[1]);
    } else {
      // multi-dimensional non-isotropic GP
      cov = gp_exp_quad_cov(x[, 1], sdgp, lscale[1]);
      for (d in 2:Dls) {
        cov = cov .* gp_exp_quad_cov(x[, d], 1, lscale[d]);
      }
    }
    for (n in 1:N) {
      // deal with numerical non-positive-definiteness
      cov[n, n] += 1e-12;
    }
    return cholesky_decompose(cov) * zgp;
  }
  
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
  int<lower=1> N;  // total number of observations
  array[N] int Y;  // response variable
  // data related to GPs
  int<lower=1> Kgp_1;  // number of sub-GPs (equal to 1 unless 'by' was used)
  int<lower=1> Dgp_1;  // GP dimension
  // number of latent GP groups
  int<lower=1> Nsubgp_1;
  // indices of latent GP groups per observation
  array[N] int<lower=1> Jgp_1;
  array[Nsubgp_1] vector[Dgp_1] Xgp_1;  // covariates of the GP
  // data for group-level effects of ID 1
  int<lower=1> N_1;  // number of grouping levels
  int<lower=1> M_1;  // number of coefficients per level
  array[N] int<lower=1> J_1;  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_1_1;
  int prior_only;  // should the likelihood be ignored?
    int<lower=1> N_trend;  // total number of_trend observations
  int<lower=1> N_trend;
  int<lower=1> N_series_trend;
  int<lower=1> N_lv_trend;
  array[N_trend, N_series_trend] int times_trend;
  array[N] int obs_trend_time;
  array[N] int obs_trend_series;
  array[n, N_series_trend] real<lower=0> time_dis;
}
transformed data {
  matrix[N_series_trend, N_lv_trend] Z = diag_matrix(rep_vector(1.0, N_lv_trend));
}
parameters {
  real Intercept;  // temporary intercept for centered predictors
  vector<lower=0>[Kgp_1] sdgp_1;  // GP standard deviation parameters
  array[Kgp_1] vector<lower=0>[1] lscale_1;  // GP length-scale parameters
  vector[Nsubgp_1] zgp_1;  // latent variables of the GP
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations
  array[M_1] vector[N_1] z_1;  // standardized group-level effects
  real Intercept_trend;  // temporary intercept for centered predictors
  cholesky_factor_corr[N_lv_trend] L_Omega_global;
  array[1] cholesky_factor_corr[N_lv_trend] L_deviation_group;
  real<lower=0, upper=1> alpha_cor;
  vector<lower=0>[N_lv_trend] sigma_trend;
  matrix[N_trend, N_lv_trend] innovations_trend;
  // CAR AR1 parameters
vector<lower=-1,upper=1>[N_lv_trend] ar1_trend;
}
transformed parameters {
  vector[N_1] r_1_1;  // actual group-level effects
  real lprior = 0;  // prior contributions to the log posterior
  lprior += student_t_lpdf(Intercept_trend | 3, 0, 2.5);
  vector[N_trend] mu_trend = rep_vector(0.0, N_trend);
  
    // Scaled innovations after applying hierarchical correlations
    matrix[N_trend, N_lv_trend] scaled_innovations_trend;

    // Apply group-specific correlations to raw innovations
    for (g in 1:n_groups) {
      // Derived group-specific correlation matrices (using existing combine_cholesky)
      array[n_groups] cholesky_factor_corr[N_lv_trend] L_Omega_group;
      for (g_idx in 1:n_groups) {
        L_Omega_group[g_idx] = combine_cholesky(L_Omega_global, L_deviation_group[g_idx], alpha_cor);
      }

      // Transform raw innovations using group correlations
      matrix[N_lv_trend, N_lv_trend] Sigma_group = diag_pre_multiply(sigma_trend, L_Omega_group[g]);
      // Apply to group time points (individual generators will specify the indexing)
    }
    // CAR latent variable evolution using shared innovation system
  matrix[N_trend, N_lv_trend] lv_trend;

  // Initialize first time point with innovations
  for (j in 1:N_lv_trend) {
    lv_trend[1, j] = scaled_innovations_trend[1, j];
  }

  // Apply continuous-time AR evolution for subsequent time points
  for (j in 1:N_lv_trend) {
    for (i in 2:N_trend) {
      lv_trend[i, j] = pow(ar1_trend[j], time_dis[i, j]) * lv_trend[i - 1, j]
                     + scaled_innovations_trend[i, j];
    }
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
  r_1_1 = (sd_1[1] * (z_1[1]));
  lprior += student_t_lpdf(Intercept | 3, 1.8, 2.5);
  lprior += student_t_lpdf(sdgp_1 | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += inv_gamma_lpdf(lscale_1[1][1] | 1.494197, 0.056607);
  lprior += student_t_lpdf(sd_1 | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
}
model {
  alpha_cor ~ beta(3, 2);
  L_Omega_global ~ lkj_corr_cholesky(1);
  for (g in 1:1) { L_deviation_group[g] ~ lkj_corr_cholesky(6); }
  // Raw innovations prior
  to_vector(innovations_trend) ~ std_normal();
  ar1_trend ~ normal(0, 0.5);
  // likelihood including constants
  if (!prior_only) {
    vector[Nsubgp_1] gp_pred_1 = gp_exp_quad(Xgp_1, sdgp_1[1], lscale_1[1], zgp_1);
    // initialize linear predictor term
    vector[N] mu = rep_vector(0.0, N);
    mu += Intercept + gp_pred_1[Jgp_1];
    
      // Add trend effects using mapping arrays
      for (n in 1:N) {
        mu[n] += trend[obs_trend_time[n], obs_trend_series[n]];
      }
    for (n in 1:N) {
      // add more terms to the linear predictor
      mu[n] += r_1_1[J_1[n]] * Z_1_1[n];
    }
    target += poisson_log_lpmf(Y | mu);
  }
  // priors including constants
  target += lprior;
  target += std_normal_lpdf(zgp_1);
  target += std_normal_lpdf(z_1[1]);
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept;
}
