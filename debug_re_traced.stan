// generated with brms 2.22.9
functions {
}
data {
  int<lower=1> N;  // total number of observations
  array[N] int Y;  // response variable
  int prior_only;  // should the likelihood be ignored?
    int<lower=1> N_trend;  // total number of observations
  int<lower=1> N_series_trend;
  int<lower=1> N_lv_trend;
  array[N_trend, N_series_trend] int times_trend;
    vector[N_trend] Z_1_1_trend;
    array[N_trend] int<lower=1> J_1_trend;  // grouping indicator per observation
    int<lower=1> N_1_trend;  // number of grouping levels
    int<lower=1> M_1_trend;  // number of coefficients per level
  array[N] int obs_trend_time;
  array[N] int obs_trend_series;
}
transformed data {
  matrix[N_series_trend, N_lv_trend] Z = diag_matrix(rep_vector(1.0, N_lv_trend));
}
parameters {
  real Intercept;  // temporary intercept for centered predictors
  real Intercept_trend;  // temporary intercept for centered predictors
vector<lower=0>[M_1_trend] sd_1_trend;  // group-level standard deviations
array[M_1_trend] vector[N_1_trend] z_1_trend;  // standardized group-level effects
  vector<lower=0>[N_lv_trend] sigma_trend;
  cholesky_factor_corr[N_lv_trend] L_Omega_trend;
  matrix[N_trend, N_lv_trend] innovations_trend;
  
}
transformed parameters {
  real lprior = 0;  // prior contributions to the log posterior
  vector[N_trend] mu_trend = rep_vector(Intercept_trend, N_trend);
  
    // Scaled innovations after applying correlations
    matrix[N_trend, N_lv_trend] scaled_innovations_trend;

    // Apply correlation transformation using efficient non-centered parameterization
    {
      matrix[N_lv_trend, N_lv_trend] L_Sigma = diag_pre_multiply(sigma_trend, L_Omega_trend);
      scaled_innovations_trend = innovations_trend * L_Sigma';
    }
  matrix[N_trend, N_lv_trend] lv_trend;
  vector[N_1_trend] r_1_1_trend;  // actual group-level effects
r_1_1_trend = (sd_1_trend[1] * (z_1_trend[1]));
lprior += student_t_lpdf(Intercept_trend | 3, -0.1, 2.5);
lprior += student_t_lpdf(sd_1_trend | 3, 0, 2.5)
- 1 * student_t_lccdf(0 | 3, 0, 2.5);
  matrix[N_lv_trend, N_lv_trend] Sigma_trend = diag_pre_multiply(sigma_trend, L_Omega_trend);
  lv_trend = scaled_innovations_trend;
    // Derived latent trends using universal computation pattern
  matrix[N_trend, N_series_trend] trend;

  // Universal trend computation: state-space dynamics + linear predictors
  // dot_product captures dynamic component, mu_trend captures trend_formula
  for (i in 1:N_trend) {
    for (s in 1:N_series_trend) {
      trend[i, s] = dot_product(Z[s, :], lv_trend[i, :]) + mu_trend[times_trend[i, s]];
    }
  }
  lprior += student_t_lpdf(Intercept | 3, 1.1, 2.5);
}
model {
  // Shared Gaussian innovation priors
  sigma_trend ~ exponential(2);
  L_Omega_trend ~ lkj_corr_cholesky(2);
  to_vector(innovations_trend) ~ std_normal();
  
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu = rep_vector(0.0, N);
    mu += Intercept;
    
      // Add trend effects using mapping arrays
      for (n in 1:N) {
        mu[n] += trend[obs_trend_time[n], obs_trend_series[n]];
      }
    target += poisson_log_lpmf(Y | mu);
  }
  // priors including constants
  target += lprior;
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept;
}
