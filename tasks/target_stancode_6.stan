// Expected Stan code for:
// mvgam_formula(y ~ gp(x) + (1 | series), trend_formula = ~ CAR())
// with family = poisson()

// generated with mvgam 2.0.0
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
}
data {
  int<lower=1> N;  // total number of observations
  array[N] int Y;  // response variable
  
  // data related to GPs
  int<lower=1> Kgp_1;  // number of sub-GPs (equal to 1 unless 'by' was used)
  int<lower=1> Dgp_1;  // GP dimension
  int<lower=1> Nsubgp_1;  // number of latent GP groups
  array[N] int<lower=1> Jgp_1;  // indices of latent GP groups per observation
  array[Nsubgp_1] vector[Dgp_1] Xgp_1;  // covariates of the GP
  
  // data for group-level effects of ID 1
  int<lower=1> N_1;  // number of grouping levels
  int<lower=1> M_1;  // number of coefficients per level
  array[N] int<lower=1> J_1;  // grouping indicator per observation
  vector[N] Z_1_1;  // group-level predictor values
  
  int prior_only;  // should the likelihood be ignored?
  
  // Trend dimensions (injected by mvgam)
  int<lower=1> N_trend;  // number of timepoints
  int<lower=1> N_series_trend;  // number of observed time series
  int<lower=1> N_lv_trend;  // number of latent states
  array[N_trend, N_series_trend] int times_trend;  // temporal order
  array[N] int obs_trend_time;  // idx to map latent states to observations
  array[N] int obs_trend_series;  // idx to map latent states to observations
  vector[1] mu_ones;  // Column of ones for glm means
  
  // CAR-specific data
  array[N_trend, N_series_trend] real<lower=0> time_dis;  // time distances for continuous AR
}
transformed data {
  // Factor loading matrix (diagonal for CAR - no factor model support)
  matrix[N_series_trend, N_lv_trend] Z = diag_matrix(rep_vector(1.0, N_lv_trend));
}
parameters {
  real Intercept;  // temporary intercept for centered predictors
  vector<lower=0>[Kgp_1] sdgp_1;  // GP standard deviation parameters
  array[Kgp_1] vector<lower=0>[1] lscale_1;  // GP length-scale parameters
  vector[Nsubgp_1] zgp_1;  // latent variables of the GP
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations
  array[M_1] vector[N_1] z_1;  // standardized group-level effects
  
  // Trend parameters (injected by mvgam)
  real Intercept_trend;  // trend intercept
  vector<lower=-1,upper=1>[N_lv_trend] ar1_trend;  // CAR AR1 coefficients
  vector<lower=0>[N_lv_trend] sigma_trend;  // innovation SDs
  matrix[N_trend, N_lv_trend] innovations_trend;  // raw innovations
}
transformed parameters {
  vector[N_1] r_1_1;  // actual group-level effects
  real lprior = 0;  // prior contributions to the log posterior
  
  r_1_1 = (sd_1[1] * (z_1[1]));
  lprior += student_t_lpdf(Intercept | 3, 1.8, 2.5);
  lprior += student_t_lpdf(sdgp_1 | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += inv_gamma_lpdf(lscale_1[1][1] | 1.494197, 0.056607);
  lprior += student_t_lpdf(sd_1 | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += student_t_lpdf(Intercept_trend | 3, 0, 2.5);
  
  // CAR trend computation (injected by mvgam)
  // Scaled innovations
  matrix[N_trend, N_lv_trend] scaled_innovations_trend;
  scaled_innovations_trend = innovations_trend * diag_matrix(sigma_trend);
  
  // CAR latent variables with continuous-time evolution
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
  
  // Latent state means
  vector[N_trend] mu_trend = rep_vector(Intercept_trend, N_trend);
  
  // Universal trend computation: state-space dynamics + linear predictors
  for (i in 1:N_trend) {
    for (s in 1:N_series_trend) {
      trend[i, s] = dot_product(Z[s, :], lv_trend[i, :]) + mu_trend[times_trend[i, s]];
    }
  }
}
model {
  // likelihood including constants
  if (!prior_only) {
    vector[Nsubgp_1] gp_pred_1 = gp_exp_quad(Xgp_1, sdgp_1[1], lscale_1[1], zgp_1);
    // initialize linear predictor term with GLM optimization
    vector[N] mu = rep_vector(0.0, N);
    mu += Intercept + gp_pred_1[Jgp_1];
    for (n in 1:N) {
      // add more terms to the linear predictor
      mu[n] += r_1_1[J_1[n]] * Z_1_1[n];
      // inject CAR trend effects
      mu[n] += trend[obs_trend_time[n], obs_trend_series[n]];
    }
    target += poisson_log_lpmf(Y | mu);
  }
  
  // priors including constants
  target += lprior;
  target += std_normal_lpdf(zgp_1);
  target += std_normal_lpdf(z_1[1]);
  
  // CAR trend priors (injected by mvgam)
  ar1_trend ~ normal(0, 0.5);
  sigma_trend ~ exponential(2);
  to_vector(innovations_trend) ~ std_normal();
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept;
}
