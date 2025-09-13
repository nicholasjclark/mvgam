// generated with brms 2.22.9
functions {
  /* Spectral density function of a Gaussian process
   * with squared exponential covariance kernel
   * Args:
   *   x: array of numeric values of dimension NB x D
   *   sdgp: marginal SD parameter
   *   lscale: vector of length-scale parameters
   * Returns:
   *   numeric vector of length NB of the SPD evaluated at 'x'
   */
  vector spd_gp_exp_quad(data array[] vector x, real sdgp, vector lscale) {
    int NB = dims(x)[1];
    int D = dims(x)[2];
    int Dls = rows(lscale);
    real constant = square(sdgp) * sqrt(2 * pi())^D;
    vector[NB] out;
    if (Dls == 1) {
      // one dimensional or isotropic GP
      real neg_half_lscale2 = -0.5 * square(lscale[1]);
      constant = constant * lscale[1]^D;
      for (m in 1:NB) {
        out[m] = constant * exp(neg_half_lscale2 * dot_self(x[m]));
      }
    } else {
      // multi-dimensional non-isotropic GP
      vector[Dls] neg_half_lscale2 = -0.5 * square(lscale);
      constant = constant * prod(lscale);
      for (m in 1:NB) {
        out[m] = constant * exp(dot_product(neg_half_lscale2, square(x[m])));
      }
    }
    return out;
  }
}
data {
  int<lower=1> N;  // total number of observations
  vector[N] Y;  // response variable
  // data related to GPs
  int<lower=1> Kgp_1;  // number of sub-GPs (equal to 1 unless 'by' was used)
  int<lower=1> Dgp_1;  // GP dimension
  // number of basis functions of an approximate GP
  int<lower=1> NBgp_1;
  // number of latent GP groups
  int<lower=1> Nsubgp_1;
  // indices of latent GP groups per observation
  array[N] int<lower=1> Jgp_1;
  // approximate GP basis matrices
  matrix[Nsubgp_1, NBgp_1] Xgp_1;
  // approximate GP eigenvalues
  array[NBgp_1] vector[Dgp_1] slambda_1;
  int prior_only;  // should the likelihood be ignored?
    int<lower=1> N_trend;  // total number of observations
  int<lower=1> N_series_trend;
  int<lower=1> N_lv_trend;
  array[N_trend, N_series_trend] int times_trend;
    int<lower=1> Dgp_1_trend;  // GP dimension
    int<lower=1> NBgp_1_trend;
    int<lower=1> Kgp_1_trend;  // number of sub-GPs (equal to 1 unless 'by' was used)
    int<lower=1> Nsubgp_1_trend;
    array[N_trend] int<lower=1> Jgp_1_trend;
    matrix[Nsubgp_1_trend, NBgp_1_trend] Xgp_1_trend;
    array[NBgp_1_trend] vector[Dgp_1_trend] slambda_1_trend;
  array[N] int obs_trend_time;
  array[N] int obs_trend_series;
}
transformed data {
  matrix[N_series_trend, N_lv_trend] Z = diag_matrix(rep_vector(1.0, N_lv_trend));
}
parameters {
  real Intercept;  // temporary intercept for centered predictors
  vector<lower=0>[Kgp_1] sdgp_1;  // GP standard deviation parameters
  array[Kgp_1] vector<lower=0>[1] lscale_1;  // GP length-scale parameters
  vector[NBgp_1] zgp_1;  // latent variables of the GP
  real<lower=0> sigma;  // dispersion parameter
  vector<lower=0>[Kgp_1_trend] sdgp_1_trend;  // GP standard deviation parameters
array[Kgp_1_trend] vector<lower=0>[1] lscale_1_trend;  // GP length-scale parameters
vector[NBgp_1_trend] zgp_1_trend;  // latent variables of the GP
  vector<lower=0>[N_lv_trend] sigma_trend;
  matrix[N_trend, N_lv_trend] innovations_trend;
  // AR coefficient parameters
vector<lower=-1,upper=1>[N_lv_trend] ar1_trend;
vector<lower=-1,upper=1>[N_lv_trend] ar12_trend;
}
transformed parameters {
  real lprior = 0;  // prior contributions to the log posterior
  vector[N_trend] mu_trend = rep_vector(0.0, N_trend);
mu_trend += gp_pred_1_trend[Jgp_1_trend];
  
    // Scaled innovations (uncorrelated case)
    matrix[N_trend, N_lv_trend] scaled_innovations_trend;

    // Apply scaling using vectorized operations
    scaled_innovations_trend = innovations_trend * diag_matrix(sigma_trend);
  matrix[N_trend, N_lv_trend] lv_trend;
  lprior += student_t_lpdf(sdgp_1_trend | 3, 0, 2.5)
- 1 * student_t_lccdf(0 | 3, 0, 2.5);
lprior += inv_gamma_lpdf(lscale_1_trend[1][1] | 1.494197, 0.056607);
    // Latent states with AR dynamics
  

  

  // Initialize first 12 time points
  for (i in 1:12) {
    lv_trend[i, :] = scaled_innovations_trend[i, :];
  }

  // Apply AR dynamics
  for (i in 13:N_trend) {
    for (j in 1:N_lv_trend) {
      lv_trend[i, j] = ar1_trend[j] * lv_trend[i-1, j] + ar12_trend[j] * lv_trend[i-12, j] + scaled_innovations_trend[i, j];
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
  lprior += student_t_lpdf(Intercept | 3, 6, 3);
  lprior += student_t_lpdf(sdgp_1 | 3, 0, 3)
    - 1 * student_t_lccdf(0 | 3, 0, 3);
  lprior += inv_gamma_lpdf(lscale_1[1][1] | 1.494197, 0.056607);
  lprior += student_t_lpdf(sigma | 3, 0, 3)
    - 1 * student_t_lccdf(0 | 3, 0, 3);
}
model {
  // Shared Gaussian innovation priors
  sigma_trend ~ exponential(2);
  to_vector(innovations_trend) ~ std_normal();
  ar1_trend ~ normal(0, 0.5);
ar12_trend ~ normal(0, 0.5);
  // likelihood including constants
  if (!prior_only) {
    // scale latent variables of the GP
    vector[NBgp_1] rgp_1 = sqrt(spd_gp_exp_quad(slambda_1, sdgp_1[1], lscale_1[1])) .* zgp_1;
    vector[Nsubgp_1] gp_pred_1 = Xgp_1 * rgp_1;
    // initialize linear predictor term
    vector[N] mu = rep_vector(0.0, N);
    mu += Intercept + gp_pred_1[Jgp_1];
    
      // Add trend effects using mapping arrays
      for (n in 1:N) {
        mu[n] += trend[obs_trend_time[n], obs_trend_series[n]];
      }
    target += normal_lpdf(Y | mu, sigma);
  }
  // priors including constants
  target += lprior;
  target += std_normal_lpdf(zgp_1);
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept;
}
