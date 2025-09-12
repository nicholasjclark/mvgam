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
  array[N_trend, N_series_trend] real<lower=0> time_dis;
    int<lower=1> Dgp_1_trend;  // GP dimension
    int<lower=1> Kgp_1_trend;  // number of sub-GPs (equal to 1 unless 'by' was used)
    int<lower=1> Nsubgp_1_trend;
    array[N_trend] int<lower=1> Jgp_1_trend;
    array[Nsubgp_1_trend] vector[Dgp_1_trend] Xgp_1_trend;  // covariates of the GP
  array[N] int obs_trend_time;
  array[N] int obs_trend_series;
}
transformed data {
  matrix[N_series_trend, N_lv_trend] Z = diag_matrix(rep_vector(1.0, N_lv_trend));
}
parameters {
  real Intercept;  // temporary intercept for centered predictors
  real Intercept_trend;  // temporary intercept for centered predictors
vector<lower=0>[Kgp_1_trend] sdgp_1_trend;  // GP standard deviation parameters
array[Kgp_1_trend] vector<lower=0>[1] lscale_1_trend;  // GP length-scale parameters
vector[Nsubgp_1_trend] zgp_1_trend;  // latent variables of the GP
  // CAR AR1 parameters
vector<lower=-1,upper=1>[N_lv_trend] ar1_trend;
  vector<lower=0>[N_lv_trend] sigma_trend;
  matrix[N_trend, N_lv_trend] innovations_trend;
}
transformed parameters {
  real lprior = 0;  // prior contributions to the log posterior
  vector[N_trend] mu_trend = rep_vector(Intercept_trend, N_trend);
  matrix[N_trend, N_lv_trend] lv_trend;
  matrix[N_trend, N_lv_trend] scaled_innovations_trend;
  scaled_innovations_trend = innovations_trend * diag_matrix(sigma_trend);
  lprior += student_t_lpdf(Intercept_trend | 3, -0.3, 2.5);
lprior += student_t_lpdf(sdgp_1_trend | 3, 0, 2.5)
- 1 * student_t_lccdf(0 | 3, 0, 2.5);
lprior += inv_gamma_lpdf(lscale_1_trend[1][1] | 1.494197, 0.056607);
    // CAR latent variable evolution using shared innovation system

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
  lprior += student_t_lpdf(Intercept | 3, 1.1, 2.5);
}
model {
  ar1_trend ~ normal(0, 0.5);
sigma_trend ~ exponential(2);
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
