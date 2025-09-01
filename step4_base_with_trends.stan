// generated with brms 2.22.9
functions {
}
data {
  int<lower=1> N;  // total number of observations
  array[N] int Y;  // response variable
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  int<lower=1> Kc;  // number of population-level effects after centering
  int prior_only;  // should the likelihood be ignored?
  int N_trend;
  int K_trend;
  real Kc_trend;
  matrix[50, 1] X_trend;
  int times_trend[50, 1];
  array[N] int obs_trend_time;
  array[N] int obs_trend_series;
}
transformed data {
  matrix[N, Kc] Xc;  // centered version of X without an intercept
  vector[Kc] means_X;  // column means of X before centering
  matrix[n_series_trend, n_lv_trend] Z = diag_matrix(rep_vector(1.0, n_lv_trend));
  for (i in 2:K) {
    means_X[i - 1] = mean(X[, i]);
    Xc[, i - 1] = X[, i] - means_X[i - 1];
  }
}
parameters {
  vector[Kc] b;  // regression coefficients
  real Intercept;  // temporary intercept for centered predictors
  real Intercept_trend;  // temporary intercept for centered predictors
  real<lower=0> sigma_trend;  // dispersion parameter
}
transformed parameters {
  real lprior = 0;  // prior contributions to_trend the log posterior
  lprior += student_t_lpdf(Intercept_trend | 3, -0.1, 2.5);
  lprior += student_t_lpdf(sigma_trend | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
}
model {
  // likelihood including_trend constants_trend
  if (!prior_only) {
    // initialize_trend linear_trend predictor_trend term_trend
    vector[N_trend] mu_trend = rep_vector(0.0, N_trend);
    mu_trend += Intercept_trend;
    target += normal_lpdf(Y | mu_trend, sigma_trend);
  vector<lower=0>[1] sigma_trend;
  matrix[n_trend, 1] innovations_trend;
  
}
transformed parameters {
  real lprior = 0;  // prior contributions to the log posterior
  real lprior = 0;  // prior contributions to_trend the log posterior
  lprior += student_t_lpdf(Intercept_trend | 3, -0.1, 2.5);
  lprior += student_t_lpdf(sigma_trend | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
}
model {
  // likelihood including_trend constants_trend
  if (!prior_only) {
    // initialize_trend linear_trend predictor_trend term_trend
    vector[N_trend] mu_trend = rep_vector(0.0, N_trend);
    mu_trend += Intercept_trend;
    target += normal_lpdf(Y | mu_trend, sigma_trend);
  
    // Scaled innovations (uncorrelated case)
    matrix[n_trend, 1] scaled_innovations_trend;

    // Apply scaling using vectorized operations
    scaled_innovations_trend = innovations_trend * diag_matrix(sigma_trend);
  matrix[n_trend, n_lv_trend] lv_trend;
lv_trend = scaled_innovations_trend;
    // Derived latent trends using universal computation pattern
  matrix[n_trend, n_series_trend] trend;

  // Universal trend computation: state-space dynamics + linear predictors
  // dot_product captures dynamic component, mu_trend captures trend_formula
  for (i in 1:n_trend) {
    for (s in 1:n_series_trend) {
      trend[i, s] = dot_product(Z[s, :], lv_trend[i, :]) + mu_trend[times_trend[i, s]];
    }
  }
  lprior += student_t_lpdf(Intercept | 3, 2.3, 2.5);
}
model {
  // Shared Gaussian innovation priors
  sigma_trend ~ exponential(2);
  to_vector(innovations_trend) ~ std_normal();
  
  // likelihood including constants
  if (!prior_only) {
    target += poisson_log_glm_lpmf(Y | Xc, Intercept, b);
  }
  // priors including constants
  target += lprior;
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept - dot_product(means_X, b);
}

