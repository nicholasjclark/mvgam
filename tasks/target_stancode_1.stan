// This is the target for:
// data <- setup_stan_test_data()$univariate
// mf_obs_only <- mvgam_formula(y ~ x)
// mf_with_trend <- mvgam_formula(y ~ x, trend_formula = ~ RW())
// code_with_trend <- stancode(mf_with_trend, data = data, family = poisson(), validate = FALSE)

// generated with mvgam 2.0.0
functions {
}
data {
  int<lower=1> N;  // total number of observations
  array[N] int Y;  // response variable
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  int<lower=1> Kc;  // number of population-level effects after centering
  int prior_only;  // should the likelihood be ignored?
  int<lower=1> n_trend;  // number of timepoints
  int<lower=1> n_series_trend;  // number of observed time series
  int<lower=1> n_lv_trend;  // number of latent states
  matrix times_trend[n_trend, n_series_trend];  // temporal order of latent states
  array[N] int obs_trend_time;  // idx to map latent states to observations
  array[N] int obs_trend_series;  // idx to map latent states to observations
}
transformed data {
  matrix[N, Kc] Xc;  // centered version of X without an intercept
  vector[Kc] means_X;  // column means of X before centering
  matrix[n_series_trend, n_lv_trend] Z = diag_matrix(rep_vector(1.0, n_lv_trend));
  for (i in 2:K) {
    means_X[i - 1] = mean(X[, i]);
    Xc[, i - 1] = X[, i] - means_X[i - 1];
  }

  // Column of ones for glm means
  vector[N] mu_ones = rep_vector(1, N);
}
parameters {
  vector[Kc] b;  // regression coefficients
  real Intercept;  // temporary intercept for centered predictors
  real Intercept_trend;  // temporary intercept for centered predictors
  real<lower=0> sigma;  // dispersion parameter
  vector<lower=0>[1] sigma_trend;
  matrix[n_trend, n_lv_trend] innovations_trend;
}
transformed parameters {
  real lprior = 0;  // prior contributions to the log posterior
  lprior += student_t_lpdf(Intercept | 3, 1.8, 2.5);
  lprior += student_t_lpdf(Intercept_trend | 3, 0, 2.5);

  // Scaled innovations (uncorrelated case)
  matrix[n_trend, 1] scaled_innovations_trend;

  // Apply scaling using vectorized operations
  scaled_innovations_trend = innovations_trend * diag_matrix(sigma_trend);

  // Latent states with RW dynamics
  matrix[n_trend, n_lv_trend] lv_trend;

  // Apply RW dynamics
  lv_trend[1, :] = scaled_innovations_trend[1, :];
  for (i in 2:n_trend) {
    lv_trend[i, :] = lv_trend[i-1, :] + scaled_innovations_trend[i, :];
  }

  // Derived latent trends using universal computation pattern
  matrix[n_trend, n_series_trend] trend;

  // Latent state means
  vector[n_trend] mu_trend = rep_vector(Intercept_trend, n_trend);

  // Universal trend computation: state-space dynamics + linear predictors
  // dot_product captures dynamic component, mu_trend captures trend_formula
  for (i in 1:n_trend) {
    for (s in 1:n_series_trend) {
      trend[i, s] = dot_product(Z[s, :], lv_trend[i, :]) + mu_trend[times_trend[i, s]];
    }
  }

  // Observation means
  vector[N] mu = Intercept + Xc * b;
  for (n in 1:N) {
    mu[n] += trend[obs_trend_time[n], obs_trend_series[n]];
  }
}

model {
  // Shared Gaussian innovation priors
  sigma_trend ~ exponential(2);
  to_vector(innovations_trend) ~ std_normal();

  // likelihood including constants
  if (!prior_only) {
    target += poisson_log_glm_lpmf(Y | mu, 0.0, mu_ones);
  }

  // priors including constants
  target += lprior;
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept - dot_product(means_X, b);
}
