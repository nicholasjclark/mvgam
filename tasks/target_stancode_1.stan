// Expected final model for the following:
// mf_with_trend <- mvgam_formula(y ~ x, trend_formula = ~ RW())
// code_with_trend <- stancode(mf_with_trend, data = data, family = poisson(), validate = FALSE)

functions {
}
data {
  int<lower=1> N;
  array[N] int Y;
  int<lower=1> K;
  matrix[N, K] X;
  int<lower=1> Kc;
  int prior_only;
  int<lower=1> N_trend;
  int<lower=1> N_series_trend;
  int<lower=1> N_lv_trend;
  array[N_trend, N_series_trend] int times_trend;
  array[N] int obs_trend_time;
  array[N] int obs_trend_series;
  vector[1] mu_ones;
}
transformed data {
  matrix[N, Kc] Xc;
  vector[Kc] means_X;
  matrix[N_series_trend, N_lv_trend] Z = diag_matrix(rep_vector(1.0, N_lv_trend));
  for (i in 2:K) {
    means_X[i - 1] = mean(X[, i]);
    Xc[, i - 1] = X[, i] - means_X[i - 1];
  }
}
parameters {
  vector[Kc] b;
  real Intercept;
  real Intercept_trend;
  vector<lower=0>[N_lv_trend] sigma_trend;
  matrix[N_trend, N_lv_trend] innovations_trend;
}
transformed parameters {
  real lprior = 0;
  lprior += student_t_lpdf(Intercept | 3, 1.8, 2.5);
  lprior += student_t_lpdf(Intercept_trend | 3, 0, 2.5);

  matrix[N_trend, N_lv_trend] scaled_innovations_trend;
  scaled_innovations_trend = innovations_trend * diag_matrix(sigma_trend);
  matrix[N_trend, N_lv_trend] lv_trend;

  lv_trend[1, :] = scaled_innovations_trend[1, :];
  for (i in 2:N_trend) {
    lv_trend[i, :] = lv_trend[i-1, :] + scaled_innovations_trend[i, :];
  }

  matrix[N_trend, N_series_trend] trend;
  vector[N_trend] mu_trend = rep_vector(0.0, N_trend);
  mu_trend += Intercept_trend;

  for (i in 1:N_trend) {
    for (s in 1:N_series_trend) {
      trend[i, s] = dot_product(Z[s, :], lv_trend[i, :]) + mu_trend[times_trend[i, s]];
    }
  }
}

model {
  sigma_trend ~ exponential(2);
  to_vector(innovations_trend) ~ std_normal();

  if (!prior_only) {
    vector[N] mu = Xc * b;
    for (n in 1:N) {
     mu[n] += Intercept + trend[obs_trend_time[n], obs_trend_series[n]];
    }
    target += poisson_log_glm_lpmf(Y | to_matrix(mu), 0.0, mu_ones);
  }

  target += lprior;
}
generated quantities {
  real b_Intercept = Intercept - dot_product(means_X, b);
}
