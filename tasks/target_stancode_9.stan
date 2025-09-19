// Expected Stan code for:
//   mf <- mvgam_formula(
//    bf(y ~ b1 * exp(b2 * x), b1 + b2 ~ 1, nl = TRUE),
//    trend_formula = ~ AR()
//  )
//  stancode(mf, data = test_data$univariate, validate = FALSE)

functions {

}
data {
  int<lower=1> N;
  vector[N] Y;
  int<lower=1> K_b1;
  matrix[N, K_b1] X_b1;
  int<lower=1> K_b2;
  matrix[N, K_b2] X_b2;
  vector[N] C_1;
  int prior_only;
  int<lower=1> N_trend;
  int<lower=1> N_series_trend;
  int<lower=1> N_lv_trend;
  array[N_trend, N_series_trend] int times_trend;
  array[N] int obs_trend_time;
  array[N] int obs_trend_series;
}
transformed data {
  matrix[N_series_trend, N_lv_trend] Z = diag_matrix(rep_vector(1.0, N_lv_trend));
}
parameters {
  vector[K_b1] b_b1;
  vector[K_b2] b_b2;
  real<lower=0> sigma;
  real Intercept_trend;
  vector<lower=0>[N_lv_trend] sigma_trend;
  matrix[N_trend, N_lv_trend] innovations_trend;
  vector<lower=-1,upper=1>[N_lv_trend] ar1_trend;
}
transformed parameters {
  real lprior = 0;  // prior contributions to the log posterior
  lprior += normal_lpdf(b_b1 | 1, 2);
  lprior += normal_lpdf(b_b2 | 0, 2);
  lprior += student_t_lpdf(sigma | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  matrix[N_trend, N_lv_trend] scaled_innovations_trend;
  scaled_innovations_trend = innovations_trend * diag_matrix(sigma_trend);
  matrix[N_trend, N_lv_trend] lv_trend;

  for (i in 1:1) {
    lv_trend[i, :] = scaled_innovations_trend[i, :];
  }

  for (i in 2:N_trend) {
    for (j in 1:N_lv_trend) {
      lv_trend[i, j] = ar1_trend[j] * lv_trend[i-1, j] + scaled_innovations_trend[i, j];
    }
  }

  vector[N_trend] mu_trend = rep_vector(0.0, N_trend);
  mu_trend += Intercept_trend;
  matrix[N_trend, N_series_trend] trend;

  for (i in 1:N_trend) {
    for (s in 1:N_series_trend) {
      trend[i, s] = dot_product(Z[s, :], lv_trend[i, :]) + mu_trend[times_trend[i, s]];
    }
  }
}
model {
  sigma_trend ~ exponential(2);
  to_vector(innovations_trend) ~ std_normal();
  ar1_trend ~ normal(0, 0.5);

  if (!prior_only) {

    vector[N] nlp_b1 = rep_vector(0.0, N);
    vector[N] nlp_b2 = rep_vector(0.0, N);
    vector[N] mu;
    nlp_b1 += X_b1 * b_b1;
    nlp_b2 += X_b2 * b_b2;

    for (n in 1:N) {
      mu[n] = ((nlp_b1[n] * exp(nlp_b2[n] * C_1[n]))) + trend[obs_trend_time[n], obs_trend_series[n]];
    }

    target += normal_lpdf(Y | mu, sigma);
  }

  target += lprior;
}
generated quantities {

}
