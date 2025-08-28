// generated with brms 2.22.9
functions {
}
data {
  int<lower=1> N;  // total number of observations
  vector[N] Y;  // response variable
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  int<lower=1> Kc;  // number of population-level effects after centering
  int prior_only;  // should the likelihood be ignored?
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
  real<lower=0> sigma;  // dispersion parameter
  vector[N] dummy_obs;
}
transformed parameters {
  real lprior = 0;  // prior contributions to the log posterior
    // Latent states with RW dynamics
  matrix[n_trend, n_lv_trend] lv_trend;
  

  

  // Apply RW dynamics
  lv_trend[1, :] = scaled_innovations_trend[1, :];
  for (i in 2:n_trend) {
    lv_trend[i, :] = lv_trend[i-1, :] + scaled_innovations_trend[i, :];
  }
    // Derived latent trends using universal computation pattern
  matrix[n_trend, n_series_trend] trend;

  // Universal trend computation: state-space dynamics + linear predictors
  // dot_product captures dynamic component, mu_trend captures trend_formula
  for (i in 1:n_trend) {
    for (s in 1:n_series_trend) {
      trend[i, s] = dot_product(Z[s, :], lv_trend[i, :]) + mu_trend[times_trend[i, s]];
    }
  }
  lprior += student_t_lpdf(Intercept | 3, -0.4, 2.5);
  lprior += student_t_lpdf(sigma | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
}
model {
  // likelihood including constants
  if (!prior_only) {
    target += normal_id_glm_lpdf(Y | Xc, Intercept, b, sigma);
  }
  // priors including constants
  target += lprior;
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept - dot_product(means_X, b);
}

