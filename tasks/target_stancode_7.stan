// Expected Stan code for:
  // mvgam_formula(y ~ (1 | series), trend_formula = ~ mo(income) + CAR())
// with family = poisson()

functions {
  /* compute monotonic effects
   * Args:
   *   scale: a simplex parameter
   *   i: index to sum over the simplex
   * Returns:
   *   a scalar between 0 and rows(scale)
   */
  real mo(vector scale, int i) {
    if (i == 0) {
      return 0;
    } else {
      return rows(scale) * sum(scale[1:i]);
    }
  }
}
data {
  int<lower=1> N;
  array[N] int Y;
  int<lower=1> N_1;
  int<lower=1> M_1;
  array[N] int<lower=1> J_1;
  vector[N] Z_1_1;
  int<lower=1> N_trend;
  int<lower=1> Ksp_trend;
  int<lower=1> Imo_trend;
  array[Imo_trend] int<lower=1> Jmo_trend;
  array[N_trend] int Xmo_1_trend;
  vector[Jmo_trend[1]] con_simo_1_trend;
  int prior_only;
  int<lower=1> N_series_trend;
  int<lower=1> N_lv_trend;
  array[N_trend, N_series_trend] int times_trend;
  array[N] int obs_trend_time;
  array[N] int obs_trend_series;
  array[N_trend, N_series_trend] real<lower=0> time_dis;
}
transformed data {
  matrix[N_series_trend, N_lv_trend] Z = diag_matrix(rep_vector(1.0, N_lv_trend));
}
parameters {
  real Intercept;
  vector<lower=0>[M_1] sd_1;
  array[M_1] vector[N_1] z_1;
  real Intercept_trend;
  simplex[Jmo_trend[1]] simo_1_trend;
  vector[Ksp_trend] bsp_trend;
  vector<lower=-1,upper=1>[N_lv_trend] ar1_trend;
  vector<lower=0>[N_lv_trend] sigma_trend;
  matrix[N_trend, N_lv_trend] innovations_trend;
}
transformed parameters {
  vector[N_1] r_1_1;
  real lprior = 0;
  r_1_1 = (sd_1[1] * (z_1[1]));
  lprior += student_t_lpdf(Intercept | 3, 1.8, 2.5);
  lprior += student_t_lpdf(sd_1 | 3, 0, 2.5)
  - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += student_t_lpdf(Intercept_trend | 3, 0, 2.5);
  lprior += dirichlet_lpdf(simo_1_trend | con_simo_1_trend);
  matrix[N_trend, N_lv_trend] scaled_innovations_trend;
  scaled_innovations_trend = innovations_trend * diag_matrix(sigma_trend);
  matrix[N_trend, N_lv_trend] lv_trend;

  for (j in 1:N_lv_trend) {
    lv_trend[1, j] = scaled_innovations_trend[1, j];
  }

  for (j in 1:N_lv_trend) {
    for (i in 2:N_trend) {
      lv_trend[i, j] = pow(ar1_trend[j], time_dis[i, j]) * lv_trend[i - 1, j]
      + scaled_innovations_trend[i, j];
    }
  }

  vector[N_trend] mu_trend = rep_vector(0.0, N_trend);
  mu_trend += Intercept_trend;

  for (n in 1:N_trend) {
    mu_trend[n] += (bsp_trend[1]) * mo(simo_1_trend, Xmo_1_trend[n]);
  }

  matrix[N_trend, N_series_trend] trend;

  for (i in 1:N_trend) {
    for (s in 1:N_series_trend) {
      trend[i, s] = dot_product(Z[s, :], lv_trend[i, :]) + mu_trend[times_trend[i, s]];
    }
  }
}
model {
  if (!prior_only) {
    vector[N] mu = rep_vector(0.0, N);

    mu += Intercept;

    for (n in 1:N) {
      mu[n] += r_1_1[J_1[n]] * Z_1_1[n];
      mu[n] += trend[obs_trend_time[n], obs_trend_series[n]];
    }

    target += poisson_log_lpmf(Y | mu);
  }

  target += lprior;
  target += std_normal_lpdf(z_1[1]);
  ar1_trend ~ normal(0, 0.5);
  sigma_trend ~ exponential(2);
  to_vector(innovations_trend) ~ std_normal();
}
generated quantities {
  real b_Intercept = Intercept;
}
