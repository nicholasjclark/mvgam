// Generated with mvgam 2.0.0 using brms 2.22.9
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
  vector gp_exp_quad(data array[] vector x, real sdgp, vector lscale,
                     vector zgp) {
    int Dls = rows(lscale);
    int N = size(x);
    matrix[N, N] cov;
    if (Dls == 1) {
      // one dimensional or isotropic GP
      cov = gp_exp_quad_cov(x, sdgp, lscale[1]);
    } else {
      // multi-dimensional non-isotropic GP
      cov = gp_exp_quad_cov(x[ : , 1], sdgp, lscale[1]);
      for (d in 2 : Dls) {
        cov = cov .* gp_exp_quad_cov(x[ : , d], 1, lscale[d]);
      }
    }
    for (n in 1 : N) {
      cov[n, n] += 1e-12;
    }
    return cholesky_decompose(cov) * zgp;
  }
}
data {
  int<lower=1> N;
  array[N] int Y;
  int<lower=1> N_1;
  int<lower=1> M_1;
  array[N] int<lower=1> J_1;
  vector[N] Z_1_1;
  int prior_only;
  int<lower=1> N_trend;
  int<lower=1> N_series_trend;
  int<lower=1> N_lv_trend;
  array[N_trend, N_series_trend] int times_trend;
  array[N_trend, N_series_trend] real<lower=0> time_dis;
  int<lower=1> Dgp_1_trend;
  int<lower=1> Kgp_1_trend;
  int<lower=1> Nsubgp_1_trend;
  array[N_trend] int<lower=1> Jgp_1_trend;
  array[Nsubgp_1_trend] vector[Dgp_1_trend] Xgp_1_trend;
  array[N] int obs_trend_time;
  array[N] int obs_trend_series;
}
transformed data {
  matrix[N_series_trend, N_lv_trend] Z = diag_matrix(rep_vector(1.0,
                                                                N_lv_trend));
}
parameters {
  real Intercept;
  vector<lower=0>[M_1] sd_1;
  array[M_1] vector[N_1] z_1;
  real Intercept_trend;
  vector<lower=0>[Kgp_1_trend] sdgp_1_trend;
  array[Kgp_1_trend] vector<lower=0>[1] lscale_1_trend;
  vector[Nsubgp_1_trend] zgp_1_trend;
  vector<lower=-1, upper=1>[N_lv_trend] ar1_trend;
  vector<lower=0>[N_lv_trend] sigma_trend;
  matrix[N_trend, N_lv_trend] innovations_trend;
}
transformed parameters {
  vector[N_1] r_1_1;
  real lprior = 0;
  vector[N_trend] mu_trend = rep_vector(0.0, N_trend);
  vector[Nsubgp_1_trend] gp_pred_1_trend = gp_exp_quad(Xgp_1_trend,
                                                       sdgp_1_trend[1],
                                                       lscale_1_trend[1],
                                                       zgp_1_trend);
  mu_trend += Intercept_trend + gp_pred_1_trend[Jgp_1_trend];
  matrix[N_trend, N_lv_trend] lv_trend;
  matrix[N_trend, N_lv_trend] scaled_innovations_trend;
  scaled_innovations_trend = innovations_trend * diag_matrix(sigma_trend);
  lprior += student_t_lpdf(Intercept_trend | 3, 0, 2.5);
  lprior += student_t_lpdf(sdgp_1_trend | 3, 0, 2.5)
            - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += inv_gamma_lpdf(lscale_1_trend[1][1] | 1.494197, 0.056607);
  for (j in 1 : N_lv_trend) {
    lv_trend[1, j] = scaled_innovations_trend[1, j];
  }
  for (j in 1 : N_lv_trend) {
    for (i in 2 : N_trend) {
      lv_trend[i, j] = pow(ar1_trend[j], time_dis[i, j]) * lv_trend[i - 1, j]
                       + scaled_innovations_trend[i, j];
    }
  }
  matrix[N_trend, N_series_trend] trend;
  for (i in 1 : N_trend) {
    for (s in 1 : N_series_trend) {
      trend[i, s] = dot_product(Z[s,  : ], lv_trend[i,  : ])
                    + mu_trend[times_trend[i, s]];
    }
  }
  r_1_1 = (sd_1[1] * (z_1[1]));
  lprior += student_t_lpdf(Intercept | 3, 1.8, 2.5);
  lprior += student_t_lpdf(sd_1 | 3, 0, 2.5)
            - 1 * student_t_lccdf(0 | 3, 0, 2.5);
}
model {
  target += std_normal_lpdf(zgp_1_trend);
  ar1_trend ~ normal(0, 0.5);
  sigma_trend ~ exponential(2);
  to_vector(innovations_trend) ~ std_normal();
  if (!prior_only) {
    vector[N] mu = rep_vector(0.0, N);
    mu += Intercept;
    for (n in 1 : N) {
      mu[n] += trend[obs_trend_time[n], obs_trend_series[n]];
    }
    for (n in 1 : N) {
      mu[n] += r_1_1[J_1[n]] * Z_1_1[n];
    }
    target += poisson_log_lpmf(Y | mu);
  }
  target += lprior;
  target += std_normal_lpdf(z_1[1]);
}
generated quantities {
  real b_Intercept = Intercept;
  real b_Intercept_trend = Intercept_trend;
}

