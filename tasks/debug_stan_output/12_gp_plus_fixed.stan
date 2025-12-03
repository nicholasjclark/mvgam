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
  int prior_only;
  int<lower=1> N_trend;
  int<lower=1> N_series_trend;
  int<lower=1> N_lv_trend;
  array[N_trend, N_series_trend] int times_trend;
  int<lower=1> K_trend;
  matrix[N_trend, K_trend] X_trend;
  int<lower=1> Dgp_1_trend;
  int<lower=1> Kgp_1_trend;
  int<lower=1> Nsubgp_1_trend;
  array[N_trend] int<lower=1> Jgp_1_trend;
  array[Nsubgp_1_trend] vector[Dgp_1_trend] Xgp_1_trend;
  array[N] int obs_trend_time;
  array[N] int obs_trend_series;
}
transformed data {
  // Factor loadings matrix: maps latent variables to observed series
  matrix[N_series_trend, N_lv_trend] Z = diag_matrix(rep_vector(1.0,
                                                                N_lv_trend));
}
parameters {
  real Intercept;
  vector[K_trend] b_trend;
  vector<lower=0>[Kgp_1_trend] sdgp_1_trend;
  array[Kgp_1_trend] vector<lower=0>[1] lscale_1_trend;
  vector[Nsubgp_1_trend] zgp_1_trend;
  vector<lower=0>[N_lv_trend] sigma_trend;
  matrix[N_trend, N_lv_trend] innovations_trend;
  vector<lower=-1, upper=1>[N_lv_trend] ar1_trend;
}
transformed parameters {
  // Prior log-probability accumulator
  real lprior = 0;
  lprior += student_t_lpdf(sdgp_1_trend | 3, 0, 2.5)
            - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += inv_gamma_lpdf(lscale_1_trend[1][1] | 1.494197, 0.056607);
  lprior += student_t_lpdf(Intercept | 3, 1.6, 2.5);
  vector[N_trend] mu_trend = rep_vector(0.0, N_trend);
  vector[Nsubgp_1_trend] gp_pred_1_trend = gp_exp_quad(Xgp_1_trend,
                                                       sdgp_1_trend[1],
                                                       lscale_1_trend[1],
                                                       zgp_1_trend);
  mu_trend += gp_pred_1_trend[Jgp_1_trend];
  mu_trend += X_trend * b_trend;
  matrix[N_trend, N_lv_trend] scaled_innovations_trend;
  scaled_innovations_trend = innovations_trend * diag_matrix(sigma_trend);
  // Latent variable trajectories over time
  matrix[N_trend, N_lv_trend] lv_trend;
  for (i in 1 : 1) {
    lv_trend[i,  : ] = scaled_innovations_trend[i,  : ];
  }
  for (i in 2 : N_trend) {
    for (j in 1 : N_lv_trend) {
      lv_trend[i, j] = ar1_trend[j] * lv_trend[i - 1, j]
                       + scaled_innovations_trend[i, j];
    }
  }
  // Final trend values for each time point and series
  matrix[N_trend, N_series_trend] trend;
  // Map latent variables to trend values via factor loadings
  for (i in 1 : N_trend) {
    for (s in 1 : N_series_trend) {
      trend[i, s] = dot_product(Z[s,  : ], lv_trend[i,  : ])
                    + mu_trend[times_trend[i, s]];
    }
  }
}
model {
  sigma_trend ~ exponential(2);
  to_vector(innovations_trend) ~ std_normal();
  ar1_trend ~ normal(0, 0.5);
  
  // Observation linear predictors and likelihoods (skipped when sampling from prior only)
  if (!prior_only) {
    vector[N] mu = rep_vector(0.0, N);
    mu += Intercept;
    for (n in 1 : N) {
      mu[n] += trend[obs_trend_time[n], obs_trend_series[n]];
    }
    // Likelihood calculations
    target += poisson_log_lpmf(Y | mu);
  }
  
  // Prior contributions
  target += lprior;
  target += std_normal_lpdf(zgp_1_trend);
}
generated quantities {
  real b_Intercept = Intercept;
}

