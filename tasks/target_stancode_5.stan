// Expected Stan code for:
// mvgam_formula(y ~ x, trend_formula = ~ PW(n_changepoints = 10), family = poisson())

// generated with mvgam 2.0.0
functions {
  matrix get_changepoint_matrix(vector t, vector t_change_trend, int T, int S) {
     /* Function to sort changepoints */
     /* credit goes to the Prophet development team at Meta
     (https://github.com/facebook/prophet/tree/main)*/
    matrix[T, S] Kappa;
    row_vector[S] a_row;
    int cp_idx;
    Kappa = rep_matrix(0, T, S);
    a_row = rep_row_vector(0, S);
    cp_idx = 1;
    for (i in 1 : T) {
      while ((cp_idx <= S) && (t[i] >= t_change_trend[cp_idx])) {
        a_row[cp_idx] = 1;
        cp_idx = cp_idx + 1;
      }
      Kappa[i] = a_row;
    }
    return Kappa;
  }

  vector linear_trend(real k, real m, vector delta, vector t, matrix Kappa_trend,
                      vector t_change_trend) {
    /* Function to compute a linear trend with changepoints */
    /* credit goes to the Prophet development team at Meta
    (https://github.com/facebook/prophet/tree/main)*/
      return (k + Kappa_trend * delta) .* t + (m + Kappa_trend * (-t_change_trend .* delta));
  }
}
data {
  int<lower=1> N;  // total number of observations
  array[N] int Y;  // response variable
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  int<lower=1> Kc;  // number of population-level effects after centering
  int prior_only;  // should the likelihood be ignored?

  // Trend dimensions (injected by mvgam)
  int<lower=1> N_trend;  // number of timepoints
  int<lower=1> N_series_trend;  // number of observed time series
  int<lower=1> N_lv_trend;  // number of latent states
  array[N_trend, N_series_trend] int times_trend;  // temporal order
  array[N] int obs_trend_time;  // idx to map latent states to observations
  array[N] int obs_trend_series;  // idx to map latent states to observations
  vector[1] mu_ones;  // Column of ones for glm means

  // Piecewise trend data
  int<lower=0> n_change_trend;  // number of potential trend changepoints
  vector[n_change_trend] t_change_trend;  // times of potential changepoints
  real<lower=0> change_scale_trend;  // scale of changepoint shock prior
}
transformed data {
  matrix[N, Kc] Xc;  // centered version of X without an intercept
  vector[Kc] means_X;  // column means of X before centering
  for (i in 2:K) {
    means_X[i - 1] = mean(X[, i]);
    Xc[, i - 1] = X[, i] - means_X[i - 1];
  }

  // Factor loading matrix (diagonal for PW trends - no factor model support)
  matrix[N_series_trend, N_lv_trend] Z = diag_matrix(rep_vector(1.0, N_lv_trend));

  // time vector for changepoint calculations
  vector[N_trend] time_trend;
  for (i in 1:N_trend) time_trend[i] = i;

  // sorted changepoint matrix
  matrix[N_trend, n_change_trend] Kappa_trend = get_changepoint_matrix(time_trend, t_change_trend, N_trend, n_change_trend);
}
parameters {
  vector[Kc] b;  // regression coefficients
  real Intercept;  // temporary intercept for centered predictors
  real Intercept_trend;  // temporary intercept for centered predictors

  // base trend growth rates
  vector[N_lv_trend] k_trend;

  // trend offset parameters
  vector[N_lv_trend] m_trend;

  // trend rate adjustments per series
  matrix[n_change_trend, N_lv_trend] delta_trend;
}
transformed parameters {
  real lprior = 0;  // prior contributions to the log posterior
  lprior += student_t_lpdf(Intercept | 3, 1.8, 2.5);
  lprior += student_t_lpdf(Intercept_trend | 3, 0, 2.5);

  // Standardized latent trend matrix (linear piecewise trends)
  matrix[N_trend, N_lv_trend] lv_trend;

  // linear trend estimates
  for (s in 1 : N_lv_trend) {
    lv_trend[1 : N_trend, s] = linear_trend(k_trend[s], m_trend[s],
                                            to_vector(delta_trend[ : , s]), time_trend,
                                            Kappa_trend,
                                            t_change_trend);
  }

  // Derived latent trends using universal computation pattern
  matrix[N_trend, N_series_trend] trend;

  // Latent state means
  vector[N_trend] mu_trend = rep_vector(Intercept_trend, N_trend);

  // Universal trend computation: state-space dynamics + linear predictors
  for (i in 1:N_trend) {
    for (s in 1:N_series_trend) {
      trend[i, s] = dot_product(Z[s, :], lv_trend[i, :]) + mu_trend[times_trend[i, s]];
    }
  }

  // Observation means with GLM optimization
  vector[N] mu = Xc * b;
  for (n in 1:N) {
    mu[n] += Intercept + trend[obs_trend_time[n], obs_trend_series[n]];
  }
}
model {
  // PW trend default priors
  m_trend ~ student_t(3, 0, 2.5);
  k_trend ~ std_normal();
  to_vector(delta_trend) ~ double_exponential(0, change_scale_trend);

  // likelihood including constants
  if (!prior_only) {
    target += poisson_log_glm_lpmf(Y | to_matrix(mu), 0.0, mu_ones);
  }

  // priors including constants
  target += lprior;
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept - dot_product(means_X, b);
}
