// Generated with mvgam 2.0.0 using brms 2.22.9
functions {
  matrix get_changepoint_matrix(vector t, vector t_change_trend, int T, int S) {
    /* Function to sort changepoints */
    
    /* credit goes to the Prophet development team at Meta */
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
  vector logistic_gamma(real k, real m, vector delta, vector t_change_trend,
                        int S) {
    /* Function to compute a logistic trend with changepoints */
    /* credit goes to the Prophet development team at Meta */
    vector[S] gamma;
    vector[S + 1] k_s;
    real m_pr;
    k_s = append_row(k, k + cumulative_sum(delta));
    m_pr = m;
    for (i in 1 : S) {
      gamma[i] = (t_change_trend[i] - m_pr) * (1 - k_s[i] / k_s[i + 1]);
      m_pr = m_pr + gamma[i];
    }
    return gamma;
  }
  vector logistic_trend(real k, real m, vector delta, vector t,
                        vector cap_trend, matrix Kappa_trend,
                        vector t_change_trend, int S) {
    /* Function to adjust a logistic trend using a carrying capacity */
    /* credit goes to the Prophet development team at Meta */
    vector[S] gamma;
    gamma = logistic_gamma(k, m, delta, t_change_trend, S);
    return cap_trend
           .* inv_logit((k + Kappa_trend * delta)
                        .* (t - (m + Kappa_trend * gamma)));
  }
  vector linear_trend(real k, real m, vector delta, vector t,
                      matrix Kappa_trend, vector t_change_trend) {
    /* Function to compute a linear trend with changepoints */
    /* credit goes to the Prophet development team at Meta */
    return (k + Kappa_trend * delta) .* t
           + (m + Kappa_trend * (-t_change_trend .* delta));
  }
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
  int<lower=0> n_change_trend;
  vector[n_change_trend] t_change_trend;
  real<lower=0> change_scale_trend;
  vector[1] mu_ones;
}
transformed data {
  matrix[N, Kc] Xc;
  vector[Kc] means_X;
  matrix[N_series_trend, N_lv_trend] Z = diag_matrix(rep_vector(1.0,
                                                                N_lv_trend));
  vector[N_trend] time_trend;
  for (i in 1 : N_trend) 
    time_trend[i] = i;
  matrix[N_trend, n_change_trend] Kappa_trend = get_changepoint_matrix(time_trend,
                                                                    t_change_trend,
                                                                    N_trend,
                                                                    n_change_trend);
  for (i in 2 : K) {
    means_X[i - 1] = mean(X[ : , i]);
    Xc[ : , i - 1] = X[ : , i] - means_X[i - 1];
  }
}
parameters {
  vector[Kc] b;
  real Intercept;
  real Intercept_trend;
  vector[N_lv_trend] k_trend;
  vector[N_lv_trend] m_trend;
  matrix[n_change_trend, N_lv_trend] delta_trend;
}
transformed parameters {
  real lprior = 0;
  vector[N_trend] mu_trend = rep_vector(0.0, N_trend);
  mu_trend += Intercept_trend;
  matrix[N_trend, N_lv_trend] lv_trend;
  lprior += student_t_lpdf(Intercept_trend | 3, -0.1, 2.5);
  for (s in 1 : N_lv_trend) {
    lv_trend[1 : N_trend, s] = linear_trend(k_trend[s], m_trend[s],
                                            to_vector(delta_trend[ : , s]),
                                            time_trend, Kappa_trend,
                                            t_change_trend);
  }
  matrix[N_trend, N_series_trend] trend;
  for (i in 1 : N_trend) {
    for (s in 1 : N_series_trend) {
      trend[i, s] = dot_product(Z[s,  : ], lv_trend[i,  : ])
                    + mu_trend[times_trend[i, s]];
    }
  }
  lprior += student_t_lpdf(Intercept | 3, 1.8, 2.5);
}
model {
  m_trend ~ student_t(3, 0, 2.5);
  k_trend ~ std_normal();
  to_vector(delta_trend) ~ double_exponential(0, 0.05);
  if (!prior_only) {
    vector[N] mu = Xc * b;
    for (n in 1 : N) {
      mu[n] += Intercept + trend[obs_trend_time[n], obs_trend_series[n]];
    }
    target += poisson_log_glm_lpmf(Y | to_matrix(mu), 0.0, mu_ones);
  }
  target += lprior;
}
generated quantities {
  real b_Intercept = Intercept - dot_product(means_X, b);
  real b_Intercept_trend = Intercept_trend;
}

