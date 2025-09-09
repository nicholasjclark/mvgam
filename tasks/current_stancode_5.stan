// generated with brms 2.22.9
functions {
  
      matrix get_changepoint_matrix(vector t, vector t_change_trend, int T, int S) {
        /* Function to sort changepoints */

        /* credit goes to the Prophet development team at Meta (https://github.com/facebook/prophet/tree/main)*/
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

      vector logistic_gamma(real k, real m, vector delta, vector t_change_trend, int S) {
        /* Function to compute a logistic trend with changepoints */

        /* credit goes to the Prophet development team at Meta (https://github.com/facebook/prophet/tree/main)*/
        vector[S] gamma; // adjusted offsets, for piecewise continuity
        vector[S + 1] k_s; // actual rate in each segment
        real m_pr;
        k_s = append_row(k, k + cumulative_sum(delta));
        m_pr = m; // The offset in the previous segment
        for (i in 1 : S) {
          gamma[i] = (t_change_trend[i] - m_pr) * (1 - k_s[i] / k_s[i + 1]);
          m_pr = m_pr + gamma[i]; // update for the next segment
        }
        return gamma;
      }

      vector logistic_trend(real k, real m, vector delta, vector t, vector cap_trend,
                            matrix Kappa_trend, vector t_change_trend, int S) {
        /* Function to adjust a logistic trend using a carrying capacity */

        /* credit goes to the Prophet development team at Meta (https://github.com/facebook/prophet/tree/main)*/
        vector[S] gamma;
        gamma = logistic_gamma(k, m, delta, t_change_trend, S);
        return cap_trend .* inv_logit((k + Kappa_trend * delta) .* (t - (m + Kappa_trend * gamma)));
      }

      vector linear_trend(real k, real m, vector delta, vector t, matrix Kappa_trend,
                          vector t_change_trend) {
        /* Function to compute a linear trend with changepoints */

        /* credit goes to the Prophet development team at Meta (https://github.com/facebook/prophet/tree/main)*/
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
    int<lower=1> N_trend;  // total number of_trend observations
  int<lower=1> N_series_trend;
  int<lower=1> N_lv_trend;
  array[N_trend, N_series_trend] int times_trend;
  array[N] int obs_trend_time;
  array[N] int obs_trend_series;
    int<lower=0> n_change_trend; // number of potential trend changepoints
  vector[n_change_trend] t_change_trend; // times of potential changepoints
  real<lower=0> change_scale_trend; // scale of changepoint shock prior
  vector[1] mu_ones;
}
transformed data {
  matrix[N, Kc] Xc;  // centered version of X without an intercept
  vector[Kc] means_X;  // column means of X before centering
  matrix[N_series_trend, N_lv_trend] Z = diag_matrix(rep_vector(1.0, N_lv_trend));
    // time vector for changepoint calculations
  vector[N_trend] time_trend;
  for (i in 1:N_trend) time_trend[i] = i;
  // sorted changepoint matrix
  matrix[N_trend, n_change_trend] Kappa_trend = get_changepoint_matrix(time_trend, t_change_trend, N_trend, n_change_trend);
  for (i in 2:K) {
    means_X[i - 1] = mean(X[, i]);
    Xc[, i - 1] = X[, i] - means_X[i - 1];
  }
}
parameters {
  vector[Kc] b;  // regression coefficients
  real Intercept;  // temporary intercept for centered predictors
  real Intercept_trend;  // temporary intercept for centered predictors
  vector<lower=0>[N_lv_trend] sigma_trend;
  matrix[N_trend, N_lv_trend] innovations_trend;
    // base trend growth rates
  vector[N_lv_trend] k_trend;

  // trend offset parameters
  vector[N_lv_trend] m_trend;

  // trend rate adjustments per series
  matrix[n_change_trend, N_lv_trend] delta_trend;
}
transformed parameters {
  real lprior = 0;  // prior contributions to the log posterior
  lprior += student_t_lpdf(Intercept_trend | 3, -0.1, 2.5);
  vector[N_trend] mu_trend = rep_vector(Intercept_trend, N_trend);
  
    // Scaled innovations (uncorrelated case)
    matrix[N_trend, N_lv_trend] scaled_innovations_trend;

    // Apply scaling using vectorized operations
    scaled_innovations_trend = innovations_trend * diag_matrix(sigma_trend);
    // Standardized latent trend matrix (linear piecewise trends)
  matrix[N_trend, N_lv_trend] lv_trend;

  // linear trend estimates
  for (s in 1 : N_lv_trend) {
    lv_trend[1 : N_trend, s] = linear_trend(k_trend[s], m_trend[s],
                                to_vector(delta_trend[ : , s]), time_trend, Kappa_trend,
                                t_change_trend);
  }
    // Derived latent trends using universal computation pattern
  matrix[N_trend, N_series_trend] trend;

  // Universal trend computation: state-space dynamics + linear predictors
  // dot_product captures dynamic component, mu_trend captures trend_formula
  for (i in 1:N_trend) {
    for (s in 1:N_series_trend) {
      trend[i, s] = dot_product(Z[s, :], lv_trend[i, :]) + mu_trend[times_trend[i, s]];
    }
  }
  lprior += student_t_lpdf(Intercept | 3, 1.8, 2.5);

  // Efficient matrix multiplication for linear predictor
  vector[N] mu = Xc * b;
  // Add intercept and trend components
  for (n in 1:N) {
    mu[n] += Intercept + trend[obs_trend_time[n], obs_trend_series[n]];
  }
}
model {
  // Shared Gaussian innovation priors
  sigma_trend ~ exponential(2);
  to_vector(innovations_trend) ~ std_normal();
    // PW trend default priors
  m_trend ~ student_t(3, 0, 2.5);
  k_trend ~ std_normal();
  to_vector(delta_trend) ~ double_exponential(0, 0.05);
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

