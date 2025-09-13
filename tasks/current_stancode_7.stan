// generated with brms 2.22.9
functions {
}
data {
  int<lower=1> N;  // total number of observations
  array[N] int Y;  // response variable
  // data for group-level effects of ID 1
  int<lower=1> N_1;  // number of grouping levels
  int<lower=1> M_1;  // number of coefficients per level
  array[N] int<lower=1> J_1;  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_1_1;
  int prior_only;  // should the likelihood be ignored?
    int<lower=1> N_trend;  // total number of observations
  int<lower=1> N_series_trend;
  int<lower=1> N_lv_trend;
  array[N_trend, N_series_trend] int times_trend;
  array[N_trend, N_series_trend] real<lower=0> time_dis;
    int<lower=1> Ksp_trend;  // number of special effects terms
    int<lower=1> Imo_trend;  // number of monotonic variables
    array[N_trend] int Xmo_1_trend;  // monotonic variable
    array[Imo_trend] int<lower=1> Jmo_trend;  // length of simplexes
    vector[Jmo_trend[1]] con_simo_1_trend;  // prior concentration of monotonic simplex
  array[N] int obs_trend_time;
  array[N] int obs_trend_series;
}
transformed data {
  matrix[N_series_trend, N_lv_trend] Z = diag_matrix(rep_vector(1.0, N_lv_trend));
}
parameters {
  real Intercept;  // temporary intercept for centered predictors
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations
  array[M_1] vector[N_1] z_1;  // standardized group-level effects
  real Intercept_trend;  // temporary intercept for centered predictors
simplex[Jmo_trend[1]] simo_1_trend;  // monotonic simplex
vector[Ksp_trend] bsp_trend;  // special effects coefficients
  // CAR AR1 parameters
vector<lower=-1,upper=1>[N_lv_trend] ar1_trend;
  vector<lower=0>[N_lv_trend] sigma_trend;
  matrix[N_trend, N_lv_trend] innovations_trend;
}
transformed parameters {
  vector[N_1] r_1_1;  // actual group-level effects
  real lprior = 0;  // prior contributions to the log posterior
  vector[N_trend] mu_trend = rep_vector(0.0, N_trend);
mu_trend += Intercept_trend;

for (n in 1:N_trend) {
  mu_trend[n] += (bsp_trend[1]) * mo(simo_1_trend, Xmo_1_trend[n]);
}
  matrix[N_trend, N_lv_trend] lv_trend;
  matrix[N_trend, N_lv_trend] scaled_innovations_trend;
  scaled_innovations_trend = innovations_trend * diag_matrix(sigma_trend);
  lprior += student_t_lpdf(Intercept_trend | 3, 0.4, 2.5);
lprior += dirichlet_lpdf(simo_1_trend | con_simo_1_trend);
    // CAR latent variable evolution using shared innovation system

  // Initialize first time point with innovations
  for (j in 1:N_lv_trend) {
    lv_trend[1, j] = scaled_innovations_trend[1, j];
  }

  // Apply continuous-time AR evolution for subsequent time points
  for (j in 1:N_lv_trend) {
    for (i in 2:N_trend) {
      lv_trend[i, j] = pow(ar1_trend[j], time_dis[i, j]) * lv_trend[i - 1, j]
                     + scaled_innovations_trend[i, j];
    }
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
  r_1_1 = (sd_1[1] * (z_1[1]));
  lprior += student_t_lpdf(Intercept | 3, 1.8, 2.5);
  lprior += student_t_lpdf(sd_1 | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
}
model {
  ar1_trend ~ normal(0, 0.5);
sigma_trend ~ exponential(2);
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu = rep_vector(0.0, N);
    mu += Intercept;
    
      // Add trend effects using mapping arrays
      for (n in 1:N) {
        mu[n] += trend[obs_trend_time[n], obs_trend_series[n]];
      }
    for (n in 1:N) {
      // add more terms to the linear predictor
      mu[n] += r_1_1[J_1[n]] * Z_1_1[n];
    }
    target += poisson_log_lpmf(Y | mu);
  }
  // priors including constants
  target += lprior;
  target += std_normal_lpdf(z_1[1]);
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept;
}
