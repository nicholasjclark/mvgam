// Expected final model for the following:
// stancode(
//  mvgam_formula(
//    formula = bf(count ~ x, family = poisson()) +
//      bf(presence ~ x, family = bernoulli()) +
//      bf(biomass ~ x, family = Gamma()),
//    trend_formula = ~ -1 + AR(p = 1, n_lv = 2, cor = TRUE),
//  ),
//  data = data
// )

functions {
}
data {
  int<lower=1> N;  // total number of observations
  int<lower=1> N_count;  // number of observations
  array[N_count] int Y_count;  // response variable
  int<lower=1> K_count;  // number of population-level effects
  matrix[N_count, K_count] X_count;  // population-level design matrix
  int<lower=1> Kc_count;  // number of population-level effects after centering
  int<lower=1> N_presence;  // number of observations
  array[N_presence] int Y_presence;  // response variable
  int<lower=1> K_presence;  // number of population-level effects
  matrix[N_presence, K_presence] X_presence;  // population-level design matrix
  int<lower=1> Kc_presence;  // number of population-level effects after centering
  int<lower=1> N_biomass;  // number of observations
  vector[N_biomass] Y_biomass;  // response variable
  int<lower=1> K_biomass;  // number of population-level effects
  matrix[N_biomass, K_biomass] X_biomass;  // population-level design matrix
  int<lower=1> Kc_biomass;  // number of population-level effects after centering
  int prior_only;  // should the likelihood be ignored?

  // Trend dimensions (injected by mvgam)
  int<lower=1> N_trend;  // number of time points
  int<lower=1> N_series_trend;  // number of series
  int<lower=1> N_lv_trend;  // latent variables

  // Observation-to-trend mappings
  array[N_count] int obs_trend_time_count;
  array[N_count] int obs_trend_series_count;
  array[N_presence] int obs_trend_time_presence;
  array[N_presence] int obs_trend_series_presence;
  array[N_biomass] int obs_trend_time_biomass;
  array[N_biomass] int obs_trend_series_biomass;

  // Times trend matrix
  array[N_trend, N_series_trend] int times_trend;

  // GLM compatibility vectors
  vector[1] mu_ones_count;  // for GLM count
  vector[1] mu_ones_presence;  // for GLM presence
}
transformed data {
  matrix[N_count, Kc_count] Xc_count;  // centered version of X_count without an intercept
  vector[Kc_count] means_X_count;  // column means of X_count before centering
  matrix[N_presence, Kc_presence] Xc_presence;  // centered version of X_presence without an intercept
  vector[Kc_presence] means_X_presence;  // column means of X_presence before centering
  matrix[N_biomass, Kc_biomass] Xc_biomass;  // centered version of X_biomass without an intercept
  vector[Kc_biomass] means_X_biomass;  // column means of X_biomass before centering
  for (i in 2:K_count) {
    means_X_count[i - 1] = mean(X_count[, i]);
    Xc_count[, i - 1] = X_count[, i] - means_X_count[i - 1];
  }
  for (i in 2:K_presence) {
    means_X_presence[i - 1] = mean(X_presence[, i]);
    Xc_presence[, i - 1] = X_presence[, i] - means_X_presence[i - 1];
  }
  for (i in 2:K_biomass) {
    means_X_biomass[i - 1] = mean(X_biomass[, i]);
    Xc_biomass[, i - 1] = X_biomass[, i] - means_X_biomass[i - 1];
  }
}
parameters {
  vector[Kc_count] b_count;  // regression coefficients
  real Intercept_count;  // temporary intercept for centered predictors
  vector[Kc_presence] b_presence;  // regression coefficients
  real Intercept_presence;  // temporary intercept for centered predictors
  vector[Kc_biomass] b_biomass;  // regression coefficients
  real Intercept_biomass;  // temporary intercept for centered predictors
  real<lower=0> shape_biomass;  // shape parameter

  // Trend parameters (injected by mvgam)
  // AR(1) coefficients with correlation
  vector<lower=-1,upper=1>[N_lv_trend] ar1_trend;  // AR coefficients

  // Innovation parameters
  vector<lower=0>[N_lv_trend] sigma_trend;  // innovation SDs
  cholesky_factor_corr[N_lv_trend] L_Omega_trend;  // innovation correlations

  // Factor loading matrix (estimated for factor model)
  vector[N_series_trend * N_lv_trend] Z_raw;  // raw factor loadings

  // Latent variable innovations
  matrix[N_trend, N_lv_trend] innovations_trend;
}
transformed parameters {
  real lprior = 0;  // prior contributions to the log posterior
  lprior += student_t_lpdf(Intercept_count | 3, 1.4, 2.5);
  lprior += student_t_lpdf(Intercept_presence | 3, 0, 2.5);
  lprior += student_t_lpdf(Intercept_biomass | 3, 0.4, 2.5);
  lprior += gamma_lpdf(shape_biomass | 0.01, 0.01);

  // Factor loading matrix with identifiability constraints
  matrix[N_series_trend, N_lv_trend] Z = rep_matrix(0, N_series_trend, N_lv_trend);
  // constraints allow identifiability of loadings
  {
    int index = 1;
    for (j in 1 : N_lv_trend) {
      for (i in j : N_series_trend) {
        Z[i, j] = Z_raw[index];
        index += 1;
      }
    }
  }

  // Innovation covariance matrix
  matrix[N_lv_trend, N_lv_trend] L_Sigma_trend = diag_pre_multiply(sigma_trend, L_Omega_trend);
  matrix[N_trend, N_lv_trend] scaled_innovations_trend = innovations_trend * L_Sigma_trend';

  // AR(1) latent variable dynamics
  matrix[N_trend, N_lv_trend] lv_trend;
  lv_trend[1, :] = scaled_innovations_trend[1, :];
  for (i in 2:N_trend) {
    for (j in 1:N_lv_trend) {
      lv_trend[i, j] = ar1_trend[j] * lv_trend[i-1, j] + scaled_innovations_trend[i, j];
    }
  }

  // Trend mean vector (zero for ~ -1, but needed for prediction compatibility)
  vector[N_trend] mu_trend = rep_vector(0.0, N_trend);

  // Compute trend matrix using factor model with universal pattern
  matrix[N_trend, N_series_trend] trend;
   for (i in 1:N_trend) {
    for (s in 1:N_series_trend) {
      trend[i, s] = dot_product(Z[s, :], lv_trend[i, :]) + mu_trend[times_trend[i, s]];
    }
  }

    // GLM-compatible linear predictors with trend injection
    vector[N_count] mu_count = Xc_count * b_count;
    vector[N_presence] mu_presence = Xc_presence * b_presence;
    vector[N_biomass] mu_biomass = rep_vector(0.0, N_biomass);
    mu_biomass += Intercept_biomass + Xc_biomass * b_biomass;

    // Inject trends into linear predictors
    for (n in 1:N_count) {
      mu_count[n] += Intercept_count + trend[obs_trend_time_count[n], obs_trend_series_count[n]];
    }
    for (n in 1:N_presence) {
      mu_presence[n] += Intercept_presence + trend[obs_trend_time_presence[n], obs_trend_series_presence[n]];
    }
    for (n in 1:N_biomass) {
      mu_biomass[n] += trend[obs_trend_time_biomass[n], obs_trend_series_biomass[n]];
    }

    // Transform biomass predictor (inverse link for Gamma)
    mu_biomass = inv(mu_biomass);
  }
  model {
    // likelihood including constants
    if (!prior_only) {
      target += poisson_log_glm_lpmf(Y_count | to_matrix(mu_count), 0.0, mu_ones_count);
      target += bernoulli_logit_glm_lpmf(Y_presence | to_matrix(mu_presence), 0.0, mu_ones_presence);
      target += gamma_lpdf(Y_biomass | shape_biomass, shape_biomass ./ mu_biomass);
    }
    // priors including constants
    target += lprior;

    // Trend priors
    ar1_trend ~ normal(0, 0.5);
    sigma_trend ~ exponential(2);
    L_Omega_trend ~ lkj_corr_cholesky(2);
    Z_raw ~ student_t(3, 0, 1);
    to_vector(innovations_trend) ~ std_normal();
  }
  generated quantities {
    // actual population-level intercept
    real b_count_Intercept = Intercept_count - dot_product(means_X_count, b_count);
    // actual population-level intercept
    real b_presence_Intercept = Intercept_presence - dot_product(means_X_presence, b_presence);
    // actual population-level intercept
    real b_biomass_Intercept = Intercept_biomass - dot_product(means_X_biomass, b_biomass);
  }
