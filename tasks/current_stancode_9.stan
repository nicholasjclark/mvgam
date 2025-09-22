// generated with brms 2.22.9
functions {
  
}
data {
  int<lower=1> N; // total number of observations
  vector[N] Y; // response variable
  int<lower=1> K_b1; // number of population-level effects
  matrix[N, K_b1] X_b1; // population-level design matrix
  int<lower=1> K_b2; // number of population-level effects
  matrix[N, K_b2] X_b2; // population-level design matrix
  
  // covariates for non-linear functions
  vector[N] C_1;
  int prior_only; // should the likelihood be ignored?
  int<lower=1> N_trend; // total number of observations
  int<lower=1> N_series_trend;
  int<lower=1> N_lv_trend;
  array[N_trend, N_series_trend] int times_trend;
  array[N] int obs_trend_time;
  array[N] int obs_trend_series;
}
transformed data {
  matrix[N_series_trend, N_lv_trend] Z = diag_matrix(rep_vector(1.0,
                                                                N_lv_trend));
}
parameters {
  vector[K_b1] b_b1; // regression coefficients
  vector[K_b2] b_b2; // regression coefficients
  real<lower=0> sigma; // dispersion parameter
  real Intercept_trend; // temporary intercept for centered predictors
  vector<lower=0>[N_lv_trend] sigma_trend;
  matrix[N_trend, N_lv_trend] innovations_trend;
  
  // AR coefficient parameters
  vector<lower=-1, upper=1>[N_lv_trend] ar1_trend;
}
transformed parameters {
  real lprior = 0; // prior contributions to the log posterior
  vector[N_trend] mu_trend = rep_vector(0.0, N_trend);
  mu_trend += Intercept_trend;
  
  // Scaled innovations (uncorrelated case)
  matrix[N_trend, N_lv_trend] scaled_innovations_trend;
  
  // Apply scaling using vectorized operations
  scaled_innovations_trend = innovations_trend * diag_matrix(sigma_trend);
  matrix[N_trend, N_lv_trend] lv_trend;
  lprior += student_t_lpdf(Intercept_trend | 3, 0.1, 2.5);
  
  // Latent states with AR dynamics
  // Initialize first 1 time points
  for (i in 1 : 1) {
    lv_trend[i,  : ] = scaled_innovations_trend[i,  : ];
  }
  
  // Apply AR dynamics
  for (i in 2 : N_trend) {
    for (j in 1 : N_lv_trend) {
      lv_trend[i, j] = ar1_trend[j] * lv_trend[i - 1, j]
                       + scaled_innovations_trend[i, j];
    }
  }
  
  // Derived latent trends using universal computation pattern
  matrix[N_trend, N_series_trend] trend;
  
  // Universal trend computation: state-space dynamics + linear predictors
  // dot_product captures dynamic component, mu_trend captures trend_formula
  for (i in 1 : N_trend) {
    for (s in 1 : N_series_trend) {
      trend[i, s] = dot_product(Z[s,  : ], lv_trend[i,  : ])
                    + mu_trend[times_trend[i, s]];
    }
  }
  lprior += normal_lpdf(b_b1 | 1, 2);
  lprior += normal_lpdf(b_b2 | 0, 2);
  lprior += student_t_lpdf(sigma | 3, 0, 3)
            - 1 * student_t_lccdf(0 | 3, 0, 3);
}
model {
  // Shared Gaussian innovation priors
  sigma_trend ~ exponential(2);
  to_vector(innovations_trend) ~ std_normal();
  ar1_trend ~ normal(0, 0.5);
  
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] nlp_b1 = rep_vector(0.0, N);
    
    // initialize linear predictor term
    vector[N] nlp_b2 = rep_vector(0.0, N);
    
    // initialize non-linear predictor term
    vector[N] mu;
    nlp_b1 += X_b1 * b_b1;
    nlp_b2 += X_b2 * b_b2;
    for (n in 1 : N) {
      // compute non-linear predictor values
      mu[n] = ((nlp_b1[n] * exp(nlp_b2[n] * C_1[n])))
              + trend[obs_trend_time[n], obs_trend_series[n]];
    }
    target += normal_lpdf(Y | mu, sigma);
  }
  
  // priors including constants
  target += lprior;
}
generated quantities {
  // actual population-level intercept
  real b_Intercept_trend = Intercept_trend;
}

