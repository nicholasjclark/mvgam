#' Trend-Specific Stan Code Generation
#'
#' @description
#' Functions to generate Stan code for different trend types (RW, AR, VAR, GP, CAR).
#' Each trend type has specific parameter declarations, model blocks, and generated quantities.

#' Generate Trend Stan Code
#' 
#' @description
#' Main dispatcher function that generates complete Stan code for specified trend type.
#' 
#' @param trend_spec List containing trend specification (type, parameters, etc.)
#' @param data_info List containing data structure information (n_series, n_times, etc.)
#' @param multivariate Logical indicating if this is a multivariate model
#' @return Character string containing complete trend Stan code
#' @noRd
generate_trend_stan_code <- function(trend_spec, data_info, multivariate = FALSE) {
  checkmate::assert_list(trend_spec, names = "named")
  checkmate::assert_list(data_info, names = "named")
  checkmate::assert_flag(multivariate)
  
  trend_type <- trend_spec$type
  
  # Dispatch to appropriate trend generator
  stan_code <- switch(trend_type,
    "RW" = generate_rw_stan_code(trend_spec, data_info, multivariate),
    "AR" = generate_ar_stan_code(trend_spec, data_info, multivariate),
    "VAR" = generate_var_stan_code(trend_spec, data_info, multivariate),
    "GP" = generate_gp_stan_code(trend_spec, data_info, multivariate),
    "CAR" = generate_car_stan_code(trend_spec, data_info, multivariate),
    stop(insight::format_error(
      "Unknown trend type: {trend_type}",
      "Supported types: RW, AR, VAR, GP, CAR"
    ))
  )
  
  return(stan_code)
}

#' Generate Random Walk Stan Code
#' 
#' @description
#' Generates Stan code for Random Walk trend models.
#' 
#' @param trend_spec Trend specification
#' @param data_info Data information
#' @param multivariate Logical for multivariate models
#' @return Character string of RW Stan code
#' @noRd
generate_rw_stan_code <- function(trend_spec, data_info, multivariate = FALSE) {
  cor_specified <- trend_spec$cor %||% FALSE
  n_series <- data_info$n_series %||% 1
  
  if (multivariate && cor_specified && n_series > 1) {
    return(generate_correlated_rw_stan_code(trend_spec, data_info))
  } else {
    return(generate_independent_rw_stan_code(trend_spec, data_info))
  }
}

#' Generate Independent Random Walk Stan Code
#' 
#' @param trend_spec Trend specification
#' @param data_info Data information
#' @return Character string of independent RW Stan code
#' @noRd
generate_independent_rw_stan_code <- function(trend_spec, data_info) {
  n_series <- data_info$n_series %||% 1
  
  glue::glue("
  functions {{
    // Random walk trend forecasting function
    vector rw_trend_forecast(vector trend_last, real sigma_trend, int n_forecast) {{
      vector[n_forecast] forecasts;
      forecasts[1] = normal_rng(trend_last[1], sigma_trend);
      for (t in 2:n_forecast) {{
        forecasts[t] = normal_rng(forecasts[t-1], sigma_trend);
      }}
      return forecasts;
    }}
  }}
  
  data {{
    int<lower=1> n_times;                    // number of time points
    int<lower=1> n_series;                   // number of series
    array[n_times, n_series] int ytimes;     // time indices for observations
  }}
  
  parameters {{
    // Random walk innovation variance
    real<lower=0> sigma_trend;
    
    // Raw trend innovations (for non-centered parameterization)
    array[n_times] vector[n_series] trend_raw;
  }}
  
  transformed parameters {{
    // Actual trend values
    array[n_times] vector[n_series] trend;
    
    // First time point (initialization)
    trend[1] = trend_raw[1] * sigma_trend;
    
    // Random walk evolution
    for (t in 2:n_times) {{
      trend[t] = trend[t-1] + trend_raw[t] * sigma_trend;
    }}
  }}
  
  model {{
    // Priors
    sigma_trend ~ student_t(3, 0, 2.5);
    
    // Random walk innovations
    for (t in 1:n_times) {{
      trend_raw[t] ~ std_normal();
    }}
  }}
  
  generated quantities {{
    // Trend effects for linear predictor
    array[n_times] vector[n_series] trend_effects = trend;
    
    // Log-likelihood contributions
    array[n_times, n_series] real log_lik_trend;
    for (t in 1:n_times) {{
      for (s in 1:n_series) {{
        if (t == 1) {{
          log_lik_trend[t, s] = normal_lpdf(trend[t, s] | 0, sigma_trend);
        }} else {{
          log_lik_trend[t, s] = normal_lpdf(trend[t, s] | trend[t-1, s], sigma_trend);
        }}
      }}
    }}
  }}
  ")
}

#' Generate Correlated Random Walk Stan Code
#' 
#' @param trend_spec Trend specification
#' @param data_info Data information
#' @return Character string of correlated RW Stan code
#' @noRd
generate_correlated_rw_stan_code <- function(trend_spec, data_info) {
  n_series <- data_info$n_series %||% 1
  
  glue::glue("
  functions {{
    // Correlated random walk trend forecasting function
    vector correlated_rw_forecast(vector trend_last, matrix Sigma, int n_forecast) {{
      vector[{n_series} * n_forecast] forecasts;
      matrix[{n_series}, {n_series}] L = cholesky_decompose(Sigma);
      
      for (t in 1:n_forecast) {{
        int start_idx = (t-1) * {n_series} + 1;
        int end_idx = t * {n_series};
        
        if (t == 1) {{
          forecasts[start_idx:end_idx] = trend_last + L * to_vector(normal_rng(rep_vector(0, {n_series}), 1));
        }} else {{
          int prev_start = (t-2) * {n_series} + 1;
          int prev_end = (t-1) * {n_series};
          forecasts[start_idx:end_idx] = forecasts[prev_start:prev_end] + L * to_vector(normal_rng(rep_vector(0, {n_series}), 1));
        }}
      }}
      return forecasts;
    }}
  }}
  
  data {{
    int<lower=1> n_times;
    int<lower=1> n_series;
    array[n_times, n_series] int ytimes;
  }}
  
  parameters {{
    // Correlation matrix for innovations
    corr_matrix[n_series] Omega_trend;
    vector<lower=0>[n_series] sigma_trend;
    
    // Raw trend innovations
    array[n_times] vector[n_series] trend_raw;
  }}
  
  transformed parameters {{
    // Covariance matrix
    matrix[n_series, n_series] Sigma_trend = quad_form_diag(Omega_trend, sigma_trend);
    matrix[n_series, n_series] L_trend = cholesky_decompose(Sigma_trend);
    
    // Actual trend values
    array[n_times] vector[n_series] trend;
    
    // First time point
    trend[1] = L_trend * trend_raw[1];
    
    // Correlated random walk evolution
    for (t in 2:n_times) {{
      trend[t] = trend[t-1] + L_trend * trend_raw[t];
    }}
  }}
  
  model {{
    // Priors
    Omega_trend ~ lkj_corr(2);
    sigma_trend ~ student_t(3, 0, 2.5);
    
    // Innovations
    for (t in 1:n_times) {{
      trend_raw[t] ~ std_normal();
    }}
  }}
  
  generated quantities {{
    array[n_times] vector[n_series] trend_effects = trend;
    
    array[n_times, n_series] real log_lik_trend;
    for (t in 1:n_times) {{
      for (s in 1:n_series) {{
        if (t == 1) {{
          log_lik_trend[t, s] = multi_normal_lpdf(trend[t] | rep_vector(0, n_series), Sigma_trend);
        }} else {{
          log_lik_trend[t, s] = multi_normal_lpdf(trend[t] | trend[t-1], Sigma_trend);
        }}
      }}
    }}
  }}
  ")
}

#' Generate AR Stan Code
#' 
#' @description
#' Generates Stan code for Autoregressive trend models.
#' 
#' @param trend_spec Trend specification
#' @param data_info Data information  
#' @param multivariate Logical for multivariate models
#' @return Character string of AR Stan code
#' @noRd
generate_ar_stan_code <- function(trend_spec, data_info, multivariate = FALSE) {
  p_order <- trend_spec$p %||% 1
  n_series <- data_info$n_series %||% 1
  
  # Handle different AR orders
  if (length(p_order) == 1) {
    return(generate_ar_p_stan_code(trend_spec, data_info, p_order))
  } else {
    # Seasonal AR with multiple lags
    return(generate_seasonal_ar_stan_code(trend_spec, data_info, p_order))
  }
}

#' Generate AR(p) Stan Code
#' 
#' @param trend_spec Trend specification
#' @param data_info Data information
#' @param p AR order
#' @return Character string of AR(p) Stan code
#' @noRd
generate_ar_p_stan_code <- function(trend_spec, data_info, p) {
  n_series <- data_info$n_series %||% 1
  
  glue::glue("
  functions {{
    // AR({p}) trend forecasting function
    vector ar{p}_trend_forecast(vector trend_last_p, vector phi, real sigma_trend, int n_forecast) {{
      vector[n_forecast] forecasts;
      vector[{p}] ar_buffer = trend_last_p;
      
      for (t in 1:n_forecast) {{
        real mu_t = dot_product(phi, ar_buffer);
        forecasts[t] = normal_rng(mu_t, sigma_trend);
        
        // Update AR buffer
        if ({p} > 1) {{
          ar_buffer[1:({p}-1)] = ar_buffer[2:{p}];
        }}
        ar_buffer[{p}] = forecasts[t];
      }}
      return forecasts;
    }}
  }}
  
  data {{
    int<lower=1> n_times;
    int<lower=1> n_series;
    array[n_times, n_series] int ytimes;
  }}
  
  parameters {{
    // AR coefficients (constrained for stationarity)
    vector[{p}] phi_raw;
    real<lower=0> sigma_trend;
    
    // Raw trend innovations
    array[n_times] vector[n_series] trend_raw;
  }}
  
  transformed parameters {{
    // Transform to stationary AR coefficients
    vector[{p}] phi;
    {{
      // Ensure stationarity using partial autocorrelations
      vector[{p}] phi_pac = tanh(phi_raw);
      phi = phi_pac;
      
      // Convert partial autocorrelations to AR coefficients for p > 1
      if ({p} > 1) {{
        for (k in 2:{p}) {{
          for (j in 1:(k-1)) {{
            phi[j] = phi[j] - phi_pac[k] * phi[k-j];
          }}
        }}
      }}
    }}
    
    // Trend values
    array[n_times] vector[n_series] trend;
    
    // Initialize first p values
    for (t in 1:min({p}, n_times)) {{
      trend[t] = trend_raw[t] * sigma_trend / sqrt(1 - dot_self(phi));
    }}
    
    // AR evolution
    for (t in ({p}+1):n_times) {{
      vector[{p}] ar_terms;
      for (lag in 1:{p}) {{
        ar_terms[lag] = trend[t-lag];
      }}
      trend[t] = dot_product(phi, ar_terms) + trend_raw[t] * sigma_trend;
    }}
  }}
  
  model {{
    // Priors
    phi_raw ~ std_normal();
    sigma_trend ~ student_t(3, 0, 2.5);
    
    // Innovations
    for (t in 1:n_times) {{
      trend_raw[t] ~ std_normal();
    }}
  }}
  
  generated quantities {{
    array[n_times] vector[n_series] trend_effects = trend;
  }}
  ")
}

#' Generate Seasonal AR Stan Code
#' 
#' @param trend_spec Trend specification
#' @param data_info Data information
#' @param p_lags Vector of AR lags (e.g., c(1, 12, 24))
#' @return Character string of seasonal AR Stan code
#' @noRd
generate_seasonal_ar_stan_code <- function(trend_spec, data_info, p_lags) {
  n_lags <- length(p_lags)
  max_lag <- max(p_lags)
  n_series <- data_info$n_series %||% 1
  
  glue::glue("
  functions {{
    // Seasonal AR trend forecasting function
    vector seasonal_ar_forecast(array[] vector trend_history, vector phi, int n_forecast) {{
      vector[n_forecast] forecasts;
      int max_lag = {max_lag};
      int n_lags = {n_lags};
      array[{n_lags}] int lags = {{{paste(p_lags, collapse = ', ')}}};
      
      for (t in 1:n_forecast) {{
        real mu_t = 0;
        int history_len = size(trend_history);
        
        for (l in 1:n_lags) {{
          int lag_idx = history_len - lags[l] + t;
          if (lag_idx > 0 && lag_idx <= history_len) {{
            mu_t += phi[l] * trend_history[lag_idx, 1]; // Assuming single series for now
          }}
        }}
        
        forecasts[t] = normal_rng(mu_t, {trend_spec$sigma %||% 1});
      }}
      return forecasts;
    }}
  }}
  
  data {{
    int<lower=1> n_times;
    int<lower=1> n_series;
    int<lower=1> n_lags;
    array[n_lags] int<lower=1> ar_lags;
    array[n_times, n_series] int ytimes;
  }}
  
  parameters {{
    vector[n_lags] phi_raw;
    real<lower=0> sigma_trend;
    array[n_times] vector[n_series] trend_raw;
  }}
  
  transformed parameters {{
    vector[n_lags] phi = tanh(phi_raw); // Constrain to (-1, 1)
    array[n_times] vector[n_series] trend;
    
    // Initialize early values
    for (t in 1:{max_lag}) {{
      trend[t] = trend_raw[t] * sigma_trend;
    }}
    
    // Seasonal AR evolution
    for (t in ({max_lag}+1):n_times) {{
      real ar_mean = 0;
      for (l in 1:n_lags) {{
        if (t > ar_lags[l]) {{
          ar_mean += phi[l] * trend[t - ar_lags[l]];
        }}
      }}
      trend[t] = ar_mean + trend_raw[t] * sigma_trend;
    }}
  }}
  
  model {{
    phi_raw ~ std_normal();
    sigma_trend ~ student_t(3, 0, 2.5);
    
    for (t in 1:n_times) {{
      trend_raw[t] ~ std_normal();
    }}
  }}
  
  generated quantities {{
    array[n_times] vector[n_series] trend_effects = trend;
  }}
  ")
}

#' Generate VAR Stan Code
#' 
#' @description
#' Generates Stan code for Vector Autoregressive trend models.
#' 
#' @param trend_spec Trend specification
#' @param data_info Data information
#' @param multivariate Logical (always TRUE for VAR)
#' @return Character string of VAR Stan code
#' @noRd
generate_var_stan_code <- function(trend_spec, data_info, multivariate = TRUE) {
  p_order <- trend_spec$p %||% 1
  n_series <- data_info$n_series %||% 2
  
  if (n_series < 2) {
    stop(insight::format_error(
      "VAR models require at least 2 series.",
      "Current n_series: {n_series}"
    ))
  }
  
  glue::glue("
  functions {{
    // VAR({p_order}) trend forecasting function
    matrix var{p_order}_forecast(matrix trend_last_p, array[] matrix A, matrix Sigma, int n_forecast) {{
      int K = rows(Sigma);
      matrix[n_forecast, K] forecasts;
      matrix[{p_order}, K] var_buffer = trend_last_p;
      matrix[K, K] L = cholesky_decompose(Sigma);
      
      for (t in 1:n_forecast) {{
        vector[K] mu_t = rep_vector(0, K);
        
        // VAR prediction
        for (p in 1:{p_order}) {{
          mu_t += A[p] * var_buffer[{p_order} - p + 1]';
        }}
        
        // Generate forecast with correlated errors
        forecasts[t] = (mu_t + L * to_vector(normal_rng(rep_vector(0, K), 1)))';
        
        // Update VAR buffer
        if ({p_order} > 1) {{
          var_buffer[1:({p_order}-1)] = var_buffer[2:{p_order}];
        }}
        var_buffer[{p_order}] = forecasts[t];
      }}
      return forecasts;
    }}
  }}
  
  data {{
    int<lower=1> n_times;
    int<lower=2> n_series;
    array[n_times, n_series] int ytimes;
  }}
  
  parameters {{
    // VAR coefficient matrices
    array[{p_order}] matrix[n_series, n_series] A_raw;
    
    // Error covariance matrix
    cov_matrix[n_series] Sigma_trend;
    
    // Raw innovations
    array[n_times] vector[n_series] trend_raw;
  }}
  
  transformed parameters {{
    // Apply stationarity constraints to VAR coefficients
    array[{p_order}] matrix[n_series, n_series] A;
    for (p in 1:{p_order}) {{
      A[p] = A_raw[p]; // Could add stationarity constraints here
    }}
    
    matrix[n_series, n_series] L_trend = cholesky_decompose(Sigma_trend);
    
    // VAR evolution
    array[n_times] vector[n_series] trend;
    
    // Initialize first p observations
    for (t in 1:min({p_order}, n_times)) {{
      trend[t] = L_trend * trend_raw[t];
    }}
    
    // VAR dynamics
    for (t in ({p_order}+1):n_times) {{
      vector[n_series] var_mean = rep_vector(0, n_series);
      
      for (p in 1:{p_order}) {{
        var_mean += A[p] * trend[t-p];
      }}
      
      trend[t] = var_mean + L_trend * trend_raw[t];
    }}
  }}
  
  model {{
    // Priors on VAR coefficients
    for (p in 1:{p_order}) {{
      to_vector(A_raw[p]) ~ normal(0, 0.5);
    }}
    
    // Prior on error covariance
    Sigma_trend ~ inv_wishart(n_series + 1, diag_matrix(rep_vector(1, n_series)));
    
    // Innovations
    for (t in 1:n_times) {{
      trend_raw[t] ~ std_normal();
    }}
  }}
  
  generated quantities {{
    array[n_times] vector[n_series] trend_effects = trend;
  }}
  ")
}

#' Generate GP Stan Code
#' 
#' @description
#' Generates Stan code for Gaussian Process trend models.
#' 
#' @param trend_spec Trend specification
#' @param data_info Data information
#' @param multivariate Logical for multivariate models
#' @return Character string of GP Stan code
#' @noRd
generate_gp_stan_code <- function(trend_spec, data_info, multivariate = FALSE) {
  kernel_type <- trend_spec$kernel %||% "exponential_quadratic"
  n_series <- data_info$n_series %||% 1
  
  glue::glue("
  functions {{
    // Gaussian Process covariance function
    matrix gp_exp_quad_cov(array[] real x, real alpha, real rho, real sigma) {{
      int N = size(x);
      matrix[N, N] K;
      
      for (i in 1:(N-1)) {{
        K[i, i] = alpha^2 + sigma^2;
        for (j in (i+1):N) {{
          K[i, j] = alpha^2 * exp(-0.5 * ((x[i] - x[j]) / rho)^2);
          K[j, i] = K[i, j];
        }}
      }}
      K[N, N] = alpha^2 + sigma^2;
      
      return K;
    }}
  }}
  
  data {{
    int<lower=1> n_times;
    int<lower=1> n_series;
    array[n_times] real time_vals;  // Actual time values for GP
    array[n_times, n_series] int ytimes;
  }}
  
  parameters {{
    // GP hyperparameters
    real<lower=0> alpha_gp;     // marginal variance
    real<lower=0> rho_gp;       // length scale
    real<lower=0> sigma_gp;     // observation noise
    
    // GP values at observed time points
    array[n_series] vector[n_times] gp_raw;
  }}
  
  transformed parameters {{
    array[n_times] vector[n_series] trend;
    
    // Compute GP covariance matrix
    matrix[n_times, n_times] K = gp_exp_quad_cov(time_vals, alpha_gp, rho_gp, sigma_gp);
    matrix[n_times, n_times] L_K = cholesky_decompose(K);
    
    // Transform raw GP values
    for (s in 1:n_series) {{
      vector[n_times] gp_values = L_K * gp_raw[s];
      trend[:, s] = gp_values;
    }}
  }}
  
  model {{
    // Priors on GP hyperparameters
    alpha_gp ~ student_t(3, 0, 2.5);
    rho_gp ~ inv_gamma(5, 5);
    sigma_gp ~ student_t(3, 0, 2.5);
    
    // GP prior
    for (s in 1:n_series) {{
      gp_raw[s] ~ std_normal();
    }}
  }}
  
  generated quantities {{
    array[n_times] vector[n_series] trend_effects = trend;
  }}
  ")
}

#' Generate CAR Stan Code
#' 
#' @description
#' Generates Stan code for Continuous AR (CAR1) trend models.
#' 
#' @param trend_spec Trend specification
#' @param data_info Data information
#' @param multivariate Logical for multivariate models
#' @return Character string of CAR Stan code
#' @noRd
generate_car_stan_code <- function(trend_spec, data_info, multivariate = FALSE) {
  n_series <- data_info$n_series %||% 1
  
  glue::glue("
  functions {{
    // CAR(1) trend forecasting function
    vector car1_forecast(vector trend_last, real phi, real sigma, array[] real time_diffs, int n_forecast) {{
      vector[n_forecast] forecasts;
      
      for (t in 1:n_forecast) {{
        real dt = time_diffs[t];
        real decay = exp(-1.0 / phi * dt);
        real conditional_var = sigma^2 * (1 - decay^2);
        
        if (t == 1) {{
          forecasts[t] = normal_rng(trend_last[1] * decay, sqrt(conditional_var));
        }} else {{
          forecasts[t] = normal_rng(forecasts[t-1] * decay, sqrt(conditional_var));
        }}
      }}
      return forecasts;
    }}
  }}
  
  data {{
    int<lower=1> n_times;
    int<lower=1> n_series;
    array[n_times-1] real<lower=0> time_diffs;  // Time differences between observations
    array[n_times, n_series] int ytimes;
  }}
  
  parameters {{
    // CAR parameters
    real<lower=0> phi_car;      // decay rate parameter
    real<lower=0> sigma_car;    // marginal variance
    
    // Initial values and innovations
    vector[n_series] trend_init;
    array[n_times-1] vector[n_series] car_innovations;
  }}
  
  transformed parameters {{
    array[n_times] vector[n_series] trend;
    
    // Initialize
    trend[1] = trend_init;
    
    // CAR(1) evolution with irregular time spacing
    for (t in 2:n_times) {{
      real dt = time_diffs[t-1];
      real decay = exp(-1.0 / phi_car * dt);
      real conditional_var = sigma_car^2 * (1 - decay^2);
      
      trend[t] = trend[t-1] * decay + car_innovations[t-1] * sqrt(conditional_var);
    }}
  }}
  
  model {{
    // Priors
    phi_car ~ inv_gamma(2, 1);
    sigma_car ~ student_t(3, 0, 2.5);
    
    // Initial state
    trend_init ~ normal(0, sigma_car);
    
    // Innovations
    for (t in 1:(n_times-1)) {{
      car_innovations[t] ~ std_normal();
    }}
  }}
  
  generated quantities {{
    array[n_times] vector[n_series] trend_effects = trend;
  }}
  ")
}