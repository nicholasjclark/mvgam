#' Trend Injection Stanvars for mvgam
#'
#' @description
#' Generates stanvars that inject temporal dynamics into brms-generated Stan code.
#' These inject parameters, functions, and model components rather than complete programs.

#' Generate Trend Injection Stanvars
#' 
#' @description
#' Main dispatcher that generates stanvars to inject temporal dynamics into brms Stan code.
#' The brms code provides trend_mus = X_trend * b_trend, we add temporal evolution.
#' 
#' @param trend_spec List containing trend specification (type, parameters, etc.)
#' @param data_info List containing data structure information
#' @return List of stanvars for injection
#' @noRd
generate_trend_injection_stanvars <- function(trend_spec, data_info) {
  checkmate::assert_list(trend_spec, names = "named")
  checkmate::assert_list(data_info, names = "named")
  
  trend_type <- trend_spec$type
  
  # Dispatch to appropriate trend stanvar generator
  stanvars <- switch(trend_type,
    "RW" = generate_rw_injection_stanvars(trend_spec, data_info),
    "AR" = generate_ar_injection_stanvars(trend_spec, data_info),
    "VAR" = generate_var_injection_stanvars(trend_spec, data_info),
    "GP" = generate_gp_injection_stanvars(trend_spec, data_info),
    "CAR" = generate_car_injection_stanvars(trend_spec, data_info),
    stop(insight::format_error(
      "Unknown trend type: {trend_type}",
      "Supported types: RW, AR, VAR, GP, CAR"
    ))
  )
  
  return(stanvars)
}

#' Generate Random Walk Injection Stanvars
#' 
#' @description
#' RW models: LV[i,j] ~ normal(trend_mus[...], sigma[j])
#' No temporal dependence, just independent evolution around trend_mus.
#' 
#' @param trend_spec Trend specification
#' @param data_info Data information
#' @return List of stanvars
#' @noRd
generate_rw_injection_stanvars <- function(trend_spec, data_info) {
  n_lv <- data_info$n_lv %||% data_info$n_series %||% 1
  
  # RW models don't need temporal parameters - they're just independent normals
  # The "random walk" behavior comes from the trend_mus evolution via brms
  stanvars <- list()
  
  # Add latent state variance parameters
  stanvars$sigma_params <- stanvar(
    x = NULL,
    name = "rw_sigma_params",
    scode = glue::glue("
    parameters {{
      // latent state SD parameters for RW
      vector<lower=0>[{n_lv}] sigma;
    }}
    ")
  )
  
  # Add RW model dynamics (no temporal dependence)
  stanvars$rw_model <- stanvar(
    x = NULL,
    name = "rw_model_block",
    scode = glue::glue("
    // RW latent state evolution (independent around mu_trend)
    for (j in 1:{n_lv}) {{
      for (i in 1:n) {{
        LV[i, j] ~ normal(mu_trend[ytimes_trend[i, j]], sigma[j]);
      }}
    }}
    ")
  )
  
  # Add priors
  stanvars$rw_priors <- stanvar(
    x = NULL,
    name = "rw_priors",
    scode = "
    // priors for RW latent state SD parameters
    sigma ~ inv_gamma(1.418, 0.452);
    "
  )
  
  return(stanvars)
}

#' Generate AR Injection Stanvars
#' 
#' @description
#' AR models: LV[i,j] ~ normal(trend_mus[...] + ar1[j] * (LV[i-1,j] - trend_mus[...]), sigma[j])
#' 
#' @param trend_spec Trend specification  
#' @param data_info Data information
#' @return List of stanvars
#' @noRd
generate_ar_injection_stanvars <- function(trend_spec, data_info) {
  n_lv <- data_info$n_lv %||% data_info$n_series %||% 1
  p_order <- trend_spec$p %||% 1
  
  stanvars <- list()
  
  if (length(p_order) == 1 && p_order == 1) {
    # AR(1) model - matches the example exactly
    stanvars$ar_params <- stanvar(
      x = NULL,
      name = "ar1_params",
      scode = glue::glue("
      parameters {{
        // AR parameters
        vector[{n_lv}] ar1;
        // latent state SD parameters  
        vector<lower=0>[{n_lv}] sigma;
      }}
      ")
    )
    
    stanvars$ar_model <- stanvar(
      x = NULL,
      name = "ar1_model_block", 
      scode = glue::glue("
      // AR(1) latent state evolution - matches mvgam pattern exactly
      for (j in 1:{n_lv}) {{
        LV[1, j] ~ normal(mu_trend[ytimes_trend[1, j]], sigma[j]);
        for (i in 2:n) {{
          LV[i, j] ~ normal(mu_trend[ytimes_trend[i, j]]
                            + ar1[j] * (LV[i - 1, j] - mu_trend[ytimes_trend[i - 1, j]]),
                            sigma[j]);
        }}
      }}
      ")
    )
    
    stanvars$ar_priors <- stanvar(
      x = NULL,
      name = "ar1_priors",
      scode = "
      // priors for AR parameters - matches mvgam exactly
      ar1 ~ std_normal();
      // priors for latent state SD parameters
      sigma ~ inv_gamma(1.418, 0.452);
      "
    )
    
  } else {
    # Higher order or seasonal AR - more complex implementation needed
    stop(insight::format_error(
      "Higher-order AR models not yet implemented.",
      "Currently only AR(1) is supported. Requested: AR(p = {paste(p_order, collapse = ', ')})"
    ))
  }
  
  return(stanvars)
}

#' Generate VAR Injection Stanvars
#' 
#' @description
#' VAR models: Complex multivariate dynamics with stationarity constraints.
#' Matches the extensive VAR(1) example provided.
#' 
#' @param trend_spec Trend specification
#' @param data_info Data information  
#' @return List of stanvars
#' @noRd
generate_var_injection_stanvars <- function(trend_spec, data_info) {
  n_lv <- data_info$n_lv %||% data_info$n_series %||% 2
  p_order <- trend_spec$p %||% 1
  
  if (n_lv < 2) {
    stop(insight::format_error(
      "VAR models require at least 2 latent factors.",
      "Current n_lv: {n_lv}"
    ))  
  }
  
  if (p_order != 1) {
    stop(insight::format_error(
      "Only VAR(1) currently supported.",
      "Requested: VAR(p = {p_order})"
    ))
  }
  
  stanvars <- list()
  
  # Add the complex VAR functions - copied exactly from the example
  stanvars$var_functions <- stanvar(
    x = NULL,
    name = "var_functions",
    scode = "
    functions {
      /* Function to compute the matrix square root */
      /* see Heaps 2022 for details (https://doi.org/10.1080/10618600.2022.2079648)*/
      matrix sqrtm(matrix A) {
        int m = rows(A);
        vector[m] root_root_evals = sqrt(sqrt(eigenvalues_sym(A)));
        matrix[m, m] evecs = eigenvectors_sym(A);
        matrix[m, m] eprod = diag_post_multiply(evecs, root_root_evals);
        return tcrossprod(eprod);
      }
      
      /* Function to transform P_real to P */
      /* see Heaps 2022 for details (https://doi.org/10.1080/10618600.2022.2079648)*/
      matrix P_realtoP(matrix P_real) {
        int m = rows(P_real);
        matrix[m, m] B = tcrossprod(P_real);
        for (i in 1 : m) {
          B[i, i] += 1.0;
        }
        return mdivide_left_spd(sqrtm(B), P_real);
      }
      
      /* Function to perform the reverse mapping*/
      /* see Heaps 2022 for details (https://doi.org/10.1080/10618600.2022.2079648)*/
      array[,] matrix rev_mapping(array[] matrix P, matrix Sigma) {
        int p = size(P);
        int m = rows(Sigma);
        array[p, p] matrix[m, m] phi_for;
        array[p, p] matrix[m, m] phi_rev;
        array[p + 1] matrix[m, m] Sigma_for;
        array[p + 1] matrix[m, m] Sigma_rev;
        matrix[m, m] S_for;
        matrix[m, m] S_rev;
        array[p + 1] matrix[m, m] S_for_list;
        array[p + 1] matrix[m, m] Gamma_trans;
        array[2, p] matrix[m, m] phiGamma;
        
        // Step 1:
        Sigma_for[p + 1] = Sigma;
        S_for_list[p + 1] = sqrtm(Sigma);
        for (s in 1 : p) {
          // In this block of code S_rev is B^{-1} and S_for is a working matrix
          S_for = -tcrossprod(P[p - s + 1]);
          for (i in 1 : m) {
            S_for[i, i] += 1.0;
          }
          S_rev = sqrtm(S_for);
          S_for_list[p - s + 1] = mdivide_right_spd(mdivide_left_spd(S_rev,
                                                                     sqrtm(
                                                                     quad_form_sym(
                                                                     Sigma_for[
                                                                     p - s + 2],
                                                                     S_rev))),
                                                    S_rev);
          Sigma_for[p - s + 1] = tcrossprod(S_for_list[p - s + 1]);
        }
        
        // Step 2:
        Sigma_rev[1] = Sigma_for[1];
        Gamma_trans[1] = Sigma_for[1];
        for (s in 0 : (p - 1)) {
          S_for = S_for_list[s + 1];
          S_rev = sqrtm(Sigma_rev[s + 1]);
          phi_for[s + 1, s + 1] = mdivide_right_spd(S_for * P[s + 1], S_rev);
          phi_rev[s + 1, s + 1] = mdivide_right_spd(S_rev * P[s + 1]', S_for);
          Gamma_trans[s + 2] = phi_for[s + 1, s + 1] * Sigma_rev[s + 1];
          if (s >= 1) {
            for (k in 1 : s) {
              phi_for[s + 1, k] = phi_for[s, k]
                                  - phi_for[s + 1, s + 1] * phi_rev[s, s - k + 1];
              phi_rev[s + 1, k] = phi_rev[s, k]
                                  - phi_rev[s + 1, s + 1] * phi_for[s, s - k + 1];
            }
            for (k in 1 : s) {
              Gamma_trans[s + 2] = Gamma_trans[s + 2]
                                   + phi_for[s, k] * Gamma_trans[s + 2 - k];
            }
          }
          Sigma_rev[s + 2] = Sigma_rev[s + 1]
                             - quad_form_sym(Sigma_for[s + 1],
                                             phi_rev[s + 1, s + 1]');
        }
        for (i in 1 : p) {
          phiGamma[1, i] = phi_for[p, i];
        }
        for (i in 1 : p) {
          phiGamma[2, i] = Gamma_trans[i]';
        }
        return phiGamma;
      }
    }
    "
  )
  
  # Add transformed data for VAR hyperparameters - from example
  stanvars$var_transformed_data <- stanvar(
    x = NULL,
    name = "var_transformed_data",
    scode = glue::glue("
    transformed data {{
      // exchangeable partial autocorrelation hyperparameters
      // see Heaps 2022 for details (https://doi.org/10.1080/10618600.2022.2079648)
      vector[2] es;
      vector<lower=0>[2] fs;
      vector<lower=0>[2] gs;
      vector<lower=0>[2] hs;
      es[1] = 0;
      es[2] = 0;
      fs[1] = sqrt(0.455);
      fs[2] = sqrt(0.455);
      gs[1] = 1.365;
      gs[2] = 1.365;
      hs[1] = 0.071175;
      hs[2] = 0.071175;
    }}
    ")
  )
  
  # Add VAR parameters - matches example exactly
  stanvars$var_params <- stanvar(
    x = NULL,
    name = "var_parameters",
    scode = glue::glue("
    parameters {{
      // latent state variance parameters
      cholesky_factor_corr[{n_lv}] L_Omega;
      vector<lower=0>[{n_lv}] sigma;
      
      // unconstrained VAR1 partial autocorrelations
      matrix[{n_lv}, {n_lv}] P_real;
      
      // partial autocorrelation hyperparameters
      vector[2] Pmu;
      vector<lower=0>[2] Pomega;
    }}
    ")
  )
  
  # Add VAR transformed parameters - matches example exactly
  stanvars$var_transformed <- stanvar(
    x = NULL,
    name = "var_transformed_params",
    scode = glue::glue("
    transformed parameters {{
      // latent state VAR1 autoregressive terms
      matrix[{n_lv}, {n_lv}] A;
      
      // LKJ form of covariance matrix
      matrix[{n_lv}, {n_lv}] L_Sigma;
      
      // computed error covariance matrix
      cov_matrix[{n_lv}] Sigma;
      
      // initial trend covariance
      cov_matrix[{n_lv}] Gamma;
      
      L_Sigma = diag_pre_multiply(sigma, L_Omega);
      Sigma = multiply_lower_tri_self_transpose(L_Sigma);
      
      // stationary VAR reparameterisation
      {{
        array[1] matrix[{n_lv}, {n_lv}] P;
        array[2, 1] matrix[{n_lv}, {n_lv}] phiGamma;
        P[1] = P_realtoP(P_real);
        phiGamma = rev_mapping(P, Sigma);
        A = phiGamma[1, 1];
        Gamma = phiGamma[2, 1];
      }}
    }}
    ")
  )
  
  # Add VAR model block - matches example exactly
  stanvars$var_model <- stanvar(
    x = NULL,
    name = "var_model_block",
    scode = glue::glue("
    // VAR model dynamics
    {{
      // latent state mean parameters
      array[n] vector[{n_lv}] mu;
      
      // priors for latent state variance parameters
      sigma ~ inv_gamma(1.418, 0.452);
      
      // LKJ error correlation prior
      L_Omega ~ lkj_corr_cholesky(2);
      
      // partial autocorrelation hyperpriors
      Pmu ~ normal(es, fs);
      Pomega ~ gamma(gs, hs);
      
      // unconstrained partial autocorrelations
      diagonal(P_real) ~ normal(Pmu[1], 1 / sqrt(Pomega[1]));
      for (i in 1:{n_lv}) {{
        for (j in 1:{n_lv}) {{
          if (i != j) {{
            P_real[i, j] ~ normal(Pmu[2], 1 / sqrt(Pomega[2]));
          }}
        }}
      }}
      
      // latent state means
      for (i in 2:n) {{
        mu[i] = A * (LV[i - 1] - mu_trend[ytimes_trend[i - 1, 1:{n_lv}]]);
      }}
      
      // VAR latent state evolution - matches example exactly  
      LV[1] ~ multi_normal(mu_trend[ytimes_trend[1, 1:{n_lv}]], Gamma);
      for (i in 2:n) {{
        LV[i] ~ multi_normal_cholesky(mu_trend[ytimes_trend[i, 1:{n_lv}]] + mu[i], L_Sigma);
      }}
    }}
    ")
  )
  
  return(stanvars)
}

#' Generate CAR Injection Stanvars
#' 
#' @description
#' CAR models: LV[i,j] ~ normal(trend_mus[...] + pow(ar1[j], time_dis[i,j]) * (...), sigma[j])
#' 
#' @param trend_spec Trend specification
#' @param data_info Data information
#' @return List of stanvars
#' @noRd
generate_car_injection_stanvars <- function(trend_spec, data_info) {
  n_lv <- data_info$n_lv %||% data_info$n_series %||% 1
  
  stanvars <- list()
  
  # CAR parameters - matches example
  stanvars$car_params <- stanvar(
    x = NULL,
    name = "car_params",
    scode = glue::glue("
    parameters {{
      // AR parameters for CAR
      vector[{n_lv}] ar1;
      // latent state SD parameters
      vector<lower=0>[{n_lv}] sigma;
    }}
    ")
  )
  
  # CAR model - matches example exactly
  stanvars$car_model <- stanvar(
    x = NULL,
    name = "car_model_block",
    scode = glue::glue("
    // CAR latent state evolution - matches mvgam pattern exactly
    for (j in 1:{n_lv}) {{
      LV[1, j] ~ normal(mu_trend[ytimes_trend[1, j]], sigma[j]);
      for (i in 2:n) {{
        LV[i, j] ~ normal(mu_trend[ytimes_trend[i, j]]
                          + pow(ar1[j], time_dis[i, j])
                            * (LV[i - 1, j] - mu_trend[ytimes_trend[i - 1, j]]),
                          sigma[j]);
      }}
    }}
    ")
  )
  
  # CAR priors
  stanvars$car_priors <- stanvar(
    x = NULL,
    name = "car_priors", 
    scode = "
    // priors for AR parameters
    ar1 ~ std_normal();
    // priors for latent state SD parameters
    sigma ~ inv_gamma(1.418, 0.452);
    "
  )
  
  return(stanvars)
}

#' Generate GP Injection Stanvars
#' 
#' @description
#' GP models: Currently not implemented as examples show GP applied to trend_formula,
#' not as temporal dynamics on LV.
#' 
#' @param trend_spec Trend specification
#' @param data_info Data information
#' @return List of stanvars
#' @noRd
generate_gp_injection_stanvars <- function(trend_spec, data_info) {
  stop(insight::format_error(
    "GP trends are handled via trend_formula, not as temporal dynamics.",
    "Use: trend_formula = ~ gp(time, k = 10) with trend_model = AR() or RW()"
  ))
}

#' Create Stanvar Helper
#' 
#' @description
#' Helper to create brms stanvar objects with proper structure.
#' 
#' @param x Data object (can be NULL for code-only stanvars)
#' @param name Character string name
#' @param scode Character string Stan code or block type
#' @return brms stanvar object
#' @noRd
stanvar <- function(x, name, scode) {
  if (!requireNamespace("brms", quietly = TRUE)) {
    stop(insight::format_error("Package {.pkg brms} required for stanvar creation"))
  }
  
  # Use brms stanvar function
  brms::stanvar(x = x, name = name, scode = scode)
}