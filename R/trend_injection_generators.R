#' Trend Injection Stanvars for mvgam
#'
#' @description
#' Generates stanvars that inject temporal dynamics into brms-generated Stan code.
#' These inject parameters, functions, and model components rather than complete programs.

#' Generate Trend Injection Stanvars
#'
#' @description
#' Registry-based dispatcher that generates stanvars to inject temporal dynamics into brms Stan code.
#' Automatically handles factor model compatibility validation and trend type dispatch.
#'
#' @param trend_spec List containing trend specification (trend_model, parameters, etc.)
#' @param data_info List containing data structure information
#' @return List of stanvars for injection
#' @noRd
generate_trend_injection_stanvars <- function(trend_spec, data_info) {
  checkmate::assert_list(trend_spec, names = "named")
  checkmate::assert_list(data_info, names = "named")
  checkmate::assert_string(trend_spec$trend_model, min.chars = 1)

  # Ensure registry is initialized
  ensure_registry_initialized()
  
  # Get trend information from registry
  trend_info <- get_trend_info(trend_spec$trend_model)
  
  # Automatic factor compatibility validation
  validate_factor_compatibility(trend_spec)
  
  # Call registered generator (handles factor vs full internally)
  stanvars <- trend_info$generator(trend_spec, data_info)
  
  return(stanvars)
}

#' Generate Random Walk Injection Stanvars
#'
#' @description
#' RW models: LV[i,j] ~ normal(trend_mus[...], sigma[j])
#' Supports both simple correlation (cor = TRUE) and hierarchical grouping (gr != 'NA').
#'
#' @param trend_spec Trend specification
#' @param data_info Data information
#' @return List of stanvars
#' @noRd
generate_rw_injection_stanvars <- function(trend_spec, data_info) {
  n_lv <- data_info$n_lv %||% data_info$n_series %||% 1
  use_cor <- trend_spec$cor %||% FALSE
  use_grouping <- !is.null(trend_spec$gr) && trend_spec$gr != 'NA'

  stanvars <- list()

  if (use_grouping) {
    # Hierarchical grouping case: RW(gr = region, subgr = species)
    n_groups <- data_info$n_groups %||% 1
    n_subgroups <- data_info$n_subgroups %||% n_lv

    stanvars$hierarchical_functions <- generate_hierarchical_functions_stanvar()
    stanvars$rw_hierarchical_params <- generate_hierarchical_params_stanvar(n_groups, n_subgroups, add_ar_params = FALSE)
    stanvars$rw_hierarchical_transformed <- generate_hierarchical_transformed_stanvar(n_groups, n_subgroups, dynamics_type = "rw")
    stanvars$rw_hierarchical_model <- generate_hierarchical_model_stanvar(n_groups, add_ar_priors = FALSE)

  } else if (use_cor) {
    # Simple correlation case: RW(cor = TRUE)
    stanvars$rw_correlated_params <- generate_simple_corr_params_stanvar(n_lv, add_ar_params = FALSE)

    stanvars$rw_correlated_transformed <- stanvar(
      x = NULL,
      name = "rw_correlated_transformed",
      scode = glue::glue("
      transformed parameters {{
        // latent states matrix
        matrix[n, {n_lv}] LV;

        // LKJ form of covariance matrix
        matrix[{n_lv}, {n_lv}] L_Sigma;

        // computed error covariance matrix
        cov_matrix[{n_lv}] Sigma;

        // derived RW latent states (cumulative sum of errors)
        for (j in 1:{n_lv}) {{
          LV[1, j] = trend_mus[ytimes_trend[1, j]] + error[1][j];
          for (i in 2 : n) {{
            // RW: LV[i] = trend_mus + LV[i-1] + error[i]
            LV[i, j] = trend_mus[ytimes_trend[i, j]] + LV[i - 1, j] + error[i][j];
          }}
        }}

        L_Sigma = diag_pre_multiply(sigma, L_Omega);
        Sigma = multiply_lower_tri_self_transpose(L_Sigma);
        
        // derived latent states
        for (i in 1:n) {{
          for (s in 1:n_series) {{
            trend[i, s] = dot_product(Z[s, :], LV[i, :]);
          }}
        }}
      }}
      ")
    )

    stanvars$rw_correlated_model <- stanvar(
      x = NULL,
      name = "rw_correlated_model",
      scode = "
      // priors for latent trend variance parameters
      sigma ~ inv_gamma(1.418, 0.452);

      // contemporaneous errors
      L_Omega ~ lkj_corr_cholesky(2);
      for (i in 1 : n) {
        error[i] ~ multi_normal_cholesky(trend_zeros, L_Sigma);
      }
      "
    )

  } else {
    # Simple uncorrelated case: RW() - non-centered parameterization
    stanvars$rw_simple_params <- stanvar(
      x = NULL,
      name = "rw_simple_params",
      scode = glue::glue("
      parameters {{
        // latent state SD parameters for RW
        vector<lower=0>[{n_lv}] sigma;
        // raw latent states for non-centered parameterization
        matrix[n, {n_lv}] LV_raw;
      }}
      ")
    )

    stanvars$rw_simple_transformed <- stanvar(
      x = NULL,
      name = "rw_simple_transformed",
      scode = glue::glue("
      transformed parameters {{
        // latent states with non-centered parameterization
        matrix[n, {n_lv}] LV;
        
        // Apply non-centered RW transformation
        LV = LV_raw .* rep_matrix(sigma', rows(LV_raw));
        for (j in 1:{n_lv}) {{
          LV[1, j] += trend_mus[ytimes_trend[1, j]];
          for (i in 2:n) {{
            LV[i, j] += trend_mus[ytimes_trend[i, j]] + LV[i - 1, j];
          }}
        }}
        
        // derived latent states
        for (i in 1:n) {{
          for (s in 1:n_series) {{
            trend[i, s] = dot_product(Z[s, :], LV[i, :]);
          }}
        }}
      }}
      ")
    )

    stanvars$rw_simple_model <- stanvar(
      x = NULL,
      name = "rw_simple_model",
      scode = "
      // Non-centered RW priors - only sample innovations
      sigma ~ inv_gamma(1.418, 0.452);
      to_vector(LV_raw) ~ std_normal();
      "
    )
  }

  return(stanvars)
}

#' Generate AR Injection Stanvars
#'
#' @description
#' AR models: Supports AR(1), higher-order AR(p), and seasonal AR with discontinuous lags.
#' Follows current mvgam Stan patterns with non-centered parameterization.
#' Supports hierarchical grouping (gr != 'NA') and correlation (cor = TRUE).
#'
#' @param trend_spec Trend specification
#' @param data_info Data information
#' @return List of stanvars
#' @noRd
generate_ar_injection_stanvars <- function(trend_spec, data_info) {
  n_lv <- data_info$n_lv %||% data_info$n_series %||% 1
  p_order <- trend_spec$p %||% 1
  use_cor <- trend_spec$cor %||% FALSE
  use_grouping <- !is.null(trend_spec$gr) && trend_spec$gr != 'NA'

  stanvars <- list()

  # Handle different AR specifications
  if (length(p_order) == 1 && p_order == 1) {
    # AR(1) model - original implementation
    if (use_grouping) {
      n_groups <- data_info$n_groups %||% 1
      n_subgroups <- data_info$n_subgroups %||% n_lv

      stanvars$hierarchical_functions <- generate_hierarchical_functions_stanvar()
      stanvars$ar_hierarchical_params <- generate_hierarchical_params_stanvar(n_groups, n_subgroups, add_ar_params = TRUE)
      stanvars$ar_hierarchical_transformed <- generate_hierarchical_transformed_stanvar(n_groups, n_subgroups, dynamics_type = "ar")
      stanvars$ar_hierarchical_model <- generate_hierarchical_model_stanvar(n_groups, add_ar_priors = TRUE)

    } else {
      stanvars$ar_params <- stanvar(
        x = NULL,
        name = "ar1_params",
        scode = glue::glue("
        parameters {{
          // AR parameters
          vector[{n_lv}] ar1;
          // latent state SD parameters
          vector<lower=0>[{n_lv}] sigma;
          // raw latent states for non-centered parameterization
          matrix[n, {n_lv}] LV_raw;
        }}
        ")
      )

      stanvars$ar_transform <- stanvar(
        x = NULL,
        name = "ar1_transform",
        scode = glue::glue("
        transformed parameters {{
          // latent states with non-centered parameterization
          matrix[n, {n_lv}] LV;
          
          // Apply non-centered transformation following mvgam pattern
          LV = LV_raw .* rep_matrix(sigma', rows(LV_raw));
          for (j in 1:{n_lv}) {{
            LV[1, j] += trend_mus[ytimes_trend[1, j]];
            for (i in 2:n) {{
              LV[i, j] += trend_mus[ytimes_trend[i, j]]
                          + ar1[j] * (LV[i - 1, j] - trend_mus[ytimes_trend[i - 1, j]]);
            }}
          }}
          
          // derived latent states
          for (i in 1:n) {{
            for (s in 1:n_series) {{
              trend[i, s] = dot_product(Z[s, :], LV[i, :]);
            }}
          }}
        }}
        ")
      )

      stanvars$ar_model <- stanvar(
        x = NULL,
        name = "ar1_model_block",
        scode = "
        // priors for AR parameters
        ar1 ~ std_normal();
        // priors for latent state SD parameters
        sigma ~ inv_gamma(1.418, 0.452);
        // non-centered innovations
        to_vector(LV_raw) ~ std_normal();
        "
      )
    }

  } else if (length(p_order) == 1 && p_order > 1) {
    # Higher-order AR(p) - continuous lags like AR(3)
    stanvars <- generate_ar_p_injection_stanvars(p_order, n_lv, data_info)
    
  } else if (length(p_order) > 1) {
    # Seasonal AR with discontinuous lags like AR(p = c(1, 12, 24))
    stanvars <- generate_seasonal_ar_injection_stanvars(p_order, n_lv, data_info)
    
  } else {
    stop(insight::format_error(
      "Invalid AR specification: {paste(p_order, collapse = ', ')}",
      "Use: AR(p = 1), AR(p = 3), or AR(p = c(1, 12, 24))"
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
      
      // derived latent states - VAR uses direct LV matrix
      for (i in 1:n) {{
        for (s in 1:n_series) {{
          trend[i, s] = dot_product(Z[s, :], LV[i, :]);
        }}
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
#' CAR models: Non-centered parameterization with continuous-time AR evolution
#' using time distances and consistent LV -> trend pattern
#'
#' @param trend_spec Trend specification
#' @param data_info Data information
#' @return List of stanvars
#' @noRd
generate_car_injection_stanvars <- function(trend_spec, data_info) {
  n_lv <- data_info$n_lv %||% data_info$n_series %||% 1

  stanvars <- list()

  # CAR parameters - non-centered parameterization
  stanvars$car_params <- stanvar(
    x = NULL,
    name = "car_params",
    scode = glue::glue("
    parameters {{
      // AR parameters for CAR
      vector[{n_lv}] ar1;
      // latent state SD parameters
      vector<lower=0>[{n_lv}] sigma;
      // raw latent states for non-centered parameterization
      matrix[n, {n_lv}] LV_raw;
    }}
    ")
  )

  # CAR data for time distances
  stanvars$car_data <- stanvar(
    x = NULL,
    name = "car_data",
    scode = glue::glue("
    data {{
      matrix[n, {n_lv}] time_dis;  // Time distances for CAR process
    }}
    ")
  )

  # CAR transformed parameters - non-centered with consistent pattern
  stanvars$car_transform <- stanvar(
    x = NULL,
    name = "car_transform",
    scode = glue::glue("
    transformed parameters {{
      // latent states with non-centered parameterization
      matrix[n, {n_lv}] LV;
      
      // Apply non-centered CAR transformation
      LV = LV_raw .* rep_matrix(sigma', rows(LV_raw));
      for (j in 1:{n_lv}) {{
        LV[1, j] += trend_mus[ytimes_trend[1, j]];
        for (i in 2:n) {{
          LV[i, j] += trend_mus[ytimes_trend[i, j]]
                      + pow(ar1[j], time_dis[i, j])
                        * (LV[i - 1, j] - trend_mus[ytimes_trend[i - 1, j]]);
        }}
      }}
      
      // derived latent states
      for (i in 1:n) {{
        for (s in 1:n_series) {{
          trend[i, s] = dot_product(Z[s, :], LV[i, :]);
        }}
      }}
    }}
    ")
  )

  # CAR model block - non-centered priors
  stanvars$car_model <- stanvar(
    x = NULL,
    name = "car_model_block",
    scode = "
    // Non-centered CAR(1) priors - only sample innovations
    ar1 ~ normal(0, 0.5);
    sigma ~ inv_gamma(1.418, 0.452);
    to_vector(LV_raw) ~ std_normal();
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
    "Use: trend_formula = ~ gp(time, k = 10)..."
  ))
}

#' Generate Piecewise (PW) Injection Stanvars
#'
#' @description
#' PW models: Piecewise linear or logistic trends with changepoints.
#' Based on Prophet-style piecewise regression with automatic changepoint detection.
#'
#' @param trend_spec Trend specification containing n_changepoints, growth type, etc.
#' @param data_info Data information
#' @return List of stanvars
#' @noRd
generate_pw_injection_stanvars <- function(trend_spec, data_info) {
  n_lv <- data_info$n_lv %||% data_info$n_series %||% 1
  n_changepoints <- trend_spec$n_changepoints %||% 10
  growth_type <- trend_spec$growth %||% "linear"
  is_logistic <- (trend_spec$trend_model == "PWlogistic" || growth_type == "logistic")

  # Extract variable names from trend specification
  time_var <- trend_spec$time %||% "time"
  series_var <- trend_spec$series %||% "series"
  cap_var <- trend_spec$cap %||% "cap"

  stanvars <- list()

  # Add PW functions - based on the current mvgam implementation
  stanvars$pw_functions <- stanvar(
    x = NULL,
    name = "pw_functions",
    scode = "
    functions {
      matrix get_changepoint_matrix(vector t, vector t_change, int T, int S) {
        /* Function to sort changepoints */
        /* credit goes to the Prophet development team at Meta (https://github.com/facebook/prophet/tree/main)*/
        matrix[T, S] A;
        row_vector[S] a_row;
        int cp_idx;
        A = rep_matrix(0, T, S);
        a_row = rep_row_vector(0, S);
        cp_idx = 1;
        for (i in 1 : T) {
          while ((cp_idx <= S) && (t[i] >= t_change[cp_idx])) {
            a_row[cp_idx] = 1;
            cp_idx = cp_idx + 1;
          }
          A[i] = a_row;
        }
        return A;
      }

      // logistic trend functions
      vector logistic_gamma(real k, real m, vector delta, vector t_change, int S) {
        /* Function to compute a logistic trend with changepoints */
        /* credit goes to the Prophet development team at Meta (https://github.com/facebook/prophet/tree/main)*/
        vector[S] gamma; // adjusted offsets, for piecewise continuity
        vector[S + 1] k_s; // actual rate in each segment
        real m_pr;
        k_s = append_row(k, k + cumulative_sum(delta));
        m_pr = m; // The offset in the previous segment
        for (i in 1 : S) {
          gamma[i] = (t_change[i] - m_pr) * (1 - k_s[i] / k_s[i + 1]);
          m_pr = m_pr + gamma[i]; // update for the next segment
        }
        return gamma;
      }

      vector logistic_trend(real k, real m, vector delta, vector t, vector cap,
                            matrix A, vector t_change, int S) {
        /* Function to adjust a logistic trend using a carrying capacity */
        /* credit goes to the Prophet development team at Meta (https://github.com/facebook/prophet/tree/main)*/
        vector[S] gamma;
        gamma = logistic_gamma(k, m, delta, t_change, S);
        return cap .* inv_logit((k + A * delta) .* (t - (m + A * gamma)));
      }

      // linear trend function
      /* Function to compute a linear trend with changepoints */
      /* credit goes to the Prophet development team at Meta (https://github.com/facebook/prophet/tree/main)*/
      vector linear_trend(real k, real m, vector delta, vector t, matrix A,
                          vector t_change) {
        return (k + A * delta) .* t + (m + A * (-t_change .* delta));
      }
    }
    "
  )

  # PW parameters - matches current mvgam implementation
  stanvars$pw_params <- stanvar(
    x = NULL,
    name = "pw_params",
    scode = glue::glue("
    parameters {{
      // base trend growth rates
      vector[n_series] k_trend;

      // trend offset parameters
      vector[n_series] m_trend;

      // trend rate adjustments per series
      matrix[n_changepoints, n_series] delta_trend;
    }}
    ")
  )

  # PW transformed parameters - consistent LV -> trend pattern
  if (is_logistic) {
    stanvars$pw_transformed <- stanvar(
      x = NULL,
      name = "pw_transformed",
      scode = glue::glue("
      transformed parameters {{
        // latent variables matrix
        matrix[n, {n_lv}] LV;

        // PW estimates using logistic growth
        for (j in 1:{n_lv}) {{
          LV[1 : n, j] = logistic_trend(k_trend[j], m_trend[j],
                                        to_vector(delta_trend[ : , j]), {time_var},
                                        to_vector({cap_var}[ : , j]), A, t_change,
                                        n_changepoints);
        }}
        
        // Add trend_mus to LV if n_lv < n_series
        if ({n_lv} < n_series) {{
          for (i in 1:n) {{
            for (j in 1:{n_lv}) {{
              LV[i, j] += trend_mus[ytimes_trend[i, j]];
            }}
          }}
        }}
        
        // derived latent states
        for (i in 1:n) {{
          for (s in 1:n_series) {{
            if ({n_lv} < n_series) {{
              trend[i, s] = dot_product(Z[s, :], LV[i, :]) + trend_mus[ytimes_trend[i, s]];
            }} else {{
              trend[i, s] = dot_product(Z[s, :], LV[i, :]);
            }}
          }}
        }}
      }}
      ")
    )
  } else {
    stanvars$pw_transformed <- stanvar(
      x = NULL,
      name = "pw_transformed",
      scode = glue::glue("
      transformed parameters {{
        // latent variables matrix
        matrix[n, {n_lv}] LV;

        // PW estimates using linear growth
        for (j in 1:{n_lv}) {{
          LV[1 : n, j] = linear_trend(k_trend[j], m_trend[j],
                                      to_vector(delta_trend[ : , j]), {time_var},
                                      A, t_change);
        }}
        
        // Add trend_mus to LV if n_lv < n_series
        if ({n_lv} < n_series) {{
          for (i in 1:n) {{
            for (j in 1:{n_lv}) {{
              LV[i, j] += trend_mus[ytimes_trend[i, j]];
            }}
          }}
        }}
        
        // derived latent states
        for (i in 1:n) {{
          for (s in 1:n_series) {{
            if ({n_lv} < n_series) {{
              trend[i, s] = dot_product(Z[s, :], LV[i, :]) + trend_mus[ytimes_trend[i, s]];
            }} else {{
              trend[i, s] = dot_product(Z[s, :], LV[i, :]);
            }}
          }}
        }}
      }}
      ")
    )
  }

  # PW priors - matches current implementation
  stanvars$pw_priors <- stanvar(
    x = NULL,
    name = "pw_priors",
    scode = glue::glue("
    // trend parameter priors
    m_trend ~ student_t(3, 0, 2.5);
    k_trend ~ std_normal();
    to_vector(delta_trend) ~ double_exponential(0, changepoint_scale);
    ")
  )

  return(stanvars)
}

#' Generate Zero-Mean Multivariate Normal (ZMVN) Injection Stanvars
#'
#' @description
#' ZMVN models: Correlated residual processes for Joint Species Distribution Models.
#' Models correlation structure without temporal dynamics.
#'
#' @param trend_spec Trend specification
#' @param data_info Data information
#' @return List of stanvars
#' @noRd
generate_zmvn_injection_stanvars <- function(trend_spec, data_info) {
  n_lv <- data_info$n_lv %||% data_info$n_series %||% 1
  use_grouping <- !is.null(trend_spec$gr) && trend_spec$gr != 'NA'
  unit_var <- trend_spec$unit %||% "time"

  stanvars <- list()

  if (use_grouping) {
    # Hierarchical ZMVN case: ZMVN(unit = site, gr = group, subgr = species)
    n_groups <- data_info$n_groups %||% 1
    n_subgroups <- data_info$n_subgroups %||% n_lv

    # Add hierarchical correlation functions (reuse if not already added)
    stanvars$hierarchical_functions <- stanvar(
      x = NULL,
      name = "hierarchical_functions",
      scode = "
      functions {
        /* Function to compute a partially pooled correlation matrix */
        matrix combine_cholesky(matrix global_chol_cor, matrix local_chol_cor,
                                real alpha) {
          int dim = rows(local_chol_cor);
          matrix[dim, dim] global_cor = multiply_lower_tri_self_transpose(global_chol_cor);
          matrix[dim, dim] local_cor = multiply_lower_tri_self_transpose(local_chol_cor);
          matrix[dim, dim] combined_chol_cor;
          combined_chol_cor = cholesky_decompose(alpha * global_cor
                                                 + (1 - alpha) * local_cor);
          return combined_chol_cor;
        }
      }
      "
    )

    # Hierarchical ZMVN parameters
    stanvars$zmvn_hierarchical_params <- stanvar(
      x = NULL,
      name = "zmvn_hierarchical_params",
      scode = glue::glue("
      parameters {{
        // correlation params for hierarchical ZMVN
        cholesky_factor_corr[{n_subgroups}] L_Omega_global;
        array[{n_groups}] cholesky_factor_corr[{n_subgroups}] L_deviation_group;
        real<lower=0, upper=1> alpha_cor;
      }}
      ")
    )

    # Hierarchical ZMVN model
    stanvars$zmvn_hierarchical_model <- stanvar(
      x = NULL,
      name = "zmvn_hierarchical_model",
      scode = glue::glue("
      {{
        // derived hierarchical correlation matrices
        array[{n_groups}] cholesky_factor_corr[{n_subgroups}] L_Omega_group;
        for (g in 1 : {n_groups}) {{
          L_Omega_group[g] = combine_cholesky(L_Omega_global, L_deviation_group[g],
                                              alpha_cor);
        }}

        // hierarchical ZMVN process
        alpha_cor ~ beta(3, 2);
        L_Omega_global ~ lkj_corr_cholesky(1);
        for (g in 1 : {n_groups}) {{
          L_deviation_group[g] ~ lkj_corr_cholesky(6);
        }}

        // ZMVN residual correlation by {unit_var}
        for (i in 1:n_{unit_var}) {{
          for (g in 1:{n_groups}) {{
            to_vector(LV[group_unit_indices[i, g]]) ~ multi_normal_cholesky(rep_vector(0, {n_subgroups}),
                                                                             L_Omega_group[g]);
          }}
        }}
      }}
      ")
    )

  } else {
    # Simple ZMVN case: ZMVN(unit = site, subgr = species)
    stanvars$zmvn_simple_params <- stanvar(
      x = NULL,
      name = "zmvn_simple_params",
      scode = glue::glue("
      parameters {{
        // correlation matrix for ZMVN
        cholesky_factor_corr[{n_lv}] L_Omega;
      }}
      ")
    )

    stanvars$zmvn_simple_model <- stanvar(
      x = NULL,
      name = "zmvn_simple_model",
      scode = glue::glue("
      // Simple ZMVN residual correlation by {unit_var}
      L_Omega ~ lkj_corr_cholesky(2);
      for (i in 1:n_{unit_var}) {{
        to_vector(LV[{unit_var}_indices[i]]) ~ multi_normal_cholesky(rep_vector(0, {n_lv}), L_Omega);
      }}
      ")
    )
  }

  return(stanvars)
}

# Helper Functions for Trend Stancode Generation
# ======================================================

#' Generate Hierarchical Correlation Functions
#' @description Common functions block for hierarchical correlation models
#' @noRd
generate_hierarchical_functions_stanvar <- function() {
  stanvar(
    x = NULL,
    name = "hierarchical_functions",
    scode = "
    functions {
      /* Function to compute a partially pooled correlation matrix */
      /* https://discourse.mc-stan.org/t/hierarchical-prior-for-partial-pooling-on-correlation-matrices*/
      matrix combine_cholesky(matrix global_chol_cor, matrix local_chol_cor,
                              real alpha) {
        int dim = rows(local_chol_cor);
        matrix[dim, dim] global_cor = multiply_lower_tri_self_transpose(global_chol_cor);
        matrix[dim, dim] local_cor = multiply_lower_tri_self_transpose(local_chol_cor);
        matrix[dim, dim] combined_chol_cor;
        combined_chol_cor = cholesky_decompose(alpha * global_cor
                                               + (1 - alpha) * local_cor);
        return combined_chol_cor;
      }
    }
    "
  )
}

#' Generate Hierarchical Correlation Parameters
#' @description Common parameters block for hierarchical models
#' @param n_groups Number of groups
#' @param n_subgroups Number of subgroups
#' @param add_ar_params Whether to include AR parameters
#' @noRd
generate_hierarchical_params_stanvar <- function(n_groups, n_subgroups, add_ar_params = FALSE) {
  ar_params_code <- if (add_ar_params) {
    "\n        // latent trend AR1 terms\n        vector<lower=-1, upper=1>[n_series] ar1;\n"
  } else {
    ""
  }

  stanvar(
    x = NULL,
    name = "hierarchical_params",
    scode = glue::glue("
    parameters {{
      // latent trend variance parameters
      vector<lower=0>[n_series] sigma;{ar_params_code}

      // correlation params and dynamic error parameters per group
      cholesky_factor_corr[{n_subgroups}] L_Omega_global;
      array[{n_groups}] cholesky_factor_corr[{n_subgroups}] L_deviation_group;
      real<lower=0, upper=1> alpha_cor;
      array[n] matrix[{n_groups}, {n_subgroups}] sub_error;
    }}
    ")
  )
}

#' Generate Hierarchical Correlation Model Block
#' @description Common model block for hierarchical correlation priors
#' @param n_groups Number of groups
#' @param add_ar_priors Whether to include AR parameter priors
#' @noRd
generate_hierarchical_model_stanvar <- function(n_groups, add_ar_priors = FALSE) {
  ar_priors_code <- if (add_ar_priors) {
    "\n      // priors for AR parameters\n      ar1 ~ std_normal();\n"
  } else {
    ""
  }

  stanvar(
    x = NULL,
    name = "hierarchical_model",
    scode = glue::glue("{ar_priors_code}
    // priors for latent trend variance parameters
    sigma ~ inv_gamma(1.418, 0.452);

    // hierarchical process error correlations
    alpha_cor ~ beta(3, 2);
    L_Omega_global ~ lkj_corr_cholesky(1);
    for (g in 1 : {n_groups}) {{
      L_deviation_group[g] ~ lkj_corr_cholesky(6);
    }}

    // contemporaneous errors
    for (i in 1 : n) {{
      for (g in 1 : {n_groups}) {{
        to_vector(sub_error[i, g]) ~ multi_normal_cholesky(trend_zeros,
                                                           L_Sigma_group[g]);
      }}
    }}
    ")
  )
}

#' Generate Hierarchical Transformed Parameters
#' @description Common transformed parameters for hierarchical models
#' @param n_groups Number of groups
#' @param n_subgroups Number of subgroups
#' @param dynamics_type Either "rw" or "ar"
#' @noRd
generate_hierarchical_transformed_stanvar <- function(n_groups, n_subgroups, dynamics_type = "rw") {
  dynamics_code <- switch(dynamics_type,
    "rw" = "
        // derived RW latent states (cumulative sum of errors)
        trend_raw[1] = error[1];
        for (i in 2 : n) {
          // RW: trend[i] = trend[i-1] + error[i]
          trend_raw[i] = trend_raw[i - 1] + error[i];
        }",
    "ar" = "
        // derived AR(1) latent states
        trend_raw[1] = error[1];
        for (i in 2 : n) {
          // full AR process: trend[i] = ar1 * trend[i-1] + error[i]
          trend_raw[i] = ar1 .* trend_raw[i - 1] + error[i];
        }"
  )

  stanvar(
    x = NULL,
    name = "hierarchical_transformed",
    scode = glue::glue("
    transformed parameters {{
      array[n] vector[n_series] trend_raw;

      // reconstructed correlated errors
      array[n] vector[n_series] error;
      array[{n_groups}] cholesky_factor_corr[{n_subgroups}] L_Omega_group;

      // LKJ forms of covariance matrices
      array[{n_groups}] matrix[{n_subgroups}, {n_subgroups}] L_Sigma_group;

      // derived error correlation and covariance matrices
      for (g in 1 : {n_groups}) {{
        L_Omega_group[g] = combine_cholesky(L_Omega_global, L_deviation_group[g],
                                            alpha_cor);
        L_Sigma_group[g] = diag_pre_multiply(sigma[group_inds[g]],
                                             L_Omega_group[g]);
      }}

      // derived correlated errors
      for (i in 1 : n) {{
        error[i] = to_vector(sub_error[i]');
      }}{dynamics_code}
    }}
    ")
  )
}

#' Generate Simple Correlation Parameters
#' @description Common parameters for simple correlation models (cor = TRUE)
#' @param n_lv Number of latent variables
#' @param add_ar_params Whether to include AR parameters
#' @noRd
generate_simple_corr_params_stanvar <- function(n_lv, add_ar_params = FALSE) {
  ar_params_code <- if (add_ar_params) {
    "\n        // AR parameters\n        vector[{n_lv}] ar1;\n"
  } else {
    ""
  }

  stanvar(
    x = NULL,
    name = "correlated_params",
    scode = glue::glue("
    parameters {{
      // latent trend variance parameters
      vector<lower=0>[{n_lv}] sigma;
      cholesky_factor_corr[{n_lv}] L_Omega;{ar_params_code}

      // dynamic error parameters
      array[n] vector[{n_lv}] error;
    }}
    ")
  )
}

#' Generate Higher-Order AR(p) Injection Stanvars
#'
#' @description
#' Generates stanvars for AR(p) models with continuous lags (e.g., AR(3)).
#' Follows current mvgam pattern with individual parameters (ar1, ar2, ar3, ...).
#'
#' @param p_order Integer AR order
#' @param n_lv Number of latent variables
#' @param data_info Data information
#' @return List of stanvars
#' @noRd
generate_ar_p_injection_stanvars <- function(p_order, n_lv, data_info) {
  
  # Generate individual AR parameter names: ar1, ar2, ar3, etc.
  ar_param_names <- paste0("ar", 1:p_order)
  ar_param_decl <- paste0("  vector<lower=-1, upper=1>[", n_lv, "] ", ar_param_names, ";", collapse = "\n")
  ar_param_priors <- paste0("  ", ar_param_names, " ~ std_normal();", collapse = "\n")
  
  stanvars <- list()
  
  # Parameters with individual AR terms following mvgam pattern
  stanvars$ar_params <- stanvar(
    x = NULL,
    name = "ar_p_params",
    scode = glue::glue("
    parameters {{
      // individual AR parameters for AR({p_order})
{ar_param_decl}
      
      // latent state SD parameters
      vector<lower=0>[{n_lv}] sigma;
      
      // raw latent states for non-centered parameterization
      matrix[n, {n_lv}] LV_raw;
    }}
    ")
  )
  
  # Generate the evolution code following the mvgam AR(3) pattern
  # Handle initial timepoints (1, 2, ..., p) separately, then general loop
  initial_cases <- character(p_order)
  for (i in 1:p_order) {
    if (i == 1) {
      initial_cases[i] <- glue::glue("    LV[1, j] += trend_mus[ytimes_trend[1, j]];")
    } else {
      ar_terms <- character(i-1)
      for (lag in 1:(i-1)) {
        ar_terms[lag] <- glue::glue("ar{lag}[j] * (LV[{i - lag}, j] - trend_mus[ytimes_trend[{i - lag}, j]])")
      }
      ar_sum <- paste(ar_terms, collapse = "\n                  + ")
      initial_cases[i] <- glue::glue("    LV[{i}, j] += trend_mus[ytimes_trend[{i}, j]]\n                  + {ar_sum};")
    }
  }
  
  # Generate AR terms for the general loop (timepoints > p)
  general_ar_terms <- character(p_order)
  for (lag in 1:p_order) {
    general_ar_terms[lag] <- glue::glue("ar{lag}[j] * (LV[i - {lag}, j] - trend_mus[ytimes_trend[i - {lag}, j]])")
  }
  general_ar_sum <- paste(general_ar_terms, collapse = "\n                  + ")
  
  stanvars$ar_transform <- stanvar(
    x = NULL,
    name = "ar_p_transform",
    scode = glue::glue("
    transformed parameters {{
      // latent states with non-centered parameterization
      matrix[n, {n_lv}] LV;
      
      // Apply non-centered transformation following mvgam AR({p_order}) pattern
      LV = LV_raw .* rep_matrix(sigma', rows(LV_raw));
      for (j in 1:{n_lv}) {{
{paste(initial_cases, collapse = '\n')}
        for (i in {p_order + 1}:n) {{
          LV[i, j] += trend_mus[ytimes_trend[i, j]]
                      + {general_ar_sum};
        }}
      }}
      
      // derived latent states
      for (i in 1:n) {{
        for (s in 1:n_series) {{
          trend[i, s] = dot_product(Z[s, :], LV[i, :]);
        }}
      }}
    }}
    ")
  )
  
  stanvars$ar_model <- stanvar(
    x = NULL,
    name = "ar_p_model_block",
    scode = glue::glue("
    // priors for AR parameters
{ar_param_priors}
    // priors for latent state SD parameters
    sigma ~ inv_gamma(1.418, 0.452);
    // non-centered innovations
    to_vector(LV_raw) ~ std_normal();
    ")
  )
  
  return(stanvars)
}

#' Generate Seasonal AR Injection Stanvars
#'
#' @description
#' Generates stanvars for seasonal AR models with discontinuous lags (e.g., AR(p = c(1, 12, 24))).
#' Creates individual parameters for each lag and handles the evolution efficiently.
#'
#' @param p_lags Vector of AR lags
#' @param n_lv Number of latent variables
#' @param data_info Data information
#' @return List of stanvars
#' @noRd
generate_seasonal_ar_injection_stanvars <- function(p_lags, n_lv, data_info) {
  n_lags <- length(p_lags)
  max_lag <- max(p_lags)
  
  # Generate individual AR parameter names for each lag
  ar_param_names <- paste0("ar", p_lags)
  ar_param_decl <- paste0("  vector<lower=-1, upper=1>[", n_lv, "] ", ar_param_names, ";", collapse = "\n")
  ar_param_priors <- paste0("  ", ar_param_names, " ~ std_normal();", collapse = "\n")
  
  stanvars <- list()
  
  # Add lag data to Stan
  stanvars$ar_data <- stanvar(
    x = as.array(p_lags),
    name = "ar_lags",
    scode = glue::glue("
    data {{
      int ar_lags[{n_lags}] = {{{paste(p_lags, collapse = ', ')}}};
      int max_lag = {max_lag};
      int n_lags = {n_lags};
    }}
    ")
  )
  
  stanvars$ar_params <- stanvar(
    x = NULL,
    name = "seasonal_ar_params",
    scode = glue::glue("
    parameters {{
      // individual AR parameters for lags: {paste(p_lags, collapse = ', ')}
{ar_param_decl}
      
      // latent state SD parameters
      vector<lower=0>[{n_lv}] sigma;
      
      // raw latent states for non-centered parameterization
      matrix[n, {n_lv}] LV_raw;
    }}
    ")
  )
  
  # Build the lag contributions dynamically for initialization phase
  lag_contributions <- character(n_lags)
  for (k in 1:n_lags) {
    lag <- p_lags[k]
    lag_contributions[k] <- glue::glue("          if (i > ar_lags[{k}]) {{\n            LV[i, j] += ar{lag}[j] * (LV[i - ar_lags[{k}], j] - trend_mus[ytimes_trend[i - ar_lags[{k}], j]]);\n          }}")
  }
  
  # Build AR contributions for the main loop
  main_ar_contributions <- character(n_lags)
  for (k in 1:n_lags) {
    lag <- p_lags[k]
    main_ar_contributions[k] <- glue::glue("            ar_contribution += ar{lag}[j] * (LV[i - ar_lags[{k}], j] - trend_mus[ytimes_trend[i - ar_lags[{k}], j]]);")
  }
  
  # Complete the transformation code
  stanvars$ar_transform <- stanvar(
    x = NULL,
    name = "seasonal_ar_transform",
    scode = glue::glue("
    transformed parameters {{
      // latent states with non-centered parameterization
      matrix[n, {n_lv}] LV;
      
      // Apply non-centered transformation for seasonal AR
      LV = LV_raw .* rep_matrix(sigma', rows(LV_raw));
      for (j in 1:{n_lv}) {{
        // Initialize first max_lag timepoints with available lags
        for (i in 1:min(max_lag, n)) {{
          LV[i, j] += trend_mus[ytimes_trend[i, j]];
{paste(lag_contributions, collapse = '\n')}
        }}
        
        // Efficient computation for remaining timepoints
        if (n > max_lag) {{
          for (i in (max_lag+1):n) {{
            real ar_contribution = 0;
{paste(main_ar_contributions, collapse = '\n')}
            LV[i, j] += trend_mus[ytimes_trend[i, j]] + ar_contribution;
          }}
        }}
      }}
      
      // derived latent states
      for (i in 1:n) {{
        for (s in 1:n_series) {{
          trend[i, s] = dot_product(Z[s, :], LV[i, :]);
        }}
      }}
    }}
    ")
  )
  
  stanvars$ar_model <- stanvar(
    x = NULL,
    name = "seasonal_ar_model_block",
    scode = glue::glue("
    // priors for seasonal AR parameters
{ar_param_priors}
    // priors for latent state SD parameters
    sigma ~ inv_gamma(1.418, 0.452);
    // non-centered innovations
    to_vector(LV_raw) ~ std_normal();
    ")
  )
  
  return(stanvars)
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
